# --- Required Packages ---
# install.packages(c(
#   "shiny", "bslib", "dplyr", "tibble", "sf", "leaflet", "leaflet.extras",
#   "mapedit", "shinyjs", "httr", "jsonlite", "promises", "future", "later",
#   "parallelly", "progress", "raster", "terra", "elevatr", "planetR", "zip",
#   "shinycssloaders", "clhs", "viridis", "DT", "rstac", "ggplot2", "lubridate"
# ))
# Fast attach: UI + async shell. Heavy spatial/stat pkgs load on first user session (see server).
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyjs)
  library(jsonlite)
  library(promises)
  library(future)
  library(later)
  library(parallelly)
  library(leaflet)
  library(leaflet.extras)
  library(mapedit)
  library(shinycssloaders)
})

.geosampler_pkg_loaded <- new.env(parent = emptyenv())

load_geosampler_packages <- function() {
  if (isTRUE(.geosampler_pkg_loaded$done)) return(invisible(TRUE))
  suppressPackageStartupMessages({
    library(dplyr)
    library(tibble)
    library(sf)
    library(httr)
    library(raster)
    library(terra)
    tryCatch({
      Sys.setenv(GDAL_DISABLE_READDIR_ON_FILE_OPEN = "EMPTY_DIR")
    }, error = function(e) invisible(NULL))
    library(zip)
    library(elevatr)
    library(clhs)
    library(viridis)
    library(planetR)
    library(rstac)
    library(ggplot2)
    library(lubridate)
    # elevatr / some raster workflows expect this package installed in the R library.
    if (requireNamespace("progress", quietly = TRUE)) {
      suppressPackageStartupMessages(library(progress))
    }
  })
  .geosampler_pkg_loaded$done <- TRUE
  invisible(TRUE)
}

VECTOR_UPLOAD_ACCEPT <- c(
  ".geojson", ".json", ".kml", ".kmz", ".zip", ".gpkg",
  ".shp", ".dbf", ".shx", ".prj", ".cpg"
)
VECTOR_UPLOAD_LABEL <- "(.geojson, .kml, .kmz, .zip, .gpkg, or .shp + sidecars)"

# Lightweight async runtime: respect cgroup CPU limits on shinyapps.io / Connect.
is_hosted_shiny <- function() {
  env <- Sys.getenv(c(
    "SHINY_SERVER_VERSION", "CONNECT_SERVER", "CONNECT_API_KEY",
    "RSTUDIO_CONNECT_APP_NAME", "RSTUDIO_CONNECT_APP_ACCOUNT",
    "R_CONFIG_ACTIVE", "SHINY_PORT"
  ), unset = "")
  if (identical(env[["R_CONFIG_ACTIVE"]], "shinyapps")) return(TRUE)
  if (any(nzchar(env[c("SHINY_SERVER_VERSION", "CONNECT_SERVER", "CONNECT_API_KEY",
                        "RSTUDIO_CONNECT_APP_NAME", "RSTUDIO_CONNECT_APP_ACCOUNT")]))) {
    return(TRUE)
  }
  grepl("shinyapps", paste(c(env, Sys.getenv("HOSTNAME", "")), collapse = " "), ignore.case = TRUE)
}

available_local_cores <- function() {
  tryCatch({
    if (requireNamespace("parallelly", quietly = TRUE)) {
      as.integer(parallelly::availableCores(methods = c("cgroups", "cgroups2", "system")))
    } else {
      as.integer(max(1L, parallel::detectCores(logical = FALSE)))
    }
  }, error = function(e) 1L)
}

use_sequential_futures <- function() {
  is_hosted_shiny() || available_local_cores() <= 1L
}

options(parallelly.maxWorkers.localhost = 1L)
if (use_sequential_futures()) {
  future::plan(future::sequential)
  options(future.globals.maxSize = 150 * 1024^2)
} else {
  future::plan(future::multisession, workers = 1L)
  options(future.globals.maxSize = 300 * 1024^2)
}
options(dplyr.summarise.inform = FALSE)

trim_session_memory <- function() {
  tryCatch({
    gc(verbose = FALSE)
    gc(verbose = FALSE)
  }, error = function(e) invisible(NULL))
  invisible(NULL)
}

unlink_temp_raster_path <- function(path) {
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) return(invisible(FALSE))
  if (!file.exists(path)) return(invisible(FALSE))
  try(unlink(path), silent = TRUE)
  invisible(TRUE)
}

release_geosampler_memory <- function() {
  trim_session_memory()
  if (requireNamespace("terra", quietly = TRUE)) {
    tryCatch(terra::gc(), error = function(e) invisible(NULL))
  }
  if (requireNamespace("raster", quietly = TRUE)) {
    tryCatch(raster::flushCache(), error = function(e) invisible(NULL))
  }
  tryCatch(gc(verbose = FALSE), error = function(e) invisible(NULL))
  tryCatch(gc(verbose = FALSE), error = function(e) invisible(NULL))
  invisible(NULL)
}

if (is_hosted_shiny()) {
  tryCatch({
    if (requireNamespace("terra", quietly = TRUE)) terra::memfrac(0.35)
  }, error = function(e) invisible(NULL))
  options(future.globals.maxSize = 120 * 1024^2)
}

compact_stac_features <- function(features, max_keep = NULL) {
  if (!is.list(features) || !length(features)) return(features)
  if (!is.null(max_keep)) {
    max_keep <- suppressWarnings(as.integer(max_keep))
    if (length(max_keep) == 1L && !is.na(max_keep) && max_keep >= 1L && length(features) > max_keep) {
      features <- features[seq_len(max_keep)]
    }
  }
  lapply(features, function(f) {
    if (!is.list(f)) return(f)
    assets <- f$assets
    slim_assets <- if (is.list(assets)) {
      keep <- intersect(c("B02", "B03", "B04", "B05", "B08", "SCL"), names(assets))
      assets[keep]
    } else assets
  list(
      type = if (is.null(f$type)) "Feature" else as.character(f$type),
      id = as.character(f$id),
      geometry = f$geometry,
      bbox = f$bbox,
      properties = f$properties,
      assets = slim_assets
    )
  })
}

PC_STAC_URL <- "https://planetarycomputer.microsoft.com/api/stac/v1/"

sentinel_feature_datetime <- function(f) {
  if (is.null(f) || !is.list(f$properties)) return(as.POSIXct(NA, tz = "UTC"))
  suppressWarnings(as.POSIXct(f$properties$datetime, tz = "UTC"))
}

sort_stac_features_by_datetime <- function(features, decreasing = FALSE) {
  if (!is.list(features) || length(features) < 2L) return(features)
  times <- vapply(features, function(f) {
    as.numeric(sentinel_feature_datetime(f))
  }, numeric(1))
  features[order(times, decreasing = decreasing, na.last = TRUE)]
}

safe_features <- function(x) {
  if (is.list(x) && !is.null(x$features) && is.list(x$features)) {
    return(x$features)
  }
  list()
}

# Evenly spaced indices across an already date-sorted scene list (for median / search cap).
sentinel_median_scene_pick_indices <- function(n_available, n_use) {
  n_avail <- suppressWarnings(as.integer(n_available))
  n_use <- suppressWarnings(as.integer(n_use))
  if (length(n_avail) != 1L || is.na(n_avail) || n_avail < 1L) return(integer())
  if (length(n_use) != 1L || is.na(n_use) || n_use < 1L) return(integer())
  n_use <- min(n_use, n_avail)
  if (n_use >= n_avail) return(seq_len(n_avail))
  unique(as.integer(round(seq(1, n_avail, length.out = n_use))))
}

# Lightweight optional scene ranking (not used for download or sampling logic).
SENTINEL_NDRE_RANK_MAX_SCENES <- function() {
  12L
}

SENTINEL_NDRE_RANK_MAX_CELLS <- function() {
  if (is_hosted_shiny()) 450L else 800L
}

SENTINEL_NDRE_RANK_MIN_PIXELS <- function() {
  8L
}

# Chart preview cap only (search keeps full list; timeseries subsamples when > cap).
SENTINEL_NDRE_EVENLY_SPACED_CAP <- function() {
  25L
}

SENTINEL_STAC_PAGE_LIMIT <- function() {
  if (is_hosted_shiny()) 100L else 250L
}

SENTINEL_STAC_SEARCH_MAX_ITEMS <- function() {
  if (is_hosted_shiny()) 200L else 500L
}

# Time-series preview: mean NDRE per scene (COG subsample; no full retrieve).
SENTINEL_NDRE_TS_MAX_SCENES <- function() {
  SENTINEL_NDRE_EVENLY_SPACED_CAP()
}

stac_link_next_href <- function(it_obj) {
  if (!is.list(it_obj)) return(NA_character_)
  lk <- it_obj$links
  if (is.null(lk)) return(NA_character_)
  if (is.data.frame(lk)) {
    w <- which(lk$rel == "next")
    if (length(w)) return(as.character(lk$href[w[1L]]))
    return(NA_character_)
  }
  if (is.list(lk)) {
    for (x in lk) {
      if (is.list(x) && identical(x$rel, "next")) {
        h <- as.character(x$href %||% "")
        if (nzchar(h)) return(h)
      }
    }
  }
  NA_character_
}

sentinel_search_results_notice_ui <- function(n_listed, stats = NULL) {
  n_listed <- suppressWarnings(as.integer(n_listed))
  if (length(n_listed) != 1L || is.na(n_listed) || n_listed < 1L) return(NULL)
  cap <- SENTINEL_NDRE_TS_MAX_SCENES()
  truncated <- is.list(stats) && isTRUE(stats$stac_truncated)
  tags$div(
    class = "alert alert-info",
    style = "font-size:11.5px; margin-top:8px; padding:8px 10px; line-height:1.4;",
    icon("info-circle"),
    tags$span(
      if (n_listed > cap) {
        paste0(
          "Search list: ", n_listed, " scene(s), oldest → newest. ",
          "Timeseries chart uses up to ", cap, " evenly spaced scenes when you rebuild ",
          "(or all ", n_listed, " if fewer than ", cap, ")."
        )
      } else {
        paste0("Search list: ", n_listed, " scene(s), oldest → newest (all used in the timeseries chart).")
      },
      if (truncated) {
        paste0(
          " Catalog page cap reached (max ", stats$max_items_cap %||% SENTINEL_STAC_SEARCH_MAX_ITEMS(),
          " items) — narrow the date range if the list looks incomplete."
        )
      } else {
        ""
      }
    )
  )
}

ndre_workflow_memory_tick <- function(scene_index = NULL, every_n = 1L) {
  if (!is.null(scene_index)) {
    idx <- suppressWarnings(as.integer(scene_index))
    every_n <- suppressWarnings(as.integer(every_n))
    if (length(every_n) != 1L || is.na(every_n) || every_n < 1L) every_n <- 1L
    if (length(idx) == 1L && !is.na(idx) && idx %% every_n != 0L) {
      return(invisible(NULL))
    }
  }
  if (requireNamespace("terra", quietly = TRUE)) {
    tryCatch(terra::gc(), error = function(e) invisible(NULL))
  }
  trim_session_memory()
  invisible(NULL)
}

recommended_buffer_m_for_ha <- function(area_ha) {
  area_ha <- suppressWarnings(as.numeric(area_ha))
  if (!is.finite(area_ha) || area_ha <= 0) return(15)
  if (area_ha <= 10) return(15)
  if (area_ha <= 30) return(25)
  if (area_ha <= 100) return(40)
  80
}

# Same area-based inward buffer as automatic sampling (recommended_buffer_m_for_ha).
interior_buffer_m_for_boundary <- function(boundary_sf, boundary_4326 = NULL) {
  b <- boundary_4326
  if (is.null(b)) b <- boundary_sf_4326(boundary_sf)
  if (is.null(b)) return(15)
  area_ha <- tryCatch(as.numeric(sum(sf::st_area(b))) / 10000, error = function(e) NA_real_)
  recommended_buffer_m_for_ha(area_ha)
}

boundary_sf_interior <- function(boundary_4326, buffer_m = NULL) {
  if (is.null(boundary_4326) || !inherits(boundary_4326, "sf") || nrow(boundary_4326) < 1L) {
    return(NULL)
  }
  if (is.null(buffer_m)) buffer_m <- interior_buffer_m_for_boundary(NULL, boundary_4326)
  buffer_m <- abs(as.numeric(buffer_m)[1L])
  if (!is.finite(buffer_m) || buffer_m <= 0) buffer_m <- 20
  b_metric <- tryCatch(sf::st_transform(boundary_4326, 3857), error = function(e) NULL)
  if (is.null(b_metric)) return(NULL)
  b_in <- tryCatch(sf::st_buffer(b_metric, dist = -buffer_m), error = function(e) NULL)
  if (is.null(b_in) || all(sf::st_is_empty(b_in))) return(NULL)
  b_in <- tryCatch(sf::st_make_valid(b_in), error = function(e) b_in)
  a_in <- tryCatch(sum(sf::st_area(b_in)), error = function(e) 0)
  if (!is.finite(a_in) || a_in <= 0) return(NULL)
  tryCatch(sf::st_transform(b_in, 4326), error = function(e) NULL)
}

# Fast NDRE spread on subsampled interior pixels (Red Edge+NIR only; no SCL/focal/k-means).
fast_interior_ndre_spread <- function(v, min_px = 8L, max_cells = 500L) {
  v <- as.numeric(v)
  v <- v[is.finite(v) & v >= -0.1 & v <= 0.9]
  if (length(v) < min_px) return(NA_real_)
  max_cells <- suppressWarnings(as.integer(max_cells))
  if (length(max_cells) == 1L && !is.na(max_cells) && max_cells >= min_px && length(v) > max_cells) {
    set.seed(42L)
    v <- sample(v, max_cells)
  }
  qs <- stats::quantile(v, probs = c(0.15, 0.85), na.rm = TRUE, type = 7)
  as.numeric(qs[[2L]] - qs[[1L]])
}

stac_asset_href <- function(asset) {
  if (is.null(asset) || !is.list(asset)) return(NA_character_)
  h <- as.character(asset$href)
  h <- h[nzchar(h)]
  if (length(h)) return(h[[1L]])
  NA_character_
}

boundary_sf_4326 <- function(boundary_sf) {
  if (is.null(boundary_sf) || !inherits(boundary_sf, "sf") || nrow(boundary_sf) < 1L) return(NULL)
  b <- tryCatch(sf::st_make_valid(boundary_sf), error = function(e) boundary_sf)
  tryCatch(sf::st_transform(b, 4326), error = function(e) NULL)
}

sign_stac_feature_for_read <- function(feature) {
  if (is.null(feature) || !is.list(feature)) return(feature)
  re_href <- stac_asset_href(feature$assets[["B05"]])
  if (isTRUE(nzchar(re_href)) && grepl("blob.core.windows.net", re_href, fixed = TRUE) &&
      !grepl("[?&](se|sig)=", re_href)) {
    tryCatch({
      mini <- list(type = "FeatureCollection", features = list(feature))
      signed <- rstac::items_sign(mini, sign_fn = rstac::sign_planetary_computer())
      if (is.list(signed$features) && length(signed$features) >= 1L) {
        return(signed$features[[1L]])
      }
    }, error = function(e) feature)
  }
  feature
}

s2_asset_scale_offset <- function(asset) {
  rb <- asset[["raster:bands"]]
  if (!is.null(rb) && length(rb) > 0) {
    sc <- rb[[1]]$scale
    if (is.null(sc)) sc <- 0.0001
    of <- rb[[1]]$offset
    if (is.null(of)) of <- 0
  } else {
    sc <- 0.0001
    of <- 0
  }
  list(scale = as.numeric(sc), offset = as.numeric(of))
}

# NDRE = (NIR - RedEdge) / (NIR + RedEdge) — same as compute_vi_formulas() (B08 NIR, B05 Red Edge).
scene_ndre_pixels_from_feature <- function(
    feature,
    boundary_sf,
    max_cells = 500L,
    boundary_4326 = NULL,
    interior_buffer_m = NULL,
    skip_sign = FALSE
) {
  if (is.null(feature) || !is.list(feature) || is.null(boundary_sf) || !inherits(boundary_sf, "sf") || nrow(boundary_sf) < 1L) {
    return(NULL)
  }
  if (!isTRUE(skip_sign)) feature <- sign_stac_feature_for_read(feature)
  re_asset <- feature$assets[["B05"]]
  n_asset <- feature$assets[["B08"]]
  if (is.null(re_asset) || is.null(n_asset)) return(NULL)
  re_href <- stac_asset_href(re_asset)
  n_href <- stac_asset_href(n_asset)
  if (!isTRUE(nzchar(re_href)) || !isTRUE(nzchar(n_href))) return(NULL)

  min_px <- SENTINEL_NDRE_RANK_MIN_PIXELS()
  max_cells <- suppressWarnings(as.integer(max_cells))
  if (length(max_cells) != 1L || is.na(max_cells) || max_cells < min_px) {
    max_cells <- SENTINEL_NDRE_RANK_MAX_CELLS()
  }

  tryCatch({
    b <- boundary_4326
    if (is.null(b)) b <- boundary_sf_4326(boundary_sf)
    if (is.null(b)) return(NULL)
    re_r <- terra::rast(paste0("/vsicurl/", re_href))
    n_r <- terra::rast(paste0("/vsicurl/", n_href))
    re_c <- n_c <- ndre <- re_vals <- n_vals <- bound_proj <- NULL
    on.exit({
      for (obj in list(re_r, n_r, re_c, n_c, ndre, re_vals, n_vals, bound_proj)) {
        if (inherits(obj, "SpatRaster") || inherits(obj, "SpatVector")) {
          try(terra::rm(obj), silent = TRUE)
        }
      }
      try(terra::gc(), silent = TRUE)
    }, add = TRUE)
    b_wkt <- tryCatch(
      sf::st_as_text(sf::st_geometry(sf::st_make_valid(sf::st_union(b)))[[1]]),
      error = function(e) NA_character_
    )
    if (!isTRUE(nzchar(b_wkt))) return(NULL)
    bound_proj <- tryCatch(
      terra::project(terra::vect(b_wkt, crs = "EPSG:4326"), terra::crs(n_r)),
      error = function(e) NULL
    )
    if (is.null(bound_proj)) return(NULL)
    n_c <- terra::mask(terra::crop(n_r, bound_proj), bound_proj)
    re_c <- terra::resample(terra::mask(terra::crop(re_r, bound_proj), bound_proj), n_c, method = "bilinear")
    sre <- s2_asset_scale_offset(re_asset)
    sn <- s2_asset_scale_offset(n_asset)
    re_vals <- terra::clamp(re_c * sre$scale + sre$offset, 0, 1)
    n_vals <- terra::clamp(n_c * sn$scale + sn$offset, 0, 1)
    ndre <- (n_vals - re_vals) / (n_vals + re_vals)
    v <- as.numeric(terra::values(ndre))
    v <- v[is.finite(v) & v >= -0.1 & v <= 0.9]
    if (length(v) < min_px) return(NULL)
    if (length(v) > max_cells) {
      set.seed(42L)
      v <- sample(v, max_cells)
    }
    v
  }, error = function(e) NULL)
}

scene_mean_ndre_from_feature <- function(
    feature,
    boundary_sf,
    max_cells = 500L,
    boundary_4326 = NULL,
    interior_buffer_m = NULL,
    skip_sign = FALSE
) {
  v <- scene_ndre_pixels_from_feature(
    feature, boundary_sf,
    max_cells = max_cells,
    boundary_4326 = boundary_4326,
    interior_buffer_m = interior_buffer_m,
    skip_sign = skip_sign
  )
  if (is.null(v) || !length(v)) return(NA_real_)
  mean(v)
}

scene_ndre_spread_from_feature <- function(
    feature,
    boundary_sf,
    max_cells = 500L,
    boundary_4326 = NULL,
    interior_buffer_m = NULL,
    skip_sign = FALSE
) {
  v <- scene_ndre_pixels_from_feature(
    feature, boundary_sf,
    max_cells = max_cells,
    boundary_4326 = boundary_4326,
    interior_buffer_m = interior_buffer_m,
    skip_sign = skip_sign
  )
  if (is.null(v)) return(NA_real_)
  fast_interior_ndre_spread(v, min_px = SENTINEL_NDRE_RANK_MIN_PIXELS(), max_cells = length(v) + 1L)
}

build_sentinel_ndre_timeseries_df <- function(it_obj, boundary_sf, progress_fn = NULL) {
  features <- sort_stac_features_by_datetime(safe_features(it_obj), decreasing = FALSE)
  n_all <- length(features)
  if (n_all < 1L) return(NULL)
  cap <- SENTINEL_NDRE_TS_MAX_SCENES()
  evenly_spaced <- n_all > cap
  idx_use <- if (evenly_spaced) {
    sentinel_median_scene_pick_indices(n_all, cap)
  } else {
    seq_len(n_all)
  }
  max_cells <- SENTINEL_NDRE_RANK_MAX_CELLS()
  boundary_4326 <- boundary_sf_4326(boundary_sf)
  interior_buffer_m <- interior_buffer_m_for_boundary(boundary_sf, boundary_4326)
  it_sorted <- it_obj
  it_sorted$features <- features
  signed <- tryCatch(
    rstac::items_sign(it_sorted, sign_fn = rstac::sign_planetary_computer()),
    error = function(e) it_sorted
  )
  tick_every <- if (is_hosted_shiny()) 1L else 3L
  rows <- vector("list", length(idx_use))
  for (ii in seq_along(idx_use)) {
    i <- idx_use[ii]
    if (is.function(progress_fn)) {
      tryCatch(
        progress_fn(ii / length(idx_use), detail = paste0("Scene ", ii, " of ", length(idx_use))),
        error = function(e) NULL
      )
    }
    f <- signed$features[[i]]
    dt <- sentinel_feature_datetime(f)
    if (is.na(dt)) next
    mean_ndre <- scene_mean_ndre_from_feature(
      f, boundary_sf,
      max_cells = max_cells,
      boundary_4326 = boundary_4326,
      interior_buffer_m = interior_buffer_m,
      skip_sign = FALSE
    )
    cloud <- suppressWarnings(as.numeric(f$properties$`eo:cloud_cover`))
    rows[[ii]] <- data.frame(
      scene_date = as.Date(dt),
      mean_ndre = mean_ndre,
      cloud_pct = cloud,
      scene_label = paste0(format(as.Date(dt), "%Y-%m-%d"), " (cloud ", round(cloud, 1), "%)"),
      stringsAsFactors = FALSE
    )
    ndre_workflow_memory_tick(ii, tick_every)
  }
  ndre_workflow_memory_tick()
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  out <- dplyr::bind_rows(rows)
  out <- out[is.finite(out$mean_ndre) & !is.na(out$scene_date), , drop = FALSE]
  if (nrow(out) < 1L) return(NULL)
  out <- out[order(out$scene_date), , drop = FALSE]
  list(
    df = out,
    n_scenes_total = n_all,
    n_scenes_used = length(idx_use),
    evenly_spaced = evenly_spaced,
    scene_cap = cap,
    max_cells_per_scene = max_cells
  )
}

# Every calendar month (1st of month) between first and last scene date — for NDRE time series x-axis.
sentinel_ts_month_range <- function(xmin, xmax) {
  xmin <- as.Date(xmin)
  xmax <- as.Date(xmax)
  if (!length(xmin) || !length(xmax) || !is.finite(xmin) || !is.finite(xmax)) {
    return(list(breaks = xmin, xmin = xmin, xmax = xmax))
  }
  m_start <- as.Date(format(xmin, "%Y-%m-01"))
  m_end <- as.Date(format(xmax, "%Y-%m-01"))
  if (m_end < xmax) {
    m_end <- seq(m_end, by = "month", length.out = 2L)[2L]
  }
  breaks <- if (m_start <= m_end) seq(m_start, m_end, by = "month") else m_start
  x_end <- if (length(breaks)) {
    seq(max(breaks), by = "month", length.out = 2L)[2L] - 1L
  } else {
    xmax
  }
  list(breaks = breaks, xmin = m_start, xmax = x_end)
}

plot_sentinel_ndre_timeseries <- function(df, meta = NULL) {
  if (is.null(df) || nrow(df) < 1L) return(NULL)
  dr_lab <- if (!is.null(meta) && !is.null(meta$date_range)) meta$date_range else "search date range"
  xmin <- min(df$scene_date, na.rm = TRUE)
  xmax <- max(df$scene_date, na.rm = TRUE)
  xr <- sentinel_ts_month_range(xmin, xmax)
  n_months <- length(xr$breaks)
  lab_angle <- if (n_months > 14L) 90L else if (n_months > 8L) 45L else 0L
  lab_hjust <- if (lab_angle > 0L) 1 else 0.5
  lab_size <- if (n_months > 20L) 7.5 else if (n_months > 12L) 8.5 else 9

  scale_x <- ggplot2::scale_x_date(
    breaks = xr$breaks,
    minor_breaks = xr$breaks,
    date_labels = "%b %Y",
    expand = ggplot2::expansion(mult = c(0.02, 0.04)),
    limits = c(xr$xmin, xr$xmax),
    guide = ggplot2::guide_axis(angle = lab_angle, n.dodge = 1, check.overlap = FALSE)
  )

  y_rng <- range(df$mean_ndre, na.rm = TRUE)
  if (all(is.finite(y_rng)) && diff(y_rng) < 1e-6) {
    pad <- 0.02
    y_rng <- y_rng + c(-pad, pad)
  }

  ggplot2::ggplot(df, ggplot2::aes(x = scene_date, y = mean_ndre)) +
    ggplot2::geom_line(color = "#2c7fb8", linewidth = 1) +
    ggplot2::geom_point(
      shape = 21, size = 3.6, stroke = 1.15,
      fill = "#ffffff", color = "#1a6fa8"
    ) +
    scale_x +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.06, 0.1)), limits = y_rng) +
    ggplot2::coord_cartesian(clip = "on", xlim = c(xr$xmin, xr$xmax)) +
    ggplot2::labs(
      title = "Mean NDRE",
      subtitle = dr_lab,
      x = NULL,
      y = "Mean NDRE"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = "#123f6a", size = 15, margin = ggplot2::margin(b = 2)),
      plot.subtitle = ggplot2::element_text(color = "#5a7a96", size = 11.5, margin = ggplot2::margin(b = 8)),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "#e8eef5"),
      panel.grid.major.y = ggplot2::element_line(color = "#eef2f6"),
      panel.border = ggplot2::element_rect(color = "#dde6f0", fill = NA, linewidth = 0.45),
      axis.text.x = ggplot2::element_text(
        angle = lab_angle, hjust = lab_hjust, vjust = 1, size = lab_size, color = "#2a4a6b",
        margin = ggplot2::margin(t = 6)
      ),
      axis.text.y = ggplot2::element_text(color = "#2a4a6b"),
      plot.margin = ggplot2::margin(14, 18, if (lab_angle >= 90L) 36 else if (lab_angle > 0L) 30 else 26, 12)
    )
}

sentinel_median_scenes_caption <- function(n_used, n_total) {
  n_used <- suppressWarnings(as.integer(n_used))
  n_total <- suppressWarnings(as.integer(n_total))
  if (length(n_total) != 1L || is.na(n_total) || n_total < 1L) n_total <- n_used
  if (length(n_used) != 1L || is.na(n_used) || n_used < 1L) n_used <- n_total
  paste0(
    "Using ", n_used, " evenly spaced scene(s) out of ", n_total,
    " (total filtered) across the search list (oldest to newest):"
  )
}

normalize_stac_datetime <- function(datetime) {
  dt <- trimws(as.character(datetime))
  if (!length(dt) || !nzchar(dt[[1L]])) return("")
  dt <- dt[[1L]]
  parts <- strsplit(dt, "/", fixed = TRUE)[[1]]
  if (length(parts) != 2L) return(dt)
  fmt_part <- function(p, end = FALSE) {
    p <- trimws(p)
    if (grepl("T", p, fixed = TRUE)) return(p)
    if (end) paste0(p, "T23:59:59Z") else paste0(p, "T00:00:00Z")
  }
  paste(fmt_part(parts[[1L]], end = FALSE), fmt_part(parts[[2L]], end = TRUE), sep = "/")
}

run_sentinel_stac_search <- function(bbox, datetime, cloud_lim, max_items = NULL) {
  bbox <- suppressWarnings(as.numeric(bbox))
  if (length(bbox) != 4L || !all(is.finite(bbox)) || bbox[3] <= bbox[1] || bbox[4] <= bbox[2]) {
    stop("Invalid boundary extent for Sentinel search. Redraw or re-upload the boundary.")
  }
  datetime <- normalize_stac_datetime(datetime)
  if (!nzchar(datetime) || !grepl("/", datetime, fixed = TRUE)) {
    stop("Invalid date range for Sentinel search.")
  }
  page_limit <- SENTINEL_STAC_PAGE_LIMIT()
  if (is.null(max_items)) max_items <- SENTINEL_STAC_SEARCH_MAX_ITEMS()
  max_items <- suppressWarnings(as.integer(max_items))
  if (length(max_items) != 1L || is.na(max_items) || max_items < 1L) {
    max_items <- SENTINEL_STAC_SEARCH_MAX_ITEMS()
  }
  cloud_lim <- suppressWarnings(as.numeric(cloud_lim))
  if (length(cloud_lim) != 1L || !is.finite(cloud_lim)) cloud_lim <- 10

  it_obj <- tryCatch({
    s <- rstac::stac(PC_STAC_URL)
    req <- rstac::stac_search(
      s,
      collections = "sentinel-2-l2a",
      bbox = bbox,
      datetime = datetime,
      limit = page_limit
    )
    rstac::get_request(req)
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("text/plain", msg, fixed = TRUE)) {
      stop(
        paste0(
          "Planetary Computer STAC search timed out or returned a non-JSON error. ",
          "Try a shorter date range (2–4 weeks) or a smaller boundary, then search again."
        ),
        call. = FALSE
      )
    }
    stop(msg, call. = FALSE)
  })

  if (!is.list(it_obj) || is.null(it_obj$features) || !is.list(it_obj$features)) {
    stop("Sentinel search returned an unexpected response format from STAC.")
  }

  all_feats <- it_obj$features
  pages_fetched <- 1L
  stac_truncated <- FALSE
  # Only follow STAC "next" when the first page is full (same as a single-page search when few scenes).
  while (length(all_feats) >= page_limit && length(all_feats) < max_items) {
    next_href <- stac_link_next_href(it_obj)
    if (!nzchar(next_href %||% "")) break
    it_next <- tryCatch(rstac::get_request(next_href), error = function(e) NULL)
    if (is.null(it_next) || is.null(it_next$features) || !length(it_next$features)) break
    it_obj <- it_next
    pages_fetched <- pages_fetched + 1L
    all_feats <- c(all_feats, it_next$features)
    if (length(all_feats) >= max_items) {
      all_feats <- all_feats[seq_len(max_items)]
      stac_truncated <- TRUE
      break
    }
    if (pages_fetched >= 50L) {
      stac_truncated <- TRUE
      break
    }
    if (length(it_next$features) < page_limit) break
  }

  n_stac_fetched <- length(all_feats)
  cc <- vapply(all_feats, function(f) {
    suppressWarnings(as.numeric(f$properties$`eo:cloud_cover`))
  }, numeric(1))
  keep <- is.na(cc) | cc <= cloud_lim
  feats <- all_feats[keep]
  if (!length(feats)) {
    it_obj$features <- list()
    it_obj$`app:search_stats` <- list(
      stac_fetched = n_stac_fetched,
      before_cloud = n_stac_fetched,
      after_cloud = 0L,
      returned = 0L,
      cloud_limit = cloud_lim,
      page_limit = page_limit,
      max_items_cap = max_items,
      pages_fetched = pages_fetched,
      stac_truncated = stac_truncated,
      sort_order = "datetime_asc",
      time_subsampled = FALSE
    )
    return(it_obj)
  }

  feats <- sort_stac_features_by_datetime(feats, decreasing = FALSE)
  n_after_cloud <- length(feats)

  it_obj$features <- feats
  it_obj$`app:search_stats` <- list(
    stac_fetched = n_stac_fetched,
    before_cloud = n_stac_fetched,
    after_cloud = n_after_cloud,
    returned = n_after_cloud,
    cloud_limit = cloud_lim,
    page_limit = page_limit,
    max_items_cap = max_items,
    pages_fetched = pages_fetched,
    stac_truncated = stac_truncated,
    sort_order = "datetime_asc",
    time_subsampled = FALSE
  )
  it_obj
}

ZONE_MAP_OPACITY <- 0.8

# ColorBrewer Set2–inspired qualitative colors (readable on satellite basemaps).
ZONE_MAP_PALETTE <- c(
  "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3",
  "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3",
  "#1B9E77", "#D95F02", "#7570B3", "#E7298A"
)

zone_map_colors <- function(n) {
  n <- max(1L, as.integer(n))
  if (n <= length(ZONE_MAP_PALETTE)) {
    ZONE_MAP_PALETTE[seq_len(n)]
  } else {
    grDevices::colorRampPalette(ZONE_MAP_PALETTE)(n)
  }
}

# --- THEME (bslib) ---
app_theme <- bs_theme(version = 5, bootswatch = "minty")

standard_vi_base_names <- function() {
  c("NDVI", "NDRE", "GNDVI", "OSAVI", "CIre", "VARI")
}

standard_elevation_layer_names <- function() {
  c("DEM", "Slope", "Aspect", "TPI", "TWI")
}

normalize_imagery_vi_names <- function(nms) {
  nms <- as.character(nms)
  nms <- nms[nzchar(nms)]
  if (!length(nms)) return(character())
  sub("_median$", "", nms, perl = TRUE)
}

format_layer_list_for_modal <- function(items) {
  items <- as.character(items)
  items <- items[nzchar(items)]
  if (!length(items)) return("—")
  paste(items, collapse = ", ")
}

resolve_stack_band <- function(r_stack, candidates) {
  if (is.null(r_stack) || length(r_stack) < 1L) return(NULL)
  nms <- names(r_stack)
  if (is.null(nms)) return(NULL)
  hit <- candidates[candidates %in% nms]
  if (length(hit)) return(r_stack[[hit[[1L]]]])
  NULL
}

compute_standard_vis <- function(r_stack, band_suffix = "") {
  sfx <- as.character(band_suffix)
  NIR <- resolve_stack_band(r_stack, c(paste0("NIR", sfx), "NIR"))
  R <- resolve_stack_band(r_stack, c(paste0("Red", sfx), "Red"))
  G <- resolve_stack_band(r_stack, c(paste0("Green", sfx), "Green"))
  RE <- resolve_stack_band(r_stack, c(paste0("RedEdge", sfx), "RedEdge", paste0("Red_Edge", sfx)))
  B <- resolve_stack_band(r_stack, c(paste0("Blue", sfx), "Blue"))
  missing <- c()
  if (is.null(NIR)) missing <- c(missing, "NIR")
  if (is.null(R)) missing <- c(missing, "Red")
  if (is.null(G)) missing <- c(missing, "Green")
  if (is.null(RE)) missing <- c(missing, "RedEdge")
  if (is.null(B)) missing <- c(missing, "Blue")
  if (length(missing)) {
    stop(paste("Missing required band(s) for VI calculation:", paste(unique(missing), collapse = ", ")))
  }
  list(
    NDVI = (NIR - R) / (NIR + R),
    NDRE = (NIR - RE) / (NIR + RE),
    GNDVI = (NIR - G) / (NIR + G),
    OSAVI = (NIR - R) / (NIR + R + 0.16),
    CIre = (NIR / RE) - 1,
    VARI = (G - R) / (G + R - B)
  )
}

vi_download_output_id <- function(prefix, base_nm) {
  slug <- tolower(base_nm)
  if (nzchar(prefix)) paste0("download_", prefix, "_", slug) else paste0("download_", slug)
}

build_vi_download_ui <- function(id_prefix = "", name_suffix = "") {
  div(
    hr(),
    h4("Download VIs"),
    div(
      style = "display:flex; flex-wrap:wrap; gap:6px;",
      lapply(standard_vi_base_names(), function(b) {
        downloadButton(
          vi_download_output_id(id_prefix, b),
          paste0("Download ", b, name_suffix, " TIFF"),
          class = "btn-sm"
        )
      })
    )
  )
}

# Lean default covariates for sampling: fewer correlated VIs → less redundant cLHS constraint.
default_sampling_covariate_selection <- function(layer_names) {
  if (!length(layer_names)) return(character(0))
  vi_layers <- layer_names[grepl("_VI_", layer_names, fixed = TRUE)]
  chosen_vi <- character(0)
  if (length(vi_layers)) {
    for (base in c("NDVI", "NDRE", "GNDVI", "OSAVI")) {
      hit <- vi_layers[grepl(paste0("_VI_", base), vi_layers, fixed = TRUE)]
      if (length(hit)) chosen_vi <- c(chosen_vi, hit[[1L]])
    }
    if (!length(chosen_vi)) {
      chosen_vi <- vi_layers[seq_len(min(4L, length(vi_layers)))]
    }
  }
  terrain_pick <- intersect(c("Elevation_DEM", "Elevation_Slope", "Elevation_TWI"), layer_names)
  remaining <- setdiff(layer_names, c(vi_layers, terrain_pick))
  other_pick <- head(remaining, 2L)
  out <- unique(c(chosen_vi, terrain_pick, other_pick))
  if (length(out) < 1L) layer_names[seq_len(min(6L, length(layer_names)))] else out
}

sampling_covariate_preset_layers <- function(layer_names, preset = c("lean", "scout", "fertility", "full")) {
  preset <- match.arg(preset)
  if (!length(layer_names)) return(character(0))
  if (preset == "full") return(layer_names)
  if (preset == "lean") return(default_sampling_covariate_selection(layer_names))
  vi_pick <- function(bases) {
    out <- character(0)
    for (b in bases) {
      hit <- layer_names[grepl(paste0("_VI_", b), layer_names, fixed = TRUE)]
      if (length(hit)) out <- c(out, hit[[1L]])
    }
    unique(out)
  }
  if (preset == "scout") {
    out <- unique(c(vi_pick("NDVI"), intersect("Elevation_DEM", layer_names)))
    if (length(out)) return(out)
    return(default_sampling_covariate_selection(layer_names))
  }
  if (preset == "fertility") {
    out <- unique(c(
      vi_pick(c("NDVI", "NDRE", "GNDVI")),
      intersect(c("Elevation_TWI", "Elevation_DEM"), layer_names)
    ))
    if (length(out)) return(out)
    return(default_sampling_covariate_selection(layer_names))
  }
  layer_names
}

points_to_report_df <- function(pts) {
  if (is.null(pts) || !inherits(pts, "sf") || nrow(pts) < 1L) return(NULL)
  p <- tryCatch(sf::st_transform(pts, 4326), error = function(e) pts)
  coords <- tryCatch(sf::st_coordinates(p), error = function(e) NULL)
  if (is.null(coords) || nrow(coords) < 1L) return(NULL)
  idv <- if ("ID" %in% names(p)) p$ID else seq_len(nrow(p))
  data.frame(
    ID = idv,
    Longitude = round(coords[, 1], 6),
    Latitude = round(coords[, 2], 6),
    stringsAsFactors = FALSE
  )
}

format_sentinel_report_lines <- function(meta) {
  if (is.null(meta) || !length(meta)) return(character(0))
  mode_lab <- if (identical(meta$mode, "median")) {
    "Median composite (multi-scene pixel stack)"
  } else {
    "Single scene (one acquisition)"
  }
  out <- c(paste0("Sentinel imagery: ", mode_lab))
  if (!is.null(meta$retrieved_at)) {
    out <- c(out, paste0("Sentinel retrieved on: ", format(meta$retrieved_at, "%Y-%m-%d %H:%M")))
  }
  if (!is.null(meta$scene_count)) {
    out <- c(out, paste0("Scenes in stack: ", meta$scene_count))
  }
  lbls <- meta$scene_labels
  if (!is.null(lbls) && length(lbls)) {
    nshow <- min(4L, length(lbls))
    for (i in seq_len(nshow)) {
      out <- c(out, paste0("  • ", lbls[[i]]))
    }
    if (length(lbls) > nshow) {
      out <- c(out, paste0("  • ... +", length(lbls) - nshow, " more scene(s)"))
    }
  }
  out
}

subsample_dataframe_rows <- function(df, max_rows = 80000L) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) < 1L) return(df)
  max_rows <- as.integer(max_rows)
  if (length(max_rows) != 1L || is.na(max_rows) || max_rows < 1L) return(df)
  if (nrow(df) <= max_rows) return(df)
  df[sample.int(nrow(df), max_rows), , drop = FALSE]
}

subsample_paired_rows <- function(cov_df, xy_df, max_rows = 80000L) {
  if (is.null(cov_df) || !is.data.frame(cov_df) || nrow(cov_df) < 1L) {
    return(list(cov = cov_df, xy = xy_df))
  }
  max_rows <- as.integer(max_rows)
  if (length(max_rows) != 1L || is.na(max_rows) || max_rows < 1L) {
    return(list(cov = cov_df, xy = xy_df))
  }
  if (nrow(cov_df) <= max_rows) return(list(cov = cov_df, xy = xy_df))
  idx <- sample.int(nrow(cov_df), max_rows)
  list(cov = cov_df[idx, , drop = FALSE], xy = xy_df[idx, , drop = FALSE])
}

mean_nearest_neighbor_distance <- function(xy) {
  if (is.null(xy) || nrow(xy) < 2L) return(NA_real_)
  pts <- as.matrix(xy[, c("x", "y"), drop = FALSE])
  d <- stats::dist(pts)
  if (!is.matrix(d)) d <- as.matrix(d)
  diag(d) <- Inf
  mean(apply(d, 1, min, na.rm = TRUE), na.rm = TRUE)
}

reference_mean_nn_regular_grid <- function(n, bbox) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n) || n < 2L) return(NA_real_)
  w <- bbox["xmax"] - bbox["xmin"]
  h <- bbox["ymax"] - bbox["ymin"]
  if (!is.finite(w) || !is.finite(h) || w <= 0 || h <= 0) return(NA_real_)
  ncol <- max(1L, ceiling(sqrt(n * w / h)))
  nrow <- max(1L, ceiling(n / ncol))
  xs <- seq(bbox["xmin"], bbox["xmax"], length.out = ncol)
  ys <- seq(bbox["ymin"], bbox["ymax"], length.out = nrow)
  grid <- expand.grid(x = xs, y = ys)
  grid <- grid[seq_len(min(n, nrow(grid))), , drop = FALSE]
  mean_nearest_neighbor_distance(grid)
}

RECOMMENDED_GENERATION_REPEATS <- function() {
  12L
}

# Extra replicates when user generates from Explore more (current sidebar settings).
GENERATION_CURRENT_SETTINGS_EXTRA_REPEATS <- function() {
  6L
}

GENERATION_FIELD_COVERAGE_TARGET_PCT <- function() {
  60
}

GENERATION_POLISH_MAX_POP <- function() 60000L
GENERATION_POLISH_MAX_N <- function() 55L

# Prefer replicates at/above target % field coverage; else highest coverage wins.
spread_pick_beats_best <- function(
  fc_new,
  sc_new,
  fc_best,
  sc_best,
  best_meets_target,
  target_pct = GENERATION_FIELD_COVERAGE_TARGET_PCT()
) {
  meets <- is.finite(fc_new) && fc_new >= target_pct
  if (meets) {
    if (!isTRUE(best_meets_target)) return(TRUE)
    if (!is.finite(fc_best) || fc_new > fc_best + 1e-9) return(TRUE)
    if (abs(fc_new - fc_best) < 1e-9 && is.finite(sc_new) && is.finite(sc_best) && sc_new > sc_best) {
      return(TRUE)
    }
    return(FALSE)
  }
  if (isTRUE(best_meets_target)) return(FALSE)
  if (!is.finite(fc_new)) {
    return(is.finite(sc_new) && is.finite(sc_best) && sc_new > sc_best)
  }
  if (!is.finite(fc_best) || fc_new > fc_best + 1e-9) return(TRUE)
  if (
    is.finite(fc_new) && is.finite(fc_best) &&
      abs(fc_new - fc_best) < 1e-9 &&
      is.finite(sc_new) && is.finite(sc_best) && sc_new > sc_best
  ) {
    return(TRUE)
  }
  FALSE
}

safe_pop_indices <- function(idx, pop_n) {
  idx <- as.integer(idx)
  pop_n <- as.integer(pop_n)
  if (!length(idx) || !is.finite(pop_n) || pop_n < 1L) return(integer(0))
  unique(idx[is.finite(idx) & idx >= 1L & idx <= pop_n])
}

median_nn_for_indices <- function(idx, pop_xy) {
  idx <- safe_pop_indices(idx, nrow(pop_xy))
  n <- length(idx)
  if (n < 2L || is.null(pop_xy) || nrow(pop_xy) < 2L) return(NA_real_)
  xy <- pop_xy[idx, c("x", "y"), drop = FALSE]
  if (!all(is.finite(xy$x)) || !all(is.finite(xy$y))) return(NA_real_)
  if (n <= 80L) {
    dmat <- tryCatch(
      {
        m <- as.matrix(stats::dist(xy))
        diag(m) <- Inf
        m
      },
      error = function(e) NULL
    )
    if (is.null(dmat)) return(NA_real_)
    nn <- apply(dmat, 1, min, na.rm = TRUE)
  } else {
    nn <- vapply(seq_len(n), function(i) {
      d <- sqrt((xy$x - xy$x[i])^2 + (xy$y - xy$y[i])^2)
      d[i] <- Inf
      min(d, na.rm = TRUE)
    }, numeric(1))
  }
  nn <- nn[is.finite(nn) & nn > 0]
  if (length(nn) < 2L) return(NA_real_)
  stats::median(nn)
}

# Median NN vs sqrt(area/n) — same definition as Sampling Quality Dashboard "Field coverage spread".
field_coverage_spread_pct <- function(idx, pop_xy, area_m2) {
  idx <- safe_pop_indices(idx, if (is.null(pop_xy)) 0L else nrow(pop_xy))
  n <- length(idx)
  if (n < 2L || is.null(pop_xy) || nrow(pop_xy) < 2L) return(NA_real_)
  if (!is.finite(area_m2) || area_m2 <= 0) return(NA_real_)
  nn_med <- median_nn_for_indices(idx, pop_xy)
  if (!is.finite(nn_med)) return(NA_real_)
  expected <- sqrt(area_m2 / n)
  if (!is.finite(expected) || expected <= 0) return(NA_real_)
  100 * nn_med / expected
}

# One pass per replicate: field coverage (metric XY), spread index (grid XY), composite pick score.
spread_replicate_metrics <- function(idx, pop_xy_grid, pop_xy_metric, n_target, area_m2) {
  pop_n <- if (is.null(pop_xy_grid)) 0L else nrow(pop_xy_grid)
  idx <- safe_pop_indices(idx, pop_n)
  if (length(idx) < 1L) {
    return(list(pick_score = NA_real_, field_coverage_pct = NA_real_, even_spread_idx = NA_real_))
  }
  fc <- NA_real_
  if (!is.null(pop_xy_metric) && nrow(pop_xy_metric) >= 2L) {
    fc <- field_coverage_spread_pct(idx, pop_xy_metric, area_m2)
  }
  es <- NA_real_
  if (!is.null(pop_xy_grid) && nrow(pop_xy_grid) >= 2L) {
    es <- even_spread_quality_score(idx, pop_xy_grid, n_target = n_target)
  }
  fc_norm <- if (is.finite(fc)) min(1.25, fc / 100) else 0
  es_norm <- if (is.finite(es)) es else 0
  pick_score <- if (is.finite(fc)) {
    0.85 * fc_norm + 0.15 * es_norm
  } else {
    es_norm
  }
  target_pct <- GENERATION_FIELD_COVERAGE_TARGET_PCT()
  if (is.finite(fc) && fc >= target_pct) {
    pick_score <- pick_score + 0.08 + (fc - target_pct) / 200
  }
  list(
    pick_score = pick_score,
    field_coverage_pct = fc,
    even_spread_idx = es
  )
}

generation_spread_pick_score <- function(idx, pop_xy, n_target, area_m2) {
  spread_replicate_metrics(idx, pop_xy, pop_xy, n_target, area_m2)$pick_score
}

generation_polish_allowed <- function(pop_n, n_use) {
  pop_n <- suppressWarnings(as.integer(pop_n))
  n_use <- suppressWarnings(as.integer(n_use))
  is.finite(pop_n) && is.finite(n_use) &&
    pop_n >= 3L && pop_n <= GENERATION_POLISH_MAX_POP() &&
    n_use >= 3L && n_use <= GENERATION_POLISH_MAX_N()
}

# Swap a few clustered points for better-spaced cells (keeps method design mostly intact).
polish_field_coverage_indices <- function(
  idx,
  pop_xy,
  pop_n,
  area_m2,
  target_pct = GENERATION_FIELD_COVERAGE_TARGET_PCT(),
  n_use = NULL,
  aggressive = FALSE
) {
  tryCatch({
    pop_n <- as.integer(pop_n)
    if (!is.finite(pop_n) || pop_n < 1L) return(idx)
    n_use <- suppressWarnings(as.integer(n_use))
    if (!is.finite(n_use) || n_use < 1L) n_use <- length(idx)
    if (!generation_polish_allowed(pop_n, n_use)) return(idx)
    idx <- compare_ensure_n_indices(idx, pop_n, n_use)
    n <- length(idx)
    if (n < 3L || !is.finite(area_m2) || area_m2 <= 0) return(idx)
    max_swaps <- if (isTRUE(aggressive)) {
      max(3L, min(8L, n - 1L, ceiling(0.3 * n)))
    } else {
      max(2L, min(5L, n - 1L, ceiling(0.2 * n)))
    }
    if (!length(setdiff(seq_len(pop_n), idx))) return(idx)
    for (swap_try in seq_len(max_swaps)) {
      cur <- field_coverage_spread_pct(idx, pop_xy, area_m2)
      if (is.finite(cur) && cur >= target_pct) break
      xy <- pop_xy[idx, c("x", "y"), drop = FALSE]
      if (nrow(xy) < 2L) break
      min_nn <- if (n <= 80L) {
        dmat <- tryCatch(
          {
            m <- as.matrix(stats::dist(xy))
            diag(m) <- Inf
            m
          },
          error = function(e) NULL
        )
        if (is.null(dmat)) break
        apply(dmat, 1, min, na.rm = TRUE)
      } else {
        vapply(seq_len(n), function(i) {
          d <- sqrt((xy$x - xy$x[i])^2 + (xy$y - xy$y[i])^2)
          d[i] <- Inf
          min(d, na.rm = TRUE)
        }, numeric(1))
      }
      if (!length(min_nn) || !any(is.finite(min_nn))) break
      worst_slot <- which.min(min_nn)[1L]
      rem <- setdiff(seq_len(pop_n), idx)
      if (!length(rem)) break
      cand_n <- min(
        length(rem),
        if (isTRUE(aggressive)) max(36L, 5L * n) else max(24L, 3L * n)
      )
      cand <- if (length(rem) <= cand_n) rem else sample(rem, cand_n)
      best_pct <- cur
      best_j <- NA_integer_
      for (j in cand) {
        trial <- idx
        trial[worst_slot] <- j
        pct <- field_coverage_spread_pct(trial, pop_xy, area_m2)
        if (is.finite(pct) && (is.na(best_pct) || pct > best_pct + 0.5)) {
          best_pct <- pct
          best_j <- j
        }
      }
      if (is.na(best_j)) break
      idx[worst_slot] <- best_j
    }
    compare_ensure_n_indices(idx, pop_n, n_use)
  }, error = function(e) idx)
}

sampling_area_m2_from_boundary <- function(boundary_sf, analysis_crs = "+proj=longlat +datum=WGS84") {
  if (is.null(boundary_sf) || !nrow(boundary_sf)) return(NA_real_)
  tryCatch({
    b <- sf::st_transform(boundary_sf, analysis_crs)
    b <- b[!sf::st_is_empty(b), , drop = FALSE]
    if (!nrow(b)) return(NA_real_)
    as.numeric(sum(sf::st_area(b)))
  }, error = function(e) NA_real_)
}

# Higher = better geographic spread: coverage (NN + grid cells) + evenness of point spacing.
even_spread_quality_score <- function(idx, pop_xy, n_target = length(idx)) {
  idx <- safe_pop_indices(idx, if (is.null(pop_xy)) 0L else nrow(pop_xy))
  n <- length(idx)
  if (n < 1L || is.null(pop_xy) || nrow(pop_xy) < 1L) return(NA_real_)
  if (n == 1L) return(1)
  if (nrow(pop_xy) < 2L) return(NA_real_)
  bbox <- c(
    xmin = min(pop_xy$x, na.rm = TRUE),
    xmax = max(pop_xy$x, na.rm = TRUE),
    ymin = min(pop_xy$y, na.rm = TRUE),
    ymax = max(pop_xy$y, na.rm = TRUE)
  )
  coverage <- spatial_balance_score(idx, pop_xy, bbox, n_target = n_target)
  if (!is.finite(coverage)) coverage <- 0
  xy <- pop_xy[idx, c("x", "y"), drop = FALSE]
  nn <- if (n <= 80L) {
    dmat <- as.matrix(stats::dist(xy))
    diag(dmat) <- Inf
    apply(dmat, 1, min, na.rm = TRUE)
  } else {
    vapply(seq_len(n), function(i) {
      d <- sqrt((xy$x - xy$x[i])^2 + (xy$y - xy$y[i])^2)
      d[i] <- Inf
      min(d, na.rm = TRUE)
    }, numeric(1))
  }
  nn <- nn[is.finite(nn) & nn > 0]
  evenness <- if (length(nn) >= 2L && is.finite(mean(nn)) && mean(nn) > 0) {
    1 / (1 + stats::sd(nn) / mean(nn))
  } else {
    0.5
  }
  as.numeric(0.62 * coverage + 0.38 * evenness)
}

# Indices used for spread scoring (trim only; no random filler).
spread_score_indices <- function(idx, pop_n, n_need) {
  idx <- as.integer(unique(idx[idx >= 1L & idx <= pop_n]))
  n_need <- as.integer(n_need)
  if (length(idx) >= n_need) return(idx[seq_len(n_need)])
  idx
}

spatial_balance_score <- function(idx, pop_xy, bbox, n_target = length(idx)) {
  idx <- safe_pop_indices(idx, if (is.null(pop_xy)) 0L else nrow(pop_xy))
  n <- length(idx)
  if (n < 2L || is.null(pop_xy) || nrow(pop_xy) < 2L) return(NA_real_)
  xy <- pop_xy[idx, c("x", "y"), drop = FALSE]
  mean_nn <- mean_nearest_neighbor_distance(xy)
  ref_nn <- reference_mean_nn_regular_grid(n, bbox)
  nn_score <- if (is.finite(mean_nn) && is.finite(ref_nn) && ref_nn > 0) {
    min(1, max(0, mean_nn / ref_nn))
  } else NA_real_
  n_gr <- max(2L, ceiling(sqrt(max(as.integer(n_target), 2L) * 2)))
  w <- bbox["xmax"] - bbox["xmin"]
  h <- bbox["ymax"] - bbox["ymin"]
  grid_score <- NA_real_
  if (is.finite(w) && is.finite(h) && w > 0 && h > 0) {
    gx <- cut(
      pop_xy$x,
      breaks = seq(bbox["xmin"], bbox["xmax"], length.out = n_gr + 1L),
      labels = FALSE, include.lowest = TRUE
    )
    gy <- cut(
      pop_xy$y,
      breaks = seq(bbox["ymin"], bbox["ymax"], length.out = n_gr + 1L),
      labels = FALSE, include.lowest = TRUE
    )
    cell_id <- gx + (gy - 1L) * n_gr
    occupied <- length(unique(cell_id[idx]))
    target_cells <- min(n_gr * n_gr, max(as.integer(n_target), 1L))
    grid_score <- min(1, max(0, occupied / target_cells))
  }
  parts <- c(nn_score, grid_score)
  parts <- parts[is.finite(parts)]
  if (!length(parts)) return(NA_real_)
  mean(parts)
}

systematic_spread_indices <- function(n, pop_xy, rng_seed = NULL) {
  n <- as.integer(n)
  pop_n <- nrow(pop_xy)
  if (length(n) != 1L || is.na(n) || n < 1L) return(integer(0))
  if (n >= pop_n) return(seq_len(pop_n))
  if (!is.null(rng_seed)) set.seed(as.integer(rng_seed))
  # Spatially stratified spread: one population cell nearest each k-means centroid (reproducible with rng_seed).
  nstart_use <- min(15L, max(3L, as.integer(ceiling(sqrt(n)))))
  km <- tryCatch(
    stats::kmeans(pop_xy, centers = n, iter.max = 50L, nstart = nstart_use, algorithm = "Hartigan-Wong"),
    error = function(e) NULL
  )
  idx <- integer(0)
  if (!is.null(km) && length(km$cluster) == pop_n) {
    idx <- vapply(seq_len(n), function(k) {
      inside <- which(km$cluster == k)
      if (!length(inside)) return(NA_integer_)
      cx <- km$centers[k, 1L]
      cy <- km$centers[k, 2L]
      dx <- pop_xy$x[inside] - cx
      dy <- pop_xy$y[inside] - cy
      inside[which.min(dx * dx + dy * dy)]
    }, integer(1))
    idx <- unique(idx[is.finite(idx) & idx >= 1L & idx <= pop_n])
  }
  if (length(idx) < n) {
    rx <- rank(pop_xy$x, ties.method = "first")
    ry <- rank(pop_xy$y, ties.method = "first")
    ord <- order(rx + ry, rx, ry)
    pick <- unique(as.integer(round(seq(1L, length(ord), length.out = n))))
    pick <- pick[pick >= 1L & pick <= length(ord)]
    if (length(pick) < n) {
      extras <- setdiff(seq_len(length(ord)), pick)
      need <- n - length(pick)
      if (need > 0L && length(extras)) {
        pick <- c(pick, extras[seq_len(min(need, length(extras)))])
      }
    }
    fb <- ord[pick[seq_len(min(n, length(pick)))]]
    idx <- unique(c(idx, fb))
  }
  compare_ensure_n_indices(as.integer(idx), pop_n, n)
}

# Fingerprint for caching WSS zone recommendations (same covariate matrix => same k).
zone_cov_fingerprint <- function(cov_df) {
  if (is.null(cov_df) || ncol(cov_df) < 1L) return("")
  paste0(nrow(cov_df), ":", ncol(cov_df), ":", paste(sort(names(cov_df)), collapse = "|"))
}

# WSS elbow zone count from a numeric covariate matrix (deterministic subsample + k-means seed).
wss_recommend_zone_count_from_cov_df <- function(cov_df, max_cells = 5000L) {
  cov_df <- cov_df[, sapply(cov_df, is.numeric), drop = FALSE]
  cov_df <- cov_df[, vapply(cov_df, function(v) stats::sd(v, na.rm = TRUE) > 0, logical(1)), drop = FALSE]
  cov_df <- cov_df[stats::complete.cases(cov_df), , drop = FALSE]
  if (ncol(cov_df) < 1L || nrow(cov_df) < 10L) {
    return(list(ok = FALSE, reason = "Not enough valid covariate data for WSS-based recommendation."))
  }
  n_all <- nrow(cov_df)
  zone_seed <- as.integer((104729L + n_all * 31L + ncol(cov_df) * 131L) %% 2147483647L)
  if (!is.finite(zone_seed) || is.na(zone_seed) || zone_seed < 1L) zone_seed <- 104729L
  cov_work <- cov_df
  if (n_all > max_cells) {
    set.seed(zone_seed)
    cov_work <- cov_work[sample.int(n_all, max_cells), , drop = FALSE]
  }
  x <- scale(cov_work)
  k_max <- min(12L, nrow(x) - 1L)
  if (k_max < 2L) {
    return(list(ok = FALSE, reason = "Insufficient data to evaluate multiple zone counts."))
  }
  k_seq <- seq.int(1L, k_max)
  set.seed(zone_seed + 17L)
  wss <- vapply(k_seq, function(k) {
    stats::kmeans(x, centers = k, nstart = 5, iter.max = 100)$tot.withinss
  }, numeric(1))
  if (length(k_seq) == 2L) {
    rec <- 2L
  } else {
    x1 <- k_seq[1]; y1 <- wss[1]
    x2 <- k_seq[length(k_seq)]; y2 <- wss[length(k_seq)]
    denom <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
    if (denom == 0) {
      rec <- 2L
    } else {
      d <- abs((y2 - y1) * k_seq - (x2 - x1) * wss + x2 * y1 - y2 * x1) / denom
      d[1] <- NA_real_
      rec <- as.integer(k_seq[which.max(d)])
      if (is.na(rec) || rec < 2L) rec <- 2L
    }
  }
  list(
    ok = TRUE,
    recommended = rec,
    k_seq = k_seq,
    wss = as.numeric(wss),
    k_max = k_max,
    n_all = n_all,
    subsampled = n_all > max_cells,
    max_cells = max_cells,
    zone_seed = zone_seed,
    fingerprint = zone_cov_fingerprint(cov_df)
  )
}

zone_wss_recommendation_message <- function(res) {
  if (!isTRUE(res$ok)) return(as.character(res$reason))
  paste0(
    "Recommended zones: ", res$recommended,
    " (WSS elbow, k=1..", res$k_max,
    if (isTRUE(res$subsampled)) paste0("; ", res$max_cells, " of ", res$n_all, " cells).") else ")."
  )
}

zone_wss_recommend_info_ui <- function(msg) {
  msg <- trimws(as.character(msg)[1L])
  if (!nzchar(msg)) {
    return(tags$p(
      class = "text-muted",
      style = "font-size:11px; margin:4px 0 0 0;",
      "Click Recommend Zones to set zone count (WSS elbow)."
    ))
  }
  if (grepl("^Recommended zones:", msg, fixed = FALSE)) {
    return(tags$div(
      style = paste0(
        "margin:8px 0 10px 0; padding:9px 11px; border:1px solid #7ec49a; border-radius:10px;",
        "background:linear-gradient(120deg,#f0faf4 0%,#e3f5ea 100%);"
      ),
      tags$div(style = "font-size:13px; font-weight:700; color:#1a6b3f; line-height:1.35;", msg)
    ))
  }
  tags$p(class = "text-muted", style = "font-size:11px; margin:6px 0 0 0; line-height:1.35;", msg)
}

# Numeric covariate column names for distribution plots (shared sampling UI).
# Safe positive integer sample size (avoids if(NA) errors in zone sampling).
safe_sample_n_points <- function(n, max_n = NULL) {
  n <- suppressWarnings(as.integer(n))
  if (length(n) != 1L || is.na(n) || n < 1L) n <- 1L
  if (!is.null(max_n)) {
    mx <- suppressWarnings(as.integer(max_n))
    if (!is.na(mx) && mx >= 1L) n <- min(n, mx)
  }
  n
}

# Single WSS zone count or NA (avoids if(logical(0)) when reactiveVal is NULL).
read_zone_k_scalar <- function(x) {
  if (is.null(x) || length(x) != 1L) return(NA_integer_)
  kz <- suppressWarnings(as.integer(x))
  if (length(kz) != 1L || is.na(kz)) NA_integer_ else kz
}

zone_layers_fingerprint <- function(layers) {
  if (is.null(layers) || !length(layers)) return("")
  paste(sort(as.character(layers)), collapse = "|")
}

zone_boundary_fingerprint <- function(b) {
  if (is.null(b) || !inherits(b, "sf") || nrow(b) < 1L) {
    return("no_aoi")
  }
  bb <- tryCatch(sf::st_bbox(sf::st_transform(b, 4326)), error = function(e) NULL)
  if (is.null(bb)) {
    return("aoi_unknown")
  }
  paste(round(as.numeric(bb), 5), collapse = ",")
}

# AOI cells to fill: inside mask but missing zone/class (NA-safe for if/any).
aoi_inside_need_fill <- function(inside_vals, out_vals) {
  m <- !is.na(inside_vals) & is.na(out_vals)
  m[is.na(m)] <- FALSE
  m
}

safe_any_true <- function(x) {
  isTRUE(any(x, na.rm = TRUE))
}

# Paint cluster zones onto a template raster; optional AOI interior fill.
build_zone_raster_from_cells <- function(ref_raster, cell_df, zone_col = "zone", boundary_sf = NULL) {
  if (is.null(ref_raster) || is.null(cell_df) || nrow(cell_df) < 1L || !zone_col %in% names(cell_df)) {
    return(list(raster = NULL, levels = integer(0)))
  }
  zone_r <- raster::raster(ref_raster)
  zone_vals <- rep(NA_real_, raster::ncell(zone_r))
  zone_cells <- raster::cellFromXY(zone_r, cell_df[, c("x", "y"), drop = FALSE])
  zvec <- suppressWarnings(as.numeric(cell_df[[zone_col]]))
  ok <- is.finite(zone_cells) & zone_cells >= 1L & zone_cells <= length(zone_vals) & is.finite(zvec)
  if (safe_any_true(ok)) zone_vals[zone_cells[ok]] <- zvec[ok]
  raster::values(zone_r) <- zone_vals
  zone_levels <- sort(unique(stats::na.omit(zvec[ok])))
  if (!is.null(boundary_sf) && inherits(boundary_sf, "sf") && nrow(boundary_sf) > 0L) {
    zone_r <- fill_zone_raster_inside_boundary(zone_r, boundary_sf, zone_levels)
  }
  list(raster = zone_r, levels = zone_levels)
}

numeric_covariate_names_from_df <- function(df, max_vars = NULL, exclude_zone = TRUE, prefer_names = NULL) {
  if (is.null(df) || ncol(df) < 1L) return(character(0))
  skip <- c("ID", "leaflet_id", "edit_id", "layerId", "x", "y")
  var_names <- setdiff(names(df), skip)
  var_names <- var_names[!grepl("leaflet|edit|layer", var_names, ignore.case = TRUE)]
  var_names <- var_names[!grepl("(^id$|_id$|^fid$|^feature_id$|^objectid$)", var_names, ignore.case = TRUE)]
  if (isTRUE(exclude_zone)) var_names <- var_names[tolower(var_names) != "zone"]
  var_names <- var_names[vapply(df[var_names, drop = FALSE], is.numeric, logical(1))]
  if (length(prefer_names)) {
    pn <- as.character(prefer_names)
    pn <- pn[nzchar(pn)]
    hit <- intersect(pn, var_names)
    miss <- setdiff(pn, var_names)
    if (length(miss)) {
      norm <- function(x) gsub("[ :]", "_", x, fixed = FALSE)
      for (m in miss) {
        cand <- var_names[norm(var_names) == norm(m)]
        if (length(cand)) hit <- c(hit, cand[[1L]])
      }
    }
    var_names <- unique(hit)
  }
  if (!is.null(max_vars)) {
    max_vars <- suppressWarnings(as.integer(max_vars))
    if (length(max_vars) == 1L && !is.na(max_vars) && max_vars >= 1L && length(var_names) > max_vars) {
      var_names <- var_names[seq_len(max_vars)]
    }
  }
  var_names
}

# Fill NA zone cells inside AOI on a zone raster (modal + distance + dominant fallback).
fill_zone_raster_inside_boundary <- function(zone_r, boundary_sf, zone_levels = NULL) {
  if (is.null(zone_r) || is.null(boundary_sf) || nrow(boundary_sf) < 1L) return(zone_r)
  if (is.null(zone_levels) || !length(zone_levels)) {
    zone_levels <- sort(unique(stats::na.omit(raster::values(zone_r))))
  }
  boundary_sp <- tryCatch(as(boundary_sf, "Spatial"), error = function(e) NULL)
  if (is.null(boundary_sp)) return(zone_r)
  aoi_mask <- tryCatch(raster::rasterize(boundary_sp, zone_r, field = 1, background = NA), error = function(e) NULL)
  if (is.null(aoi_mask)) return(zone_r)
  inside_vals <- raster::values(aoi_mask)
  for (k in seq_len(8L)) {
    out_vals <- raster::values(zone_r)
    need_fill <- aoi_inside_need_fill(inside_vals, out_vals)
    if (!any(need_fill)) break
    mode3 <- raster::focal(zone_r, w = matrix(1, 3, 3), fun = raster::modal, na.rm = TRUE, pad = TRUE, padValue = NA)
    out_vals[need_fill] <- raster::values(mode3)[need_fill]
    raster::values(zone_r) <- out_vals
  }
  out_vals <- raster::values(zone_r)
  need_fill <- aoi_inside_need_fill(inside_vals, out_vals)
  if (any(need_fill) && length(zone_levels) > 0) {
    dist_stack <- lapply(zone_levels, function(z) {
      raster::distance(raster::calc(zone_r, fun = function(x) ifelse(x == z, 1, NA)))
    })
    dm <- do.call(cbind, lapply(dist_stack, function(dr) raster::values(dr)[need_fill]))
    dm[!is.finite(dm)] <- Inf
    ok <- apply(dm, 1, function(v) isTRUE(any(is.finite(v))))
    cls_idx <- rep(NA_integer_, nrow(dm))
    if (safe_any_true(ok)) cls_idx[ok] <- apply(dm[ok, , drop = FALSE], 1, which.min)
    out_vals[which(need_fill)[ok]] <- as.numeric(zone_levels[cls_idx[ok]])
    raster::values(zone_r) <- out_vals
  }
  out_vals <- raster::values(zone_r)
  need_fill <- aoi_inside_need_fill(inside_vals, out_vals)
  if (any(need_fill) && length(zone_levels) > 0) {
    inside_zone_vals <- out_vals[!is.na(inside_vals) & !is.na(out_vals)]
    if (length(inside_zone_vals) > 0) {
      dominant_zone <- as.numeric(names(which.max(table(inside_zone_vals))))
      out_vals[need_fill] <- dominant_zone
      raster::values(zone_r) <- out_vals
    }
  }
  zone_r
}

# Spatially spread candidate pool, then cLHS within pool (combines coverage + covariate representativeness).
spread_clhs_indices <- function(n, pop_xy, pop_cov, rng_seed = NULL, candidate_multiplier = 3L) {
  n <- as.integer(n)
  pop_n <- nrow(pop_xy)
  if (length(n) != 1L || is.na(n) || n < 1L) return(integer(0))
  if (n >= pop_n) return(seq_len(pop_n))
  if (!is.null(rng_seed)) set.seed(as.integer(rng_seed))
  mult <- max(2L, as.integer(candidate_multiplier))
  n_cand <- min(pop_n, max(as.integer(n) * mult, as.integer(n) + 5L))
  cand_idx <- systematic_spread_indices(n_cand, pop_xy, rng_seed = NULL)
  cand_idx <- unique(cand_idx[is.finite(cand_idx) & cand_idx >= 1L & cand_idx <= pop_n])
  if (length(cand_idx) <= n) {
    return(compare_ensure_n_indices(cand_idx, pop_n, n))
  }
  cov_cols <- vapply(pop_cov, is.numeric, logical(1))
  cand_cov <- pop_cov[cand_idx, cov_cols, drop = FALSE]
  cand_cov <- cand_cov[, vapply(cand_cov, function(col) length(unique(col[is.finite(col)])) > 1L, logical(1)), drop = FALSE]
  if (!ncol(cand_cov) || nrow(cand_cov) < n) {
    return(compare_ensure_n_indices(cand_idx[seq_len(min(n, length(cand_idx)))], pop_n, n))
  }
  clhs_res <- tryCatch(
    clhs::clhs(cand_cov, size = n, progress = FALSE, simple = TRUE),
    error = function(e) NULL
  )
  if (!is.null(clhs_res)) {
    local_idx <- clhs_sample_indices(clhs_res)
    local_idx <- local_idx[is.finite(local_idx) & local_idx >= 1L & local_idx <= length(cand_idx)]
    if (length(local_idx) >= 1L) {
      return(compare_ensure_n_indices(cand_idx[local_idx], pop_n, n))
    }
  }
  compare_ensure_n_indices(cand_idx[seq_len(min(n, length(cand_idx)))], pop_n, n)
}

# Univariate representativeness via KS distance; skip noisy ks.test when sample is degenerate.
ks_uniformity_score <- function(sample_vals, ref_vals) {
  sv <- sample_vals[is.finite(sample_vals)]
  rv <- ref_vals[is.finite(ref_vals)]
  if (length(rv) < 5L || length(sv) < 2L) return(1)
  if (length(unique(sv)) <= 1L) return(1)
  ks <- suppressWarnings(tryCatch(
    stats::ks.test(sv, rv, exact = FALSE)$statistic,
    error = function(e) 1
  ))
  max(0, 1 - as.numeric(ks))
}

# How well sample min/max reach population lower/upper tails (q10/q90 by default).
quantile_tail_coverage_score <- function(
    sdf,
    pop_cov,
    pop_vn,
    q_lo = 0.10,
    q_hi = 0.90
) {
  if (is.null(sdf) || nrow(sdf) < 2L || is.null(pop_cov) || !length(pop_vn)) return(NA_real_)
  q_lo <- as.numeric(q_lo)
  q_hi <- as.numeric(q_hi)
  if (length(q_lo) != 1L || length(q_hi) != 1L || q_lo >= q_hi) return(NA_real_)
  var_scores <- vapply(pop_vn, function(vn) {
    pv <- pop_cov[[vn]]
    sv <- sdf[[vn]]
    pv <- pv[is.finite(pv)]
    sv <- sv[is.finite(sv)]
    if (length(pv) < 5L || length(sv) < 2L) return(1)
    ql <- stats::quantile(pv, q_lo, na.rm = TRUE, names = FALSE)
    qh <- stats::quantile(pv, q_hi, na.rm = TRUE, names = FALSE)
    q05 <- stats::quantile(pv, 0.05, na.rm = TRUE, names = FALSE)
    q95 <- stats::quantile(pv, 0.95, na.rm = TRUE, names = FALSE)
    pr <- diff(range(pv, na.rm = TRUE))
    if (!is.finite(pr) || pr <= 0) return(1)
    min_s <- min(sv, na.rm = TRUE)
    max_s <- max(sv, na.rm = TRUE)
    hit_lo <- as.numeric(min_s <= ql)
    hit_hi <- as.numeric(max_s >= qh)
    pair_hit <- if (hit_lo > 0 && hit_hi > 0) 1 else if (hit_lo > 0 || hit_hi > 0) 0.55 else 0
    low_depth <- min(1, max(0, (ql - min_s) / max(1e-8, ql - q05)))
    high_depth <- min(1, max(0, (max_s - qh) / max(1e-8, q95 - qh)))
    span_ratio <- min(1, max(0, (max_s - min_s) / pr))
    mean(c(pair_hit, low_depth, high_depth, span_ratio))
  }, numeric(1))
  mean(var_scores, na.rm = TRUE)
}

pca_coverage_score_robust <- function(pop_pc, samp_pc, n_components, n_sample = nrow(samp_pc)) {
  if (n_sample < 2L || is.null(pop_pc) || nrow(pop_pc) < 2L) return(1)
  n_components <- min(as.integer(n_components), ncol(pop_pc), ncol(samp_pc))
  if (n_components < 1L) return(1)
  comp_scores <- vapply(seq_len(n_components), function(j) {
    pv <- pop_pc[, j]
    sv <- samp_pc[, j]
    pv <- pv[is.finite(pv)]
    sv <- sv[is.finite(sv)]
    if (length(pv) < 2L || length(sv) < 2L) return(1)
    q_pop <- stats::quantile(pv, probs = c(0.05, 0.95), na.rm = TRUE, names = FALSE)
    pr <- q_pop[2] - q_pop[1]
    if (!is.finite(pr) || pr <= 0) return(1)
    sr <- diff(range(sv, na.rm = TRUE))
    if (!is.finite(sr) || sr < 0) sr <- 0
    n_eff <- max(2L, as.integer(n_sample))
    expected_span <- ((n_eff - 1) / (n_eff + 1)) * pr
    range_score <- min(1, max(0, sr / max(expected_span, 1e-8)))
    pop_sd <- stats::sd(pv, na.rm = TRUE)
    center_score <- if (is.finite(pop_sd) && pop_sd > 0) {
      min(1, max(0, 1 - abs(mean(sv, na.rm = TRUE) - mean(pv, na.rm = TRUE)) / (2 * pop_sd)))
    } else 1
    q10 <- stats::quantile(pv, 0.10, na.rm = TRUE)
    q90 <- stats::quantile(pv, 0.90, na.rm = TRUE)
    core <- pv >= q10 & pv <= q90
    if (!any(core, na.rm = TRUE)) return(mean(c(range_score, center_score)))
    smin <- min(sv, na.rm = TRUE)
    smax <- max(sv, na.rm = TRUE)
    covered <- core & pv >= smin & pv <= smax
    envelope_score <- sum(covered, na.rm = TRUE) / sum(core, na.rm = TRUE)
    mean(c(range_score, center_score, envelope_score))
  }, numeric(1))
  mean(comp_scores, na.rm = TRUE)
}

pick_report_ndvi_raster <- function(all_rasters) {
  if (is.null(all_rasters) || !length(all_rasters)) return(NULL)
  nm <- names(all_rasters)
  pri <- unique(c(
    nm[grepl("^Sentinel_VI_NDVI", nm)],
    nm[grepl("^Planet_VI_NDVI", nm)],
    nm[grepl("^MS_VI_NDVI", nm)],
    nm[grepl("_VI_NDVI$", nm)]
  ))
  pri <- pri[nzchar(pri)]
  if (length(pri)) return(all_rasters[[pri[[1L]]]])
  NULL
}

prepare_raster_for_report_plot <- function(r, boundary_sf = NULL, max_cells = 120000L) {
  if (is.null(r)) return(NULL)
  out <- r[[1]]
  if (is.null(out)) return(NULL)
  if (!is.null(boundary_sf) && nrow(boundary_sf) > 0) {
    out <- tryCatch({
      b <- sf::st_transform(boundary_sf, raster::crs(out))
      bb <- sf::st_bbox(b)
      pad_x <- max((bb["xmax"] - bb["xmin"]) * 0.03, 1e-6)
      pad_y <- max((bb["ymax"] - bb["ymin"]) * 0.03, 1e-6)
      ext <- raster::extent(bb["xmin"] - pad_x, bb["xmax"] + pad_x, bb["ymin"] - pad_y, bb["ymax"] + pad_y)
      raster::mask(raster::crop(out, ext), b)
    }, error = function(e) out)
  }
  nc <- tryCatch(raster::ncell(out), error = function(e) NA_real_)
  if (is.finite(nc) && nc > max_cells) {
    fact <- max(2L, as.integer(ceiling(sqrt(nc / max_cells))))
    out <- tryCatch(raster::aggregate(out, fact = fact, fun = mean, na.rm = TRUE), error = function(e) out)
  }
  out
}

prepare_zone_raster_for_report_plot <- function(r, boundary_sf = NULL, max_cells = 120000L) {
  if (is.null(r)) return(NULL)
  out <- r[[1]]
  if (is.null(out)) return(NULL)
  if (!is.null(boundary_sf) && nrow(boundary_sf) > 0) {
    out <- tryCatch({
      b <- sf::st_transform(boundary_sf, raster::crs(out))
      bb <- sf::st_bbox(b)
      pad_x <- max((bb["xmax"] - bb["xmin"]) * 0.03, 1e-6)
      pad_y <- max((bb["ymax"] - bb["ymin"]) * 0.03, 1e-6)
      ext <- raster::extent(bb["xmin"] - pad_x, bb["xmax"] + pad_x, bb["ymin"] - pad_y, bb["ymax"] + pad_y)
      raster::mask(raster::crop(out, ext), b)
    }, error = function(e) out)
  }
  nc <- tryCatch(raster::ncell(out), error = function(e) NA_real_)
  if (is.finite(nc) && nc > max_cells) {
    fact <- max(2L, as.integer(ceiling(sqrt(nc / max_cells))))
    modal_fun <- function(x) {
      ux <- stats::na.omit(x)
      if (!length(ux)) return(NA_real_)
      as.numeric(names(sort(table(ux), decreasing = TRUE)[1]))
    }
    out <- tryCatch(raster::aggregate(out, fact = fact, fun = modal_fun), error = function(e) out)
  }
  out
}

format_zone_report_lines <- function(zone_count = NULL, zone_summary = NULL) {
  out <- character(0)
  if (!is.null(zone_count) && length(zone_count) == 1L && !is.na(zone_count)) {
    out <- c(out, paste0("Management zones in field: ", as.integer(zone_count)))
  }
  if (!is.null(zone_summary) && nrow(zone_summary) > 0L) {
    out <- c(out, "Sample points per zone:")
    for (i in seq_len(nrow(zone_summary))) {
      out <- c(out, paste0("  Zone ", zone_summary$Zone[i], ": ", zone_summary$Sample_Count[i], " sample(s)"))
    }
  }
  out
}

compute_sampling_quality_metrics <- function(
  pts,
  boundary_sf,
  analysis_crs = "+proj=longlat +datum=WGS84",
  spread_grid_context = NULL
) {
  if (is.null(pts) || nrow(pts) < 2L) {
    return(list(
      nn_med = NA_real_,
      zone_bal = NA_real_,
      spread_pct = NA_real_,
      even_spread_idx = NA_real_,
      lines = character(0)
    ))
  }
  pts_m <- tryCatch(sf::st_transform(pts, analysis_crs), error = function(e) pts)
  dmat <- tryCatch(as.matrix(sf::st_distance(pts_m)), error = function(e) NULL)
  nn_med <- NA_real_
  if (!is.null(dmat) && nrow(dmat) > 1L) {
    diag(dmat) <- Inf
    nn <- apply(dmat, 1, min, na.rm = TRUE)
    nn_med <- suppressWarnings(stats::median(as.numeric(nn), na.rm = TRUE))
  }
  zone_bal <- NA_real_
  if ("zone" %in% names(pts)) {
    ztab <- table(pts$zone)
    pz <- as.numeric(ztab) / sum(ztab)
    entropy <- -sum(pz * log(pz + 1e-12))
    max_entropy <- log(length(ztab))
    zone_bal <- if (max_entropy > 0) 100 * entropy / max_entropy else 100
  }
  spread_pct <- NA_real_
  if (!is.null(boundary_sf) && nrow(boundary_sf) > 0L && is.finite(nn_med)) {
    area_m2 <- as.numeric(sum(sf::st_area(sf::st_transform(boundary_sf, analysis_crs))))
    expected <- sqrt(area_m2 / nrow(pts))
    if (is.finite(expected) && expected > 0) spread_pct <- 100 * (nn_med / expected)
  }
  even_spread_idx <- NA_real_
  field_coverage_pick_pct <- NA_real_
  if (!is.null(spread_grid_context) && is.list(spread_grid_context)) {
    sc <- spread_grid_context$even_spread_idx
    if (is.finite(sc)) even_spread_idx <- sc
    fc_pick <- spread_grid_context$field_coverage_pct
    if (is.finite(fc_pick)) field_coverage_pick_pct <- fc_pick
  }
  lines <- c(
    "Sampling quality:",
    paste0(
      "  Typical nearest-neighbor spacing: ",
      if (is.finite(nn_med)) paste0(round(nn_med, 2), " m") else "NA",
      " (greater distance = less clustering)."
    ),
    paste0(
      "  Zone allocation balance: ",
      if (is.finite(zone_bal)) paste0(round(zone_bal, 1), "%") else "N/A",
      if (is.finite(zone_bal)) " (100% = even spread across zones)." else " (zone-labeled sampling only)."
    ),
    paste0(
      "  Field coverage spread: ",
      if (is.finite(spread_pct)) paste0(round(spread_pct, 1), "%") else "NA",
      " (100% ≈ even spacing across the AOI; Generate optimizes this metric across replicates)."
    ),
    paste0(
      "  Geographic spread index (", RECOMMENDED_GENERATION_REPEATS(), "-rep pick): ",
      if (is.finite(even_spread_idx)) round(even_spread_idx, 3) else "NA",
      " (0–1, higher = better; same metric used to choose the best replicate)."
    )
  )
  list(
    nn_med = nn_med,
    zone_bal = zone_bal,
    spread_pct = spread_pct,
    even_spread_idx = even_spread_idx,
    field_coverage_pick_pct = field_coverage_pick_pct,
    lines = lines
  )
}

draw_report_zones_page <- function(r, title = "Field management zones", n_zones = NULL) {
  if (is.null(r)) return(invisible(FALSE))
  v <- tryCatch(raster::values(r), error = function(e) NULL)
  if (is.null(v)) return(invisible(FALSE))
  levels <- sort(unique(as.integer(stats::na.omit(v))))
  if (!length(levels)) return(invisible(FALSE))
  if (is.null(n_zones) || is.na(n_zones)) n_zones <- length(levels)
  pal <- tryCatch(grDevices::hcl.colors(length(levels), palette = "Viridis"), error = function(e) {
    grDevices::rainbow(length(levels))
  })
  graphics::par(mar = c(2.4, 2.4, 3.6, 6.2), bg = "white", fg = "black")
  brks <- c(levels - 0.5, max(levels) + 0.5)
  graphics::plot(
    r, col = pal, breaks = brks, axes = FALSE, legend = FALSE,
    main = title, cex.main = 1.2, font.main = 2, useRaster = TRUE
  )
  graphics::box(lwd = 1.2, col = "gray70")
  graphics::legend(
    "right", inset = c(-0.16, 0.02),
    legend = paste("Zone", levels), fill = pal, border = NA, bty = "n",
    title = "Zones", cex = 0.72, title.cex = 0.8
  )
  graphics::mtext(
    paste0(n_zones, " zone", if (as.integer(n_zones) == 1L) "" else "s", " from covariate clustering"),
    side = 3, line = 0.15, adj = 1, cex = 0.72, col = "gray35"
  )
  graphics::mtext("GeoSampler field report", side = 1, line = 0.4, cex = 0.65, col = "gray45")
  invisible(TRUE)
}

report_ndvi_colors <- function(n = 64L) {
  grDevices::colorRampPalette(c(
    "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3",
    "#e0f3db", "#a6dba0", "#5aae61", "#1b7837", "#00441b"
  ))(as.integer(n))
}

layer_distribution_palette <- function(layer_name, n = 256L) {
  nm <- tolower(as.character(layer_name))
  n <- max(2L, as.integer(n))
  if (grepl("ndvi|ndre|gndvi|evi|osavi|vari|ci_re|cire|ireci|gci", nm, perl = TRUE)) {
    report_ndvi_colors(n)
  } else if (grepl("elev|dem|dtm|slope|aspect|altitude|height", nm, perl = TRUE)) {
    grDevices::terrain.colors(n)
  } else {
    viridis::viridis(n)
  }
}

layer_value_limits <- function(values, layer_name = NULL, raster_limits = NULL) {
  vals <- suppressWarnings(as.numeric(values))
  vals <- vals[is.finite(vals)]
  if (!is.null(layer_name) && !is.null(raster_limits) && !is.null(raster_limits[[layer_name]])) {
    lim <- raster_limits[[layer_name]]
    if (all(is.finite(lim))) return(lim)
  }
  if (length(vals) < 1L) return(c(0, 1))
  rng <- range(vals, na.rm = TRUE)
  if (!all(is.finite(rng)) || abs(rng[2] - rng[1]) < 1e-9) rng <- rng + c(-0.5, 0.5)
  rng
}

precompute_raster_layer_limits <- function(raster_stack) {
  if (is.null(raster_stack) || length(raster_stack) < 1L) return(list())
  limits <- lapply(names(raster_stack), function(nm) {
    rv <- tryCatch(raster::values(raster_stack[[nm]]), error = function(e) NULL)
    rv <- rv[is.finite(rv)]
    if (length(rv) < 1L) return(NULL)
    q <- stats::quantile(rv, c(0.02, 0.98), na.rm = TRUE, names = FALSE)
    if (all(is.finite(q)) && abs(q[2] - q[1]) > 1e-8) q else range(rv, na.rm = TRUE)
  })
  stats::setNames(limits, names(raster_stack))
}

DISTRIBUTION_PLOT_WIDTH_PX <- 720L
DISTRIBUTION_PLOT_PER_PANEL_PX <- 280L

distribution_stack_height_px <- function(n_plots, per_plot_px = DISTRIBUTION_PLOT_PER_PANEL_PX) {
  n_plots <- max(0L, as.integer(n_plots))
  per_plot_px <- max(220L, as.integer(per_plot_px))
  as.integer(max(per_plot_px, min(6000L, n_plots * per_plot_px)))
}

compute_action_bar <- function(button_id, label, icon_name = "calculator") {
  tags$div(
    class = "compute-action-bar",
    style = paste0(
      "margin:0 0 14px 0; padding:12px 14px; ",
      "background:linear-gradient(120deg,#e8f2ff 0%,#f4f9ff 100%); ",
      "border:1px solid #bfd7ff; border-radius:10px; ",
      "display:flex; align-items:center; flex-wrap:wrap; gap:10px;"
    ),
    actionButton(button_id, label, class = "btn-primary btn-one-shot", icon = icon(icon_name))
  )
}

distribution_download_bar <- function(button_id, label = "Download plot (PNG)") {
  tags$div(
    class = "distribution-download-bar",
    style = "display:flex; gap:8px; flex-wrap:wrap; margin:0 0 10px 0; align-items:center;",
    downloadButton(button_id, label, class = "btn-modern btn-sm")
  )
}

sampling_points_data_tabs <- function(tabs_id, table_output_id, plot_output_id, download_plot_id, plot_height = "420px") {
  tabsetPanel(
    id = tabs_id,
    tabPanel(
      "Table",
      value = paste0(tabs_id, "_table"),
      tags$div(class = "pop-sample-summary-table-wrap", DT::dataTableOutput(table_output_id))
    ),
    tabPanel(
      "Distribution",
      value = paste0(tabs_id, "_dist"),
      distribution_download_bar(download_plot_id),
      tags$p(
        class = "text-muted",
        style = "font-size:12px; margin:0 0 8px 0;",
        "Density of covariate values at sample points (up to 8 numeric layers; summary tables round stats to 3 decimals)."
      ),
      tags$div(
        class = "summary-distribution-scroll",
        plotOutput(plot_output_id, height = plot_height)
      )
    )
  )
}

imagery_timeseries_view_tab <- function() {
  tabPanel(
    "Timeseries viewer",
    icon = icon("chart-line"),
    value = "imagery_timeseries_tab",
    tags$div(
      class = "field-compare-hero",
      h3(style = "margin-top:0; color:#1b3f66; font-weight:700;", "NDRE trend (preview)"),
      p(
        class = "text-muted",
        style = "margin-bottom:8px;",
        strong("Search"), " in the sidebar updates the available Sentinel date list only (no scene download and no chart build). ",
        "Click ", strong("Build timeseries"), " when you are ready to visualize the NDRE trend."
      ),
      tags$p(
        class = "text-muted",
        style = "margin-bottom:0; font-size:12.5px;",
        "Pick an acquisition date from the chart, then open ",
        strong("Imagery Viewer"), " to retrieve that scene."
      ),
      tags$p(
        class = "text-muted",
        style = "margin:8px 0 0 0; font-size:11px; line-height:1.35;",
        "Chart uses all search scenes if ", SENTINEL_NDRE_EVENLY_SPACED_CAP(), " or fewer; otherwise ",
        SENTINEL_NDRE_EVENLY_SPACED_CAP(), " evenly spaced (oldest to newest). Subsampled pixels only."
      )
    ),
    uiOutput("sentinel_timeseries_status_ui"),
    compute_action_bar("rebuild_sentinel_timeseries", "Build timeseries", "chart-line"),
    conditionalPanel(
      condition = "output.sentinel_ndre_timeseries_ready",
      distribution_download_bar("download_sentinel_ndre_timeseries_plot"),
      tags$div(
        class = "sentinel-ndre-timeseries-plot-wrap",
        withSpinner(
          plotOutput("sentinel_ndre_timeseries_plot", height = "420px")
        )
      ),
      uiOutput("sentinel_ndre_timeseries_caption_ui")
    ),
    conditionalPanel(
      condition = "!output.sentinel_ndre_timeseries_ready",
      tags$div(
        class = "field-compare-empty-state",
        style = "padding:24px; margin:12px 0; border:1px dashed #bfd7ff; border-radius:12px; background:#f8fbff; text-align:center;",
        icon("chart-line", style = "font-size:28px; color:#7a9bc4; margin-bottom:8px;"),
        tags$p(
          style = "margin:0 0 6px 0; color:#4a5f78;",
          strong("Search"), " in the sidebar to update available dates, then click ", strong("Build timeseries"),
          " to draw the chart."
        ),
        tags$p(
          style = "margin:0; color:#6a7f96; font-size:12px;",
          "Then open ", strong("Imagery Viewer"), " to retrieve your chosen date."
        )
      )
    )
  )
}

imagery_map_view_tab <- function(imagery_source = NULL) {
  source <- as.character(imagery_source %||% "")[1L]
  if (is.na(source)) source <- ""
  intro <- switch(
    source,
    download_sentinel = tagList(
      "Retrieve the Sentinel scene for the date you chose in ",
      strong("Timeseries viewer"), " (map, bands, VI, download)."
    ),
    download_planet = "Retrieve Planet imagery, preview bands, calculate VIs, and download the processed stack.",
    upload_ms = "Preview uploaded imagery, assign bands, calculate VIs, and download processed layers.",
    "Preview imagery layers, calculate VIs, and download processed outputs."
  )
  tabPanel(
    "Imagery Viewer",
    icon = icon("map"),
    value = "imagery_map_tab",
    tags$p(
      class = "text-muted",
      style = "margin:0 0 10px 0; font-size:12.5px;",
      intro
    ),
    div(
      class = "map-container",
      withSpinner(leafletOutput("imagery_map"))
    ),
    div(
      class = "main-panel-map-follow",
      uiOutput("planet_band_selector_ui"),
      uiOutput("sentinel_band_selector_ui"),
      uiOutput("vi_calculator_ui"),
      uiOutput("sentinel_vi_calculator_ui"),
      uiOutput("vi_download_ui"),
      uiOutput("sentinel_vi_download_ui"),
      uiOutput("ms_band_selector_ui"),
      uiOutput("ms_vi_calculator_ui"),
      uiOutput("individual_vi_calculator_ui"),
      uiOutput("ms_vi_download_ui"),
      uiOutput("planet_download_ui"),
      uiOutput("sentinel_download_ui")
    ),
    div(class = "map-zoom-below compact-zoom", uiOutput("zoom_button_ui_imagery")),
    uiOutput("planet_status_ui"),
    uiOutput("sentinel_status_ui"),
    uiOutput("sentinel_console_panel_ui")
  )
}

imagery_view_subtabs_ui <- function(imagery_source = NULL, selected = NULL) {
  source <- as.character(imagery_source %||% "")[1L]
  if (is.na(source)) source <- ""
  selected <- as.character(selected %||% "")[1L]
  if (is.na(selected)) selected <- ""
  include_timeseries <- identical(source, "download_sentinel")
  tabs <- list(id = "imagery_view_subtabs")
  if (include_timeseries) {
    tabs$selected <- if (nzchar(selected)) selected else "imagery_timeseries_tab"
    tabs <- c(tabs, list(imagery_timeseries_view_tab()))
  } else {
    tabs$selected <- "imagery_map_tab"
  }
  tabs <- c(tabs, list(imagery_map_view_tab(source)))
  do.call(tabsetPanel, tabs)
}

write_distribution_plots_png <- function(file, plots, width_px = DISTRIBUTION_PLOT_WIDTH_PX, per_panel_px = DISTRIBUTION_PLOT_PER_PANEL_PX) {
  plots <- plots[!vapply(plots, is.null, logical(1))]
  if (!length(plots)) stop("No plots to save.", call. = FALSE)
  h_px <- distribution_stack_height_px(length(plots), per_plot_px = per_panel_px)
  w_in <- max(6, as.numeric(width_px) / 96)
  h_in <- max(4, as.numeric(h_px) / 96)
  grDevices::png(file, width = w_in, height = h_in, units = "in", res = 120, bg = "#eef2f7")
  on.exit(grDevices::dev.off(), add = TRUE)
  draw_distribution_plot_stack(plots)
  invisible(file)
}

build_sampling_density_facets_plot <- function(df, fill_color = "#8ecae6", prefer_names = NULL) {
  if (is.null(df) || nrow(df) < 1L) return(NULL)
  var_names <- numeric_covariate_names_from_df(
    df,
    max_vars = NULL,
    exclude_zone = TRUE,
    prefer_names = prefer_names
  )
  if (!length(var_names)) return(NULL)
  long <- do.call(
    rbind,
    lapply(var_names, function(vn) data.frame(variable = vn, value = df[[vn]], stringsAsFactors = FALSE))
  )
  long$value <- suppressWarnings(as.numeric(long$value))
  long <- long[is.finite(long$value), , drop = FALSE]
  if (!nrow(long)) return(NULL)
  ggplot2::ggplot(long, ggplot2::aes(x = value)) +
    ggplot2::geom_density(fill = fill_color, alpha = 0.72, color = "#1b3f66", linewidth = 0.35) +
    ggplot2::facet_wrap(
      ggplot2::vars(variable),
      scales = "free",
      ncol = if (length(var_names) > 8L) 3L else 2L
    ) +
    ggplot2::labs(x = NULL, y = NULL, title = "Sample point covariate distributions") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = "#1b3f66"),
      strip.text = ggplot2::element_text(face = "bold", color = "#174b7c")
    )
}

build_population_sample_distribution_plot <- function(
  layer_name,
  sample_df,
  pop_values = NULL,
  raster_limits = NULL,
  sample_border_cols = c(
    "App-generated" = "#1f5f99",
    "Historical" = "#c45c00",
    "App sample" = "#1f5f99"
  )
) {
  if (is.null(sample_df) || nrow(sample_df) < 1L) return(NULL)
  sample_df$Value <- suppressWarnings(as.numeric(sample_df$Value))
  sample_df <- sample_df[is.finite(sample_df$Value), , drop = FALSE]
  if (nrow(sample_df) < 1L) return(NULL)

  pop_vals <- suppressWarnings(as.numeric(pop_values))
  pop_vals <- pop_vals[is.finite(pop_vals)]

  all_vals <- c(pop_vals, sample_df$Value)
  val_rng <- layer_value_limits(all_vals, layer_name, raster_limits)
  pal_cols <- layer_distribution_palette(layer_name, 64L)

  sample_df$Source <- factor(as.character(sample_df$Source))
  sample_df$border_col <- unname(sample_border_cols[as.character(sample_df$Source)])
  sample_df$border_col[is.na(sample_df$border_col)] <- "#1f5f99"

  pop_cloud_alpha <- 0.34
  subtitle <- if (length(pop_vals) >= 1L) {
    paste0(
      "Gray cloud: population (n = ", length(pop_vals), "); ",
      "blue: sample (n = ", nrow(sample_df), ") — overlaid violins"
    )
  } else {
    paste0("Sample (n = ", nrow(sample_df), "); population cloud unavailable")
  }

  p <- ggplot2::ggplot()
  x_overlay <- 1

  if (length(pop_vals) >= 3L) {
    pop_plot_df <- data.frame(x = x_overlay, Value = pop_vals)
    p <- p +
      ggplot2::geom_violin(
        data = pop_plot_df,
        ggplot2::aes(x = x, y = Value),
        fill = "#b8c9d9",
        color = grDevices::adjustcolor("#6b7c93", alpha.f = 0.45),
        alpha = pop_cloud_alpha,
        linewidth = 0.35,
        trim = TRUE,
        width = 0.9
      ) +
      ggplot2::geom_point(
        data = pop_plot_df,
        ggplot2::aes(x = x, y = Value, fill = Value),
        shape = 21,
        color = grDevices::adjustcolor("#5a6d82", alpha.f = 0.45),
        size = 1.85,
        alpha = pop_cloud_alpha,
        stroke = 0.3,
        position = ggplot2::position_jitter(width = 0.07, height = 0, seed = 42L)
      )
  }

  if (nrow(sample_df) >= 3L) {
    sample_plot_df <- data.frame(x = x_overlay, Value = sample_df$Value)
    p <- p +
      ggplot2::geom_violin(
        data = sample_plot_df,
        ggplot2::aes(x = x, y = Value),
        fill = "#9ec5e8",
        color = grDevices::adjustcolor("#1f5f99", alpha.f = 0.65),
        alpha = 0.38,
        linewidth = 0.45,
        trim = TRUE,
        width = 0.9
      )
  }

  p <- p +
    ggplot2::geom_point(
      data = sample_df,
      ggplot2::aes(x = x_overlay, y = Value, fill = Value, color = border_col),
      shape = 21,
      size = 3.15,
      alpha = 0.94,
      stroke = 0.7,
      position = ggplot2::position_jitter(width = 0.045, height = 0, seed = 7L)
    ) +
    ggplot2::scale_color_identity(guide = "none") +
    ggplot2::scale_fill_gradientn(
      colors = pal_cols,
      limits = val_rng,
      name = "Value",
      na.value = "#b0b8c4"
    ) +
    ggplot2::scale_x_continuous(
      breaks = NULL,
      labels = NULL,
      limits = c(0.52, 1.48),
      expand = ggplot2::expansion(mult = 0.02)
    ) +
    ggplot2::labs(title = layer_name, subtitle = subtitle, x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 17, color = "#1b3f66", margin = ggplot2::margin(b = 4)),
      plot.subtitle = ggplot2::element_text(size = 11, color = "#5a6d82", margin = ggplot2::margin(b = 8)),
      plot.background = ggplot2::element_rect(fill = "#eef2f7", color = "#d5dee8", linewidth = 0.4),
      panel.background = ggplot2::element_rect(fill = "#f7f9fc", color = NA),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(8, 92, 8, 8),
      legend.position = c(0.99, 0.99),
      legend.justification = c(1, 1),
      legend.box = "vertical",
      legend.background = ggplot2::element_rect(
        fill = grDevices::adjustcolor("white", alpha.f = 0.9),
        color = "#cfd8e3",
        linewidth = 0.35
      ),
      legend.margin = ggplot2::margin(4, 6, 4, 6),
      legend.text = ggplot2::element_text(size = 10),
      legend.title = ggplot2::element_text(size = 10, face = "bold")
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        title = "Value",
        barheight = ggplot2::unit(36, "pt"),
        barwidth = ggplot2::unit(5.5, "pt"),
        frame.colour = "#cfd8e3",
        ticks.colour = "#6b7c93"
      )
    )

  sources_present <- unique(as.character(sample_df$Source))
  if (length(sources_present) > 0L) {
    src_lbl <- paste(sources_present, collapse = " · ")
    p <- p + ggplot2::labs(subtitle = paste0(subtitle, " | ", src_lbl))
  }

  p
}

resolve_cost_currency_label <- function(preset, custom = "") {
  preset <- as.character(preset)[1L]
  custom <- trimws(as.character(custom)[1L])
  if (identical(preset, "custom") && nzchar(custom)) return(custom)
  switch(
    preset,
    usd = "USD ($)",
    eur = "EUR (\u20ac)",
    gbp = "GBP (\u00a3)",
    cad = "CAD (C$)",
    aud = "AUD (A$)",
    if (nzchar(custom)) custom else "USD ($)"
  )
}

compute_cost_comparison <- function(prior_n, app_n, cost_per_sample) {
  prior_n <- suppressWarnings(as.integer(prior_n)[1L])
  app_n <- suppressWarnings(as.integer(app_n)[1L])
  cost_per_sample <- suppressWarnings(as.numeric(cost_per_sample)[1L])
  if (!is.finite(prior_n) || prior_n < 1L) prior_n <- NA_integer_
  if (!is.finite(app_n) || app_n < 1L) app_n <- NA_integer_
  if (!is.finite(cost_per_sample) || cost_per_sample < 0) cost_per_sample <- NA_real_
  if (is.na(prior_n) || is.na(app_n) || is.na(cost_per_sample)) {
    return(list(ok = FALSE, reason = "Enter sample counts (≥ 1) and a non-negative cost per sample."))
  }
  prior_total <- prior_n * cost_per_sample
  app_total <- app_n * cost_per_sample
  savings_abs <- prior_total - app_total
  savings_pct <- if (prior_total > 0) (savings_abs / prior_total) * 100 else NA_real_
  list(
    ok = TRUE,
    prior_n = prior_n,
    app_n = app_n,
    cost_per_sample = cost_per_sample,
    prior_total = prior_total,
    app_total = app_total,
    savings_abs = savings_abs,
    savings_pct = savings_pct
  )
}

build_cost_comparison_plot <- function(cc, currency_label = "USD ($)") {
  if (!isTRUE(cc$ok)) return(NULL)
  df <- data.frame(
    Design = factor(
      c("Prior / grid sampling", "App-recommended design"),
      levels = c("Prior / grid sampling", "App-recommended design")
    ),
    total_cost = c(cc$prior_total, cc$app_total),
    n_samples = c(cc$prior_n, cc$app_n),
    stringsAsFactors = TRUE
  )
  ggplot2::ggplot(df, ggplot2::aes(x = Design, y = total_cost, fill = Design)) +
    ggplot2::geom_col(width = 0.62, color = "#1b3f66", linewidth = 0.35) +
    ggplot2::scale_fill_manual(values = c("#e8b88a", "#4a90d9"), guide = "none") +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", total_cost)),
      vjust = -0.35,
      size = 3.6,
      fontface = "bold",
      color = "#1b3f66"
    ) +
    ggplot2::labs(
      title = "Total field sampling cost",
      subtitle = paste0(
        "Prior / grid: n = ", cc$prior_n, "  |  App design: n = ", cc$app_n,
        "  |  Unit cost = ", format(cc$cost_per_sample, digits = 4), " (", currency_label, " per sample)"
      ),
      x = NULL,
      y = paste0("Total cost (", currency_label, ")")
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = "#1b3f66"),
      plot.subtitle = ggplot2::element_text(size = 9.5, color = "#4a5f78"),
      axis.text.x = ggplot2::element_text(face = "bold", color = "#1b3f66"),
      panel.grid.major.x = ggplot2::element_blank()
    ) +
    ggplot2::expand_limits(y = max(df$total_cost) * 1.12)
}

build_field_compare_distribution_plot <- function(
  layer_name,
  app_df,
  hist_df,
  raster_limits = NULL,
  hist_cloud_alpha = 0.4
) {
  if (!is.null(app_df) && nrow(app_df) > 0L) {
    app_df$Value <- suppressWarnings(as.numeric(app_df$Value))
    app_df <- app_df[is.finite(app_df$Value), , drop = FALSE]
  } else {
    app_df <- app_df[0, , drop = FALSE]
  }
  if (!is.null(hist_df) && nrow(hist_df) > 0L) {
    hist_df$Value <- suppressWarnings(as.numeric(hist_df$Value))
    hist_df <- hist_df[is.finite(hist_df$Value), , drop = FALSE]
  } else {
    hist_df <- hist_df[0, , drop = FALSE]
  }
  if (nrow(app_df) < 1L && nrow(hist_df) < 1L) return(NULL)

  all_vals <- c(app_df$Value, hist_df$Value)
  val_rng <- layer_value_limits(all_vals, layer_name, raster_limits)
  pal_cols <- layer_distribution_palette(layer_name, 64L)

  subtitle <- paste0(
    "Orange cloud: historical (n = ", nrow(hist_df), "); ",
    "blue: app sample (n = ", nrow(app_df), ") — overlaid violins"
  )

  p <- ggplot2::ggplot()
  x_overlay <- 1

  if (nrow(hist_df) >= 3L) {
    hist_plot_df <- data.frame(x = x_overlay, Value = hist_df$Value)
    p <- p +
      ggplot2::geom_violin(
        data = hist_plot_df,
        ggplot2::aes(x = x, y = Value),
        fill = "#e8b88a",
        color = grDevices::adjustcolor("#c45c00", alpha.f = 0.45),
        alpha = hist_cloud_alpha,
        linewidth = 0.35,
        trim = TRUE,
        width = 0.9
      ) +
      ggplot2::geom_point(
        data = hist_plot_df,
        ggplot2::aes(x = x, y = Value, fill = Value),
        shape = 21,
        color = grDevices::adjustcolor("#c45c00", alpha.f = 0.55),
        size = 1.85,
        alpha = hist_cloud_alpha,
        stroke = 0.25,
        position = ggplot2::position_jitter(width = 0.07, height = 0, seed = 11L)
      )
  }

  if (nrow(app_df) >= 3L) {
    app_plot_df <- data.frame(x = x_overlay, Value = app_df$Value)
    p <- p +
      ggplot2::geom_violin(
        data = app_plot_df,
        ggplot2::aes(x = x, y = Value),
        fill = "#9ec5e8",
        color = grDevices::adjustcolor("#1f5f99", alpha.f = 0.65),
        alpha = 0.38,
        linewidth = 0.45,
        trim = TRUE,
        width = 0.9
      )
  }

  if (nrow(app_df) >= 1L) {
    p <- p +
      ggplot2::geom_point(
        data = app_df,
        ggplot2::aes(x = x_overlay, y = Value, fill = Value),
        shape = 21,
        color = "#1f5f99",
        size = 3.25,
        alpha = 0.96,
        stroke = 0.75,
        position = ggplot2::position_jitter(width = 0.045, height = 0, seed = 7L)
      )
  }

  p <- p +
    ggplot2::scale_fill_gradientn(
      colors = pal_cols,
      limits = val_rng,
      name = "Value",
      na.value = "#b0b8c4"
    ) +
    ggplot2::scale_x_continuous(
      breaks = NULL,
      labels = NULL,
      limits = c(0.52, 1.48),
      expand = ggplot2::expansion(mult = 0.02)
    ) +
    ggplot2::labs(title = layer_name, subtitle = subtitle, x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 17, color = "#1b3f66", margin = ggplot2::margin(b = 4)),
      plot.subtitle = ggplot2::element_text(size = 11, color = "#5a6d82", margin = ggplot2::margin(b = 8)),
      plot.background = ggplot2::element_rect(fill = "#eef2f7", color = "#d5dee8", linewidth = 0.4),
      panel.background = ggplot2::element_rect(fill = "#f7f9fc", color = NA),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(8, 92, 8, 8),
      legend.position = c(0.99, 0.99),
      legend.justification = c(1, 1),
      legend.box = "vertical",
      legend.background = ggplot2::element_rect(
        fill = grDevices::adjustcolor("white", alpha.f = 0.9),
        color = "#cfd8e3",
        linewidth = 0.35
      ),
      legend.margin = ggplot2::margin(4, 6, 4, 6),
      legend.text = ggplot2::element_text(size = 10),
      legend.title = ggplot2::element_text(size = 10, face = "bold")
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        title = "Value",
        barheight = ggplot2::unit(36, "pt"),
        barwidth = ggplot2::unit(5.5, "pt"),
        frame.colour = "#cfd8e3",
        ticks.colour = "#6b7c93"
      )
    )

  p
}

REPORT_MAX_DIST_PLOTS <- 20L
REPORT_MAX_FIELD_COMPARE_PLOTS <- 4L
REPORT_POP_CELLS_PER_LAYER <- 6000L

COMPARE_LOOP_METRIC_COLS <- c(
  "Univariate", "Multivariate", "PCA_Coverage", "Range_Coverage",
  "Correlation_Preservation", "Spatial_Coverage"
)
COMPARE_LOOP_METRIC_WEIGHTS <- c(
  Univariate = 0.15,
  Multivariate = 0.15,
  PCA_Coverage = 0.15,
  Range_Coverage = 0.22,
  Correlation_Preservation = 0.15,
  Spatial_Coverage = 0.18
)

COMPARE_METRIC_COLS <- COMPARE_LOOP_METRIC_COLS
COMPARE_METRIC_WEIGHTS <- COMPARE_LOOP_METRIC_WEIGHTS
COMPARE_WEIGHTS_LABEL <- "covariate balance (range & quantile tails 0.22); modest spatial/field coverage (0.18) to limit bias toward spread-only designs"

compare_metric_display_name <- function(col) {
  nm <- c(
    Univariate = "KS / Univariate",
    Multivariate = "Multivariate (Mahalanobis)",
    PCA_Coverage = "PCA coverage",
    Range_Coverage = "Range & quantile tails",
    Correlation_Preservation = "Correlation",
    Spatial_Coverage = "Spatial coverage"
  )
  unname(nm[col])
}

compare_raw_metrics_display_df <- function(d) {
  d <- as.data.frame(d)
  label_map <- c(
    Method = "Method",
    Rank = "Rank",
    Univariate = "Univariate (higher is better)",
    Multivariate = "Multivariate (higher is better)",
    PCA_Coverage = "PCA coverage (higher is better)",
    Range_Coverage = "Range & quantile tails (higher is better)",
    Correlation_Preservation = "Correlation (higher is better)",
    Spatial_Coverage = "Spatial coverage (higher is better)",
    Final_Score = "Final score (higher is better)",
    Final_Score_SD = "Score SD across repeats (lower is better)"
  )
  new_names <- vapply(names(d), function(nm) {
    if (!is.na(label_map[nm])) return(unname(label_map[nm]))
    nm
  }, character(1))
  names(d) <- new_names
  d
}

compare_method_short_name <- function(x) {
  x <- as.character(x)
  ifelse(
    x == "Hybrid Zonal cLHS", "HZC",
    ifelse(x == "Spread + cLHS", "S+cLHS", x)
  )
}

clhs_sample_indices <- function(clhs_res) {
  if (is.null(clhs_res)) return(integer(0))
  if (is.list(clhs_res)) {
    if (!is.null(clhs_res$index_samples)) return(as.integer(clhs_res$index_samples))
    if (!is.null(clhs_res$indices)) return(as.integer(clhs_res$indices))
    if (length(clhs_res) == 1L && is.atomic(clhs_res[[1L]])) return(as.integer(clhs_res[[1L]]))
    return(integer(0))
  }
  if (is.matrix(clhs_res) || (is.data.frame(clhs_res) && ncol(clhs_res) == 1L)) {
    return(as.integer(clhs_res[, 1L]))
  }
  as.integer(clhs_res)
}

compare_ensure_n_indices <- function(idx, pop_n, n_need) {
  idx <- as.integer(idx)
  n_need <- as.integer(n_need)
  pop_n <- as.integer(pop_n)
  idx <- unique(idx[idx >= 1L & idx <= pop_n])
  if (length(idx) >= n_need) return(idx[seq_len(n_need)])
  rem <- setdiff(seq_len(pop_n), idx)
  extra <- min(length(rem), n_need - length(idx))
  if (extra > 0L) idx <- c(idx, sample(rem, extra))
  unique(idx)
}

pick_fair_sample_size <- function(candidates, evaluated_sizes) {
  candidates <- as.integer(unique(candidates[!is.na(candidates)]))
  if (!length(candidates)) return(NA_integer_)
  if (length(candidates) == 1L) return(candidates[1L])
  mid <- stats::median(as.numeric(evaluated_sizes), na.rm = TRUE)
  candidates[which.min(abs(candidates - mid))]
}

compute_compare_composite <- function(df, metric_cols = COMPARE_METRIC_COLS, w = COMPARE_METRIC_WEIGHTS) {
  wt <- as.numeric(w[metric_cols])
  as.vector(as.matrix(df[, metric_cols, drop = FALSE]) %*% matrix(wt, ncol = 1))
}

metric_ranks_to_scores <- function(values, n_methods = length(values)) {
  r <- rank(-as.numeric(values), ties.method = "average", na.last = TRUE)
  (n_methods - r) / max(1L, n_methods - 1L)
}

apply_rank_based_final_score <- function(mdf, metric_cols = COMPARE_METRIC_COLS, w = COMPARE_METRIC_WEIGHTS) {
  n_methods <- nrow(mdf)
  rank_mat <- mdf[, metric_cols, drop = FALSE]
  for (mc in metric_cols) {
    rank_mat[[mc]] <- metric_ranks_to_scores(mdf[[mc]], n_methods = n_methods)
  }
  compute_compare_composite(rank_mat, metric_cols = metric_cols, w = w)
}

build_winner_gap_table <- function(methods_df, metric_cols = COMPARE_METRIC_COLS) {
  if (is.null(methods_df) || nrow(methods_df) < 2L) return(NULL)
  win_row <- methods_df[1L, , drop = FALSE]
  run_row <- methods_df[2L, , drop = FALSE]
  win <- as.character(win_row$Method[1])
  runner <- as.character(run_row$Method[1])
  win_vals <- suppressWarnings(as.numeric(unlist(win_row[, metric_cols, drop = FALSE])))
  run_vals <- suppressWarnings(as.numeric(unlist(run_row[, metric_cols, drop = FALSE])))
  gaps <- data.frame(
    Metric = vapply(metric_cols, compare_metric_display_name, character(1)),
    Winner = win,
    Runner_up = runner,
    Winner_value = win_vals,
    Runner_up_value = run_vals,
    Gap = win_vals - run_vals,
    stringsAsFactors = FALSE
  )
  gaps[, c("Winner_value", "Runner_up_value", "Gap")] <- round(gaps[, c("Winner_value", "Runner_up_value", "Gap")], 3)
  gaps
}

format_winner_gap_narrative <- function(gaps_df) {
  if (is.null(gaps_df) || !nrow(gaps_df)) return(character(0))
  win <- compare_method_short_name(gaps_df$Winner[1])
  runner <- compare_method_short_name(gaps_df$Runner_up[1])
  parts <- vapply(seq_len(nrow(gaps_df)), function(i) {
    g <- gaps_df$Gap[i]
    if (!is.finite(g) || abs(g) < 5e-4) return(NA_character_)
    verb <- if (g > 0) "leads" else "trails"
    sprintf("%s %s %s by %+0.3f on %s", win, verb, runner, g, gaps_df$Metric[i])
  }, character(1))
  parts <- parts[!is.na(parts) & nzchar(parts)]
  if (!length(parts)) {
    return(paste0(win, " and ", runner, " are effectively tied across all metrics at the recommended sample size."))
  }
  paste0(paste(parts, collapse = ", "), ".")
}

build_comparison_curve_plot <- function(res) {
  if (is.null(res) || is.null(res$all_summary) || nrow(res$all_summary) < 1L) return(NULL)
  ggplot2::ggplot(res$all_summary, ggplot2::aes(x = Sample_Size, y = Final_Score, color = Method, group = Method)) +
    ggplot2::geom_line(linewidth = 0.95) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::geom_vline(xintercept = res$n_points, linetype = "dashed", color = "#c0392b") +
    ggplot2::labs(
      title = "Technique comparison by method",
      subtitle = paste0("Recommended n = ", res$n_points),
      x = "Sample size",
      y = "Final score (rank-based composite, 0–1)"
    ) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = "#1b3f66"),
      plot.subtitle = ggplot2::element_text(color = "#6b7c93", size = 10),
      legend.position = "bottom"
    )
}

draw_report_ggplot_page <- function(p, title = NULL, description = NULL) {
  if (is.null(p)) return(invisible(FALSE))
  grid::grid.newpage()
  y <- 0.96
  if (!is.null(title) && nzchar(as.character(title))) {
    grid::grid.text(
      as.character(title),
      x = 0.5, y = y,
      gp = grid::gpar(fontsize = 13, fontface = "bold", col = "#1b3f66")
    )
    y <- y - 0.045
  }
  if (!is.null(description) && nzchar(as.character(description))) {
    wrapped <- unlist(strwrap(as.character(description), width = 108))
    for (i in seq_along(wrapped)) {
      grid::grid.text(
        wrapped[[i]],
        x = 0.5, y = y - (i - 1L) * 0.028,
        gp = grid::gpar(fontsize = 8.2, col = "gray35")
      )
    }
    y <- y - length(wrapped) * 0.028 - 0.02
  }
  vp_h <- max(0.55, min(0.82, y - 0.06))
  vp_y <- max(0.32, vp_h / 2)
  vp <- grid::viewport(x = 0.5, y = vp_y, width = 0.9, height = vp_h)
  grid::pushViewport(vp)
  grid::grid.draw(ggplot2::ggplotGrob(p))
  grid::popViewport()
  invisible(TRUE)
}

draw_report_page_footer <- function(description) {
  if (is.null(description) || !nzchar(as.character(description))) return(invisible(NULL))
  wrapped <- paste(strwrap(as.character(description), width = 108), collapse = "\n")
  graphics::par(xpd = NA)
  graphics::mtext(wrapped, side = 1, line = 0.35, cex = 0.62, col = "gray35")
  invisible(TRUE)
}

draw_report_section_page <- function(heading, subtitle = NULL, bullets = character(0)) {
  if (is.null(heading) || !nzchar(as.character(heading))) return(invisible(NULL))
  grid::grid.newpage()
  grid::grid.rect(
    x = 0.5, y = 0.9, width = 0.94, height = 0.11,
    gp = grid::gpar(fill = "#e8f2fc", col = "#9bb8d9", lwd = 1.2)
  )
  grid::grid.text(
    as.character(heading),
    x = 0.5, y = 0.9,
    gp = grid::gpar(fontsize = 15.5, fontface = "bold", col = "#1b3f66")
  )
  y <- 0.8
  if (!is.null(subtitle) && nzchar(as.character(subtitle))) {
    for (w in strwrap(as.character(subtitle), width = 100)) {
      grid::grid.text(w, x = 0.5, y = y, gp = grid::gpar(fontsize = 10.2, col = "#4a5f78"))
      y <- y - 0.036
    }
    y <- y - 0.02
  }
  bullets <- as.character(bullets)
  bullets <- bullets[nzchar(bullets)]
  if (length(bullets)) {
    for (ln in bullets) {
      for (w in strwrap(ln, width = 96)) {
        grid::grid.text(
          paste0("\u2022 ", w),
          x = 0.07, y = y, just = "left",
          gp = grid::gpar(fontsize = 9.1, col = "#1f2d3d")
        )
        y <- y - 0.034
      }
      y <- y - 0.01
    }
  }
  grid::grid.segments(
    x0 = 0.06, x1 = 0.94, y0 = 0.14, y1 = 0.14,
    gp = grid::gpar(col = "#cfe6ff", lwd = 2)
  )
  grid::grid.text(
    "GeoSampler field report",
    x = 0.5, y = 0.06,
    gp = grid::gpar(fontsize = 8.5, col = "gray45")
  )
  invisible(TRUE)
}

limit_ggplot_list <- function(plots, max_n) {
  plots <- plots[!vapply(plots, is.null, logical(1))]
  if (length(plots) > as.integer(max_n)) plots <- plots[seq_len(as.integer(max_n))]
  plots
}

ggplot_list_to_report_plot_pages <- function(
  plots,
  title_prefix,
  description,
  kind = "distribution",
  max_n = REPORT_MAX_DIST_PLOTS
) {
  plots <- limit_ggplot_list(plots, max_n)
  if (!length(plots)) return(list())
  lapply(plots, function(p) {
    ttl <- tryCatch(as.character(p$labels$title), error = function(e) "Covariate")
    list(
      kind = kind,
      title = paste0(title_prefix, " \u2014 ", ttl),
      description = description,
      plot = p
    )
  })
}

draw_distribution_plot_stack <- function(plots) {
  plots <- plots[!vapply(plots, is.null, logical(1))]
  n <- length(plots)
  if (n < 1L) return(invisible(NULL))
  grobs <- lapply(plots, ggplot2::ggplotGrob)
  grid::grid.newpage()
  if (n == 1L) {
    grid::grid.draw(grobs[[1]])
    return(invisible(TRUE))
  }
  lay <- grid::grid.layout(nrow = n, ncol = 1, heights = grid::unit(rep(1, n), "null"))
  grid::pushViewport(grid::viewport(layout = lay))
  for (i in seq_len(n)) {
    grid::pushViewport(grid::viewport(layout.pos.row = i, layout.pos.col = 1L))
    grid::grid.draw(grobs[[i]])
    grid::popViewport()
  }
  grid::popViewport()
  invisible(TRUE)
}

draw_report_raster_page <- function(r, title, style = c("ndvi", "elev")) {
  style <- match.arg(style)
  if (is.null(r)) return(invisible(FALSE))
  graphics::par(mar = c(2.4, 2.4, 3.4, 5.8), bg = "white", fg = "black")
  cols <- if (style == "ndvi") report_ndvi_colors(64L) else grDevices::terrain.colors(64L)
  v <- tryCatch(raster::values(r), error = function(e) NULL)
  zlim <- NULL
  if (!is.null(v) && any(is.finite(v))) {
    q <- stats::quantile(v[is.finite(v)], c(0.02, 0.98), na.rm = TRUE, names = FALSE)
    if (all(is.finite(q)) && abs(q[2] - q[1]) > 1e-8) zlim <- q else zlim <- range(v, na.rm = TRUE)
  }
  graphics::plot(r, col = cols, axes = FALSE, legend = FALSE, zlim = zlim,
                 main = title, cex.main = 1.2, font.main = 2, useRaster = TRUE)
  graphics::box(lwd = 1.2, col = "gray70")
  if (!is.null(zlim) && all(is.finite(zlim))) {
    lg <- grDevices::colorRampPalette(cols)(200)
    graphics::legend(
      "right", inset = c(-0.15, 0.02),
      legend = c(sprintf("%.2f", zlim[2]), sprintf("%.2f", zlim[1])),
      fill = c(lg[1], lg[200]), border = NA, bty = "n",
      title = if (style == "ndvi") "NDVI" else "Elev.", cex = 0.72, title.cex = 0.8
    )
  }
  graphics::mtext("GeoSampler field report", side = 1, line = 0.4, cex = 0.65, col = "gray45")
  invisible(TRUE)
}

draw_report_points_page <- function(boundary_sf, points_sf, title = "Sample points and field boundary") {
  if (is.null(boundary_sf) || nrow(boundary_sf) < 1L) return(invisible(FALSE))
  if (is.null(points_sf) || !inherits(points_sf, "sf") || nrow(points_sf) < 1L) return(invisible(FALSE))
  graphics::par(mar = c(2.8, 2.8, 3.4, 2.8), bg = "white", fg = "black")
  b <- tryCatch(sf::st_transform(boundary_sf, 4326), error = function(e) boundary_sf)
  p <- tryCatch(sf::st_transform(points_sf, 4326), error = function(e) points_sf)
  bb <- sf::st_bbox(b)
  pad_x <- max((bb["xmax"] - bb["xmin"]) * 0.1, 1e-5)
  pad_y <- max((bb["ymax"] - bb["ymin"]) * 0.1, 1e-5)
  graphics::plot(
    sf::st_geometry(b), col = "#fdecea", border = "#c0392b", lwd = 2.4,
    xlim = c(bb["xmin"] - pad_x, bb["xmax"] + pad_x),
    ylim = c(bb["ymin"] - pad_y, bb["ymax"] + pad_y),
    main = title, cex.main = 1.2, font.main = 2, axes = TRUE, cex.axis = 0.72,
    xlab = "Longitude", ylab = "Latitude"
  )
  coords <- sf::st_coordinates(p)
  graphics::points(coords[, 1], coords[, 2], pch = 21, bg = "#f4d03f", col = "#1f2d3d", cex = 1.15, lwd = 1.3)
  n_lab <- min(40L, nrow(coords))
  if (n_lab > 0L) {
    for (i in seq_len(n_lab)) {
      graphics::text(coords[i, 1], coords[i, 2], labels = i, cex = 0.52, font = 2, col = "white")
    }
  }
  graphics::legend(
    "bottomleft", bg = adjustcolor("white", 0.85),
    legend = c("Field boundary", "Sample point"),
    lty = c(1, NA), pch = c(NA, 21), pt.bg = c(NA, "#f4d03f"),
    col = c("#c0392b", "#1f2d3d"), bty = "n", cex = 0.78, lwd = 2.2, pt.cex = 1.1
  )
  graphics::mtext(paste0(nrow(p), " sample point(s)"), side = 3, line = 0.15, adj = 1, cex = 0.72, col = "gray35")
  invisible(TRUE)
}

draw_report_points_compare_page <- function(
  boundary_sf,
  app_points_sf = NULL,
  historical_points_sf = NULL,
  title = "App vs historical sample locations"
) {
  if (is.null(boundary_sf) || nrow(boundary_sf) < 1L) return(invisible(FALSE))
  has_app <- !is.null(app_points_sf) && inherits(app_points_sf, "sf") && nrow(app_points_sf) > 0L
  has_hist <- !is.null(historical_points_sf) && inherits(historical_points_sf, "sf") && nrow(historical_points_sf) > 0L
  if (!has_app && !has_hist) return(invisible(FALSE))
  graphics::par(mar = c(2.8, 2.8, 3.4, 2.8), bg = "white", fg = "black")
  b <- tryCatch(sf::st_transform(boundary_sf, 4326), error = function(e) boundary_sf)
  bb <- sf::st_bbox(b)
  pad_x <- max((bb["xmax"] - bb["xmin"]) * 0.1, 1e-5)
  pad_y <- max((bb["ymax"] - bb["ymin"]) * 0.1, 1e-5)
  graphics::plot(
    sf::st_geometry(b), col = "#fdecea", border = "#c0392b", lwd = 2.4,
    xlim = c(bb["xmin"] - pad_x, bb["xmax"] + pad_x),
    ylim = c(bb["ymin"] - pad_y, bb["ymax"] + pad_y),
    main = title, cex.main = 1.15, font.main = 2, axes = TRUE, cex.axis = 0.72,
    xlab = "Longitude", ylab = "Latitude"
  )
  leg <- c("Field boundary")
  lty <- c(1)
  pch <- c(NA)
  pt_bg <- c(NA)
  col <- c("#c0392b")
  if (has_hist) {
    hp <- tryCatch(sf::st_transform(historical_points_sf, 4326), error = function(e) historical_points_sf)
    hc <- sf::st_coordinates(hp)
    graphics::points(hc[, 1], hc[, 2], pch = 21, bg = "#e67e22", col = "#7d3c00", cex = 0.95, lwd = 0.9)
    leg <- c(leg, paste0("Historical (n=", nrow(hp), ")"))
    lty <- c(lty, NA)
    pch <- c(pch, 21)
    pt_bg <- c(pt_bg, "#e67e22")
    col <- c(col, "#7d3c00")
  }
  if (has_app) {
    ap <- tryCatch(sf::st_transform(app_points_sf, 4326), error = function(e) app_points_sf)
    ac <- sf::st_coordinates(ap)
    graphics::points(ac[, 1], ac[, 2], pch = 21, bg = "#5dade2", col = "#1b4f72", cex = 1.05, lwd = 1.1)
    leg <- c(leg, paste0("App sample (n=", nrow(ap), ")"))
    lty <- c(lty, NA)
    pch <- c(pch, 21)
    pt_bg <- c(pt_bg, "#5dade2")
    col <- c(col, "#1b4f72")
  }
  graphics::legend(
    "bottomleft", bg = adjustcolor("white", 0.88),
    legend = leg, lty = lty, pch = pch, pt.bg = pt_bg,
    col = col, bty = "n", cex = 0.76, lwd = 2, pt.cex = 1
  )
  invisible(TRUE)
}

write_sampling_report_pdf <- function(
  file,
  lines,
  point_df = NULL,
  max_rows = 22L,
  report_sections = list()
) {
  grDevices::pdf(file, width = 8.5, height = 11, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)

  graphics::par(mar = c(1.2, 1.2, 2.4, 1.2), bg = "white")
  graphics::plot.new()
  y <- 0.95
  lh <- 0.026
  graphics::text(0.5, y, "GeoSampler — Field sampling plan", cex = 1.35, font = 2, col = "#1b3f66")
  y <- y - 0.04
  graphics::text(0.5, y, paste("Report generated:", format(Sys.time(), "%Y-%m-%d %H:%M")), cex = 0.78, col = "gray40")
  y <- y - 0.035
  graphics::segments(0.04, y, 0.96, y, col = "#cfe6ff", lwd = 1.5)
  y <- y - 0.025
  graphics::text(0.04, y, "Section overview:", adj = c(0, 1), font = 2, cex = 0.8, col = "#1b3f66")
  y <- y - lh * 1.1
  for (sec in report_sections) {
    if (!is.list(sec) || is.null(sec$heading) || !nzchar(as.character(sec$heading))) next
    for (w in strwrap(paste0("\u2022 ", sec$heading), width = 102)) {
      if (y < 0.1) break
      graphics::text(0.05, y, w, adj = c(0, 1), cex = 0.72, col = "#1f2d3d")
      y <- y - lh
    }
  }
  y <- y - 0.01
  for (ln in lines) {
    if (y < 0.1) break
    for (w in strwrap(as.character(ln), width = 105)) {
      if (y < 0.1) break
      graphics::text(0.04, y, w, adj = c(0, 1), cex = 0.74, col = "#1f2d3d")
      y <- y - lh
    }
  }
  if (!is.null(point_df) && nrow(point_df) > 0L) {
    y <- y - 0.015
    graphics::text(0.04, y, "Sample points (coordinates):", adj = c(0, 1), font = 2, cex = 0.82, col = "#1b3f66")
    y <- y - lh
    nshow <- min(as.integer(max_rows), nrow(point_df))
    for (i in seq_len(nshow)) {
      if (y < 0.06) break
      rowtxt <- paste(names(point_df), point_df[i, ], sep = "=", collapse = "  ")
      for (w in strwrap(rowtxt, width = 105)) {
        if (y < 0.06) break
        graphics::text(0.04, y, w, adj = c(0, 1), cex = 0.68, col = "gray25")
        y <- y - lh * 0.92
      }
    }
    if (nrow(point_df) > nshow) {
      graphics::text(
        0.04, max(0.04, y),
        paste0("... ", nrow(point_df) - nshow, " more point(s). Export GeoJSON for the full list."),
        adj = c(0, 1), cex = 0.68, font = 3, col = "gray40"
      )
    }
  }

  for (sec in report_sections) {
    if (!is.list(sec)) next
    map_specs <- sec$map_specs %||% list()
    plot_pages <- sec$plot_pages %||% list()
    n_maps <- sum(vapply(map_specs, function(spec) is.list(spec) && !is.null(spec$type), logical(1)))
    n_plots <- sum(vapply(plot_pages, function(pg) is.list(pg) && !is.null(pg$plot), logical(1)))
    if (n_maps < 1L && n_plots < 1L) next

    tryCatch(
      draw_report_section_page(
        sec$heading,
        subtitle = sec$subtitle %||% NULL,
        bullets = sec$bullets %||% character(0)
      ),
      error = function(e) invisible(NULL)
    )

    for (spec in map_specs) {
      if (!is.list(spec) || is.null(spec$type)) next
      tryCatch({
        if (identical(spec$type, "raster") && !is.null(spec$raster)) {
          draw_report_raster_page(spec$raster, title = spec$title %||% "Map", style = spec$style %||% "ndvi")
        } else if (identical(spec$type, "zones") && !is.null(spec$raster)) {
          draw_report_zones_page(spec$raster, title = spec$title %||% "Field management zones", n_zones = spec$n_zones)
        } else if (identical(spec$type, "points")) {
          draw_report_points_page(spec$boundary, spec$points, title = spec$title %||% "Sample points and field boundary")
        } else if (identical(spec$type, "points_compare")) {
          draw_report_points_compare_page(
            spec$boundary,
            app_points_sf = spec$app_points,
            historical_points_sf = spec$historical_points,
            title = spec$title %||% "App vs historical sample locations"
          )
        }
        draw_report_page_footer(spec$description)
      }, error = function(e) invisible(NULL))
      tryCatch(gc(verbose = FALSE), error = function(e) invisible(NULL))
    }

    for (pg in plot_pages) {
      if (!is.list(pg) || is.null(pg$plot)) next
      tryCatch({
        draw_report_ggplot_page(pg$plot, title = pg$title %||% NULL, description = pg$description %||% NULL)
      }, error = function(e) invisible(NULL))
      tryCatch(gc(verbose = FALSE), error = function(e) invisible(NULL))
    }
  }
}

`%||%` <- function(x, y) if (is.null(x)) y else x

boundary_action_ui <- function(input_id) {
  div(
    class = "boundary-action-row",
    actionButton(input_id, "Add Boundary to Map", class = "btn-add-boundary btn-sm"),
    tags$small(class = "hint-micro", "Click the button if the boundary is not shown automatically.")
  )
}

section_title_ui <- function(title) {
  div(class = "section-title", title)
}

main_tab_title <- function(label, tip = NULL, icon_name = NULL) {
  attrs <- list(class = "geo-main-tab-label")
  if (!is.null(tip) && nzchar(as.character(tip)[1L])) {
    attrs[["data-tab-tip"]] <- as.character(tip)[1L]
  }
  children <- list()
  if (!is.null(icon_name) && nzchar(as.character(icon_name)[1L])) {
    children[[length(children) + 1L]] <- icon(as.character(icon_name)[1L], class = "geo-main-tab-icon")
  }
  children[[length(children) + 1L]] <- tags$strong(label)
  do.call(tags$span, c(attrs, children))
}

subtab_tab_title <- function(label, tip = NULL) {
  attrs <- list()
  if (!is.null(tip) && nzchar(as.character(tip)[1L])) {
    attrs[["data-tab-tip"]] <- as.character(tip)[1L]
  }
  do.call(tags$span, c(attrs, list(label)))
}

wf_tree_btn <- function(id, label, hint = NULL, extra_class = "") {
  btn <- actionButton(
    inputId = id,
    label = label,
    class = paste("workflow-tree-btn", extra_class)
  )
  btn$attribs$`data-wf-tip` <- hint %||% label
  btn
}

welcome_workflow_tree_ui <- function() {
  div(
    class = "workflow-tree-panel",
    div(
      class = "workflow-one-boundary-notice",
      strong("Important: "),
      "This app follows a one-boundary-at-a-time workflow. Keep one active AOI boundary, then clear or replace it when switching to a different field."
    ),
    h4(class = "workflow-tree-heading", icon("sitemap"), " Workflow map"),
    p(
      class = "workflow-tree-lead",
      "Follow the tree from left to right. ",
      strong("Hover"),
      " any step for what it does; ",
      strong("click"),
      " to open that tab or section."
    ),
    div(
      class = "workflow-tree-scroll",
      div(
        class = "workflow-tree",
        div(
          class = "wf-stage",
          div(class = "wf-stage-badge", "1"),
          div(class = "wf-stage-title", "Boundary"),
          div(class = "wf-stage-body",
              div(class = "wf-node-row wf-node-row-center",
                  wf_tree_btn("wf_nav_boundary", "Boundary", "Define your field area of interest (AOI). Every map, download, and sample point is clipped to this polygon.", "wf-node-primary")),
              div(class = "wf-connector wf-connector-split"),
              div(class = "wf-node-row",
                  wf_tree_btn("wf_nav_boundary_digitize", "Draw on map", "Boundary tab: draw a polygon or rectangle on the map, then Finish and Done to save one active AOI.", "wf-node-leaf"),
                  wf_tree_btn("wf_nav_boundary_upload", "Upload file", "Boundary tab: import an existing polygon (GeoJSON preferred, or KML/KMZ/zipped shapefile).", "wf-node-leaf")
              )
          )
        ),
        div(class = "wf-stage-join", HTML("&#8594;")),
        div(
          class = "wf-stage wf-stage-wide",
          div(class = "wf-stage-badge", "2"),
          div(class = "wf-stage-title", "Variables"),
          div(class = "wf-stage-body",
              div(class = "wf-node-row wf-node-row-center",
                  wf_tree_btn("wf_nav_variables_imagery", "Imagery", "Imagery tab: retrieve Planet or Sentinel-2, or upload GeoTIFF stacks, then compute vegetation indices (NDVI, NDRE, GNDVI, etc.) for sampling.", "wf-node-primary")),
              div(class = "wf-connector wf-connector-split"),
              div(class = "wf-node-row",
                  wf_tree_btn("wf_nav_imagery_planet", "Planet", "Order PlanetScope imagery (API key required). Orders can take 10–15 minutes; data clips to your boundary.", "wf-node-leaf"),
                  wf_tree_btn("wf_nav_imagery_sentinel", "Sentinel-2", "Free Sentinel-2: set dates and cloud limit, Search, then retrieve a scene or composite inside your AOI.", "wf-node-leaf"),
                  wf_tree_btn("wf_nav_imagery_upload", "Upload imagery", "Upload your own multispectral GeoTIFF (stack or individual bands), assign bands, and derive VIs.", "wf-node-leaf")
              ),
              div(class = "wf-connector wf-connector-split wf-connector-narrow"),
              div(class = "wf-node-row",
                  wf_tree_btn("wf_nav_sentinel_single", "Single scene", "Sentinel mode: pick one acquisition from search results—best when you want a specific date.", "wf-node-leaf wf-node-sentinel"),
                  wf_tree_btn("wf_nav_sentinel_median", "Per-pixel median", "Sentinel mode: median composite across the date window—good for seasonal/crop-period summaries.", "wf-node-leaf wf-node-sentinel")
              ),
              div(class = "wf-connector wf-connector-merge"),
              div(class = "wf-node-row wf-node-row-center",
                  tags$span(class = "wf-node-hint", title = "After imagery loads, use the VI buttons on the Imagery map to add NDVI, NDRE, GNDVI, OSAVI, and related layers.", "Compute VIs: NDVI, NDRE, GNDVI, OSAVI, …")),
              hr(class = "wf-stage-divider"),
              div(class = "wf-node-row wf-node-row-center",
                  wf_tree_btn("wf_nav_variables_elevation", "Elevation", "Elevation tab: get a DEM for your AOI, then compute slope, aspect, TPI, and TWI as sampling covariates.", "wf-node-primary")),
              div(class = "wf-connector wf-connector-split"),
              div(class = "wf-node-row",
                  wf_tree_btn("wf_nav_elevation_download", "Retrieve DEM", "Download AWS terrain tiles for your boundary (can take several minutes on large AOIs).", "wf-node-leaf"),
                  wf_tree_btn("wf_nav_elevation_upload", "Upload DEM", "Upload one elevation GeoTIFF only; then click Calculate Slope, Aspect, TPI, TWI on the map panel.", "wf-node-leaf")
              ),
              div(class = "wf-connector wf-connector-merge"),
              div(class = "wf-node-row wf-node-row-center",
                  tags$span(class = "wf-node-hint", title = "Terrain derivatives are built from the DEM with one button—no need to upload slope/aspect/TPI/TWI separately.", "Derivatives: Slope, Aspect, TPI, TWI (one-click from DEM)")),
              hr(class = "wf-stage-divider"),
              div(class = "wf-node-row wf-node-row-split2",
                  wf_tree_btn("wf_nav_variables_other", "Other layers", "Other layers tab: add extra GeoTIFF predictors (soil EC, climate, management layers) one at a time.", "wf-node-secondary"),
                  wf_tree_btn("wf_nav_variables_summary", "Variable summary", "Variable summary tab: min, max, mean, and median for every loaded layer inside the AOI (QA before sampling).", "wf-node-secondary")
              )
          )
        ),
        div(class = "wf-stage-join", HTML("&#8594;")),
        div(
          class = "wf-stage",
          div(class = "wf-stage-badge", "3"),
          div(class = "wf-stage-title", "Sampling"),
          div(class = "wf-stage-body",
              div(class = "wf-node-row wf-node-row-center",
                  wf_tree_btn("wf_nav_sampling_compare", "Technique comparison", "Optional: compare six sampling designs across sample sizes to recommend method and n before you generate points.", "wf-node-secondary")),
              div(class = "wf-connector wf-connector-merge"),
              div(class = "wf-node-row wf-node-row-center",
                  wf_tree_btn("wf_nav_sampling_generate", "Generate sample points", "Sampling tab: create automatic designs or place manual markers, review on the map, and export GeoJSON.", "wf-node-primary")),
              div(class = "wf-connector wf-connector-split"),
              div(class = "wf-node-row",
                  wf_tree_btn("wf_nav_sampling_manual", "Manual markers", "Place field points yourself with the map marker tool; extract covariates and download when done.", "wf-node-leaf"),
                  wf_tree_btn("wf_nav_sampling_auto", "Automatic design", "Spread+cLHS, systematic spread, cLHS, zone-based, or hybrid zonal designs using your loaded covariate layers.", "wf-node-leaf")
              ),
              div(class = "wf-connector wf-connector-merge"),
              div(class = "wf-node-row wf-node-row-center",
                  wf_tree_btn("wf_nav_sampling_summary", "Population vs sample", "Summary sub-tab: population vs sample statistics and dual-violin plots to check representativeness.", "wf-node-secondary"))
          )
        ),
        div(class = "wf-stage-join", HTML("&#8594;")),
        div(
          class = "wf-stage wf-stage-final",
          div(class = "wf-stage-badge", "4"),
          div(class = "wf-stage-title", "App vs prior / Cost / Report"),
          div(class = "wf-stage-body",
              div(class = "wf-node-row wf-node-row-stack",
                  wf_tree_btn("wf_nav_field_compare", "App vs prior", "Upload historical field GPS points and compare locations and covariate distributions to new app samples.", "wf-node-secondary"),
                  wf_tree_btn("wf_nav_cost_compare", "Cost", "Compare total sampling cost for your prior grid design vs the app-recommended sample size.", "wf-node-secondary"),
                  wf_tree_btn("wf_nav_report", "Report", "Preview and download a one-page field sampling PDF (boundary, zones, points, key maps).", "wf-node-secondary")
              )
          )
        )
      )
    )
  )
}

# --- UI Definition ---
ui <- fluidPage(
  theme = app_theme,
  useShinyjs(),
  
  # Keep-alive script to prevent idle disconnect
  tags$head(
    tags$script(src = "geo_sampler.js?v=20260526j"),
    tags$link(rel = "stylesheet", type = "text/css", href = "geo_sampler.css?v=20260526bo")
  ),
  
  div(
    class = "app-header",
    div(
      class = "geosampler-brand",
      div(
        class = "geosampler-brand-mark",
        tags$span(class = "geosampler-brand-g", "G"),
        tags$span(class = "geosampler-brand-s", "S")
      ),
      div(
        class = "geosampler-brand-copy",
        div(class = "geosampler-brand-name", "GeoSampler"),
        div(class = "geosampler-brand-subtitle", "SPATIAL INTELLIGENCE")
      )
    ),
    div(
      class = "app-header-reset",
      actionButton(
        "reload_app_session",
        label = tagList(icon("rotate-right"), tags$span("Reset")),
        class = "btn-reset-session",
        title = "Reset",
        onclick = paste0(
          "try{sessionStorage.setItem('geosampler_reload_active','1');}catch(e){};",
          "if(window.GeoSamplerReloadOverlay){GeoSamplerReloadOverlay.begin();}",
          "return true;"
        )
      )
    )
  ),
  
  tags$div(
    id = "geosampler-reload-overlay",
    class = "geosampler-reload-overlay",
    role = "dialog",
    `aria-modal` = "true",
    `aria-labelledby` = "geosampler-reload-title",
    `aria-hidden` = "true",
    tags$div(
      class = "geosampler-reload-overlay-card",
      icon("rotate-right", class = "fa-spin geosampler-reload-overlay-icon"),
      tags$p(id = "geosampler-reload-title", class = "geosampler-reload-overlay-title", "Reloading GeoSampler"),
      tags$p(
        class = "geosampler-reload-overlay-text",
        "Clearing maps, layers, caches, and memory. Please do not use the app for 10–15 seconds while the session restarts."
      ),
      tags$ul(
        class = "geosampler-reload-steps",
        tags$li(class = "geosampler-reload-step is-active", `data-step` = "1", "Clearing maps, layers, and caches"),
        tags$li(class = "geosampler-reload-step", `data-step` = "2", "Freeing memory and preparing a fresh session"),
        tags$li(class = "geosampler-reload-step", `data-step` = "3", "Reloading the page and reconnecting")
      ),
      tags$p(class = "geosampler-reload-overlay-sub", "Please do not click tabs or buttons until this message disappears.")
    )
  ),
  shiny::div(
    style = "display:none;",
    textInput("geosampler_post_reload_ping", label = NULL, value = "")
  ),
  
  tags$div(
    id = "force-dashboard-status",
    style = "display:none; margin: 6px 8px 0 8px; padding: 8px 12px; border-radius: 8px; background: #eef6ff; border: 1px solid #bdd7ff; color: #174b7c; font-weight: 600;"
  ),
  
  tabsetPanel(
    id = "main_tabs",
    # Welcome Tab
    tabPanel(
      title = main_tab_title("Dashboard", "Overview of the app workflow and guidance.", "house"),
      value = "Welcome",
          div(class = "main-panel", style = "margin: 14px 0 6px 0; width: 100%;",
          div(class = "welcome-hero",
              h2("Welcome to GeoSampler"),
              welcome_workflow_tree_ui(),
              uiOutput("boundary_risk_banner_welcome"),
              p(class = "welcome-subtitle", "A modern geospatial sampling workspace for field planning, remote sensing analysis, and statistically defensible point design."),
              p("GeoSampler helps you move from area definition to export-ready sample locations in one guided flow. You can combine satellite imagery, elevation-derived predictors, and custom rasters, then generate sampling points that are either random or covariate-representative."),
              p(strong("Recommended first step: "), "click ", strong("Draw on map"), " or ", strong("Upload file"), " in the workflow map above, then follow the tree left to right.")
          ),
          fluidRow(class = "welcome-grid-row",
                   column(6, class = "welcome-grid-col",
                          tags$details(class = "welcome-card disclosure-card", open = "open",
                                       tags$summary("Quick Start Workflow"),
                                       div(class = "welcome-card-body",
                                           tags$ol(
                                             tags$li(strong("Boundary"), ": Draw or upload one AOI polygon; editing keeps one active boundary."),
                                             tags$li(strong("Variables & derivatives"), ": Imagery, elevation, other layers, and variable summary."),
                                             tags$li(strong("Sampling"), ": Technique comparison (optional), generate sample points, population vs sample review."),
                                            tags$li(strong("App vs prior"), ": Upload prior field GPS points; compare map and dual-violin distributions to new app samples."),
                                             tags$li(strong("Cost comparison"), ": Compare total sampling cost for prior grid vs app-recommended sample size."),
                                             tags$li(strong("Report (PDF)"), ": Field-plan PDF; GeoJSON boundary/points from Boundary and Sampling.")
                                           )
                                       )
                          ),
                          tags$details(class = "welcome-card disclosure-card", open = "open",
                                       tags$summary("What Each App Area Does"),
                                       div(class = "welcome-card-body",
                                           tags$ul(
                                             tags$li(strong("Boundary tab"), ": area setup, AOI cleanup/recreate, area summary, export."),
                                             tags$li(strong("Imagery"), ": Planet/Sentinel retrieval, local multispectral uploads, VI generation."),
                                             tags$li(strong("Elevation"), ": DEM retrieval/upload, one-click terrain derivatives."),
                                             tags$li(strong("Other layers"), ": add supporting GeoTIFF predictors for sampling."),
                                             tags$li(strong("Variable summary"), ": min/max/mean/median QA table inside the AOI."),
                                             tags$li(strong("Technique comparison"), ", ", strong("Generate sample points"), ", ", strong("Population vs sample"), " under Sampling."),
                                            tags$li(strong("App vs prior"), ": historical vs app sample locations and covariate distributions."),
                                             tags$li(strong("Cost comparison"), ": bar chart and percent savings for prior vs app sampling cost."),
                                             tags$li(strong("Report (PDF)"), ": field-plan PDF preview and download.")
                                           )
                                       )
                          ),
                          tags$details(class = "welcome-card disclosure-card", open = "open",
                                       tags$summary("Sampling Methods and Expectations"),
                                       div(class = "welcome-card-body",
                                           tags$ul(
                                             tags$li(strong("Simple Random"), ": uniform spatial random points inside AOI."),
                                             tags$li(strong("Systematic spread"), ": even geographic coverage via spatial stratification (not covariate-optimized)."),
                                             tags$li(strong("cLHS / zonal"), ": covariate-representative or zone-balanced designs; rasters harmonize to common CRS/extent and coarsest resolution first."),
                                             tags$li("Use ", strong("Technique comparison"), " when unsure which design fits; use simple random when speed is the priority.")
                                           )
                                       )
                          )
                   ),
                   column(6, class = "welcome-grid-col",
                          tags$details(class = "welcome-card disclosure-card", open = "open",
                                       tags$summary("Detailed Operating Guide"),
                                       div(class = "welcome-card-body",
                                           tags$ol(
                                             tags$li(strong("AOI setup"), ": keep geometry simple; very large AOIs can increase retrieval and sampling runtime."),
                                             tags$li(strong("Remote retrieval"), ": choose date range/cloud constraints carefully; broader windows improve coverage."),
                                             tags$li(strong("Layer QA"), ": after loading, verify visual footprint and value range before sampling."),
                                             tags$li(strong("Sampling prep"), ": ensure relevant predictors are loaded and meaningful for your field objective."),
                                             tags$li(strong("Output review"), ": inspect density plots and sampled attribute ranges before export.")
                                           )
                                       )
                          ),
                          tags$details(class = "welcome-card disclosure-card", open = "open",
                                       tags$summary("Data Standards and Best Practices"),
                                       div(class = "welcome-card-body",
                                           tags$ul(
                                             tags$li("Raster upload controls accept ", strong("GeoTIFF"), " only; avoid vector inputs there."),
                                             tags$li("Keep consistent CRS when preparing external rasters to reduce reprojection artifacts."),
                                             tags$li("Use ", strong("Zoom to Area"), " after imports if layers appear off-screen (Imagery: button below the map)."),
                                             tags$li("Keep a fixed random seed for reproducible point generation runs."),
                                             tags$li("After clearing Sentinel sessions, wait briefly before new retrieval.")
                                           )
                                       )
                          ),
                          tags$details(class = "welcome-card disclosure-card", open = "open",
                                       tags$summary("Export Products and Typical Use"),
                                       div(class = "welcome-card-body",
                                           tags$ul(
                                             tags$li(strong("Boundary GeoJSON"), " for planning boundaries in external GIS."),
                                             tags$li(strong("GeoTIFF layers"), " for raster analysis and archived covariate stacks."),
                                             tags$li(strong("Sample points + extracted values"), " for field campaigns and lab sheets."),
                                             tags$li(strong("Comparative reruns"), " by changing seed/method and comparing distributions.")
                                           )
                                       )
                          )
                   )
          ),
          div(class = "welcome-hero", style = "margin-top: 12px;",
              h4(style = "margin-top: 0; color: #174b7c;", "Practical Next Action"),
              p("Go to ", strong("Boundary"), ", draw/upload your AOI, then proceed to ", strong("Variables & derivatives"), " and load your first raster. Once that looks correct on map, open ", strong("Sampling"), " → ", strong("Generate sample points"), " for a pilot set."),
              p(strong("Tip:"), " Hover workflow steps and tabs for purpose hints. In ", strong("Technique comparison"), " use ", strong("Recommend Zones"), " and the ", strong("WSS Curve"), " to inspect zone count.")
          )
      )
    ),
    # Boundary Tab
    tabPanel(
      title = main_tab_title("Boundary", "Define one AOI (draw or upload). All maps and downloads use this boundary.", "draw-polygon"),
      value = "Boundary",
      sidebarLayout(
        sidebarPanel(
          class = "sidebar-panel",
          tags$details(class = "tips-highlight",
            tags$summary(strong("Tips and Instructions")),
            tags$ul(
              tags$li(strong("Start here first:"), " define one AOI before using Variables or Sampling."),
              tags$li("Choose ", strong("digitize"), " to draw on-map, or ", strong("upload"), " for an existing boundary file."),
              tags$li("For ", strong("digitize"), ": set location, draw polygon/rectangle, click ", strong("Finish"), " and then ", strong("Done"), " to save edits."),
              tags$li("For ", strong("upload"), ": use polygon vector files only (GeoJSON/KML/KMZ/ZIP shapefile). GeoJSON is preferred."),
              tags$li("Use ", strong("Download GeoJSON of Boundary"), " to keep a copy of the active AOI for reproducibility."),
              tags$li("Only one AOI is active at a time; clear/delete and redraw if you need to replace it."),
              tags$li("After AOI is ready, click ", strong("Next: Variables & derivatives"), " to continue workflow.")
            )
          ),
          h3("Boundary Definition"),
          selectInput("boundary_method", "Select Boundary Method:",
                      choices = c("Select an option" = "",
                                  "I want to digitize" = "digitize",
                                  "I have my own boundary layer" = "upload")),
          hr(),
          conditionalPanel(
            condition = "input.boundary_method == 'digitize'",
            h3("Set Map Location"),
            actionButton("use_my_location", "Use My Location", class="btn-success", icon = icon("location-crosshairs")),
            p(em("Click to automatically center the map on your current location")),
            hr(),
            textInput("lat", "Latitude", value = "33.9519"),
            textInput("lon", "Longitude", value = "-83.3576"),
            actionButton("set_location", "Set Location", class="btn-primary"),
            hr(),
            h3("Digitizing Instructions"),
            p("1. Use the drawing toolbar on the map to draw a polygon."),
            p("2. Click 'Finish' (the checkmark) to complete a shape, then 'Done' to save all edits.")
          ),
          conditionalPanel(
            condition = "input.boundary_method == 'upload'",
            p(strong("Note:"), "GeoJSON format is preferred for boundary files."),
            div(class="file-input-row", div(class="file-input-wrapper", fileInput("upload_boundary", paste("Boundary File", VECTOR_UPLOAD_LABEL), multiple = TRUE, accept = VECTOR_UPLOAD_ACCEPT)), actionButton("clear_boundary", "X", class="btn-danger btn-sm clear-file-btn"))
          ),
          hr(),
          downloadButton("download_geojson", "Download GeoJSON of Boundary"),
          textOutput("upload_tip_boundary")
        ),
        mainPanel(
          class = "main-panel",
          h3("Define Your Boundary"),
          div(class = "map-container",
              editModUI("map")
          ),
          div(class = "map-zoom-below", uiOutput("zoom_button_ui_boundary")),
          div(
            class = "boundary-map-status",
          textOutput("digitize_status"),
            textOutput("boundary_area")
          ),
          hr(),
          actionButton("next_to_variables", "Next: Variables & derivatives", class = "btn-success btn-lg")
        )
      )
    ),
    # Variables Tab with Sub-tabs
    tabPanel(
      title = main_tab_title("Variables", "Imagery, elevation, other layers, and variable summary—build covariates before sampling.", "layer-group"),
      value = "Variables",
      tagList(
      uiOutput("boundary_required_banner_variables"),
      tabsetPanel(
        id = "variables_subtabs",
        # Imagery Analysis Sub-tab
        tabPanel(
          "Imagery", icon = icon("satellite-dish"),
          value = "imagery",
          sidebarLayout(
            sidebarPanel(
              class = "sidebar-panel",
              uiOutput("boundary_risk_banner_variables"),
              tags$details(class = "tips-highlight",
                tags$summary(strong("Tips and Instructions")),
                tags$ul(
                  tags$li(strong("Prerequisite:"), " boundary must be defined in Boundary tab."),
                  tags$li("Choose one source: ", strong("Planet"), " (API key), ", strong("Sentinel-2"), " (free), or ", strong("Upload My Own Imagery"), "."),
                  tags$li("For ", strong("Planet"), ": set date/cloud limits, then retrieve. Orders may take several minutes."),
                  tags$li("For ", strong("Sentinel"), ": choose retrieval mode first, run ", strong("Search"), ", review date list, then retrieve."),
                  tags$li("For ", strong("uploads"), ": use GeoTIFF rasters; for stacks, assign NIR/Red/RedEdge/Blue/Green correctly before analysis."),
                  tags$li("After data loads, use ", strong("Select to Display"), " to check bands and run NDVI/NDRE/GNDVI as needed."),
                  tags$li("Use ", strong("Zoom to Area"), " below the Imagery map if layers are off-screen, then proceed to Elevation/Other or Sampling.")
                )
              ),
              selectInput("imagery_source", "Select Imagery Source:",
                          choices = c("Select an option" = "",
                                      "Retrieve Planet Imagery (Requires API Key)" = "download_planet",
                                      "Retrieve Sentinel-2 Imagery (Free, No Key Required)" = "download_sentinel",
                                      "Upload My Own Imagery" = "upload_ms")),
              
              conditionalPanel(
                condition = "input.imagery_source == 'download_planet'",
                div(class = "source-panel",
                  boundary_action_ui("add_boundary_planet"),
                  section_title_ui("Planet Imagery Order"),
                p(strong("Note:"), "Planet requires an API key (sign up at planet.com)."),
                p(strong("Disclaimer:"), "Orders can take 10-15 minutes. This ", strong("retrieves"), " data from Planet's servers to the dashboard."),
                textInput("api_key", "Enter Planet API Key:"),
                textInput("item_name", "Item Type:", value = "PSScene"),
                textInput("product_bundle", "Product Bundle:", value = "analytic_8b_sr_udm2"),
                textInput("asset", "Asset Type:", value = "ortho_udm2"),
                dateRangeInput("date_range", "Select Date Range:", start = Sys.Date() - 30, end = Sys.Date()),
                sliderInput("cloud_limit", "Maximum Cloud Cover:", min = 0, max = 1, value = 0.05, step = 0.01),
                actionButton("download_planet", "Retrieve Planet Data", class="btn-primary"),
                actionButton("stop_planet", "Stop", class = "btn-danger")
                )
              ),
              
              conditionalPanel(
                condition = "input.imagery_source == 'download_sentinel'",
                div(class = "source-panel",
                  boundary_action_ui("add_boundary_sentinel"),
                  section_title_ui("Sentinel-2 Imagery (Free)"),
                p(strong("Note:"), "Sentinel-2 data is free and open. No API key required."),
                p(strong("Info:"), "This ", strong("retrieves"), " data from cloud archives to the dashboard."),
                radioButtons(
                  "sentinel_retrieval_mode",
                  "Retrieve stack as:",
                  choices = c(
                    "Single scene (pick from search results)" = "single",
                    "Per-pixel median across scenes in range (season / crop period summary)" = "median"
                  ),
                  selected = "single"
                ),
                uiOutput("sentinel_lock_info_ui"),
                dateRangeInput("sentinel_date_range", "Select Date Range:", start = Sys.Date() - 30, end = Sys.Date()),
                conditionalPanel(
                  condition = "input.sentinel_retrieval_mode == 'median'",
                    numericInput("sentinel_median_max_scenes", "Max scenes used for median (memory / runtime):", value = 10, min = 2, max = 24, step = 1),
                    tags$p(
                      class = "text-muted",
                      style = "font-size:11px; margin:6px 0 0 0; line-height:1.35;",
                      "Up to N evenly spaced scenes (oldest to newest). ",
                      tags$strong("Raise N"),
                      " for a smoother seasonal median—more memory and runtime. Cloud filter and search cap still apply; widen the date range or cloud % for more candidates."
                    )
                ),
                sliderInput("sentinel_cloud_limit", "Maximum Cloud Cover (%):", min = 0, max = 100, value = 10, step = 1),
                actionButton("search_sentinel", "Search", class="btn-primary"),
                uiOutput("sentinel_search_cap_notice_ui"),
                uiOutput("sentinel_gndvi_rank_ui"),
                uiOutput("sentinel_select_ui"),
                uiOutput("sentinel_retrieve_ui"),
                  uiOutput("sentinel_retrieval_hint_ui"),
                uiOutput("clear_sentinel_ui"),
                actionButton("stop_sentinel", "Stop", class = "btn-danger")
                )
              ),
              # Collapsible Panel for MS Upload
              conditionalPanel(
                condition = "input.imagery_source == 'upload_ms'",
                p("Use this section to upload a ", strong("raster (GeoTIFF)"), " stack or bands. ", strong("Vector"), " files (shapefile, GeoJSON) are not supported here."),
                hr(),
                radioButtons("ms_upload_type", "Select Upload Type:",
                             choices = c("Stacked Raster" = "stack", "Individual Bands" = "individual", "Calculated VIs" = "vi"),
                             selected = "stack"),
                hr(),
                conditionalPanel(
                  condition = "input.ms_upload_type == 'stack'",
                  h3("Upload Raster Stack"),
                  div(class="file-input-row", div(class="file-input-wrapper", fileInput("upload_ms_stack", "Upload Raster Stack/Brick")), actionButton("clear_ms_stack", "X", class="btn-danger btn-sm clear-file-btn")),
                  uiOutput("band_assignment_ui"),
                  actionButton("assign_bands", "Assign Bands", class = "btn-primary")
                ),
                conditionalPanel(
                  condition = "input.ms_upload_type == 'individual'",
                  h3("Upload Individual Bands"),
                  div(class="file-input-row", div(class="file-input-wrapper", fileInput("upload_nir", "NIR Band (.tif, .tiff)")), actionButton("clear_nir", "X", class="btn-danger btn-sm clear-file-btn")),
                  div(class="file-input-row", div(class="file-input-wrapper", fileInput("upload_red", "Red Band (.tif, .tiff)")), actionButton("clear_red", "X", class="btn-danger btn-sm clear-file-btn")),
                  div(class="file-input-row", div(class="file-input-wrapper", fileInput("upload_re", "Red Edge Band (.tif, .tiff)")), actionButton("clear_re", "X", class="btn-danger btn-sm clear-file-btn")),
                  div(class="file-input-row", div(class="file-input-wrapper", fileInput("upload_blue", "Blue Band (.tif, .tiff)")), actionButton("clear_blue", "X", class="btn-danger btn-sm clear-file-btn")),
                  div(class="file-input-row", div(class="file-input-wrapper", fileInput("upload_green", "Green Band (.tif, .tiff)")), actionButton("clear_green", "X", class="btn-danger btn-sm clear-file-btn")),
                  actionButton("stack_bands", "Stack Individual Bands", class = "btn-primary")
                ),
                conditionalPanel(
                  condition = "input.ms_upload_type == 'vi'",
                  h3("Upload Calculated VI Layers"),
                  textInput("vi_layer_name", "VI Layer Name (e.g., NDVI)"),
                  div(class="file-input-row", div(class="file-input-wrapper", fileInput("upload_vi_tif", "Upload VI TIFF")), actionButton("add_vi_layer", "Add VI Layer", class="btn-primary"), actionButton("clear_vi_layers", "Clear All VIs", class="btn-danger clear-file-btn")),
                  hr(),
                  uiOutput("remove_vi_ui")
                ),
                textOutput("upload_tip_imagery")
              )
            ),
            mainPanel(
              class = "main-panel",
              tabsetPanel(
                id = "imagery_view_subtabs",
                selected = "imagery_map_tab",
                imagery_timeseries_view_tab(),
                imagery_map_view_tab(NULL)
              )
            )
          )
        ),
        # Elevation Data Sub-tab
        tabPanel(
          "Elevation", icon = icon("mountain-sun"),
          value = "elevation",
          sidebarLayout(
            sidebarPanel(
              class = "sidebar-panel",
              tags$details(class = "tips-highlight",
                tags$summary(strong("Tips and Instructions")),
                tags$ul(
                  tags$li(strong("Prerequisite:"), " AOI boundary must already exist."),
                  tags$li("Pick source: ", strong("Retrieve Elevation Data"), " or ", strong("Upload My Own Elevation Data"), "."),
                  tags$li("For download: start DEM retrieval and wait for processing."),
                  tags$li("For upload: provide ", strong("one elevation GeoTIFF (DEM)"), " only; avoid vector files."),
                  tags$li("After DEM loads, click ", strong("Calculate Slope, Aspect, TPI, TWI"), " on the map panel—no need to upload derivatives separately."),
                  tags$li("Verify each terrain layer in ", strong("Select Layer to Display"), " before sampling."),
                  tags$li("Use ", strong("Zoom to Area"), " if data is not visible, then continue to Sampling.")
                )
              ),
              selectInput("elevation_source", "Select Elevation Data Source:",
                          choices = c("Select an option" = "",
                                      "Retrieve Elevation Data" = "download_elevation",
                                      "Upload My Own Elevation Data" = "upload_elevation")),
              conditionalPanel(
                condition = "input.elevation_source == 'download_elevation'",
                div(class = "source-panel",
                  boundary_action_ui("add_boundary_elevation"),
                  section_title_ui("Retrieve DEM"),
                  p(strong("Disclaimer:"), "Elevation retrieval can take several minutes."),
                  actionButton("download_elevation", "Retrieve Elevation Data", class="btn-primary"),
                actionButton("stop_elevation", "Stop", class = "btn-danger")
                )
              ),
              conditionalPanel(
                condition = "input.elevation_source == 'upload_elevation'",
                h3("Upload DEM"),
                p(
                  em(
                    "Upload a single ", strong("elevation GeoTIFF (DEM)"), " for your field. ",
                    "After it appears on the map, use ", strong("Calculate Slope, Aspect, TPI, TWI"), " in the viewer panel to build all terrain layers in one click."
                  )
                ),
                div(class="file-input-row", div(class="file-input-wrapper", fileInput("upload_elevation_tif", "Upload Elevation (DEM) GeoTIFF")), actionButton("clear_elevation_tif", "X", class="btn-danger btn-sm clear-file-btn")),
                textOutput("upload_tip_elevation")
              )
            ),
            mainPanel(
              class = "main-panel",
              h3("Elevation Viewer"),
              div(class = "map-container",
                  withSpinner(leafletOutput("elevation_map"))
              ),
              div(
                class = "main-panel-map-follow",
              uiOutput("elevation_info_ui"),
              uiOutput("elevation_selector_ui"),
              uiOutput("elevation_derivative_ui"),
              uiOutput("derivative_download_ui"),
              uiOutput("elevation_download_ui")
              ),
              div(class = "map-zoom-below compact-zoom", uiOutput("zoom_button_ui_elevation"))
            )
          )
        ),
        # Other Layers Sub-tab
        tabPanel(
          "Other layers", icon = icon("layer-group"),
          value = "other_layers",
          sidebarLayout(
            sidebarPanel(
              class = "sidebar-panel",
              tags$details(class = "tips-highlight",
                tags$summary(strong("Tips and Instructions")),
                tags$ul(
                  tags$li(strong("Use this tab for extra predictors"), " such as climate, management, terrain products, or any custom covariates."),
                  tags$li("Provide a clear layer name first, upload GeoTIFF, then click ", strong("Add Layer"), "."),
                  tags$li("Add multiple layers one by one; remove specific layers with the remove selector if needed."),
                  tags$li("Use ", strong("Select Layer to Display"), " to QA each raster before sampling."),
                  tags$li("Keep layer naming consistent to simplify sampling interpretation and exports."),
                  tags$li("After validating layers, proceed to ", strong("Sampling"), " tab.")
                )
              ),
              selectInput("other_source", "Select Data Source:",
                          choices = c("Select an option" = "",
                                      "Upload Raster Layers" = "upload_other")),
              hr(),
              conditionalPanel(
                condition = "input.other_source == 'upload_other'",
                h3("Upload Raster Layers"),
                p(em(strong("GeoTIFF rasters only"), " — e.g. climate, indices. ", strong("Not"), " shapefiles or other vector data.")),
                textInput("other_layer_name", "Layer Name"),
                div(class="file-input-row", div(class="file-input-wrapper", fileInput("upload_other_tif", "Upload TIFF")), actionButton("add_other_layer", "Add Layer", class="btn-primary"), actionButton("clear_other_layers", "Clear All", class="btn-danger clear-file-btn")),
                hr(),
                uiOutput("remove_other_ui"),
                uiOutput("other_download_ui"),
                textOutput("upload_tip_other")
              )
            ),
            mainPanel(
              class = "main-panel",
              h3("Other layers viewer"),
              div(class = "map-container",
                  withSpinner(leafletOutput("other_map"))
              ),
              div(class = "main-panel-map-follow", uiOutput("other_selector_ui")),
              div(class = "map-zoom-below compact-zoom", uiOutput("zoom_button_ui_other"))
            )
          )
        ),
        tabPanel(
          "Variable summary",
          icon = icon("table"),
          value = "var_summary",
          fluidRow(
            column(
              width = 12,
              tags$details(
                class = "tips-highlight disclosure-card",
                open = TRUE,
                tags$summary(strong("Extracted variable summary (within boundary)")),
                p(
                  em(
                    "Min, max, mean, and median use pixels inside the digitized boundary for every loaded layer (imagery, VIs, elevation, derivatives, soil, other uploads). Large layers use a random 150k-pixel subsample for speed."
                  )
                ),
                p(class = "text-muted", style = "font-size:12px; margin-bottom:6px;", "Tip: hover a row in the table for the same floating hint used on tabs (full values for that layer)."),
                actionButton("compute_variables_summary", "Compute / refresh summary", class = "btn-modern btn-one-shot"),
                br(), br(),
                div(
                  class = "variables-summary-table-wrap",
                  style = "overflow-x:auto; border:1px solid #e2ecf7; border-radius:10px; background:#fbfdff; padding:8px 10px;",
                  DT::dataTableOutput("variables_summary_table_out")
                )
              )
            )
          )
        )
        )
      )
    ),
    # Sampling Tab with sub-tabs
    tabPanel(
      title = main_tab_title("Sampling", "Technique comparison, generate points, and population vs sample review.", "location-dot"),
      value = "Sampling",
      tagList(
      uiOutput("boundary_required_banner_sampling"),
      tabsetPanel(
        id = "sampling_subtabs",
        tabPanel(
          "Technique comparison",
          value = "tcmp",
          sidebarLayout(
            sidebarPanel(
              class = "sidebar-panel",
              tags$details(
                class = "disclosure-card",
                tags$summary(strong("What This Does")),
                tags$ul(
                  tags$li("Compares ", strong("Simple Random"), ", ", strong("Systematic Spread"), ", ", strong("Spread + cLHS"), ", ", strong("cLHS"), ", ", strong("Zone-based"), ", and ", strong("Hybrid Zonal cLHS"), " using the ", strong("covariate layers you select below"), " (default: all loaded layers)."),
                  tags$li("The same layer selection drives ", strong("technique comparison"), ", ", strong("Generate sample points"), ", the population vs sample table, and distribution plots."),
                  tags$li("For ", strong("Zone-based"), " and ", strong("Hybrid Zonal cLHS"), ", click ", strong("Recommend Zones"), " once (WSS elbow); that zone count (k) is reused for comparison, sampling, and point generation."),
                  tags$li("Defaults: ", strong("four"), " sample sizes and ", strong("three"), " repeats per method (edit if you need more precision)."),
                  tags$li("Large AOIs: the app may ", strong("randomly subsample field cells"), " for the comparison loop (same logic as full analysis, smaller grid) so cLHS and k-means stay fast and memory-safe. ", strong("Generate sample points"), " still uses the ", strong("full"), " harmonized cell set.")
                )
              ),
              tags$div(
                class = "sampling-segment-box",
                style = "margin:0 0 10px 0;",
                tags$div(class = "sampling-segment-heading", "Covariates (comparison & sampling)"),
                tags$details(
                  class = "covariate-preset-details explore-more-inner",
                  tags$summary("Covariate preset"),
                  tags$div(
                    class = "sampling-segment-body",
                    radioButtons(
                      "covariate_preset_choice",
                      label = NULL,
                      choices = c(
                        "Lean default" = "lean",
                        "Quick scout (NDVI + elevation)" = "scout",
                        "Fertility-focused (NDVI, NDRE, GNDVI + TWI)" = "fertility",
                        "Full variability (all layers)" = "full"
                      ),
                        selected = "lean"
                      ),
                      actionButton("apply_covariate_preset", "Apply preset", class = "btn-modern btn-sm"),
                      tags$p(
                        class = "text-muted",
                        style = "font-size:10px; margin:4px 0 0 0;",
                        "Presets update the checkboxes when you click Apply. Default is ", strong("Lean"), " (fewer correlated VIs)."
                      )
                  )
                ),
                uiOutput("compare_covariate_layers_ui")
              ),
              tags$div(
                style = "margin:6px 0; padding:5px 8px; border:1px solid #f2c97a; border-radius:8px; background:#fff8e9; font-size:10.5px; line-height:1.3; color:#5a4a20;",
                tags$strong(style = "font-size:10.5px;", "Note: "),
                "For ", tags$strong("Zone-based"), " and ", tags$strong("Hybrid Zonal cLHS"), ": click ", tags$strong("Recommend Zones"), " once before comparing; same k for comparison, sampling, and generation."
              ),
              actionButton("recommend_zones", "Recommend Zones", class = "btn-modern btn-sm btn-one-shot"),
              uiOutput("compare_zone_recommend_info"),
              tags$details(
                class = "disclosure-card",
                style = "margin:10px 0; border:1px solid #d8e6fb; border-radius:12px; background:linear-gradient(120deg,#f8fbff 0%,#eef6ff 100%); padding:8px 10px;",
                tags$summary(strong("WSS Curve (Why this zone count was recommended)")),
                plotOutput("compare_zone_wss_plot", height = "180px"),
                tags$p(class = "text-muted", style = "font-size:12px; margin:4px 0 0 0;", "Elbow point in WSS curve supports the recommended zone count.")
              ),
              textInput("compare_sample_sizes", "Sample sizes to evaluate (comma-separated):", value = "20,30,40,50"),
              tags$p(
                class = "text-muted",
                style = "font-size:11px; margin:-6px 0 6px 0;",
                "Default is four sample sizes (lighter on the server). Sizes adjust with AOI area until you edit this field."
              ),
              numericInput("compare_repeats", "Repeats per method per sample size:", value = 3, min = 2, max = 8, step = 1),
              div(
                style = "display:flex; gap:8px; align-items:center; flex-wrap:wrap;",
                actionButton("compare_sampling_methods", "Compare Sampling Techniques", class = "btn-primary btn-one-shot"),
                actionButton("stop_compare_sampling", "Stop Comparison", class = "btn-danger")
              ),
              tags$details(
                class = "disclosure-card",
                style = "margin:10px 0; border:1px solid #d8e6fb; border-radius:12px; background:linear-gradient(120deg,#f8fbff 0%,#eef6ff 100%); padding:8px 10px;",
                tags$summary(strong("Best Sample Size Rules")),
                sliderInput("compare_threshold_pct", "Threshold target (% of max score):", min = 80, max = 99, value = 80, step = 1),
                tags$p(class = "text-muted", style = "font-size:12px; margin-top:-8px;", "Fair pick among sample sizes that meet this fraction of the top final score (not always the smallest)."),
                sliderInput("compare_min_range_coverage", "Minimum acceptable range coverage (%):", min = 70, max = 99, value = 75, step = 1),
                tags$p(class = "text-muted", style = "font-size:12px; margin-top:-8px;", "Fair pick among sizes where at least one repeat meets this minimum on range & quantile-tail coverage (low/high tails of each covariate)."),
                sliderInput("compare_cost_weight", "Cost penalty weight (accuracy vs cost):", min = 0, max = 1, value = 0.5, step = 0.05),
                tags$p(class = "text-muted", style = "font-size:12px; margin-top:-8px;", "Higher weight favors smaller sample sizes when scores are similar.")
              )
            ),
            mainPanel(
              class = "main-panel",
              h3("Sampling Technique Comparison"),
              uiOutput("comparison_headline_ui"),
              uiOutput("comparison_podium_ui"),
              uiOutput("comparison_performance_ui"),
              uiOutput("comparison_method_description_ui"),
              tags$details(
                class = "disclosure-card",
                style = "margin:8px 0; border:1px solid #e2ecf7; border-radius:10px; padding:6px 10px; background:#fbfdff;",
                tags$summary(strong("All methods at recommended sample size (raw metrics)")),
                tags$p(class = "text-muted", style = "font-size:11px; margin:4px 0 6px 0;", "Column headers note direction; all design metrics are higher-is-better except score SD across repeats."),
                DT::dataTableOutput("comparison_methods_at_n_table")
              ),
              tags$details(
                class = "disclosure-card",
                style = "margin:8px 0; border:1px solid #e2ecf7; border-radius:10px; padding:6px 10px; background:#fbfdff;",
                tags$summary(strong("Winner vs population (% difference by covariate)")),
                uiOutput("comparison_covariate_balance_status_ui"),
                DT::dataTableOutput("comparison_covariate_balance_table")
              )
            )
          )
        ),
        tabPanel(
          "Generate sample points",
          value = "samp",
          sidebarLayout(
            sidebarPanel(
              class = "sidebar-panel",
              tags$details(class = "tips-highlight",
                tags$summary(strong("Tips and Instructions")),
                tags$ul(
                  tags$li(strong("Prerequisites:"), " boundary + at least one raster variable should be loaded."),
                  tags$li("Choose ", strong("manual"), " to place field points yourself, or ", strong("automatic"), " for algorithmic point generation."),
                  tags$li("Manual: place markers, save edits, review extracted values and density plots, then download points."),
                  tags$li("Automatic: set point count and method under ", strong("Explore more"), ", then ", strong("Generate sample points"), " — runs ", RECOMMENDED_GENERATION_REPEATS(), " replicates and keeps the best ", strong("field coverage %"), " (except ", strong("Grid-based"), ")."),
                  tags$li("Optional: run ", strong("Technique comparison"), " first, then ", strong("Generate using recommendation"), " for the winning method and ", strong("n"), "."),
                  tags$li("Covariates: choose layers on the ", strong("Technique comparison"), " tab (same set used for sampling and summary plots)."),
                  tags$li("cLHS and zonal methods harmonize the selected layers to a common grid (coarsest resolution anchor)."),
                  tags$li("Use negative buffer to avoid edge effects and set random seed for reproducible outputs."),
                  tags$li("Review generated points on map + table + distributions before final GeoJSON export.")
                )
              ),
              tags$div(
                class = "sampling-segment-box sampling-segment-core",
              h3("Sampling"),
              uiOutput("sampling_best_recommendation_ui"),
                conditionalPanel(
                  condition = "input.sampling_method == 'automatic'",
                  uiOutput("auto_layer_selector_ui")
                ),
              conditionalPanel(
                condition = "input.sampling_method == 'manual'",
                h4("Manual Point Creation"),
                p("Use the marker tool on the map to place points."),
                p("Click 'Finish' then 'Done' to save the points."),
                p(em("Use the eraser from the map if you want to remove your sampling points.")),
                hr(),
                uiOutput("manual_layer_selector_ui"),
                hr(),
                downloadButton("download_manual_points", "Download Manual Points (GeoJSON)")
                )
              ),
              tags$div(
                class = "sampling-segment-box sampling-segment-explore",
                tags$details(
                  class = "explore-more-details",
                  tags$summary(strong("Explore more")),
                  tags$div(
                    class = "sampling-segment-body",
                    selectInput(
                      "sampling_method", "Select Sampling Method:",
                      choices = c(
                        "I want to sample manually" = "manual",
                        "I want to sample automatically" = "automatic"
                      ),
                      selected = "automatic"
              ),
              conditionalPanel(
                condition = "input.sampling_method == 'automatic'",
                tags$details(
                        class = "explore-more-inner",
                  tags$summary(strong("Sampling Parameters")),
                        tags$div(
                          class = "sampling-segment-body",
                  conditionalPanel(
                    condition = "input.sample_type != 'Grid-based'",
                    numericInput("n_points", "Number of Sample Points:", 10, min = 1)
                  ),
                          selectInput(
                            "sample_type", "Select Sampling Type:",
                            choices = c(
                              "Simple Random",
                              "Spread + cLHS (best coverage)",
                              "Systematic spread (coverage)",
                              "Conditioned Latin Hypercube (cLHS)",
                              "Grid-based",
                              "Zone-based",
                              "Hybrid Zonal cLHS"
                            )
                          ),
                  conditionalPanel(
                    condition = "input.sample_type == 'Spread + cLHS (best coverage)'",
                    p(em("Recommended balance: systematic spatial spread to pick a candidate pool, then cLHS within that pool for covariate representativeness."))
                  ),
                  conditionalPanel(
                    condition = "input.sample_type == 'Systematic spread (coverage)'",
                    p(em("K-means XY stratification on the harmonized raster grid (not covariate-optimized like cLHS). Choose this when map coverage matters most."))
                  ),
                  conditionalPanel(
                    condition = "input.sample_type == 'Conditioned Latin Hypercube (cLHS)'",
                            p(em("cLHS uses the covariate layers you select below. Layers are resampled to the coarsest resolution and a common extent before analysis.")),
                            actionButton("build_adaptive_sampling_recommendation", "Build Adaptive Sampling Recommendation", class = "btn-primary btn-sm"),
                            checkboxInput("show_adaptive_similarity_classes", "Show Adaptive Similarity Classes", value = TRUE),
                            p(em("Builds a similarity score map classifying field areas as Similar, Transition, or Dissimilar to your cLHS point profile."))
                  ),
                  conditionalPanel(
                    condition = "input.sample_type == 'Grid-based'",
                    numericInput("grid_size_m", "Grid size (meters, must be > 5):", value = 20, min = 6, step = 1),
                    p(em("Smaller grids create many points and can be computationally heavy."))
                  ),
                  conditionalPanel(
                            condition = "input.sample_type == 'Zone-based' || input.sample_type == 'Hybrid Zonal cLHS'",
                            tags$div(
                              style = "margin:6px 0 8px; padding:5px 8px; border:1px solid #f2c97a; border-radius:8px; background:#fff8e9; font-size:10.5px; line-height:1.3; color:#5a4a20;",
                              tags$strong(style = "font-size:10.5px;", "Note: "),
                              "For ", tags$strong("Zone-based"), " and ", tags$strong("Hybrid Zonal cLHS"), ": click ",
                              tags$strong("Recommend Zones"), " once on the Technique comparison tab before comparing or generating points; same k for comparison, sampling, and generation."
                            ),
                            uiOutput("sampling_zone_controls_ui"),
                            textOutput("zone_recommend_info"),
                            p(em("Zones use k-means on standardized selected covariates. Sample counts per zone follow Neyman allocation (area × within-zone variance), not area alone.")),
                            p(em("Use ", strong("Show zone overlay on map"), " below the sampling map to toggle the zone layer."))
                          )
                  )
                ),
                tags$details(
                        class = "explore-more-inner",
                  tags$summary(strong("Advanced Options")),
                        tags$div(
                          class = "sampling-segment-body",
                          numericInput("buffer_distance", "Negative Buffer (meters):", 10, min = 0, step = 10),
                          p(em("Apply a negative buffer to keep sample points inside the boundary (default 10 m).")),
                  numericInput("random_seed", "Random Seed (for reproducibility):", value = 123, min = 1),
                  p(em("Set a specific seed to get the same random samples each time"))
                        )
                      )
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.sampling_method == 'automatic'",
                uiOutput("sampling_custom_actions_ui")
              ),
              textOutput("upload_tip_sampling")
            ),
            mainPanel(
              class = "main-panel",
              div(id = "manual_sampling_div", style = "display: none;",
                  h3("Manually Digitize Points"),
                  div(class = "map-container",
                      editModUI("sampling_manual_map")
                  ),
                  div(class = "map-zoom-below", uiOutput("zoom_button_ui_sampling_manual")),
                  uiOutput("manual_marker_notice_ui"),
                  hr(),
                  uiOutput("manual_info_ui"),
                  h3("Manual Sampling Points Data"),
                  sampling_points_data_tabs(
                    "manual_sampling_data_tabs",
                    "manual_table",
                    "manual_density_plots",
                    "download_manual_density_plot"
                  )
              ),
              div(id = "auto_sampling_div",
                  h3("Generated Sample Points"),
                  div(class = "map-container",
                      withSpinner(leafletOutput("sampling_auto_map"))
                  ),
                  div(
                    class = "main-panel-map-follow",
                    conditionalPanel(
                      condition = "output.zones_on_map_available",
                      checkboxInput("show_sampling_zones_on_map", "Show zone overlay on map", value = TRUE)
                  ),
                  p(em("Automatic points are editable on the map: use edit/delete/add marker tools, then click Save Auto Point Edits.")),
                  actionButton("save_auto_point_edits", "Save Auto Point Edits", class = "btn-modern btn-sm"),
                  uiOutput("sampling_info_ui"),
                    uiOutput("sampling_quality_dashboard_ui"),
                    uiOutput("adaptive_recommendation_ui"),
                    uiOutput("zonal_cluster_means_ui")
                  ),
                  div(class = "map-zoom-below compact-zoom", uiOutput("zoom_button_ui_sampling_auto")),
                  h3("Automatic Sampling Points Data"),
                  sampling_points_data_tabs(
                    "auto_sampling_data_tabs",
                    "sampling_table",
                    "sampling_density_plots",
                    "download_sampling_density_plot"
                  )
              )
            )
          )
        ),
        tabPanel(
          "Population vs sample",
          value = "samp_summary",
          icon = icon("table-list"),
          sidebarLayout(
            sidebarPanel(
              class = "sidebar-panel",
              tags$details(
                class = "tips-highlight disclosure-card",
                tags$summary(strong("What this shows")),
                tags$ul(
                  tags$li("Population columns summarize ", strong("raster cells inside your AOI"), " on the same harmonized grid used for automatic sampling (up to 150k random cells per layer for speed)."),
                  tags$li("Sample columns summarize ", strong("values at your current sample points"), "; stats are rounded to ", strong("3 decimals"), " and ", strong("% diff"), " is computed from those rounded values (signed % relative to population)."),
                  tags$li("Select covariates and use ", strong("Explore more"), " on ", strong("Generate sample points"), ", then generate or edit points."),
                  tags$li("Open ", strong("Table"), " for population vs sample stats (updates automatically). ", strong("Distribution"), " uses its Compute button for violin plots.")
                )
              )
            ),
            mainPanel(
              class = "main-panel",
              tags$div(
                class = "pop-sample-summary-hero",
                h3(style = "margin-top:0; color:#1b3f66; font-weight:700;", "Population vs sample"),
                p(class = "text-muted", style = "margin-bottom:10px;", "Compare field-scale raster statistics with values at your current sample points.")
              ),
              tabsetPanel(
                id = "summary_subtabs",
                tabPanel(
                  "Table",
                  value = "summary_table_tab",
                  uiOutput("pop_sample_summary_status_ui"),
                  div(
                    class = "pop-sample-summary-table-wrap",
                    withSpinner(DT::dataTableOutput("pop_sample_summary_table"), type = 6)
                  )
                ),
                tabPanel(
                  "Distribution",
                  value = "summary_dist_tab",
                  compute_action_bar("compute_summary_distributions", "Compute distributions", "chart-area"),
                  distribution_download_bar("download_pop_sample_distribution_plot"),
                  tags$p(
                    class = "text-muted",
                    style = "margin:8px 0 10px 0;",
                    "Population values as a faint gray cloud with violin (n ≥ 3); app samples as a blue violin plus points on top (n ≥ 3 for violin). Colors follow each variable's map ramp."
                  ),
                  uiOutput("summary_distribution_status_ui"),
                  div(
                    class = "field-compare-plots-scroll summary-distribution-scroll",
                    uiOutput("summary_distribution_plots_ui")
                  )
                )
              )
            )
          )
        )
      )
      )
    ),
    tabPanel(
      title = main_tab_title("App vs prior", "Historical field GPS vs app samples—map and covariate distributions.", "chart-column"),
      value = "FieldCompare",
      tagList(
          uiOutput("boundary_required_banner_field_compare"),
          sidebarLayout(
            sidebarPanel(
              class = "sidebar-panel",
              tags$details(
                class = "tips-highlight disclosure-card",
                tags$summary(strong("Compare with field samples")),
                tags$ul(
                  tags$li("Upload ", strong("historical / previously collected"), " field sample points first (", VECTOR_UPLOAD_LABEL, "). Map and distributions stay hidden until then."),
                  tags$li("Points must lie ", strong("inside the field boundary"), " and overlap loaded covariate rasters."),
                  tags$li("Generate or save ", strong("app sample points"), " on the ", strong("Sampling"), " tab to compare against historical locations."),
                  tags$li("After upload, covariate values are extracted for ", strong("all loaded layers"), " and the ", strong("Map"), " updates automatically."),
                  tags$li("Open ", strong("Distributions"), " and click ", strong("Compute distributions"), " to build dual-violin plots (not automatic).")
                )
              ),
              tags$div(
                class = "field-compare-info-banner",
                tags$p(
                  style = "margin:0 0 6px 0;",
                  tags$strong("Have points from a previous season or lab submission? "),
                  "Upload them here as ", strong("historical / previously collected"), " points. This helps you judge whether the app’s new sample plan revisits the same areas, fills under-sampled zones, or targets different parts of the field covariate space."
                ),
                tags$p(
                  style = "margin:0; font-size:12px; color:#4a5f78;",
                  "Hover the ", strong("Map"), " and ", strong("Distributions"), " tabs above for a short description of each view."
                )
              ),
              tags$div(
                class = "sampling-segment-box sampling-segment-core",
                style = "margin-top:8px;",
                tags$div(class = "sampling-segment-heading", style = "color:#174b7c; border-color:#bfd7ff;", "Historical points"),
                div(
                  class = "file-input-row",
                  div(
                    class = "file-input-wrapper",
                    fileInput(
                      "upload_historical_points",
                      paste("Field sample points", VECTOR_UPLOAD_LABEL),
                      multiple = TRUE,
                      accept = VECTOR_UPLOAD_ACCEPT
                    )
                  ),
                  actionButton("clear_historical_points", "Clear", class = "btn-danger btn-sm clear-file-btn")
                ),
                textOutput("field_compare_upload_tip"),
                uiOutput("field_compare_upload_status_ui")
              ),
              uiOutput("field_compare_sidebar_status_ui")
            ),
            mainPanel(
              class = "main-panel",
              tabsetPanel(
                id = "field_compare_subtabs",
                tabPanel(
                  title = subtab_tab_title(
                    "Map",
                    "Side-by-side map of app sample points (blue) and uploaded historical field points (orange)."
                  ),
                  value = "field_compare_map_tab",
                  tags$div(
                    class = "field-compare-hero",
                    h3(style = "margin-top:0; color:#1b3f66; font-weight:700;", "App vs field sample locations"),
                    p(class = "text-muted", style = "margin-bottom:8px;", "Blue = app-generated (or saved manual) points. Orange = uploaded historical / previously collected field samples."),
                    tags$ul(
                      class = "field-compare-hero-list text-muted",
                      tags$li("Use this map to see if new recommendations revisit old sample locations or fill gaps."),
                      tags$li("Historical points can be from last season, lab GPS exports, or any prior field collection.")
                    )
                  ),
                  uiOutput("field_compare_map_status_ui"),
                  conditionalPanel(
                    condition = "output.field_compare_historical_ready",
                    div(
                      class = "map-container",
                      withSpinner(leafletOutput("field_compare_map", height = 480))
                    ),
                    div(class = "map-zoom-below compact-zoom", uiOutput("zoom_button_ui_field_compare"))
                  ),
                  conditionalPanel(
                    condition = "!output.field_compare_historical_ready",
                    tags$div(
                      class = "field-compare-empty-state",
                      style = "padding:24px; margin:12px 0; border:1px dashed #bfd7ff; border-radius:12px; background:#f8fbff; text-align:center;",
                      icon("map-location-dot", style = "font-size:28px; color:#7a9bc4; margin-bottom:8px;"),
                      tags$p(style = "margin:0; color:#4a5f78;", "Upload historical field sample points in the sidebar to show the comparison map.")
                    )
                  )
                ),
                tabPanel(
                  title = subtab_tab_title(
                    "Distributions",
                    "Per-layer violin plots comparing covariate values from historical vs app sample points."
                  ),
                  value = "field_compare_dist_tab",
                  conditionalPanel(
                    condition = "output.field_compare_historical_ready",
                    compute_action_bar("compute_field_compare_distributions", "Compute distributions", "chart-area")
                  ),
                  tags$div(
                    class = "field-compare-hero",
                    h3(style = "margin-top:0; color:#1b3f66; font-weight:700;", "Covariate distributions by source"),
                    p(class = "text-muted", style = "margin-bottom:8px;", "One panel per covariate: historical values as a faint orange cloud (alpha 0.4) with an orange violin; app samples as clear blue points with a blue violin when n ≥ 3."),
                    tags$ul(
                      class = "field-compare-hero-list text-muted",
                      tags$li("Requires both app sample points and uploaded historical / previously collected points."),
                      tags$li("Use the blue ", strong("Compute distributions"), " button at the top of this tab after uploading historical points and generating app samples."),
                      tags$li("Helps answer: are new samples targeting similar NDVI, elevation, or soil values as past collections?")
                    )
                  ),
                  uiOutput("field_compare_plots_status_ui"),
                  conditionalPanel(
                    condition = "output.field_compare_historical_ready",
                    div(
                      style = "display:flex; gap:8px; flex-wrap:wrap; margin:0 0 10px 0;",
                      downloadButton("download_field_compare_plot", "Download plot (PNG)", class = "btn-modern btn-sm")
                    ),
                    div(
                      class = "field-compare-plots-scroll summary-distribution-scroll",
                      uiOutput("field_compare_plots_grid_ui")
                    )
                  ),
                  conditionalPanel(
                    condition = "!output.field_compare_historical_ready",
                    tags$div(
                      class = "field-compare-empty-state",
                      style = "padding:24px; margin:12px 0; border:1px dashed #bfd7ff; border-radius:12px; background:#f8fbff; text-align:center;",
                      icon("chart-area", style = "font-size:28px; color:#7a9bc4; margin-bottom:8px;"),
                      tags$p(style = "margin:0; color:#4a5f78;", "Upload historical field sample points to view covariate distribution comparisons.")
                    )
                  )
                )
              )
            )
          )
      )
    ),
    tabPanel(
      title = main_tab_title(
        "Cost",
        "Estimate total sampling cost for your prior grid or uniform design vs the app-recommended sample size.",
        "coins"
      ),
      value = "CostCompare",
      sidebarLayout(
        sidebarPanel(
          class = "sidebar-panel",
          tags$details(
            class = "tips-highlight disclosure-card",
            open = "open",
            tags$summary(strong("Sampling cost inputs")),
            tags$ul(
              tags$li(strong("Prior / grid sample count"), ": how many points you collected with your previous uniform or grid design."),
              tags$li(strong("App sample count"), ": pre-filled from technique comparison or current generated points; edit if needed."),
              tags$li(strong("Cost per sample"), ": your field cost per visit (lab, travel, time, etc.)."),
              tags$li(strong("Currency"), ": choose a preset or enter a custom unit label for plots and the PDF report.")
            )
          ),
          uiOutput("cost_app_recommendation_ui"),
          numericInput(
            "cost_prior_samples",
            "Prior / grid sample count",
            value = 40L,
            min = 1L,
            step = 1L
          ),
          textOutput("cost_prior_hint"),
          numericInput(
            "cost_app_samples",
            "App-recommended sample count",
            value = 10L,
            min = 1L,
            step = 1L
          ),
          numericInput(
            "cost_per_sample",
            "Cost per sample",
            value = 25,
            min = 0,
            step = 1
          ),
          selectInput(
            "cost_currency_preset",
            "Currency",
            choices = c(
              "USD ($)" = "usd",
              "EUR (\u20ac)" = "eur",
              "GBP (\u00a3)" = "gbp",
              "CAD (C$)" = "cad",
              "AUD (A$)" = "aud",
              "Custom" = "custom"
            ),
            selected = "usd"
          ),
          conditionalPanel(
            condition = "input.cost_currency_preset == 'custom'",
            textInput(
              "cost_currency_custom",
              "Custom currency label",
              value = "",
              placeholder = "e.g. NOK, INR, CHF"
            )
          ),
          tags$p(
            class = "text-muted",
            style = "font-size:12px; margin-top:8px;",
            "Hover main tabs and workflow steps for short descriptions. Cost comparison does not require a boundary."
          )
        ),
        mainPanel(
          class = "main-panel",
          tags$div(
            class = "field-compare-hero",
            h3(style = "margin-top:0; color:#1b3f66; font-weight:700;", "Sampling cost: prior design vs app plan"),
            p(
              class = "text-muted",
              style = "margin-bottom:8px;",
              "Bar heights show total cost (sample count \u00d7 cost per sample) for your previous approach and the app-recommended design."
            )
          ),
          withSpinner(plotOutput("cost_comparison_plot", height = 380)),
          uiOutput("cost_comparison_summary_ui")
        )
      )
    ),
    tabPanel(
      title = main_tab_title("Report", "One-page field sampling PDF for planning and sharing.", "file-lines"),
      value = "Report",
      sidebarLayout(
        sidebarPanel(
          class = "sidebar-panel",
          tags$details(
            class = "disclosure-card",
            tags$summary(strong("About this report")),
            tags$p(
              style = "font-size:0.9rem; margin:8px 0 0 0;",
              "Summary page with section dividers, then maps, sample density distributions, population vs sample violins, historical vs app comparison (when uploaded), cost comparison (from Cost comparison tab inputs), and technique comparison. Plot counts are capped to keep PDF export light on 1 GB hosting."
            )
          ),
          downloadButton("download_sampling_report_pdf", "Download PDF report"),
          tags$p(
            class = "text-muted",
            style = "font-size:12px; margin-top:10px;",
            "For the full coordinate list, export GeoJSON from the Sampling tab after generating points."
          )
        ),
        mainPanel(
          class = "main-panel",
          tags$div(
            class = "report-hero",
            h3(style = "margin-top:0; color:#1b3f66; font-weight:700;", "Field sampling plan"),
            p(class = "text-muted", style = "margin-bottom:0;", "Review what you have set up before heading to the field.")
          ),
          uiOutput("report_summary_ui"),
          hr(),
          tags$strong("PDF preview (text)"),
          verbatimTextOutput("report_preview_text")
        )
      )
    )
  ),
  div(
    id = "message_log_toggle_wrap",
    actionButton("toggle_message_log", NULL, icon = icon("bars"), class = "btn-modern btn-xs", title = "Message log")
  ),
  div(
    id = "message_log_drawer",
    style = "display:none;",
    div(style = "display:flex; justify-content:space-between; align-items:center; flex-wrap:wrap; gap:6px; margin-bottom:8px;",
        tags$strong("Messages and Errors"),
        div(style = "display:flex; gap:6px; flex-wrap:wrap;",
            downloadButton("download_message_log", "Download log", class = "btn-modern btn-xs"),
            actionButton("clear_message_log", "Clear", class = "btn-danger btn-xs")
        )
    ),
    tags$p(
      class = "text-muted",
      style = "font-size:10px; margin:0 0 8px 0; line-height:1.3;",
      "If something goes wrong, the app should keep running. Download this log and send it when reporting an issue."
    ),
    uiOutput("message_log_drawer_ui")
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  load_geosampler_packages()
  # --- Reactive Values ---
  digitized_features <- reactiveVal(NULL)
  sampling_defaults_by_area <- reactiveValues(
    area_tier = NA_character_,
    last_auto_compare_sizes = "20,30,40,50",
    last_auto_n_points = 10L
  )
  map_state <- reactiveValues(center = list(lng = -83.3576, lat = 33.9519), zoom = 15)
  ALL_MAP_IDS <- c(
    "map-map", "imagery_map", "elevation_map", "soil_map", "other_map",
    "sampling_auto_map", "sampling_manual_map-map", "field_compare_map"
  )

  # Leaflet must not initialize in a hidden tab (0-size container → gray map with no tiles).
  invalidate_leaflet_maps_client <- function(
    map_ids = ALL_MAP_IDS,
    delays_ms = c(180L, 600L)
  ) {
    ids <- as.character(map_ids)
    ids <- ids[nzchar(ids)]
    if (!length(ids)) return(invisible(NULL))
    for (d in as.integer(delays_ms)) {
      shinyjs::delay(d, {
        tryCatch(
          session$sendCustomMessage("invalidateLeafletMaps", list(mapIds = ids)),
          error = function(e) invisible(NULL)
        )
      })
    }
    invisible(NULL)
  }

  map_ids_for_current_tab <- function() {
    main <- isolate(input$main_tabs)
    if (identical(main, "Boundary")) return("map-map")
    if (identical(main, "Variables")) {
      sub <- isolate(input$variables_subtabs)
      return(switch(
        sub,
        imagery = "imagery_map",
        elevation = "elevation_map",
        soil = "soil_map",
        other = "other_map",
        character(0)
      ))
    }
    if (identical(main, "Sampling")) {
      sub <- isolate(input$sampling_subtabs)
      if (identical(sub, "manual")) return("sampling_manual_map-map")
      return("sampling_auto_map")
    }
    if (identical(main, "FieldCompare")) return("field_compare_map")
    character(0)
  }

  lock_action_button <- function(id) {
    id <- as.character(id)[1L]
    if (!nzchar(id)) return(invisible(NULL))
    tryCatch(shinyjs::disable(id), error = function(e) NULL)
    tryCatch(session$sendCustomMessage("markButtonUsed", list(id = id)), error = function(e) NULL)
    invisible(NULL)
  }

  unlock_action_button <- function(id) {
    id <- as.character(id)[1L]
    if (!nzchar(id)) return(invisible(NULL))
    tryCatch(shinyjs::enable(id), error = function(e) NULL)
    tryCatch(session$sendCustomMessage("markButtonActive", list(id = id)), error = function(e) NULL)
    invisible(NULL)
  }

  unlock_summary_compute_buttons <- function() {
    unlock_action_button("compute_summary_distributions")
  }

  unlock_generate_sample_buttons <- function() {
    unlock_action_button("generate_samples_custom")
    unlock_action_button("generate_samples_recommended")
  }

  unlock_all_session_action_buttons <- function() {
    ids <- c(
      "recommend_zones", "compare_sampling_methods", "compute_variables_summary",
      "compute_summary_distributions",
      "compute_field_compare_distributions",
      "generate_samples_custom", "generate_samples_recommended",
      "download_sentinel", "search_sentinel", "rank_sentinel_gndvi_cv",
      "rebuild_sentinel_timeseries", "reload_app_session"
    )
    for (id in ids) unlock_action_button(id)
    tryCatch(
      session$sendCustomMessage("geosamplerReenableAllButtons", list()),
      error = function(e) invisible(NULL)
    )
    invisible(NULL)
  }

  boundary_padded_bounds <- function(boundary_sf, padding_factor = 0.20) {
    if (is.null(boundary_sf) || nrow(boundary_sf) == 0) return(NULL)
    bb <- tryCatch(sf::st_bbox(sf::st_transform(boundary_sf, 4326)), error = function(e) NULL)
    if (is.null(bb)) return(NULL)
    vals <- unlist(bb[c("xmin", "ymin", "xmax", "ymax")])
    if (!all(is.finite(vals))) return(NULL)
    x_range <- bb[["xmax"]] - bb[["xmin"]]
    y_range <- bb[["ymax"]] - bb[["ymin"]]
    pad_x <- if (x_range <= 0) 0.001 else x_range * padding_factor
    pad_y <- if (y_range <= 0) 0.001 else y_range * padding_factor
    list(
      xmin = bb[["xmin"]] - pad_x,
      ymin = bb[["ymin"]] - pad_y,
      xmax = bb[["xmax"]] + pad_x,
      ymax = bb[["ymax"]] + pad_y
    )
  }

  fit_all_maps_to_bounds <- function(bb) {
    if (is.null(bb)) return(invisible(NULL))
    xs <- bb[["xmin"]]; ys <- bb[["ymin"]]; xe <- bb[["xmax"]]; ye <- bb[["ymax"]]
    if (!all(is.finite(c(xs, ys, xe, ye)))) return(invisible(NULL))
    for (mid in ALL_MAP_IDS) {
      tryCatch({
        leafletProxy(mid) %>%
          leaflet::fitBounds(
            lng1 = xs, lat1 = ys, lng2 = xe, lat2 = ye,
            options = list(padding = c(24, 24), maxZoom = 18)
          )
      }, error = function(e) NULL)
    }
    invisible(bb)
  }

  sync_maps_to_boundary <- function(delay_fit = TRUE) {
    df <- digitized_features()
    if (is.null(df) || nrow(df) == 0) return(invisible(NULL))
    bb <- boundary_padded_bounds(df)
    if (is.null(bb)) return(invisible(NULL))
    tryCatch({
      ctr <- sf::st_coordinates(sf::st_centroid(sf::st_transform(df, 4326)))[1, , drop = TRUE]
      map_state$center <- list(lng = unname(ctr[["X"]]), lat = unname(ctr[["Y"]]))
    }, error = function(e) invisible(NULL))
    fit_all_maps_to_bounds(bb)
    session$sendCustomMessage(
      "fitLeafletMapsToBounds",
      list(bounds = bb, mapIds = ALL_MAP_IDS, delayFit = isTRUE(delay_fit))
    )
    if (isTRUE(delay_fit)) {
      shinyjs::delay(350, fit_all_maps_to_bounds(bb))
      shinyjs::delay(900, fit_all_maps_to_bounds(bb))
      shinyjs::delay(1600, fit_all_maps_to_bounds(bb))
    }
    invisible(bb)
  }

  boundary_secondary_map_proxies <- function() {
    list(
      leafletProxy("imagery_map"),
      leafletProxy("elevation_map"),
      leafletProxy("soil_map"),
      leafletProxy("other_map"),
      leafletProxy("sampling_auto_map"),
      leafletProxy("sampling_manual_map-map"),
      leafletProxy("field_compare_map")
    )
  }

  apply_boundary_overlay_to_maps <- function(boundary_sf = NULL) {
    df <- if (is.null(boundary_sf)) digitized_features() else boundary_sf
    if (is.null(df) || nrow(df) == 0) {
      for (p in boundary_secondary_map_proxies()) {
        p %>% clearGroup("Digitized Boundary")
      }
      leafletProxy("map-map") %>% clearGroup("BoundaryOverlay")
      return(invisible(NULL))
    }
    dfx <- tryCatch(sf::st_transform(df, 4326), error = function(e) df)
    for (p in boundary_secondary_map_proxies()) {
      p %>% clearGroup("Digitized Boundary") %>%
        addPolygons(
          data = dfx,
          color = "#d62728",
          fillOpacity = 0,
          fillColor = "transparent",
          weight = 3,
          opacity = 0.95,
          group = "Digitized Boundary"
        )
    }
    leafletProxy("map-map") %>%
      clearGroup("BoundaryOverlay") %>%
      addPolygons(
        data = dfx,
        color = "#0066cc",
        fillOpacity = 0.15,
        fillColor = "#0066cc",
        weight = 2,
        group = "BoundaryOverlay",
        layerId = paste0("boundary_overlay_", seq_len(nrow(dfx))),
        options = pathOptions(interactive = FALSE)
      )
    invisible(dfx)
  }

  refresh_boundary_on_maps <- function(delay_fit = TRUE) {
    if (is.null(digitized_features())) return(invisible(NULL))
    apply_boundary_overlay_to_maps()
    sync_maps_to_boundary(delay_fit = isTRUE(delay_fit))
  }

  render_sampling_zones_on_map <- function(zone_r = NULL, show_zones = NULL) {
    proxy <- leafletProxy("sampling_auto_map")
    proxy %>% clearGroup("Sampling Zones") %>% removeControl("SamplingZones_legend")
    if (is.null(show_zones)) {
      show_zones <- isTRUE(isolate(input$show_sampling_zones_on_map))
    }
    if (!isTRUE(show_zones)) return(invisible(NULL))
    if (is.null(zone_r)) zone_r <- zonal_zone_raster()
    if (is.null(zone_r)) return(invisible(NULL))
    zone_r_disp <- prepare_discrete_raster_for_leaflet(zone_r)
    zone_plot_vals <- suppressWarnings(as.integer(round(raster::values(zone_r_disp))))
    zone_plot_vals <- sort(unique(stats::na.omit(zone_plot_vals)))
    if (!length(zone_plot_vals)) return(invisible(NULL))
    zone_pal <- colorFactor(
      zone_map_colors(length(zone_plot_vals)),
      domain = zone_plot_vals
    )
    proxy %>%
      addRasterImage(
        zone_r_disp, colors = zone_pal, opacity = ZONE_MAP_OPACITY,
        group = "Sampling Zones", project = FALSE
      ) %>%
      addLegend(
        position = "bottomleft", pal = zone_pal, values = zone_plot_vals,
        title = "Management zones", layerId = "SamplingZones_legend"
      )
    invisible(NULL)
  }

  active_overlays <- reactiveVal(c("Digitized Boundary"))
  
  # For storing results and managing aborts
  planet_result <- reactiveVal(NULL)
  sentinel_result <- reactiveVal(NULL)
  elevation_result <- reactiveVal(NULL)
  elevation_download_zoom <- reactiveVal(13L)
  abort_planet <- reactiveVal(FALSE)
  abort_sentinel <- reactiveVal(FALSE)
  abort_elevation <- reactiveVal(FALSE)
  abort_compare_sampling <- reactiveVal(FALSE)
  is_comparing_sampling <- reactiveVal(FALSE)
  
  # Status for planet
  planet_status <- reactiveVal(NULL)
  is_downloading_planet <- reactiveVal(FALSE)
  planet_progress_pct <- reactiveVal(0)
  planet_progress_detail <- reactiveVal("")
  
  # Status for sentinel
  sentinel_status <- reactiveVal(NULL)
  is_downloading_sentinel <- reactiveVal(FALSE)
  is_ranking_variability <- reactiveVal(FALSE)
  is_building_ndre_timeseries <- reactiveVal(FALSE)
  sentinel_ndre_timeseries_df <- reactiveVal(NULL)
  sentinel_ndre_timeseries_meta <- reactiveVal(NULL)
  sentinel_search_results <- reactiveVal(NULL)
  sentinel_search_active <- reactiveVal(FALSE)  # <<< NEW
  sentinel_retrieval_used <- reactiveVal("single")
  sentinel_retrieval_meta <- reactiveVal(NULL)
  sentinel_progress_pct <- reactiveVal(0)
  sentinel_progress_detail <- reactiveVal("")
  sentinel_last_logged_status <- reactiveVal("")
  sentinel_status_flush <- reactiveVal(0L)
  sentinel_active_since <- reactiveVal(NULL)
  sentinel_last_completed_at <- reactiveVal(NULL)
  sampling_prefill_active <- reactiveVal(FALSE)
  recommended_sample_type <- reactiveVal(NULL)
  recommended_n_points <- reactiveVal(NULL)
  recommended_n_zones <- reactiveVal(NULL)
  zone_wss_cache <- reactiveVal(NULL)
  zones_wss_locked <- reactiveVal(FALSE)
  zone_wss_context_fingerprint <- reactiveVal(NULL)
  committed_wss_zone_k <- reactiveVal(NA_integer_)
  wss_zone_layers_at_lock <- reactiveVal(NULL)
  wss_zone_boundary_fp_at_lock <- reactiveVal(NULL)
  recommended_buffer_distance <- reactiveVal(NULL)
  recommended_random_seed <- reactiveVal(NULL)
  recommended_grid_size_m <- reactiveVal(NULL)
  sentinel_data_locked <- reactive({
    length(sentinel_vi_rasters()) > 0L ||
      !is.null(sentinel_result()) ||
      !is.null(uploaded_sentinel_raster())
  })
  
  # For storing uploaded data
  uploaded_planet_raster <- reactiveVal(NULL)
  uploaded_sentinel_raster <- reactiveVal(NULL)
  uploaded_elevation_raster <- reactiveVal(NULL)
  elevation_aux_layers <- reactiveVal(list()) # For derivatives and uploads
  uploaded_ms_raster_temp <- reactiveVal(NULL) # Temporary for stack before assignment
  uploaded_ms_raster <- reactiveVal(NULL)
  soil_layers <- reactiveVal(list())
  other_layers <- reactiveVal(list())
  vi_rasters <- reactiveVal(list())
  sentinel_vi_rasters <- reactiveVal(list())
  ms_vi_rasters <- reactiveVal(list())
  variables_summary_df <- reactiveVal(NULL)
  
  sample_points <- reactiveVal(NULL)
  sampling_spread_pick_context <- reactiveVal(NULL)
  manual_points <- reactiveVal(NULL)
  historical_sample_points <- reactiveVal(NULL)
  app_compare_values_df <- reactiveVal(NULL)
  historical_compare_values_df <- reactiveVal(NULL)
  summary_distribution_gen <- reactiveVal(list(state = "idle", done = 0L, total = 0L))
  field_compare_distribution_gen <- reactiveVal(list(state = "idle", done = 0L, total = 0L))
  summary_distribution_plots_cache <- reactiveVal(list())
  field_compare_distribution_plots_cache <- reactiveVal(list())
  zonal_cluster_summary <- reactiveVal(NULL)
  zonal_cluster_means <- reactiveVal(NULL)
  zonal_cluster_model <- reactiveVal(NULL)
  zonal_zone_raster <- reactiveVal(NULL)
  zonal_zone_count <- reactiveVal(NULL)
  clhs_similarity_zone <- reactiveVal(NULL)
  clhs_weak_gps_zone <- reactiveVal(NULL)
  clhs_similarity_threshold <- reactiveVal(NULL)
  clhs_similarity_polygon_count <- reactiveVal(NULL)
  clhs_weak_zone_area_ha <- reactiveVal(NULL)
  adaptive_recommendation_summary <- reactiveVal(NULL)
  adaptive_similarity_raster <- reactiveVal(NULL)
  adaptive_recommendation_hidden <- reactiveVal(TRUE)
  comparison_results <- reactiveVal(NULL)
  compare_zone_recommend_msg <- reactiveVal("Click 'Recommend Zones' to estimate zone count from current covariates (WSS elbow).")
  zone_recommend_message <- reactiveVal("Recommendation method: WSS elbow (data-driven). Click 'Recommend zones' after loading covariates and boundary.")
  message_log_tbl_empty <- function() {
    tibble::tibble(time = character(), type = character(), message = character())
  }
  message_log_store <- message_log_tbl_empty()
  message_log_version <- reactiveVal(0L)
  push_message_log_ui <- function() {
    if (is.null(shiny::getDefaultReactiveDomain())) return(invisible(NULL))
    tryCatch(
      message_log_version(isolate(message_log_version()) + 1L),
      error = function(e) {
        if (requireNamespace("later", quietly = TRUE)) {
          later::later(function() {
            tryCatch(
              message_log_version(isolate(message_log_version()) + 1L),
              error = function(e2) NULL
            )
          }, delay = 0.05)
        }
      }
    )
    invisible(NULL)
  }
  sentinel_console_lines <- reactiveVal(character(0))
  sentinel_console_hidden <- reactiveVal(FALSE)
  sentinel_heartbeat_next <- reactiveVal(NULL)
  dashboard_status_last <- reactiveVal("")
  dashboard_status_tick <- reactiveVal(NULL)
  memory_cleanup_last <- reactiveVal(Sys.time())
  
  append_message_log <- function(msg, type = "message", context = NULL) {
    msg <- as.character(msg)
    msg <- msg[nzchar(msg)]
    if (!length(msg)) return(invisible(NULL))
    msg <- paste(msg, collapse = " ")
    if (length(context) && nzchar(as.character(context)[1L])) {
      msg <- paste0("[", as.character(context)[1L], "] ", msg)
    }
    new_row <- tibble::tibble(
      time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      type = as.character(type),
      message = msg
    )
    message_log_store <<- dplyr::bind_rows(message_log_store, new_row)
    if (nrow(message_log_store) > 250L) {
      message_log_store <<- message_log_store[seq_len(nrow(message_log_store) - 250L + 1L), , drop = FALSE]
    }
    push_message_log_ui()
    invisible(NULL)
  }

  approx_r_heap_mb <- function() {
    g <- tryCatch(gc(verbose = FALSE), error = function(e) NULL)
    if (is.null(g) || !is.matrix(g) || ncol(g) < 2L) return(NA_real_)
    mb <- suppressWarnings(sum(as.numeric(g[, 2L]), na.rm = TRUE))
    if (is.finite(mb)) mb else NA_real_
  }

  run_memory_maintenance <- function(reason = "maintenance", notify_user = FALSE, min_interval_sec = 0) {
    now <- Sys.time()
    last <- tryCatch(isolate(memory_cleanup_last()), error = function(e) as.POSIXct(0, origin = "1970-01-01"))
    elapsed <- suppressWarnings(as.numeric(difftime(now, last, units = "secs")))
    if (is.finite(elapsed) && elapsed < min_interval_sec) return(invisible(FALSE))
    before_mb <- approx_r_heap_mb()
    tryCatch(release_geosampler_memory(), error = function(e) invisible(NULL))
    after_mb <- approx_r_heap_mb()
    memory_cleanup_last(now)
    msg <- paste0(
      "Memory cleanup completed",
      if (nzchar(reason)) paste0(" (", reason, ")") else "",
      if (is.finite(after_mb)) paste0("; approx R heap ", round(after_mb, 1), " MB") else "",
      if (is.finite(before_mb) && is.finite(after_mb)) paste0("; reclaimed about ", max(0, round(before_mb - after_mb, 1)), " MB") else "",
      "."
    )
    append_message_log(msg, type = "message", context = "Memory")
    if (isTRUE(notify_user)) {
      shiny::showNotification(msg, type = "message", duration = 6, session = session)
    }
    invisible(TRUE)
  }

  # Log + notify without crashing the session (used by global Shiny error hook and safe_run).
  handle_app_error <- function(e, context = "App", notify_user = TRUE, duration = 14) {
    tryCatch({
      msg <- if (inherits(e, "condition")) conditionMessage(e) else as.character(e)
      if (!length(msg) || !nzchar(msg)) msg <- "Unknown error"
      trace_hint <- tryCatch({
        tr <- utils::capture.output(traceback(2))
        tr <- tr[nzchar(tr)]
        if (length(tr)) paste(utils::head(tr, 3L), collapse = " <- ") else ""
      }, error = function(x) "")
      log_body <- msg
      if (nzchar(trace_hint)) {
        log_body <- paste0(msg, " | trace: ", substr(trace_hint, 1L, 350L))
      }
      append_message_log(log_body, type = "error", context = context)
      if (isTRUE(notify_user)) {
        shiny::showNotification(
          paste0(
            context, " failed: ", msg,
            ". Open Messages & Errors (top-right) and use Download log to report this."
          ),
          type = "error",
          duration = duration,
          session = session
        )
      }
      tryCatch(run_memory_maintenance(reason = paste0("after ", context, " error")), error = function(e3) invisible(NULL))
    }, error = function(e2) invisible(NULL))
    invisible(NULL)
  }

  safe_run <- function(expr, context = "Operation", user_msg = NULL, notify = TRUE) {
    tryCatch(
      expr,
      error = function(e) {
        if (inherits(e, "shiny.silent.error") || inherits(e, "validation")) {
          return(invisible(NULL))
        }
        handle_app_error(e, context = context, notify_user = notify)
        if (!is.null(user_msg) && nzchar(user_msg)) {
          shiny::showNotification(as.character(user_msg), type = "warning", duration = 8, session = session)
        }
        invisible(NULL)
      }
    )
  }

  append_sentinel_console <- function(msg) {
    msg <- trimws(as.character(msg))
    if (!length(msg) || !any(nzchar(msg))) return(invisible(NULL))
    ts <- format(Sys.time(), "%H:%M:%S")
    line <- paste0("[", ts, "] ", msg)
    cat("[Sentinel] ", line, "\n", sep = "")
    old <- sentinel_console_lines()
    updated <- c(line, old)
    if (length(updated) > 12L) updated <- head(updated, 12L)
    sentinel_console_lines(updated)
    sentinel_console_hidden(FALSE)
  }
  
  set_sentinel_step <- function(status = NULL, detail = NULL, type = "message", log_it = TRUE) {
    st_in <- status
    det_in <- detail
    typ_in <- type
    log_in <- log_it
    apply_step <- function() {
      if (!is.null(st_in)) sentinel_status(st_in)
      if (!is.null(det_in)) sentinel_progress_detail(det_in)
      if (!isTRUE(log_in)) {
        sentinel_status_flush(sentinel_status_flush() + 1L)
        return(invisible(NULL))
      }
    st <- sentinel_status()
    det <- sentinel_progress_detail()
      parts <- c(
        if (!is.null(st) && length(st) && any(nzchar(as.character(st)))) as.character(st) else NULL,
        if (!is.null(det) && length(det) && any(nzchar(as.character(det)))) as.character(det) else NULL
      )
      msg <- if (length(parts)) paste(parts, collapse = " | ") else ""
      if (!length(msg) || !nzchar(msg) || identical(msg, sentinel_last_logged_status())) {
        sentinel_status_flush(sentinel_status_flush() + 1L)
        return(invisible(NULL))
      }
      append_message_log(msg, type = typ_in)
      append_sentinel_console(msg)
    sentinel_last_logged_status(msg)
      sentinel_status_flush(sentinel_status_flush() + 1L)
    }
    apply_step()
    invisible(NULL)
  }

  sentinel_scene_choice_label <- function(f, ndre_spread = NA_real_, mark_highest = FALSE) {
    dt_raw <- tryCatch(as.character(f$properties$datetime), error = function(e) NA_character_)
    dt_chr <- tryCatch({
      format(as.POSIXct(dt_raw, tz = "UTC"), format = "%Y-%m-%d %H:%M UTC")
    }, error = function(e) dt_raw)
    ccv <- suppressWarnings(as.numeric(tryCatch(f$properties$`eo:cloud_cover`, error = function(e) NA_real_)))
    cc_chr <- if (is.na(ccv)) "NA" else sprintf("%.1f", ccv)
    var_chr <- if (is.finite(ndre_spread)) sprintf(" | NDRE spread: %.3f", ndre_spread) else ""
    star_chr <- if (isTRUE(mark_highest)) " ★" else ""
    paste0(dt_chr, " | Cloud: ", cc_chr, "%", var_chr, star_chr)
  }

  scene_ndre_spread_vector <- function(res) {
    if (is.null(res)) return(NULL)
    spread <- res$`app:scene_ndre_spread`
    if (is.null(spread)) spread <- res$`app:scene_gndvi_cv`
    cv <- suppressWarnings(as.numeric(spread))
    if (!length(cv)) return(NULL)
    cv
  }

  sentinel_single_scene_choices <- function(features, cv_vec = NULL) {
    n <- length(features)
    if (n < 1L) {
      return(list(choices = setNames(integer(0), character(0)), selected = 1L, best_idx = NA_integer_))
    }
    labels <- vapply(seq_len(n), function(i) {
      sentinel_scene_choice_label(features[[i]], ndre_spread = if (!is.null(cv_vec)) cv_vec[i] else NA_real_)
    }, character(1))
    if (!is.null(cv_vec) && length(cv_vec) == n && any(is.finite(cv_vec))) {
      ord <- order(cv_vec, decreasing = TRUE, na.last = TRUE)
      best_idx <- ord[1L]
      labels[best_idx] <- sentinel_scene_choice_label(
        features[[best_idx]],
        ndre_spread = cv_vec[best_idx],
        mark_highest = TRUE
      )
      list(
        choices = setNames(ord, labels[ord]),
        selected = ord[1L],
        best_idx = best_idx
      )
    } else {
      list(
        choices = setNames(seq_len(n), labels),
        selected = n,
        best_idx = NA_integer_
      )
    }
  }

  render_sentinel_select_ui <- function(res) {
    features <- safe_features(res)
    if (!length(features)) {
      output$sentinel_select_ui <- renderUI({})
      return(invisible(NULL))
    }
    mode_now <- if (is.null(input$sentinel_retrieval_mode)) "single" else input$sentinel_retrieval_mode
    cv_vec <- scene_ndre_spread_vector(res)
    if (identical(mode_now, "single")) {
      ch <- sentinel_single_scene_choices(features, cv_vec)
      sel_label <- if (!is.null(cv_vec) && any(is.finite(cv_vec))) {
        "Select image (highest NDRE spread first):"
      } else {
        "Select image (oldest to newest):"
      }
      output$sentinel_select_ui <- renderUI({
        selectInput("sentinel_selected", sel_label, choices = ch$choices, selected = ch$selected)
      })
    } else if (length(features) < 2L) {
      output$sentinel_select_ui <- renderUI({
        tags$p(
          class = "text-warning",
          style = "margin-top:8px; line-height:1.35;",
          "Only ", length(features), " scene found. Per-pixel median needs at least 2 scenes — widen the date range or relax the cloud filter, then search again."
        )
      })
    } else {
      idx_use <- sentinel_median_scene_indices(length(features), input$sentinel_median_max_scenes, 8L)
      used_dates <- vapply(features[idx_use], function(f) {
        as.character(f$properties$datetime)
      }, character(1))
      output$sentinel_select_ui <- renderUI({
        tagList(
          h5("Scenes used for median"),
          tags$small(
            class = "text-muted",
            sentinel_median_scenes_caption(length(idx_use), length(features))
          ),
          if (!is.null(cv_vec) && any(is.finite(cv_vec))) {
            tags$p(
              class = "text-muted",
              style = "font-size:10.5px; margin:4px 0 0 0;",
              "NDRE spread ranking applies to single-scene mode only; median still uses evenly spaced dates."
            )
          },
          tags$ul(lapply(used_dates, tags$li))
        )
      })
    }
    invisible(NULL)
  }

  rank_sentinel_search_by_ndre <- function(it_obj, boundary_sf, progress_fn = NULL) {
    features <- sort_stac_features_by_datetime(safe_features(it_obj), decreasing = FALSE)
    n_all <- length(features)
    if (n_all < 1L) return(it_obj)
    cap <- SENTINEL_NDRE_RANK_MAX_SCENES()
    max_score <- if (n_all <= cap) n_all else min(n_all, cap)
    idx_score <- if (n_all > max_score) {
      sentinel_median_scene_pick_indices(n_all, max_score)
    } else {
      seq_len(n_all)
    }
    max_cells <- SENTINEL_NDRE_RANK_MAX_CELLS()
    boundary_4326 <- boundary_sf_4326(boundary_sf)
    interior_buffer_m <- interior_buffer_m_for_boundary(boundary_sf, boundary_4326)
    it_sorted <- it_obj
    it_sorted$features <- features
    signed <- tryCatch(
      rstac::items_sign(it_sorted, sign_fn = rstac::sign_planetary_computer()),
      error = function(e) it_sorted
    )
    spread_vec <- rep(NA_real_, n_all)
    tick_every <- if (is_hosted_shiny()) 1L else 3L
    for (ii in seq_along(idx_score)) {
      i <- idx_score[ii]
      if (is.function(progress_fn)) {
        tryCatch(progress_fn(ii / length(idx_score), detail = NULL), error = function(e) NULL)
      }
      feat <- signed$features[[i]]
      spread_vec[i] <- scene_ndre_spread_from_feature(
        feat, boundary_sf,
        max_cells = max_cells,
        boundary_4326 = boundary_4326,
        interior_buffer_m = interior_buffer_m,
        skip_sign = FALSE
      )
      ndre_workflow_memory_tick(ii, tick_every)
    }
    signed$`app:scene_ndre_spread` <- spread_vec
    signed$`app:ndre_rank_scored_idx` <- idx_score
    signed$`app:ndre_rank_done` <- TRUE
    signed$`app:ndre_rank_scored_n` <- length(idx_score)
    signed$`app:ndre_rank_fail_n` <- sum(!is.finite(spread_vec[idx_score]))
    ndre_workflow_memory_tick()
    signed
  }

  force_dashboard_status <- function(text = NULL) {
    txt <- if (is.null(text)) "" else trimws(as.character(text))
    if (identical(txt, dashboard_status_last())) return(invisible(NULL))
    dashboard_status_last(txt)
    session$sendCustomMessage("forceDashboardStatus", list(text = txt))
  }
  
  # Wrap showNotification so all user-facing notices are logged with timestamp/type.
  showNotification <- function(ui, action = NULL, duration = 5, closeButton = TRUE, id = NULL,
                               type = c("default", "message", "warning", "error"),
                               session = shiny::getDefaultReactiveDomain()) {
    type <- match.arg(type)
    msg_txt <- tryCatch({
      if (is.character(ui)) ui else paste0(gsub("\\s+", " ", paste(capture.output(print(ui)), collapse = " ")))
    }, error = function(e) "Notification")
    append_message_log(msg_txt, type = type)
    shiny::showNotification(ui = ui, action = action, duration = duration, closeButton = closeButton, id = id, type = type, session = session)
  }

  options(shiny.error = function() {
    handle_app_error(simpleError(geterrmessage()), context = "Unexpected", notify_user = TRUE)
  })
  if (exists("onUnhandledErrorInObserver", where = asNamespace("shiny"), inherits = FALSE)) {
  tryCatch(
    shiny::onUnhandledErrorInObserver(function(e) {
      handle_app_error(e, context = "Background", notify_user = TRUE)
    }),
    error = function(e) invisible(NULL)
  )
  }
  observe({
    append_message_log("GeoSampler session ready.", type = "message", context = "System")
  }, priority = 1000)

  observe({
    invalidateLater(if (is_hosted_shiny()) 180000L else 300000L, session)
    busy <- isTRUE(isolate(is_downloading_sentinel())) ||
      isTRUE(isolate(is_downloading_planet())) ||
      isTRUE(isolate(is_comparing_sampling())) ||
      isTRUE(isolate(is_ranking_variability()))
    if (!isTRUE(busy)) {
      run_memory_maintenance(reason = "idle periodic cleanup", min_interval_sec = if (is_hosted_shiny()) 150 else 240)
    }
  }, priority = -1000)

  session$onSessionEnded(function() {
    tryCatch(release_geosampler_memory(), error = function(e) invisible(NULL))
  })

  observeEvent(input$geosampler_client_error, {
    err <- input$geosampler_client_error
    msg <- tryCatch(as.character(err$message %||% err), error = function(e) "Browser-side error")
    src <- tryCatch(as.character(err$source %||% "browser"), error = function(e) "browser")
    append_message_log(msg, type = "error", context = paste("Client", src))
    showNotification(
      "A browser-side UI error was logged. If something looks stuck, refresh or use Reset, then download the log.",
      type = "warning",
      duration = 10
    )
  }, ignoreInit = TRUE)
  
  promise_error_message <- function(err) {
    as_text <- function(x) {
      if (is.null(x)) return("")
      if (is.character(x) && length(x) >= 1L) return(trimws(x[[1L]]))
      if (is.atomic(x) && length(x) == 1L) return(trimws(as.character(x)))
      ""
    }
    txt <- ""
    if (inherits(err, "condition")) txt <- conditionMessage(err)
    if (!nzchar(txt) && is.list(err) && !is.null(err$message)) txt <- as_text(err$message)
    if (!nzchar(txt)) txt <- as_text(err)
    if (!nzchar(txt)) txt <- "Unexpected asynchronous error. Please retry."
    noisy <- grepl("shiny\\.render\\.function|function \\(\\.\\.\\.\\)|Mutable|cacheHint", txt, ignore.case = TRUE)
    if (isTRUE(noisy)) txt <- "Unexpected callback state after retrieval. The data may still be loaded; please verify map/layers and retry only if needed."
    txt
  }
  
  safe_features <- function(x) {
    if (is.list(x) && !is.null(x$features) && is.list(x$features)) {
      return(x$features)
    }
    list()
  }

  # Median UI can be cleared or invalid; never pass NA into seq_len / seq.
  # Never return more scenes than are available (avoids subscript errors when search finds 1 scene).
  sentinel_median_scene_count_safe <- function(raw_input, n_available, fallback = 8L) {
    n_avail <- suppressWarnings(as.integer(n_available))
    if (length(n_avail) != 1L || is.na(n_avail) || n_avail < 1L) return(0L)
    if (n_avail < 2L) return(n_avail)
    v <- if (is.null(raw_input)) NA_integer_ else suppressWarnings(as.integer(raw_input))
    if (length(v) != 1L || is.na(v) || v < 2L) v <- as.integer(fallback)
    v <- max(2L, min(300L, v))
    min(v, n_avail)
  }

  sentinel_median_scene_indices <- function(n_available, raw_input = NULL, fallback = 8L) {
    n_use <- sentinel_median_scene_count_safe(raw_input, n_available, fallback)
    sentinel_median_scene_pick_indices(n_available, n_use)
  }
  
  # --- CRS Definition ---
  TARGET_CRS <- "+proj=longlat +datum=WGS84"

  # Continuous rasters: bilinear reprojection; categorical (factor): nearest neighbour.
  project_raster_bilinear <- function(r, ...) {
    if (is.null(r)) return(NULL)
    args <- list(...)
    if (is.null(args$method)) {
      args$method <- tryCatch(if (raster::is.factor(r)) "ngb" else "bilinear", error = function(e) "bilinear")
    }
    do.call(raster::projectRaster, c(list(r), args))
  }

  LAYER_MAP_TIP <- ""
  BOUNDARY_MAP_TIP <- ""

  set_layer_tip <- function(channel = c("imagery", "elevation", "other", "soil", "boundary", "sampling")) {
    channel <- match.arg(channel)
    output_id <- switch(channel,
      imagery = "upload_tip_imagery",
      elevation = "upload_tip_elevation",
      other = "upload_tip_other",
      soil = "upload_tip_soil",
      boundary = "upload_tip_boundary",
      sampling = "upload_tip_sampling"
    )
    output[[output_id]] <- renderText("")
  }

  notify_layer_ready <- function(what, source = c("download", "upload")) {
    source <- match.arg(source)
    verb <- if (identical(source, "download")) "retrieved" else "loaded"
    showNotification(
      paste0(what, " ", verb, ". Choose a layer in Select to Display to view it on the map."),
      type = "message",
      duration = 6
    )
  }

  prepare_uploaded_raster_file <- function(datapath, brick = FALSE) {
    r <- if (isTRUE(brick)) raster::brick(datapath) else raster::raster(datapath)
    if (is.na(raster::crs(r))) raster::crs(r) <- TARGET_CRS
    project_raster_bilinear(r, crs = TARGET_CRS)
  }

  finalize_layer_on_map <- function(redraw = NULL) {
    refresh_boundary_on_maps(delay_fit = FALSE)
    if (is.function(redraw)) {
      shinyjs::delay(200, {
        tryCatch(redraw(), error = function(e) NULL)
        refresh_boundary_on_maps(delay_fit = TRUE)
      })
    } else {
      shinyjs::delay(250, refresh_boundary_on_maps(delay_fit = TRUE))
    }
    trim_session_memory()
  }

  notify_band_staged <- function(band_label) {
    showNotification(
      paste(band_label, "band staged. Upload remaining bands, then click Stack Individual Bands."),
      type = "message",
      duration = 5
    )
    set_layer_tip("imagery")
  }

  notify_layers_added <- function(count, what, source = c("download", "upload"), tip_channel = "imagery", redraw = NULL) {
    source <- match.arg(source)
    noun <- if (count == 1L) paste("1", what, "layer") else paste(count, what, "layers")
    notify_layer_ready(noun, source)
    set_layer_tip(tip_channel)
    finalize_layer_on_map(redraw)
  }

  local_metric_crs_from_sf <- function(x_sf) {
    x_wgs <- tryCatch(sf::st_transform(x_sf, 4326), error = function(e) x_sf)
    ctr <- tryCatch(sf::st_coordinates(sf::st_centroid(sf::st_union(x_wgs))), error = function(e) matrix(c(0, 0), ncol = 2))
    lon <- ctr[1, 1]; lat <- ctr[1, 2]
    utm_zone <- floor((lon + 180) / 6) + 1
    paste0("+proj=utm +zone=", utm_zone, ifelse(lat < 0, " +south", ""), " +datum=WGS84 +units=m +no_defs")
  }

  analysis_crs_string <- reactive({
    b <- digitized_features()
    if (isTRUE(input$strict_projected_analysis) && !is.null(b) && nrow(b) > 0) {
      return(local_metric_crs_from_sf(b))
    }
    TARGET_CRS
  })

  deployment_safety_params <- reactive({
    hosted <- is_hosted_shiny()
    b <- digitized_features()
    area_ha <- NA_real_
    if (!is.null(b) && nrow(b) > 0) {
      m <- compute_boundary_risk_metrics(b)
      if (!is.null(m)) area_ha <- m$area_ha
    }
    list(
      active = hosted,
      area_ha = area_ha,
      recommended_buffer_m = recommended_buffer_m_for_ha(area_ha),
      harmonize_scale = 1L,
      scene_cap = NULL,
      pop_cap = if (hosted) 150000L else 250000L,
      clhs_cap = Inf,
      compare_pop_cap = if (hosted) 28000L else 65000L,
      display_max_cells = if (hosted) 650000L else 1200000L,
      fill_na_max_cells = if (hosted) 280000L else 500000L
    )
  })

  resample_raster_to_meter_resolution <- function(r, target_m, boundary_sf = NULL) {
    target_m <- suppressWarnings(as.numeric(target_m))
    if (is.na(target_m) || target_m <= 0) return(r)
    metric_crs <- tryCatch({
      if (!is.null(boundary_sf) && nrow(boundary_sf) > 0) local_metric_crs_from_sf(boundary_sf) else "+proj=merc +datum=WGS84 +units=m +no_defs"
    }, error = function(e) "+proj=merc +datum=WGS84 +units=m +no_defs")
    r_m <- tryCatch(project_raster_bilinear(r, crs = metric_crs), error = function(e) NULL)
    if (is.null(r_m)) return(r)
    template <- raster::raster(
      xmn = raster::extent(r_m)@xmin,
      xmx = raster::extent(r_m)@xmax,
      ymn = raster::extent(r_m)@ymin,
      ymx = raster::extent(r_m)@ymax,
      crs = raster::crs(r_m)
    )
    raster::res(template) <- c(target_m, target_m)
    r_m_rs <- tryCatch(raster::resample(r_m, template, method = "bilinear"), error = function(e) r_m)
    tryCatch(project_raster_bilinear(r_m_rs, crs = TARGET_CRS), error = function(e) r)
  }
  
  # Single AOI only: if multiple features, keep the first and warn (upload / Done).
  normalize_single_boundary <- function(boundary_sf) {
    boundary_sf <- tryCatch(sf::st_make_valid(boundary_sf), error = function(e) boundary_sf)
    if (is.na(sf::st_crs(boundary_sf))) {
      sf::st_crs(boundary_sf) <- 4326
    }
    boundary_sf <- st_transform(boundary_sf, st_crs(TARGET_CRS))
    boundary_sf <- suppressWarnings(st_zm(boundary_sf, drop = TRUE, what = "ZM"))
    poly_mask <- st_geometry_type(boundary_sf) %in% c("POLYGON", "MULTIPOLYGON")
    boundary_sf <- boundary_sf[poly_mask, , drop = FALSE]
    if (nrow(boundary_sf) == 0) return(NULL)
    if (nrow(boundary_sf) > 1) {
      showNotification("Only one boundary polygon is allowed. Using the first feature only. Remove the others in your GIS file and re-upload if needed.", type = "warning", duration = 8)
      boundary_sf <- boundary_sf[1, , drop = FALSE]
    }
    boundary_sf
  }

  commit_boundary_to_app <- function(boundary_sf, status_msg = NULL) {
    if (is.null(boundary_sf) || nrow(boundary_sf) == 0) return(invisible(NULL))
    norm <- tryCatch(normalize_single_boundary(boundary_sf), error = function(e) NULL)
    if (is.null(norm)) {
      showNotification("Could not use this boundary geometry.", type = "error")
      return(invisible(NULL))
    }
    digitized_features(norm)
    if (!is.null(status_msg) && length(status_msg) && any(nzchar(as.character(status_msg)))) {
      output$digitize_status <- renderText(status_msg)
    }
    refresh_boundary_on_maps(delay_fit = FALSE)
    shinyjs::delay(200, refresh_boundary_on_maps(delay_fit = TRUE))
    shinyjs::delay(700, refresh_boundary_on_maps(delay_fit = TRUE))
    shinyjs::delay(1400, refresh_boundary_on_maps(delay_fit = TRUE))
    invisible(norm)
  }

  strict_crop_mask_raster <- function(r, boundary_sf, exclude_boundary_touch = TRUE) {
    if (is.null(r) || is.null(boundary_sf) || nrow(boundary_sf) == 0) return(r)
    b <- tryCatch(sf::st_make_valid(boundary_sf), error = function(e) boundary_sf)
    b <- b[!sf::st_is_empty(b), , drop = FALSE]
    if (nrow(b) == 0) return(r)

    r_crs <- tryCatch(sf::st_crs(raster::crs(r)), error = function(e) NA)
    if (!is.null(r_crs) && !is.na(r_crs)) {
      b <- tryCatch(sf::st_transform(b, r_crs), error = function(e) b)
    }

    if (isTRUE(exclude_boundary_touch)) {
      cell_size <- tryCatch(max(abs(raster::res(r))), error = function(e) NA_real_)
      if (is.finite(cell_size) && cell_size > 0) {
        b_inner <- tryCatch(sf::st_buffer(b, dist = -0.51 * cell_size), error = function(e) b)
        b_inner <- tryCatch(sf::st_make_valid(b_inner), error = function(e) b_inner)
        b_inner <- b_inner[!sf::st_is_empty(b_inner), , drop = FALSE]
        if (nrow(b_inner) > 0) b <- b_inner
      }
    }

    b_sp <- tryCatch(as(b, "Spatial"), error = function(e) NULL)
    if (is.null(b_sp)) return(r)
    rr <- tryCatch(raster::crop(r, b_sp), error = function(e) r)
    rr <- tryCatch(raster::mask(rr, b_sp), error = function(e) rr)
    rr
  }

  read_uploaded_vector <- function(upload_info, kind = "vector") {
    req(upload_info)
    if (is.null(upload_info$datapath) || nrow(upload_info) == 0) {
      stop(paste0("No ", kind, " files were uploaded."))
    }

    upload_info$ext <- tolower(tools::file_ext(upload_info$name))
    archive_idx <- which(upload_info$ext %in% c("zip", "kmz"))
    if (length(archive_idx) > 0) {
      archive_path <- upload_info$datapath[archive_idx[1]]
      temp_dir <- tempfile(pattern = "vector_upload_")
      dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
      utils::unzip(archive_path, exdir = temp_dir)
      shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)[1]
      kml_file <- list.files(temp_dir, pattern = "\\.kml$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)[1]
      gpkg_file <- list.files(temp_dir, pattern = "\\.gpkg$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)[1]
      geojson_file <- list.files(temp_dir, pattern = "\\.(geojson|json)$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)[1]
      if (!is.na(shp_file)) return(sf::st_read(shp_file, quiet = TRUE))
      if (!is.na(kml_file)) return(sf::st_read(kml_file, quiet = TRUE))
      if (!is.na(gpkg_file)) return(sf::st_read(gpkg_file, quiet = TRUE))
      if (!is.na(geojson_file)) return(sf::st_read(geojson_file, quiet = TRUE))
      stop(paste0(
        "No valid ", kind, " file found in archive. Include .shp (with sidecars), .kml, .gpkg, or .geojson."
      ))
    }

    direct_idx <- which(upload_info$ext %in% c("geojson", "json", "kml", "gpkg", "kmz"))
    if (length(direct_idx) > 0) {
      path <- upload_info$datapath[direct_idx[1]]
      if (identical(upload_info$ext[direct_idx[1]], "kmz")) {
        temp_dir <- tempfile(pattern = "vector_kmz_")
        dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
        utils::unzip(path, exdir = temp_dir)
        kml_file <- list.files(temp_dir, pattern = "\\.kml$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)[1]
        if (is.na(kml_file)) stop(paste0("KMZ archive did not contain a readable .kml for ", kind, "."))
        return(sf::st_read(kml_file, quiet = TRUE))
      }
      return(sf::st_read(path, quiet = TRUE))
    }

    shp_idx <- which(upload_info$ext == "shp")
    if (length(shp_idx) > 0) {
      shp_name <- upload_info$name[shp_idx[1]]
      shp_base <- tools::file_path_sans_ext(shp_name)
      temp_dir <- tempfile(pattern = "vector_shp_")
      dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
      for (i in seq_len(nrow(upload_info))) {
        this_base <- tools::file_path_sans_ext(upload_info$name[i])
        if (identical(this_base, shp_base)) {
          out_path <- file.path(temp_dir, upload_info$name[i])
          ok <- file.copy(upload_info$datapath[i], out_path, overwrite = TRUE)
          if (!isTRUE(ok)) stop("Could not prepare uploaded shapefile components.")
        }
      }
      shp_path <- file.path(temp_dir, shp_name)
      if (!file.exists(shp_path)) stop("Uploaded shapefile (.shp) could not be located.")
      return(sf::st_read(shp_path, quiet = TRUE))
    }

    stop(paste0(
      "Unsupported ", kind, " upload. Use GeoJSON/KML/GPKG, ZIP/KMZ archive, or SHP with sidecar files."
    ))
  }

  read_uploaded_boundary <- function(upload_info) {
    read_uploaded_vector(upload_info, kind = "boundary")
  }

  read_uploaded_sample_points <- function(upload_info) {
    read_uploaded_vector(upload_info, kind = "sample points")
  }
  
  # --- Boundary Tab Logic ---
  initial_map <- leaflet(options = leafletOptions(maxZoom = 20)) %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addProviderTiles("OpenStreetMap", group = "Street Map") %>%
    setView(lng = -83.3576, lat = 33.9519, zoom = 15) %>%
    addLayersControl(baseGroups = c("Satellite", "Street Map"))
  
  # Customized editor for boundary: only rectangle and polygon
  edit_map <- callModule(module = editMod, id = "map", leafmap = initial_map, editor = "leaflet",
                         editorOptions = list(
                           polylineOptions = FALSE,
                           polygonOptions = drawPolygonOptions(),
                           rectangleOptions = drawRectangleOptions(),
                           circleOptions = FALSE,
                           markerOptions = FALSE,
                           circleMarkerOptions = FALSE,
                           editOptions = editToolbarOptions()
                         ))
  
  observe({
    req(input$`map-map_center`, input$`map-map_zoom`)
    map_state$center <- input$`map-map_center`
    map_state$zoom <- input$`map-map_zoom`
  })
  
  observeEvent(input$set_location, {
    lat <- as.numeric(input$lat); lon <- as.numeric(input$lon)
    if (!is.na(lat) && !is.na(lon)) {
      leafletProxy("map-map") %>% setView(lng = lon, lat = lat, zoom = 15)
    } else {
      showNotification("Please enter valid latitude and longitude values.", type = "error")
    }
  })
  
  # Handler for "Use My Location" button
  observeEvent(input$use_my_location, {
    session$sendCustomMessage("getLocation", list())
  })
  
  # Update map when user location is received
  observeEvent(c(input$user_lat, input$user_lon), {
    req(input$user_lat, input$user_lon)
    updateTextInput(session, "lat", value = as.character(round(input$user_lat, 6)))
    updateTextInput(session, "lon", value = as.character(round(input$user_lon, 6)))
    leafletProxy("map-map") %>% setView(lng = input$user_lon, lat = input$user_lat, zoom = 15)
    showNotification("Map centered on your location!", type = "message")
  })
  
  observeEvent(edit_map()$finished, {
    req(edit_map()$finished)
    finished <- edit_map()$finished
    if (!is.null(finished) && nrow(finished) > 0) {
      poly_mask <- st_geometry_type(finished) %in% c("POLYGON", "MULTIPOLYGON")
      finished <- finished[poly_mask, , drop = FALSE]
      if (nrow(finished) > 0) {
        if (nrow(finished) > 1) {
          showNotification("Only one polygon is allowed per AOI. The first shape was kept; use the trash tool to clear and draw again to replace it.", type = "warning", duration = 8)
        }
        commit_boundary_to_app(finished, "Digitization complete! Click 'Next' to proceed.")
        set_layer_tip("boundary")
      }
    }
  })
  
  observeEvent(input$too_many_drawn_polygons, {
    showNotification("Only one boundary polygon can be drawn. The extra shape was removed.", type = "warning", duration = 5)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Reset boundaries when draw:deleted fires or user clicks clear.
  # This is triggered by the JavaScript draw:deleted event listener added in the UI.
  clear_boundary_and_samples <- function(
    msg_digitize = TRUE,
    reset_workflow = TRUE,
    workflow_notification = "Boundary removed. Tabs were reset to a clean start."
  ) {
    digitized_features(NULL)
    sample_points(NULL)
    sampling_spread_pick_context(NULL)
    manual_points(NULL)
    historical_sample_points(NULL)
    app_compare_values_df(NULL)
    historical_compare_values_df(NULL)
    zonal_cluster_summary(NULL)
    zonal_cluster_means(NULL)
    zonal_cluster_model(NULL)
    zonal_zone_raster(NULL)
    zonal_zone_count(NULL)
    if (msg_digitize) {
      output$digitize_status <- renderText("Boundary cleared from map. Draw a new boundary and click 'Done'.")
    }
    output$boundary_area <- renderText("")
    leafletProxy("map-map") %>%
      clearGroup("BoundaryOverlay") %>%
      clearShapes() %>%
      clearMarkers()
    session$sendCustomMessage("clearLeafletDrawFeatures", list(mapId = "map-map"))
    for (p in list(leafletProxy("imagery_map"), leafletProxy("elevation_map"), leafletProxy("other_map"),
                   leafletProxy("soil_map"), leafletProxy("sampling_auto_map"), leafletProxy("sampling_manual_map-map"))) {
      p %>% clearGroup("Digitized Boundary")
    }
    # Force-refresh boundary map state so redraw can start immediately.
    try(
      leafletProxy("map-map") %>% setView(lng = map_state$center$lng, lat = map_state$center$lat, zoom = map_state$zoom),
      silent = TRUE
    )
    if (isTRUE(reset_workflow)) {
      reset_to_clean_workflow_state(workflow_notification)
    }
  }
  
  observeEvent(input$boundary_draw_deleted, {
    clear_boundary_and_samples(TRUE)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Immediate upload feedback so users see activity right after file selection.
  observeEvent(input$upload_boundary, {
    output$digitize_status <- renderText("Uploading boundary file...")
    output$upload_tip_boundary <- renderText("Uploading boundary file. Please wait...")
  }, ignoreInit = TRUE, priority = 1000)

  observeEvent(input$upload_ms_stack, {
    output$upload_tip_imagery <- renderText("Uploading imagery file. Please wait...")
  }, ignoreInit = TRUE, priority = 1000)
  observeEvent(input$upload_blue, {
    output$upload_tip_imagery <- renderText("Uploading blue band. Please wait...")
  }, ignoreInit = TRUE, priority = 1000)
  observeEvent(input$upload_green, {
    output$upload_tip_imagery <- renderText("Uploading green band. Please wait...")
  }, ignoreInit = TRUE, priority = 1000)
  observeEvent(input$upload_red, {
    output$upload_tip_imagery <- renderText("Uploading red band. Please wait...")
  }, ignoreInit = TRUE, priority = 1000)
  observeEvent(input$upload_re, {
    output$upload_tip_imagery <- renderText("Uploading red-edge band. Please wait...")
  }, ignoreInit = TRUE, priority = 1000)
  observeEvent(input$upload_nir, {
    output$upload_tip_imagery <- renderText("Uploading NIR band. Please wait...")
  }, ignoreInit = TRUE, priority = 1000)
  observeEvent(input$upload_vi_tif, {
    output$upload_tip_imagery <- renderText("File selected. Click 'Add VI Layer' to start upload.")
  }, ignoreInit = TRUE, priority = 1000)
  observeEvent(input$upload_soil_tif, {
    output$upload_tip_soil <- renderText("File selected. Click 'Add Soil Layer' to start upload.")
  }, ignoreInit = TRUE, priority = 1000)
  observeEvent(input$upload_other_tif, {
    output$upload_tip_other <- renderText("File selected. Click 'Add Layer' to start upload.")
  }, ignoreInit = TRUE, priority = 1000)

  observeEvent(input$upload_elevation_tif, {
    output$upload_tip_elevation <- renderText("Uploading DEM. Please wait...")
  }, ignoreInit = TRUE, priority = 1000)

  observeEvent(input$upload_historical_points, {
    output$field_compare_upload_tip <- renderText("Uploading field sample points. Please wait...")
  }, ignoreInit = TRUE, priority = 1000)
  
  observeEvent(input$upload_boundary, {
    req(input$upload_boundary)
    withProgress(message = "Uploading and validating boundary...", value = 0.5, {
      tryCatch({
        boundary_sf <- read_uploaded_boundary(input$upload_boundary)
        if (is.na(sf::st_crs(boundary_sf))) {
          showNotification("Boundary has no CRS; assuming WGS84 (EPSG:4326).", type = "warning", duration = 6)
          sf::st_crs(boundary_sf) <- 4326
        }
        boundary_sf <- tryCatch(sf::st_make_valid(boundary_sf), error = function(e) boundary_sf)
        geom_types <- st_geometry_type(boundary_sf)
        if (!all(geom_types %in% c("POLYGON", "MULTIPOLYGON"))) {
          showNotification("Please upload a valid boundary file. The boundary must be a polygon or multipolygon vector file.", type = "error")
          return()
        }
        committed <- commit_boundary_to_app(boundary_sf, "Boundary file uploaded successfully!")
        if (is.null(committed)) return(invisible(NULL))
        showNotification("Boundary uploaded and centered on the map.", type = "message")
        set_layer_tip("boundary")
      }, error = function(e) {
        showNotification(paste("Error reading boundary file:", e$message), type = "error")
      })
    })
  })
  
  observeEvent(input$clear_boundary, {
    shinyjs::reset("upload_boundary")
    clear_boundary_and_samples(FALSE)
    output$digitize_status <- renderText("")
    output$upload_tip_boundary <- renderText("")
    output$boundary_area <- renderText("")
  })
  
  # --- Conditional UI for Zoom Button ---
  has_zoomable_content <- reactive({
    !is.null(digitized_features()) ||
      !is.null(uploaded_planet_raster()) ||
      !is.null(uploaded_sentinel_raster()) ||
      !is.null(uploaded_elevation_raster()) ||
      length(elevation_aux_layers()) > 0 ||
      !is.null(uploaded_ms_raster()) ||
      length(soil_layers()) > 0 ||
      length(other_layers()) > 0 ||
      !is.null(sample_points()) ||
      !is.null(manual_points())
  })
  
  output$zoom_button_ui_boundary <- renderUI({ if(has_zoomable_content()) actionButton("zoom_to_area_boundary", "Zoom to Area", class = "btn-zoom btn-zoom-below") })
  output$zoom_button_ui_imagery <- renderUI({ if(has_zoomable_content()) actionButton("zoom_to_area_imagery", "Zoom to Area", class = "btn-zoom btn-zoom-below") })
  output$zoom_button_ui_elevation <- renderUI({ if(has_zoomable_content()) actionButton("zoom_to_area_elevation", "Zoom to Area", class = "btn-zoom btn-zoom-below") })
  output$zoom_button_ui_soil <- renderUI({ if(has_zoomable_content()) actionButton("zoom_to_area_soil", "Zoom to Area", class = "btn-zoom btn-zoom-below") })
  output$zoom_button_ui_other <- renderUI({ if(has_zoomable_content()) actionButton("zoom_to_area_other", "Zoom to Area", class = "btn-zoom btn-zoom-below") })
  output$zoom_button_ui_sampling_manual <- renderUI({ if(has_zoomable_content()) actionButton("zoom_to_area_sampling_manual", "Zoom to Area", class = "btn-zoom btn-zoom-below") })
  output$zoom_button_ui_sampling_auto <- renderUI({ if(has_zoomable_content()) actionButton("zoom_to_area_sampling_auto", "Zoom to Area", class = "btn-zoom btn-zoom-below") })
  
  # --- Observer for Zoom Button ---
  observeEvent(input$zoom_to_area_boundary, { trigger_zoom_modal() })
  observeEvent(input$zoom_to_area_imagery, { trigger_zoom_modal() })
  observeEvent(input$zoom_to_area_elevation, { trigger_zoom_modal() })
  observeEvent(input$zoom_to_area_soil, { trigger_zoom_modal() })
  observeEvent(input$zoom_to_area_other, { trigger_zoom_modal() })
  observeEvent(input$zoom_to_area_sampling_manual, { trigger_zoom_modal() })
  observeEvent(input$zoom_to_area_sampling_auto, { trigger_zoom_modal() })
  
  trigger_zoom_modal <- function() {
    available_layers <- list()
    if (!is.null(digitized_features())) available_layers[["Defined Boundary"]] <- "boundary"
    if (!is.null(uploaded_planet_raster())) available_layers[["Uploaded Planet Data"]] <- "planet"
    if (!is.null(uploaded_sentinel_raster())) available_layers[["Uploaded Sentinel Data"]] <- "sentinel"
    if (!is.null(uploaded_ms_raster())) available_layers[["Uploaded MS Data"]] <- "ms"
    if (!is.null(uploaded_elevation_raster())) available_layers[["Elevation DEM"]] <- "elevation_dem"
    if ("Slope" %in% names(elevation_aux_layers())) available_layers[["Elevation Slope"]] <- "elevation_slope"
    if ("Aspect" %in% names(elevation_aux_layers())) available_layers[["Elevation Aspect"]] <- "elevation_aspect"
    if ("TPI" %in% names(elevation_aux_layers())) available_layers[["Elevation TPI"]] <- "elevation_tpi"
    if ("TWI" %in% names(elevation_aux_layers())) available_layers[["Elevation TWI"]] <- "elevation_twi"
    if (length(soil_layers()) > 0) available_layers[["Uploaded Soil Data"]] <- "soil"
    if (length(other_layers()) > 0) available_layers[["Uploaded Other Data"]] <- "other"
    if (!is.null(sample_points())) available_layers[["Sample Points"]] <- "samples"
    if (!is.null(manual_points())) available_layers[["Manual Points"]] <- "manual_samples"
    
    if (length(available_layers) == 1) {
      zoom_to_layer(names(available_layers)[1])
    } else if (length(available_layers) > 1) {
      showModal(modalDialog(
        title = "Zoom to Layer",
        radioButtons("zoom_choice", "Choose a layer to zoom to:", choices = names(available_layers), selected = names(available_layers)[1]),
        footer = tagList(modalButton("Cancel"), actionButton("confirm_zoom", "Zoom"))
      ))
    } else {
      showNotification("No boundary or layer to zoom to.", type = "warning")
    }
  }
  
  # --- Helper function to perform the zoom ---
  zoom_to_layer <- function(layer_name) {
    target_bbox <- NULL
    get_raster_bbox <- function(r) {
      r_proj <- project_raster_bilinear(r, crs = TARGET_CRS)
      e <- extent(r_proj)
      c(xmin = e@xmin, ymin = e@ymin, xmax = e@xmax, ymax = e@ymax)
    }
    
    if (layer_name == "Defined Boundary") {
      target_bbox <- sf::st_bbox(sf::st_transform(digitized_features(), 4326))
    } else if (layer_name == "Uploaded Planet Data") target_bbox <- get_raster_bbox(uploaded_planet_raster())
    else if (layer_name == "Uploaded Sentinel Data") target_bbox <- get_raster_bbox(uploaded_sentinel_raster())
    else if (layer_name == "Uploaded MS Data") target_bbox <- get_raster_bbox(uploaded_ms_raster())
    else if (layer_name == "Elevation DEM") target_bbox <- get_raster_bbox(uploaded_elevation_raster())
    else if (layer_name == "Elevation Slope") target_bbox <- get_raster_bbox(elevation_aux_layers()[["Slope"]])
    else if (layer_name == "Elevation Aspect") target_bbox <- get_raster_bbox(elevation_aux_layers()[["Aspect"]])
    else if (layer_name == "Elevation TPI") target_bbox <- get_raster_bbox(elevation_aux_layers()[["TPI"]])
    else if (layer_name == "Elevation TWI") target_bbox <- get_raster_bbox(elevation_aux_layers()[["TWI"]])
    else if (layer_name == "Uploaded Soil Data") target_bbox <- get_raster_bbox(soil_layers()[[1]]) # Use first as example
    else if (layer_name == "Uploaded Other Data") target_bbox <- get_raster_bbox(other_layers()[[1]]) # Use first as example
    else if (layer_name == "Sample Points") target_bbox <- st_bbox(sample_points())
    else if (layer_name == "Manual Points") target_bbox <- st_bbox(manual_points())
    
    if (!is.null(target_bbox)) {
      x_range <- target_bbox[["xmax"]] - target_bbox[["xmin"]]; y_range <- target_bbox[["ymax"]] - target_bbox[["ymin"]]
      padding_factor <- 0.20
      padded_bbox <- c(
        xmin = target_bbox[["xmin"]] - (x_range * padding_factor),
        ymin = target_bbox[["ymin"]] - (y_range * padding_factor),
        xmax = target_bbox[["xmax"]] + (x_range * padding_factor),
        ymax = target_bbox[["ymax"]] + (y_range * padding_factor)
      )
      
      map_proxies <- list(leafletProxy("map-map"), leafletProxy("imagery_map"), leafletProxy("elevation_map"), leafletProxy("soil_map"), leafletProxy("other_map"), leafletProxy("sampling_auto_map"), leafletProxy("sampling_manual_map-map"))
      for (p in map_proxies) p %>% fitBounds(padded_bbox[["xmin"]], padded_bbox[["ymin"]], padded_bbox[["xmax"]], padded_bbox[["ymax"]])
      sync_maps_to_boundary(delay_fit = TRUE)
      apply_boundary_overlay_to_maps()
      
      shinyjs::delay(500, {
        if (!is.null(uploaded_planet_raster())) redraw_planet_raster()
        if (!is.null(uploaded_sentinel_raster())) redraw_sentinel_raster()
        if (!is.null(uploaded_ms_raster())) redraw_ms_raster()
        if (!is.null(uploaded_elevation_raster())) redraw_elevation_layers()
        if (length(soil_layers()) > 0) redraw_soil_layers()
        if (length(other_layers()) > 0) redraw_other_layers()
      })
    }
  }
  
  center_boundary_on_all_maps <- function(boundary_sf = NULL, padding_factor = 0.20) {
    refresh_boundary_on_maps(delay_fit = TRUE)
  }
  
  observeEvent(input$confirm_zoom, {
    req(input$zoom_choice); zoom_to_layer(input$zoom_choice); removeModal()
  })

  compute_boundary_risk_metrics <- function(boundary_sf) {
    if (is.null(boundary_sf) || nrow(boundary_sf) == 0) return(NULL)
    area_m2 <- as.numeric(sum(st_area(boundary_sf)))
    area_ha <- area_m2 / 10000
    area_km2 <- area_ha / 100
    area_acre <- area_ha * 2.47105
    caution_km2 <- 8
    safe_km2 <- 12
    level <- if (area_km2 > safe_km2) "danger" else if (area_km2 > caution_km2) "caution" else "ok"
    list(
      area_ha = area_ha,
      area_acre = area_acre,
      area_km2 = area_km2,
      safe_km2 = safe_km2,
      caution_km2 = caution_km2,
      level = level,
      recommended_buffer_m = recommended_buffer_m_for_ha(area_ha)
    )
  }

  area_tier_for_sampling_defaults <- function(area_ha) {
    area_ha <- suppressWarnings(as.numeric(area_ha))
    if (!is.finite(area_ha) || area_ha <= 0) return(NA_character_)
    if (area_ha < 5) return("xs")
    if (area_ha < 20) return("s")
    if (area_ha < 80) return("m")
    if (area_ha < 200) return("l")
    "xl"
  }

  default_compare_sample_sizes_for_area_ha <- function(area_ha, hosted = FALSE) {
    area_ha <- suppressWarnings(as.numeric(area_ha))
    if (!is.finite(area_ha) || area_ha <= 0) return("20,30,40,50")
    base <- if (area_ha < 5) {
      c(10L, 15L, 20L, 25L)
    } else if (area_ha < 20) {
      c(15L, 20L, 30L, 40L)
    } else if (area_ha < 80) {
      c(20L, 30L, 40L, 50L)
    } else if (area_ha < 200) {
      c(40L, 60L, 80L, 100L)
    } else {
      c(60L, 90L, 120L, 150L)
    }
    if (hosted) {
      top <- min(90L, max(base))
      low <- max(5L, min(base))
      if (top > low) {
        base <- unique(as.integer(round(seq(low, top, length.out = 4L))))
      }
    }
    base <- unique(sort(base[base >= 5L]))
    if (length(base) < 2L) base <- c(5L, max(10L, max(base)))
    if (length(base) > 4L) {
      base <- unique(as.integer(round(seq(min(base), max(base), length.out = 4L))))
    }
    paste(base, collapse = ",")
  }

  default_n_points_for_area_ha <- function(area_ha) {
    area_ha <- suppressWarnings(as.numeric(area_ha))
    if (!is.finite(area_ha) || area_ha <= 0) return(10L)
    if (area_ha < 5) return(10L)
    if (area_ha < 20) return(15L)
    if (area_ha < 80) return(25L)
    if (area_ha < 200) return(40L)
    60L
  }

  observeEvent(digitized_features(), {
    b <- digitized_features()
    if (is.null(b) || nrow(b) < 1L) {
      sampling_defaults_by_area$area_tier <- NA_character_
      return(invisible(NULL))
    }
    m <- compute_boundary_risk_metrics(b)
    if (is.null(m)) return(invisible(NULL))
    tier <- area_tier_for_sampling_defaults(m$area_ha)
    tier_prev <- sampling_defaults_by_area$area_tier
    if (identical(tier, tier_prev)) return(invisible(NULL))
    sampling_defaults_by_area$area_tier <- tier

    new_sizes <- default_compare_sample_sizes_for_area_ha(m$area_ha, hosted = is_hosted_shiny())
    new_n <- default_n_points_for_area_ha(m$area_ha)
    fresh_aoi <- is.na(tier_prev) || !nzchar(as.character(tier_prev))

    cur_sz <- isolate(input$compare_sample_sizes)
    cur_sz_n <- gsub("\\s+", "", if (is.null(cur_sz)) "" else cur_sz)
    auto_sz <- gsub("\\s+", "", sampling_defaults_by_area$last_auto_compare_sizes)
    if (fresh_aoi || !nzchar(cur_sz_n) || identical(cur_sz_n, auto_sz)) {
      updateTextInput(session, "compare_sample_sizes", value = new_sizes)
      sampling_defaults_by_area$last_auto_compare_sizes <- new_sizes
    }

    cur_n <- suppressWarnings(as.integer(isolate(input$n_points)))
    if (is.na(cur_n)) cur_n <- 10L
    if (fresh_aoi || identical(cur_n, as.integer(sampling_defaults_by_area$last_auto_n_points))) {
      updateNumericInput(session, "n_points", value = as.integer(new_n), min = 1L)
      sampling_defaults_by_area$last_auto_n_points <- as.integer(new_n)
    }
  }, ignoreNULL = FALSE)

  # Signed % difference of sample vs population: 100 * (sample - pop) / |pop|.
  pct_diff_sample_vs_pop <- function(sample_val, pop_val) {
    sample_val <- as.numeric(sample_val)
    pop_val <- as.numeric(pop_val)
    if (!is.finite(sample_val) || !is.finite(pop_val)) return(NA_real_)
    denom <- abs(pop_val)
    if (denom < 1e-12) {
      if (abs(sample_val) < 1e-12) return(0)
      return(NA_real_)
    }
    100 * (sample_val - pop_val) / denom
  }

  POP_SAMPLE_STAT_DIGITS <- 3L
  POP_SAMPLE_PCT_DIGITS <- 1L

  round_pop_sample_stat <- function(x) {
    x <- as.numeric(x)
    if (length(x) != 1L || !is.finite(x)) return(NA_real_)
    round(x, POP_SAMPLE_STAT_DIGITS)
  }

  pct_diff_sample_vs_pop_display <- function(sample_val, pop_val) {
    pct <- pct_diff_sample_vs_pop(
      round_pop_sample_stat(sample_val),
      round_pop_sample_stat(pop_val)
    )
    if (!is.finite(pct)) return(NA_real_)
    round(pct, POP_SAMPLE_PCT_DIGITS)
  }

  build_pop_sample_balance_row <- function(layer_nm, min_p, min_s, max_p, max_s, mean_p, mean_s, med_p, med_s) {
    min_p <- round_pop_sample_stat(min_p)
    min_s <- round_pop_sample_stat(min_s)
    max_p <- round_pop_sample_stat(max_p)
    max_s <- round_pop_sample_stat(max_s)
    mean_p <- round_pop_sample_stat(mean_p)
    mean_s <- round_pop_sample_stat(mean_s)
    med_p <- round_pop_sample_stat(med_p)
    med_s <- round_pop_sample_stat(med_s)
    data.frame(
      Layer = layer_nm,
      Min_pop = min_p, Min_samp = min_s,
      PctDiff_min = pct_diff_sample_vs_pop_display(min_s, min_p),
      Max_pop = max_p, Max_samp = max_s,
      PctDiff_max = pct_diff_sample_vs_pop_display(max_s, max_p),
      Mean_pop = mean_p, Mean_samp = mean_s,
      PctDiff_mean = pct_diff_sample_vs_pop_display(mean_s, mean_p),
      Median_pop = med_p, Median_samp = med_s,
      PctDiff_median = pct_diff_sample_vs_pop_display(med_s, med_p),
      stringsAsFactors = FALSE
    )
  }

  build_pop_vs_sample_balance_df <- function(pop_cov, sample_indices) {
    if (is.null(pop_cov) || ncol(pop_cov) < 1L || length(sample_indices) < 1L) return(NULL)
    samp <- pop_cov[sample_indices, , drop = FALSE]
    if (nrow(samp) < 1L) return(NULL)
    rows <- lapply(names(pop_cov), function(vn) {
      pv <- pop_cov[[vn]]
      sv <- samp[[vn]]
      pv <- pv[is.finite(pv)]
      sv <- sv[is.finite(sv)]
      if (length(pv) < 1L || length(sv) < 1L) return(NULL)
      build_pop_sample_balance_row(
        vn,
        min(pv), min(sv), max(pv), max(sv),
        mean(pv), mean(sv), stats::median(pv), stats::median(sv)
      )
    })
    rows <- Filter(Negate(is.null), rows)
    if (!length(rows)) return(NULL)
    dplyr::bind_rows(rows)
  }

  # Technique comparison display order: simple → complex.
  sampling_method_order <- c(
    "Simple Random", "Systematic Spread", "Spread + cLHS",
    "Zone-based", "cLHS", "Hybrid Zonal cLHS"
  )

  order_sampling_methods_df <- function(df, col = "Method") {
    if (is.null(df) || !nrow(df) || !col %in% names(df)) return(df)
    df[[col]] <- factor(df[[col]], levels = sampling_method_order)
    df <- df[order(df[[col]], na.last = TRUE), , drop = FALSE]
    df[[col]] <- as.character(df[[col]])
    df
  }

  elev_zoom_for_aoi <- function(boundary_sf, preferred_z = 13L) {
    preferred_z <- as.integer(preferred_z)
    if (length(preferred_z) != 1L || is.na(preferred_z)) preferred_z <- 13L
    preferred_z <- max(1L, min(14L, preferred_z))
    m <- compute_boundary_risk_metrics(boundary_sf)
    if (is.null(m)) return(preferred_z)
    if (m$area_km2 > m$safe_km2) return(max(10L, preferred_z - 2L))
    if (m$area_km2 > m$caution_km2) return(max(11L, preferred_z - 1L))
    preferred_z
  }

  render_boundary_risk_banner <- function(ctx = c("welcome", "variables")) {
    ctx <- match.arg(ctx)
    b <- digitized_features()
    if (is.null(b) || nrow(b) == 0) return(NULL)
    m <- compute_boundary_risk_metrics(b)
    if (is.null(m)) return(NULL)
    if (identical(m$level, "ok")) return(NULL)
    style <- if (identical(m$level, "danger")) {
      "margin: 8px 0 10px 0; padding: 10px 12px; border-radius: 10px; background: linear-gradient(120deg,#ffe9e9 0%,#ffd6d6 100%); border: 1px solid #f1a7a7; color: #7a1e1e; font-weight: 600;"
    } else {
      "margin: 8px 0 10px 0; padding: 10px 12px; border-radius: 10px; background: linear-gradient(120deg,#fff6df 0%,#ffe9b6 100%); border: 1px solid #f0cb7c; color: #6a4b00; font-weight: 600;"
    }
    prefix <- if (identical(ctx, "welcome")) "AOI Memory Warning" else "Variable Processing Warning"
    tags$div(
      style = style,
      strong(paste0(prefix, ": ")),
      paste0(
        "For 1 GB hosting, keep boundary around ", round(m$caution_km2, 1), " km² (",
        round(m$caution_km2 * 100, 0), " ha, ", round(m$caution_km2 * 100 * 2.47105, 0),
        " acres) or smaller for smoother runs. Around ",
        round(m$safe_km2, 1), " km² (", round(m$safe_km2 * 100, 0), " ha, ",
        round(m$safe_km2 * 100 * 2.47105, 0), " acres) and above can become unstable/crash. Current boundary: ",
        round(m$area_km2, 2), " km² (", round(m$area_ha, 1), " ha, ", round(m$area_acre, 1), " acres)."
      )
    )
  }

  output$boundary_risk_banner_welcome <- renderUI({
    render_boundary_risk_banner("welcome")
  })
  output$boundary_risk_banner_variables <- renderUI({
    render_boundary_risk_banner("variables")
  })


  field_report_content <- reactive({
    lines <- character(0)
    b <- digitized_features()
    if (is.null(b) || nrow(b) < 1L) {
      return(list(
        lines = c(
          "Status: boundary not defined.",
          "Next: draw or upload a boundary, load variables, then generate sample points."
        ),
        point_df = NULL,
        ready = FALSE
      ))
    }
    m <- compute_boundary_risk_metrics(b)
    if (!is.null(m)) {
      lines <- c(lines, paste0("Field area: ", round(m$area_ha, 2), " ha (", round(m$area_acre, 1), " acres)"))
    }
    all <- available_rasters()
    n_layers <- if (is.null(all)) 0L else length(all)
    lines <- c(lines, paste0("Covariate layers loaded: ", n_layers))
    s_meta <- sentinel_retrieval_meta()
    if (!is.null(s_meta)) {
      lines <- c(lines, format_sentinel_report_lines(s_meta))
    } else if (!is.null(uploaded_sentinel_raster()) || length(sentinel_vi_rasters()) > 0L) {
      mode_lab <- if (identical(sentinel_retrieval_used(), "median")) "Median composite" else "Single scene"
      lines <- c(lines, paste0("Sentinel imagery loaded (", mode_lab, "); scene details not recorded — re-retrieve to capture dates in the report."))
    }
    cov_sel <- isolate(input$sampling_covariate_layers)
    if (!is.null(cov_sel) && length(cov_sel)) {
      show_cov <- if (length(cov_sel) > 8L) {
        c(head(cov_sel, 8L), paste0("... +", length(cov_sel) - 8L, " more"))
      } else cov_sel
      lines <- c(lines, paste0("Covariates selected for sampling: ", paste(show_cov, collapse = ", ")))
    }
    cmp <- comparison_results()
    if (!is.null(cmp) && !is.null(cmp$winner)) {
      lines <- c(lines, paste0("Technique comparison winner: ", cmp$winner, " at n = ", cmp$n_points))
    }
    nz_field <- zonal_zone_count()
    if (is.null(nz_field) || is.na(nz_field)) {
      nz_cfg <- get_recommended_zone_k()
      if (!is.na(nz_cfg) && nz_cfg >= 2L) {
        lines <- c(lines, paste0("Zone setting (WSS): ", nz_cfg, " (click Apply Zones or run zone-based sampling to build the zone map)"))
      }
    }
    zsum <- zonal_cluster_summary()
    lines <- c(lines, format_zone_report_lines(nz_field, zsum))
    sm <- isolate(input$sampling_method)
    lines <- c(lines, paste0("Sampling mode: ", if (identical(sm, "manual")) "Manual digitizing" else "Automatic"))
    if (identical(sm, "manual")) {
      pts <- manual_points()
      if (!is.null(pts) && nrow(pts) > 0L) {
        lines <- c(lines, paste0("Manual sample points: ", nrow(pts)))
      } else {
        lines <- c(lines, "Manual sample points: none saved yet.")
      }
    } else {
      st <- isolate(input$sample_type)
      np <- isolate(input$n_points)
      buf <- isolate(input$buffer_distance)
      seed <- isolate(input$random_seed)
      lines <- c(lines, paste0("Automatic method: ", st))
      if (!identical(st, "Grid-based")) lines <- c(lines, paste0("Target sample count: ", np))
      if (identical(st, "Grid-based")) {
        gs <- isolate(input$grid_size_m)
        lines <- c(lines, paste0("Grid cell size: ", gs, " m"))
      }
      if (st %in% c("Zone-based", "Hybrid Zonal cLHS")) {
        nz <- get_recommended_zone_k()
        if ((is.null(nz_field) || is.na(nz_field)) && !is.na(nz) && nz >= 2L) {
          lines <- c(lines, paste0("Sampling zones (WSS): ", nz))
        }
      }
      lines <- c(lines, paste0("Inside boundary buffer: ", buf, " m"), paste0("Random seed: ", seed))
      pts <- sample_points()
      if (!is.null(pts) && nrow(pts) > 0L) {
        lines <- c(lines, paste0("Generated sample points: ", nrow(pts)))
      } else {
        lines <- c(lines, "Generated sample points: none yet — run automatic sampling first.")
      }
    }
    hp <- historical_sample_points()
    if (!is.null(hp) && inherits(hp, "sf") && nrow(hp) > 0L) {
      lines <- c(lines, paste0("Historical / previously collected points: ", nrow(hp)))
    }
    pt_src <- if (identical(sm, "manual")) manual_points() else sample_points()
    if (!is.null(pt_src) && nrow(pt_src) >= 2L) {
      qm <- compute_sampling_quality_metrics(pt_src, b, isolate(analysis_crs_string()))
      lines <- c(lines, qm$lines)
    }
    list(
      lines = lines,
      point_df = points_to_report_df(pt_src),
      ready = !is.null(pt_src) && inherits(pt_src, "sf") && nrow(pt_src) > 0L
    )
  })

  output$report_summary_ui <- renderUI({
    fc <- field_report_content()
    if (!isTRUE(fc$ready)) {
      return(tags$div(
        class = "alert alert-warning",
        style = "margin-top:12px;",
        tags$strong("Not ready for export yet. "),
        "Define a boundary, load at least one variable layer, and generate or digitize sample points."
      ))
    }
    tags$div(
      style = "margin-top:12px; padding:10px 12px; border:1px solid #cfe6ff; border-radius:10px; background:#f5faff;",
      tags$strong("Plan looks complete. "),
      "Download the PDF for the summary, sectioned maps, sample density plots, historical comparison (if uploaded), and technique comparison when available."
    )
  })

  output$report_preview_text <- renderText({
    paste(field_report_content()$lines, collapse = "\n")
  })
  outputOptions(output, "report_preview_text", suspendWhenHidden = TRUE)
  outputOptions(output, "report_summary_ui", suspendWhenHidden = TRUE)

  output$download_sampling_report_pdf <- downloadHandler(
    filename = function() paste0("geosampler_sampling_plan_", Sys.Date(), ".pdf"),
    content = function(file) {
      fc <- field_report_content()
      b <- digitized_features()
      all <- available_rasters()
      sm <- isolate(input$sampling_method)
      pts <- if (identical(sm, "manual")) manual_points() else sample_points()
      hist_pts <- historical_sample_points()
      report_sections <- list()

      map_specs <- list()
      zone_r <- zonal_zone_raster()
      nz <- zonal_zone_count()
      if (!is.null(zone_r)) {
        if (is.null(nz) || is.na(nz)) {
          lv <- unique(as.integer(stats::na.omit(raster::values(zone_r))))
          nz <- if (length(lv)) length(lv) else NULL
        }
        map_specs[[length(map_specs) + 1L]] <- list(
          type = "zones",
          title = "Field management zones",
          raster = prepare_zone_raster_for_report_plot(zone_r, b),
          n_zones = nz,
          description = "Covariate-based management zones used for zone-based and hybrid zonal cLHS sampling. Downsampled for PDF export."
        )
      }
      ndvi_r <- pick_report_ndvi_raster(all)
      if (!is.null(ndvi_r)) {
        map_specs[[length(map_specs) + 1L]] <- list(
          type = "raster", style = "ndvi", title = "NDVI",
          raster = prepare_raster_for_report_plot(ndvi_r, b),
          description = "Vegetation greenness (NDVI) across the field boundary."
        )
      }
      elev_r <- if (!is.null(all) && "Elevation_DEM" %in% names(all)) all[["Elevation_DEM"]] else uploaded_elevation_raster()
      if (!is.null(elev_r)) {
        map_specs[[length(map_specs) + 1L]] <- list(
          type = "raster", style = "elev", title = "Elevation (DEM)",
          raster = prepare_raster_for_report_plot(elev_r, b),
          description = "Elevation surface across the AOI."
        )
      }
      if (!is.null(b) && !is.null(pts) && inherits(pts, "sf") && nrow(pts) > 0L) {
        map_specs[[length(map_specs) + 1L]] <- list(
          type = "points", title = "Sample points and field boundary",
          boundary = b, points = pts,
          description = "App-generated or saved manual sample points within the field boundary."
        )
      }
      if (!is.null(b) && !is.null(hist_pts) && inherits(hist_pts, "sf") && nrow(hist_pts) > 0L) {
        map_specs[[length(map_specs) + 1L]] <- list(
          type = "points_compare",
          title = "App vs historical sample locations",
          boundary = b,
          app_points = pts,
          historical_points = hist_pts,
          description = "Blue = app sample plan; orange = previously collected historical GPS points."
        )
      }
      if (length(map_specs)) {
        report_sections[[length(report_sections) + 1L]] <- list(
          heading = "Section 1 — Field maps",
          subtitle = "Zones, vegetation, terrain, and sample locations",
          bullets = c(
            "Raster layers are downsampled inside the boundary for a fast PDF.",
            paste0(length(map_specs), " map page(s) in this section.")
          ),
          map_specs = map_specs,
          plot_pages = list()
        )
      }

      density_plot <- NULL
      if (!is.null(pts) && inherits(pts, "sf") && nrow(pts) > 0L) {
        density_plot <- tryCatch(
          build_sampling_density_facets_plot(sf::st_drop_geometry(pts), fill_color = "#8ecae6"),
          error = function(e) NULL
        )
      }
      if (!is.null(density_plot)) {
        report_sections[[length(report_sections) + 1L]] <- list(
          heading = "Section 2 — Sample covariate densities",
          subtitle = "Kernel density of values extracted at app sample points (selected covariate layers)",
          bullets = c(
            "Matches the density view on the Sampling tab.",
            "One combined page with a panel per covariate layer."
          ),
          map_specs = list(),
          plot_pages = list(list(
            kind = "density_facets",
            title = "Covariate density distributions (app samples)",
            description = "Smoothed density of covariate values at each app sample location. Useful for spotting skewed or multi-modal sample coverage in covariate space.",
            plot = density_plot
          ))
        )
      }

      violin_plots <- limit_ggplot_list(summary_distribution_plots_cache(), REPORT_MAX_DIST_PLOTS)
      if (!length(violin_plots)) {
        app_df <- app_compare_values_df()
        if (!is.null(app_df) && nrow(app_df) > 0L) {
          app_df <- app_df[app_df$Source == "App-generated", , drop = FALSE]
          if (nrow(app_df) > 0L) {
            raster_limits <- precompute_raster_layer_limits(all)
            layers <- unique(as.character(app_df$Layer))
            cov_keep <- isolate(active_covariate_layer_names_r())
            if (length(cov_keep)) {
              layers <- intersect(layers, cov_keep)
            }
            if (length(layers) > REPORT_MAX_DIST_PLOTS) {
              layers <- layers[seq_len(REPORT_MAX_DIST_PLOTS)]
            }
            pop_long <- tryCatch(
              population_values_long_for_layers(
                layers,
                boundary_sf = b,
                all_r = all,
                max_cells_per_layer = REPORT_POP_CELLS_PER_LAYER
              ),
              error = function(e) NULL
            )
            violin_plots <- lapply(layers, function(lyr) {
              pop_vals <- NULL
              if (!is.null(pop_long) && nrow(pop_long) > 0L) {
                pop_vals <- pop_long$Value[pop_long$Layer == lyr]
              }
              tryCatch(
                build_population_sample_distribution_plot(
                  layer_name = lyr,
                  sample_df = app_df[app_df$Layer == lyr, , drop = FALSE],
                  pop_values = pop_vals,
                  raster_limits = raster_limits
                ),
                error = function(e) NULL
              )
            })
            violin_plots <- limit_ggplot_list(violin_plots, REPORT_MAX_DIST_PLOTS)
          }
        }
      }
      violin_pages <- ggplot_list_to_report_plot_pages(
        violin_plots,
        title_prefix = "Sample vs population",
        description = paste0(
          "Gray cloud = population subsample in the AOI (max ", REPORT_POP_CELLS_PER_LAYER,
          " cells/layer for PDF); blue points = app sample values. Up to ",
          REPORT_MAX_DIST_PLOTS, " layers per report."
        ),
        kind = "distribution",
        max_n = REPORT_MAX_DIST_PLOTS
      )
      if (length(violin_pages)) {
        report_sections[[length(report_sections) + 1L]] <- list(
          heading = "Section 3 — Sample vs population distributions",
          subtitle = "Per-layer violin overlays (app sample vs field population)",
          bullets = c(
            "Population cloud is subsampled for memory-safe PDF export.",
            paste0(length(violin_pages), " layer plot(s) included (cap = ", REPORT_MAX_DIST_PLOTS, ").")
          ),
          map_specs = list(),
          plot_pages = violin_pages
        )
      }

      if (!is.null(hist_pts) && inherits(hist_pts, "sf") && nrow(hist_pts) > 0L) {
        fc_plots <- limit_ggplot_list(field_compare_distribution_plots_cache(), REPORT_MAX_FIELD_COMPARE_PLOTS)
        if (!length(fc_plots)) {
          app_df_fc <- app_compare_values_df()
          hist_df_fc <- historical_compare_values_df()
          if (is.null(app_df_fc) || is.null(hist_df_fc) ||
              nrow(app_df_fc) < 1L || nrow(hist_df_fc) < 1L) {
            tryCatch(run_field_compare_extraction(), error = function(e) invisible(NULL))
            app_df_fc <- app_compare_values_df()
            hist_df_fc <- historical_compare_values_df()
          }
          if (!is.null(app_df_fc) && !is.null(hist_df_fc) &&
              nrow(app_df_fc) > 0L && nrow(hist_df_fc) > 0L) {
            d_fc <- dplyr::bind_rows(app_df_fc, hist_df_fc)
            raster_limits <- precompute_raster_layer_limits(all)
            layers_fc <- unique(as.character(d_fc$Layer))
            if (length(layers_fc) > REPORT_MAX_FIELD_COMPARE_PLOTS) {
              layers_fc <- layers_fc[seq_len(REPORT_MAX_FIELD_COMPARE_PLOTS)]
            }
            fc_plots <- lapply(layers_fc, function(lyr) {
              lyr_df <- d_fc[d_fc$Layer == lyr, , drop = FALSE]
              tryCatch(
                build_field_compare_distribution_plot(
                  layer_name = lyr,
                  app_df = lyr_df[lyr_df$Source == "App-generated", , drop = FALSE],
                  hist_df = lyr_df[lyr_df$Source == "Historical", , drop = FALSE],
                  raster_limits = raster_limits
                ),
                error = function(e) NULL
              )
            })
            fc_plots <- limit_ggplot_list(fc_plots, REPORT_MAX_FIELD_COMPARE_PLOTS)
          }
        }
        fc_pages <- ggplot_list_to_report_plot_pages(
          fc_plots,
          title_prefix = "Historical vs app",
          description = paste0(
            "Orange cloud = previously collected historical points; blue = app sample plan. Up to ",
            REPORT_MAX_FIELD_COMPARE_PLOTS, " layers per report."
          ),
          kind = "field_compare",
          max_n = REPORT_MAX_FIELD_COMPARE_PLOTS
        )
        if (length(fc_pages)) {
          report_sections[[length(report_sections) + 1L]] <- list(
            heading = "Section 4 — Historical vs app sample comparison",
            subtitle = "Covariate distributions for uploaded historical GPS points vs new app samples",
            bullets = c(
              paste0("Historical points uploaded: ", nrow(hist_pts), "."),
              paste0(length(fc_pages), " layer plot(s) included (cap = ", REPORT_MAX_FIELD_COMPARE_PLOTS, ").")
            ),
            map_specs = list(),
            plot_pages = fc_pages
          )
        }
      }

      cost_cc <- compute_cost_comparison(
        isolate(input$cost_prior_samples),
        isolate(input$cost_app_samples),
        isolate(input$cost_per_sample)
      )
      if (isTRUE(cost_cc$ok)) {
        cost_cur <- resolve_cost_currency_label(
          isolate(input$cost_currency_preset),
          isolate(input$cost_currency_custom)
        )
        cost_plot <- tryCatch(
          build_cost_comparison_plot(cost_cc, cost_cur),
          error = function(e) NULL
        )
        if (!is.null(cost_plot)) {
          savings_bullet <- if (is.finite(cost_cc$savings_pct) && cost_cc$savings_pct > 0) {
            sprintf(
              "Estimated savings vs prior design: %.1f%% lower total cost (%s %s).",
              cost_cc$savings_pct,
              format(round(cost_cc$savings_abs, 2), big.mark = ",", trim = TRUE),
              cost_cur
            )
          } else {
            "Adjust sample counts and unit cost on the Cost comparison tab to refresh this section."
          }
          report_sections[[length(report_sections) + 1L]] <- list(
            heading = "Section 5 — Sampling cost comparison",
            subtitle = "Total cost for prior / grid sampling vs app-recommended sample size",
            bullets = c(
              paste0("Prior / grid: n = ", cost_cc$prior_n, ", total = ",
                     format(round(cost_cc$prior_total, 2), big.mark = ",", trim = TRUE), " ", cost_cur, "."),
              paste0("App design: n = ", cost_cc$app_n, ", total = ",
                     format(round(cost_cc$app_total, 2), big.mark = ",", trim = TRUE), " ", cost_cur, "."),
              paste0("Unit cost per sample: ", cost_cc$cost_per_sample, " (", cost_cur, ")."),
              savings_bullet
            ),
            map_specs = list(),
            plot_pages = list(list(
              kind = "cost_comparison",
              title = "Sampling cost: prior vs app design",
              description = paste0(
                "Orange bar = prior / grid (n = ", cost_cc$prior_n, "); blue bar = app plan (n = ", cost_cc$app_n, ")."
              ),
              plot = cost_plot
            ))
          )
        }
      }

      cmp <- comparison_results()
      cmp_plot <- build_comparison_curve_plot(cmp)
      if (!is.null(cmp_plot)) {
        cmp_desc <- paste0(
          "Compares sampling methods at multiple sample sizes (", COMPARE_WEIGHTS_LABEL, "). ",
          if (!is.null(cmp$winner) && !is.null(cmp$n_points)) {
            paste0("Winner: ", cmp$winner, " at n = ", cmp$n_points, ". ")
          } else "",
          "Dashed line: recommended sample size."
        )
        report_sections[[length(report_sections) + 1L]] <- list(
          heading = "Section 6 — Technique comparison",
          subtitle = "Recommended sample size from multi-method evaluation",
          bullets = c(
            "Run Technique comparison on the Sampling tab to refresh this section.",
            "Each line is a method; y-axis is the composite score across repeats."
          ),
          map_specs = list(),
          plot_pages = list(list(
            kind = "comparison",
            title = "Technique comparison by sample size",
            description = cmp_desc,
            plot = cmp_plot
          ))
        )
      }

      write_sampling_report_pdf(file, fc$lines, fc$point_df, report_sections = report_sections)
      tryCatch(gc(verbose = FALSE), error = function(e) invisible(NULL))
    }
  )
  
  output$download_geojson <- downloadHandler(
    filename = function() { paste0("boundary_", Sys.Date(), ".geojson") },
    content = function(file) {
      req(!is.null(digitized_features()))
      suppressWarnings(st_write(digitized_features(), file, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE))
    }
  )
  
  BOUNDARY_REQUIRED_MAIN_TABS <- c("Variables", "Sampling", "FieldCompare", "Report")

  boundary_is_defined <- function() {
    b <- digitized_features()
    !is.null(b) && inherits(b, "sf") && nrow(b) > 0L
  }

  notify_boundary_required <- function(context = "this step") {
    showNotification(
      paste0(
        "No field boundary yet. Draw or upload your AOI on the Boundary tab before ",
        context,
        ". Maps, imagery retrieval, and sampling all need an active boundary."
      ),
      type = "warning",
      duration = 9
    )
    invisible(NULL)
  }

  redirect_to_boundary_tab <- function() {
    updateTabsetPanel(session, "main_tabs", selected = "Boundary")
    invisible(NULL)
  }

  guard_boundary_for_main_tab <- function(main_tab, context = NULL) {
    main_tab <- as.character(main_tab)[1L]
    if (!nzchar(main_tab) || !main_tab %in% BOUNDARY_REQUIRED_MAIN_TABS) {
      return(TRUE)
    }
    if (boundary_is_defined()) return(TRUE)
    ctx <- context %||% paste0("using ", main_tab)
    notify_boundary_required(ctx)
    redirect_to_boundary_tab()
    FALSE
  }

  render_boundary_required_banner <- function() {
    if (boundary_is_defined()) return(NULL)
    tags$div(
      class = "boundary-required-banner",
      icon("triangle-exclamation"),
      tags$div(
        class = "boundary-required-banner-text",
        tags$strong("Boundary required"),
        tags$p(
          style = "margin:6px 0 0 0; font-weight:500;",
          "You have not drawn or uploaded a field boundary (AOI) yet. ",
          "Go to the ",
          tags$strong("Boundary"),
          " tab, digitize a polygon or upload a boundary file, then return here. ",
          "Variable maps and data retrieval will not work correctly without an AOI."
        ),
        actionButton(
          "go_to_boundary_from_guard",
          "Go to Boundary tab",
          class = "btn-warning btn-sm",
          style = "margin-top:8px;"
        )
      )
    )
  }

  observeEvent(input$next_to_variables, {
    if (!boundary_is_defined()) {
      notify_boundary_required("opening Variables")
      redirect_to_boundary_tab()
      return(invisible(NULL))
    }
    updateTabsetPanel(session, "main_tabs", selected = "Variables")
  })

  observeEvent(input$go_to_boundary_from_guard, {
    redirect_to_boundary_tab()
  }, ignoreInit = TRUE)

  sampling_covariate_guard_proceed <- reactiveVal(FALSE)

  resolve_imagery_source_for_guard <- function() {
    src <- isolate(input$imagery_source) %||% ""
    if (nzchar(src)) return(src)
    if (length(sentinel_vi_rasters()) > 0L ||
        !is.null(isolate(uploaded_sentinel_raster())) ||
        !is.null(isolate(sentinel_result()))) {
      return("download_sentinel")
    }
    if (length(vi_rasters()) > 0L ||
        !is.null(isolate(uploaded_planet_raster())) ||
        !is.null(isolate(planet_result()))) {
      return("download_planet")
    }
    if (length(ms_vi_rasters()) > 0L || !is.null(isolate(uploaded_ms_raster()))) {
      return("upload_ms")
    }
    ""
  }

  present_imagery_vis_for_source <- function(src) {
    raw <- switch(
      src,
      download_planet = names(vi_rasters()),
      download_sentinel = names(sentinel_vi_rasters()),
      upload_ms = names(ms_vi_rasters()),
      character()
    )
    normalize_imagery_vi_names(raw)
  }

  imagery_has_spectral_stack_for_source <- function(src) {
    switch(
      src,
      download_planet = !is.null(uploaded_planet_raster()),
      download_sentinel = !is.null(uploaded_sentinel_raster()),
      upload_ms = !is.null(uploaded_ms_raster()),
      FALSE
    )
  }

  sampling_covariate_layer_status <- function() {
    std_vis <- standard_vi_base_names()
    src <- resolve_imagery_source_for_guard()
    present_vis <- if (nzchar(src)) present_imagery_vis_for_source(src) else character()
    missing_vis <- setdiff(std_vis, present_vis)
    has_stack <- if (nzchar(src)) imagery_has_spectral_stack_for_source(src) else FALSE

    elev_needed <- standard_elevation_layer_names()
    missing_elev <- character()
    if (is.null(uploaded_elevation_raster())) {
      missing_elev <- elev_needed
    } else {
      aux <- names(elevation_aux_layers())
      for (lyr in c("Slope", "Aspect", "TPI", "TWI")) {
        if (!lyr %in% aux) missing_elev <- c(missing_elev, lyr)
      }
    }

    list(
      imagery_source = src,
      missing_imagery = missing_vis,
      missing_elevation = missing_elev,
      imagery_incomplete = length(missing_vis) > 0L,
      elevation_incomplete = length(missing_elev) > 0L,
      imagery_has_stack = has_stack,
      needs_warning = length(missing_vis) > 0L || length(missing_elev) > 0L
    )
  }

  show_sampling_covariate_guard_modal <- function(st) {
    img_hint <- if (isTRUE(st$imagery_incomplete)) {
      if (isTRUE(st$imagery_has_stack)) {
        "Spectral imagery is loaded but vegetation indices are not calculated yet. On the Imagery map panel, click Calculate VIs."
      } else if (nzchar(st$imagery_source)) {
        "Retrieve or upload imagery, then click Calculate VIs on the Imagery map panel."
      } else {
        "Choose Planet, Sentinel-2, or Upload on the Imagery tab, load a stack, then click Calculate VIs."
      }
    } else {
      NULL
    }
    elev_hint <- if (isTRUE(st$elevation_incomplete)) {
      if ("DEM" %in% st$missing_elevation) {
        "Retrieve or upload a DEM on the Elevation tab, then click Calculate Slope, Aspect, TPI, TWI."
      } else {
        "DEM is loaded but terrain derivatives are missing. On the Elevation map panel, click Calculate Slope, Aspect, TPI, TWI."
      }
    } else {
      NULL
    }

    footer_btns <- list()
    if (isTRUE(st$imagery_incomplete)) {
      footer_btns[[length(footer_btns) + 1L]] <- actionButton(
        "sampling_guard_go_imagery",
        "Calculate imagery VIs",
        class = "btn-primary"
      )
    }
    if (isTRUE(st$elevation_incomplete)) {
      footer_btns[[length(footer_btns) + 1L]] <- actionButton(
        "sampling_guard_go_elevation",
        "Calculate terrain layers",
        class = "btn-info"
      )
    }
    footer_btns[[length(footer_btns) + 1L]] <- actionButton(
      "sampling_guard_proceed_sampling",
      "Yes, continue",
      class = "btn-success"
    )

    showModal(modalDialog(
      title = "Covariate layers not ready",
      easyClose = FALSE,
      footer = do.call(tagList, footer_btns),
      tags$p(
        "Some standard layers used for sampling are not calculated yet. ",
        "You can go calculate them now or continue to Sampling without them."
      ),
      if (isTRUE(st$imagery_incomplete)) {
        tags$div(
          style = "margin:12px 0 10px 0; padding:10px 12px; border-radius:10px; background:#f8fbff; border:1px solid #cfe3ff;",
          tags$p(style = "margin:0 0 6px 0; font-weight:700; color:#1b3f66;",
                 icon("satellite"), " Imagery — 6 vegetation indices"),
          tags$p(style = "margin:0; font-size:12.5px; color:#4a5f78;",
                 "Missing: ", tags$strong(format_layer_list_for_modal(st$missing_imagery))),
          if (!is.null(img_hint)) tags$p(style = "margin:6px 0 0 0; font-size:12px; color:#6b7c93;", img_hint)
        )
      },
      if (isTRUE(st$elevation_incomplete)) {
        tags$div(
          style = "margin:0 0 4px 0; padding:10px 12px; border-radius:10px; background:#f6faf4; border:1px solid #cfe8c9;",
          tags$p(style = "margin:0 0 6px 0; font-weight:700; color:#1a5c32;",
                 icon("mountain"), " Elevation — 5 terrain layers (DEM + derivatives)"),
          tags$p(style = "margin:0; font-size:12.5px; color:#4a5f78;",
                 "Missing: ", tags$strong(format_layer_list_for_modal(st$missing_elevation))),
          if (!is.null(elev_hint)) tags$p(style = "margin:6px 0 0 0; font-size:12px; color:#6b7c93;", elev_hint)
        )
      }
    ))
  }

  maybe_show_sampling_covariate_guard <- function() {
    if (isTRUE(sampling_covariate_guard_proceed())) return(invisible(NULL))
    st <- sampling_covariate_layer_status()
    if (!isTRUE(st$needs_warning)) return(invisible(NULL))
    show_sampling_covariate_guard_modal(st)
    invisible(NULL)
  }

  observeEvent(input$sampling_guard_proceed_sampling, {
    removeModal()
    sampling_covariate_guard_proceed(TRUE)
  }, ignoreInit = TRUE)

  observeEvent(input$sampling_guard_go_imagery, {
    removeModal()
    st <- sampling_covariate_layer_status()
    src <- st$imagery_source
    if (!nzchar(src)) src <- "download_sentinel"
    navigate_workflow(
      main_tab = "Variables",
      variables_sub = "imagery",
      imagery_source = src,
      toast = "Load imagery if needed, then click Calculate VIs for all six indices."
    )
  }, ignoreInit = TRUE)

  observeEvent(input$sampling_guard_go_elevation, {
    removeModal()
    elev_src <- isolate(input$elevation_source) %||% ""
    if (!nzchar(elev_src)) elev_src <- "download_elevation"
    navigate_workflow(
      main_tab = "Variables",
      variables_sub = "elevation",
      elevation_source = elev_src,
      toast = "Load a DEM if needed, then click Calculate Slope, Aspect, TPI, TWI."
    )
  }, ignoreInit = TRUE)

  observeEvent(input$main_tabs, {
    tab <- input$main_tabs
    if (!nzchar(tab) || !tab %in% BOUNDARY_REQUIRED_MAIN_TABS) return(invisible(NULL))
    if (!boundary_is_defined()) {
      ctx <- switch(
        tab,
        Variables = "opening Variables (imagery, elevation, and maps need an AOI)",
        Sampling = "opening Sampling",
        FieldCompare = "opening App vs prior",
        Report = "opening Report",
        paste0("opening ", tab)
      )
      notify_boundary_required(ctx)
      redirect_to_boundary_tab()
      return(invisible(NULL))
    }
    if (identical(tab, "Sampling")) {
      maybe_show_sampling_covariate_guard()
    }
    invisible(NULL)
  }, ignoreInit = TRUE)

  output$boundary_required_banner_variables <- renderUI({
    req(identical(input$main_tabs, "Variables"))
    render_boundary_required_banner()
  })
  outputOptions(output, "boundary_required_banner_variables", suspendWhenHidden = FALSE)

  output$boundary_required_banner_sampling <- renderUI({
    req(identical(input$main_tabs, "Sampling"))
    render_boundary_required_banner()
  })
  outputOptions(output, "boundary_required_banner_sampling", suspendWhenHidden = FALSE)

  output$boundary_required_banner_field_compare <- renderUI({
    req(identical(input$main_tabs, "FieldCompare"))
    render_boundary_required_banner()
  })
  outputOptions(output, "boundary_required_banner_field_compare", suspendWhenHidden = FALSE)

  navigate_workflow <- function(
    main_tab = NULL,
    variables_sub = NULL,
    sampling_sub = NULL,
    field_compare_sub = NULL,
    boundary_method = NULL,
    imagery_source = NULL,
    elevation_source = NULL,
    sentinel_mode = NULL,
    sampling_method = NULL,
    toast = NULL
  ) {
    if (!is.null(main_tab) && nzchar(main_tab)) {
      if (!guard_boundary_for_main_tab(main_tab, context = toast %||% paste0("using ", main_tab))) {
        return(invisible(NULL))
      }
    }
    if (!is.null(main_tab) && nzchar(main_tab)) {
      updateTabsetPanel(session, "main_tabs", selected = main_tab)
    }
    if (!is.null(variables_sub) && nzchar(variables_sub)) {
      tryCatch(updateTabsetPanel(session, "variables_subtabs", selected = variables_sub), error = function(e) NULL)
    }
    if (!is.null(sampling_sub) && nzchar(sampling_sub)) {
      tryCatch(updateTabsetPanel(session, "sampling_subtabs", selected = sampling_sub), error = function(e) NULL)
    }
    if (!is.null(field_compare_sub) && nzchar(field_compare_sub)) {
      tryCatch(updateTabsetPanel(session, "field_compare_subtabs", selected = field_compare_sub), error = function(e) NULL)
    }
    if (!is.null(boundary_method) && nzchar(boundary_method)) {
      tryCatch(updateSelectInput(session, "boundary_method", selected = boundary_method), error = function(e) NULL)
    }
    if (!is.null(imagery_source) && nzchar(imagery_source)) {
      tryCatch(updateSelectInput(session, "imagery_source", selected = imagery_source), error = function(e) NULL)
    }
    if (!is.null(elevation_source) && nzchar(elevation_source)) {
      tryCatch(updateSelectInput(session, "elevation_source", selected = elevation_source), error = function(e) NULL)
    }
    if (!is.null(sentinel_mode) && nzchar(sentinel_mode)) {
      tryCatch(updateRadioButtons(session, "sentinel_retrieval_mode", selected = sentinel_mode), error = function(e) NULL)
    }
    if (!is.null(sampling_method) && nzchar(sampling_method)) {
      tryCatch(updateSelectInput(session, "sampling_method", selected = sampling_method), error = function(e) NULL)
    }
    if (!is.null(toast) && nzchar(toast)) {
      showNotification(toast, type = "message", duration = 4)
    }
    invisible(NULL)
  }

  wf_nav_specs <- list(
    wf_nav_boundary = list(main_tab = "Boundary", toast = "Boundary — draw or upload your AOI."),
    wf_nav_boundary_digitize = list(main_tab = "Boundary", boundary_method = "digitize", toast = "Boundary — draw your polygon on the map."),
    wf_nav_boundary_upload = list(main_tab = "Boundary", boundary_method = "upload", toast = "Boundary — upload an existing polygon file."),
    wf_nav_variables_imagery = list(main_tab = "Variables", variables_sub = "imagery", toast = "Variables & derivatives — Imagery."),
    wf_nav_imagery_planet = list(main_tab = "Variables", variables_sub = "imagery", imagery_source = "download_planet", toast = "Imagery — Planet retrieval."),
    wf_nav_imagery_sentinel = list(main_tab = "Variables", variables_sub = "imagery", imagery_source = "download_sentinel", toast = "Imagery — Sentinel-2 (free)."),
    wf_nav_imagery_upload = list(main_tab = "Variables", variables_sub = "imagery", imagery_source = "upload_ms", toast = "Imagery — upload your own rasters."),
    wf_nav_sentinel_single = list(main_tab = "Variables", variables_sub = "imagery", imagery_source = "download_sentinel", sentinel_mode = "single", toast = "Sentinel — single-scene mode. Search, then retrieve."),
    wf_nav_sentinel_median = list(main_tab = "Variables", variables_sub = "imagery", imagery_source = "download_sentinel", sentinel_mode = "median", toast = "Sentinel — per-pixel median composite."),
    wf_nav_variables_elevation = list(main_tab = "Variables", variables_sub = "elevation", toast = "Variables & derivatives — Elevation."),
    wf_nav_elevation_download = list(main_tab = "Variables", variables_sub = "elevation", elevation_source = "download_elevation", toast = "Elevation — retrieve DEM for AOI."),
    wf_nav_elevation_upload = list(main_tab = "Variables", variables_sub = "elevation", elevation_source = "upload_elevation", toast = "Elevation — upload DEM / terrain layers."),
    wf_nav_variables_other = list(main_tab = "Variables", variables_sub = "other_layers", toast = "Variables & derivatives — Other layers."),
    wf_nav_variables_summary = list(main_tab = "Variables", variables_sub = "var_summary", toast = "Variable summary — compute min/max/mean/median."),
    wf_nav_sampling_compare = list(main_tab = "Sampling", sampling_sub = "tcmp", toast = "Technique comparison (optional)."),
    wf_nav_sampling_generate = list(main_tab = "Sampling", sampling_sub = "samp", toast = "Sampling — generate or edit points."),
    wf_nav_sampling_manual = list(main_tab = "Sampling", sampling_sub = "samp", sampling_method = "manual", toast = "Sampling — manual point placement."),
    wf_nav_sampling_auto = list(main_tab = "Sampling", sampling_sub = "samp", sampling_method = "automatic", toast = "Sampling — automatic designs (Spread+cLHS, cLHS, zones, …)."),
    wf_nav_sampling_summary = list(main_tab = "Sampling", sampling_sub = "samp_summary", toast = "Population vs sample summary and plots."),
    wf_nav_field_compare = list(main_tab = "FieldCompare", field_compare_sub = "field_compare_map_tab", toast = "App vs prior — upload historical points."),
    wf_nav_cost_compare = list(main_tab = "CostCompare", toast = "Cost — prior grid vs app sample cost."),
    wf_nav_report = list(main_tab = "Report", toast = "Report — preview and download PDF.")
  )

  for (wf_id in names(wf_nav_specs)) {
    local({
      btn_id <- wf_id
      spec <- wf_nav_specs[[btn_id]]
      observeEvent(
        input[[btn_id]],
        {
          do.call(navigate_workflow, spec)
        },
        ignoreInit = TRUE
      )
    })
  }
  
  # --- Centralized Layer Control Observer (skip when no map tab is active) ---
  observe({
    current_layers <- active_overlays()
    main <- isolate(input$main_tabs)
    if (nzchar(main) && !main %in% c("Welcome", "Report")) {
      map_proxies <- list(leafletProxy("imagery_map"), leafletProxy("elevation_map"), leafletProxy("soil_map"), leafletProxy("other_map"), leafletProxy("sampling_auto_map"), leafletProxy("sampling_manual_map-map"))
      for (p in map_proxies) {
        tryCatch({
          p %>% clearControls() %>% addLayersControl(
            baseGroups = c("Satellite", "Street Map"),
            overlayGroups = current_layers,
            options = layersControlOptions(collapsed = FALSE)
          )
        }, error = function(e) NULL)
      }
    }
  })
  
  # --- Render Base Maps ---
  output$imagery_map <- renderLeaflet({
    initial_map %>% setView(lng = map_state$center$lng, lat = map_state$center$lat, zoom = map_state$zoom)
  })
  output$elevation_map <- renderLeaflet({
    initial_map %>% setView(lng = map_state$center$lng, lat = map_state$center$lat, zoom = map_state$zoom)
  })
  output$other_map <- renderLeaflet({
    initial_map %>% setView(lng = map_state$center$lng, lat = map_state$center$lat, zoom = map_state$zoom)
  })
  output$soil_map <- renderLeaflet({
    initial_map %>% setView(lng = map_state$center$lng, lat = map_state$center$lat, zoom = map_state$zoom)
  })
  output$sampling_auto_map <- renderLeaflet({
    initial_map %>%
      setView(lng = map_state$center$lng, lat = map_state$center$lat, zoom = map_state$zoom) %>%
      addDrawToolbar(
        targetGroup = "Sample Points",
        polylineOptions = FALSE,
        polygonOptions = FALSE,
        rectangleOptions = FALSE,
        circleOptions = FALSE,
        circleMarkerOptions = FALSE,
        markerOptions = drawMarkerOptions(repeatMode = TRUE),
        editOptions = editToolbarOptions()
      )
  })
  output$field_compare_map <- renderLeaflet({
    field_compare_refresh_tick()
    isolate(input$sampling_method)
    build_field_compare_leaflet_map()
  })
  outputOptions(output, "imagery_map", suspendWhenHidden = TRUE)
  outputOptions(output, "elevation_map", suspendWhenHidden = TRUE)
  outputOptions(output, "other_map", suspendWhenHidden = TRUE)
  outputOptions(output, "soil_map", suspendWhenHidden = TRUE)
  outputOptions(output, "sampling_auto_map", suspendWhenHidden = TRUE)
  outputOptions(output, "field_compare_map", suspendWhenHidden = TRUE)
  manual_points_plot_df <- reactiveVal(NULL)
  output$manual_table <- DT::renderDataTable({
    df <- manual_points_plot_df()
    if (is.null(df) || nrow(df) < 1L) return(NULL)
    num_cols <- which(vapply(df, is.numeric, logical(1)))
    dt <- DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE), class = "compact stripe hover nowrap")
    if (length(num_cols)) dt <- DT::formatRound(dt, columns = num_cols, digits = 2)
    dt
  })
  outputOptions(output, "manual_table", suspendWhenHidden = FALSE)

  output$sampling_info_ui <- renderUI({
    sp <- sample_points()
    if (is.null(sp) || nrow(sp) < 1L) return(NULL)
    df <- st_drop_geometry(sp)
    zsum <- zonal_cluster_summary()
    if (!is.null(zsum) && nrow(zsum) > 0) {
      tagList(
        h4("Sample Point Data"),
        p(paste("Total points:", nrow(df))),
        h5("Zone distribution"),
        tags$p(em("Counts show how many final sample points were selected in each zone.")),
        tags$ul(lapply(seq_len(nrow(zsum)), function(i) {
          tags$li(paste("Zone", zsum$Zone[i], ":", zsum$Sample_Count[i], "samples"))
        }))
      )
    } else {
      tagList(h4("Sample Point Data"), p(paste("Total points:", nrow(df))))
    }
  })

  output$adaptive_recommendation_ui <- renderUI({
    if (isTRUE(adaptive_recommendation_hidden())) return(NULL)
    fr <- adaptive_recommendation_summary()
    if (is.null(fr)) return(NULL)
    cls_tbl <- fr$class_point_counts
    fld_tbl <- fr$class_field_counts
    na_top <- fr$na_layer_top
    tagList(
      hr(),
      tags$details(
        style = "margin:6px 0 10px 0; border:1px solid #bfd7ff; border-radius:12px; background:linear-gradient(120deg,#f5faff 0%,#e6f1ff 100%); padding:8px 10px;",
        tags$summary(strong("Adaptive Sampling Recommendation")),
        tags$div(
          style = "display:flex; justify-content:flex-end; margin:6px 0;",
          actionButton("close_adaptive_recommendation", "Close", class = "btn btn-default btn-sm")
        ),
        tags$ul(
          tags$li(paste0("Representative points (cLHS): ", fr$n_points)),
          if (!is.null(fr$used_layer_count)) tags$li(paste0("Covariates used for this adaptive map: ", fr$used_layer_count, " layer(s).")),
          tags$li("How classes are computed from your cLHS points: the app learns the typical covariate profile of those points, scores every field pixel by similarity to that profile, then labels the closest pixels as Similar (green), middle as Transition (yellow), and farthest as Dissimilar (red)."),
          tags$li("Map meaning: green = Similar, yellow = Transition, red = Dissimilar (relative to your cLHS covariate profile)."),
          if (!is.null(fld_tbl) && nrow(fld_tbl) > 0) tags$li(
            tags$span("How much of the field is in each class: "),
            paste(apply(fld_tbl, 1, function(r) paste0(r[["class_name"]], " = ", r[["field_share_pct"]], "%")), collapse = "; ")
          ),
          if (!is.null(cls_tbl) && nrow(cls_tbl) > 0) tags$li(
            tags$span("Current cLHS points by class: "),
            paste(apply(cls_tbl, 1, function(r) paste0(r[["class_name"]], " = ", r[["point_count"]], " (", r[["point_share_pct"]], "%)")), collapse = "; ")
          ),
          if (!is.null(fr$inside_unclassified_pct) && !is.na(fr$inside_unclassified_pct)) {
            tags$li(paste0("Gray cells inside your boundary: ", fr$inside_unclassified_pct, "%. The app fills most internal gaps using nearby class majority. Any remaining gray cells are typically places with too little nearby valid data after masking/resampling."))
          } else if (!is.null(fr$unclassified_pct)) {
            tags$li(paste0("Gray/unclassified map area: ", fr$unclassified_pct, "%. Gray means the app does not have complete covariate values for those cells, so they cannot be assigned to Similar/Transition/Dissimilar."))
          },
          if (!is.null(na_top) && nrow(na_top) > 0) tags$li(
            tags$span("Top layers causing missing pixels inside boundary: "),
            paste(apply(na_top, 1, function(r) paste0(r[["layer"]], " (", r[["na_pct_inside"]], "% missing)")), collapse = "; ")
          ),
          tags$li("Why more points may appear in Similar areas: cLHS is designed to represent the full field; if most of the field is Similar, many representative points naturally fall there."),
          tags$li("Dissimilar areas are often small pockets; compare point counts by class below to see whether Dissimilar zones are under-represented.")
        )
      )
    )
  })

  output$zonal_cluster_means_ui <- renderUI({
    zmeans <- zonal_cluster_means()
    if (is.null(zmeans) || nrow(zmeans) < 1L) return(NULL)
    tagList(
      hr(),
      h5("Cluster means by variable"),
      tags$p(em("Mean values are calculated for all valid raster cells in each zone to support interpretation and management decisions.")),
      DT::dataTableOutput("zonal_cluster_means_table")
    )
  })

  output$zonal_cluster_means_table <- DT::renderDataTable({
    zmeans <- zonal_cluster_means()
    req(!is.null(zmeans), nrow(zmeans) > 0)
    num_cols <- which(vapply(zmeans, is.numeric, logical(1)))
    dt <- DT::datatable(zmeans, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE), class = "compact stripe hover")
    if (length(num_cols)) dt <- DT::formatRound(dt, columns = num_cols, digits = 2)
    dt
  })

  output$sampling_table <- DT::renderDataTable({
    sp <- sample_points()
    if (is.null(sp) || nrow(sp) < 1L) return(NULL)
    d <- st_drop_geometry(sp)
    num_cols <- which(vapply(d, is.numeric, logical(1)))
    dt <- DT::datatable(d, options = list(pageLength = 10, scrollX = TRUE), class = "compact stripe hover nowrap")
    if (length(num_cols)) dt <- DT::formatRound(dt, columns = num_cols, digits = 2)
    dt
  })
  outputOptions(output, "sampling_table", suspendWhenHidden = FALSE)

  sampling_density_plot_r <- reactive({
    sp <- sample_points()
    req(!is.null(sp), nrow(sp) > 0)
    build_sampling_density_facets_plot(
      st_drop_geometry(sp),
      fill_color = "#8ecae6",
      prefer_names = active_covariate_layer_names_r()
    )
  })

  output$sampling_density_plots <- renderPlot({
    p <- sampling_density_plot_r()
    req(!is.null(p))
    print(p)
  }, bg = "white", height = 420)

  output$download_sampling_density_plot <- downloadHandler(
    filename = function() paste0("automatic_sample_distributions_", format(Sys.time(), "%Y%m%d_%H%M"), ".png"),
    content = function(file) {
      p <- sampling_density_plot_r()
      if (is.null(p)) stop("Generate automatic sample points first.", call. = FALSE)
      ggplot2::ggsave(file, plot = p, width = 10, height = 7, dpi = 150, bg = "white")
    }
  )

  output$manual_info_ui <- renderUI({
    df <- manual_points_plot_df()
    if (is.null(df) || nrow(df) < 1L) return(NULL)
    h4(paste(nrow(df), "manual points with extracted data."))
  })

  manual_density_plot_r <- reactive({
    df <- manual_points_plot_df()
    req(!is.null(df), nrow(df) > 0)
    build_sampling_density_facets_plot(
      df,
      fill_color = "#95d5b2",
      prefer_names = active_covariate_layer_names_r()
    )
  })

  output$manual_density_plots <- renderPlot({
    p <- manual_density_plot_r()
    req(!is.null(p))
    print(p)
  }, bg = "white", height = 420)

  output$download_manual_density_plot <- downloadHandler(
    filename = function() paste0("manual_sample_distributions_", format(Sys.time(), "%Y%m%d_%H%M"), ".png"),
    content = function(file) {
      p <- manual_density_plot_r()
      if (is.null(p)) stop("Add manual sample points first.", call. = FALSE)
      ggplot2::ggsave(file, plot = p, width = 10, height = 7, dpi = 150, bg = "white")
    }
  )
  
  # Customized editor for manual sampling: only marker
  sampling_manual_map_obj <- callModule(module = editMod, id = "sampling_manual_map", leafmap = initial_map, editor = "leaflet",
                                        editorOptions = list(
                                          polylineOptions = FALSE,
                                          polygonOptions = FALSE,
                                          rectangleOptions = FALSE,
                                          circleOptions = FALSE,
                                          markerOptions = drawMarkerOptions(repeatMode = TRUE),
                                          circleMarkerOptions = FALSE,
                                          editOptions = editToolbarOptions()
                                        ))
  
  observeEvent(sampling_manual_map_obj()$finished, {
    req(sampling_manual_map_obj()$finished)
    tryCatch({
      mp <- st_transform(sampling_manual_map_obj()$finished, st_crs(TARGET_CRS))
      mp$ID <- seq_len(nrow(mp))
      manual_points(mp)
      showNotification(paste(nrow(mp), "manual points saved."), type = "message")
    }, error = function(e) {
      showNotification(paste("Error in manual sampling:", e$message), type = "error")
    })
  })

  output$manual_marker_notice_ui <- renderUI({
    pts <- manual_points()
    if (is.null(pts) || nrow(pts) == 0) return(NULL)
    tags$div(
      style = "margin-top:8px; padding:8px 10px; border:1px solid #f2c97a; border-radius:10px; background:#fff8e9;",
      tags$strong("Important: "),
      "Manually added markers must be manually removed using the trash tool on the map."
    )
  })
  
  # --- Sampling Method Toggle ---
  winner_method_to_sample_type <- function(w) {
    switch(
      as.character(w),
      "Simple Random" = "Simple Random",
      "Systematic Spread" = "Systematic spread (coverage)",
      "Spread + cLHS" = "Spread + cLHS (best coverage)",
      "cLHS" = "Conditioned Latin Hypercube (cLHS)",
      "Zone-based" = "Zone-based",
      "Hybrid Zonal cLHS" = "Hybrid Zonal cLHS",
      NULL
    )
  }

  sample_type_to_compare_method <- function(st) {
    switch(
      as.character(st)[1L],
      "Simple Random" = "Simple Random",
      "Systematic spread (coverage)" = "Systematic Spread",
      "Spread + cLHS (best coverage)" = "Spread + cLHS",
      "Conditioned Latin Hypercube (cLHS)" = "cLHS",
      "Zone-based" = "Zone-based",
      "Hybrid Zonal cLHS" = "Hybrid Zonal cLHS",
      NULL
    )
  }

  spread_pick_progress_message <- function(
    compare_method,
    n_reps = RECOMMENDED_GENERATION_REPEATS(),
    target_pct = GENERATION_FIELD_COVERAGE_TARGET_PCT()
  ) {
    paste0(
      "Generating ", compare_method, " (",
      n_reps, " replicates, ≥", target_pct, "% field coverage target)"
    )
  }

  # Method + n match comparison → same zone k-means seed family as technique comparison.
  generation_matches_comparison_snapshot <- function(cmp = NULL) {
    if (is.null(cmp)) cmp <- isolate(comparison_results())
    if (is.null(cmp) || is.null(cmp$winner) || is.null(cmp$n_points)) return(FALSE)
    cm <- sample_type_to_compare_method(isolate(input$sample_type))
    if (is.null(cm) || !identical(as.character(cm), as.character(cmp$winner))) return(FALSE)
    n_pts <- safe_sample_n_points(
      suppressWarnings(as.integer(isolate(input$n_points))),
      max_n = 50000L
    )
    identical(as.integer(n_pts), as.integer(cmp$n_points))
  }

  launch_automatic_spread_pick <- function(from_recommended = FALSE) {
    if (!identical(isolate(input$sampling_method), "automatic")) {
      showNotification(
        "Switch to 'I want to sample automatically', then generate again.",
        type = "warning",
        duration = 8
      )
      return(invisible(NULL))
    }
    if (length(sampling_selected_rasters()) == 0L) {
      showNotification("No raster layers available for sampling.", type = "warning")
      return(invisible(NULL))
    }

    if (isTRUE(from_recommended)) {
      cmp <- comparison_results()
      if (is.null(cmp) || is.null(cmp$winner) || is.null(cmp$n_points)) {
        showNotification("Run Technique comparison first.", type = "warning")
        return(invisible(NULL))
      }
      compare_method <- as.character(cmp$winner)[1L]
      n_target <- as.integer(cmp$n_points)
      base_seed <- suppressWarnings(as.integer(cmp$compare_base_seed))
      if (is.na(base_seed)) {
        base_seed <- suppressWarnings(as.integer(isolate(input$random_seed)))
      }
      if (is.na(base_seed)) base_seed <- 123L
      buf <- suppressWarnings(as.numeric(isolate(recommended_buffer_distance())))
      if (!is.finite(buf)) {
        buf <- suppressWarnings(as.numeric(isolate(input$buffer_distance)))
      }
      run_automatic_samples_spread_pick(
        compare_method = compare_method,
        n_target = n_target,
        base_seed = base_seed,
        buffer_m = buf,
        update_comparison_spread = TRUE,
        comparison_snapshot = cmp,
        n_reps = RECOMMENDED_GENERATION_REPEATS(),
        target_field_coverage_pct = GENERATION_FIELD_COVERAGE_TARGET_PCT(),
        polish_aggressive = TRUE,
        progress_message = spread_pick_progress_message(
          compare_method,
          n_reps = RECOMMENDED_GENERATION_REPEATS()
        )
      )
    } else {
      compare_method <- sample_type_to_compare_method(isolate(input$sample_type))
      if (is.null(compare_method)) {
        return(invisible("grid_based"))
      }
      n_target <- safe_sample_n_points(
        suppressWarnings(as.integer(isolate(input$n_points))),
        max_n = 50000L
      )
      cmp <- isolate(comparison_results())
      aligned <- generation_matches_comparison_snapshot(cmp)
      base_seed <- suppressWarnings(as.integer(isolate(input$random_seed)))
      if (is.na(base_seed)) base_seed <- 123L
      buf <- suppressWarnings(as.numeric(isolate(input$buffer_distance)))
      n_reps_cur <- RECOMMENDED_GENERATION_REPEATS() + GENERATION_CURRENT_SETTINGS_EXTRA_REPEATS()
      run_automatic_samples_spread_pick(
        compare_method = compare_method,
        n_target = n_target,
        base_seed = base_seed,
        buffer_m = buf,
        update_comparison_spread = FALSE,
        comparison_snapshot = if (aligned) cmp else NULL,
        n_reps = n_reps_cur,
        target_field_coverage_pct = GENERATION_FIELD_COVERAGE_TARGET_PCT(),
        polish_aggressive = TRUE,
        from_current_settings = TRUE,
        progress_message = spread_pick_progress_message(
          compare_method,
          n_reps = n_reps_cur
        )
      )
    }
  }

  store_zone_wss_recommendation <- function(res) {
    if (!isTRUE(res$ok)) return(invisible(FALSE))
    zone_wss_cache(list(
      fingerprint = res$fingerprint,
      recommended = as.integer(res$recommended),
      k = res$k_seq,
      wss = res$wss,
      k_max = res$k_max,
      n_all = res$n_all
    ))
    invisible(TRUE)
  }

  get_recommended_zone_k <- function() {
    kz <- read_zone_k_scalar(isolate(committed_wss_zone_k()))
    if (!is.na(kz) && kz >= 2L) return(kz)
    kz <- read_zone_k_scalar(isolate(recommended_n_zones()))
    if (!is.na(kz) && kz >= 2L) return(kz)
    cached <- isolate(zone_wss_cache())
    if (!is.null(cached) && !is.null(cached$recommended)) {
      kz <- read_zone_k_scalar(cached$recommended)
      if (!is.na(kz) && kz >= 2L) return(kz)
    }
    NA_integer_
  }

  restore_recommended_zone_k_if_missing <- function() {
    kz <- get_recommended_zone_k()
    if (is.na(kz) || kz < 2L) return(NA_integer_)
    apply_wss_zone_inputs(kz)
    kz
  }

  zone_wss_context_changed_since_lock <- function() {
    if (!isTRUE(isolate(zones_wss_locked()))) return(FALSE)
    locked_layers_fp <- zone_layers_fingerprint(isolate(wss_zone_layers_at_lock()))
    cur_layers_fp <- zone_layers_fingerprint(isolate(input$sampling_covariate_layers))
    layers_changed <- FALSE
    if (nzchar(locked_layers_fp) || nzchar(cur_layers_fp)) {
      if (!nzchar(cur_layers_fp) && nzchar(locked_layers_fp)) {
        layers_changed <- FALSE
      } else {
        layers_changed <- !identical(locked_layers_fp, cur_layers_fp)
      }
    }
    locked_b <- isolate(wss_zone_boundary_fp_at_lock())
    cur_b <- zone_boundary_fingerprint(isolate(digitized_features()))
    boundary_changed <- !identical(locked_b, cur_b)
    isTRUE(layers_changed) || isTRUE(boundary_changed)
  }

  active_wss_zone_message <- function() {
    kz <- get_recommended_zone_k()
    if (!is.na(kz) && kz >= 2L) {
      return(paste0(
        "WSS zone count: ", kz,
        " — used for technique comparison, Generate sample points (zone-based / hybrid), and Apply Zones to Map."
      ))
    }
    zone_recommend_message()
  }

  current_zone_wss_context_fingerprint <- function() {
    paste(
      zone_layers_fingerprint(isolate(input$sampling_covariate_layers)),
      zone_boundary_fingerprint(isolate(digitized_features())),
      sep = "::"
    )
  }

  sync_zone_recommend_button_lock <- function() {
    if (!isTRUE(isolate(zones_wss_locked()))) return(invisible(NULL))
    kz <- get_recommended_zone_k()
    if (!is.finite(kz) || is.na(kz) || kz < 2L) return(invisible(NULL))
    lock_action_button("recommend_zones")
    if (requireNamespace("later", quietly = TRUE)) {
      later::later(function() {
        shiny::withReactiveDomain(session, {
          shiny::isolate({
            if (isTRUE(zones_wss_locked()) && !is.na(get_recommended_zone_k())) {
              lock_action_button("recommend_zones")
            }
          })
        })
      }, delay = 0.35)
    }
    invisible(NULL)
  }

  invalidate_zone_recommendation <- function() {
    zones_wss_locked(FALSE)
    zone_wss_context_fingerprint(NULL)
    committed_wss_zone_k(NA_integer_)
    wss_zone_layers_at_lock(NULL)
    wss_zone_boundary_fp_at_lock(NULL)
    recommended_n_zones(NULL)
    zone_wss_cache(NULL)
    compare_zone_recommend_msg("Click 'Recommend Zones' once to set zone count from covariates (WSS elbow).")
    zone_recommend_message("Click 'Recommend Zones' on the Technique comparison tab (one time per covariate set).")
    unlock_action_button("recommend_zones")
    invisible(TRUE)
  }

  invalidate_zone_recommendation_if_context_changed <- function() {
    if (!isTRUE(isolate(zones_wss_locked()))) {
      return(invisible(FALSE))
    }
    if (!zone_wss_context_changed_since_lock()) {
      kz <- restore_recommended_zone_k_if_missing()
      if (!is.na(kz) && kz >= 2L) {
        zone_wss_context_fingerprint(current_zone_wss_context_fingerprint())
        sync_zone_recommend_button_lock()
      }
      return(invisible(FALSE))
    }
    invalidate_zone_recommendation()
    invisible(TRUE)
  }

  run_zone_wss_recommendation <- function() {
    all_r <- sampling_selected_rasters()
    if (length(all_r) == 0) {
      showNotification("No raster layers available. Load covariates first.", type = "warning")
      return(invisible(NULL))
    }
    sp <- deployment_safety_params()
    cov_df <- build_cov_df_for_zone_wss(
      all_r,
      digitized_features(),
      analysis_crs_string(),
      sp$harmonize_scale
    )
    if (is.null(cov_df) || ncol(cov_df) < 1L || nrow(cov_df) < 10L) {
      showNotification("Not enough valid covariate data for WSS-based recommendation.", type = "warning")
      return(invisible(NULL))
    }
    res <- wss_recommend_zone_count_from_cov_df(cov_df)
    if (!isTRUE(res$ok)) {
      showNotification(res$reason, type = "warning")
      return(invisible(NULL))
    }
    store_zone_wss_recommendation(res)
    rec_k <- read_zone_k_scalar(res$recommended)
    committed_wss_zone_k(rec_k)
    lay <- isolate(input$sampling_covariate_layers)
    if (is.null(lay) || !length(lay)) {
      lay <- names(all_r)
    }
    wss_zone_layers_at_lock(lay)
    wss_zone_boundary_fp_at_lock(zone_boundary_fingerprint(isolate(digitized_features())))
    apply_wss_zone_inputs(rec_k)
    zones_wss_locked(TRUE)
    zone_wss_context_fingerprint(current_zone_wss_context_fingerprint())
    msg <- zone_wss_recommendation_message(res)
    zone_recommend_message(msg)
    compare_zone_recommend_msg(msg)
    sync_zone_recommend_button_lock()
    showNotification(
      paste0("Zone count set to ", res$recommended, " (WSS). This value is used for comparison, zone-based sampling, and hybrid zonal cLHS."),
      type = "message",
      duration = 8
    )
    invisible(res)
  }

  apply_wss_zone_inputs <- function(rec) {
    rec <- read_zone_k_scalar(rec)
    if (is.na(rec) || rec < 2L) return(invisible(FALSE))
    recommended_n_zones(rec)
    msg <- active_wss_zone_message()
    zone_recommend_message(msg)
    compare_zone_recommend_msg(msg)
    invisible(TRUE)
  }

  resolve_zone_k_for_generation <- function() {
    kz <- get_recommended_zone_k()
    if (is.na(kz) || kz < 2L) {
      showNotification(
        "For Zone-based and Hybrid Zonal cLHS: click Recommend Zones once on Technique comparison before comparing or generating; same k everywhere.",
        type = "warning",
        duration = 8
      )
      return(NA_integer_)
    }
    apply_wss_zone_inputs(kz)
    kz
  }

  sync_sampling_inputs_from_recommendation_snapshot <- function() {
    cmp <- comparison_results()
    rt <- recommended_sample_type()
    if (is.null(rt) && !is.null(cmp)) rt <- winner_method_to_sample_type(cmp$winner)
    rn <- recommended_n_points()
    if (is.null(rn) && !is.null(cmp)) rn <- as.integer(cmp$n_points)
    rb <- recommended_buffer_distance()
    rs <- recommended_random_seed()
    rg <- recommended_grid_size_m()

    if (!is.null(rt) && nzchar(rt)) updateSelectInput(session, "sample_type", selected = rt)
    if (!is.null(rn)) updateNumericInput(session, "n_points", value = as.integer(rn))
    if (!is.null(rb)) updateNumericInput(session, "buffer_distance", value = as.numeric(rb))
    if (!is.null(rg) && !is.na(rg) && identical(rt, "Grid-based")) {
      updateNumericInput(session, "grid_size_m", value = as.integer(rg))
    }
    if (!is.null(rs) && length(rs) == 1L && !is.na(rs)) {
      updateNumericInput(session, "random_seed", value = as.integer(rs))
    }
    restore_recommended_zone_k_if_missing()
    sampling_prefill_active(TRUE)
    invisible(TRUE)
  }

  observeEvent(input$generate_samples_recommended, {
    cmp <- comparison_results()
    if (is.null(cmp) || is.null(cmp$n_points)) {
      showNotification("Run Technique comparison on that sub-tab first.", type = "warning")
      return(invisible(NULL))
    }
    if (!identical(input$sampling_method, "automatic")) {
      updateSelectInput(session, "sampling_method", selected = "automatic")
      shinyjs::show("auto_sampling_div")
      shinyjs::hide("manual_sampling_div")
    }
    sp <- deployment_safety_params()
    if (is.finite(sp$recommended_buffer_m)) {
      recommended_buffer_distance(sp$recommended_buffer_m)
      updateNumericInput(session, "buffer_distance", value = as.numeric(sp$recommended_buffer_m))
    }
    sync_sampling_inputs_from_recommendation_snapshot()
    launch_automatic_spread_pick(from_recommended = TRUE)
  })

  observeEvent(input$sampling_method, {
    if (input$sampling_method == "manual") {
      shinyjs::show("manual_sampling_div")
      shinyjs::hide("auto_sampling_div")
    } else if (input$sampling_method == "automatic") {
      shinyjs::show("auto_sampling_div")
      shinyjs::hide("manual_sampling_div")
      
      # If comparison exists, prefill automatic sampling with recommended settings.
      cmp <- comparison_results()
      if (!is.null(cmp) && !is.null(cmp$winner) && !is.null(cmp$n_points)) {
        rec_type <- winner_method_to_sample_type(cmp$winner)
        if (is.null(rec_type)) rec_type <- input$sample_type
        if (!is.null(rec_type) && nzchar(rec_type)) {
          updateSelectInput(session, "sample_type", selected = rec_type)
        }
        updateNumericInput(session, "n_points", value = as.integer(cmp$n_points))
        recommended_sample_type(rec_type)
        recommended_n_points(as.integer(cmp$n_points))
        restore_recommended_zone_k_if_missing()
        sp <- deployment_safety_params()
        rec_buf <- if (is.finite(sp$recommended_buffer_m)) sp$recommended_buffer_m else suppressWarnings(as.numeric(isolate(input$buffer_distance)))
        recommended_buffer_distance(rec_buf)
        updateNumericInput(session, "buffer_distance", value = as.numeric(rec_buf))
        recommended_random_seed(suppressWarnings(as.integer(isolate(input$random_seed))))
        recommended_grid_size_m(suppressWarnings(as.integer(isolate(input$grid_size_m))))
        sampling_prefill_active(TRUE)
        showNotification(
          paste0("Automatic sampling prefilled from comparison: ", cmp$winner, " with sample size ", cmp$n_points, ". You can still change settings."),
          type = "message",
          duration = 6
        )
      }
    }
    set_layer_tip("sampling")
  })
  
  observeEvent(
    list(
      input$sample_type,
      input$n_points,
      input$sampling_method,
      input$buffer_distance,
      input$random_seed,
      input$grid_size_m
    ),
    {
    if (!identical(input$sampling_method, "automatic")) {
      sampling_prefill_active(FALSE)
      return(invisible(NULL))
    }
    rec_type <- recommended_sample_type()
    rec_n <- recommended_n_points()
    rec_nz <- read_zone_k_scalar(isolate(recommended_n_zones()))
    rec_buf <- recommended_buffer_distance()
    rec_seed <- recommended_random_seed()
    rec_grid <- recommended_grid_size_m()
    if (is.null(rec_type) || is.null(rec_n) || is.null(input$sample_type) || is.null(input$n_points)) {
      sampling_prefill_active(FALSE)
      return(invisible(NULL))
    }
    if (is.null(rec_buf) || is.null(rec_seed)) {
      sampling_prefill_active(FALSE)
      return(invisible(NULL))
    }
    same_type <- identical(as.character(input$sample_type), as.character(rec_type))
    same_n <- suppressWarnings(as.integer(input$n_points) == as.integer(rec_n))
    needs_zones <- input$sample_type %in% c("Zone-based", "Hybrid Zonal cLHS")
    same_zones <- if (!needs_zones) {
      TRUE
    } else {
      identical(rec_nz, get_recommended_zone_k())
    }
    same_buf <- suppressWarnings(as.numeric(input$buffer_distance) == as.numeric(rec_buf))
    same_seed <- suppressWarnings(as.integer(input$random_seed) == as.integer(rec_seed))
    needs_grid <- identical(input$sample_type, "Grid-based")
    same_grid <- if (!needs_grid) {
      TRUE
    } else if (is.null(rec_grid) || length(rec_grid) == 0L || is.na(rec_grid)) {
      FALSE
    } else {
      suppressWarnings(as.integer(input$grid_size_m) == as.integer(rec_grid))
    }
    sampling_prefill_active(
      isTRUE(same_type) &&
        isTRUE(same_n) &&
        isTRUE(same_zones) &&
        isTRUE(same_buf) &&
        isTRUE(same_seed) &&
        isTRUE(same_grid)
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$recommend_zones, {
    if (isTRUE(isolate(zones_wss_locked())) && !is.na(get_recommended_zone_k())) {
      sync_zone_recommend_button_lock()
      showNotification(
        paste0(
          "Zone count already set (WSS k = ", get_recommended_zone_k(),
          "). Change covariate layers or boundary to recommend again."
        ),
        type = "message",
        duration = 6
      )
      return(invisible(NULL))
    }
    withProgress(message = "Recommending zones (WSS elbow)...", value = 0.1, {
      tryCatch({
        setProgress(0.2, detail = "Preparing covariates...")
        setProgress(0.65, detail = "Computing WSS curve...")
        run_zone_wss_recommendation()
        setProgress(1, detail = "Recommendation complete.")
      }, error = function(e) {
        zone_recommend_message("WSS recommendation failed. Check covariates/boundary and try again.")
        compare_zone_recommend_msg("WSS recommendation failed. Check covariates/boundary and try again.")
        showNotification(paste("Could not compute WSS recommendation:", e$message), type = "error")
      })
    })
  }, ignoreInit = TRUE)

  output$sampling_zone_controls_ui <- renderUI({
    kz <- get_recommended_zone_k()
    locked <- isTRUE(isolate(zones_wss_locked()))
    if (!is.na(kz) && kz >= 2L) {
      tagList(
        tags$p(
          style = "margin:0 0 8px 0; font-size:13px;",
          strong(paste0("WSS zone count (k): ", kz)),
          if (locked) {
            tags$span(class = "text-muted", " — locked from Recommend Zones; same k for comparison, sampling, and generation.")
          } else {
            tags$span(class = "text-muted", " — from WSS cache; click Recommend Zones on Technique comparison to lock.")
          }
        ),
        actionButton("apply_zones", "Apply Zones to Map", class = "btn-primary btn-sm")
      )
    } else {
      tagList(
        tags$p(
          class = "text-muted",
          style = "font-size:12px; margin:0 0 8px 0;",
          "On ",
          strong("Technique comparison"),
          ", click ",
          strong("Recommend Zones"),
          " once before comparing or generating points. Same k for comparison, sampling, and generation (Zone-based and Hybrid Zonal cLHS)."
        ),
        actionButton("apply_zones", "Apply Zones to Map", class = "btn-primary btn-sm", disabled = TRUE)
      )
    }
  })
  
  output$zone_recommend_info <- renderText({
    active_wss_zone_message()
  })

  output$zones_on_map_available <- reactive({
    !is.null(zonal_zone_raster())
  })
  outputOptions(output, "zones_on_map_available", suspendWhenHidden = FALSE)

  observeEvent(input$show_sampling_zones_on_map, {
    render_sampling_zones_on_map()
  }, ignoreNULL = FALSE)
  
  run_apply_zones_to_map <- function() {
    all_r <- sampling_selected_rasters()
    if (length(all_r) == 0L) {
      showNotification("No raster layers available. Load covariates first.", type = "warning")
      return(invisible(FALSE))
    }
    kz <- get_recommended_zone_k()
    if (is.na(kz) || kz < 2L) {
      showNotification("Click Recommend Zones on the Technique comparison tab first.", type = "warning", duration = 8)
      return(invisible(FALSE))
    }
    withProgress(message = "Applying zones to map...", value = 0.1, {
      on.exit({
        tryCatch(release_geosampler_memory(), error = function(e) invisible(NULL))
      }, add = TRUE)
      tryCatch({
        setProgress(0.25, detail = "Preparing covariates...")
        boundary <- digitized_features()
        sp <- deployment_safety_params()
        harmonized <- harmonize_covariate_layers(
          all_r,
          boundary_sf = boundary,
          analysis_crs = analysis_crs_string(),
          harmonize_scale = sp$harmonize_scale
        )
        if (length(harmonized) == 0L) stop("No valid harmonized rasters for zoning.", call. = FALSE)
        combined_stack <- stack(harmonized)
        setProgress(0.55, detail = "Running k-means zoning...")
        zdf <- as.data.frame(combined_stack, xy = TRUE, na.rm = TRUE)
        if (nrow(zdf) < 5L) stop("Not enough valid cells to create zones.", call. = FALSE)
        zcov <- zdf[, -c(1, 2), drop = FALSE]
        zcov <- zcov[, vapply(zcov, is.numeric, logical(1)), drop = FALSE]
        zcov <- zcov[, vapply(zcov, function(v) stats::sd(v, na.rm = TRUE) > 0, logical(1)), drop = FALSE]
        if (ncol(zcov) < 1L) stop("No numeric covariates available for zoning.", call. = FALSE)
        kz_use <- min(as.integer(kz), nrow(zdf))
        if (kz_use < 2L) stop("Zone count must be at least 2.", call. = FALSE)
        km <- stats::kmeans(scale(zcov), centers = kz_use, iter.max = 100, nstart = 5)
        zdf$zone <- as.integer(km$cluster)
        setProgress(0.8, detail = "Rendering zones...")
        zb <- build_zone_raster_from_cells(
          combined_stack[[1]],
          zdf,
          zone_col = "zone",
          boundary_sf = boundary
        )
        zone_r <- zb$raster
        zone_levels <- zb$levels
        if (is.null(zone_r) || !length(zone_levels)) {
          stop("Zone raster is empty. Check boundary and selected covariates.", call. = FALSE)
        }
        zonal_zone_raster(zone_r)
        zonal_zone_count(as.integer(length(zone_levels)))
        render_sampling_zones_on_map(zone_r, show_zones = TRUE)
        rm(combined_stack, zdf, zcov, km, zone_r)
        setProgress(1, detail = "Zones applied.")
        showNotification(
          paste0("Applied ", kz_use, " zones to map. Use Show zone overlay on map below the sampling map if the layer is hidden."),
          type = "message",
          duration = 7
        )
        TRUE
      }, error = function(e) {
        handle_app_error(e, context = "Apply zones to map", notify_user = TRUE, duration = 12)
        FALSE
      })
    })
  }

  observeEvent(input$apply_zones, {
    run_apply_zones_to_map()
  }, ignoreInit = TRUE)

  observeEvent(input$sample_type, {
    if (!identical(isolate(input$sampling_method), "automatic")) return(invisible(NULL))
    st <- as.character(isolate(input$sample_type))
    if (!st %in% c("Zone-based", "Hybrid Zonal cLHS")) return(invisible(NULL))
    if (!is.null(zonal_zone_raster())) return(invisible(NULL))
    showNotification(
      paste0(
        "For ", st, ": click Apply Zones to Map under Explore more ",
        "(after Recommend Zones) to preview zones before generating points."
      ),
      type = "message",
      duration = 9
    )
  }, ignoreInit = TRUE)
  
  observeEvent(digitized_features(), {
    df <- digitized_features()
    if (is.null(df)) {
      output$boundary_area <- renderText("")
      apply_boundary_overlay_to_maps(NULL)
    } else {
    refresh_boundary_on_maps(delay_fit = FALSE)
    shinyjs::delay(400, refresh_boundary_on_maps(delay_fit = TRUE))
    m <- compute_boundary_risk_metrics(df)
    if (is.null(m)) {
      output$boundary_area <- renderText("")
    } else if (m$area_km2 > m$safe_km2) {
      output$boundary_area <- renderText(
        paste0(
          "Boundary Area: ", round(m$area_ha, 2), " ha (", round(m$area_acre, 2), " acres, ",
          round(m$area_km2, 2), " km²) — HIGH crash risk on 1 GB hosting. Practical guide: keep AOI around ",
          round(m$caution_km2, 1), " km² (", round(m$caution_km2 * 100, 0), " ha, ",
          round(m$caution_km2 * 100 * 2.47105, 0), " acres) or less; around ",
          round(m$safe_km2, 1), " km² (", round(m$safe_km2 * 100, 0), " ha, ",
          round(m$safe_km2 * 100 * 2.47105, 0), " acres) and above often becomes unstable."
        )
      )
      showNotification(
        paste0(
          "Warning: AOI exceeds conservative 1 GB safe threshold (~", round(m$safe_km2, 1),
          " km²). Reduce AOI, cloud range, or median scenes."
        ),
        type = "warning", duration = 10
      )
    } else if (m$area_km2 > m$caution_km2) {
      output$boundary_area <- renderText(
        paste0(
          "Boundary Area: ", round(m$area_ha, 2), " ha (", round(m$area_acre, 2), " acres, ",
          round(m$area_km2, 2), " km²) — caution for 1 GB hosting. Recommended practical size is up to ~",
          round(m$caution_km2, 1), " km² (", round(m$caution_km2 * 100, 0), " ha, ",
          round(m$caution_km2 * 100 * 2.47105, 0), " acres); close to ",
          round(m$safe_km2, 1), " km² can become unstable."
        )
      )
      showNotification(
        paste0(
          "Caution: AOI is above ~", round(m$caution_km2, 1),
          " km². Processing can be slow/unstable on 1 GB hosting."
        ),
        type = "warning", duration = 6
      )
    } else {
      output$boundary_area <- renderText(
        paste0(
          "Boundary Area: ", round(m$area_ha, 2), " ha (", round(m$area_acre, 2), " acres, ",
          round(m$area_km2, 2), " km²). This is within a practical range for 1 GB hosting. Guideline: keep boundaries around ",
          round(m$caution_km2, 1), " km² (", round(m$caution_km2 * 100, 0), " ha, ",
          round(m$caution_km2 * 100 * 2.47105, 0), " acres) or less."
        )
      )
    }
    }
  }, ignoreNULL = FALSE)
  
  map_refit_scheduled <- reactiveVal(FALSE)

  redraw_imagery_analysis_layers <- function() {
    if (!identical(isolate(input$main_tabs), "Variables")) return(invisible(NULL))
    if (!identical(isolate(input$variables_subtabs), "imagery")) return(invisible(NULL))
    session$sendCustomMessage("invalidateLeafletMaps", list(mapIds = c("imagery_map")))
    if (length(isolate(sentinel_vi_rasters())) > 0L || !is.null(isolate(uploaded_sentinel_raster()))) {
      tryCatch(redraw_sentinel_raster(), error = function(e) NULL)
    }
    if (length(isolate(vi_rasters())) > 0L || !is.null(isolate(uploaded_planet_raster()))) {
      tryCatch(redraw_planet_raster(), error = function(e) NULL)
    }
    if (length(isolate(ms_vi_rasters())) > 0L || !is.null(isolate(uploaded_ms_raster()))) {
      tryCatch(redraw_ms_raster(), error = function(e) NULL)
    }
    invisible(NULL)
  }

  # Re-center maps and redraw boundary when users switch tabs/subtabs (debounced).
  refit_maps_on_tab <- function() {
    if (isTRUE(isolate(map_refit_scheduled()))) {
      invisible(NULL)
    } else {
    map_refit_scheduled(TRUE)
    shinyjs::delay(200, {
      map_refit_scheduled(FALSE)
      visible_mid <- map_ids_for_current_tab()
      if (length(visible_mid)) {
        invalidate_leaflet_maps_client(visible_mid, delays_ms = 280L)
      }
      if (!is.null(digitized_features())) {
        main <- isolate(input$main_tabs)
        if (identical(main, "Variables")) {
          refresh_boundary_on_maps(delay_fit = TRUE)
          sub <- isolate(input$variables_subtabs)
          if (identical(sub, "elevation")) {
            tryCatch(redraw_elevation_layers(), error = function(e) NULL)
          } else if (identical(sub, "imagery")) {
            redraw_imagery_analysis_layers()
          }
        } else if (identical(main, "Boundary") || identical(main, "Sampling") || identical(main, "FieldCompare")) {
          refresh_boundary_on_maps(delay_fit = TRUE)
        }
      }
    })
    invisible(NULL)
    }
  }

  observeEvent(input$map_tab_shown, {
    refit_maps_on_tab()
  }, ignoreInit = TRUE)

  observeEvent(
    list(input$main_tabs, input$variables_subtabs, input$sampling_subtabs),
    {
      refit_maps_on_tab()
    },
    ignoreInit = TRUE
  )

  observeEvent(input$imagery_view_subtabs, {
    if (identical(input$imagery_view_subtabs, "imagery_map_tab")) {
      shinyjs::delay(200, refit_maps_on_tab())
    }
  }, ignoreInit = TRUE)
  
  observeEvent(
    list(input$add_boundary_planet, input$add_boundary_sentinel, input$add_boundary_elevation),
    {
    req(!is.null(digitized_features()))
      refresh_boundary_on_maps(delay_fit = TRUE)
    },
    ignoreInit = TRUE
  )
  
  observeEvent(input$stop_planet, { abort_planet(TRUE); showNotification("Planet download aborted.", type="warning") })
  observeEvent(input$stop_sentinel, { abort_sentinel(TRUE); showNotification("Sentinel download aborted.", type="warning") })
  observeEvent(input$stop_elevation, { abort_elevation(TRUE); showNotification("Elevation download aborted.", type="warning") })
  observeEvent(input$stop_compare_sampling, {
    abort_compare_sampling(TRUE)
    if (isTRUE(is_comparing_sampling())) {
      showNotification("Sampling comparison stop requested. Finishing current iteration and stopping...", type = "warning", duration = 6)
    }
  })
  
  # --- Planet Status UI ---
  output$planet_status_ui <- renderUI({
    if (is_downloading_planet()) {
      pct <- max(0, min(100, as.integer(planet_progress_pct())))
      det <- planet_progress_detail()
      div(
        class="loading-ui",
        icon("spinner", class="fa-spin"),
        planet_status(),
        if (!is.null(det) && length(det) && any(nzchar(as.character(det)))) tags$div(style = "font-size:0.92rem; margin-top:6px;", det),
        tags$div(
          class = "progress",
          style = "height: 18px; margin-top: 10px;",
          tags$div(
            class = "progress-bar progress-bar-striped active",
            role = "progressbar",
            `aria-valuenow` = pct, `aria-valuemin` = 0, `aria-valuemax` = 100,
            style = paste0("width:", pct, "%;")
          )
        )
      )
    } else {
      NULL
    }
  })
  
  # --- Sentinel Status UI ---
  output$sentinel_status_ui <- renderUI({
    sentinel_status_flush()
    status_txt <- sentinel_status()
    detail_txt <- sentinel_progress_detail()
    is_busy <- isTRUE(is_downloading_sentinel())
    if (!is_busy) return(NULL)
    if (is_busy) {
      started_at <- sentinel_active_since()
      elapsed_secs <- if (!is.null(started_at)) as.integer(difftime(Sys.time(), started_at, units = "secs")) else 0L
      pct <- max(0, min(100, as.integer(sentinel_progress_pct())))
      return(tags$div(
      class = "sentinel-live-status",
        tags$div(class = "sentinel-live-status-title", tagList(icon("spinner", class = "fa-spin"), " Sentinel Status")),
        if (!is.null(status_txt) && length(status_txt) && any(nzchar(as.character(status_txt)))) tags$div(class = "sentinel-live-status-text", status_txt),
        if (!is.null(detail_txt) && length(detail_txt) && any(nzchar(as.character(detail_txt)))) tags$div(class = "sentinel-live-status-text", paste0("• ", detail_txt)),
        tags$div(
          class = "progress",
          style = "height: 18px; margin-top: 10px;",
          tags$div(
            class = "progress-bar progress-bar-striped progress-bar-animated",
            role = "progressbar",
            `aria-valuenow` = pct, `aria-valuemin` = 0, `aria-valuemax` = 100,
            style = paste0("width:", pct, "%;")
          )
        ),
        tags$div(class = "sentinel-live-status-time", paste("Working for", elapsed_secs, "sec"))
      ))
    }
    NULL
  })
  outputOptions(output, "sentinel_status_ui", suspendWhenHidden = FALSE)
  
  output$sentinel_console_panel_ui <- renderUI({
    lines <- sentinel_console_lines()
    if (isTRUE(sentinel_console_hidden()) || length(lines) == 0) return(NULL)
    tags$div(
      class = "sentinel-live-status",
      tags$div(
        style = "display:flex; justify-content:space-between; align-items:center;",
        tags$div(class = "sentinel-live-status-title", "Sentinel Live Log"),
        actionButton(
          "dismiss_sentinel_console",
          label = HTML("&times;"),
          class = "btn btn-default btn-sm",
          style = "padding:0 8px; line-height:1.2;"
        )
      ),
      tags$pre(
        style = "white-space: pre-wrap; max-height: 170px; overflow-y: auto; margin: 6px 0 0 0;",
        paste(lines, collapse = "\n")
      )
    )
  })
  outputOptions(output, "sentinel_console_panel_ui", suspendWhenHidden = FALSE)

  observeEvent(input$dismiss_sentinel_console, {
    sentinel_console_hidden(TRUE)
  })

  observeEvent(is_downloading_sentinel(), {
    if (isTRUE(is_downloading_sentinel())) {
      sentinel_active_since(Sys.time())
      sentinel_heartbeat_next(Sys.time() + 6)
    } else {
      sentinel_active_since(NULL)
      sentinel_heartbeat_next(NULL)
    }
  }, ignoreInit = FALSE)

  observe({
    if (isTRUE(is_downloading_sentinel())) {
      invalidateLater(1000, session)
      nxt <- sentinel_heartbeat_next()
      if (!is.null(nxt) && Sys.time() >= nxt) {
        started_at <- sentinel_active_since()
        elapsed_secs <- if (!is.null(started_at)) as.integer(difftime(Sys.time(), started_at, units = "secs")) else NA_integer_
        msg <- if (is.na(elapsed_secs)) {
          "Still processing Sentinel request..."
        } else {
          paste0("Still processing Sentinel request... (", elapsed_secs, " sec elapsed)")
        }
        append_sentinel_console(msg)
        sentinel_progress_detail(msg)
        sentinel_status_flush(sentinel_status_flush() + 1L)
        sentinel_heartbeat_next(Sys.time() + 6)
      }
    }
  })

  # Dashboard status only while long-running jobs are active (avoids idle 2s polling).
  observe({
    busy <- isTRUE(is_downloading_sentinel()) || isTRUE(is_downloading_planet()) || isTRUE(is_comparing_sampling())
    if (!busy) {
      force_dashboard_status(NULL)
    } else {
    invalidateLater(2000, session)
    active_msgs <- character(0)
    if (isTRUE(is_downloading_sentinel())) {
      active_msgs <- c(active_msgs, paste0("Sentinel running: ", sentinel_progress_detail()))
    }
    if (isTRUE(is_downloading_planet())) {
      active_msgs <- c(active_msgs, paste0("Planet running: ", planet_progress_detail()))
    }
    if (isTRUE(is_comparing_sampling())) {
      active_msgs <- c(active_msgs, "Technique comparison running...")
    }
    force_dashboard_status(paste(active_msgs, collapse = " | "))
    dashboard_status_tick(Sys.time())
    }
  })
  
  output$sentinel_lock_info_ui <- renderUI({
    if (!isTRUE(sentinel_data_locked())) return(NULL)
      tags$p(
        class = "text-danger",
        strong("Current Sentinel data is loaded. Click 'Clear Sentinel Data' before running a new Search or changing 'Retrieve stack as'.")
      )
  })

  output$sentinel_retrieval_hint_ui <- renderUI({
    if (!identical(input$imagery_source, "download_sentinel")) return(NULL)
    tags$div(
      class = "sentinel-retrieval-hint",
      tags$p(
        style = "margin: 0;",
        tags$span(class = "sentinel-retrieving-label", "Retrieving"),
        " is memory- and bandwidth-heavy on this server—expect noticeable load while data transfers."
      ),
      tags$p(
        style = "margin: 8px 0 0 0; color: #4a5568; font-size: 12.5px;",
        "After retrieval, clear current Sentinel data before starting a new search cycle with different retrieval options."
      )
    )
  })
  
  observe({
    locked <- isTRUE(sentinel_data_locked())
    shinyjs::toggleState("search_sentinel", condition = !locked)
    shinyjs::toggleState("sentinel_retrieval_mode", condition = !locked)
    shinyjs::toggleState("sentinel_median_max_scenes", condition = !locked)
    shinyjs::toggleState("sentinel_cloud_limit", condition = !locked)
    shinyjs::toggleState("sentinel_date_range", condition = !locked)
    shinyjs::toggleState(
      "rank_sentinel_gndvi_cv",
      condition = !locked && !isTRUE(is_downloading_sentinel()) && !isTRUE(is_ranking_variability()) &&
        !isTRUE(is_building_ndre_timeseries())
    )
    shinyjs::toggleState(
      "rebuild_sentinel_timeseries",
      condition = !locked && !isTRUE(is_downloading_sentinel()) && !isTRUE(is_ranking_variability()) &&
        !isTRUE(is_building_ndre_timeseries()) && length(safe_features(sentinel_search_results())) > 0L
    )
  })
  
  output$message_log_drawer_ui <- renderUI({
    message_log_version()
    logs <- message_log_store
    if (is.null(logs) || nrow(logs) == 0) {
      return(tags$p(class = "text-muted", "No messages yet."))
    }
    idx <- rev(seq_len(nrow(logs)))
    tagList(
      lapply(idx, function(i) {
        tags$div(
          class = {
            typ <- tolower(as.character(logs$type[i]))
            if (identical(typ, "error")) "msg-log-item msg-log-error"
            else if (identical(typ, "warning")) "msg-log-item msg-log-warning"
            else "msg-log-item"
          },
          tags$div(class = "msg-log-meta", paste0(logs$time[i], "  |  ", toupper(tolower(as.character(logs$type[i]))))),
          tags$div(class = "msg-log-text", logs$message[i])
        )
      })
    )
  })
  outputOptions(output, "message_log_drawer_ui", suspendWhenHidden = FALSE)

  output$download_message_log <- downloadHandler(
    filename = function() {
      paste0("geosampler_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
    },
    content = function(file) {
      logs <- message_log_store
      hdr <- c(
        "GeoSampler message log",
        paste("Exported:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
        paste("Session token (first 12):", substr(as.character(session$token), 1L, 12L)),
        "---",
        ""
      )
      if (is.null(logs) || nrow(logs) == 0L) {
        writeLines(c(hdr, "(empty)"), file)
      } else {
        lines <- paste0(logs$time, " [", logs$type, "] ", logs$message)
        writeLines(c(hdr, lines), file)
      }
    }
  )
  
  observeEvent(input$toggle_message_log, {
    shinyjs::toggle("message_log_drawer")
    push_message_log_ui()
  }, ignoreInit = TRUE)
  
  observeEvent(input$clear_message_log, {
    message_log_store <<- message_log_tbl_empty()
    push_message_log_ui()
  }, ignoreInit = TRUE)
  
  # --- Planet Download Logic using provided chunk ---
  observeEvent(input$download_planet, {
    if(!is.null(digitized_features()) && nchar(input$api_key) > 0){
      abort_planet(FALSE)
      is_downloading_planet(TRUE)
      planet_status("Initializing download...")
      planet_progress_pct(10)
      planet_progress_detail("Initializing request...")
      shinyjs::delay(500, { if (isTRUE(is_downloading_planet())) { planet_progress_pct(30); planet_progress_detail("Searching available scenes...") } })
      shinyjs::delay(3500, { if (isTRUE(is_downloading_planet())) { planet_progress_pct(55); planet_progress_detail("Placing order and downloading...") } })
      shinyjs::delay(8000, { if (isTRUE(is_downloading_planet())) { planet_progress_pct(80); planet_progress_detail("Cropping/masking and finalizing...") } })
      
      shinyjs::disable("download_planet")
      
      mid_bound <- digitized_features() %>% st_transform(crs = 4326)
      bbox <- raster::extent(mid_bound)
      
      date_start <- input$date_range[1]
      date_end <- input$date_range[2]
      
      api_key <- input$api_key
      item_name <- input$item_name
      product_bundle <- input$product_bundle
      asset <- input$asset
      cloud_lim <- input$cloud_limit
      
      # Set/Create Export Folder in temp
      exportfolder <- paste("site", item_name, asset, lubridate::year(date_start), lubridate::year(date_end),  lubridate::yday(date_start),  lubridate::yday(date_end), sep = "_")
      download_dir <- file.path(tempdir(), "exports", exportfolder)
      
      future({
        oldwd <- getwd()
        on.exit(setwd(oldwd))
        
        dir.create(file.path(tempdir(), "exports"), showWarnings = FALSE, recursive = TRUE)
        setwd(file.path(tempdir(), "exports"))
        
        # Search
        planet_status("Requesting search results...")
        response <- planet_search(bbox = bbox,
                                  date_end = date_end,
                                  date_start = date_start,
                                  cloud_lim = cloud_lim, 
                                  item_name = item_name, 
                                  asset = asset,
                                  api_key = api_key)
        
        planet_status(paste("Images available:", length(response), item_name, asset))
        
        if (length(response) == 0) {
          stop("No imagery found for the specified criteria.")
        }
        
        # Order
        planet_status("Placing order...")
        planet_order(api_key = api_key, 
                     bbox = bbox, 
                     date_end = date_end,
                     date_start = date_start,
                     cloud_lim = cloud_lim, 
                     item_name = item_name, 
                     product_bundle = product_bundle,
                     asset = asset,
                     order_name = exportfolder,
                     mostrecent = 1)
        
        planet_status("Queuing and downloading...")
        
        tif_files <- list.files(exportfolder, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
        
        if (length(tif_files) == 0) {
          stop("No TIFF files found in download.")
        }
        
        # Load the first SR tif
        first_tif <- tif_files[grep("SR", tif_files)][1]
        if (is.na(first_tif)) {
          stop("No suitable SR TIFF file found.")
        }
        r <- raster::brick(first_tif)
        names(r) <- c("CoastalBlue", "Blue", "GreenI", "Green", "Yellow", "Red", "RedEdge", "NIR")
        
        # Crop and mask to boundary
        planet_status("Cropping and masking to boundary...")
        boundary_proj <- st_transform(mid_bound, crs = crs(r))
        r_cropped <- crop(r, boundary_proj)
        r_masked <- mask(r_cropped, boundary_proj)
        
        r_masked <- project_raster_bilinear(r_masked, crs = TARGET_CRS)
        # Re-mask after reprojection so no outside-AOI edge pixels remain.
        boundary_target <- tryCatch(st_transform(mid_bound, crs = crs(r_masked)), error = function(e) NULL)
        if (!is.null(boundary_target)) {
          r_masked <- tryCatch(mask(crop(r_masked, boundary_target), boundary_target), error = function(e) r_masked)
        }
        
        # Save processed raster
        processed_path <- file.path(exportfolder, "processed_SR.tif")
        writeRaster(r_masked, processed_path, overwrite = TRUE)
        
        planet_status("Data loaded successfully.")
        
        zip_filepath <- tempfile(fileext = ".zip")
        zip::zip(zipfile = zip_filepath, files = basename(tif_files), root = exportfolder)
        
        return(list(zip_path = zip_filepath, raster = r_masked))
        
      }, seed = TRUE) %...>% (function(result) {
        if (is.null(result) || isolate(abort_planet())) {
          showNotification("Planet download was cancelled.", type="warning")
          is_downloading_planet(FALSE)
          planet_progress_pct(0)
          planet_progress_detail("")
          return(NULL)
        }
        planet_result(result$zip_path) # Store zip path
        uploaded_planet_raster(result$raster) # Store raster
        planet_status("Download complete & loaded!")
        planet_progress_pct(100)
        planet_progress_detail("Planet retrieval complete.")
        output$planet_download_ui <- renderUI({
          downloadButton("download_planet_zip", "Download Planet Data (ZIP)")
        })
        is_downloading_planet(FALSE)
        notify_layer_ready("Planet imagery", "download")
        set_layer_tip("imagery")
        finalize_layer_on_map()
      }) %...!% (function(error) {
        err_msg <- promise_error_message(error)
        planet_status(paste("Error:", err_msg))
        showNotification(paste("Error downloading Planet data:", err_msg), type = "error", duration = 15)
        is_downloading_planet(FALSE)
        planet_progress_pct(0)
        planet_progress_detail("")
      }) %...>% (function() {
        shinyjs::enable("download_planet")
      })
      
      return(NULL)
    } else {
      showNotification("Please define a boundary and provide API key.", type = "error")
    }
  })
  
  output$download_planet_zip <- downloadHandler(
    filename = function() { "planet_data.zip" },
    content = function(file) { req(!is.null(planet_result())); file.copy(planet_result(), file) }
  )
  
  run_sentinel_ndre_timeseries_workflow <- function(notify = TRUE) {
    if (isTRUE(is_building_ndre_timeseries())) return(invisible(FALSE))
    if (isTRUE(is_downloading_sentinel())) return(invisible(FALSE))
    boundary <- digitized_features()
    if (is.null(boundary) || nrow(boundary) < 1L) {
      showNotification("Draw or upload a field boundary before building the NDRE time series.", type = "warning", duration = 8)
      return(invisible(FALSE))
    }
    res <- sentinel_search_results()
    features <- safe_features(res)
    if (length(features) < 1L) {
      showNotification("Run Sentinel Search first (Imagery sidebar).", type = "warning", duration = 5)
      return(invisible(FALSE))
    }
    dr <- input$sentinel_date_range
    dr_chr <- if (!is.null(dr) && length(dr) == 2L) {
      paste(format(dr[1], "%Y-%m-%d"), "to", format(dr[2], "%Y-%m-%d"))
    } else {
      "selected range"
    }
    is_building_ndre_timeseries(TRUE)
    lock_action_button("rebuild_sentinel_timeseries")
    shinyjs::disable("rank_sentinel_gndvi_cv")
    n_ts <- min(length(features), SENTINEL_NDRE_TS_MAX_SCENES())
    on_ts_error <- function(e) {
      is_building_ndre_timeseries(FALSE)
      unlock_action_button("rebuild_sentinel_timeseries")
      tryCatch(shinyjs::enable("rank_sentinel_gndvi_cv"), error = function(err) invisible(NULL))
      err_msg <- if (inherits(e, "condition")) conditionMessage(e) else as.character(e)
      showNotification(paste("NDRE time series failed:", err_msg), type = "error", duration = 12)
      release_geosampler_memory()
    }
    withProgress(message = "Building NDRE chart...", value = 0, {
      tryCatch({
        setProgress(0.08, detail = paste0("Up to ", n_ts, " evenly spaced scene(s) in ", dr_chr))
        ts_out <- build_sentinel_ndre_timeseries_df(
          res, boundary,
          progress_fn = function(val, detail) setProgress(0.12 + 0.82 * val, detail = detail)
        )
        if (is.null(ts_out) || is.null(ts_out$df) || nrow(ts_out$df) < 1L) {
          sentinel_ndre_timeseries_df(NULL)
          sentinel_ndre_timeseries_meta(NULL)
          stop("No valid mean NDRE values could be computed for scenes in this search.", call. = FALSE)
        }
        ts_df <- ts_out$df
        sentinel_ndre_timeseries_df(ts_df)
        sentinel_ndre_timeseries_meta(list(
          date_range = dr_chr,
          n_scenes_searched = length(features),
          n_scenes_plotted = nrow(ts_df),
          n_scenes_attempted = ts_out$n_scenes_used,
          n_scenes_total = ts_out$n_scenes_total,
          evenly_spaced = isTRUE(ts_out$evenly_spaced),
          max_cells_per_scene = ts_out$max_cells_per_scene,
          cloud_limit = suppressWarnings(as.numeric(input$sentinel_cloud_limit))
        ))
        setProgress(1, detail = "Done")
        if (isTRUE(notify)) {
          cap_note <- if (isTRUE(ts_out$evenly_spaced)) {
            paste0(" (", ts_out$n_scenes_used, " evenly spaced of ", ts_out$n_scenes_total, ")")
          } else {
            ""
          }
          showNotification(
            paste0("NDRE chart ready (", nrow(ts_df), " scene", if (nrow(ts_df) == 1L) "" else "s", cap_note, ")."),
            type = "message",
            duration = 6
          )
        }
      }, error = on_ts_error)
    })
    is_building_ndre_timeseries(FALSE)
    unlock_action_button("rebuild_sentinel_timeseries")
    tryCatch(shinyjs::enable("rank_sentinel_gndvi_cv"), error = function(e) invisible(NULL))
    release_geosampler_memory()
    invisible(TRUE)
  }

  # --- Sentinel Search Logic (Search updates catalog; Build timeseries draws chart on demand) ---
  run_sentinel_stac_search_from_sidebar <- function(navigate_to_timeseries = TRUE, from_rebuild = FALSE) {
    if (isTRUE(sentinel_data_locked())) {
      showNotification("Clear Sentinel Data before running a new search or changing retrieval mode.", type = "warning", duration = 7)
      return(invisible(NULL))
    }
    if (is.null(digitized_features())) {
      showNotification("Draw or upload a field boundary before searching Sentinel data.", type = "warning", duration = 7)
      return(invisible(NULL))
    }
    if (isTRUE(is_downloading_sentinel())) {
      showNotification("A Sentinel search is already running.", type = "message", duration = 4)
      return(invisible(NULL))
    }
    status_label <- if (isTRUE(from_rebuild)) "Refreshing Sentinel search..." else "Searching Sentinel data..."
    set_sentinel_step(status = status_label, detail = "Searching catalog...", type = "message")
    is_downloading_sentinel(TRUE)  # Use for search too
    tryCatch(shinyjs::disable("rank_sentinel_gndvi_cv"), error = function(e) invisible(NULL))
    tryCatch(lock_action_button("rebuild_sentinel_timeseries"), error = function(e) invisible(NULL))
    tryCatch(lock_action_button("search_sentinel"), error = function(e) invisible(NULL))
    sentinel_progress_pct(20)
    shinyjs::delay(400, { if (isTRUE(is_downloading_sentinel())) { sentinel_progress_pct(35); set_sentinel_step(detail = "Applying filters...", type = "message") } })
    shinyjs::delay(900, { if (isTRUE(is_downloading_sentinel())) { sentinel_progress_pct(55); set_sentinel_step(detail = "Finalizing search results...", type = "message") } })
    
    mid_bound <- sf::st_transform(digitized_features(), 4326)
    bb <- sf::st_bbox(mid_bound)
    bbox <- c(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    
    date_start <- format(input$sentinel_date_range[1], "%Y-%m-%d")
    date_end <- format(input$sentinel_date_range[2], "%Y-%m-%d")
    cloud_lim <- suppressWarnings(as.numeric(input$sentinel_cloud_limit))
    if (length(cloud_lim) != 1L || !is.finite(cloud_lim)) cloud_lim <- 10
    max_items <- SENTINEL_STAC_SEARCH_MAX_ITEMS()
    sp <- deployment_safety_params()
    if (isTRUE(sp$active) && !is.null(sp$scene_cap)) {
      cap_sc <- suppressWarnings(as.integer(sp$scene_cap))
      if (length(cap_sc) == 1L && !is.na(cap_sc)) {
        max_items <- min(max_items, max(50L, cap_sc * 8L))
      }
    }
    datetime <- paste0(date_start, "/", date_end)

    finish_sentinel_search <- function(result) {
      feats_raw <- safe_features(result)
      features <- compact_stac_features(feats_raw, max_keep = length(feats_raw))
      result$features <- features
      sentinel_ndre_timeseries_df(NULL)
      sentinel_ndre_timeseries_meta(NULL)
      sentinel_search_results(result)
      stats <- result$`app:search_stats`
      set_sentinel_step(status = paste("Found", length(features), "images."), detail = "Search complete.", type = "message")
      if (is.list(stats) && !is.null(stats$after_cloud)) {
        extra <- if (isTRUE(stats$stac_truncated)) {
          paste0(" STAC page cap (", stats$max_items_cap, " items) — narrow the date range if the list looks short.")
        } else if (isTRUE(from_rebuild)) {
          " Click Build timeseries to visualize the refreshed scene list."
        } else {
          " Click Build timeseries to visualize the NDRE trend."
        }
        showNotification(
          paste0(
            "Search complete: ", length(features), " scene(s) after cloud filter (<= ", stats$cloud_limit, "%)",
            if (!is.null(stats$stac_fetched) && stats$stac_fetched != stats$after_cloud) {
              paste0(" from ", stats$stac_fetched, " STAC item(s) in range")
            } else {
              ""
            },
            ".", extra
          ),
          type = "message",
          duration = 9
        )
      } else {
        showNotification(
          paste0("Search complete: found ", length(features), " Sentinel image(s), oldest to newest."),
          type = "message",
          duration = 6
        )
      }
      n_found <- length(features)
      if (!is.null(n_found) && n_found >= 2) {
        default_n <- max(2L, min(10L, n_found))
        max_cap <- min(300L, max(2L, n_found))
        updateNumericInput(session, "sentinel_median_max_scenes", max = max_cap, value = min(default_n, max_cap))
      }
      if (length(features) > 0) {
        render_sentinel_select_ui(result)
        mode_now <- if (is.null(input$sentinel_retrieval_mode)) "single" else input$sentinel_retrieval_mode
        if (identical(mode_now, "median") && length(features) < 2L) {
          showNotification(
            "Median mode needs at least 2 Sentinel scenes. Your search returned 1 — try a wider date range or higher cloud limit.",
            type = "warning",
            duration = 10
          )
        }
      } else {
        showNotification("No images found for the criteria.", type = "warning")
      }
      is_downloading_sentinel(FALSE)
      sentinel_progress_pct(100)
      sentinel_last_completed_at(Sys.time())
      tryCatch(shinyjs::enable("rank_sentinel_gndvi_cv"), error = function(e) invisible(NULL))
      tryCatch(unlock_action_button("rebuild_sentinel_timeseries"), error = function(e) invisible(NULL))
      tryCatch(unlock_action_button("search_sentinel"), error = function(e) invisible(NULL))
      release_geosampler_memory()
      if (length(features) > 0L && isTRUE(navigate_to_timeseries)) {
        tryCatch(updateTabsetPanel(session, "main_tabs", selected = "Variables"), error = function(e) NULL)
        tryCatch(updateTabsetPanel(session, "variables_subtabs", selected = "imagery"), error = function(e) NULL)
        tryCatch(
          updateTabsetPanel(session, "imagery_view_subtabs", selected = "imagery_timeseries_tab"),
          error = function(e) NULL
        )
      }
    }

    on_search_error <- function(error) {
      err_msg <- if (inherits(error, "condition")) promise_error_message(error) else as.character(error)
      set_sentinel_step(status = paste("Error:", err_msg), detail = "", type = "error")
      showNotification(paste("Error searching Sentinel data:", err_msg), type = "error", duration = 15)
      is_downloading_sentinel(FALSE)
      sentinel_progress_pct(0)
      sentinel_progress_detail("")
      tryCatch(shinyjs::enable("rank_sentinel_gndvi_cv"), error = function(e) invisible(NULL))
      tryCatch(unlock_action_button("rebuild_sentinel_timeseries"), error = function(e) invisible(NULL))
      tryCatch(unlock_action_button("search_sentinel"), error = function(e) invisible(NULL))
    }

    if (use_sequential_futures()) {
      tryCatch({
        result <- run_sentinel_stac_search(bbox, datetime, cloud_lim, max_items)
        finish_sentinel_search(result)
      }, error = on_search_error)
      return(invisible(NULL))
    }

    promises::future_promise({
      run_sentinel_stac_search(bbox, datetime, cloud_lim, max_items)
    }, seed = TRUE) %...>% (function(result) {
      finish_sentinel_search(result)
    }) %...!% (function(error) {
      on_search_error(error)
    })

    invisible(NULL)
  }

  observeEvent(input$search_sentinel, {
    run_sentinel_stac_search_from_sidebar(navigate_to_timeseries = FALSE, from_rebuild = FALSE)
  }, ignoreInit = TRUE)

  observeEvent(input$rebuild_sentinel_timeseries, {
    run_sentinel_ndre_timeseries_workflow(notify = TRUE)
  }, ignoreInit = TRUE)

  output$sentinel_search_cap_notice_ui <- renderUI({
    res <- sentinel_search_results()
    if (is.null(res)) return(NULL)
    sentinel_search_results_notice_ui(length(safe_features(res)), res$`app:search_stats`)
  })
  outputOptions(output, "sentinel_search_cap_notice_ui", suspendWhenHidden = FALSE)

  output$sentinel_retrieve_ui <- renderUI({
    res <- sentinel_search_results()
    features <- safe_features(res)
    mode_now <- if (is.null(input$sentinel_retrieval_mode)) "single" else input$sentinel_retrieval_mode
    is_busy <- isTRUE(is_downloading_sentinel())
    if (isTRUE(sentinel_data_locked()) && length(features) == 0) {
      return(tags$p(class = "text-danger", "Current Sentinel data is loaded. Click 'Clear Sentinel Data' to start a new search/retrieval cycle."))
    }
    if (length(features) == 0) {
      return(tags$p(class = "text-muted", "Run Search first to enable retrieval."))
    }
    if (is_busy && identical(mode_now, "median")) {
      return(tags$button(
        id = "download_sentinel",
        type = "button",
        class = "btn btn-warning",
        disabled = "disabled",
        icon("spinner", class = "fa-spin"),
        " Computing Median..."
      ))
    }
    btn_label <- if (identical(mode_now, "median")) "Retrieve Median Values" else "Retrieve Selected"
    actionButton("download_sentinel", btn_label, class = "btn-primary")
  })

  output$sentinel_gndvi_rank_ui <- renderUI({
    if (isTRUE(sentinel_data_locked())) return(NULL)
    if (isTRUE(is_downloading_sentinel())) return(NULL)
    res <- sentinel_search_results()
    feats <- safe_features(res)
    if (length(feats) < 1L) return(NULL)
    mode_now <- if (is.null(input$sentinel_retrieval_mode)) "single" else input$sentinel_retrieval_mode
    tagList(
      actionButton(
        "rank_sentinel_gndvi_cv",
        "Rank scenes (quick NDRE hint)",
        class = "btn-modern btn-sm btn-outline-primary",
        icon = icon("sort-amount-down")
      ),
      if (identical(mode_now, "single")) {
        tags$p(
          class = "text-muted",
          style = "font-size:10.5px; margin:6px 0 0 0; line-height:1.35;",
            "Optional: ranks up to ", SENTINEL_NDRE_RANK_MAX_SCENES(), " scenes by NDRE spread. ",
            tags$strong("Search"), " updates the date list; ",
            tags$strong("Build timeseries"), " draws the NDRE trend when you are ready."
        )
      } else {
        tags$p(
          class = "text-muted",
          style = "font-size:10.5px; margin:6px 0 0 0; line-height:1.35;",
          "Ranking applies in single-scene mode only."
        )
      }
    )
  })

  finish_variability_rank <- function(ranked, features) {
    is_ranking_variability(FALSE)
    sentinel_search_results(ranked)
    render_sentinel_select_ui(ranked)
    cv <- scene_ndre_spread_vector(ranked)
    ch <- sentinel_single_scene_choices(safe_features(ranked), cv)
    n_scored <- sum(is.finite(cv))
    if (n_scored >= 1L && is.finite(cv[ch$best_idx])) {
      showNotification(
        paste0(
              "Scene hint ready (", n_scored, " scored). Highest NDRE spread ★: scene ",
              ch$best_idx, " (", sprintf("%.3f", cv[ch$best_idx]), ")."
        ),
        type = "message",
        duration = 10
      )
    } else {
      showNotification(
        paste0(
          "Could not score variability in your boundary for ", length(features),
          " scene(s). Check that the boundary overlaps each scene, try a slightly larger AOI, ",
          "or confirm Planetary Computer access (same network as Search). Cloud cover in the AOI can also leave too few crop pixels."
        ),
        type = "warning",
        duration = 12
      )
    }
    tryCatch(shinyjs::enable("rank_sentinel_gndvi_cv"), error = function(e) invisible(NULL))
    release_geosampler_memory()
    invisible(NULL)
  }

  observeEvent(input$rank_sentinel_gndvi_cv, {
    if (isTRUE(is_ranking_variability())) return(invisible(NULL))
    if (isTRUE(sentinel_data_locked())) {
      showNotification("Clear Sentinel data before ranking search results.", type = "warning", duration = 6)
      return(invisible(NULL))
    }
    boundary <- digitized_features()
    if (is.null(boundary) || nrow(boundary) < 1L) {
      showNotification("Draw or upload a field boundary before ranking scenes by field variability.", type = "warning", duration = 8)
      return(invisible(NULL))
    }
    res <- sentinel_search_results()
    features <- safe_features(res)
    if (length(features) < 1L) {
      showNotification("Run Search first.", type = "warning", duration = 5)
      return(invisible(NULL))
    }
    is_ranking_variability(TRUE)
    shinyjs::disable("rank_sentinel_gndvi_cv")
    n_score <- min(length(features), SENTINEL_NDRE_RANK_MAX_SCENES())
    on_rank_error <- function(e) {
      is_ranking_variability(FALSE)
      tryCatch(shinyjs::enable("rank_sentinel_gndvi_cv"), error = function(err) invisible(NULL))
      err_msg <- if (inherits(e, "condition")) conditionMessage(e) else as.character(e)
      showNotification(paste("NDRE spread ranking failed:", err_msg), type = "error", duration = 12)
      release_geosampler_memory()
    }

    withProgress(message = "Quick NDRE spread hint...", value = 0, {
      tryCatch({
        ranked <- rank_sentinel_search_by_ndre(
          res, boundary,
          progress_fn = function(val, detail) {
            step <- suppressWarnings(as.integer(round(val * n_score)))
            step <- max(1L, min(n_score, step))
            setProgress(0.08 + 0.84 * val, detail = paste0(step, "/", n_score, " scenes"))
          }
        )
        finish_variability_rank(ranked, features)
        setProgress(1, detail = NULL)
      }, error = on_rank_error)
    })
    invisible(NULL)
  }, ignoreInit = TRUE)

  output$sentinel_ndre_timeseries_ready <- reactive({
    df <- sentinel_ndre_timeseries_df()
    !is.null(df) && nrow(df) > 0L
  })
  outputOptions(output, "sentinel_ndre_timeseries_ready", suspendWhenHidden = FALSE)

  output$sentinel_timeseries_status_ui <- renderUI({
    if (isTRUE(is_building_ndre_timeseries())) {
      return(tags$div(
        class = "alert alert-info",
        style = "margin-top:10px; font-size:12px;",
        icon("spinner", class = "fa-spin"),
        " Building NDRE chart from search scenes…"
      ))
    }
    meta <- sentinel_ndre_timeseries_meta()
    df <- sentinel_ndre_timeseries_df()
    if (is.null(df) || nrow(df) < 1L) {
      return(tags$div(
        class = "text-muted",
        style = "margin-top:10px; font-size:12px;",
        tags$strong("Search"), " to update available dates, then ", tags$strong("Build timeseries"), " to view the trend."
      ))
    }
    tags$div(
      class = "text-muted",
      style = "margin-top:10px; font-size:12px;",
      nrow(df), " scene(s) in chart",
      if (!is.null(meta) && isTRUE(meta$evenly_spaced) && !is.null(meta$n_scenes_total)) {
        paste0(" (evenly spaced of ", meta$n_scenes_total, ")")
      },
      if (!is.null(meta) && !is.null(meta$date_range)) paste0(" · ", meta$date_range),
      " — pick a date, then retrieve on ", tags$strong("Imagery Viewer"), "."
    )
  })

  output$sentinel_ndre_timeseries_caption_ui <- renderUI({
    meta <- sentinel_ndre_timeseries_meta()
    df <- sentinel_ndre_timeseries_df()
    if (is.null(df) || nrow(df) < 1L) {
      return(tags$p(
        class = "text-muted",
        tags$strong("Search"), ", then ", tags$strong("Build timeseries"), " to choose a retrieve date."
      ))
    }
    n_try <- if (!is.null(meta$n_scenes_attempted)) meta$n_scenes_attempted else nrow(df)
    n_find <- if (!is.null(meta$n_scenes_searched)) meta$n_scenes_searched else NA_integer_
    tags$p(
      class = "text-muted",
      style = "font-size:11px; margin-top:8px;",
      "Mean NDRE per date (subsampled pixels, no full download). ",
      if (!is.null(meta) && isTRUE(meta$evenly_spaced) && !is.null(meta$n_scenes_total)) {
        paste0("Evenly spaced ", n_try, " of ", meta$n_scenes_total, " search scenes. ")
      } else if (!is.na(n_find)) {
        paste0(n_try, "/", n_find, " scenes scored. ")
      },
      "Retrieve your chosen date on ", tags$strong("Imagery Viewer"), "."
    )
  })

  output$download_sentinel_ndre_timeseries_plot <- downloadHandler(
    filename = function() {
      paste0("sentinel_ndre_timeseries_", format(Sys.time(), "%Y%m%d_%H%M"), ".png")
    },
    content = function(file) {
      df <- sentinel_ndre_timeseries_df()
      if (is.null(df) || nrow(df) < 1L) {
        stop("Rebuild on Timeseries viewer first.", call. = FALSE)
      }
      p <- plot_sentinel_ndre_timeseries(df, sentinel_ndre_timeseries_meta())
      if (is.null(p)) stop("Could not build NDRE time series plot.", call. = FALSE)
      ggplot2::ggsave(
        filename = file,
        plot = p,
        width = 10,
        height = 5.5,
        units = "in",
        dpi = 150,
        bg = "white"
      )
    }
  )

  output$sentinel_ndre_timeseries_plot <- renderPlot({
    df <- sentinel_ndre_timeseries_df()
    req(!is.null(df), nrow(df) > 0L)
    p <- plot_sentinel_ndre_timeseries(df, sentinel_ndre_timeseries_meta())
    req(!is.null(p))
    print(p)
  },
  height = 420,
  width = function() {
    input$imagery_view_subtabs
    w <- session$clientData$output_sentinel_ndre_timeseries_plot_width
    if (is.null(w) || !is.finite(w) || w < 280) 960L else as.integer(w)
  },
  res = 110,
  bg = "transparent")
  outputOptions(output, "sentinel_ndre_timeseries_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "sentinel_timeseries_status_ui", suspendWhenHidden = FALSE)

  observeEvent(
    list(input$sentinel_retrieval_mode, input$sentinel_median_max_scenes, sentinel_search_results()),
    {
      res <- sentinel_search_results()
      if (!length(safe_features(res))) return(invisible(NULL))
      render_sentinel_select_ui(res)
    },
    ignoreInit = TRUE
  )
  
  observeEvent(input$sentinel_retrieval_mode, {
    if (isTRUE(sentinel_data_locked())) return(invisible(NULL))
    if (is.null(sentinel_search_results())) return(invisible(NULL))
    sentinel_ndre_timeseries_df(NULL)
    sentinel_ndre_timeseries_meta(NULL)
    sentinel_search_results(NULL)
    output$sentinel_select_ui <- renderUI({})
    showNotification("Retrieve stack mode changed. Search results were cleared so you can start fresh with this mode.", type = "message", duration = 6)
  }, ignoreInit = TRUE)
  
  # --- Sentinel Download Logic ---
  observeEvent(input$download_sentinel, {
    req(!is.null(sentinel_search_results()))
    req(length(safe_features(sentinel_search_results())) > 0)
    mode <- if (is.null(input$sentinel_retrieval_mode)) "single" else input$sentinel_retrieval_mode
    if (identical(mode, "single")) {
      req(input$sentinel_selected)
    }
    tryCatch(updateTabsetPanel(session, "main_tabs", selected = "Variables"), error = function(e) NULL)
    tryCatch(updateTabsetPanel(session, "variables_subtabs", selected = "imagery"), error = function(e) NULL)
    tryCatch(
      updateTabsetPanel(session, "imagery_view_subtabs", selected = "imagery_map_tab"),
      error = function(e) NULL
    )
    shinyjs::delay(250, refit_maps_on_tab())
    abort_sentinel(FALSE)
    is_downloading_sentinel(TRUE)
    set_sentinel_step(
      status = if (identical(mode, "median")) "Building median composite..." else "Downloading Sentinel data...",
      detail = if (identical(mode, "median")) "Preparing scenes for median composite..." else "Preparing selected scene...",
      type = "message"
    )
    sentinel_progress_pct(10)
    if (identical(mode, "median")) {
      shinyjs::delay(1500, { if (isTRUE(is_downloading_sentinel())) { sentinel_progress_pct(25); set_sentinel_step(detail = "Reading and harmonizing scenes...", type = "message") } })
      shinyjs::delay(4500, { if (isTRUE(is_downloading_sentinel())) { sentinel_progress_pct(45); set_sentinel_step(detail = "Calculating per-pixel median...", type = "message") } })
      shinyjs::delay(9000, { if (isTRUE(is_downloading_sentinel())) { sentinel_progress_pct(70); set_sentinel_step(detail = "Finalizing median stack...", type = "message") } })
      shinyjs::delay(14000, { if (isTRUE(is_downloading_sentinel())) { sentinel_progress_pct(85); set_sentinel_step(detail = "Writing output raster...", type = "message") } })
    } else {
      shinyjs::delay(1200, { if (isTRUE(is_downloading_sentinel())) { sentinel_progress_pct(45); set_sentinel_step(detail = "Downloading and clipping selected scene...", type = "message") } })
      shinyjs::delay(5000, { if (isTRUE(is_downloading_sentinel())) { sentinel_progress_pct(80); set_sentinel_step(detail = "Writing output raster...", type = "message") } })
    }
    
    shinyjs::disable("download_sentinel")
    
    it_obj <- sentinel_search_results()
    all_features <- safe_features(it_obj)
    if (!length(all_features)) {
      set_sentinel_step(status = "Error: No Sentinel scenes available to retrieve.", detail = "", type = "error")
      is_downloading_sentinel(FALSE)
      shinyjs::enable("download_sentinel")
      return(invisible(NULL))
    }
    sp <- deployment_safety_params()
    median_cap <- sentinel_median_scene_count_safe(input$sentinel_median_max_scenes, length(all_features), 8L)
    if (isTRUE(sp$active) && !is.null(sp$scene_cap)) {
      cap_hi <- suppressWarnings(as.integer(max(2L, floor(sp$scene_cap / 2L))))
      if (length(cap_hi) == 1L && !is.na(cap_hi)) {
        median_cap <- min(median_cap, cap_hi, length(all_features))
        if (length(all_features) >= 2L) median_cap <- max(2L, median_cap)
      }
      showNotification(paste0("Safety mode active: median scene count capped to ", median_cap, "."), type = "warning", duration = 6)
    }
    bound <- digitized_features()
    
    feature_indices <- if (identical(mode, "single")) {
      idx <- as.integer(input$sentinel_selected)
      if (is.na(idx) || idx < 1L || idx > length(all_features)) {
        set_sentinel_step(status = "Error: Selected Sentinel scene is no longer available. Please run Search again.", detail = "", type = "error")
        is_downloading_sentinel(FALSE)
        shinyjs::enable("download_sentinel")
        return(invisible(NULL))
      }
      idx
    } else {
      if (length(all_features) < 2L) {
        set_sentinel_step(
          status = "Error: Per-pixel median needs at least 2 Sentinel scenes in search results.",
          detail = "Widen the date range or relax the cloud filter, then search again.",
          type = "error"
        )
        showNotification(
          "Cannot build a median composite: only 1 scene matched your search. Use a wider date range or higher cloud limit.",
          type = "error",
          duration = 12
        )
        is_downloading_sentinel(FALSE)
        shinyjs::enable("download_sentinel")
        return(invisible(NULL))
      }
      sentinel_median_scene_indices(length(all_features), input$sentinel_median_max_scenes, 8L)
    }
    meta_features <- all_features[feature_indices]

    worker_detail <- if (use_sequential_futures()) "Processing in session..." else "Dispatching background worker..."
    set_sentinel_step(
      status = if (identical(mode, "median")) "Building median composite..." else "Downloading Sentinel data...",
      detail = worker_detail,
      type = "message"
    )

    promises::future_promise({
      # Fresh SAS tokens at retrieve time (unsigned blob URLs fail with /vsicurl/).
      signed_collection <- rstac::items_sign(it_obj, sign_fn = rstac::sign_planetary_computer())
      
      get_s2_scale_offset <- function(asset) {
        rb <- asset[["raster:bands"]]
        if (!is.null(rb) && length(rb) > 0) {
          sc <- rb[[1]]$scale;  if (is.null(sc)) sc <- 0.0001
          of <- rb[[1]]$offset; if (is.null(of)) of <- 0
        } else {
          sc <- 0.0001; of <- 0
        }
        list(scale = as.numeric(sc), offset = as.numeric(of))
      }
      
      apply_s2_scale <- function(r, asset) {
        so <- get_s2_scale_offset(asset)
        terra::clamp(r * so$scale + so$offset, 0, 1)
      }
      
      scene_stack_proj <- function(feature) {
        blue_url  <- paste0("/vsicurl/", feature$assets$B02$href)
        green_url <- paste0("/vsicurl/", feature$assets$B03$href)
        red_url   <- paste0("/vsicurl/", feature$assets$B04$href)
        nir_url   <- paste0("/vsicurl/", feature$assets$B08$href)
        re_url    <- paste0("/vsicurl/", feature$assets$B05$href)
        
        blue  <- terra::rast(blue_url)
        green <- terra::rast(green_url)
        red   <- terra::rast(red_url)
        nir   <- terra::rast(nir_url)
        re    <- terra::rast(re_url)

        bound_proj <- terra::project(terra::vect(bound_wkt, crs = "EPSG:4326"), terra::crs(blue))

        blue_c  <- terra::mask(terra::crop(blue,  bound_proj), bound_proj)
        green_c <- terra::mask(terra::crop(green, bound_proj), bound_proj)
        red_c   <- terra::mask(terra::crop(red,   bound_proj), bound_proj)
        nir_c   <- terra::mask(terra::crop(nir,   bound_proj), bound_proj)
        re_c    <- terra::resample(terra::mask(terra::crop(re, bound_proj), bound_proj), nir_c, method = "bilinear")
        blue_c  <- terra::resample(blue_c,  nir_c, method = "bilinear")
        green_c <- terra::resample(green_c, nir_c, method = "bilinear")
        red_c   <- terra::resample(red_c,   nir_c, method = "bilinear")
        
        blue_c  <- apply_s2_scale(blue_c,  feature$assets$B02)
        green_c <- apply_s2_scale(green_c, feature$assets$B03)
        red_c   <- apply_s2_scale(red_c,   feature$assets$B04)
        re_c    <- apply_s2_scale(re_c,    feature$assets$B05)
        nir_c   <- apply_s2_scale(nir_c,   feature$assets$B08)

        scl_asset <- feature$assets[["SCL"]]
        if (!is.null(scl_asset) && !is.null(scl_asset$href) && nzchar(as.character(scl_asset$href))) {
          scl_wm <- tryCatch({
            scl_u <- terra::rast(paste0("/vsicurl/", scl_asset$href))
            scl_c <- terra::mask(terra::crop(scl_u, bound_proj), bound_proj)
            scl_r <- terra::resample(scl_c, nir_c, method = "near")
            terra::ifel(scl_r %in% c(3, 8, 9, 10), NA, 1)
          }, error = function(e) NULL)
          if (!is.null(scl_wm)) {
            blue_c  <- terra::mask(blue_c,  scl_wm)
            green_c <- terra::mask(green_c, scl_wm)
            red_c   <- terra::mask(red_c,   scl_wm)
            re_c    <- terra::mask(re_c,    scl_wm)
            nir_c   <- terra::mask(nir_c,   scl_wm)
          }
        }
        
        sentinel_stack <- c(blue_c, green_c, red_c, re_c, nir_c)
        names(sentinel_stack) <- c("Blue", "Green", "Red", "RedEdge", "NIR")
        sentinel_proj <- terra::project(sentinel_stack, TARGET_CRS)
        bound_target <- tryCatch(
          terra::project(terra::vect(bound_wkt, crs = "EPSG:4326"), terra::crs(sentinel_proj)),
          error = function(e) NULL
        )
        if (!is.null(bound_target)) {
          sentinel_proj <- tryCatch(
            terra::mask(terra::crop(sentinel_proj, bound_target), bound_target),
            error = function(e) sentinel_proj
          )
        }
        sentinel_proj
      }

      if (!length(feature_indices)) stop("No Sentinel scenes available for retrieval.")
      
      if (identical(mode, "single")) {
        feature <- signed_collection$features[[feature_indices[[1L]]]]
        if (is.null(feature)) stop("Selected Sentinel scene is no longer available. Please run Search again.")
        sentinel_stack_proj <- scene_stack_proj(feature)
      } else {
        if (length(feature_indices) < 2L) stop("Median mode needs at least 2 scenes in the search results.")
        stacks <- vector("list", length(feature_indices))
        for (ki in seq_along(feature_indices)) {
          feature <- signed_collection$features[[feature_indices[[ki]]]]
          if (is.null(feature)) stop(paste0("Scene ", feature_indices[[ki]], " is no longer available. Please run Search again."))
          stacks[[ki]] <- scene_stack_proj(feature)
        }
        ref <- stacks[[1]]
        if (length(stacks) > 1L) {
          for (ki in 2L:length(stacks)) {
            stacks[[ki]] <- terra::resample(stacks[[ki]], ref, method = "bilinear")
          }
        }
        band_names <- c("Blue", "Green", "Red", "RedEdge", "NIR")
        med_layers <- vector("list", 5L)
        for (bi in seq_len(5L)) {
          rl <- terra::rast(lapply(stacks, function(s) s[[bi]]))
          med_layers[[bi]] <- terra::app(rl, fun = function(z) stats::median(z, na.rm = TRUE))
        }
        sentinel_stack_proj <- terra::rast(med_layers)
        names(sentinel_stack_proj) <- band_names
      }
      if (identical(mode, "median")) {
        names(sentinel_stack_proj) <- paste0(names(sentinel_stack_proj), "_median")
      }
      
      tif_filepath <- tempfile(fileext = ".tif")
      terra::writeRaster(sentinel_stack_proj, tif_filepath, overwrite = TRUE, datatype = "FLT4S")

      list(
        tif_path = tif_filepath,
        band_names = names(sentinel_stack_proj),
        retrieval_mode = mode
      )
    }, seed = TRUE, globals = list(
      it_obj = it_obj,
      feature_indices = feature_indices,
      bound_wkt = sf::st_as_text(sf::st_geometry(sf::st_transform(bound, 4326))[[1]]),
      mode = mode,
      TARGET_CRS = TARGET_CRS
    ), packages = c("terra", "raster", "sf", "rstac", "httr")) %...>% (function(result) {
      if (is.null(result) || isolate(abort_sentinel())) {
        showNotification("Sentinel download was cancelled.", type="warning")
        is_downloading_sentinel(FALSE)
        sentinel_progress_pct(0)
        sentinel_progress_detail("")
        return(NULL)
      }
      set_sentinel_step(status = "Loading Sentinel raster into app...", detail = "Preparing layers for display...", type = "message")
      sentinel_result(result$tif_path)
      raster_loaded <- brick(result$tif_path)
      names(raster_loaded) <- result$band_names
      uploaded_sentinel_raster(raster_loaded)
      sentinel_retrieval_used(result$retrieval_mode)
      meta_labels <- tryCatch(
        vapply(meta_features, sentinel_scene_choice_label, character(1)),
        error = function(e) character(0)
      )
      meta_dates <- tryCatch(
        vapply(meta_features, function(f) as.character(f$properties$datetime), character(1)),
        error = function(e) character(0)
      )
      sentinel_retrieval_meta(list(
        mode = result$retrieval_mode,
        retrieved_at = Sys.time(),
        scene_count = length(meta_features),
        scene_dates = meta_dates,
        scene_labels = meta_labels
      ))
      set_sentinel_step(
        status = if (identical(result$retrieval_mode, "median")) "Median composite ready & loaded!" else "Download complete & loaded!",
        detail = if (identical(result$retrieval_mode, "median")) "Median retrieval complete." else "Scene retrieval complete.",
        type = "message"
      )
      output$sentinel_download_ui <- renderUI({
        downloadButton("download_sentinel_tif", "Download Sentinel Stack TIFF")
      })
      output$clear_sentinel_ui <- renderUI({
        actionButton("clear_sentinel", "Clear Sentinel Data", class = "btn-danger")
      })
      is_downloading_sentinel(FALSE)
      sentinel_progress_pct(100)
      sentinel_last_completed_at(Sys.time())
      notify_layer_ready("Sentinel imagery", "download")
      set_layer_tip("imagery")
      band0 <- tryCatch(names(isolate(uploaded_sentinel_raster()))[[1]], error = function(e) NULL)
      if (!is.null(band0) && nzchar(band0)) {
        shinyjs::delay(400, tryCatch(redraw_sentinel_raster(band0), error = function(e) NULL))
      }
      finalize_layer_on_map()
    }) %...!% (function(error) {
      err_msg <- promise_error_message(error)
      set_sentinel_step(status = paste("Error:", err_msg), detail = "", type = "error")
      showNotification(paste("Error downloading Sentinel data:", err_msg), type = "error", duration = 15)
      is_downloading_sentinel(FALSE)
      sentinel_progress_pct(0)
      sentinel_progress_detail("")
    }) %...>% (function() {
      shinyjs::enable("download_sentinel")
    })
    
    return(NULL)
  })
  
  output$download_sentinel_tif <- downloadHandler(
    filename = function() { "sentinel_stack.tif" },
    content = function(file) { req(!is.null(sentinel_result())); file.copy(sentinel_result(), file) }
  )
  
  observeEvent(input$clear_sentinel, {
    remove_sentinel_data()
    sentinel_ndre_timeseries_df(NULL)
    sentinel_ndre_timeseries_meta(NULL)
    release_geosampler_memory()
    reset_sampling_comparison_state()
    sync_sampling_after_covariate_change(notify = NULL)
    # Reset ALL sentinel reactive state so a fresh search+retrieve cycle is possible
    sentinel_result(NULL)
    sentinel_search_results(NULL)
    sentinel_status(NULL)
    sentinel_retrieval_used("single")
    sentinel_retrieval_meta(NULL)
    is_downloading_sentinel(FALSE)
    sentinel_progress_pct(0)
    sentinel_progress_detail("")
    sentinel_last_completed_at(NULL)
    sentinel_console_lines(character(0))
    showNotification("Sentinel data cleared. Prior sample points and technique comparison were reset.", type = "warning")
    showNotification(
      paste(
        "Search and retrieve a new scene, calculate vegetation indices, then generate samples again.",
        "Use Generate (custom settings) directly, or re-run Technique comparison first if you use the recommendation button.",
        "Wait 5–10 seconds before clicking Search."
      ),
      type = "message",
      duration = 12
    )
    # Reset every sentinel-related UI output
    output$clear_sentinel_ui       <- renderUI({})
    output$sentinel_band_selector_ui <- renderUI({})
    output$sentinel_vi_calculator_ui <- renderUI({})
    output$sentinel_vi_download_ui   <- renderUI({})
    output$sentinel_download_ui      <- renderUI({})
    output$sentinel_select_ui        <- renderUI({})   # clear the image-selection dropdown
    shinyjs::enable("download_sentinel")               # re-enable button if it was disabled
  })
  
  observeEvent(input$imagery_source, {
    req(!is.null(input$imagery_source), nzchar(input$imagery_source))
    tryCatch(
      session$sendCustomMessage(
        "setSentinelTimeseriesVisible",
        list(visible = identical(input$imagery_source, "download_sentinel"))
      ),
      error = function(e) NULL
    )
    if (identical(input$imagery_source, "download_sentinel")) {
      tryCatch(
        updateTabsetPanel(session, "imagery_view_subtabs", selected = "imagery_timeseries_tab"),
        error = function(e) NULL
      )
      return(invisible(NULL))
    }
    tryCatch(
      updateTabsetPanel(session, "imagery_view_subtabs", selected = "imagery_map_tab"),
      error = function(e) NULL
    )
    shinyjs::delay(250, refit_maps_on_tab())
    if (!is.null(uploaded_sentinel_raster()) || !is.null(sentinel_search_results()) || !is.null(sentinel_result())) {
      remove_sentinel_data()
      reset_sampling_comparison_state()
      sync_sampling_after_covariate_change(notify = NULL)
      sentinel_result(NULL)
      sentinel_search_results(NULL)
      sentinel_status(NULL)
      sentinel_retrieval_used("single")
      sentinel_retrieval_meta(NULL)
      is_downloading_sentinel(FALSE)
      sentinel_progress_pct(0)
      sentinel_progress_detail("")
      sentinel_last_completed_at(NULL)
      sentinel_console_lines(character(0))
      output$clear_sentinel_ui         <- renderUI({})
      output$sentinel_band_selector_ui  <- renderUI({})
      output$sentinel_vi_calculator_ui  <- renderUI({})
      output$sentinel_vi_download_ui    <- renderUI({})
      output$sentinel_download_ui       <- renderUI({})
      output$sentinel_select_ui         <- renderUI({})
      shinyjs::enable("download_sentinel")
      showNotification("Sentinel layer cleared because imagery source changed.", type = "message")
    }
  }, ignoreInit = TRUE)
  
  # --- Elevation Download Logic ---
  observeEvent(input$download_elevation, {
    req(!is.null(digitized_features()), message = "Please define a boundary before retrieving elevation data.")
    abort_elevation(FALSE)
    
    progress <- shiny::Progress$new(session, min = 0, max = 1)
    progress$set(message = "Retrieving Elevation Data", detail = "Requesting AWS terrain tiles...", value = 0.1)
    
    aoi <- sf::st_transform(digitized_features(), 4326)
    elev_z <- elev_zoom_for_aoi(digitized_features(), preferred_z = 13L)
    elevation_download_zoom(elev_z)
    if (elev_z < 13L) {
      showNotification(
        paste0(
          "AOI size: using elevatr zoom z = ", elev_z,
          " instead of z = 13 to limit DEM memory on this server (~4× fewer pixels per step down)."
        ),
        type = "warning",
        duration = 9
      )
    }
    
    future({
      # Native grid from elevatr (no forced TARGET_CRS or 10 m resample here; harmonize at sampling).
      NED_raster <- elevatr::get_elev_raster(locations = aoi, z = elev_z, clip = "locations")
      raster_filepath <- tempfile(fileext = ".tif")
      raster::writeRaster(NED_raster, raster_filepath, overwrite = TRUE)
      return(list(tif_file = raster_filepath, raster_for_map = NED_raster))
    }, seed = TRUE) %...>% (function(result) {
      if (is.null(result) || isolate(abort_elevation())) {
        showNotification("Elevation retrieval was cancelled.", type="warning")
        return(NULL)
      }
      progress$set(detail = "Download complete!", value = 1)
      try(progress$close(), silent = TRUE)
      elevation_result(result)
      uploaded_elevation_raster(result$raster_for_map)
      notify_layer_ready("Elevation DEM", "download")
      set_layer_tip("elevation")
      shinyjs::delay(350, redraw_elevation_layers("DEM"))
      finalize_layer_on_map(redraw = NULL)
    }) %...!% (function(error) {
      try(progress$close(), silent = TRUE)
      err_msg <- promise_error_message(error)
      showNotification(paste("Error retrieving elevation data:", err_msg), type = "error", duration = 10)
    })
    
    return(NULL)
  })
  
  output$elevation_info_ui <- renderUI({
    r <- uploaded_elevation_raster()
    if (is.null(r)) {
      return(tags$p(class = "app-meta-text", "Load a DEM to see resolution details here."))
    }
    rs <- tryCatch(raster::res(r), error = function(e) c(NA_real_, NA_real_))
    is_ll <- tryCatch(raster::isLonLat(r), error = function(e) FALSE)
    res_line <- if (isTRUE(is_ll)) {
      ex <- tryCatch(raster::extent(r), error = function(e) NULL)
      lat_mid <- if (!is.null(ex)) mean(c(ex@ymin, ex@ymax)) else 0
      mx <- abs(rs[1]) * 111320 * cos(lat_mid * pi / 180)
      my <- abs(rs[2]) * 110540
      sprintf("Resolution ≈ %.0f × %.0f m.", mx, my)
    } else {
      sprintf("Resolution %.2f × %.2f map units.", abs(rs[1]), abs(rs[2]))
    }
    src <- if (identical(isolate(input$elevation_source), "download_elevation")) {
      ez <- suppressWarnings(as.integer(isolate(elevation_download_zoom())))
      if (length(ez) != 1L || is.na(ez)) ez <- 13L
      tagList(
        tags$p(
          class = "app-meta-text",
          sprintf(
            "Source: AWS Terrain Tiles (Mapzen-derived) via elevatr, tile zoom z = %s.",
            ez
          )
        ),
        tags$p(class = "app-meta-text", res_line)
      )
    } else {
      tagList(
        tags$p(class = "app-meta-text", "Source: uploaded elevation GeoTIFF."),
        tags$p(class = "app-meta-text", res_line)
      )
    }
    tags$div(class = "app-meta-block", src)
  })
  
  output$download_elevation_tif <- downloadHandler(
    filename = function() { "elevation_data.tif" },
    content = function(file) { req(!is.null(elevation_result()$tif_file)); file.copy(elevation_result()$tif_file, file) }
  )
  
  # --- Data Removal Functions ---
  remove_raster_group <- function(group_name) {
    leafletProxy("imagery_map") %>% clearGroup(group_name) %>% removeControl(paste0(group_name, "_legend"))
    leafletProxy("elevation_map") %>% clearGroup(group_name) %>% removeControl(paste0(group_name, "_legend"))
    leafletProxy("soil_map") %>% clearGroup(group_name) %>% removeControl(paste0(group_name, "_legend"))
    leafletProxy("other_map") %>% clearGroup(group_name) %>% removeControl(paste0(group_name, "_legend"))
    leafletProxy("sampling_auto_map") %>% clearGroup(group_name) %>% removeControl(paste0(group_name, "_legend"))
    leafletProxy("sampling_manual_map-map") %>% clearGroup(group_name) %>% removeControl(paste0(group_name, "_legend"))
  }
  
  remove_planet_data <- function() {
    remove_raster_group("Planet")
    uploaded_planet_raster(NULL)
    vi_rasters(list())
    removeUI(selector = "#planet_band_selector_ui > div", immediate = TRUE)
    removeUI(selector = "#vi_calculator_ui > div", immediate = TRUE)
    removeUI(selector = "#vi_download_ui > div", immediate = TRUE)
    tryCatch(clear_distribution_plot_caches(), error = function(e) invisible(NULL))
    release_geosampler_memory()
  }
  
  remove_sentinel_data <- function() {
    remove_raster_group("Sentinel")
    uploaded_sentinel_raster(NULL)
    sentinel_vi_rasters(list())
    removeUI(selector = "#sentinel_band_selector_ui > div", immediate = TRUE)
    removeUI(selector = "#sentinel_vi_calculator_ui > div", immediate = TRUE)
    removeUI(selector = "#sentinel_vi_download_ui > div", immediate = TRUE)
    tryCatch(clear_distribution_plot_caches(), error = function(e) invisible(NULL))
    release_geosampler_memory()
  }
  
  remove_ms_data <- function() {
    remove_raster_group("MS")
    uploaded_ms_raster(NULL)
    ms_vi_rasters(list())
    removeUI(selector = "#ms_band_selector_ui > div", immediate = TRUE)
    removeUI(selector = "#ms_vi_calculator_ui > div", immediate = TRUE)
    removeUI(selector = "#ms_vi_download_ui > div", immediate = TRUE)
    output$upload_tip_imagery <- renderText("")
  }
  
  remove_elevation_data <- function() {
    remove_raster_group("Elevation")
    uploaded_elevation_raster(NULL)
    elevation_aux_layers(list())
    output$upload_tip_elevation <- renderText("")
    tryCatch(update_elevation_layer_select(), error = function(e) invisible(NULL))
  }
  
  remove_soil_data <- function() {
    remove_raster_group("Soil")
    soil_layers(list())
    output$upload_tip_soil <- renderText("")
  }
  
  remove_other_data <- function(){
    remove_raster_group("Other")
    other_layers(list())
    output$upload_tip_other <- renderText("")
  }
  
  reset_sampling_ui_defaults <- function() {
    updateSelectInput(session, "sampling_method", selected = "automatic")
    updateSelectInput(session, "sample_type", selected = "Simple Random")
    updateNumericInput(session, "n_points", value = 10)
    updateNumericInput(session, "buffer_distance", value = 10)
    updateNumericInput(session, "random_seed", value = 123)
  }
  
  sync_sampling_after_covariate_change <- function(notify = NULL) {
    invalidate_pop_sample_summary()
    unlock_generate_sample_buttons()
    unlock_action_button("compare_sampling_methods")
    unlock_action_button("recommend_zones")
    all <- tryCatch(available_rasters(), error = function(e) list())
    if (!is.null(all) && length(all) > 0L) {
      nm <- names(all)
      cur <- isolate(input$sampling_covariate_layers)
      preset <- isolate(input$covariate_preset_choice) %||% "lean"
      if (identical(preset, "full")) {
        sel <- nm
      } else if ((is.null(cur) || length(cur) == 0L) && identical(preset, "lean")) {
        sel <- sampling_covariate_preset_layers(nm, "lean")
        if (length(sel) < 1L) sel <- nm[seq_len(min(6L, length(nm)))]
      } else {
        sel <- intersect(cur, nm)
        if (length(sel) == 0L) sel <- sampling_covariate_preset_layers(nm, preset)
        if (length(sel) == 0L) sel <- nm[seq_len(min(6L, length(nm)))]
      }
      sampling_covariate_layers_programmatic_update(TRUE)
      updateCheckboxGroupInput(
        session, "sampling_covariate_layers",
        choices = nm, selected = sel, inline = FALSE
      )
      sampling_covariate_layers_programmatic_update(FALSE)
    }
    invalidate_zone_recommendation()
    if (!is.null(notify) && nzchar(notify)) {
      showNotification(notify, type = "message", duration = 10)
    }
    invisible(NULL)
  }

  reset_sampling_comparison_state <- function() {
    invalidate_pop_sample_summary()
    sample_points(NULL)
    sampling_spread_pick_context(NULL)
    manual_points(NULL)
    zonal_cluster_summary(NULL)
    zonal_cluster_means(NULL)
    zonal_cluster_model(NULL)
    zonal_zone_raster(NULL)
    zonal_zone_count(NULL)
    comparison_results(NULL)
    invalidate_zone_recommendation()
    recommended_sample_type(NULL)
    recommended_n_points(NULL)
    recommended_buffer_distance(NULL)
    recommended_random_seed(NULL)
    recommended_grid_size_m(NULL)
    sampling_prefill_active(FALSE)
    clhs_similarity_zone(NULL)
    clhs_weak_gps_zone(NULL)
    clhs_similarity_threshold(NULL)
    clhs_similarity_polygon_count(NULL)
    clhs_weak_zone_area_ha(NULL)
    adaptive_recommendation_summary(NULL)
    adaptive_similarity_raster(NULL)
    reset_sampling_ui_defaults()
    leafletProxy("sampling_auto_map") %>%
      clearGroup("Sample Points") %>%
      clearGroup("Sampling Zones") %>%
      clearGroup("Adaptive Similarity Classes") %>%
      removeControl("SamplingZones_legend") %>%
      removeControl("AdaptiveSimilarity_legend") %>%
      removeControl("CLHSSimilarity_legend")
  }
  
  reset_to_clean_workflow_state <- function(notification_message = NULL) {
    remove_planet_data()
    remove_sentinel_data()
    remove_ms_data()
    remove_elevation_data()
    remove_soil_data()
    remove_other_data()
    
    planet_result(NULL)
    sentinel_result(NULL)
    elevation_result(NULL)
    
    planet_status(NULL)
    is_downloading_planet(FALSE)
    planet_progress_pct(0)
    planet_progress_detail("")
    
    sentinel_status(NULL)
    sentinel_search_results(NULL)
    sentinel_ndre_timeseries_df(NULL)
    sentinel_ndre_timeseries_meta(NULL)
    sentinel_search_active(FALSE)
    sentinel_retrieval_used("single")
    sentinel_retrieval_meta(NULL)
    sentinel_last_logged_status("")
    is_downloading_sentinel(FALSE)
    sentinel_progress_pct(0)
    sentinel_progress_detail("")
    
    vi_rasters(list())
    sentinel_vi_rasters(list())
    ms_vi_rasters(list())
    
    reset_sampling_comparison_state()
    
    output$planet_download_ui <- renderUI({})
    output$sentinel_download_ui <- renderUI({})
    output$clear_sentinel_ui <- renderUI({})
    output$sentinel_select_ui <- renderUI({})
    output$sentinel_band_selector_ui <- renderUI({})
    output$sentinel_vi_calculator_ui <- renderUI({})
    output$sentinel_vi_download_ui <- renderUI({})
    output$planet_band_selector_ui <- renderUI({})
    output$vi_calculator_ui <- renderUI({})
    output$vi_download_ui <- renderUI({})
    output$ms_band_selector_ui <- renderUI({})
    output$ms_vi_calculator_ui <- renderUI({})
    output$ms_vi_download_ui <- renderUI({})
    tryCatch(update_elevation_layer_select(), error = function(e) invisible(NULL))
    output$upload_tip_imagery <- renderText("")
    
    shinyjs::enable("download_sentinel")
    
    if (!is.null(notification_message) && nzchar(notification_message)) {
      showNotification(notification_message, type = "warning", duration = 7)
    }
  }
  
  # --- Harmonize Rasters Function (Fixed CRS comparison) ---
  harmonize_rasters <- function(rasters, ref_raster = NULL, analysis_crs = TARGET_CRS, resolution_scale = 1) {
    if (length(rasters) == 0) return(list())
    if (is.null(ref_raster)) {
      resolutions <- sapply(rasters, function(r) max(res(r)))
      master_idx <- which.max(resolutions)  # coarsest
      ref_raster <- rasters[[master_idx]]
    }
    # Build an explicit template from coarsest raster so all layers are forced
    # to the same CRS, extent, resolution, and origin.
    template <- raster::raster(
      xmn = raster::extent(ref_raster)@xmin,
      xmx = raster::extent(ref_raster)@xmax,
      ymn = raster::extent(ref_raster)@ymin,
      ymx = raster::extent(ref_raster)@ymax,
      crs = raster::crs(ref_raster)
    )
    rs0 <- raster::res(ref_raster)
    scl <- suppressWarnings(as.numeric(resolution_scale))
    if (is.na(scl) || scl < 1) scl <- 1
    raster::res(template) <- rs0 * scl

    fill_internal_na <- function(r, n_iter = 1L, max_cells = 400000L, max_na_frac = 0.40) {
      out <- r
      v0 <- raster::values(out)
      if (length(v0) == 0) return(out)
      na_frac <- sum(is.na(v0)) / length(v0)
      # Keep this lightweight: skip focal fill when raster is huge or mostly NA.
      if (length(v0) > max_cells || na_frac > max_na_frac) return(out)
      for (k in seq_len(n_iter)) {
        v <- raster::values(out)
        if (!any(is.na(v))) break
        sm <- raster::focal(
          out,
          w = matrix(1, 3, 3),
          fun = function(x, ...) stats::median(x, na.rm = TRUE),
          na.rm = FALSE,
          pad = TRUE,
          padValue = NA
        )
        sv <- raster::values(sm)
        v[is.na(v)] <- sv[is.na(v)]
        raster::values(out) <- v
      }
      out
    }

    harmonized <- lapply(rasters, function(r) {
      method <- if (raster::is.factor(r)) "ngb" else "bilinear"
      rr <- r
      if (!is.null(analysis_crs) && nzchar(analysis_crs)) {
        rr <- tryCatch(project_raster_bilinear(rr, crs = analysis_crs), error = function(e) rr)
      }
      # Always align directly to the coarsest template to avoid subtle grid drift.
      rr <- tryCatch(
        raster::projectRaster(rr, to = template, method = method),
        error = function(e) {
          rr2 <- if (raster::crs(rr, asText = TRUE) != raster::crs(template, asText = TRUE)) {
            raster::projectRaster(rr, crs = raster::crs(template, asText = TRUE), method = method)
          } else rr
          raster::resample(rr2, template, method = method)
        }
      )
      rr <- raster::crop(rr, raster::extent(template), snap = "near")
      rr <- fill_internal_na(rr, n_iter = 1L)
      rr
    })
    harmonized
  }

  harmonize_covariate_layers <- function(all_r, boundary_sf = NULL, analysis_crs = NULL, harmonize_scale = 1L) {
    if (length(all_r) == 0L) return(list())
    if (is.null(analysis_crs) || !nzchar(analysis_crs)) {
      analysis_crs <- tryCatch(analysis_crs_string(), error = function(e) TARGET_CRS)
    }
    cropped <- lapply(all_r, function(r) {
      if (is.null(r)) return(NULL)
      if (!is.null(boundary_sf) && nrow(boundary_sf) > 0) {
        tryCatch(
          strict_crop_mask_raster(r, boundary_sf, exclude_boundary_touch = TRUE),
          error = function(e) NULL
        )
      } else {
        r
      }
    })
    cropped <- cropped[!vapply(cropped, is.null, logical(1))]
    if (!length(cropped)) return(list())
    coarsest_idx <- which.max(vapply(cropped, function(r) max(raster::res(r)), numeric(1)))
    harmonize_rasters(
      cropped,
      ref_raster = cropped[[coarsest_idx]],
      analysis_crs = analysis_crs,
      resolution_scale = harmonize_scale
    )
  }

  # Covariate matrix for WSS zone recommend (uses same crop/harmonize path as comparison).
  build_cov_df_for_zone_wss <- function(all_r, boundary_sf, analysis_crs, harmonize_scale) {
    harmonized <- harmonize_covariate_layers(all_r, boundary_sf, analysis_crs, harmonize_scale)
    if (length(harmonized) == 0L) return(NULL)
    st <- stack(harmonized)
    df <- as.data.frame(st, xy = TRUE, na.rm = TRUE)
    cov_df <- df[, -c(1, 2), drop = FALSE]
    cov_df <- cov_df[, sapply(cov_df, is.numeric), drop = FALSE]
    cov_df <- cov_df[, vapply(cov_df, function(v) stats::sd(v, na.rm = TRUE) > 0, logical(1)), drop = FALSE]
    cov_df <- cov_df[stats::complete.cases(cov_df), , drop = FALSE]
    cov_df
  }
  
  # --- Available Rasters Logic ---
  available_rasters <- reactive({
    rasters <- list()
    
    # Elevation and derivatives
    if (!is.null(uploaded_elevation_raster())) {
      rasters[["Elevation_DEM"]] <- uploaded_elevation_raster()
    }
    if (length(elevation_aux_layers()) > 0) {
      for (name in names(elevation_aux_layers())) {
        rasters[[paste0("Elevation_", name)]] <- elevation_aux_layers()[[name]]
      }
    }
    
    # Soil
    if (length(soil_layers()) > 0) {
      for (name in names(soil_layers())) {
        rasters[[paste0("Soil_", name)]] <- soil_layers()[[name]]
      }
    }
    
    # Other
    if (length(other_layers()) > 0) {
      for (name in names(other_layers())) {
        rasters[[paste0("Other_", name)]] <- other_layers()[[name]]
      }
    }
    
    # Planet: VIs if calculated, else individual bands
    if (length(vi_rasters()) > 0) {
      for (name in names(vi_rasters())) {
        rasters[[paste0("Planet_VI_", name)]] <- vi_rasters()[[name]]
      }
    } else if (!is.null(uploaded_planet_raster())) {
      pr <- uploaded_planet_raster()
      band_names <- names(pr)
      for (i in 1:nlayers(pr)) {
        rasters[[paste0("Planet_Band_", band_names[i])]] <- pr[[i]]
      }
    }
    
    # Sentinel: VIs if calculated, else individual bands
    if (length(sentinel_vi_rasters()) > 0) {
      for (name in names(sentinel_vi_rasters())) {
        rasters[[paste0("Sentinel_VI_", name)]] <- sentinel_vi_rasters()[[name]]
      }
    } else if (!is.null(uploaded_sentinel_raster())) {
      sr <- uploaded_sentinel_raster()
      band_names <- names(sr)
      for (i in 1:nlayers(sr)) {
        rasters[[paste0("Sentinel_Band_", band_names[i])]] <- sr[[i]]
      }
    }
    
    # MS: VIs if calculated or uploaded, else individual bands
    if (length(ms_vi_rasters()) > 0) {
      for (name in names(ms_vi_rasters())) {
        rasters[[paste0("MS_VI_", name)]] <- ms_vi_rasters()[[name]]
      }
    } else if (!is.null(uploaded_ms_raster())) {
      msr <- uploaded_ms_raster()
      band_names <- names(msr)
      for (i in 1:nlayers(msr)) {
        rasters[[paste0("MS_Band_", band_names[i])]] <- msr[[i]]
      }
    }
    
    rasters
  })

  active_covariate_layer_names_r <- reactive({
    all <- available_rasters()
    nm <- names(all)
    if (!length(nm)) return(character(0))
    sel <- input$sampling_covariate_layers
    if (is.null(sel) || !length(sel)) return(nm)
    keep <- intersect(as.character(sel), nm)
    if (length(keep) < 1L) nm else keep
  })

  sampling_selected_rasters <- reactive({
    all <- available_rasters()
    if (is.null(all) || length(all) == 0L) return(all)
    keep <- active_covariate_layer_names_r()
    if (!length(keep)) return(all[0])
    all[keep]
  })

  output$compare_covariate_layers_ui <- renderUI({
    all <- available_rasters()
    if (is.null(all) || length(all) == 0L) {
      return(tags$p(class = "text-muted", style = "font-size:12px;", "Load raster variables first (Imagery, Elevation, etc.), then select covariates here."))
    }
    nm <- names(all)
    # Avoid a reactive dependency on input$sampling_covariate_layers: rebuilding this <details>
    # on every toggle collapses the panel; checkbox state still updates in place via Shiny.
    cur <- shiny::isolate(input$sampling_covariate_layers)
    locked_layers <- shiny::isolate(wss_zone_layers_at_lock())
    if (isTRUE(shiny::isolate(zones_wss_locked())) && !is.null(locked_layers) && length(locked_layers) > 0L) {
      sel <- intersect(locked_layers, nm)
      if (length(sel) == 0L) sel <- nm
    } else if (is.null(cur) || length(cur) == 0L) {
      preset <- shiny::isolate(input$covariate_preset_choice) %||% "lean"
      sel <- if (identical(preset, "full")) {
        nm
      } else {
        sampling_covariate_preset_layers(nm, preset)
      }
      if (length(sel) < 1L) sel <- nm[seq_len(min(6L, length(nm)))]
    } else {
      sel <- intersect(cur, nm)
      if (length(sel) == 0L) sel <- nm
    }
    tags$details(
      class = "covariate-layers-details",
      tags$summary("Covariate layers"),
      tags$div(
        class = "sampling-segment-body",
        style = "max-height:200px; overflow-y:auto;",
        tags$p(
          class = "text-muted",
          style = "font-size:10px; margin:0 0 6px 0;",
          length(sel), " of ", length(nm), " layer(s) selected. These feed comparison, sample generation, tables, and distribution plots."
        ),
        checkboxGroupInput("sampling_covariate_layers", label = NULL, choices = nm, selected = sel, inline = FALSE)
      )
    )
  })

  prepare_raster_for_display <- function(r, max_cells = NULL, for_leaflet_overlay = FALSE) {
    if (is.null(max_cells)) max_cells <- deployment_safety_params()$display_max_cells
    if (is.null(r)) return(NULL)
    out <- r
    nc <- tryCatch(raster::ncell(out), error = function(e) NA_real_)
    if (is.finite(nc) && nc > max_cells) {
      fact <- max(2L, as.integer(ceiling(sqrt(nc / max_cells))))
      out <- tryCatch(raster::aggregate(out, fact = fact, fun = mean, na.rm = TRUE), error = function(e) out)
    }
    if (isTRUE(for_leaflet_overlay) && !raster::is.factor(out)) {
      v <- tryCatch(raster::values(out), error = function(e) NULL)
      if (is.numeric(v) && length(v) > 0 && any(is.na(v))) {
        na_frac <- sum(is.na(v)) / length(v)
        if (na_frac > 0 && na_frac < 0.08 && length(v) <= max_cells * 3L) {
          sm <- tryCatch(
            raster::focal(
              out,
              w = matrix(1, 3, 3),
              fun = function(x, ...) stats::median(x, na.rm = TRUE),
              pad = TRUE,
              na.rm = FALSE,
              padValue = NA
            ),
            error = function(e) NULL
          )
          if (!is.null(sm)) {
            sv <- raster::values(sm)
            vv <- v
            ii <- is.na(vv)
            vv[ii] <- sv[ii]
            rout <- out
            raster::values(rout) <- vv
            out <- rout
          }
        }
      }
      wc <- tryCatch(raster::crs("EPSG:3857"), error = function(e) NULL)
      if (!is.null(wc)) {
        out <- tryCatch(raster::projectRaster(out, crs = wc, method = "bilinear"), error = function(e) out)
      }
    }
    out
  }

  # Leaflet colorNumeric needs a finite min < max; empty/all-NA/flat rasters otherwise error on shinyapps.
  leaflet_numeric_palette <- function(r) {
    if (is.null(r)) return(NULL)
    valid_vals <- tryCatch(raster::values(r), error = function(e) NULL)
    if (is.null(valid_vals)) return(NULL)
    valid_vals <- valid_vals[is.finite(valid_vals)]
    if (!length(valid_vals)) return(NULL)
    val_range <- range(valid_vals, na.rm = TRUE)
    if (!all(is.finite(val_range))) return(NULL)
    if (val_range[1] == val_range[2]) {
      pad <- max(abs(val_range[1]) * 0.01, 1e-6)
      val_range <- c(val_range[1] - pad, val_range[2] + pad)
    }
    list(
      val_range = val_range,
      pal = colorNumeric(viridis(256), domain = val_range, na.color = "transparent")
    )
  }

  # Discrete rasters (zones, adaptive classes): downsample with majority, fill tiny NA holes, then nearest-neighbour to Web Mercator for smooth Leaflet edges without blending class colours.
  prepare_discrete_raster_for_leaflet <- function(r, max_cells = 700000L) {
    if (is.null(r)) return(NULL)
    out <- r
    nc <- tryCatch(raster::ncell(out), error = function(e) NA_real_)
    if (is.finite(nc) && nc > max_cells) {
      fact <- max(2L, as.integer(ceiling(sqrt(nc / max_cells))))
      out <- tryCatch(
        raster::aggregate(
          out,
          fact = fact,
          fun = function(x, ...) {
            v <- stats::na.omit(as.numeric(x))
            if (!length(v)) return(NA_real_)
            uv <- unique(v)
            if (length(uv) == 1L) return(uv[1])
            tb <- tabulate(match(v, uv))
            uv[which.max(tb)]
          }
        ),
        error = function(e) out
      )
    }
    v <- tryCatch(raster::values(out), error = function(e) NULL)
    if (!is.null(v) && any(is.na(v))) {
      na_frac <- sum(is.na(v)) / length(v)
      if (is.finite(na_frac) && na_frac > 0 && na_frac < 0.15 && length(v) <= max_cells * 2L) {
        sm <- tryCatch(
          raster::focal(out, w = matrix(1, 3, 3), fun = raster::modal, na.rm = TRUE, pad = TRUE, padValue = NA),
          error = function(e) NULL
        )
        if (!is.null(sm)) {
          sv <- raster::values(sm)
          vv <- v
          ii <- is.na(vv)
          vv[ii] <- sv[ii]
          rout <- out
          raster::values(rout) <- vv
          out <- rout
        }
      }
    }
    wc <- tryCatch(raster::crs("EPSG:3857"), error = function(e) NULL)
    if (!is.null(wc)) {
      out <- tryCatch(raster::projectRaster(out, crs = wc, method = "ngb"), error = function(e) out)
    }
    out
  }

  layer_summary_stats_masked <- function(r, boundary_sf, max_sample = 150000L) {
    if (is.null(r) || is.null(boundary_sf) || nrow(boundary_sf) == 0) return(NULL)
    rr <- tryCatch({
      b2 <- sf::st_make_valid(boundary_sf)
      b2 <- sf::st_transform(b2, raster::crs(r))
      bsp <- as(b2, "Spatial")
      raster::mask(raster::crop(r, bsp), bsp)
    }, error = function(e) r)
    v <- raster::values(rr)
    v <- as.numeric(v)
    v <- v[is.finite(v)]
    if (!length(v)) return(NULL)
    if (length(v) > max_sample) v <- sample(v, max_sample)
    c(Min = min(v), Max = max(v), Mean = mean(v), Median = stats::median(v))
  }

  layer_summary_stats_unmasked <- function(r, max_sample = 150000L) {
    if (is.null(r)) return(NULL)
    v <- tryCatch(as.numeric(raster::values(r)), error = function(e) NULL)
    if (is.null(v)) return(NULL)
    v <- v[is.finite(v)]
    if (!length(v)) return(NULL)
    if (length(v) > max_sample) v <- sample(v, max_sample)
    c(Min = min(v), Max = max(v), Mean = mean(v), Median = stats::median(v))
  }

  pop_sample_summary_error <- reactiveVal(NULL)
  covariate_data_revision <- reactiveVal(0L)

  invalidate_pop_sample_summary <- function() {
    pop_sample_summary_error(NULL)
    covariate_data_revision(isolate(covariate_data_revision()) + 1L)
    invisible(NULL)
  }

  observeEvent(input$compute_summary_distributions, {
    pts <- current_app_sample_points()
    if (is.null(pts) || nrow(pts) < 1L) {
      showNotification("Generate or save sample points first.", type = "warning", duration = 6)
      return(invisible(NULL))
    }
    lock_action_button("compute_summary_distributions")
    rebuild_summary_violin_plots()
  }, ignoreInit = TRUE)

  observeEvent(input$apply_covariate_preset, {
    all <- available_rasters()
    req(length(all) > 0L)
    nm <- names(all)
    preset <- isolate(input$covariate_preset_choice)
    if (is.null(preset) || !nzchar(preset)) preset <- "lean"
    sel <- sampling_covariate_preset_layers(nm, preset)
    if (length(sel) < 1L) sel <- nm[1L]
    sampling_covariate_layers_programmatic_update(TRUE)
    updateCheckboxGroupInput(session, "sampling_covariate_layers", choices = nm, selected = sel, inline = FALSE)
    sampling_covariate_layers_programmatic_update(FALSE)
    comparison_results(NULL)
    sampling_prefill_active(FALSE)
    invalidate_pop_sample_summary()
    clear_distribution_plot_caches()
    unlock_action_button("compare_sampling_methods")
    unlock_action_button("recommend_zones")
    showNotification(
      paste0(
        "Covariate preset applied: ", preset, " (", length(sel), " layer(s)). ",
        "Re-run Technique comparison if you changed layers after a prior run."
      ),
      type = "message",
      duration = 7
    )
  })

  observeEvent(input$sampling_covariate_layers, {
    all <- available_rasters()
    if (!is.null(all) && length(all) > 0L) {
      nm <- names(all)
      cur <- input$sampling_covariate_layers
      if (is.null(cur) || length(cur) == 0L) {
        locked_layers <- isolate(wss_zone_layers_at_lock())
        if (isTRUE(isolate(zones_wss_locked())) && !is.null(locked_layers) && length(locked_layers) > 0L) {
          keep <- intersect(locked_layers, nm)
          if (length(keep) < 1L) keep <- nm
          updateCheckboxGroupInput(session, "sampling_covariate_layers", choices = nm, selected = keep, inline = FALSE)
          return(invisible(NULL))
        }
        updateCheckboxGroupInput(session, "sampling_covariate_layers", choices = nm, selected = nm[1L], inline = FALSE)
        showNotification("At least one covariate layer must stay selected; restored the first layer.", type = "warning", duration = 4)
      }
    }
    invalidate_zone_recommendation_if_context_changed()
    unlock_action_button("compare_sampling_methods")
    if (!isTRUE(isolate(sampling_covariate_layers_programmatic_update()))) {
      comparison_results(NULL)
      sampling_prefill_active(FALSE)
      invalidate_pop_sample_summary()
      clear_distribution_plot_caches()
    }
  }, ignoreInit = TRUE)

  sampling_covariate_layers_programmatic_update <- reactiveVal(FALSE)

  # Also depend on sampling_method so switching to automatic with no auto points can clear stale caches.
  observeEvent(
    list(sample_points(), input$sampling_method),
    {
      sm <- input$sampling_method
      sp <- sample_points()
      has_auto <- !is.null(sp) && nrow(sp) > 0L
      if (has_auto) {
        invalidate_pop_sample_summary()
        tryCatch(refresh_app_sample_extraction(), error = function(e) invisible(NULL))
        if (isTRUE(field_compare_historical_ready())) {
          tryCatch(
            refresh_field_compare_map_and_values(notify = FALSE, progress_msg = "Updating field comparison values..."),
            error = function(e) invisible(NULL)
          )
        }
      } else if (identical(sm, "automatic") && !isTRUE(field_compare_historical_ready())) {
        # Only drop app-side extraction when automatic points are gone; manual mode uses manual_points().
        app_compare_values_df(NULL)
      }
      # Empty automatic points must not wipe distribution ggplots while the user is in manual sampling:
      # sample_points() is often NULL then even though manual_points() is populated.
      if (identical(sm, "automatic") && (is.null(sp) || nrow(sp) < 1L)) {
        clear_distribution_plot_caches()
        if (isTRUE(field_compare_historical_ready())) {
          tryCatch(refresh_field_compare_map_and_values(notify = FALSE), error = function(e) invisible(NULL))
        }
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(
    list(manual_points(), input$sampling_method),
    {
      sm <- input$sampling_method
      mp <- manual_points()
      has_manual <- !is.null(mp) && nrow(mp) > 0L
      if (has_manual) {
        invalidate_pop_sample_summary()
        tryCatch(refresh_app_sample_extraction(), error = function(e) invisible(NULL))
        if (isTRUE(field_compare_historical_ready())) {
          tryCatch(
            refresh_field_compare_map_and_values(notify = FALSE, progress_msg = "Updating field comparison values..."),
            error = function(e) invisible(NULL)
          )
        }
      } else if (identical(sm, "manual") && !isTRUE(field_compare_historical_ready())) {
        app_compare_values_df(NULL)
      }
      if (identical(sm, "manual") && (is.null(mp) || nrow(mp) < 1L)) {
        clear_distribution_plot_caches()
        if (isTRUE(field_compare_historical_ready())) {
          tryCatch(refresh_field_compare_map_and_values(notify = FALSE), error = function(e) invisible(NULL))
        }
      }
    },
    ignoreInit = TRUE
  )

  pop_vs_sample_summary_df <- reactive({
    req(identical(input$summary_subtabs, "summary_table_tab"))
    covariate_data_revision()
    input$sampling_method
    input$sampling_covariate_layers
    tryCatch({
    pop_sample_summary_error(NULL)
    pts <- current_app_sample_points()
    if (is.null(pts) || nrow(pts) < 1L) return(NULL)
    all_r <- sampling_selected_rasters()
    if (is.null(all_r) || length(all_r) < 1L) {
      all_r <- available_rasters()
    }
    if (is.null(all_r) || length(all_r) < 1L) return(NULL)

    sp <- deployment_safety_params()
    boundary <- digitized_features()
    bd <- boundary
    use_boundary <- !is.null(boundary)

    if (isTRUE(use_boundary) && suppressWarnings(as.numeric(input$buffer_distance)) > 0) {
      buffer_ok <- FALSE
      tryCatch({
        b0 <- tryCatch(sf::st_make_valid(boundary), error = function(e) boundary)
        metric_crs <- local_metric_crs_from_sf(b0)
        b_metric <- sf::st_transform(b0, metric_crs)
        b_metric <- tryCatch(sf::st_make_valid(b_metric), error = function(e) b_metric)
        b_metric <- b_metric[!sf::st_is_empty(b_metric), , drop = FALSE]
        if (nrow(b_metric) > 0) {
          b_buf <- suppressWarnings(sf::st_buffer(b_metric, dist = -as.numeric(input$buffer_distance)))
          b_buf <- tryCatch(sf::st_make_valid(b_buf), error = function(e) b_buf)
          b_buf <- b_buf[!sf::st_is_empty(b_buf), , drop = FALSE]
          if (nrow(b_buf) > 0 && !all(as.numeric(sf::st_area(b_buf)) <= 0, na.rm = TRUE)) {
            bd <- sf::st_transform(b_buf, sf::st_crs(boundary))
            buffer_ok <- TRUE
          }
        }
      }, error = function(e) invisible(NULL))
      if (!isTRUE(buffer_ok)) bd <- boundary
    }

    harmonized <- harmonize_covariate_layers(
      all_r,
      boundary_sf = bd,
      analysis_crs = analysis_crs_string(),
      harmonize_scale = sp$harmonize_scale
    )
    if (length(harmonized) == 0L) return(NULL)
    kept_names <- names(harmonized)
    combined_stack <- raster::stack(harmonized)
    names(combined_stack) <- gsub("[ :]", "_", kept_names)

    extracted <- tryCatch(
      extract_covariate_values_at_points(
        pts,
        all_r,
        boundary_sf = if (isTRUE(use_boundary)) bd else NULL,
        buffer_m = 0,
        analysis_crs = analysis_crs_string(),
        harmonize_scale = sp$harmonize_scale
      ),
      error = function(e) NULL
    )
    if (is.null(extracted) || ncol(extracted) < 1L) return(NULL)

    rows <- vector("list", length(kept_names))
    for (i in seq_along(kept_names)) {
      orig_nm <- kept_names[i]
      scol <- gsub("[ :]", "_", orig_nm)
      col_nm <- if (orig_nm %in% names(extracted)) {
        orig_nm
      } else if (scol %in% names(extracted)) {
        scol
      } else {
        NA_character_
      }
      if (is.na(col_nm)) next
      sv <- suppressWarnings(as.numeric(extracted[[col_nm]]))
      sv <- sv[is.finite(sv)]
      if (length(sv) < 1L) next
      rl <- harmonized[[orig_nm]]
      if (is.null(rl)) {
        rl <- tryCatch(combined_stack[[i]], error = function(e) NULL)
      }
      if (is.null(rl)) next
      pop_max <- suppressWarnings(as.integer(sp$pop_cap))
      if (length(pop_max) != 1L || is.na(pop_max) || !is.finite(pop_max) || pop_max < 1000L) {
        pop_max <- 150000L
      }
      pop <- if (!is.null(bd) && nrow(bd) > 0) {
        tryCatch(layer_summary_stats_masked(rl, bd, max_sample = pop_max), error = function(e) NULL)
      } else {
        layer_summary_stats_unmasked(rl, max_sample = pop_max)
      }
      if (is.null(pop)) next
      samp <- c(Min = min(sv), Max = max(sv), Mean = mean(sv), Median = stats::median(sv))
      min_p <- unname(pop[["Min"]]); min_s <- unname(samp[["Min"]])
      max_p <- unname(pop[["Max"]]); max_s <- unname(samp[["Max"]])
      mean_p <- unname(pop[["Mean"]])
      mean_s <- unname(samp[["Mean"]])
      med_p <- unname(pop[["Median"]])
      med_s <- unname(samp[["Median"]])
      rows[[i]] <- build_pop_sample_balance_row(
        orig_nm,
        min_p, min_s, max_p, max_s,
        mean_p, mean_s, med_p, med_s
      )
    }
    rows <- Filter(Negate(is.null), rows)
    if (!length(rows)) return(NULL)
    out <- dplyr::bind_rows(rows)
    if (is.null(out) || nrow(out) == 0L) NULL else out
    }, error = function(e) {
      pop_sample_summary_error(conditionMessage(e))
      handle_app_error(e, context = "Population vs sample table", notify_user = FALSE)
      NULL
    })
  })

  output$pop_sample_summary_status_ui <- renderUI({
    if (!identical(input$summary_subtabs, "summary_table_tab")) return(NULL)
    pts <- current_app_sample_points()
    if (is.null(pts) || nrow(pts) < 1L) {
      return(tags$div(
        class = "text-muted",
        "Generate or save sample points on the ", tags$strong("Sampling"), " sub-tab to fill this table."
      ))
    }
    all_r <- available_rasters()
    if (is.null(all_r) || length(all_r) < 1L) {
      return(tags$div(
        class = "text-muted",
        "Load variables (imagery VIs, elevation, etc.) on the ", tags$strong("Variables"), " tab first."
      ))
    }
    d <- pop_vs_sample_summary_df()
    if (is.null(d) || nrow(d) == 0L) {
      err <- isolate(pop_sample_summary_error())
      return(tags$div(
        class = "text-muted",
        if (nzchar(err %||% "")) {
          paste0("Could not build table: ", err)
        } else {
          "No rows yet — select covariate layers on Technique comparison and confirm points fall inside the boundary."
        }
      ))
    }
    tags$div(
      class = "text-muted",
      style = "margin-bottom:8px; font-size:13px;",
      paste0(nrow(d), " layer(s) — population stats from raster cells in the AOI (subsampled); sample stats from values at the ", nrow(pts), " current point(s).")
    )
  })

  output$pop_sample_summary_table <- DT::renderDataTable({
    req(identical(input$summary_subtabs, "summary_table_tab"))
    pts <- current_app_sample_points()
    if (is.null(pts) || nrow(pts) < 1L) {
      return(DT::datatable(
        data.frame(Message = "Generate or save sample points on the Sampling sub-tab first."),
        rownames = FALSE,
        options = list(dom = "t", ordering = FALSE),
        class = "stripe hover"
      ))
    }
    d <- pop_vs_sample_summary_df()
    if (is.null(d) || nrow(d) == 0L) {
      err <- isolate(pop_sample_summary_error())
      msg <- if (nzchar(err %||% "")) {
        paste0("Table not available: ", err)
      } else {
        "No summary rows — check covariate checkboxes and boundary."
      }
      return(DT::datatable(
        data.frame(Message = msg),
        rownames = FALSE,
        options = list(dom = "t", ordering = FALSE),
        class = "stripe hover"
      ))
    }
    colnms <- c(
      "Layer",
      "Min (population)", "Min (sample)", "% diff min",
      "Max (population)", "Max (sample)", "% diff max",
      "Mean (population)", "Mean (sample)", "% diff mean",
      "Median (population)", "Median (sample)", "% diff median"
    )
    d2 <- d
    colnames(d2) <- colnms
    tryCatch({
      dt <- DT::datatable(
        d2,
        rownames = FALSE,
        options = list(dom = "ft", pageLength = 20, scrollX = TRUE, ordering = TRUE, deferRender = TRUE),
        class = "stripe hover compact nowrap"
      ) %>%
        DT::formatRound(columns = c(2, 3, 5, 6, 8, 9, 11, 12), digits = POP_SAMPLE_STAT_DIGITS) %>%
        DT::formatRound(columns = c(4, 7, 10, 13), digits = POP_SAMPLE_PCT_DIGITS)
      dt <- DT::formatStyle(dt, columns = c(2, 5, 8, 11), backgroundColor = "#e6f2fc", fontWeight = "600")
      dt <- DT::formatStyle(dt, columns = c(3, 6, 9, 12), backgroundColor = "#e8f7ec", fontWeight = "600")
      dt <- DT::formatStyle(dt, columns = c(4, 7, 10, 13), backgroundColor = "#fff4e6", fontWeight = "600")
      dt
    }, error = function(e) {
      DT::datatable(
        data.frame(Message = paste0("Could not render table: ", conditionMessage(e))),
        rownames = FALSE,
        options = list(dom = "t", ordering = FALSE),
        class = "stripe hover"
      )
    })
  })
  outputOptions(output, "pop_sample_summary_table", suspendWhenHidden = FALSE)
  outputOptions(output, "pop_sample_summary_status_ui", suspendWhenHidden = FALSE)

  output$summary_distribution_status_ui <- renderUI({
    gen <- summary_distribution_gen()
    gen_banner <- distribution_gen_status_ui(gen)
    pts <- current_app_sample_points()
    if (is.null(pts) || nrow(pts) < 1L) {
      return(tagList(
        gen_banner,
        tags$div(class = "text-muted", "Generate or save sample points on the ", strong("Sampling"), " sub-tab first.")
      ))
    }
    d <- app_compare_values_df()
    if (is.null(d) || nrow(d) < 1L) {
      return(tagList(
        gen_banner,
        tags$div(
          class = "text-muted",
          paste0(
            "No extracted values yet for ", nrow(pts), " point(s). ",
            "Load raster variables, then regenerate sample points or open this Distribution tab to refresh."
          )
        )
      ))
    }
    d_app <- d[d$Source == "App-generated", , drop = FALSE]
    tagList(
      gen_banner,
      tags$p(
        class = "text-muted",
        style = "margin-bottom:8px;",
        paste0(
          length(unique(d_app$Layer)), " layer(s); population cloud + ",
          nrow(d_app), " sample value(s). Open this tab to refresh plots after generating points."
        )
      )
    )
  })

  normalize_uploaded_sample_points <- function(sf_obj) {
    if (is.null(sf_obj) || nrow(sf_obj) == 0L) return(NULL)
    sf_obj <- tryCatch(sf::st_make_valid(sf_obj), error = function(e) sf_obj)
    if (is.na(sf::st_crs(sf_obj))) {
      sf::st_crs(sf_obj) <- 4326
    }
    sf_obj <- suppressWarnings(sf::st_zm(sf_obj, drop = TRUE, what = "ZM"))
    gt <- sf::st_geometry_type(sf_obj, by_geometry = TRUE)
    keep <- gt %in% c("POINT", "MULTIPOINT")
    if (!any(keep)) {
      stop(
        "Upload must contain point features (POINT or MULTIPOINT). ",
        "Use the same vector formats as boundary upload, but the layer must be sample points—not polygons or lines."
      )
    }
    pts <- sf_obj[keep, , drop = FALSE]
    pts <- tryCatch(sf::st_cast(pts, "POINT", warn = FALSE), error = function(e) pts)
    pts <- sf::st_transform(pts, 4326)
    pts$hist_id <- seq_len(nrow(pts))
    pts
  }

  field_compare_point_ids <- function(pts, bad_idx) {
    if ("hist_id" %in% names(pts)) return(pts$hist_id[bad_idx])
    if ("ID" %in% names(pts)) return(pts$ID[bad_idx])
    bad_idx
  }

  validate_field_compare_points <- function(pts, boundary_sf, all_r, point_label = "point") {
    if (is.null(pts) || nrow(pts) < 1L) {
      stop("No sample points to validate.", call. = FALSE)
    }
    if (is.null(boundary_sf) || nrow(boundary_sf) < 1L) {
      stop("Digitize or upload a field boundary before uploading field sample points.", call. = FALSE)
    }
    if (length(all_r) < 1L) {
      stop("Load covariate raster layers before comparing field sample points.", call. = FALSE)
    }
    b <- tryCatch(sf::st_make_valid(boundary_sf), error = function(e) boundary_sf)
    pts_chk <- sf::st_transform(pts, sf::st_crs(b))
    b_union <- tryCatch(sf::st_union(b), error = function(e) b)
    inside <- lengths(sf::st_within(pts_chk, b_union)) > 0L
    if (!all(inside)) {
      bad <- which(!inside)
      ids <- field_compare_point_ids(pts, bad)
      stop(
        paste0(
          length(bad), " ", point_label, if (length(bad) == 1L) " lies" else " lie",
          " outside the field boundary (e.g. ",
          paste(head(ids, 6), collapse = ", "),
          if (length(bad) > 6L) ", …" else "",
          "). Check CRS and coordinates."
        ),
        call. = FALSE
      )
    }
    sp <- deployment_safety_params()
    wide <- extract_covariate_values_at_points(
      pts,
      all_r,
      boundary_sf = boundary_sf,
      buffer_m = 0,
      analysis_crs = analysis_crs_string(),
      harmonize_scale = sp$harmonize_scale
    )
    if (is.null(wide) || nrow(wide) < 1L) {
      stop(
        paste0(
          "Could not read raster values at ", point_label, " locations. ",
          "Points must overlap loaded covariate layers inside the boundary."
        ),
        call. = FALSE
      )
    }
    if (nrow(wide) != nrow(pts)) {
      stop(paste0("Raster extraction failed for some ", point_label, " points."), call. = FALSE)
    }
    num_cols <- vapply(wide, is.numeric, logical(1))
    if (!any(num_cols)) {
      stop(paste0("No numeric covariate values extracted for ", point_label, " points."), call. = FALSE)
    }
    mat <- as.matrix(wide[, num_cols, drop = FALSE])
    row_ok <- apply(mat, 1, function(row) any(is.finite(row)))
    if (!all(row_ok)) {
      bad <- which(!row_ok)
      ids <- field_compare_point_ids(pts, bad)
      stop(
        paste0(
          length(bad), " ", point_label, if (length(bad) == 1L) " has" else " have",
          " no raster data at its coordinates (not aligned with loaded layers; e.g. ",
          paste(head(ids, 6), collapse = ", "),
          if (length(bad) > 6L) ", …" else "",
          "). Verify rasters cover the field and CRS matches the boundary."
        ),
        call. = FALSE
      )
    }
    invisible(wide)
  }

  extract_covariate_values_at_points <- function(pts, all_r, boundary_sf = NULL, buffer_m = 0,
                                                 analysis_crs = TARGET_CRS, harmonize_scale = 1) {
    if (is.null(pts) || nrow(pts) < 1L || length(all_r) < 1L) return(NULL)
    kept_names <- names(all_r)
    bd <- boundary_sf
    if (!is.null(bd) && is.finite(suppressWarnings(as.numeric(buffer_m))) && as.numeric(buffer_m) > 0) {
      tryCatch({
        b0 <- sf::st_make_valid(bd)
        metric_crs <- local_metric_crs_from_sf(b0)
        b_metric <- sf::st_transform(b0, metric_crs)
        b_buf <- suppressWarnings(sf::st_buffer(b_metric, dist = -as.numeric(buffer_m)))
        b_buf <- b_buf[!sf::st_is_empty(b_buf), , drop = FALSE]
        if (nrow(b_buf) > 0) bd <- sf::st_transform(b_buf, sf::st_crs(bd))
      }, error = function(e) invisible(NULL))
    }
    harmonized <- harmonize_covariate_layers(
      all_r,
      boundary_sf = bd,
      analysis_crs = analysis_crs,
      harmonize_scale = harmonize_scale
    )
    if (!length(harmonized)) return(NULL)
    combined_stack <- raster::stack(harmonized)
    sanitized_names <- gsub("[ :]", "_", kept_names)
    names(combined_stack) <- sanitized_names
    pts_work <- sf::st_transform(pts, raster::crs(combined_stack))
    extracted <- tryCatch(
      raster::extract(combined_stack, pts_work, df = TRUE, na.rm = FALSE),
      error = function(e) NULL
    )
    if (is.null(extracted) || ncol(extracted) < 2L) return(NULL)
    vals <- as.data.frame(extracted[, -1, drop = FALSE], stringsAsFactors = FALSE)
    mapped <- kept_names[match(names(vals), sanitized_names)]
    names(vals) <- ifelse(is.na(mapped), names(vals), mapped)
    vals
  }

  compare_values_wide_to_long <- function(wide_df, source_label) {
    if (is.null(wide_df) || nrow(wide_df) < 1L) return(NULL)
    rows <- list()
    for (lyr in names(wide_df)) {
      v <- suppressWarnings(as.numeric(wide_df[[lyr]]))
      v <- v[is.finite(v)]
      if (length(v) < 1L) next
      rows[[length(rows) + 1L]] <- data.frame(
        Layer = lyr, Source = source_label, Value = v, stringsAsFactors = FALSE
      )
    }
    if (!length(rows)) return(NULL)
    dplyr::bind_rows(rows)
  }

  field_compare_historical_ready <- reactive({
    hp <- historical_sample_points()
    !is.null(hp) && nrow(hp) > 0L
  })
  output$field_compare_historical_ready <- reactive({
    field_compare_historical_ready()
  })
  outputOptions(output, "field_compare_historical_ready", suspendWhenHidden = FALSE)

  app_recommended_sample_n <- function() {
    rn <- recommended_n_points()
    if (!is.null(rn) && length(rn) == 1L && is.finite(rn) && rn >= 1L) {
      return(as.integer(rn))
    }
    cmp <- comparison_results()
    if (!is.null(cmp) && !is.null(cmp$n_points) && is.finite(cmp$n_points) && cmp$n_points >= 1L) {
      return(as.integer(cmp$n_points))
    }
    sm <- isolate(input$sampling_method)
    pts <- if (identical(sm, "manual")) manual_points() else sample_points()
    if (!is.null(pts) && inherits(pts, "sf") && nrow(pts) > 0L) {
      return(as.integer(nrow(pts)))
    }
    np <- suppressWarnings(as.integer(isolate(input$n_points)))
    if (is.finite(np) && np >= 1L) np else NULL
  }

  cost_comparison_state <- reactive({
    cc <- compute_cost_comparison(
      input$cost_prior_samples,
      input$cost_app_samples,
      input$cost_per_sample
    )
    list(
      cc = cc,
      currency = resolve_cost_currency_label(
        input$cost_currency_preset,
        input$cost_currency_custom
      )
    )
  })

  observeEvent(comparison_results(), {
    cmp <- comparison_results()
    if (is.null(cmp) || is.null(cmp$n_points)) return(invisible(NULL))
    updateNumericInput(session, "cost_app_samples", value = as.integer(cmp$n_points))
  }, ignoreInit = TRUE)

  observeEvent(recommended_n_points(), {
    rn <- recommended_n_points()
    if (is.null(rn) || !is.finite(rn) || rn < 1L) return(invisible(NULL))
    updateNumericInput(session, "cost_app_samples", value = as.integer(rn))
  }, ignoreInit = TRUE)

  output$cost_app_recommendation_ui <- renderUI({
    rn <- app_recommended_sample_n()
    cmp <- comparison_results()
    method_lbl <- if (!is.null(cmp) && !is.null(cmp$winner) && nzchar(cmp$winner)) {
      paste0(" (", cmp$winner, " from technique comparison)")
    } else {
      ""
    }
    if (is.null(rn)) {
      return(tags$p(
        class = "text-muted",
        style = "font-size:12px; margin:0 0 8px 0;",
        "Run ", strong("Technique comparison"), " or ", strong("Generate sample points"), " to pre-fill the app sample count."
      ))
    }
    tags$p(
      class = "text-muted",
      style = "font-size:12px; margin:0 0 8px 0; padding:8px 10px; background:#f0f7ff; border-radius:8px; border:1px solid #cfe6ff;",
      "Current app recommendation: ",
      tags$strong(rn),
      " sample",
      if (rn == 1L) "" else "s",
      method_lbl,
      ". Edit ", strong("App-recommended sample count"), " below if your plan differs."
    )
  })

  output$cost_prior_hint <- renderText({
    hp <- historical_sample_points()
    if (!is.null(hp) && nrow(hp) > 0L) {
      return(sprintf(
        "Tip: %d historical points uploaded — often matches your prior sample count.",
        nrow(hp)
      ))
    }
    ""
  })

  output$cost_comparison_plot <- renderPlot({
    st <- cost_comparison_state()
    cc <- st$cc
    if (!isTRUE(cc$ok)) {
      grid::grid.newpage()
      grid::grid.text(
        cc$reason %||% "Enter valid cost inputs in the sidebar.",
        x = 0.5, y = 0.5, gp = grid::gpar(col = "#4a5f78", fontsize = 12)
      )
      return(invisible(NULL))
    }
    p <- build_cost_comparison_plot(cc, st$currency)
    if (is.null(p)) {
      grid::grid.newpage()
      return(invisible(NULL))
    }
    print(p)
  }, bg = "white")

  output$cost_comparison_summary_ui <- renderUI({
    st <- cost_comparison_state()
    cc <- st$cc
    cur <- st$currency
    if (!isTRUE(cc$ok)) {
      return(tags$p(class = "text-muted", style = "margin-top:12px;", cc$reason))
    }
    fmt <- function(x) format(round(x, 2), big.mark = ",", nsmall = 2, trim = TRUE)
    children <- list(
      tags$p(
        style = "margin-top:14px; font-size:14px; color:#1f2d3d;",
        tags$strong("Prior / grid: "),
        sprintf(
          "%s samples \u00d7 %s = %s %s total",
          cc$prior_n, fmt(cc$cost_per_sample), fmt(cc$prior_total), cur
        )
      ),
      tags$p(
        style = "margin-top:6px; font-size:14px; color:#1f2d3d;",
        tags$strong("App design: "),
        sprintf(
          "%s samples \u00d7 %s = %s %s total",
          cc$app_n, fmt(cc$cost_per_sample), fmt(cc$app_total), cur
        )
      )
    )
    sp <- cc$savings_pct
    if (is.finite(sp) && sp > 0.05) {
      children[[length(children) + 1L]] <- tags$p(
        class = "cost-comparison-summary-line",
        style = "margin-top:14px; font-size:15px;",
        "Estimated savings vs your prior design: ",
        tags$span(
          class = "cost-savings-highlight",
          sprintf("%.1f%% lower total cost", sp)
        ),
        if (cc$savings_abs > 0) {
          tags$span(
            style = "color:#4a5f78; font-weight:400;",
            sprintf(" (%s %s)", fmt(cc$savings_abs), cur)
          )
        } else {
          NULL
        }
      )
    } else if (is.finite(sp) && sp < -0.05) {
      children[[length(children) + 1L]] <- tags$p(
        class = "text-muted",
        style = "margin-top:14px;",
        sprintf(
          "App design is %.1f%% higher total cost than the prior count at the same unit cost.",
          abs(sp)
        )
      )
    } else {
      children[[length(children) + 1L]] <- tags$p(
        class = "text-muted",
        style = "margin-top:14px;",
        "Total costs are similar at these sample counts and unit cost."
      )
    }
    do.call(tags$div, children)
  })

  refresh_app_sample_extraction <- function() {
    all_r <- sampling_selected_rasters()
    if (length(all_r) < 1L) {
      app_compare_values_df(NULL)
      return(invisible(FALSE))
    }
    boundary <- digitized_features()
    sm <- isolate(input$sampling_method)
    ap <- if (identical(sm, "manual")) manual_points() else sample_points()
    if (is.null(ap) || nrow(ap) < 1L) {
      app_compare_values_df(NULL)
      return(invisible(FALSE))
    }
    sp <- deployment_safety_params()
    av <- extract_covariate_values_at_points(
      ap,
      all_r,
      boundary,
      buffer_m = 0,
      analysis_crs = analysis_crs_string(),
      harmonize_scale = sp$harmonize_scale
    )
    app_compare_values_df(compare_values_wide_to_long(av, "App-generated"))
    invisible(TRUE)
  }

  population_values_long_for_layers <- function(
    layer_names,
    boundary_sf = NULL,
    max_cells_per_layer = 8000L,
    all_r = NULL
  ) {
    if (is.null(all_r)) all_r <- available_rasters()
    if (length(all_r) < 1L || length(layer_names) < 1L) return(NULL)
    layer_names <- intersect(as.character(layer_names), names(all_r))
    if (!length(layer_names)) return(NULL)
    sel <- all_r[layer_names]
    sp <- deployment_safety_params()
    harmonized <- harmonize_covariate_layers(
      sel,
      boundary_sf = boundary_sf,
      analysis_crs = analysis_crs_string(),
      harmonize_scale = sp$harmonize_scale
    )
    if (!length(harmonized)) return(NULL)
    rows <- vector("list", length(harmonized))
    for (i in seq_along(harmonized)) {
      nm <- names(harmonized)[i]
      r <- harmonized[[i]]
      v <- tryCatch(suppressWarnings(as.numeric(raster::values(r))), error = function(e) NULL)
      if (is.null(v)) next
      v <- v[is.finite(v)]
      if (length(v) < 1L) next
      if (length(v) > max_cells_per_layer) {
        set.seed(42L + i)
        v <- sample(v, max_cells_per_layer)
      }
      rows[[i]] <- data.frame(
        Layer = nm,
        Source = "Population",
        Value = v,
        stringsAsFactors = FALSE
      )
    }
    rows <- Filter(Negate(is.null), rows)
    if (!length(rows)) return(NULL)
    dplyr::bind_rows(rows)
  }

  run_field_compare_extraction <- function() {
    hp <- historical_sample_points()
    if (!isTRUE(field_compare_historical_ready())) {
      app_compare_values_df(NULL)
      historical_compare_values_df(NULL)
      return(invisible(FALSE))
    }
    all_r <- available_rasters()
    sel_r <- sampling_selected_rasters()
    boundary <- digitized_features()
    if (length(all_r) < 1L) {
      historical_compare_values_df(NULL)
      app_compare_values_df(NULL)
      return(invisible(FALSE))
    }
    sp <- deployment_safety_params()
    crs_lab <- analysis_crs_string()

    tryCatch(
      validate_field_compare_points(hp, boundary, all_r, "historical point"),
      error = function(e) {
        historical_compare_values_df(NULL)
        app_compare_values_df(NULL)
        stop(conditionMessage(e), call. = FALSE)
      }
    )
    hv <- extract_covariate_values_at_points(
      hp, all_r, boundary, buffer_m = 0, crs_lab, sp$harmonize_scale
    )
    historical_compare_values_df(compare_values_wide_to_long(hv, "Historical"))

    sm <- isolate(input$sampling_method)
    ap <- if (identical(sm, "manual")) manual_points() else sample_points()
    if (!is.null(ap) && nrow(ap) > 0L) {
      tryCatch(
        {
          validate_field_compare_points(ap, boundary, sel_r, "app sample point")
          av <- extract_covariate_values_at_points(
            ap, sel_r, boundary, buffer_m = 0, crs_lab, sp$harmonize_scale
          )
          app_compare_values_df(compare_values_wide_to_long(av, "App-generated"))
        },
        error = function(e) {
          app_compare_values_df(NULL)
          showNotification(conditionMessage(e), type = "warning", duration = 10)
        }
      )
    } else {
      app_compare_values_df(NULL)
    }
    invisible(TRUE)
  }

  current_app_sample_points <- reactive({
    sm <- isolate(input$sampling_method)
    if (identical(sm, "manual")) manual_points() else sample_points()
  })

  field_compare_refresh_tick <- reactiveVal(0L)

  refresh_field_compare_map_and_values <- function(notify = TRUE, progress_msg = "Extracting covariate values at sample points...") {
    if (!isTRUE(field_compare_historical_ready())) return(invisible(FALSE))
    ok <- FALSE
    withProgress(message = progress_msg, value = 0.5, {
      ok <- tryCatch(
        run_field_compare_extraction(),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = 10)
          FALSE
        }
      )
      if (!isTRUE(ok)) {
        if (isTRUE(notify)) {
          showNotification("No raster layers loaded yet. Load variables on the Variables tab first.", type = "warning", duration = 6)
        }
        return(invisible(FALSE))
      }
      field_compare_refresh_tick(isolate(field_compare_refresh_tick()) + 1L)
      schedule_field_compare_map_redraw()
      if (isTRUE(notify)) {
        showNotification("Covariate values extracted for all layers; comparison map updated.", type = "message", duration = 4)
      }
    })
    invisible(isTRUE(ok))
  }

  observeEvent(input$compute_field_compare_distributions, {
    if (!isTRUE(field_compare_historical_ready())) {
      showNotification("Upload historical field sample points first.", type = "warning", duration = 6)
      return(invisible(NULL))
    }
    lock_action_button("compute_field_compare_distributions")
    pts <- current_app_sample_points()
    if (is.null(pts) || nrow(pts) < 1L) {
      showNotification("Generate or save app sample points on the Sampling tab first.", type = "warning", duration = 6)
      return(invisible(NULL))
    }
    withProgress(message = "Building field comparison distributions...", value = 0.5, {
      ok <- tryCatch(
        run_field_compare_extraction(),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = 10)
          FALSE
        }
      )
      if (!isTRUE(ok)) {
        showNotification("No raster layers loaded yet. Load variables first.", type = "warning", duration = 6)
        return(invisible(NULL))
      }
      field_compare_refresh_tick(isolate(field_compare_refresh_tick()) + 1L)
      rebuild_field_compare_violin_plots()
    })
  }, ignoreInit = TRUE)

  clear_distribution_plot_caches <- function() {
    summary_distribution_plots_cache(list())
    field_compare_distribution_plots_cache(list())
    summary_distribution_gen(list(state = "idle", done = 0L, total = 0L))
    field_compare_distribution_gen(list(state = "idle", done = 0L, total = 0L))
    unlock_summary_compute_buttons()
    unlock_action_button("compute_field_compare_distributions")
  }

  distribution_gen_status_ui <- function(gen) {
    if (is.null(gen) || identical(gen$state, "idle")) return(NULL)
    if (identical(gen$state, "generating")) {
      return(tags$div(
        class = "distribution-gen-status distribution-gen-status--busy",
        icon("spinner", class = "fa-spin"),
        paste0(
          " Generating distribution plots… ",
          gen$done, " of ", gen$total,
          " complete. Each layer is rendered separately and may take a moment."
        )
      ))
    }
    if (identical(gen$state, "complete") && isTRUE(gen$total > 0L)) {
      return(tags$div(
        class = "distribution-gen-status distribution-gen-status--done",
        icon("circle-check"),
        paste0(
          " Completed generating ",
          gen$total,
          " distribution plot",
          if (as.integer(gen$total) == 1L) "" else "s",
          "."
        )
      ))
    }
    NULL
  }

  build_violin_layer_plot <- function(layer_df, layer_name, single_source_label = NULL, raster_limits = NULL, pop_values = NULL) {
    if (!is.null(single_source_label) && length(single_source_label) == 1L && !is.null(layer_df)) {
      layer_df$Source <- single_source_label
    }
    if (is.null(pop_values) && !is.null(layer_df)) {
      pop_rows <- layer_df[layer_df$Source == "Population", , drop = FALSE]
      if (nrow(pop_rows) > 0L) pop_values <- pop_rows$Value
    }
    sample_df <- layer_df
    if (!is.null(sample_df) && "Source" %in% names(sample_df)) {
      sample_df <- sample_df[sample_df$Source != "Population", , drop = FALSE]
    }
    build_population_sample_distribution_plot(
      layer_name = layer_name,
      sample_df = sample_df,
      pop_values = pop_values,
      raster_limits = raster_limits
    )
  }

  rebuild_distribution_plot_outputs <- function(
    data_df,
    grid_output_id,
    stack_output_id,
    cache_setter,
    gen_setter,
    single_source_label = NULL,
    plot_mode = c("summary", "field_compare")
  ) {
    plot_mode <- match.arg(plot_mode)
    if (is.null(data_df) || nrow(data_df) < 1L) {
      gen_setter(list(state = "idle", done = 0L, total = 0L))
      cache_setter(list())
      output[[grid_output_id]] <- renderUI({
        tags$p(class = "text-muted", style = "margin:8px 0;", "No distribution data to plot yet.")
      })
      return(invisible(NULL))
    }

    sample_df <- data_df
    if (!is.null(single_source_label) && length(single_source_label) == 1L) {
      sample_df$Source <- single_source_label
    }
    if ("Source" %in% names(sample_df)) {
      sample_df <- sample_df[sample_df$Source != "Population", , drop = FALSE]
    }
    layers <- unique(as.character(sample_df$Layer))
    n_layers <- length(layers)
    if (n_layers < 1L) {
      gen_setter(list(state = "idle", done = 0L, total = 0L))
      cache_setter(list())
      output[[grid_output_id]] <- renderUI({
        tags$p(class = "text-muted", style = "margin:8px 0;", "No sample values to plot.")
      })
      return(invisible(NULL))
    }
    gen_setter(list(state = "generating", done = 0L, total = n_layers))
    raster_stack <- if (identical(plot_mode, "field_compare")) {
      isolate(available_rasters())
    } else {
      rs <- isolate(sampling_selected_rasters())
      if (length(rs)) rs else isolate(available_rasters())
    }
    raster_limits <- precompute_raster_layer_limits(raster_stack)
    boundary_sf <- digitized_features()
    pop_long <- NULL
    if (identical(plot_mode, "summary")) {
      pop_long <- tryCatch(
        population_values_long_for_layers(layers, boundary_sf = boundary_sf, all_r = raster_stack),
        error = function(e) NULL
      )
    }

    built_plots <- vector("list", n_layers)
    withProgress(message = "Building distribution plots...", value = 0, {
      for (i in seq_along(layers)) {
        lyr <- layers[[i]]
        lyr_sample <- sample_df[sample_df$Layer == lyr, , drop = FALSE]
        if (identical(plot_mode, "field_compare")) {
          lyr_app <- lyr_sample[lyr_sample$Source == "App-generated", , drop = FALSE]
          lyr_hist <- lyr_sample[lyr_sample$Source == "Historical", , drop = FALSE]
          built_plots[[i]] <- build_field_compare_distribution_plot(
            layer_name = lyr,
            app_df = lyr_app,
            hist_df = lyr_hist,
            raster_limits = raster_limits
          )
        } else {
          pop_vals <- NULL
          if (!is.null(pop_long) && nrow(pop_long) > 0L) {
            pop_vals <- pop_long$Value[pop_long$Layer == lyr]
          }
          built_plots[[i]] <- build_population_sample_distribution_plot(
            layer_name = lyr,
            sample_df = lyr_sample,
            pop_values = pop_vals,
            raster_limits = raster_limits
          )
        }
        gen_setter(list(state = "generating", done = i, total = n_layers))
        setProgress(i / n_layers, detail = paste0("Layer ", i, " of ", n_layers, ": ", lyr))
      }
    })

    cache_setter(built_plots)
    plot_h <- distribution_stack_height_px(length(built_plots), per_plot_px = DISTRIBUTION_PLOT_PER_PANEL_PX)
    plot_w <- paste0(DISTRIBUTION_PLOT_WIDTH_PX, "px")
    output[[grid_output_id]] <- renderUI({
      plot_ui <- withSpinner(plotOutput(
        stack_output_id,
        height = paste0(plot_h, "px"),
        width = plot_w
      ))
      tags$div(class = "summary-distribution-plot-wrap", plot_ui)
    })

    gen_setter(list(state = "complete", done = n_layers, total = n_layers))
    tryCatch(gc(verbose = FALSE), error = function(e) invisible(NULL))
    invisible(TRUE)
  }

  rebuild_field_compare_violin_plots <- function() {
    if (!isTRUE(field_compare_historical_ready())) {
      field_compare_distribution_gen(list(state = "idle", done = 0L, total = 0L))
      rebuild_distribution_plot_outputs(
        NULL,
        "field_compare_plots_grid_ui",
        "field_compare_distribution_plot_stack",
        field_compare_distribution_plots_cache,
        field_compare_distribution_gen
      )
      return(invisible(NULL))
    }
    app_df <- app_compare_values_df()
    hist_df <- historical_compare_values_df()
    d <- NULL
    if (!is.null(app_df) && nrow(app_df) > 0L) d <- app_df
    if (!is.null(hist_df) && nrow(hist_df) > 0L) {
      d <- if (is.null(d)) hist_df else dplyr::bind_rows(d, hist_df)
    }
    if (is.null(d) || nrow(d) < 1L) {
      field_compare_distribution_gen(list(state = "idle", done = 0L, total = 0L))
      rebuild_distribution_plot_outputs(
        d,
        "field_compare_plots_grid_ui",
        "field_compare_distribution_plot_stack",
        field_compare_distribution_plots_cache,
        field_compare_distribution_gen
      )
      return(invisible(NULL))
    }
    n_layers <- length(unique(as.character(d$Layer)))
    field_compare_distribution_gen(list(state = "generating", done = 0L, total = n_layers))
    rebuild_distribution_plot_outputs(
      d,
      "field_compare_plots_grid_ui",
      "field_compare_distribution_plot_stack",
      field_compare_distribution_plots_cache,
      field_compare_distribution_gen,
      plot_mode = "field_compare"
    )
  }

  rebuild_summary_violin_plots <- function() {
    pts <- current_app_sample_points()
    if (is.null(pts) || nrow(pts) < 1L) {
      summary_distribution_gen(list(state = "idle", done = 0L, total = 0L))
      rebuild_distribution_plot_outputs(
        NULL,
        "summary_distribution_plots_ui",
        "summary_distribution_plot_stack",
        summary_distribution_plots_cache,
        summary_distribution_gen
      )
      return(invisible(NULL))
    }
    tryCatch(refresh_app_sample_extraction(), error = function(e) invisible(NULL))
    d <- app_compare_values_df()
    if (!is.null(d) && nrow(d) > 0L) {
      d <- d[d$Source == "App-generated", , drop = FALSE]
      keep_lyr <- active_covariate_layer_names_r()
      if (length(keep_lyr)) {
        d <- d[d$Layer %in% keep_lyr, , drop = FALSE]
      }
    }
    if (is.null(d) || nrow(d) < 1L) {
      summary_distribution_gen(list(state = "idle", done = 0L, total = 0L))
      rebuild_distribution_plot_outputs(
        NULL,
        "summary_distribution_plots_ui",
        "summary_distribution_plot_stack",
        summary_distribution_plots_cache,
        summary_distribution_gen
      )
      return(invisible(NULL))
    }
    n_layers <- length(unique(as.character(d$Layer)))
    summary_distribution_gen(list(state = "generating", done = 0L, total = n_layers))
    rebuild_distribution_plot_outputs(
      d,
      "summary_distribution_plots_ui",
      "summary_distribution_plot_stack",
      summary_distribution_plots_cache,
      summary_distribution_gen,
      plot_mode = "summary"
    )
  }

  build_field_compare_distribution_plots_list <- function(d) {
    cached <- field_compare_distribution_plots_cache()
    if (length(cached)) return(cached)
    if (is.null(d) || nrow(d) < 1L) return(list())
    raster_stack <- available_rasters()
    raster_limits <- precompute_raster_layer_limits(raster_stack)
    layers <- unique(as.character(d$Layer))
    lapply(layers, function(lyr) {
      lyr_df <- d[d$Layer == lyr, , drop = FALSE]
      build_field_compare_distribution_plot(
        layer_name = lyr,
        app_df = lyr_df[lyr_df$Source == "App-generated", , drop = FALSE],
        hist_df = lyr_df[lyr_df$Source == "Historical", , drop = FALSE],
        raster_limits = raster_limits
      )
    })
  }

  output$field_compare_plots_grid_ui <- renderUI({
    if (!isTRUE(field_compare_historical_ready())) {
      return(tags$p(class = "text-muted", style = "margin:8px 0;", "Upload historical field sample points to view distributions."))
    }
    tags$p(class = "text-muted", style = "margin:8px 0;", "Click ", strong("Compute distributions"), " in the sidebar to build plots.")
  })
  output$summary_distribution_plots_ui <- renderUI({
    tags$p(class = "text-muted", style = "margin:8px 0;", "Click ", strong("Compute distributions"), " on the Summary sub-tab to build plots.")
  })
  output$summary_distribution_plot_stack <- renderPlot({
    plots <- summary_distribution_plots_cache()
    if (!length(plots)) {
      grid::grid.newpage()
      grid::grid.text("Generate sample points to view per-layer distributions.", x = 0.5, y = 0.5)
      return(invisible(NULL))
    }
    draw_distribution_plot_stack(plots)
  }, bg = "#eef2f7", height = function() {
    distribution_stack_height_px(length(summary_distribution_plots_cache()), per_plot_px = DISTRIBUTION_PLOT_PER_PANEL_PX)
  })
  output$field_compare_distribution_plot_stack <- renderPlot({
    if (!isTRUE(field_compare_historical_ready())) {
      grid::grid.newpage()
      grid::grid.text("Upload historical field sample points first.", x = 0.5, y = 0.5)
      return(invisible(NULL))
    }
    plots <- field_compare_distribution_plots_cache()
    if (!length(plots)) {
      grid::grid.newpage()
      grid::grid.text("Distribution plots appear after extraction.", x = 0.5, y = 0.5)
      return(invisible(NULL))
    }
    draw_distribution_plot_stack(plots)
  }, bg = "#eef2f7", height = function() {
    distribution_stack_height_px(length(field_compare_distribution_plots_cache()), per_plot_px = DISTRIBUTION_PLOT_PER_PANEL_PX)
  })

  field_compare_values_df <- reactive({
    req(isTRUE(field_compare_historical_ready()))
    field_compare_refresh_tick()
    app_df <- app_compare_values_df()
    hist_df <- historical_compare_values_df()
    if (is.null(app_df) && is.null(hist_df)) return(NULL)
    if (is.null(app_df)) return(hist_df)
    if (is.null(hist_df)) return(app_df)
    dplyr::bind_rows(app_df, hist_df)
  })

  field_compare_combined_bbox <- function() {
    bb <- NULL
    hist_pts <- historical_sample_points()
    if (!is.null(hist_pts) && nrow(hist_pts) > 0L) {
      bb <- sf::st_bbox(sf::st_transform(hist_pts, 4326))
    }
    app_pts <- current_app_sample_points()
    if (!is.null(app_pts) && nrow(app_pts) > 0L) {
      ab <- sf::st_bbox(sf::st_transform(app_pts, 4326))
      bb <- if (is.null(bb)) {
        ab
      } else {
        c(
          xmin = min(bb[["xmin"]], ab[["xmin"]]),
          ymin = min(bb[["ymin"]], ab[["ymin"]]),
          xmax = max(bb[["xmax"]], ab[["xmax"]]),
          ymax = max(bb[["ymax"]], ab[["ymax"]])
        )
      }
    }
    if (is.null(bb) && !is.null(digitized_features()) && nrow(digitized_features()) > 0L) {
      bb <- sf::st_bbox(sf::st_transform(digitized_features(), 4326))
    }
    bb
  }

  build_field_compare_leaflet_map <- function() {
    m <- initial_map %>%
      setView(lng = map_state$center$lng, lat = map_state$center$lat, zoom = map_state$zoom)
    b <- digitized_features()
    if (!is.null(b) && nrow(b) > 0L) {
      dfx <- tryCatch(sf::st_transform(b, 4326), error = function(e) b)
      m <- m %>% addPolygons(
        data = dfx,
        color = "#d62728",
        fillOpacity = 0,
        fillColor = "transparent",
        weight = 3,
        opacity = 0.95,
        group = "Digitized Boundary"
      )
    }
    overlay_groups <- character(0)
    hist_pts <- historical_sample_points()
    if (!is.null(hist_pts) && nrow(hist_pts) > 0L) {
      hist_pts <- sf::st_transform(hist_pts, 4326)
      m <- m %>% addCircleMarkers(
        data = hist_pts,
        radius = 7,
        stroke = TRUE,
        weight = 2,
        color = "#c45c00",
        fillColor = "#e67e22",
        fillOpacity = 0.92,
        group = "Historical",
        popup = if ("hist_id" %in% names(hist_pts)) ~paste0("Historical point #", hist_id) else "Historical point"
      )
      overlay_groups <- c(overlay_groups, "Historical")
    }
    app_pts <- current_app_sample_points()
    if (!is.null(app_pts) && nrow(app_pts) > 0L) {
      app_pts <- sf::st_transform(app_pts, 4326)
      m <- m %>% addCircleMarkers(
        data = app_pts,
        radius = 7,
        stroke = TRUE,
        weight = 2,
        color = "#1f5f99",
        fillColor = "#2c7fb8",
        fillOpacity = 0.92,
        group = "App-generated",
        popup = if ("ID" %in% names(app_pts)) ~paste0("App point #", ID) else "App point"
      )
      overlay_groups <- c(overlay_groups, "App-generated")
    }
    if (length(overlay_groups)) {
      m <- m %>% addLayersControl(
        overlayGroups = overlay_groups,
        options = layersControlOptions(collapsed = FALSE)
      )
    }
    bb <- field_compare_combined_bbox()
    if (!is.null(bb)) {
      xs <- bb[["xmin"]]
      ys <- bb[["ymin"]]
      xe <- bb[["xmax"]]
      ye <- bb[["ymax"]]
      if (all(is.finite(c(xs, ys, xe, ye)))) {
        m <- m %>% fitBounds(
          lng1 = xs, lat1 = ys, lng2 = xe, lat2 = ye,
          options = list(padding = c(28, 28), maxZoom = 18)
        )
      }
    }
    m
  }

  schedule_field_compare_map_redraw <- function() {
    field_compare_refresh_tick(isolate(field_compare_refresh_tick()) + 1L)
    shinyjs::delay(80, {
      field_compare_refresh_tick(isolate(field_compare_refresh_tick()) + 1L)
      tryCatch(
        invalidate_leaflet_maps_client("field_compare_map", delays_ms = 120L),
        error = function(e) invisible(NULL)
      )
    })
    shinyjs::delay(450, {
      field_compare_refresh_tick(isolate(field_compare_refresh_tick()) + 1L)
    })
    invisible(NULL)
  }

  render_field_compare_map <- function() {
    schedule_field_compare_map_redraw()
  }

  observeEvent(field_compare_historical_ready(), {
    if (!isTRUE(field_compare_historical_ready())) return(invisible(NULL))
    schedule_field_compare_map_redraw()
  }, ignoreInit = TRUE)

  observeEvent(
    list(input$main_tabs, input$field_compare_subtabs),
    {
      if (!identical(isolate(input$main_tabs), "FieldCompare")) return(invisible(NULL))
      if (!identical(isolate(input$field_compare_subtabs), "field_compare_map_tab")) return(invisible(NULL))
      if (!isTRUE(field_compare_historical_ready())) return(invisible(NULL))
      schedule_field_compare_map_redraw()
    },
    ignoreInit = TRUE
  )

  observeEvent(input$upload_historical_points, {
    req(input$upload_historical_points)
    withProgress(message = "Uploading and validating field sample points...", value = 0.5, {
      tryCatch({
        sf_obj <- read_uploaded_sample_points(input$upload_historical_points)
        if (is.na(sf::st_crs(sf_obj))) {
          showNotification("Sample points have no CRS; assuming WGS84 (EPSG:4326).", type = "warning", duration = 6)
          sf::st_crs(sf_obj) <- 4326
        }
        pts <- normalize_uploaded_sample_points(sf_obj)
        boundary <- digitized_features()
        all_r <- available_rasters()
        validate_field_compare_points(pts, boundary, all_r, "historical point")
        historical_sample_points(pts)
        clear_distribution_plot_caches()
        tryCatch(updateTabsetPanel(session, "field_compare_subtabs", selected = "field_compare_map_tab"), silent = TRUE)
        output$field_compare_upload_tip <- renderText(
          paste0("Validated ", nrow(pts), " point(s). Extracting covariate values for all layers...")
        )
        schedule_field_compare_map_redraw()
        refresh_field_compare_map_and_values(
          notify = TRUE,
          progress_msg = "Extracting covariate values at historical points for all layers..."
        )
        output$field_compare_upload_tip <- renderText(
          paste0("Validated ", nrow(pts), " point(s); values extracted for all loaded layers. Map updated.")
        )
        if (!is.null(digitized_features())) apply_boundary_overlay_to_maps()
      }, error = function(e) {
        historical_sample_points(NULL)
        historical_compare_values_df(NULL)
        app_compare_values_df(NULL)
        clear_distribution_plot_caches()
        output$field_compare_upload_tip <- renderText("")
        showNotification(paste("Historical points upload failed:", e$message), type = "error", duration = 10)
      })
    })
  }, ignoreNULL = TRUE)

  observeEvent(input$clear_historical_points, {
    historical_sample_points(NULL)
    historical_compare_values_df(NULL)
    app_compare_values_df(NULL)
    clear_distribution_plot_caches()
    field_compare_refresh_tick(isolate(field_compare_refresh_tick()) + 1L)
    shinyjs::reset("upload_historical_points")
    output$field_compare_upload_tip <- renderText("")
    tryCatch(
      leafletProxy("field_compare_map") %>%
        clearGroup("App-generated") %>%
        clearGroup("Historical") %>%
        clearControls(),
      error = function(e) invisible(NULL)
    )
    showNotification("Historical sample points cleared.", type = "message", duration = 4)
  }, ignoreInit = TRUE)

  output$field_compare_upload_status_ui <- renderUI({
    pts <- historical_sample_points()
    if (is.null(pts) || nrow(pts) < 1L) {
      return(tags$p(class = "text-muted", style = "font-size:11px; margin:6px 0 0 0;", "No historical points uploaded yet."))
    }
    tags$p(
      style = "font-size:11px; margin:6px 0 0 0; color:#2f6f58; font-weight:600;",
      paste0(nrow(pts), " historical point(s) loaded.")
    )
  })

  output$field_compare_sidebar_status_ui <- renderUI({
    hist_pts <- historical_sample_points()
    if (is.null(hist_pts) || nrow(hist_pts) < 1L) {
      return(tags$div(
        class = "alert alert-warning",
        style = "margin-top:10px; border-radius:10px; font-size:12px;",
        "Upload ", strong("historical field sample points"), " above to enable the map and distributions. Points must be inside the boundary and overlap loaded rasters."
      ))
    }
    app_pts <- current_app_sample_points()
    if (is.null(app_pts) || nrow(app_pts) < 1L) {
      return(tags$p(
        class = "text-muted",
        style = "font-size:12px; margin-top:10px;",
        nrow(hist_pts), " historical point(s) loaded. Generate app sample points on ",
        tags$strong("Sampling"), " to compare."
      ))
    }
    tags$div(
      style = "margin-top:10px; padding:8px 10px; border:1px solid #cfe6ff; border-radius:10px; background:#f5faff; font-size:12px;",
      tags$strong("Ready to compare: "),
      paste0(nrow(app_pts), " app point(s) vs ", nrow(hist_pts), " historical point(s).")
    )
  })

  output$field_compare_map_status_ui <- renderUI({
    if (!isTRUE(field_compare_historical_ready())) return(NULL)
    app_pts <- current_app_sample_points()
    hist_pts <- historical_sample_points()
    if (is.null(app_pts) || nrow(app_pts) < 1L) {
      return(tags$div(class = "text-muted", "Historical points shown in orange. Generate app sample points on Sampling to add blue markers."))
    }
    tags$p(
      class = "text-muted",
      style = "margin-bottom:8px; font-size:13px;",
      paste0("Blue: ", nrow(app_pts), " app point(s); orange: ", nrow(hist_pts), " historical point(s).")
    )
  })

  output$field_compare_plots_status_ui <- renderUI({
    if (!isTRUE(field_compare_historical_ready())) return(NULL)
    gen_banner <- distribution_gen_status_ui(field_compare_distribution_gen())
    app_pts <- current_app_sample_points()
    hist_pts <- historical_sample_points()
    if (is.null(hist_pts) || nrow(hist_pts) < 1L) {
      return(tagList(gen_banner, tags$div(class = "text-muted", "Upload historical field sample points in the sidebar.")))
    }
    if (is.null(app_pts) || nrow(app_pts) < 1L) {
      return(tagList(gen_banner, tags$div(class = "text-muted", "Generate or save app sample points on the ", strong("Sampling"), " sub-tab to compare distributions.")))
    }
    d <- field_compare_values_df()
    if (!is.null(d) && nrow(d) > 0L) {
      return(tagList(
        gen_banner,
        tags$p(
          class = "text-muted",
          style = "margin-bottom:8px;",
          paste0(
            length(unique(d$Layer)), " layer(s) plotted; ",
            sum(d$Source == "App-generated"), " app values and ",
            sum(d$Source == "Historical"), " historical values extracted."
          )
        )
      ))
    }
    all_r <- available_rasters()
    tagList(
      gen_banner,
      tags$div(
        class = "text-muted",
        tags$p(style = "margin:0 0 6px 0;", "Points are loaded, but no covariate values could be extracted for plotting yet."),
        tags$p(
          style = "margin:0 0 6px 0; font-size:12px;",
          paste0(
            "App points: ", nrow(app_pts), "; historical: ", nrow(hist_pts),
            "; loaded raster layers: ", length(all_r), "."
          )
        ),
        tags$p(
          style = "margin:0; font-size:12px;",
          "Covariate values are extracted automatically after upload. Open the ",
          strong("Distributions"), " tab and click ", strong("Compute distributions"), " to build plots."
        )
      )
    )
  })

  output$download_field_compare_plot <- downloadHandler(
    filename = function() {
      paste0("field_compare_distributions_", format(Sys.time(), "%Y%m%d_%H%M"), ".png")
    },
    content = function(file) {
      if (!isTRUE(field_compare_historical_ready())) {
        stop("Upload historical field sample points before downloading comparison plots.", call. = FALSE)
      }
      plots <- field_compare_distribution_plots_cache()
      if (!length(plots)) {
        d <- field_compare_values_df()
        if (is.null(d) || nrow(d) < 1L) stop("No comparison data to download yet. Click Compute distributions first.", call. = FALSE)
        plots <- build_field_compare_distribution_plots_list(d)
      }
      if (!length(plots)) stop("Could not build comparison plots.", call. = FALSE)
      write_distribution_plots_png(file, plots)
    }
  )

  output$download_pop_sample_distribution_plot <- downloadHandler(
    filename = function() {
      paste0("population_vs_sample_distributions_", format(Sys.time(), "%Y%m%d_%H%M"), ".png")
    },
    content = function(file) {
      plots <- summary_distribution_plots_cache()
      if (!length(plots)) {
        stop("Click Compute distributions on the Population vs sample tab first.", call. = FALSE)
      }
      write_distribution_plots_png(file, plots)
    }
  )

  output$zoom_button_ui_field_compare <- renderUI({
    if (has_zoomable_content()) {
      actionButton("zoom_to_area_field_compare", "Zoom to Area", class = "btn-zoom btn-zoom-below")
    }
  })

  observeEvent(input$zoom_to_area_field_compare, {
    if (!isTRUE(field_compare_historical_ready())) return(invisible(NULL))
    bb <- NULL
    app_pts <- current_app_sample_points()
    hist_pts <- historical_sample_points()
    if (!is.null(app_pts) && nrow(app_pts) > 0L) bb <- sf::st_bbox(app_pts)
    if (!is.null(hist_pts) && nrow(hist_pts) > 0L) {
      hb <- sf::st_bbox(hist_pts)
      bb <- if (is.null(bb)) hb else c(
        xmin = min(bb[["xmin"]], hb[["xmin"]]),
        ymin = min(bb[["ymin"]], hb[["ymin"]]),
        xmax = max(bb[["xmax"]], hb[["xmax"]]),
        ymax = max(bb[["ymax"]], hb[["ymax"]])
      )
    }
    if (is.null(bb) && !is.null(digitized_features())) bb <- sf::st_bbox(digitized_features())
    if (!is.null(bb)) {
      xs <- bb[["xmin"]]; ys <- bb[["ymin"]]; xe <- bb[["xmax"]]; ye <- bb[["ymax"]]
      if (all(is.finite(c(xs, ys, xe, ye)))) {
        leafletProxy("field_compare_map") %>%
          leaflet::fitBounds(lng1 = xs, lat1 = ys, lng2 = xe, lat2 = ye, options = list(padding = c(24, 24), maxZoom = 18))
      }
    }
  }, ignoreInit = TRUE)

  outputOptions(output, "field_compare_upload_status_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "field_compare_sidebar_status_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "field_compare_map_status_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "field_compare_plots_status_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "field_compare_plots_grid_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "summary_distribution_status_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "summary_distribution_plots_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "summary_distribution_plot_stack", suspendWhenHidden = FALSE)
  outputOptions(output, "field_compare_distribution_plot_stack", suspendWhenHidden = FALSE)

  observeEvent(input$compute_variables_summary, {
    req(digitized_features())
    b <- digitized_features()
    rs <- available_rasters()
    if (!length(rs)) {
      showNotification("No loaded rasters (VIs, elevation, soil, etc.) to summarize.", type = "warning")
      return()
    }
    lock_action_button("compute_variables_summary")
    rows <- vector("list", length(rs))
    for (i in seq_along(rs)) {
      nm <- names(rs)[i]
      st <- tryCatch(layer_summary_stats_masked(rs[[i]], b), error = function(e) NULL)
      rows[[i]] <- if (is.null(st)) {
        NULL
      } else {
        data.frame(Layer = nm, t(st), stringsAsFactors = FALSE, check.names = FALSE)
      }
    }
    rows <- Filter(Negate(is.null), rows)
    if (!length(rows)) {
      variables_summary_df(NULL)
      showNotification("Could not compute summaries (check boundary overlap and CRS).", type = "warning")
      return()
    }
    variables_summary_df(do.call(rbind, rows))
    showNotification("Variable summary table updated.", type = "message")
  })

  output$variables_summary_table_out <- DT::renderDataTable({
    d <- variables_summary_df()
    if (is.null(d) || !nrow(d)) return(NULL)
    num_i <- which(sapply(d, is.numeric))
    dt <- DT::datatable(
      d,
      rownames = FALSE,
      options = list(
        dom = "t",
        ordering = TRUE,
        scrollX = TRUE,
        deferRender = TRUE,
        rowCallback = DT::JS("function(row, data) { var txt = 'Stats use pixels inside boundary (large layers: random 150k subsample). | ' + data.join(' | '); $(row).attr('data-row-tip', txt).removeAttr('title'); }")
      ),
      class = "stripe hover compact"
    )
    if (length(num_i)) dt <- DT::formatRound(dt, columns = num_i, digits = 3)
    dt
  })
  outputOptions(output, "variables_summary_table_out", suspendWhenHidden = TRUE)
  
  # --- Manual Sampling Layer Selector ---
  output$manual_layer_selector_ui <- renderUI({
    rasters <- available_rasters()
    if (length(rasters) > 0) {
      selectInput("manual_layer_select", "Select Layer to View:", choices = c("None", names(rasters)))
    } else {
      p("No raster layers available to display.")
    }
  })
  
  observeEvent(input$manual_layer_select, {
    safe_run({
    req(input$sampling_method == "manual", input$manual_layer_select)
    proxy <- leafletProxy("sampling_manual_map-map")
    
    # Clear previous raster and legend
    proxy %>% clearGroup("manual_raster") %>% removeControl("manual_raster_legend")
    
    if (input$manual_layer_select != "None") {
      rasters <- available_rasters()
      selected_raster <- prepare_raster_for_display(rasters[[input$manual_layer_select]], for_leaflet_overlay = TRUE)
      pal_info <- leaflet_numeric_palette(selected_raster)
      if (is.null(pal_info)) {
        showNotification(
          paste("Layer", input$manual_layer_select, "has no valid pixels to display on the map."),
          type = "warning", duration = 6
        )
        return(invisible(NULL))
      }
      proxy %>%
        addRasterImage(selected_raster, colors = pal_info$pal, opacity = 0.7, group = "manual_raster", project = FALSE) %>%
        addLegend(
          pal = pal_info$pal, values = pal_info$val_range, title = input$manual_layer_select,
          position = "bottomright", layerId = "manual_raster_legend"
        )
    }
    }, context = "Manual layer preview", notify = TRUE)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  
  output$auto_layer_selector_ui <- renderUI({
    rasters <- available_rasters()
    if (length(rasters) > 0) {
      selectInput("auto_layer_select", "Select Layer to View:", choices = c("None", names(rasters)))
    } else {
      p("No raster layers available to display.")
    }
  })
  
  observeEvent(input$auto_layer_select, {
    safe_run({
    req(input$sampling_method == "automatic", input$auto_layer_select)
    proxy <- leafletProxy("sampling_auto_map")
    
    # Clear previous raster and legend
    proxy %>% clearGroup("auto_raster") %>% removeControl("auto_raster_legend")
    
    if (input$auto_layer_select != "None") {
      rasters <- available_rasters()
      selected_raster <- prepare_raster_for_display(rasters[[input$auto_layer_select]], for_leaflet_overlay = TRUE)
      pal_info <- leaflet_numeric_palette(selected_raster)
      if (is.null(pal_info)) {
        showNotification(
          paste("Layer", input$auto_layer_select, "has no valid pixels to display on the map."),
          type = "warning", duration = 6
        )
        return(invisible(NULL))
      }
      proxy %>%
        addRasterImage(selected_raster, colors = pal_info$pal, opacity = 0.7, group = "auto_raster", project = FALSE) %>%
        addLegend(
          pal = pal_info$pal, values = pal_info$val_range, title = input$auto_layer_select,
          position = "bottomright", layerId = "auto_raster_legend"
        )
    }
    }, context = "Automatic layer preview", notify = TRUE)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  
  # --- Sync maps when switching sampling method ---
  observeEvent(input$sampling_method, {
    req(input$main_tabs == "Sampling", input$sampling_method)
    
    map_id <- if (input$sampling_method == "manual") "sampling_manual_map-map" else "sampling_auto_map"
    proxy <- leafletProxy(map_id)
    
    # Redraw boundary on the active sampling map
    if (!is.null(digitized_features())) {
      apply_boundary_overlay_to_maps()
    }
    
    all_rasters <- available_rasters()
    
    # Clear any existing raster on the target map
    proxy %>% clearGroup(c("manual_raster", "auto_raster")) %>% removeControl(c("manual_raster_legend", "auto_raster_legend"))
    
    if(length(all_rasters) > 0) {
      # Determine which raster to show
      raster_to_show <- NULL
      raster_name <- NULL
      layer_select <- if (input$sampling_method == "manual") input$manual_layer_select else input$auto_layer_select
      
      if (!is.null(layer_select) && layer_select != "None") {
        raster_to_show <- all_rasters[[layer_select]]
        raster_name <- layer_select
      } else {
        # Otherwise, default to the last added raster
        raster_to_show <- all_rasters[[length(all_rasters)]]
        raster_name <- names(all_rasters)[length(all_rasters)]
      }
      
      if (!is.null(raster_to_show)) {
        raster_to_show <- prepare_raster_for_display(raster_to_show, for_leaflet_overlay = TRUE)
        pal_info <- leaflet_numeric_palette(raster_to_show)
        if (!is.null(pal_info)) {
          group_name <- if (input$sampling_method == "manual") "manual_raster" else "auto_raster"
          legend_id <- if (input$sampling_method == "manual") "manual_raster_legend" else "auto_raster_legend"
          proxy %>%
            addRasterImage(raster_to_show, colors = pal_info$pal, opacity = 0.7, group = group_name, project = FALSE) %>%
            addLegend(pal = pal_info$pal, values = pal_info$val_range, title = raster_name, layerId = legend_id)
        }
      }
    }
  })
  
  # --- MS Imagery Upload Logic ---
  observeEvent(input$upload_ms_stack, {
    req(input$upload_ms_stack)
    withProgress(message = "Uploading MS stack...", value = 0.5, {
      tryCatch({
        r <- prepare_uploaded_raster_file(input$upload_ms_stack$datapath, brick = TRUE)
        uploaded_ms_raster_temp(r)
        showNotification("Multispectral stack uploaded. Assign bands to finish.", type = "message", duration = 6)
        set_layer_tip("imagery")
      }, error = function(e) {
        showNotification(paste("Error reading raster stack:", e$message), type="error")
      })
    })
  })
  observeEvent(input$clear_ms_stack, { shinyjs::reset("upload_ms_stack"); uploaded_ms_raster_temp(NULL); remove_ms_data() })
  
  output$band_assignment_ui <- renderUI({
    req(uploaded_ms_raster_temp())
    band_names <- names(uploaded_ms_raster_temp())
    tagList(
      selectInput("assign_NIR", "NIR Layer:", choices = band_names),
      selectInput("assign_Red", "Red Layer:", choices = band_names),
      selectInput("assign_RedEdge", "Red Edge Layer:", choices = band_names),
      selectInput("assign_Blue", "Blue Layer:", choices = band_names),
      selectInput("assign_Green", "Green Layer:", choices = band_names)
    )
  })
  
  observeEvent(input$assign_bands, {
    req(uploaded_ms_raster_temp(), input$assign_NIR, input$assign_Red, input$assign_RedEdge, input$assign_Blue, input$assign_Green)
    withProgress(message = "Assigning multispectral bands...", value = 0.5, {
      tryCatch({
        r <- uploaded_ms_raster_temp()
        assigned_stack <- stack(
          r[[input$assign_NIR]],
          r[[input$assign_Red]],
          r[[input$assign_RedEdge]],
          r[[input$assign_Blue]],
          r[[input$assign_Green]]
        )
        names(assigned_stack) <- c("NIR", "Red", "RedEdge", "Blue", "Green")
        assigned_stack <- project_raster_bilinear(assigned_stack, crs = TARGET_CRS)
        uploaded_ms_raster(assigned_stack)
        notify_layer_ready("Multispectral imagery", "upload")
        set_layer_tip("imagery")
        finalize_layer_on_map(redraw = redraw_ms_raster)
      }, error = function(e) {
        showNotification(paste("Error assigning bands:", e$message), type = "error")
      })
    })
  })
  
  # --- Individual Band Upload & Stacking ---
  band_inputs <- reactiveValues(B=NULL, G=NULL, R=NULL, RE=NULL, NIR=NULL)
  observeEvent(input$upload_blue, { 
    withProgress(message = "Uploading Blue band...", value = 0.5, {
      tryCatch({
        b <- prepare_uploaded_raster_file(input$upload_blue$datapath)
        band_inputs$B <- b
        notify_band_staged("Blue")
      }, error = function(e) showNotification(paste("Error uploading Blue band:", e$message), type = "error"))
    })
  })
  observeEvent(input$upload_green, { 
    withProgress(message = "Uploading Green band...", value = 0.5, {
      tryCatch({
        g <- prepare_uploaded_raster_file(input$upload_green$datapath)
        band_inputs$G <- g
        notify_band_staged("Green")
      }, error = function(e) showNotification(paste("Error uploading Green band:", e$message), type = "error"))
    })
  })
  observeEvent(input$upload_red, { 
    withProgress(message = "Uploading Red band...", value = 0.5, {
      tryCatch({
        red <- prepare_uploaded_raster_file(input$upload_red$datapath)
        band_inputs$R <- red
        notify_band_staged("Red")
      }, error = function(e) showNotification(paste("Error uploading Red band:", e$message), type = "error"))
    })
  })
  observeEvent(input$upload_re, { 
    withProgress(message = "Uploading Red Edge band...", value = 0.5, {
      tryCatch({
        re <- prepare_uploaded_raster_file(input$upload_re$datapath)
        band_inputs$RE <- re
        notify_band_staged("Red Edge")
      }, error = function(e) showNotification(paste("Error uploading Red Edge band:", e$message), type = "error"))
    })
  })
  observeEvent(input$upload_nir, { 
    withProgress(message = "Uploading NIR band...", value = 0.5, {
      tryCatch({
        nir <- prepare_uploaded_raster_file(input$upload_nir$datapath)
        band_inputs$NIR <- nir
        notify_band_staged("NIR")
      }, error = function(e) showNotification(paste("Error uploading NIR band:", e$message), type = "error"))
    })
  })
  
  observeEvent(input$clear_blue, { shinyjs::reset("upload_blue"); band_inputs$B <- NULL })
  observeEvent(input$clear_green, { shinyjs::reset("upload_green"); band_inputs$G <- NULL })
  observeEvent(input$clear_red, { shinyjs::reset("upload_red"); band_inputs$R <- NULL })
  observeEvent(input$clear_re, { shinyjs::reset("upload_re"); band_inputs$RE <- NULL })
  observeEvent(input$clear_nir, { shinyjs::reset("upload_nir"); band_inputs$NIR <- NULL })

  RELOAD_COOLDOWN_MS <- 12500L

  clear_all_leaflet_maps_for_reset <- function() {
    groups <- c(
      "BoundaryOverlay", "Digitized Boundary", "Planet", "Sentinel", "MS",
      "Elevation", "Soil", "Other", "Sample Points", "Sampling Zones",
      "Adaptive Similarity Classes", "Historical Points", "App Sample Points"
    )
    for (mid in ALL_MAP_IDS) {
      tryCatch({
        p <- leafletProxy(mid)
        for (g in groups) {
          p <- p %>% clearGroup(g)
        }
        p %>% clearMarkers() %>% clearShapes()
      }, error = function(e) NULL)
    }
    for (map_id in c("map-map", "sampling_auto_map", "sampling_manual_map-map")) {
      session$sendCustomMessage("clearLeafletDrawFeatures", list(mapId = map_id))
    }
    for (mid in c("map-map", "imagery_map", "elevation_map", "sampling_auto_map", "field_compare_map")) {
      try(
        leafletProxy(mid) %>% setView(
          lng = map_state$center$lng, lat = map_state$center$lat, zoom = map_state$zoom
        ),
        silent = TRUE
      )
    }
    invisible(NULL)
  }

  reset_upload_file_inputs <- function() {
    upload_ids <- c(
      "upload_boundary", "upload_ms_stack", "upload_blue", "upload_green", "upload_red",
      "upload_re", "upload_nir", "upload_vi_tif", "upload_elevation_tif", "upload_soil_tif",
      "upload_other_tif", "upload_historical_points"
    )
    for (uid in upload_ids) {
      tryCatch(shinyjs::reset(uid), error = function(e) invisible(NULL))
    }
    invisible(NULL)
  }

  clear_session_derived_outputs <- function() {
    output$digitize_status <- renderText("")
    output$upload_tip_boundary <- renderText("")
    output$boundary_area <- renderText("")
    output$upload_tip_imagery <- renderText("")
    output$upload_tip_elevation <- renderText("")
    output$upload_tip_soil <- renderText("")
    output$upload_tip_other <- renderText("")
    output$field_compare_upload_tip <- renderText("")
    output$field_compare_upload_status_ui <- renderUI(NULL)
    output$field_compare_sidebar_status_ui <- renderUI(NULL)
    output$field_compare_map_status_ui <- renderUI(NULL)
    output$field_compare_plots_status_ui <- renderUI(NULL)
    output$sampling_info_ui <- renderUI(NULL)
    output$adaptive_recommendation_ui <- renderUI(NULL)
    output$sampling_table <- DT::renderDataTable(NULL)
    output$manual_table <- DT::renderDataTable(NULL)
    output$sampling_density_plots <- renderPlot(NULL)
    output$zonal_cluster_means_ui <- renderUI(NULL)
    output$zonal_cluster_means_table <- DT::renderDataTable(NULL)
    output$comparison_headline_ui <- renderUI(NULL)
    output$comparison_podium_ui <- renderUI(NULL)
    output$comparison_performance_ui <- renderUI(NULL)
    output$comparison_performance_curve <- renderPlot(NULL)
    output$compare_zone_recommend_info <- renderUI(NULL)
    output$compare_zone_wss_plot <- renderPlot(NULL)
    output$comparison_covariate_balance_status_ui <- renderUI(NULL)
    output$comparison_covariate_balance_table <- DT::renderDataTable(NULL)
    output$comparison_methods_at_n_table <- DT::renderDataTable(NULL)
    output$summary_distribution_plots_ui <- renderUI(NULL)
    output$summary_distribution_plot_stack <- renderPlot(NULL)
    output$summary_distribution_status_ui <- renderUI(NULL)
    output$pop_sample_summary_status_ui <- renderUI(NULL)
    output$pop_sample_summary_table <- DT::renderDataTable(NULL)
    output$zone_recommend_info <- renderUI(NULL)
    output$sampling_zone_controls_ui <- renderUI(NULL)
    output$field_compare_plots_grid_ui <- renderUI(NULL)
    output$field_compare_distribution_plot_stack <- renderPlot(NULL)
    output$variables_summary_table_out <- DT::renderDataTable(NULL)
    output$report_summary_ui <- renderUI(NULL)
    output$report_preview_text <- renderText("")
    output$planet_status_ui <- renderUI(NULL)
    output$sentinel_status_ui <- renderUI(NULL)
    output$sentinel_console_panel_ui <- renderUI(NULL)
    output$sentinel_retrieve_ui <- renderUI(NULL)
    output$sentinel_select_ui <- renderUI({})
    output$sentinel_lock_info_ui <- renderUI(NULL)
    output$planet_download_ui <- renderUI({})
    output$sentinel_download_ui <- renderUI({})
    output$clear_sentinel_ui <- renderUI({})
    output$sentinel_band_selector_ui <- renderUI({})
    output$sentinel_vi_calculator_ui <- renderUI({})
    output$sentinel_vi_download_ui <- renderUI({})
    output$planet_band_selector_ui <- renderUI({})
    output$vi_calculator_ui <- renderUI({})
    output$vi_download_ui <- renderUI({})
    output$ms_band_selector_ui <- renderUI({})
    output$ms_vi_calculator_ui <- renderUI({})
    output$ms_vi_download_ui <- renderUI({})
    invisible(NULL)
  }

  reset_session_widget_inputs <- function() {
    tryCatch(updateSelectInput(session, "imagery_source", selected = ""), error = function(e) invisible(NULL))
    tryCatch(updateSelectInput(session, "elevation_source", selected = ""), error = function(e) invisible(NULL))
    tryCatch(updateSelectInput(session, "ms_upload_type", selected = "bands"), error = function(e) invisible(NULL))
    tryCatch(updateRadioButtons(session, "sentinel_retrieval_mode", selected = "single"), error = function(e) invisible(NULL))
    tryCatch(updateSelectInput(session, "sampling_method", selected = "automatic"), error = function(e) invisible(NULL))
    tryCatch(updateSelectInput(session, "sample_type", selected = "Simple Random"), error = function(e) invisible(NULL))
    tryCatch(updateNumericInput(session, "n_points", value = 10), error = function(e) invisible(NULL))
    tryCatch(updateNumericInput(session, "buffer_distance", value = 10), error = function(e) invisible(NULL))
    tryCatch(updateNumericInput(session, "random_seed", value = 123), error = function(e) invisible(NULL))
    tryCatch(updateTextInput(session, "compare_sample_sizes", value = "20,30,40,50"), error = function(e) invisible(NULL))
    tryCatch(updateNumericInput(session, "compare_repeats", value = 3), error = function(e) invisible(NULL))
    tryCatch(updateNumericInput(session, "sentinel_median_max_scenes", value = 10), error = function(e) invisible(NULL))
    tryCatch(updateSliderInput(session, "sentinel_cloud_limit", value = 10), error = function(e) invisible(NULL))
    tryCatch(updateNumericInput(session, "cost_prior_samples", value = 40L), error = function(e) invisible(NULL))
    tryCatch(updateNumericInput(session, "cost_app_samples", value = 10L), error = function(e) invisible(NULL))
    tryCatch(updateNumericInput(session, "cost_per_sample", value = 25), error = function(e) invisible(NULL))
    tryCatch(updateSelectInput(session, "cost_currency_preset", selected = "usd"), error = function(e) invisible(NULL))
    tryCatch(updateTextInput(session, "cost_currency_custom", value = ""), error = function(e) invisible(NULL))
    invisible(NULL)
  }

  reset_session_reactive_extras <- function() {
    variables_summary_df(NULL)
    covariate_data_revision(0L)
    pop_sample_summary_error(NULL)
    manual_points_plot_df(NULL)
    map_refit_scheduled(FALSE)
    field_compare_refresh_tick(0L)
    compare_zone_recommend_msg("Click 'Recommend Zones' to estimate zone count from current covariates (WSS elbow).")
    zone_recommend_message("Recommendation method: WSS elbow (data-driven). Click 'Recommend zones' after loading covariates and boundary.")
    tryCatch(clear_distribution_plot_caches(), error = function(e) invisible(NULL))
    invisible(NULL)
  }

  perform_full_session_reset <- function() {
    abort_planet(TRUE)
    abort_sentinel(TRUE)
    abort_elevation(TRUE)
    abort_compare_sampling(TRUE)
    is_comparing_sampling(FALSE)
    is_downloading_planet(FALSE)
    is_downloading_sentinel(FALSE)
    sentinel_search_active(FALSE)

    unlink_temp_raster_path(isolate(sentinel_result()))
    unlink_temp_raster_path(isolate(planet_result()))
    elev_res <- isolate(elevation_result())
    if (is.list(elev_res) && !is.null(elev_res$tif_file)) {
      unlink_temp_raster_path(elev_res$tif_file)
    }

    reset_to_clean_workflow_state(NULL)
    clear_boundary_and_samples(msg_digitize = FALSE, reset_workflow = FALSE)
    reset_session_reactive_extras()

    message_log_store <<- message_log_tbl_empty()
    push_message_log_ui()
    sentinel_console_lines(character(0))
    sentinel_console_hidden(FALSE)
    sentinel_heartbeat_next(NULL)
    sentinel_status_flush(0L)
    sentinel_active_since(NULL)
    sentinel_last_completed_at(NULL)
    dashboard_status_last("")
    force_dashboard_status(NULL)

    sampling_defaults_by_area$area_tier <- NA_character_
    sampling_defaults_by_area$last_auto_compare_sizes <- "20,30,40,50"
    sampling_defaults_by_area$last_auto_n_points <- 10L

    map_state$center <- list(lng = -83.3576, lat = 33.9519)
    map_state$zoom <- 15
    active_overlays(c("Digitized Boundary"))
    uploaded_ms_raster_temp(NULL)
    uploaded_ms_raster(NULL)
    elevation_download_zoom(13L)

    band_inputs$B <- NULL
    band_inputs$G <- NULL
    band_inputs$R <- NULL
    band_inputs$RE <- NULL
    band_inputs$NIR <- NULL

    adaptive_recommendation_hidden(TRUE)

    reset_upload_file_inputs()
    reset_session_widget_inputs()
    unlock_all_session_action_buttons()
    clear_all_leaflet_maps_for_reset()
    clear_session_derived_outputs()

    updateTabsetPanel(session, "main_tabs", selected = "Welcome")
    tryCatch(updateTabsetPanel(session, "variables_subtabs", selected = "imagery"), silent = TRUE)
    tryCatch(updateTabsetPanel(session, "sampling_subtabs", selected = "samp"), silent = TRUE)
    tryCatch(updateTabsetPanel(session, "field_compare_subtabs", selected = "field_compare_map_tab"), silent = TRUE)

    reset_sampling_ui_defaults()
    release_geosampler_memory()
    trim_session_memory()
    invisible(TRUE)
  }

  observeEvent(input$reload_app_session, {
    shinyjs::disable("reload_app_session")
    send_reload_overlay <- function(text, step = NULL) {
      session$sendCustomMessage(
        "geosamplerSessionReload",
        list(show = TRUE, text = text, step = step)
      )
    }
    send_reload_overlay(
      paste(
        "Clearing maps, layers, caches, and memory.",
        "Please do not click tabs or buttons for 10–15 seconds while GeoSampler restarts."
      ),
      step = 1L
    )
    shinyjs::runjs(
      "if (window.GeoSamplerReloadOverlay) { GeoSamplerReloadOverlay.begin(); }"
    )
    shinyjs::delay(120L, {
      tryCatch(
        perform_full_session_reset(),
        error = function(e) {
          append_message_log(
            paste("Full reset cleanup hit a recoverable error:", conditionMessage(e)),
            type = "error",
            context = "Reset"
          )
          tryCatch(release_geosampler_memory(), error = function(e2) invisible(NULL))
          tryCatch(unlock_all_session_action_buttons(), error = function(e2) invisible(NULL))
        }
      )
      shinyjs::delay(4500L, {
        send_reload_overlay(
          "Freeing memory and preparing a fresh session — please keep waiting…",
          step = 2L
        )
      })
      shinyjs::delay(9000L, {
        send_reload_overlay(
          "Reloading the page now. The app will reconnect in a few seconds…",
          step = 3L
        )
      })
      shinyjs::delay(RELOAD_COOLDOWN_MS, {
        shinyjs::runjs(paste0(
          "try { sessionStorage.setItem('geosampler_just_reloaded', '1'); ",
          "sessionStorage.setItem('geosampler_reload_active', '1'); } catch (e) {} ",
          "try { document.querySelectorAll('.btn-action-used').forEach(function(b){ ",
          "b.classList.remove('btn-action-used'); b.disabled=false; b.removeAttribute('aria-disabled'); }); } catch (e) {}"
        ))
        session$reload()
      })
    })
  }, ignoreInit = TRUE)

  dismiss_reload_overlay <- function() {
    session$sendCustomMessage("geosamplerSessionReload", list(show = FALSE))
    shinyjs::runjs(
      "if (window.GeoSamplerReloadOverlay) { GeoSamplerReloadOverlay.finish(); }"
    )
    invisible(NULL)
  }

  observeEvent(input$geosampler_post_reload_ping, {
    dismiss_reload_overlay()
    invalidate_leaflet_maps_client("map-map", delays_ms = 220L)
    showNotification(
      "GeoSampler reloaded with a clean session. Open a map tab (Boundary or Variables) if a panel looks gray.",
      type = "message",
      duration = 12
    )
    shinyjs::enable("reload_app_session")
  }, ignoreInit = TRUE)
  
  observeEvent(input$stack_bands, {
    req(band_inputs$B, band_inputs$G, band_inputs$R, band_inputs$RE, band_inputs$NIR, message="Please upload all 5 bands.")
    withProgress(message = "Stacking multispectral bands...", value = 0.5, {
      tryCatch({
        band_stack <- raster::stack(band_inputs$NIR, band_inputs$R, band_inputs$RE, band_inputs$B, band_inputs$G)
        names(band_stack) <- c("NIR", "Red", "RedEdge", "Blue", "Green")
        uploaded_ms_raster(band_stack)
        notify_layer_ready("Multispectral imagery", "upload")
        set_layer_tip("imagery")
        finalize_layer_on_map(redraw = redraw_ms_raster)
      }, error = function(e) {
        showNotification(paste("Error stacking bands:", e$message), type = "error")
      })
    })
  })
  
  # --- Upload VI Layers ---
  observeEvent(input$add_vi_layer, {
    if (nchar(input$vi_layer_name) == 0) {
      showNotification("Please provide a layer name.", type = "error")
      return()
    }
    req(input$vi_layer_name, input$upload_vi_tif)
    withProgress(message = "Uploading VI layer...", value = 0.5, {
      tryCatch({
        r <- prepare_uploaded_raster_file(input$upload_vi_tif$datapath)
        current_list <- ms_vi_rasters()
        current_list[[input$vi_layer_name]] <- r
        ms_vi_rasters(current_list)
        shinyjs::reset("upload_vi_tif")
        updateTextInput(session, "vi_layer_name", value = "")
        notify_layer_ready(paste0("VI layer '", input$vi_layer_name, "'"), "upload")
        set_layer_tip("imagery")
        finalize_layer_on_map(redraw = redraw_ms_raster)
      }, error = function(e) {
        showNotification(paste("Error uploading VI:", e$message), type = "error")
      })
    })
  })
  
  observeEvent(input$clear_vi_layers, {
    ms_vi_rasters(list())
    showNotification("All VIs cleared.", type = "warning")
    leafletProxy("imagery_map") %>% clearGroup("MS") %>% removeControl("MS_legend")
  })
  
  output$remove_vi_ui <- renderUI({
    req(length(ms_vi_rasters()) > 0)
    tagList(
      selectInput("remove_vi_select", "Select VI to Remove:", choices = names(ms_vi_rasters())),
      actionButton("remove_selected_vi", "Remove Selected", class = "btn-warning")
    )
  })
  
  observeEvent(input$remove_selected_vi, {
    req(input$remove_vi_select)
    current_list <- ms_vi_rasters()
    current_list[[input$remove_vi_select]] <- NULL
    ms_vi_rasters(current_list)
    showNotification(paste(input$remove_vi_select, "removed."), type = "warning")
    if (length(current_list) == 0) {
      leafletProxy("imagery_map") %>% clearGroup("MS") %>% removeControl("MS_legend")
    }
  })
  
  # --- Elevation & Derivatives Upload Logic ---
  observeEvent(input$upload_elevation_tif, {
    req(input$upload_elevation_tif)
    withProgress(message = "Uploading elevation...", value = 0.5, {
      tryCatch({ 
        r <- raster::raster(input$upload_elevation_tif$datapath)
        if (is.na(raster::crs(r))) {
          showNotification("DEM has no CRS; assigning WGS84 geographic. Verify CRS in your GIS if values look wrong.", type = "warning")
          raster::crs(r) <- raster::crs(TARGET_CRS)
        }
        uploaded_elevation_raster(r)
        notify_layer_ready("Elevation DEM", "upload")
        set_layer_tip("elevation")
        finalize_layer_on_map(redraw = redraw_elevation_layers)
      }, error=function(e)showNotification(e$message, type="error"))
    })
  })
  observeEvent(input$clear_elevation_tif, { shinyjs::reset("upload_elevation_tif"); remove_elevation_data() })
  
  # --- Soil Layers Upload Logic ---
  observeEvent(input$add_soil_layer, {
    if (nchar(input$soil_layer_name) == 0) {
      showNotification("Please provide a layer name.", type = "error")
      return()
    }
    req(input$soil_layer_name, input$upload_soil_tif)
    layer_name <- input$soil_layer_name
    withProgress(message = "Uploading soil layer...", value = 0.5, {
      tryCatch({
        r <- prepare_uploaded_raster_file(input$upload_soil_tif$datapath)
        current_list <- soil_layers()
        current_list[[layer_name]] <- r
        soil_layers(current_list)
        shinyjs::reset("upload_soil_tif")
        updateTextInput(session, "soil_layer_name", value = "")
        notify_layer_ready(paste0("Soil layer '", layer_name, "'"), "upload")
        set_layer_tip("soil")
        finalize_layer_on_map(redraw = redraw_soil_layers)
      }, error = function(e) {
        showNotification(paste("Error uploading soil layer:", e$message), type = "error")
      })
    })
  })
  
  observeEvent(input$clear_soil_layers, {
    soil_layers(list())
    showNotification("All soil layers cleared.", type = "warning")
    leafletProxy("soil_map") %>% clearGroup("Soil") %>% removeControl("Soil_legend")
  })
  
  output$remove_soil_ui <- renderUI({
    req(length(soil_layers()) > 0)
    tagList(
      selectInput("remove_soil_select", "Select Soil Layer to Remove:", choices = names(soil_layers())),
      actionButton("remove_selected_soil", "Remove Selected", class = "btn-warning")
    )
  })
  
  observeEvent(input$remove_selected_soil, {
    req(input$remove_soil_select)
    current_list <- soil_layers()
    current_list[[input$remove_soil_select]] <- NULL
    soil_layers(current_list)
    showNotification(paste(input$remove_soil_select, "removed."), type = "warning")
    if (length(current_list) == 0) {
      leafletProxy("soil_map") %>% clearGroup("Soil") %>% removeControl("Soil_legend")
    }
  })
  
  
  # --- Other Layers Upload Logic ---
  observeEvent(input$add_other_layer, {
    if (nchar(input$other_layer_name) == 0) {
      showNotification("Please provide a layer name.", type = "error")
      return()
    }
    req(input$other_layer_name, input$upload_other_tif)
    layer_name <- input$other_layer_name
    withProgress(message = "Uploading other layer...", value = 0.5, {
      tryCatch({
        r <- prepare_uploaded_raster_file(input$upload_other_tif$datapath)
        current_list <- other_layers()
        current_list[[layer_name]] <- r
        other_layers(current_list)
        shinyjs::reset("upload_other_tif")
        updateTextInput(session, "other_layer_name", value = "")
        notify_layer_ready(paste0("Layer '", layer_name, "'"), "upload")
        set_layer_tip("other")
        finalize_layer_on_map(redraw = redraw_other_layers)
      }, error = function(e) {
        showNotification(paste("Error uploading other layer:", e$message), type = "error")
      })
    })
  })
  
  observeEvent(input$clear_other_layers, {
    other_layers(list())
    showNotification("All other layers cleared.", type = "warning")
    leafletProxy("other_map") %>% clearGroup("Other") %>% removeControl("Other_legend")
  })
  
  output$remove_other_ui <- renderUI({
    req(length(other_layers()) > 0)
    tagList(
      selectInput("remove_other_select", "Select Other Layer to Remove:", choices = names(other_layers())),
      actionButton("remove_selected_other", "Remove Selected", class = "btn-warning")
    )
  })
  
  observeEvent(input$remove_selected_other, {
    req(input$remove_other_select)
    current_list <- other_layers()
    current_list[[input$remove_other_select]] <- NULL
    other_layers(current_list)
    showNotification(paste(input$remove_other_select, "removed."), type = "warning")
    if (length(current_list) == 0) {
      leafletProxy("other_map") %>% clearGroup("Other") %>% removeControl("Other_legend")
    }
  })
  
  # --- Dynamic UI and Redraw Logic for Rasters ---
  # Planet
  observeEvent({
    uploaded_planet_raster()
    vi_rasters()
  }, {
    if (!is.null(uploaded_planet_raster()) || length(vi_rasters()) > 0) {
      choices <- if (length(vi_rasters()) > 0) names(vi_rasters()) else names(uploaded_planet_raster())
      output$planet_band_selector_ui <- renderUI({
        div(hr(), selectInput("planet_band_select", "Select to Display:", choices = choices))
      })
      output$vi_calculator_ui <- renderUI({
        div(hr(), h4("Calculate Vegetation Indices"),
            p(class = "text-muted", style = "font-size:12px; margin-bottom:6px;",
              "NDVI, NDRE, GNDVI, OSAVI, CIre, VARI (requires Blue, Green, Red, Red Edge, NIR)."),
            actionButton("calc_vi", "Calculate VIs", class = "btn-modern"))
      })
      # Removed: redraw_planet_raster()  # Let input$planet_band_select trigger it
      shinyjs::delay(250, tryCatch(redraw_planet_raster(), error = function(e) NULL))
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$planet_band_select, {
    shinyjs::delay(30, redraw_planet_raster())
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  
  redraw_planet_raster <- function(layer_name = NULL) {
    layer_name <- layer_name %||% isolate(input$planet_band_select)
    if (!length(layer_name)) return(invisible(NULL))
    r <- if (length(vi_rasters()) > 0) vi_rasters()[[layer_name]] else uploaded_planet_raster()[[layer_name]]
    req(!is.null(r))
    r_disp <- prepare_raster_for_display(r, for_leaflet_overlay = TRUE)
    req(!is.null(r_disp))
    valid_vals <- raster::values(r_disp)
    valid_vals <- valid_vals[is.finite(valid_vals)]
    req(length(valid_vals) > 0)
    val_range <- range(valid_vals, na.rm = TRUE)
    pal <- colorNumeric(viridis(256), domain = val_range, na.color = "transparent")
    layer_id <- paste0("planet_raster_", gsub("[^A-Za-z0-9_]+", "_", layer_name))
    leafletProxy("imagery_map") %>%
      clearGroup("Planet") %>%
      addRasterImage(
        r_disp, colors = pal, opacity = 0.7, group = "Planet",
        layerId = layer_id, project = FALSE
      ) %>%
      removeControl("Planet_legend") %>%
      addLegend(pal = pal, values = val_range, title = layer_name, layerId = "Planet_legend")
  }
  
  # Sentinel
  observeEvent({
    uploaded_sentinel_raster()
    sentinel_vi_rasters()
  }, {
    if (!is.null(uploaded_sentinel_raster()) || length(sentinel_vi_rasters()) > 0) {
      choices <- if (length(sentinel_vi_rasters()) > 0) names(sentinel_vi_rasters()) else names(uploaded_sentinel_raster())
      output$sentinel_band_selector_ui <- renderUI({
        div(hr(), selectInput("sentinel_band_select", "Select to Display:", choices = choices))
      })
      output$sentinel_vi_calculator_ui <- renderUI({
        div(hr(), h4("Calculate Vegetation Indices"),
            p(class = "text-muted", style = "font-size:12px; margin-bottom:6px;",
              "NDVI, NDRE, GNDVI, OSAVI, CIre, VARI (requires Blue, Green, Red, Red Edge, NIR)."),
            actionButton("calc_sentinel_vi", "Calculate VIs", class = "btn-modern"))
      })
      shinyjs::delay(250, tryCatch(redraw_sentinel_raster(), error = function(e) NULL))
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$sentinel_band_select, {
    shinyjs::delay(30, redraw_sentinel_raster())
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  
  redraw_sentinel_raster <- function(layer_name = NULL) {
    layer_name <- layer_name %||% isolate(input$sentinel_band_select)
    if (!length(layer_name)) return(invisible(NULL))
    r <- if (length(sentinel_vi_rasters()) > 0) {
      sentinel_vi_rasters()[[layer_name]]
    } else {
      uploaded_sentinel_raster()[[layer_name]]
    }
    req(!is.null(r))
    r_disp <- prepare_raster_for_display(r, max_cells = 900000L, for_leaflet_overlay = TRUE)
    req(!is.null(r_disp))
    vals_disp <- raster::values(r_disp)
    valid_vals <- vals_disp[is.finite(vals_disp)]
    if (!length(valid_vals)) {
      showNotification("Selected Sentinel layer has no valid pixels to display.", type = "warning")
    leafletProxy("imagery_map") %>%
      clearGroup("Sentinel") %>%
        removeControl("Sentinel_legend")
      return(invisible(NULL))
    }
    val_range <- range(valid_vals, na.rm = TRUE)
    if (!all(is.finite(val_range)) || val_range[1] == val_range[2]) {
      pad <- max(abs(val_range[1]) * 0.01, 1e-6)
      val_range <- c(val_range[1] - pad, val_range[2] + pad)
    }
    pal <- colorNumeric(viridis(256), domain = val_range, na.color = "transparent")
    layer_id <- paste0("sentinel_raster_", gsub("[^A-Za-z0-9_]+", "_", layer_name))
    tryCatch({
      leafletProxy("imagery_map") %>%
        clearGroup("Sentinel") %>%
        addRasterImage(
          r_disp, colors = pal, opacity = 0.7, group = "Sentinel",
          layerId = layer_id, project = FALSE
        ) %>%
      removeControl("Sentinel_legend") %>%
        addLegend(
          pal = pal, values = val_range, title = layer_name, layerId = "Sentinel_legend"
        )
    }, error = function(e) {
      showNotification(paste("Could not render Sentinel layer on map:", e$message), type = "warning")
      leafletProxy("imagery_map") %>%
        clearGroup("Sentinel") %>%
        removeControl("Sentinel_legend")
    })
  }
  
  # MS Imagery
  observeEvent({
    uploaded_ms_raster()
    ms_vi_rasters()
  }, {
    if (!is.null(uploaded_ms_raster()) || length(ms_vi_rasters()) > 0) {
      choices <- if (length(ms_vi_rasters()) > 0) names(ms_vi_rasters()) else names(uploaded_ms_raster())
      output$ms_band_selector_ui <- renderUI({
        div(hr(), selectInput("ms_band_select", "Select to Display:", choices = choices))
      })
      output$ms_vi_calculator_ui <- renderUI({
        if (input$ms_upload_type != "vi") {
          div(hr(), h4("Calculate Vegetation Indices"),
              p(class = "text-muted", style = "font-size:12px; margin-bottom:6px;",
                "NDVI, NDRE, GNDVI, OSAVI, CIre, VARI (requires Blue, Green, Red, Red Edge, NIR)."),
              actionButton("calc_ms_vi", "Calculate VIs", class = "btn-modern"))
        }
      })
      # Removed: redraw_ms_raster()  # Let input$ms_band_select trigger it
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$ms_band_select, {
    shinyjs::delay(30, redraw_ms_raster())
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  
  redraw_ms_raster <- function(layer_name = NULL) {
    layer_name <- layer_name %||% isolate(input$ms_band_select)
    if (!length(layer_name)) return(invisible(NULL))
    r <- if (length(ms_vi_rasters()) > 0) ms_vi_rasters()[[layer_name]] else uploaded_ms_raster()[[layer_name]]
    req(!is.null(r))
    r_disp <- prepare_raster_for_display(r, for_leaflet_overlay = TRUE)
    req(!is.null(r_disp))
    valid_vals <- raster::values(r_disp)
    valid_vals <- valid_vals[is.finite(valid_vals)]
    req(length(valid_vals) > 0)
    val_range <- range(valid_vals, na.rm = TRUE)
    pal <- colorNumeric(viridis(256), domain = val_range, na.color = "transparent")
    layer_id <- paste0("ms_raster_", gsub("[^A-Za-z0-9_]+", "_", layer_name))
    leafletProxy("imagery_map") %>%
      clearGroup("MS") %>%
      addRasterImage(
        r_disp, colors = pal, opacity = 0.7, group = "MS",
        layerId = layer_id, project = FALSE
      ) %>%
      removeControl("MS_legend") %>%
      addLegend(pal = pal, values = val_range, title = layer_name, layerId = "MS_legend")
  }
  
  # Elevation — keep selectInput mounted; refresh choices with updateSelectInput (avoid removeUI races).
  elevation_layer_choices <- reactive({
    ch <- character(0)
    if (!is.null(uploaded_elevation_raster())) ch <- c(ch, "DEM")
    aux <- elevation_aux_layers()
    if (length(aux) > 0L) ch <- c(ch, names(aux))
    unique(ch)
  })

  update_elevation_layer_select <- function() {
    ch <- elevation_layer_choices()
    placeholder <- c("Load a DEM to display layers" = "")
    cur <- isolate(input$elevation_layer_select)
    if (!length(ch)) {
      tryCatch(
        updateSelectInput(session, "elevation_layer_select", choices = placeholder, selected = ""),
        error = function(e) invisible(NULL)
      )
      return(invisible(character(0)))
    }
    if (is.null(cur) || !length(cur) || !nzchar(cur) || !(cur %in% ch)) {
      cur <- ch[1L]
    }
    apply_update <- function() {
      updateSelectInput(session, "elevation_layer_select", choices = ch, selected = cur)
    }
    tryCatch(apply_update(), error = function(e) {
      shinyjs::delay(120, function() {
        tryCatch(apply_update(), error = function(e2) invisible(NULL))
      })
    })
    invisible(cur)
  }

  output$elevation_selector_ui <- renderUI({
    tagList(
      hr(),
      selectInput(
        "elevation_layer_select",
        "Select Layer to Display:",
        choices = c("Load a DEM to display layers" = ""),
        selected = ""
      )
    )
  })
  outputOptions(output, "elevation_selector_ui", suspendWhenHidden = FALSE)

  output$elevation_derivative_ui <- renderUI({
    if (is.null(uploaded_elevation_raster())) return(NULL)
    tagList(
      hr(),
      h4("Calculate Terrain Variables"),
      p(class = "text-muted", style = "font-size:12px; margin:0 0 8px 0;",
        "Builds slope, aspect, TPI, and TWI from the loaded DEM in one step (retrieve or upload)."),
      actionButton("calc_derivatives", "Calculate Slope, Aspect, TPI, TWI", class = "btn-modern")
    )
  })
  outputOptions(output, "elevation_derivative_ui", suspendWhenHidden = FALSE)

  output$derivative_download_ui <- renderUI({
    aux <- elevation_aux_layers()
    if (!any(c("Slope", "Aspect", "TPI", "TWI") %in% names(aux))) return(NULL)
    tagList(
      hr(),
      h4("Download Terrain Variables"),
      downloadButton("download_slope", "Download Slope"),
      downloadButton("download_aspect", "Download Aspect"),
      downloadButton("download_tpi", "Download TPI"),
      downloadButton("download_twi", "Download TWI")
    )
  })
  outputOptions(output, "derivative_download_ui", suspendWhenHidden = FALSE)

  observe({
    elevation_layer_choices()
    update_elevation_layer_select()
  })

  observeEvent({
    uploaded_elevation_raster()
    elevation_aux_layers()
  }, {
    sel <- update_elevation_layer_select()
    if (length(sel)) shinyjs::delay(200, redraw_elevation_layers(sel))
  }, ignoreNULL = FALSE)

  observeEvent(input$variables_subtabs, {
    if (!identical(input$variables_subtabs, "elevation")) return(invisible(NULL))
    shinyjs::delay(80, update_elevation_layer_select)
    shinyjs::delay(350, function() {
      sel <- update_elevation_layer_select()
      if (length(sel)) redraw_elevation_layers(sel)
    })
  }, ignoreInit = TRUE)

  observeEvent(input$elevation_layer_select, {
    layer <- isolate(input$elevation_layer_select)
    if (!length(layer) || !nzchar(layer)) return(invisible(NULL))
    shinyjs::delay(30, redraw_elevation_layers(layer))
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  
  redraw_elevation_layers <- function(layer_name = NULL) {
    layer_name <- layer_name %||% isolate(input$elevation_layer_select)
    if (!length(layer_name) || !nzchar(layer_name)) return(invisible(NULL))
    
    r <- if (identical(layer_name, "DEM")) {
      uploaded_elevation_raster()
    } else {
      elevation_aux_layers()[[layer_name]]
    }
    
    if (is.null(r)) return(invisible(NULL))
    r_disp <- prepare_raster_for_display(r, for_leaflet_overlay = TRUE)
    if (is.null(r_disp)) return(invisible(NULL))
    valid_vals <- raster::values(r_disp)
    valid_vals <- valid_vals[is.finite(valid_vals)]
    if (!length(valid_vals)) return(invisible(NULL))
    val_range <- range(valid_vals, na.rm = TRUE)
    pal <- colorNumeric(viridis(256), domain = val_range, na.color = "transparent")
    layer_id <- paste0("elevation_raster_", gsub("[^A-Za-z0-9_]+", "_", layer_name))
    tryCatch({
    leafletProxy("elevation_map") %>%
      clearGroup("Elevation") %>%
        addRasterImage(
          r_disp, colors = pal, opacity = 0.7, group = "Elevation",
          layerId = layer_id, project = FALSE
        ) %>%
      removeControl("Elevation_legend") %>%
        addLegend(pal = pal, values = val_range, title = layer_name, layerId = "Elevation_legend")
    }, error = function(e) {
      showNotification(paste("Could not render elevation layer on map:", e$message), type = "warning")
    })
    invisible(NULL)
  }
  
  # Soil
  redraw_soil_layers <- function() {
    sl <- soil_layers()
    if (length(sl) == 0) {
      try(
        leafletProxy("soil_map") %>% clearGroup("Soil") %>% removeControl("Soil_legend"),
        silent = TRUE
      )
      return(invisible(NULL))
    }
    sel <- input$soil_layer_select
    if (is.null(sel) || !sel %in% names(sl)) {
      sel <- names(sl)[[1]]
    }
    r <- sl[[sel]]
    if (is.null(r)) {
      return(invisible(NULL))
    }
    is_f <- tryCatch(raster::is.factor(r), error = function(e) FALSE)
    r <- prepare_raster_for_display(r, for_leaflet_overlay = !is_f)
    val_range <- range(raster::values(r), na.rm = TRUE)
    if (any(!is.finite(val_range))) {
      val_range <- c(0, 1)
    }
    pal <- colorNumeric(viridis(256), domain = val_range, na.color = "transparent")
    leafletProxy("soil_map") %>%
      clearGroup("Soil") %>%
      addRasterImage(r, colors = pal, opacity = 0.7, group = "Soil", project = is_f) %>%
      removeControl("Soil_legend") %>%
      addLegend(pal = pal, values = val_range, title = sel, layerId = "Soil_legend")
  }
  
  output$soil_selector_ui <- renderUI({
    div(hr(), selectInput("soil_layer_select", "Select Soil Layer to Display:", choices = NULL))
  })
  
  observe({
    new_choices <- names(soil_layers())
    
    current_selected <- input$soil_layer_select
    if (!current_selected %in% new_choices && length(new_choices) > 0) {
      current_selected <- new_choices[1]
    }
    
    updateSelectInput(session, "soil_layer_select", choices = new_choices, selected = current_selected)
    
    if (length(new_choices) > 0) {
      redraw_soil_layers()
    }
  })
  
  observeEvent(input$soil_layer_select, { redraw_soil_layers() }, ignoreInit = TRUE)
  
  # Other
  output$other_selector_ui <- renderUI({
    div(hr(), selectInput("other_layer_select", "Select Other Layer to Display:", choices = NULL))
  })
  
  observe({
    new_choices <- names(other_layers())
    
    current_selected <- input$other_layer_select
    if (!current_selected %in% new_choices && length(new_choices) > 0) current_selected <- new_choices[1]
    
    updateSelectInput(session, "other_layer_select", choices = new_choices, selected = current_selected)
    
    # redraw_other_layers()
  })
  
  observeEvent(input$other_layer_select, { redraw_other_layers() }, ignoreInit = TRUE)
  
  redraw_other_layers <- function(){
    req(input$other_layer_select, length(other_layers()) > 0)
    r <- other_layers()[[input$other_layer_select]]
    is_f <- tryCatch(raster::is.factor(r), error = function(e) FALSE)
    r <- prepare_raster_for_display(r, for_leaflet_overlay = !is_f)
    val_range <- range(raster::values(r), na.rm = TRUE)
    pal <- colorNumeric(viridis(256), domain = val_range, na.color="transparent")
    leafletProxy("other_map") %>%
      clearGroup("Other") %>%
      addRasterImage(r, colors=pal, opacity=0.7, group="Other", project = is_f) %>%
      removeControl("Other_legend") %>%
      addLegend(pal=pal, values=val_range, title=input$other_layer_select, layerId="Other_legend")
  }
  # --- Calculation Logic ---
  # Planet VI
  observeEvent(input$calc_vi, {
    req(!is.null(uploaded_planet_raster()))
    withProgress(message = "Calculating vegetation indices", value = 0, {
      tryCatch({
      r_stack <- uploaded_planet_raster()
        incProgress(0.15, detail = "Reading bands...")
        vi_list <- compute_standard_vis(r_stack, band_suffix = "")
        incProgress(0.75, detail = "Storing layers...")
      vi_rasters(vi_list)
        incProgress(1, detail = "Done")
        showNotification(
          paste0("Vegetation indices calculated (", length(vi_list), " layers). Spectral stack released from memory to save RAM; use Retrieve again if you need raw bands."),
          type = "message"
        )
        output$vi_download_ui <- renderUI(build_vi_download_ui("", ""))
        uploaded_planet_raster(NULL)
      }, error = function(e) {
        showNotification(paste("Error calculating vegetation indices:", e$message), type = "error", duration = 12)
      })
    })
  })
  
  # Sentinel VI
  observeEvent(input$calc_sentinel_vi, {
    req(!is.null(uploaded_sentinel_raster()))
    withProgress(message = "Calculating vegetation indices", value = 0, {
      tryCatch({
      r_stack <- uploaded_sentinel_raster()
      band_suffix <- if (identical(sentinel_retrieval_used(), "median")) "_median" else ""
        incProgress(0.15, detail = "Reading bands...")
        vi_list <- compute_standard_vis(r_stack, band_suffix = band_suffix)
        if (nzchar(band_suffix)) names(vi_list) <- paste0(names(vi_list), band_suffix)
        incProgress(0.75, detail = "Storing layers...")
      sentinel_vi_rasters(vi_list)
        incProgress(1, detail = "Done")
      showNotification(
        if (identical(sentinel_retrieval_used(), "median")) {
          paste0(
            "Vegetation indices for Sentinel median composite calculated (", length(vi_list),
            " layers). Raw stack, search results, and scene metadata were cleared to free memory; only VIs are kept. Re-run Search if you need another retrieve."
          )
        } else {
          paste0(
            "Vegetation indices for Sentinel data calculated (", length(vi_list),
            " layers). Raw stack, search results, and scene metadata were cleared to free memory; only VIs are kept. Re-run Search if you need another retrieve."
          )
        },
        type = "message"
      )
        label_suf <- if (nzchar(band_suffix)) band_suffix else ""
        output$sentinel_vi_download_ui <- renderUI(build_vi_download_ui("sentinel", label_suf))
        shinyjs::delay(300, tryCatch(redraw_sentinel_raster(names(vi_list)[[1]]), error = function(e) NULL))
        uploaded_sentinel_raster(NULL)
        # Free retrieve-side memory/disk: STAC results, scene/date metadata, and the temp spectral stack TIFF (VIs remain).
        sp <- sentinel_result()
        if (is.character(sp) && length(sp) == 1L && nzchar(sp)) {
          try(unlink(sp), silent = TRUE)
        }
        sentinel_result(NULL)
        sentinel_search_results(NULL)
        sentinel_retrieval_meta(NULL)
        output$sentinel_select_ui <- renderUI({})
        output$sentinel_download_ui <- renderUI({})
        sync_sampling_after_covariate_change(
          notify = paste(
            "Sentinel vegetation indices are ready for sampling.",
            "Check covariate layers on the Sampling tab, then generate new sample points.",
            "Re-run Technique comparison first if you use Generate using recommendation."
          )
        )
      }, error = function(e) {
        showNotification(paste("Error calculating Sentinel vegetation indices:", e$message), type = "error", duration = 12)
      })
    })
  })
  
  # MS VI
  observeEvent(input$calc_ms_vi, {
    req(!is.null(uploaded_ms_raster()))
    withProgress(message = "Calculating vegetation indices", value = 0, {
      tryCatch({
      r_stack <- uploaded_ms_raster()
        incProgress(0.15, detail = "Reading bands...")
        vi_list <- compute_standard_vis(r_stack, band_suffix = "")
        incProgress(0.75, detail = "Storing layers...")
      ms_vi_rasters(vi_list)
        incProgress(1, detail = "Done")
        showNotification(
          paste0("Vegetation indices for MS data calculated (", length(vi_list), " layers). Spectral stack released from memory."),
          type = "message"
        )
        output$ms_vi_download_ui <- renderUI(build_vi_download_ui("ms", ""))
        uploaded_ms_raster(NULL)
      }, error = function(e) {
        showNotification(paste("Error calculating MS vegetation indices:", e$message), type = "error", duration = 12)
      })
    })
  })
  
  # Elevation Derivatives
  observeEvent(input$calc_derivatives, {
    req(!is.null(uploaded_elevation_raster()))
    withProgress(message="Calculating Terrain Variables", value=0, {
      dem_src <- uploaded_elevation_raster()
      dem_crs <- tryCatch(raster::crs(dem_src, asText = TRUE), error = function(e) "")
      b <- digitized_features()

      dem_work <- dem_src
      if (nzchar(dem_crs) && grepl("longlat", dem_crs, ignore.case = TRUE)) {
        mc <- if (!is.null(b) && nrow(b) > 0) {
          local_metric_crs_from_sf(b)
        } else {
          "+proj=merc +datum=WGS84 +units=m +no_defs"
        }
        dem_work <- tryCatch(project_raster_bilinear(dem_src, crs = mc), error = function(e) dem_src)
      }
      
      incProgress(0.2, detail="Calculating Slope")
      slope_w <- raster::terrain(dem_work, opt='slope', unit='degrees')
      
      incProgress(0.2, detail="Calculating Aspect")
      aspect_w <- raster::terrain(dem_work, opt='aspect', unit='degrees')
      
      incProgress(0.2, detail="Calculating TPI")
      tpi_w <- raster::terrain(dem_work, opt='TPI')
      
      incProgress(0.2, detail="Calculating TWI")
      slope_rad_w <- raster::terrain(dem_work, opt='slope', unit='radians')
      slope_rad_w[slope_rad_w == 0] <- 0.001
      twi_w <- log((raster::cellStats(dem_work, stat='mean') + 1) / (tan(slope_rad_w) + 0.001))

      snap_to_dem <- function(x) {
        if (raster::compareCRS(x, dem_src)) return(x)
        tryCatch(project_raster_bilinear(x, to = dem_src), error = function(e) x)
      }
      
      incProgress(0.2, detail="Finalizing")
      current_list <- elevation_aux_layers()
      current_list[["Slope"]] <- snap_to_dem(slope_w)
      current_list[["Aspect"]] <- snap_to_dem(aspect_w)
      current_list[["TPI"]] <- snap_to_dem(tpi_w)
      current_list[["TWI"]] <- snap_to_dem(twi_w)
      elevation_aux_layers(current_list)
      
      showNotification("Terrain variables calculated: Slope, Aspect, TPI, and TWI.", type="message")
      shinyjs::delay(100, update_elevation_layer_select)
      shinyjs::delay(350, redraw_elevation_layers(isolate(input$elevation_layer_select) %||% "Slope"))
    })
  })
  
  render_auto_points_map <- function(pts) {
    if (is.null(pts) || nrow(pts) == 0) return(invisible(NULL))
    pts <- st_transform(pts, 4326)
    # Clear Leaflet draw feature layers so stale editable points do not persist across new runs.
    session$sendCustomMessage("clearLeafletDrawFeatures", list(mapId = "sampling_auto_map"))
    leafletProxy("sampling_auto_map") %>%
      clearGroup("Sample Points") %>%
      addMarkers(
        data = pts,
        layerId = ~as.character(ID),
        group = "Sample Points",
        popup = ~paste("Point ID:", ID)
      )
    if (!is.null(digitized_features()) && nrow(digitized_features()) > 0) {
      apply_boundary_overlay_to_maps()
    }
  }
  
  assign_points_to_zones <- function(df_points, zone_model) {
    if (is.null(zone_model) || is.null(df_points) || nrow(df_points) == 0) return(NULL)
    cov_names <- zone_model$cov_names
    if (length(cov_names) == 0 || !all(cov_names %in% names(df_points))) return(NULL)
    x <- as.matrix(df_points[, cov_names, drop = FALSE])
    for (j in seq_len(ncol(x))) {
      x[, j] <- (x[, j] - zone_model$scale_center[j]) / ifelse(zone_model$scale_scale[j] == 0, 1, zone_model$scale_scale[j])
    }
    centers <- as.matrix(zone_model$centers)
    dmat <- sapply(seq_len(nrow(centers)), function(i) rowSums((x - matrix(centers[i, ], nrow = nrow(x), ncol = ncol(x), byrow = TRUE))^2))
    if (is.null(dim(dmat))) dmat <- matrix(dmat, ncol = 1)
    max.col(-dmat, ties.method = "first")
  }

  # Neyman allocation: points per zone ∝ (zone pixel count) × mean(within-zone column variance).
  neyman_allocate_points_across_zones <- function(z, cov_df, n_points) {
    if (is.null(cov_df) || nrow(cov_df) == 0L || length(z) != nrow(cov_df)) {
      return(structure(integer(0), names = character(0)))
    }
    n_points <- safe_sample_n_points(n_points, max_n = nrow(cov_df))
    z <- as.integer(z)
    if (any(is.na(z))) {
      ok <- stats::complete.cases(cov_df) & !is.na(z)
      if (!any(ok)) return(structure(integer(0), names = character(0)))
      z <- z[ok]
      cov_df <- cov_df[ok, , drop = FALSE]
    }
    uz <- sort(unique(z[!is.na(z)]))
    if (!length(uz)) return(structure(integer(0), names = character(0)))
    zf <- factor(z, levels = uz)
    ztab <- table(zf)
    znames <- names(ztab)

    zone_var <- vapply(znames, function(zn) {
      idx <- which(as.integer(zf) == as.integer(zn))
      zz <- cov_df[idx, , drop = FALSE]
      if (nrow(zz) < 2L) return(0)
      v <- apply(zz, 2, stats::var, na.rm = TRUE)
      v <- v[is.finite(v)]
      if (!length(v)) 0 else mean(v, na.rm = TRUE)
    }, numeric(1))
    zone_var[!is.finite(zone_var)] <- 0

    w <- as.numeric(ztab) * pmax(zone_var, 1e-8)
    if (!is.finite(sum(w)) || sum(w) <= 0) w <- as.numeric(ztab)
    if (!length(w) || sum(w) <= 0) w <- rep(1, length(ztab))

    alloc <- floor((w / sum(w)) * n_points)
    alloc[!is.finite(alloc)] <- 0L
    names(alloc) <- znames

    if (n_points >= length(alloc)) {
      low <- which(alloc < 1L)
      if (length(low)) alloc[low] <- 1L
    }
    dn <- n_points - sum(alloc)
    if (dn != 0L) {
      ord <- order(w, decreasing = TRUE)
      ii <- 1L
      guard <- 0L
      while (dn != 0L && length(ord) > 0 && guard < (abs(dn) * length(ord) + 5L)) {
        guard <- guard + 1L
        zi <- znames[ord[ii]]
        cap <- suppressWarnings(as.integer(ztab[zi]))
        if (is.na(cap)) cap <- 0L
        cur <- as.integer(alloc[[zi]])
        if (is.na(cur)) cur <- 0L
        if (isTRUE(dn > 0L) && cap > 0L && cur < cap) {
          alloc[[zi]] <- cur + 1L
          dn <- dn - 1L
        } else if (isTRUE(dn < 0L) && cur > 0L) {
          alloc[[zi]] <- cur - 1L
          dn <- dn + 1L
        }
        ii <- ii + 1L
        if (ii > length(ord)) ii <- 1L
      }
    }
    alloc <- as.integer(alloc)
    alloc[!is.finite(alloc) | is.na(alloc)] <- 0L
    names(alloc) <- znames
    alloc
  }

  hybrid_zone_clhs_indices <- function(cov_df, n_points, kz, z_clusters = NULL) {
    if (is.null(cov_df) || nrow(cov_df) == 0 || ncol(cov_df) == 0) return(integer(0))
    cov_df <- as.data.frame(cov_df)
    row_map <- seq_len(nrow(cov_df))
    z_pre <- if (!is.null(z_clusters) && length(z_clusters) == nrow(cov_df)) {
      as.integer(z_clusters)
    } else {
      NULL
    }
    cov_df <- cov_df[, vapply(cov_df, is.numeric, logical(1)), drop = FALSE]
    cov_df <- cov_df[, vapply(cov_df, function(v) stats::sd(v, na.rm = TRUE) > 0, logical(1)), drop = FALSE]
    keep <- stats::complete.cases(cov_df)
    if (!is.null(z_pre)) keep <- keep & !is.na(z_pre)
    if (!any(keep)) return(integer(0))
    row_map <- row_map[keep]
    cov_df <- cov_df[keep, , drop = FALSE]
    if (!ncol(cov_df) || !nrow(cov_df)) return(integer(0))
    n_points <- safe_sample_n_points(n_points, max_n = nrow(cov_df))
    kz_use <- min(max(2L, safe_sample_n_points(kz, max_n = nrow(cov_df))), n_points, nrow(cov_df))
    if (!is.null(z_pre)) {
      z <- z_pre[keep]
    } else {
      x_scaled <- scale(cov_df)
      km <- stats::kmeans(x_scaled, centers = kz_use, iter.max = 50, nstart = 3)
      z <- as.integer(km$cluster)
    }
    alloc <- neyman_allocate_points_across_zones(z, cov_df, n_points)
    if (length(alloc) == 0L) return(integer(0))

    out_idx <- integer(0)
    for (zname in names(alloc)) {
      n_take <- suppressWarnings(as.integer(alloc[[zname]]))
      if (length(n_take) != 1L || is.na(n_take) || n_take <= 0L) next
      idx_zone <- which(z == as.integer(zname))
      if (length(idx_zone) == 0L) next
      n_take <- min(n_take, length(idx_zone))
      zone_df <- cov_df[idx_zone, , drop = FALSE]
      if (length(idx_zone) <= n_take) {
        out_idx <- c(out_idx, idx_zone)
      } else {
        id_local <- tryCatch({
          clhs_sample_indices(clhs::clhs(zone_df, size = n_take, progress = FALSE, simple = TRUE))
        }, error = function(e) sample.int(length(idx_zone), n_take))
        out_idx <- c(out_idx, idx_zone[id_local])
      }
    }
    out_idx <- unique(out_idx)
    if (length(out_idx) < n_points) {
      rem <- setdiff(seq_len(nrow(cov_df)), out_idx)
      extra <- min(length(rem), n_points - length(out_idx))
      if (length(extra) == 1L && !is.na(extra) && extra > 0L) {
        out_idx <- c(out_idx, sample(rem, extra))
      }
    }
    unique(row_map[out_idx])
  }

  filter_complete_sampling_grid <- function(df) {
    if (is.null(df) || nrow(df) < 1L) {
      return(list(df = df, pop_xy = NULL, cov_df = NULL, pop_n = 0L))
    }
    pop_xy <- df[, c("x", "y"), drop = FALSE]
    cov_df <- df[, setdiff(names(df), c("x", "y")), drop = FALSE]
    cov_df <- cov_df[, vapply(cov_df, is.numeric, logical(1)), drop = FALSE]
    cov_df <- cov_df[, vapply(cov_df, function(v) stats::sd(v, na.rm = TRUE) > 0, logical(1)), drop = FALSE]
    keep <- stats::complete.cases(cov_df)
    list(
      df = df[keep, , drop = FALSE],
      pop_xy = pop_xy[keep, , drop = FALSE],
      cov_df = cov_df[keep, , drop = FALSE],
      pop_n = sum(keep)
    )
  }

  sample_indices_for_compare_method <- function(
    method,
    n,
    pop_xy,
    pop_cov,
    pop_n,
    rng_seed,
    zone_clusters = NULL,
    zone_k = NULL,
    strict_count = FALSE
  ) {
    method <- as.character(method)[1L]
    n <- suppressWarnings(as.integer(n))
    pop_n <- suppressWarnings(as.integer(pop_n))
    if (length(n) != 1L || is.na(n) || n < 1L || length(pop_n) != 1L || is.na(pop_n) || pop_n < 1L) {
      return(integer(0))
    }
    n <- min(n, pop_n)
    seed <- suppressWarnings(as.integer(rng_seed))
    if (is.na(seed)) seed <- 123L

    if (identical(method, "Simple Random")) {
      set.seed(seed)
      return(sample.int(pop_n, size = n))
    }
    if (identical(method, "Systematic Spread")) {
      return(systematic_spread_indices(n, pop_xy, rng_seed = seed + 7919L))
    }
    if (identical(method, "Spread + cLHS")) {
      return(spread_clhs_indices(n, pop_xy, pop_cov, rng_seed = seed + 12007L))
    }
    if (identical(method, "cLHS")) {
      set.seed(seed)
      clhs_res <- tryCatch(
        clhs::clhs(pop_cov, size = n, progress = FALSE, simple = TRUE),
        error = function(e) NULL
      )
      idx_clhs <- clhs_sample_indices(clhs_res)
      if (isTRUE(strict_count)) return(idx_clhs)
      return(compare_ensure_n_indices(idx_clhs, pop_n, n))
    }
    if (identical(method, "Zone-based")) {
      if (is.null(zone_clusters) || length(zone_clusters) != pop_n) return(integer(0))
      set.seed(seed)
      alloc <- neyman_allocate_points_across_zones(as.integer(zone_clusters), pop_cov, n)
      idx_out <- integer(0)
      for (zname in names(alloc)) {
        idx_zone <- which(as.integer(zone_clusters) == as.integer(zname))
        n_take <- suppressWarnings(as.integer(alloc[[zname]]))
        if (length(n_take) != 1L || is.na(n_take) || n_take < 1L) next
        n_take <- min(length(idx_zone), n_take)
        if (n_take > 0L) idx_out <- c(idx_out, sample(idx_zone, n_take))
      }
      idx_out <- unique(idx_out)
      if (isTRUE(strict_count)) return(idx_out)
      return(compare_ensure_n_indices(idx_out, pop_n, n))
    }
    if (identical(method, "Hybrid Zonal cLHS")) {
      if (is.null(zone_clusters) || is.null(zone_k)) return(integer(0))
      set.seed(seed)
      idx_h <- hybrid_zone_clhs_indices(
        cov_df = pop_cov,
        n_points = n,
        kz = as.integer(zone_k),
        z_clusters = as.integer(zone_clusters)
      )
      if (isTRUE(strict_count)) return(idx_h)
      return(compare_ensure_n_indices(idx_h, pop_n, n))
    }
    integer(0)
  }

  finalize_zone_sample_from_indices <- function(df, cov_df, idx, combined_stack, zone_clusters, method_label, zone_k = NULL) {
    zc <- as.integer(zone_clusters)
    if (length(zc) != nrow(cov_df)) {
      kz_fb <- suppressWarnings(as.integer(zone_k))
      if (is.na(kz_fb) || kz_fb < 2L) kz_fb <- min(12L, nrow(cov_df))
      kz_fb <- min(kz_fb, nrow(cov_df))
      scaled_cov <- scale(cov_df)
      km <- stats::kmeans(scaled_cov, centers = kz_fb, iter.max = 50, nstart = 3)
      zc <- as.integer(km$cluster)
      zonal_cluster_model(list(
        cov_names = colnames(cov_df),
        centers = km$centers,
        scale_center = attr(scaled_cov, "scaled:center"),
        scale_scale = attr(scaled_cov, "scaled:scale")
      ))
    } else {
      scaled_cov <- scale(cov_df)
      zonal_cluster_model(list(
        cov_names = colnames(cov_df),
        centers = NULL,
        scale_center = attr(scaled_cov, "scaled:center"),
        scale_scale = attr(scaled_cov, "scaled:scale")
      ))
    }
    df$zone <- zc
    boundary_local <- digitized_features()
    zb <- build_zone_raster_from_cells(
      combined_stack[[1]], df, zone_col = "zone", boundary_sf = boundary_local
    )
    zone_r <- zb$raster
    zone_levels <- zb$levels
    if (is.null(zone_r) || !length(zone_levels)) {
      stop("Could not build zone map layer.", call. = FALSE)
    }
    zonal_zone_raster(zone_r)
    zonal_zone_count(as.integer(length(zone_levels)))
    render_sampling_zones_on_map(zone_r, show_zones = TRUE)
    sampled_df <- df[idx, , drop = FALSE]
    pts <- st_as_sf(sampled_df, coords = c("x", "y"), crs = crs(combined_stack))
    pts$ID <- seq_len(nrow(pts))
    zone_summary <- as.data.frame(table(pts$zone), stringsAsFactors = FALSE)
    names(zone_summary) <- c("Zone", "Sample_Count")
    zone_summary$Zone <- as.integer(as.character(zone_summary$Zone))
    zone_summary <- zone_summary[order(zone_summary$Zone), , drop = FALSE]
    zonal_cluster_summary(zone_summary)
    mean_vars <- names(cov_df)
    zone_means <- stats::aggregate(
      df[, mean_vars, drop = FALSE],
      by = list(Zone = as.integer(df$zone)),
      FUN = function(v) mean(v, na.rm = TRUE)
    )
    zone_means <- zone_means[order(zone_means$Zone), , drop = FALSE]
    zone_means <- dplyr::left_join(zone_summary, zone_means, by = "Zone")
    for (nm in setdiff(names(zone_means), c("Zone", "Sample_Count"))) {
      zone_means[[nm]] <- round(zone_means[[nm]], 3)
    }
    zonal_cluster_means(zone_means)
    pts <- st_transform(pts, 4326)
    sample_points(pts)
    render_auto_points_map(pts)
    showNotification(
      paste0(method_label, " complete (", nrow(pts), " points; best of ", RECOMMENDED_GENERATION_REPEATS(), " spread replicates)."),
      type = "message",
      duration = 8
    )
    invisible(pts)
  }

  COMPARE_METHOD_NAMES <- c(
    "Simple Random", "Systematic Spread", "Spread + cLHS",
    "cLHS", "Zone-based", "Hybrid Zonal cLHS"
  )

  pick_best_spread_replicate <- function(
    compare_method,
    n_use,
    pop_xy_grid,
    pop_xy_metric,
    cov_df,
    pop_n,
    base_seed,
    n_reps,
    zone_clusters = NULL,
    zone_k = NA_integer_,
    area_m2 = NA_real_,
    target_field_coverage_pct = GENERATION_FIELD_COVERAGE_TARGET_PCT()
  ) {
    best_idx <- NULL
    best_score <- -Inf
    best_fc_pct <- NA_real_
    best_even <- NA_real_
    best_meets_target <- FALSE
    best_rep <- 1L
    best_seed <- base_seed
    target_pct <- suppressWarnings(as.numeric(target_field_coverage_pct))
    if (!is.finite(target_pct)) target_pct <- GENERATION_FIELD_COVERAGE_TARGET_PCT()
    n_reps <- suppressWarnings(as.integer(n_reps))
    if (!is.finite(n_reps) || n_reps < 1L) n_reps <- RECOMMENDED_GENERATION_REPEATS()
    for (rep in seq_len(n_reps)) {
      rep_seed <- base_seed + 9000L + rep * 17L
      idx <- tryCatch(
        sample_indices_for_compare_method(
          compare_method,
          n_use,
          pop_xy_grid,
          cov_df,
          pop_n,
          rep_seed,
          zone_clusters = zone_clusters,
          zone_k = zone_k,
          strict_count = TRUE
        ),
        error = function(e) integer(0)
      )
      idx <- spread_score_indices(idx, pop_n, n_use)
      if (length(idx) < 1L) next
      idx_deploy <- compare_ensure_n_indices(idx, pop_n, n_use)
      met <- spread_replicate_metrics(
        idx_deploy, pop_xy_grid, pop_xy_metric, n_use, area_m2
      )
      sc <- met$pick_score
      fc <- met$field_coverage_pct
      if (!is.finite(sc) && !is.finite(fc)) next
      if (spread_pick_beats_best(
        fc, sc, best_fc_pct, best_score, best_meets_target, target_pct = target_pct
      )) {
        best_score <- sc
        best_fc_pct <- fc
        best_even <- met$even_spread_idx
        best_meets_target <- is.finite(fc) && fc >= target_pct
        best_idx <- idx_deploy
        best_rep <- rep
        best_seed <- rep_seed
      }
    }
    list(
      idx = best_idx,
      score = best_score,
      field_coverage_pct = best_fc_pct,
      even_spread_idx = best_even,
      rep = best_rep,
      seed = best_seed
    )
  }

  run_automatic_samples_spread_pick <- function(
    compare_method,
    n_target,
    base_seed = NULL,
    buffer_m = NULL,
    update_comparison_spread = FALSE,
    comparison_snapshot = NULL,
    n_reps = NULL,
    target_field_coverage_pct = GENERATION_FIELD_COVERAGE_TARGET_PCT(),
    polish_aggressive = FALSE,
    from_current_settings = FALSE,
    progress_message = "Generating samples (even spread pick)"
  ) {
    if (!identical(input$sampling_method, "automatic")) {
      showNotification("Switch to 'I want to sample automatically', then generate again.", type = "warning", duration = 8)
      return(invisible(NULL))
    }
    compare_method <- as.character(compare_method)[1L]
    if (!compare_method %in% COMPARE_METHOD_NAMES) {
      showNotification(paste0("Unknown sampling method: ", compare_method), type = "error")
      return(invisible(NULL))
    }
    n_target <- suppressWarnings(as.integer(n_target))
    if (is.na(n_target) || n_target < 1L) {
      showNotification("Invalid sample size.", type = "warning")
      return(invisible(NULL))
    }
    if (isTRUE(update_comparison_spread) && is.null(comparison_snapshot)) {
      comparison_snapshot <- comparison_results()
    }
    all_r <- sampling_selected_rasters()
    if (length(all_r) == 0L) {
      showNotification("No raster layers available for sampling.", type = "warning")
      return(invisible(NULL))
    }
    if (compare_method %in% c("Zone-based", "Hybrid Zonal cLHS")) {
      kz_chk <- resolve_zone_k_for_generation()
      if (is.na(kz_chk) || kz_chk < 2L) return(invisible(NULL))
    }

    on.exit(unlock_generate_sample_buttons(), add = TRUE)
    lock_action_button("generate_samples_custom")
    lock_action_button("generate_samples_recommended")

    n_reps <- suppressWarnings(as.integer(n_reps))
    if (!is.finite(n_reps) || n_reps < 1L) {
      n_reps <- RECOMMENDED_GENERATION_REPEATS()
    }
    target_fc <- suppressWarnings(as.numeric(target_field_coverage_pct))
    if (!is.finite(target_fc)) target_fc <- GENERATION_FIELD_COVERAGE_TARGET_PCT()
    sampling_spread_pick_context(NULL)
    withProgress(message = progress_message, value = 0, {
      zonal_cluster_summary(NULL)
      zonal_cluster_means(NULL)
      zonal_cluster_model(NULL)
      clhs_similarity_zone(NULL)
      clhs_weak_gps_zone(NULL)
      clhs_similarity_threshold(NULL)
      clhs_similarity_polygon_count(NULL)
      clhs_weak_zone_area_ha(NULL)
      adaptive_recommendation_summary(NULL)
      adaptive_similarity_raster(NULL)
      session$sendCustomMessage("clearLeafletDrawFeatures", list(mapId = "sampling_auto_map"))
      leafletProxy("sampling_auto_map") %>% clearGroup("Sample Points")
      leafletProxy("sampling_auto_map") %>%
        clearGroup("Adaptive Similarity Classes") %>%
        removeControl("AdaptiveSimilarity_legend") %>%
        removeControl("CLHSSimilarity_legend")

      base_seed <- suppressWarnings(as.integer(base_seed))
      if (is.na(base_seed)) base_seed <- suppressWarnings(as.integer(isolate(input$random_seed)))
      if (is.na(base_seed)) base_seed <- 123L

      setProgress(0.08, detail = "Preparing boundary...")
      boundary <- digitized_features()
      use_boundary <- !is.null(boundary)
      buf_dist <- suppressWarnings(as.numeric(buffer_m))
      if (!is.finite(buf_dist)) buf_dist <- suppressWarnings(as.numeric(input$buffer_distance))
      if (use_boundary && is.finite(buf_dist) && buf_dist > 0) {
        buffer_ok <- FALSE
        tryCatch({
          b0 <- tryCatch(sf::st_make_valid(boundary), error = function(e) boundary)
          metric_crs <- local_metric_crs_from_sf(b0)
          b_metric <- st_transform(b0, metric_crs)
          b_metric <- tryCatch(sf::st_make_valid(b_metric), error = function(e) b_metric)
          b_metric <- b_metric[!sf::st_is_empty(b_metric), , drop = FALSE]
          if (nrow(b_metric) > 0L) {
            b_buf <- suppressWarnings(st_buffer(b_metric, dist = -buf_dist))
            b_buf <- tryCatch(sf::st_make_valid(b_buf), error = function(e) b_buf)
            b_buf <- b_buf[!sf::st_is_empty(b_buf), , drop = FALSE]
            if (nrow(b_buf) > 0L && any(as.numeric(sf::st_area(b_buf)) > 0, na.rm = TRUE)) {
              boundary <- st_transform(b_buf, st_crs(boundary))
              buffer_ok <- TRUE
            }
          }
        }, error = function(e) invisible(NULL))
        if (!buffer_ok) {
          showNotification("Buffer too large or invalid — using unbuffered boundary.", type = "warning")
        }
      }

      setProgress(0.2, detail = "Harmonizing covariates (full grid)...")
      sp <- deployment_safety_params()
      harmonized <- harmonize_covariate_layers(
        all_r,
        boundary_sf = if (use_boundary) boundary else NULL,
        analysis_crs = analysis_crs_string(),
        harmonize_scale = sp$harmonize_scale
      )
      if (length(harmonized) == 0L) {
        showNotification("No valid harmonized rasters for sampling.", type = "error")
        return(invisible(NULL))
      }
      ref_nm <- names(harmonized)[which.max(vapply(harmonized, function(r) max(raster::res(r)), numeric(1)))]
      showNotification(
        paste0(
          "Harmonized ", length(harmonized), " layer(s) to ", ref_nm,
          " (full AOI grid; ", RECOMMENDED_GENERATION_REPEATS(),
          " replicates, best field coverage kept)."
        ),
        type = "message",
        duration = 7
      )
      combined_stack <- stack(harmonized)
      names(combined_stack) <- gsub("[ :]", "_", names(harmonized))
      leafletProxy("sampling_auto_map") %>% clearGroup("Sampling Zones") %>% removeControl("SamplingZones_legend")

      setProgress(0.35, detail = "Building full covariate grid...")
      df <- as.data.frame(combined_stack, xy = TRUE, na.rm = TRUE)
      if (nrow(df) < n_target) {
        showNotification("Not enough valid cells for recommended sample size.", type = "error")
        return(invisible(NULL))
      }
      grid <- filter_complete_sampling_grid(df)
      df <- grid$df
      pop_xy <- grid$pop_xy
      cov_df <- grid$cov_df
      pop_n <- grid$pop_n
      n_use <- min(n_target, pop_n)
      if (pop_n < n_use) {
        showNotification("Not enough valid cells after filtering.", type = "error")
        return(invisible(NULL))
      }

      zone_clusters <- NULL
      zone_k_use <- NA_integer_
      if (compare_method %in% c("Zone-based", "Hybrid Zonal cLHS")) {
        kz <- resolve_zone_k_for_generation()
        zone_k_use <- min(kz, n_use, pop_n)
        zone_kmeans_seed <- base_seed + n_use * 97L
        if (!is.null(comparison_snapshot)) {
          cmp_base <- suppressWarnings(as.integer(comparison_snapshot$compare_base_seed))
          cmp_n <- suppressWarnings(as.integer(comparison_snapshot$n_points))
          if (!is.na(cmp_base) && !is.na(cmp_n) && cmp_n == n_use) {
            zone_kmeans_seed <- cmp_base + n_use * 97L
          }
        }
        set.seed(zone_kmeans_seed)
        scaled_cmp <- scale(cov_df)
        km_z <- stats::kmeans(scaled_cmp, centers = zone_k_use, iter.max = 15L, nstart = 1L)
        zone_clusters <- as.integer(km_z$cluster)
      }

      acrs <- tryCatch({
        if (use_boundary && !is.null(boundary) && nrow(boundary) > 0L) {
          local_metric_crs_from_sf(boundary)
        } else {
          NA_character_
        }
      }, error = function(e) NA_character_)
      pop_xy_metric <- pop_xy
      area_m2 <- NA_real_
      metric_ok <- tryCatch({
        pop_sf_grid <- sf::st_as_sf(pop_xy, coords = c("x", "y"), crs = crs(combined_stack))
        if (!is.character(acrs) || !nzchar(acrs[1L])) {
          acrs <- local_metric_crs_from_sf(pop_sf_grid)
        }
        pop_sf_metric <- sf::st_transform(pop_sf_grid, acrs)
        pop_xy_metric <- as.data.frame(sf::st_coordinates(pop_sf_metric))
        names(pop_xy_metric) <- c("x", "y")
        area_m2 <- sampling_area_m2_from_boundary(
          if (use_boundary) boundary else NULL,
          analysis_crs = acrs
        )
        if (!is.finite(area_m2) || area_m2 <= 0) {
          hull <- tryCatch(sf::st_convex_hull(sf::st_union(pop_sf_metric)), error = function(e) NULL)
          if (!is.null(hull)) area_m2 <- sampling_area_m2_from_boundary(hull, analysis_crs = acrs)
        }
        TRUE
      }, error = function(e) FALSE)

      setProgress(0.48, detail = paste0("Trying ", n_reps, " replicates (", compare_method, ")..."))
      pick <- tryCatch(
        pick_best_spread_replicate(
          compare_method = compare_method,
          n_use = n_use,
          pop_xy_grid = pop_xy,
          pop_xy_metric = if (metric_ok) pop_xy_metric else pop_xy,
          cov_df = cov_df,
          pop_n = pop_n,
          base_seed = base_seed,
          n_reps = n_reps,
          zone_clusters = zone_clusters,
          zone_k = zone_k_use,
          area_m2 = area_m2,
          target_field_coverage_pct = target_fc
        ),
        error = function(e) {
          list(
            idx = NULL,
            score = -Inf,
            field_coverage_pct = NA_real_,
            even_spread_idx = NA_real_,
            rep = 1L,
            seed = base_seed
          )
        }
      )
      best_idx <- pick$idx
      best_rep <- pick$rep
      best_seed <- pick$seed
      best_fc_pct <- pick$field_coverage_pct
      best_even <- pick$even_spread_idx
      polished <- FALSE
      if (
        !is.null(best_idx) && length(best_idx) >= 2L &&
          metric_ok && generation_polish_allowed(pop_n, n_use)
      ) {
        idx_pol <- polish_field_coverage_indices(
          best_idx,
          pop_xy_metric,
          pop_n,
          area_m2,
          target_pct = target_fc,
          n_use = n_use,
          aggressive = isTRUE(polish_aggressive)
        )
        if (length(idx_pol) >= 2L) {
          best_idx <- idx_pol
          polished <- TRUE
        }
      }
      if (isTRUE(polished)) {
        met_final <- spread_replicate_metrics(
          best_idx, pop_xy, pop_xy_metric, n_use, area_m2
        )
        if (is.finite(met_final$field_coverage_pct)) best_fc_pct <- met_final$field_coverage_pct
        if (is.finite(met_final$even_spread_idx)) best_even <- met_final$even_spread_idx
      }

      if (is.null(best_idx) || length(best_idx) < 1L) {
        showNotification(
          paste0(
            "Could not generate sample points for ", compare_method,
            ". Check boundary, covariates, zone count (Recommend Zones), and sample size."
          ),
          type = "error",
          duration = 10
        )
        return(invisible(NULL))
      }

      updateNumericInput(session, "random_seed", value = as.integer(best_seed))
      if (isTRUE(update_comparison_spread)) {
        recommended_random_seed(as.integer(best_seed))
      }

      setProgress(0.92, detail = "Building sample points...")
      if (isTRUE(update_comparison_spread)) {
        sample_type_lbl <- winner_method_to_sample_type(compare_method)
        if (!is.null(sample_type_lbl) && nzchar(sample_type_lbl)) {
          updateSelectInput(session, "sample_type", selected = sample_type_lbl)
        }
        updateNumericInput(session, "n_points", value = as.integer(n_use))
      }

      if (compare_method %in% c("Zone-based", "Hybrid Zonal cLHS")) {
        zone_ok <- tryCatch(
          {
            finalize_zone_sample_from_indices(
              df, cov_df, best_idx, combined_stack, zone_clusters, compare_method, zone_k = zone_k_use
            )
            TRUE
          },
          error = function(e) {
            showNotification(
              paste0("Zone sample build failed: ", conditionMessage(e)),
              type = "error",
              duration = 10
            )
            FALSE
          }
        )
        if (!isTRUE(zone_ok)) return(invisible(NULL))
      } else {
        sampled_df <- tryCatch(
          df[best_idx, , drop = FALSE],
          error = function(e) NULL
        )
        if (is.null(sampled_df) || nrow(sampled_df) < 1L) {
          showNotification("Failed to build sample points from grid indices.", type = "error", duration = 10)
          return(invisible(NULL))
        }
        pts <- tryCatch({
          out <- st_as_sf(sampled_df, coords = c("x", "y"), crs = crs(combined_stack))
          out$ID <- seq_len(nrow(out))
          st_transform(out, 4326)
        }, error = function(e) NULL)
        if (is.null(pts) || nrow(pts) < 1L) {
          showNotification("Failed to create sample point geometry.", type = "error", duration = 10)
          return(invisible(NULL))
        }
        sample_points(pts)
        render_auto_points_map(pts)
      }
      sampling_spread_pick_context(list(
        pop_xy = pop_xy,
        n_target = n_use,
        even_spread_idx = best_even,
        field_coverage_pct = best_fc_pct,
        method = compare_method,
        rep = as.integer(best_rep),
        seed = as.integer(best_seed)
      ))

      fc_msg <- if (is.finite(best_fc_pct)) paste0(round(best_fc_pct, 1), "% field coverage") else "field coverage NA"
      es_msg <- if (is.finite(best_even)) paste0("; spread index ", round(best_even, 3)) else ""
      target_note <- if (is.finite(best_fc_pct) && best_fc_pct >= target_fc) {
        paste0(" (met ≥", target_fc, "% target).")
      } else if (isTRUE(from_current_settings)) {
        paste0(
          " (best of ", n_reps, " for your ", compare_method, " at n = ", n_use,
          "; below ", target_fc, "% target — try Systematic Spread / Spread + cLHS or more points)."
        )
      } else {
        paste0(" (below ", target_fc, "% target — best available replicate).")
      }
      pick_notify_type <- if (is.finite(best_fc_pct) && best_fc_pct >= target_fc) {
        "message"
      } else {
        "warning"
      }
      showNotification(
        paste0(
          compare_method, ": kept replicate ", best_rep, "/", n_reps,
          " (", fc_msg, es_msg, target_note
        ),
        type = pick_notify_type,
        duration = 10
      )

      if (isTRUE(update_comparison_spread)) {
        cmp2 <- comparison_results()
        if (!is.null(cmp2)) {
          cmp2$spread_pick_rep <- as.integer(best_rep)
          cmp2$spread_pick_score <- if (is.finite(best_even)) round(best_even, 4) else NA_real_
          cmp2$spread_pick_field_coverage_pct <- if (is.finite(best_fc_pct)) round(best_fc_pct, 2) else NA_real_
          cmp2$spread_pick_seed <- as.integer(best_seed)
          comparison_results(cmp2)
        }
      }

      setProgress(1, detail = "Done.")
      release_geosampler_memory()
    })
    invisible(TRUE)
  }

  output$compare_zone_recommend_info <- renderUI({
    zone_wss_recommend_info_ui(compare_zone_recommend_msg())
  })
  
  output$compare_zone_wss_plot <- renderPlot({
    w <- zone_wss_cache()
    req(!is.null(w), length(w$k) > 1, length(w$wss) > 1)
    d <- data.frame(k = as.integer(w$k), wss = as.numeric(w$wss))
    yrng <- range(d$wss, na.rm = TRUE)
    y_lab <- yrng[2] - 0.04 * diff(yrng)
    ggplot2::ggplot(d, ggplot2::aes(x = k, y = wss)) +
      ggplot2::geom_line(color = "#2c7fb8", linewidth = 1) +
      ggplot2::geom_point(color = "#2c7fb8", size = 2.4) +
      ggplot2::geom_vline(xintercept = w$recommended, linetype = "dashed", color = "#c0392b") +
      ggplot2::annotate(
        "label",
        x = w$recommended,
        y = y_lab,
        label = paste0("Recommended k = ", w$recommended),
        fill = "#fff8f0",
        color = "#c0392b",
        size = 3.6,
        label.size = 0.2
      ) +
      ggplot2::labs(
        title = "Within-cluster variation vs number of zones",
        x = "Number of zones (k)",
        y = "Total within-zone sum of squares (WSS)"
      ) +
      ggplot2::theme_minimal(base_size = 15) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        axis.title = ggplot2::element_text(size = 16, face = "bold"),
        axis.text = ggplot2::element_text(size = 13)
      )
  })
  
  observeEvent(
    list(input$compare_sample_sizes, input$compare_repeats),
    {
      unlock_action_button("compare_sampling_methods")
    },
    ignoreInit = TRUE
  )

  observeEvent(digitized_features(), {
    if (isTRUE(invalidate_zone_recommendation_if_context_changed())) {
      unlock_action_button("compare_sampling_methods")
    }
  }, ignoreInit = TRUE)

  observeEvent(
    input$sampling_subtabs,
    {
      if (!identical(isolate(input$main_tabs), "Sampling")) {
        return(invisible(NULL))
      }
      if (identical(input$sampling_subtabs, "tcmp")) {
        invalidate_zone_recommendation_if_context_changed()
        restore_recommended_zone_k_if_missing()
        sync_zone_recommend_button_lock()
        if (!is.null(isolate(comparison_results()))) {
          lock_action_button("compare_sampling_methods")
        }
      } else if (identical(input$sampling_subtabs, "samp")) {
        invalidate_zone_recommendation_if_context_changed()
        restore_recommended_zone_k_if_missing()
        sync_zone_recommend_button_lock()
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(input$compare_sampling_methods, {
    abort_compare_sampling(FALSE)
    is_comparing_sampling(TRUE)
    compare_ok <- FALSE
    on.exit({
      is_comparing_sampling(FALSE)
      if (!isTRUE(compare_ok)) unlock_action_button("compare_sampling_methods")
    }, add = TRUE)
    all_r <- sampling_selected_rasters()
    if (length(all_r) == 0) {
      showNotification("No raster variables available. Add variables first, then compare.", type = "warning")
      return(invisible(NULL))
    }
    lock_action_button("compare_sampling_methods")
    size_tokens <- unlist(strsplit(gsub("\\s+", "", ifelse(is.null(input$compare_sample_sizes), "", input$compare_sample_sizes)), ","))
    sample_sizes <- sort(unique(suppressWarnings(as.integer(size_tokens))))
    sample_sizes <- sample_sizes[!is.na(sample_sizes) & sample_sizes >= 5]
    if (length(sample_sizes) == 0) {
      showNotification("Provide valid sample sizes (e.g., 20,30,40).", type = "warning")
      return(invisible(NULL))
    }
    n_reps <- suppressWarnings(as.integer(input$compare_repeats))
    if (is.na(n_reps) || n_reps < 2) {
      showNotification("Repeats must be at least 2.", type = "warning")
      return(invisible(NULL))
    }
    
    withProgress(message = "Comparing sampling techniques...", value = 0.05, {
      tryCatch({
        check_compare_abort <- function() {
          try(later::run_now(timeout = 0), silent = TRUE)
          if (isTRUE(abort_compare_sampling())) stop("Comparison stopped by user.", call. = FALSE)
        }
        check_compare_abort()
        setProgress(0.12, detail = "Preparing covariate population...")
        boundary <- digitized_features()
        use_boundary <- !is.null(boundary) && nrow(boundary) > 0
        
        sp_prep <- deployment_safety_params()
        harmonized <- harmonize_covariate_layers(
          all_r,
          boundary_sf = if (use_boundary) boundary else NULL,
          analysis_crs = analysis_crs_string(),
          harmonize_scale = sp_prep$harmonize_scale
        )
        if (length(harmonized) == 0L) {
          showNotification("Could not prepare covariate stack for comparison.", type = "error")
          return(invisible(NULL))
        }
        pop_stack <- stack(harmonized)
        pop_df <- as.data.frame(pop_stack, xy = TRUE, na.rm = TRUE)
        pop_xy <- pop_df[, c("x", "y"), drop = FALSE]
        pop_cov <- pop_df[, setdiff(names(pop_df), c("x", "y")), drop = FALSE]
        pop_cov <- pop_cov[, sapply(pop_cov, is.numeric), drop = FALSE]
        pop_cov <- pop_cov[, sapply(pop_cov, function(v) stats::sd(v, na.rm = TRUE) > 0), drop = FALSE]
        keep_rows <- stats::complete.cases(pop_cov)
        pop_cov <- pop_cov[keep_rows, , drop = FALSE]
        pop_xy <- pop_xy[keep_rows, , drop = FALSE]
        
        if (ncol(pop_cov) < 1 || nrow(pop_cov) < 20) {
          showNotification("Not enough valid covariate data for robust method comparison.", type = "error")
          return(invisible(NULL))
        }

        compare_base_seed <- suppressWarnings(as.integer(isolate(input$random_seed)))
        if (is.na(compare_base_seed)) compare_base_seed <- 123L
        compare_pop_cap <- suppressWarnings(as.integer(sp_prep$compare_pop_cap))
        pop_cells_full_n <- nrow(pop_cov)
        compare_used_subsample <- FALSE
        if (is.finite(compare_pop_cap) && !is.na(compare_pop_cap) && pop_cells_full_n > compare_pop_cap) {
          set.seed(compare_base_seed + 7331L)
          spair <- subsample_paired_rows(pop_cov, pop_xy, compare_pop_cap)
          pop_cov <- spair$cov
          pop_xy <- spair$xy
          compare_used_subsample <- TRUE
        }
        pop_n <- nrow(pop_cov)
        if (pop_n < 20L) {
          showNotification("Not enough valid covariate cells for comparison after subsampling.", type = "error")
          return(invisible(NULL))
        }

        compare_zone_k_frozen <- get_recommended_zone_k()
        if (is.na(compare_zone_k_frozen) || compare_zone_k_frozen < 2L) {
          showNotification(
            "For Zone-based and Hybrid Zonal cLHS: click Recommend Zones once before comparing; same k for comparison, sampling, and generation.",
            type = "warning",
            duration = 9
          )
          return(invisible(NULL))
        }

        sample_sizes_use <- sample_sizes[sample_sizes <= pop_n & !is.na(sample_sizes) & sample_sizes >= 5L]
        sample_sizes_use <- sort(unique(sample_sizes_use))
        if (length(sample_sizes_use) == 0L) {
          showNotification("Requested sample sizes exceed the comparison grid (valid cells in AOI). Lower sizes or reduce negative buffer.", type = "error")
          return(invisible(NULL))
        }

        rm(pop_stack, pop_df)
        tryCatch(gc(verbose = FALSE), error = function(e) invisible(NULL))

        pop_bbox <- c(
          xmin = min(pop_xy$x, na.rm = TRUE),
          xmax = max(pop_xy$x, na.rm = TRUE),
          ymin = min(pop_xy$y, na.rm = TRUE),
          ymax = max(pop_xy$y, na.rm = TRUE)
        )
        pop_cor <- if (ncol(pop_cov) >= 2) suppressWarnings(stats::cor(pop_cov, use = "pairwise.complete.obs")) else matrix(1, 1, 1)
        pca_obj <- tryCatch(stats::prcomp(pop_cov, center = TRUE, scale. = TRUE), error = function(e) NULL)
        pca_n_components <- 1L
        if (!is.null(pca_obj) && length(pca_obj$sdev) >= 1L) {
          cumvar <- cumsum(pca_obj$sdev^2) / sum(pca_obj$sdev^2)
          hit <- which(cumvar >= 0.90)
          pca_n_components <- if (length(hit)) as.integer(hit[1]) else length(pca_obj$sdev)
          pca_n_components <- max(1L, min(pca_n_components, length(pca_obj$sdev)))
        }
        pop_vn <- names(pop_cov)
        pop_center_cmp <- colMeans(pop_cov, na.rm = TRUE)
        cov_mat_cmp <- stats::cov(pop_cov, use = "pairwise.complete.obs")
        if (!is.matrix(cov_mat_cmp)) cov_mat_cmp <- matrix(as.numeric(cov_mat_cmp), nrow = 1, ncol = 1)
        cov_mat_cmp <- cov_mat_cmp + diag(1e-8, nrow(cov_mat_cmp))
        pop_pc_pre <- if (!is.null(pca_obj) && ncol(pop_cov) >= 2) {
          stats::predict(pca_obj, newdata = pop_cov)
        } else NULL
        pop_var_ranges <- vapply(pop_vn, function(v) diff(range(pop_cov[[v]], na.rm = TRUE)), numeric(1))
        pop_scaled_cmp <- scale(pop_cov)
        pop_uni_const <- vapply(pop_vn, function(vn) length(unique(pop_cov[[vn]])) <= 1L, logical(1))
        ks_ref_n <- min(2000L, pop_n)
        set.seed(compare_base_seed + 4242L)
        ks_ref_ix <- if (pop_n > ks_ref_n) sample.int(pop_n, ks_ref_n) else seq_len(pop_n)
        pop_ks_ref <- pop_cov[ks_ref_ix, , drop = FALSE]
        pop_cor_ut <- if (ncol(pop_cov) >= 2L) upper.tri(pop_cor) else logical(0)
        ref_nn_cache <- stats::setNames(
          vapply(sample_sizes_use, function(nsz) reference_mean_nn_regular_grid(nsz, pop_bbox), numeric(1)),
          as.character(sample_sizes_use)
        )
        
        eval_sample <- function(sdf) {
          if (is.null(sdf) || nrow(sdf) < 2L) return(rep(NA_real_, 5))
          sdf <- sdf[, pop_vn, drop = FALSE]
          
          univ_scores <- vapply(pop_vn, function(vn) {
            if (pop_uni_const[[vn]]) return(1)
            ks_uniformity_score(sdf[[vn]], pop_ks_ref[[vn]])
          }, numeric(1))
          univariate_score <- mean(univ_scores, na.rm = TRUE)
          
          samp_center <- colMeans(sdf, na.rm = TRUE)
          md <- tryCatch(
            stats::mahalanobis(
              matrix(samp_center, nrow = 1L),
              pop_center_cmp,
              cov_mat_cmp
            ),
            error = function(e) Inf
          )
          multivariate_score <- if (is.finite(md)) 1 / (1 + as.numeric(md)) else 0
          
          pca_score <- if (!is.null(pop_pc_pre) && ncol(pop_cov) >= 2L) {
            samp_pc <- stats::predict(pca_obj, newdata = sdf)
            pca_coverage_score_robust(
              pop_pc_pre, samp_pc,
              n_components = pca_n_components,
              n_sample = nrow(sdf)
            )
          } else 1
          
          range_ratios <- vapply(pop_vn, function(vn) {
            pr <- pop_var_ranges[[vn]]
            sr <- diff(range(sdf[[vn]], na.rm = TRUE))
            if (is.na(pr) || pr <= 0) return(1)
            min(1, max(0, sr / pr))
          }, numeric(1))
          tail_score <- quantile_tail_coverage_score(sdf, pop_cov, pop_vn, q_lo = 0.10, q_hi = 0.90)
          range_score <- 0.35 * mean(range_ratios, na.rm = TRUE) + 0.65 * tail_score
          
          corr_score <- 1
          if (length(pop_cor_ut) && nrow(sdf) > 2L) {
            sc <- suppressWarnings(stats::cor(sdf, use = "pairwise.complete.obs"))
            if (is.matrix(sc) && nrow(sc) == nrow(pop_cor) && ncol(sc) == ncol(pop_cor)) {
              mae <- mean(abs(pop_cor[pop_cor_ut] - sc[pop_cor_ut]), na.rm = TRUE)
              corr_score <- max(0, 1 - (mae / 2))
            }
          }
          
          c(univariate_score, multivariate_score, pca_score, range_score, corr_score)
        }
        
        loop_metric_cols <- COMPARE_LOOP_METRIC_COLS
        summary_metric_cols <- COMPARE_METRIC_COLS
        kz <- compare_zone_k_frozen
        results_long <- list()
        total_runs <- length(sample_sizes_use) * n_reps
        run_counter <- 0L
        
        for (n_points_cmp in sample_sizes_use) {
          check_compare_abort()
          kz_use <- min(kz, n_points_cmp, pop_n)
          set.seed(compare_base_seed + n_points_cmp * 97L)
          km_cmp <- stats::kmeans(pop_scaled_cmp, centers = kz_use, iter.max = 15L, nstart = 1L)
          z_cl_cached <- km_cmp$cluster
          # Field coverage score (efficient): NN ratio + share of coarse grid cells touched (recomputed per n only).
          bbox_cmp <- c(
            xmin = min(pop_xy$x, na.rm = TRUE),
            xmax = max(pop_xy$x, na.rm = TRUE),
            ymin = min(pop_xy$y, na.rm = TRUE),
            ymax = max(pop_xy$y, na.rm = TRUE)
          )
          n_gr_cmp <- max(2L, ceiling(sqrt(max(as.integer(n_points_cmp), 2L) * 2)))
          gx_cmp <- cut(
            pop_xy$x,
            breaks = seq(bbox_cmp["xmin"], bbox_cmp["xmax"], length.out = n_gr_cmp + 1L),
            labels = FALSE,
            include.lowest = TRUE
          )
          gy_cmp <- cut(
            pop_xy$y,
            breaks = seq(bbox_cmp["ymin"], bbox_cmp["ymax"], length.out = n_gr_cmp + 1L),
            labels = FALSE,
            include.lowest = TRUE
          )
          cell_id_cmp <- gx_cmp + (gy_cmp - 1L) * n_gr_cmp
          target_cells_cmp <- min(n_gr_cmp * n_gr_cmp, max(as.integer(n_points_cmp), 1L))
          eval_field_coverage_score <- function(idx) {
            idx <- as.integer(idx)
            n <- length(idx)
            if (n < 2L) return(NA_real_)
            mean_nn <- mean_nearest_neighbor_distance(pop_xy[idx, c("x", "y"), drop = FALSE])
            ref_nn <- ref_nn_cache[[as.character(as.integer(n_points_cmp))]]
            nn_score <- if (is.finite(mean_nn) && is.finite(ref_nn) && ref_nn > 0) {
              min(1, max(0, mean_nn / ref_nn))
            } else NA_real_
            occ <- length(unique(cell_id_cmp[idx]))
            grid_score <- min(1, max(0, occ / target_cells_cmp))
            parts <- c(nn_score, grid_score)
            parts <- parts[is.finite(parts)]
            if (!length(parts)) return(NA_real_)
            mean(parts)
          }

          idx_sys <- systematic_spread_indices(n_points_cmp, pop_xy, rng_seed = compare_base_seed + n_points_cmp * 93L)
          sys_df <- pop_cov[idx_sys, , drop = FALSE]
          sys_sb <- eval_field_coverage_score(idx_sys)
          sys_met <- c(eval_sample(sys_df), sys_sb)
          
          for (rp in seq_len(n_reps)) {
            check_compare_abort()
            run_counter <- run_counter + 1L
            setProgress(0.22 + 0.60 * (run_counter / total_runs),
                        detail = paste0("Size ", n_points_cmp, " | repeat ", rp, "/", n_reps))
            
            set.seed(compare_base_seed + n_points_cmp * 100L + rp)
            idx_sr <- sample.int(pop_n, size = n_points_cmp)
            sr_df <- pop_cov[idx_sr, , drop = FALSE]
            
            check_compare_abort()
            clhs_res <- tryCatch(
              clhs::clhs(pop_cov, size = n_points_cmp, progress = FALSE, simple = TRUE),
              error = function(e) stop(paste("cLHS failed:", e$message), call. = FALSE)
            )
            idx_clhs <- compare_ensure_n_indices(
              clhs_sample_indices(clhs_res), pop_n, n_points_cmp
            )
            clhs_df <- pop_cov[idx_clhs, , drop = FALSE]
            
            check_compare_abort()
            idx_sclhs <- compare_ensure_n_indices(
              spread_clhs_indices(
                n_points_cmp, pop_xy, pop_cov,
                rng_seed = compare_base_seed + n_points_cmp * 100L + rp + 13007L
              ),
              pop_n, n_points_cmp
            )
            sclhs_df <- pop_cov[idx_sclhs, , drop = FALSE]
            
            check_compare_abort()
            z_cl <- z_cl_cached
            alloc <- neyman_allocate_points_across_zones(z_cl, pop_cov, n_points_cmp)
            idx_z <- integer(0)
            for (zn in names(alloc)) {
              zid <- which(z_cl == as.integer(zn))
              nt <- suppressWarnings(as.integer(alloc[[zn]]))
              if (length(nt) != 1L || is.na(nt) || nt < 1L) next
              nt <- min(length(zid), nt)
              if (nt > 0L) idx_z <- c(idx_z, sample(zid, nt))
            }
            idx_z <- compare_ensure_n_indices(unique(idx_z), pop_n, n_points_cmp)
            zonal_df <- pop_cov[idx_z, , drop = FALSE]
            check_compare_abort()
            idx_h <- compare_ensure_n_indices(
              hybrid_zone_clhs_indices(
                pop_cov, n_points = n_points_cmp, kz = kz_use, z_clusters = z_cl_cached
              ),
              pop_n, n_points_cmp
            )
            hybrid_df <- pop_cov[idx_h, , drop = FALSE]
            
            metrics <- rbind(
              `Simple Random` = c(eval_sample(sr_df), eval_field_coverage_score(idx_sr)),
              `Systematic Spread` = sys_met,
              `Spread + cLHS` = c(eval_sample(sclhs_df), eval_field_coverage_score(idx_sclhs)),
              `cLHS` = c(eval_sample(clhs_df), eval_field_coverage_score(idx_clhs)),
              `Zone-based` = c(eval_sample(zonal_df), eval_field_coverage_score(idx_z)),
              `Hybrid Zonal cLHS` = c(eval_sample(hybrid_df), eval_field_coverage_score(idx_h))
            )
            colnames(metrics) <- loop_metric_cols
            mdf <- as.data.frame(metrics)
            mdf$Method <- rownames(mdf)
            mdf$Final_Score <- apply_rank_based_final_score(
              mdf,
              metric_cols = loop_metric_cols,
              w = COMPARE_LOOP_METRIC_WEIGHTS
            )
            mdf$Sample_Size <- n_points_cmp
            mdf$Repeat <- rp
            results_long[[length(results_long) + 1L]] <- mdf
          }
          rm(km_cmp, idx_sr, idx_clhs, idx_sclhs, idx_z, idx_h, sr_df, clhs_df, sclhs_df, zonal_df, hybrid_df, sys_df)
        }
        tryCatch(gc(verbose = FALSE), error = function(e) invisible(NULL))
        
        setProgress(0.88, detail = "Aggregating repeat statistics...")
        long_df <- dplyr::bind_rows(results_long)
        rm(results_long)
        tryCatch(gc(verbose = FALSE), error = function(e) invisible(NULL))
        loop_metric_cols <- COMPARE_LOOP_METRIC_COLS
        agg_long <- long_df %>%
          dplyr::group_by(Sample_Size, Method) %>%
          dplyr::summarise(
            dplyr::across(dplyr::all_of(loop_metric_cols), ~ mean(.x, na.rm = TRUE)),
            Final_Score_Mean = mean(Final_Score, na.rm = TRUE),
            Final_Score_SD = stats::sd(Final_Score, na.rm = TRUE),
            .groups = "drop"
          )
        summary_df <- dplyr::bind_rows(lapply(split(agg_long, agg_long$Sample_Size), function(df_sz) {
          df_sz$Final_Score <- apply_rank_based_final_score(
            df_sz,
            metric_cols = COMPARE_METRIC_COLS,
            w = COMPARE_METRIC_WEIGHTS
          )
          df_sz
        }))
        summary_df <- summary_df %>%
          dplyr::mutate(
            Final_Score_CV = dplyr::if_else(
              is.finite(Final_Score_Mean) & abs(Final_Score_Mean) > 1e-8,
              Final_Score_SD / abs(Final_Score_Mean),
              Inf
            ),
            Final_Score_CV = dplyr::if_else(is.finite(Final_Score_CV), Final_Score_CV, Inf)
          )
        
        best_by_size <- summary_df %>%
          dplyr::group_by(Sample_Size) %>%
          dplyr::arrange(dplyr::desc(Final_Score), Final_Score_CV, .by_group = TRUE) %>%
          dplyr::slice(1) %>%
          ungroup() %>%
          arrange(Sample_Size)
        
        # Method 1: Elbow (on best score vs sample size)
        if (nrow(best_by_size) <= 2) {
          elbow_size <- best_by_size$Sample_Size[nrow(best_by_size)]
        } else {
          xs <- best_by_size$Sample_Size
          ys <- best_by_size$Final_Score
          x1 <- xs[1]; y1 <- ys[1]; x2 <- xs[length(xs)]; y2 <- ys[length(ys)]
          denom <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
          if (denom == 0) {
            elbow_size <- xs[1]
          } else {
            d <- abs((y2 - y1) * xs - (x2 - x1) * ys + x2 * y1 - y2 * x1) / denom
            elbow_size <- xs[which.max(d)]
          }
        }
        
        # Method 2: Threshold-based (fair pick among sizes near the best score)
        lam <- as.numeric(input$compare_cost_weight)
        thr <- as.numeric(input$compare_threshold_pct) / 100
        target <- max(best_by_size$Final_Score, na.rm = TRUE) * thr
        thr_candidates <- best_by_size$Sample_Size[best_by_size$Final_Score >= target]
        threshold_size <- if (length(thr_candidates)) {
          pick_fair_sample_size(thr_candidates, sample_sizes_use)
        } else {
          pick_fair_sample_size(best_by_size$Sample_Size, sample_sizes_use)
        }
        
        # Method 3: Minimum acceptable range coverage (mean-over-repeats metric)
        coverage_thr <- as.numeric(input$compare_min_range_coverage) / 100
        coverage_hits <- best_by_size$Sample_Size[!is.na(best_by_size$Range_Coverage) & best_by_size$Range_Coverage >= coverage_thr]
        coverage_hits <- sort(unique(coverage_hits))
        coverage_size <- if (length(coverage_hits)) {
          pick_fair_sample_size(coverage_hits, sample_sizes_use)
        } else {
          pick_fair_sample_size(sample_sizes_use, sample_sizes_use)
        }
        
        # Method 4: Cost vs Accuracy (fair pick among top utility sizes)
        size_norm <- (best_by_size$Sample_Size - min(best_by_size$Sample_Size)) / pmax(max(best_by_size$Sample_Size) - min(best_by_size$Sample_Size), 1e-8)
        score_norm <- (best_by_size$Final_Score - min(best_by_size$Final_Score)) / pmax(max(best_by_size$Final_Score) - min(best_by_size$Final_Score), 1e-8)
        utility <- score_norm - lam * size_norm
        top_u <- max(utility, na.rm = TRUE)
        u_candidates <- best_by_size$Sample_Size[utility >= (top_u - 1e-10)]
        cost_size <- if (length(u_candidates)) {
          pick_fair_sample_size(u_candidates, sample_sizes_use)
        } else {
          pick_fair_sample_size(best_by_size$Sample_Size, sample_sizes_use)
        }
        
        # Method 5: Stability (lowest CV among per-size leaders)
        finite_cv_sizes <- best_by_size$Sample_Size[is.finite(best_by_size$Final_Score_CV)]
        stability_size <- if (length(finite_cv_sizes)) {
          cv_df <- best_by_size %>% dplyr::filter(Sample_Size %in% finite_cv_sizes)
          cv_min <- min(cv_df$Final_Score_CV, na.rm = TRUE)
          cv_tol <- max(0.01, 0.12 * cv_min)
          cv_candidates <- cv_df$Sample_Size[cv_df$Final_Score_CV <= (cv_min + cv_tol)]
          if (length(cv_candidates)) pick_fair_sample_size(cv_candidates, sample_sizes_use) else pick_fair_sample_size(finite_cv_sizes, sample_sizes_use)
        } else {
          pick_fair_sample_size(best_by_size$Sample_Size, sample_sizes_use)
        }
        
        rec_votes <- c(elbow_size, threshold_size, coverage_size, cost_size, stability_size)
        rec_table <- table(rec_votes)
        top_vote <- max(rec_table)
        tied_sizes <- as.integer(names(rec_table)[rec_table == top_vote])
        rule_consensus_size <- pick_fair_sample_size(tied_sizes, sample_sizes_use)

        max_final_score <- max(best_by_size$Final_Score, na.rm = TRUE)
        score_band <- max(0.02, 0.02 * max_final_score)
        score_candidates <- best_by_size$Sample_Size[best_by_size$Final_Score >= max_final_score - score_band]
        final_size <- if (length(score_candidates)) {
          score_df <- best_by_size %>% dplyr::filter(Sample_Size %in% score_candidates)
          cv_floor <- min(score_df$Final_Score_CV, na.rm = TRUE)
          if (!is.finite(cv_floor)) {
            pick_fair_sample_size(score_candidates, sample_sizes_use)
          } else {
            cv_tol <- max(0.01, 0.10 * cv_floor)
            stable_score_candidates <- score_df$Sample_Size[score_df$Final_Score_CV <= (cv_floor + cv_tol)]
            if (length(stable_score_candidates)) pick_fair_sample_size(stable_score_candidates, sample_sizes_use) else pick_fair_sample_size(score_candidates, sample_sizes_use)
          }
        } else {
          pick_fair_sample_size(best_by_size$Sample_Size[which.max(best_by_size$Final_Score)], sample_sizes_use)
        }
        
        sw_fin_all <- summary_df %>% dplyr::filter(Sample_Size == final_size)
        best_sc_m <- max(sw_fin_all$Final_Score, na.rm = TRUE)
        sc_rng_m <- suppressWarnings(diff(range(sw_fin_all$Final_Score, na.rm = TRUE)))
        if (!is.finite(sc_rng_m) || sc_rng_m <= 0) sc_rng_m <- 1e-6
        method_score_tol <- max(0.02, 0.05 * sc_rng_m, 0.02 * abs(best_sc_m))
        near_best_methods <- sw_fin_all %>% dplyr::filter(Final_Score >= best_sc_m - method_score_tol)
        pick_winner_df <- near_best_methods %>%
          dplyr::arrange(
            Final_Score_CV,
            dplyr::desc(Range_Coverage),
            dplyr::desc(Spatial_Coverage),
            dplyr::desc(Final_Score)
          ) %>%
          dplyr::slice(1)
        w1 <- as.character(pick_winner_df$Method[1])
        score_only_leader <- sw_fin_all %>%
          dplyr::arrange(dplyr::desc(Final_Score), dplyr::desc(Spatial_Coverage)) %>%
          dplyr::slice(1)
        spread_tiebreak_used <- !identical(as.character(score_only_leader$Method[1]), w1)

        final_methods <- dplyr::bind_rows(
          pick_winner_df,
          sw_fin_all %>% dplyr::filter(Method != w1) %>% dplyr::arrange(dplyr::desc(Final_Score))
        ) %>%
          dplyr::mutate(
            Rank = dplyr::row_number(),
            Final_Score = round(Final_Score, 3),
            Final_Score_Mean = round(Final_Score_Mean, 3),
            Final_Score_SD = round(Final_Score_SD, 3),
            Final_Score_CV = round(Final_Score_CV, 4),
            Univariate = round(Univariate, 3),
            Multivariate = round(Multivariate, 3),
            PCA_Coverage = round(PCA_Coverage, 3),
            Range_Coverage = round(Range_Coverage, 3),
            Correlation_Preservation = round(Correlation_Preservation, 3),
            Spatial_Coverage = round(Spatial_Coverage, 3)
          )

        sw_fin <- sw_fin_all
        metric_cols_w <- COMPARE_METRIC_COLS
        methods_at_n_display <- order_sampling_methods_df(
          sw_fin %>%
            dplyr::select(dplyr::any_of(c("Method", metric_cols_w, "Final_Score", "Final_Score_Mean", "Final_Score_SD", "Final_Score_CV")))
        )

        rec_st <- winner_method_to_sample_type(w1)
        recommended_sample_type(rec_st)
        recommended_n_points(as.integer(final_size))
        if (!is.na(compare_zone_k_frozen) && compare_zone_k_frozen >= 2L) {
          apply_wss_zone_inputs(compare_zone_k_frozen)
        }
        sp_end <- deployment_safety_params()
        rec_buf <- if (is.finite(sp_end$recommended_buffer_m)) sp_end$recommended_buffer_m else 10
        recommended_buffer_distance(rec_buf)
        updateNumericInput(session, "buffer_distance", value = as.numeric(rec_buf))
        recommended_grid_size_m(suppressWarnings(as.integer(isolate(input$grid_size_m))))

        winner_rows <- long_df %>%
          dplyr::filter(.data$Method == w1, .data$Sample_Size == final_size)
        cov_balance <- NULL
        best_repeat <- 1L
        generation_seed <- compare_base_seed + as.integer(final_size) * 100L + best_repeat
        zone_clusters_final <- NULL
        zone_k_final <- NA_integer_
        if (w1 %in% c("Zone-based", "Hybrid Zonal cLHS")) {
          zone_k_final <- min(kz, final_size, pop_n)
          set.seed(compare_base_seed + as.integer(final_size) * 97L)
          km_fin <- stats::kmeans(pop_scaled_cmp, centers = zone_k_final, iter.max = 15L, nstart = 1L)
          zone_clusters_final <- as.integer(km_fin$cluster)
          rm(km_fin)
        }
        if (nrow(winner_rows) > 0L) {
          top_sc_r <- max(winner_rows$Final_Score, na.rm = TRUE)
          r_sc <- winner_rows$Final_Score
          sc_rng_r <- suppressWarnings(diff(range(r_sc, na.rm = TRUE)))
          if (!is.finite(sc_rng_r) || sc_rng_r <= 0) sc_rng_r <- 1e-6
          rep_tol <- max(0.03, 0.06 * sc_rng_r, 0.025 * abs(top_sc_r))
          near_r <- winner_rows %>%
            dplyr::filter(.data$Final_Score >= top_sc_r - rep_tol)
          if (nrow(near_r) < 1L) near_r <- winner_rows
          near_r$rep_quality <- 0.40 * near_r$Spatial_Coverage + 0.60 * near_r$Range_Coverage
          near_r$rep_quality[!is.finite(near_r$rep_quality)] <- 0
          best_row <- near_r[which.max(near_r$rep_quality), , drop = FALSE]
          br <- suppressWarnings(as.integer(best_row$Repeat[1L]))
          if (!is.na(br) && br >= 1L) best_repeat <- br
          generation_seed <- compare_base_seed + as.integer(final_size) * 100L + best_repeat
        }
        idx_balance <- sample_indices_for_compare_method(
          w1,
          final_size,
          pop_xy,
          pop_cov,
          pop_n,
          generation_seed,
          zone_clusters = zone_clusters_final,
          zone_k = zone_k_final
        )
        idx_balance <- compare_ensure_n_indices(idx_balance, pop_n, final_size)
        if (length(idx_balance) >= 1L) {
          cov_balance <- build_pop_vs_sample_balance_df(pop_cov, idx_balance)
        }
        recommended_random_seed(as.integer(generation_seed))
        rm(winner_rows, zone_clusters_final)
        tryCatch(gc(verbose = FALSE), error = function(e) invisible(NULL))

        clear_sampling_after_comparison()

        comparison_results(list(
          table = final_methods,
          winner = final_methods$Method[1],
          winner_score = final_methods$Final_Score[1],
          n_population = pop_n,
          n_population_field = pop_cells_full_n,
          compare_used_subsample = isTRUE(compare_used_subsample),
          compare_pop_cap_applied = if (isTRUE(compare_used_subsample)) compare_pop_cap else NA_integer_,
          n_points = final_size,
          recommended_buffer_m = rec_buf,
          covariate_balance = cov_balance,
          summary_by_size = best_by_size,
          all_summary = summary_df,
          spread_tiebreak_used = isTRUE(spread_tiebreak_used),
          coverage_leader_method = as.character(score_only_leader$Method[1]),
          runner_up = if (nrow(final_methods) >= 2L) final_methods$Method[2] else NA_character_,
          methods_at_n = methods_at_n_display,
          recommendation_table = dplyr::bind_rows(
            tibble::tibble(
              Rule = c("Elbow", "Threshold-based", "Minimum range coverage", "Cost vs Accuracy", "Stability (lowest CV)", "Rule Consensus"),
              Suggested_Sample_Size = c(elbow_size, threshold_size, coverage_size, cost_size, stability_size, rule_consensus_size)
            ),
            tibble::tibble(
              Rule = "Final Recommended (fair near-best score)",
              Suggested_Sample_Size = final_size
            )
          ),
          compare_base_seed = compare_base_seed,
          best_repeat = as.integer(best_repeat),
          generation_seed = as.integer(generation_seed),
          recommended_wss_zones = as.integer(compare_zone_k_frozen)
        ))
        setProgress(1, detail = "Comparison complete.")
        subsample_tail <- if (isTRUE(compare_used_subsample)) {
          paste0(
            " Speed mode: compared methods on ", pop_n, " random cells (of ",
            pop_cells_full_n, " in AOI). Generate sample points still uses the full harmonized grid."
          )
        } else {
          ""
        }
        spread_note <- if (isTRUE(spread_tiebreak_used)) {
          paste0(
            " Covariate tie-break vs ", as.character(score_only_leader$Method[1]),
            " (similar overall score; preferred lower variability and stronger range coverage)."
          )
        } else {
          ""
        }
        zone_note <- if (!is.na(compare_zone_k_frozen) && compare_zone_k_frozen >= 2L) {
          paste0(" | WSS zones k = ", compare_zone_k_frozen)
        } else {
          ""
        }
        showNotification(
          paste0(
            "Comparison complete. Winner: ", final_methods$Method[1],
            " | n = ", final_size,
            zone_note,
            spread_note,
            ". Both generate buttons run ", RECOMMENDED_GENERATION_REPEATS(),
            " replicates on the full grid and pick the best field coverage spread. Prior sample points were cleared.",
            subsample_tail
          ),
          type = "message",
          duration = if (nzchar(subsample_tail)) 12 else 8
        )
        compare_ok <- TRUE
        tryCatch(gc(verbose = FALSE), error = function(e) invisible(NULL))
      }, error = function(e) {
        if (grepl("Comparison stopped by user", e$message, fixed = TRUE)) {
          showNotification("Sampling comparison stopped by user.", type = "warning")
        } else {
        comparison_results(NULL)
          recommended_sample_type(NULL)
          recommended_n_points(NULL)
          recommended_buffer_distance(NULL)
          recommended_random_seed(NULL)
          recommended_grid_size_m(NULL)
          sampling_prefill_active(FALSE)
        handle_app_error(e, context = "Technique comparison", notify_user = TRUE, duration = 12)
        }
      })
    })
  }, ignoreInit = TRUE)
  
  # --- Sampling Logic ---
  run_generate_automatic_samples <- function() {
    spread_res <- launch_automatic_spread_pick(from_recommended = FALSE)
    if (!identical(spread_res, "grid_based")) {
      return(invisible(spread_res))
    }

    if (!identical(isolate(input$sample_type), "Grid-based")) {
      showNotification(
        paste0(
          "Unrecognized sampling type: ",
          tryCatch(as.character(input$sample_type), error = function(e) "unknown")
        ),
        type = "error"
      )
      return(invisible(NULL))
    }

    on.exit(unlock_generate_sample_buttons(), add = TRUE)
    lock_action_button("generate_samples_custom")
    lock_action_button("generate_samples_recommended")
    sampling_spread_pick_context(NULL)

    withProgress(message="Generating Samples", value=0, {
      sp <- deployment_safety_params()
      zonal_cluster_summary(NULL)
      zonal_cluster_means(NULL)
      zonal_cluster_model(NULL)
      clhs_similarity_zone(NULL)
      clhs_weak_gps_zone(NULL)
      clhs_similarity_threshold(NULL)
      clhs_similarity_polygon_count(NULL)
      clhs_weak_zone_area_ha(NULL)
      adaptive_recommendation_summary(NULL)
      adaptive_similarity_raster(NULL)
      session$sendCustomMessage("clearLeafletDrawFeatures", list(mapId = "sampling_auto_map"))
      leafletProxy("sampling_auto_map") %>% clearGroup("Sample Points")
      leafletProxy("sampling_auto_map") %>%
        clearGroup("Adaptive Similarity Classes") %>%
        removeControl("AdaptiveSimilarity_legend") %>%
        removeControl("CLHSSimilarity_legend")
      setProgress(0.08, detail = "Preparing boundary...")
      
      # Set random seed for reproducibility
      set.seed(input$random_seed)
      
      boundary <- digitized_features()
      use_boundary <- !is.null(boundary)
      
      # Apply negative buffer if specified
      buf_dist <- suppressWarnings(as.numeric(input$buffer_distance))
      if (use_boundary && is.finite(buf_dist) && buf_dist > 0) {
        buffer_ok <- FALSE
        tryCatch({
          b0 <- tryCatch(sf::st_make_valid(boundary), error = function(e) boundary)
          metric_crs <- local_metric_crs_from_sf(b0)
          b_metric <- st_transform(b0, metric_crs)
          b_metric <- tryCatch(sf::st_make_valid(b_metric), error = function(e) b_metric)
          b_metric <- b_metric[!sf::st_is_empty(b_metric), , drop = FALSE]
          if (nrow(b_metric) == 0) {
            showNotification("Boundary is empty/invalid before buffering. Redraw or upload boundary again.", type = "error")
          } else {
            b_buf <- suppressWarnings(st_buffer(b_metric, dist = -buf_dist))
            b_buf <- tryCatch(sf::st_make_valid(b_buf), error = function(e) b_buf)
            b_buf <- b_buf[!sf::st_is_empty(b_buf), , drop = FALSE]
            if (nrow(b_buf) == 0 || all(as.numeric(sf::st_area(b_buf)) <= 0, na.rm = TRUE)) {
            showNotification("Buffer distance too large - no area remaining. Try a smaller buffer.", type = "error")
            } else {
              boundary <- st_transform(b_buf, st_crs(boundary))
              buffer_ok <- TRUE
          }
          }
        }, error = function(e) {
          showNotification(paste("Error applying buffer:", e$message), type = "error")
        })
        if (!buffer_ok) return(invisible(NULL))
        showNotification(paste("Applied", buf_dist, "meter negative buffer to boundary."), type = "message")
      }
      
      setProgress(0.22, detail = "Harmonizing rasters to coarsest resolution...")
      harmonized <- harmonize_covariate_layers(
        all_r,
        boundary_sf = if (use_boundary) boundary else NULL,
        analysis_crs = analysis_crs_string(),
        harmonize_scale = sp$harmonize_scale
      )
      if (length(harmonized) == 0L) {
        showNotification("No valid harmonized rasters for sampling.", type = "error")
        return(invisible(NULL))
      }
      ref_nm <- names(harmonized)[which.max(vapply(harmonized, function(r) max(raster::res(r)), numeric(1)))]
      ref_raster <- harmonized[[ref_nm]]
      ref_res <- tryCatch(raster::res(ref_raster), error = function(e) c(NA_real_, NA_real_))
      ref_res_m <- ref_res
      if (isTRUE(raster::isLonLat(ref_raster)) && use_boundary) {
        lat_mid <- tryCatch(as.numeric(sf::st_coordinates(sf::st_centroid(sf::st_union(boundary)))[2]), error = function(e) 0)
        ref_res_m <- c(abs(ref_res[1]) * 111320 * cos(lat_mid * pi / 180), abs(ref_res[2]) * 110540)
      }
      showNotification(
        paste0(
          "Sampling covariates harmonized to coarsest layer: ", ref_nm,
          " (approx. ", round(ref_res_m[1], 1), " x ", round(ref_res_m[2], 1), " m)."
        ),
        type = "message",
        duration = 6
      )
      setProgress(0.38, detail = "Building covariate grid...")
      combined_stack <- stack(harmonized)
      names(combined_stack) <- gsub("[ :]", "_", names(harmonized))
      leafletProxy("sampling_auto_map") %>% clearGroup("Sampling Zones") %>% removeControl("SamplingZones_legend")

      if (!use_boundary) {
          showNotification("Grid-based sampling requires a boundary AOI.", type = "error")
          return()
        }
        gsz <- suppressWarnings(as.numeric(input$grid_size_m))
        if (is.na(gsz) || gsz <= 5) {
          showNotification("Grid size must be greater than 5 meters.", type = "error")
          return()
        }
        setProgress(0.45, detail = "Creating regular grid...")
        tryCatch({
          centroid <- st_centroid(st_union(boundary))
          lon <- st_coordinates(centroid)[1]
          lat <- st_coordinates(centroid)[2]
          utm_zone <- floor((lon + 180) / 6) + 1
          hemisphere <- ifelse(lat >= 0, "north", "south")
          utm_crs <- paste0("+proj=utm +zone=", utm_zone,
                            ifelse(hemisphere == "south", " +south", ""),
                            " +datum=WGS84 +units=m +no_defs")
          
          boundary_utm <- st_transform(boundary, crs = utm_crs)
          grid_centers <- st_make_grid(boundary_utm, cellsize = gsz, what = "centers", square = TRUE)
          pts_utm <- st_as_sf(grid_centers)
          inside <- st_within(pts_utm, boundary_utm, sparse = FALSE)[, 1]
          pts_utm <- pts_utm[inside, , drop = FALSE]
          if (nrow(pts_utm) == 0) {
            showNotification("No grid points generated for this AOI and grid size. Try a larger AOI or smaller cell size.", type = "error")
            return()
          }
          if (nrow(pts_utm) > 5000L) {
            showNotification(
              paste0("Grid capped at 5000 points (", nrow(pts_utm), " were possible) for memory safety."),
              type = "warning", duration = 7
            )
            pts_utm <- pts_utm[sample.int(nrow(pts_utm), 5000L), , drop = FALSE]
          }
          
          setProgress(0.7, detail = "Extracting raster values at grid points...")
          pts <- st_transform(pts_utm, st_crs(combined_stack))
          extracted_vals <- raster::extract(combined_stack, pts, df = TRUE, na.rm = TRUE)
          pts <- st_sf(cbind(st_drop_geometry(pts), extracted_vals[, -1, drop = FALSE]), geometry = st_geometry(pts))
          pts$ID <- seq_len(nrow(pts))
          pts <- st_transform(pts, 4326)
          sample_points(pts)
          
          setProgress(1, detail = "Grid-based sampling complete.")
          showNotification(paste("Grid-based sampling complete. Generated", nrow(pts), "points at", gsz, "m grid spacing."), type = "message", duration = 7)
          render_auto_points_map(pts)
        }, error = function(e) {
          showNotification(paste("Error in grid-based sampling:", e$message), type = "error")
        })

      release_geosampler_memory()
    })
  }
  
  observeEvent(input$generate_samples_custom, {
    run_generate_automatic_samples()
  })
  
  observeEvent(input$save_auto_point_edits, {
    all_features <- input$sampling_auto_map_draw_all_features
    if (is.null(all_features)) {
      showNotification("No editable map changes found. Use the marker/edit/delete tools, then click Save Auto Point Edits.", type = "warning")
      return(invisible(NULL))
    }
    tryCatch({
      gj <- jsonlite::toJSON(all_features, auto_unbox = TRUE, null = "null")
      gj_file <- tempfile(fileext = ".geojson")
      writeLines(gj, gj_file, useBytes = TRUE)
      pts <- sf::st_read(gj_file, quiet = TRUE)
      if (is.null(pts) || nrow(pts) == 0) {
        sample_points(NULL)
        zonal_cluster_summary(NULL)
        zonal_cluster_means(NULL)
        leafletProxy("sampling_auto_map") %>% clearGroup("Sample Points")
        showNotification("All automatic points were removed from map.", type = "warning")
        return(invisible(NULL))
      }
      pts <- pts[st_geometry_type(pts) %in% c("POINT"), , drop = FALSE]
      if (nrow(pts) == 0) {
        showNotification("Only point features are supported for automatic sampling edits.", type = "error")
        return(invisible(NULL))
      }
      pts <- st_transform(pts, st_crs(TARGET_CRS))
      pts$ID <- seq_len(nrow(pts))
      
      all_r <- available_rasters()
      if (length(all_r) > 0) {
        harmonized <- harmonize_rasters(all_r, analysis_crs = analysis_crs_string(), resolution_scale = deployment_safety_params()$harmonize_scale)
        extracted_vals <- raster::extract(stack(harmonized), pts, df = TRUE, na.rm = TRUE)
        if (!is.null(extracted_vals) && ncol(extracted_vals) > 1) {
          pts <- st_sf(cbind(st_drop_geometry(pts), extracted_vals[, -1, drop = FALSE]), geometry = st_geometry(pts))
        }
      }
      
      zone_model <- zonal_cluster_model()
      if (!is.null(zone_model)) {
        pts_df <- st_drop_geometry(pts)
        zone_pred <- assign_points_to_zones(pts_df, zone_model)
        if (!is.null(zone_pred)) {
          pts$zone <- as.integer(zone_pred)
          zone_summary <- as.data.frame(table(pts$zone), stringsAsFactors = FALSE)
          names(zone_summary) <- c("Zone", "Sample_Count")
          zone_summary$Zone <- as.integer(as.character(zone_summary$Zone))
          zone_summary <- zone_summary[order(zone_summary$Zone), , drop = FALSE]
          zonal_cluster_summary(zone_summary)
        } else {
          zonal_cluster_summary(NULL)
        }
      } else {
        zonal_cluster_summary(NULL)
      }
      zonal_cluster_means(NULL)
      sample_points(st_transform(pts, 4326))
      render_auto_points_map(sample_points())
      showNotification("Automatic point edits saved (added/moved/deleted).", type = "message")
    }, error = function(e) {
      showNotification(paste("Could not save automatic point edits:", e$message), type = "error")
    })
  }, ignoreInit = TRUE)
  
  observeEvent(input$build_adaptive_sampling_recommendation, {
    if (!identical(input$sampling_method, "automatic") || !identical(input$sample_type, "Conditioned Latin Hypercube (cLHS)")) {
      showNotification("This workflow is available for automatic cLHS sampling.", type = "warning")
      return(invisible(NULL))
    }
    pts <- sample_points()
    if (is.null(pts) || nrow(pts) < 3) {
      showNotification("Generate cLHS points first, then build adaptive recommendation.", type = "warning")
      return(invisible(NULL))
    }
    all_r_raw <- sampling_selected_rasters()
    if (length(all_r_raw) == 0) {
      showNotification("No raster variables available for adaptive recommendation.", type = "warning")
      return(invisible(NULL))
    }
    sentinel_r <- all_r_raw[grepl("^Sentinel_", names(all_r_raw))]
    all_r <- if (length(sentinel_r) > 0) sentinel_r else all_r_raw

    withProgress(message = "Building adaptive sampling recommendation...", value = 0.05, {
      tryCatch({
        setProgress(0.15, detail = "Step 1/5: Building covariate stack...")
        if (length(sentinel_r) > 0) {
          showNotification(paste0("Adaptive recommendation is using Sentinel-only covariates (", length(sentinel_r), " layers)."), type = "message", duration = 5)
        } else {
          showNotification("No Sentinel-prefixed layers found; using all currently available covariates.", type = "warning", duration = 6)
        }
        boundary <- digitized_features()
        use_boundary <- !is.null(boundary)
        cropped_rasters <- lapply(all_r, function(r) {
          if (!use_boundary) return(r)
          strict_crop_mask_raster(r, boundary, exclude_boundary_touch = TRUE)
        })
        cropped_rasters <- cropped_rasters[!sapply(cropped_rasters, is.null)]
        if (!length(cropped_rasters)) stop("No valid covariate rasters after AOI mask.")
        resolutions <- sapply(cropped_rasters, function(r) max(res(r)))
        ref_idx <- which.max(resolutions)
        combined_stack <- stack(harmonize_rasters(cropped_rasters, ref_raster = cropped_rasters[[ref_idx]], analysis_crs = analysis_crs_string(), resolution_scale = deployment_safety_params()$harmonize_scale))

        setProgress(0.35, detail = "Step 2/5: Using cLHS representative points...")
        pts_cov_crs <- st_transform(pts, st_crs(combined_stack))
        pts_vals <- raster::extract(combined_stack, as(pts_cov_crs, "Spatial"), df = TRUE, na.rm = TRUE)
        pts_vals <- as.data.frame(pts_vals[, -1, drop = FALSE])
        pts_vals <- pts_vals[, sapply(pts_vals, is.numeric), drop = FALSE]
        pts_vals <- pts_vals[, sapply(pts_vals, function(v) stats::sd(v, na.rm = TRUE) > 0), drop = FALSE]
        if (ncol(pts_vals) < 2 || nrow(pts_vals) < 3) stop("Not enough valid cLHS point covariate values.")

        setProgress(0.55, detail = "Step 3/5: Computing similarity score map...")
        pop_df <- as.data.frame(combined_stack, xy = TRUE, na.rm = TRUE)
        cov_df <- pop_df[, -c(1, 2), drop = FALSE]
        cov_df <- cov_df[, intersect(names(cov_df), names(pts_vals)), drop = FALSE]
        cov_df <- cov_df[, names(pts_vals), drop = FALSE]
        pca_obj <- stats::prcomp(cov_df, center = TRUE, scale. = TRUE)
        pve <- (pca_obj$sdev^2) / sum(pca_obj$sdev^2)
        pc12 <- round(100 * sum(pve[seq_len(min(2, length(pve)))]), 1)

        center <- colMeans(pts_vals, na.rm = TRUE)
        cov_mat <- stats::cov(pts_vals, use = "pairwise.complete.obs")
        cov_mat <- cov_mat + diag(1e-8, nrow(cov_mat))
        dvals <- sqrt(stats::mahalanobis(cov_df, center = center, cov = cov_mat))
        q70 <- as.numeric(stats::quantile(dvals, probs = 0.70, na.rm = TRUE))
        q90 <- as.numeric(stats::quantile(dvals, probs = 0.90, na.rm = TRUE))
        cls <- ifelse(dvals <= q70, 1L, ifelse(dvals <= q90, 2L, 3L))

        cls_r <- raster::raster(combined_stack[[1]])
        cls_vals <- rep(NA_real_, raster::ncell(cls_r))
        cls_cells <- raster::cellFromXY(cls_r, pop_df[, c("x", "y"), drop = FALSE])
        cls_vals[cls_cells] <- cls
        raster::values(cls_r) <- cls_vals
        na_layer_top <- NULL
        # Fill interior NA holes (within AOI) using neighborhood majority class so
        # users get a fully classified field even when some covariate layers have NA gaps.
        if (isTRUE(use_boundary)) {
          boundary_sp <- tryCatch(as(boundary, "Spatial"), error = function(e) NULL)
          if (!is.null(boundary_sp)) {
            aoi_mask <- tryCatch(raster::rasterize(boundary_sp, cls_r, field = 1, background = NA), error = function(e) NULL)
            if (!is.null(aoi_mask)) {
              inside_idx <- !is.na(raster::values(aoi_mask))
              if (any(inside_idx, na.rm = TRUE)) {
                na_df <- data.frame(
                  layer = names(combined_stack),
                  na_pct_inside = vapply(seq_len(nlayers(combined_stack)), function(i) {
                    lv <- raster::values(combined_stack[[i]])
                    round(100 * (sum(is.na(lv[inside_idx])) / sum(inside_idx)), 2)
                  }, numeric(1)),
                  stringsAsFactors = FALSE
                )
                na_df <- na_df[order(na_df$na_pct_inside, decreasing = TRUE), , drop = FALSE]
                na_layer_top <- utils::head(na_df, n = min(3, nrow(na_df)))
              }
              fill_na_with_mode <- function(r, mask_r, max_iter = 6L) {
                out <- r
                for (k in seq_len(max_iter)) {
                  inside_vals <- raster::values(mask_r)
                  out_vals <- raster::values(out)
                  need_fill <- aoi_inside_need_fill(inside_vals, out_vals)
                  if (!any(need_fill)) break
                  mode3 <- raster::focal(out, w = matrix(1, 3, 3), fun = raster::modal, na.rm = TRUE, pad = TRUE, padValue = NA)
                  out_vals[need_fill] <- raster::values(mode3)[need_fill]
                  raster::values(out) <- out_vals
                }
                out
              }
              cls_r <- fill_na_with_mode(cls_r, aoi_mask, max_iter = 8L)
              # Fallback pass with a wider neighborhood for isolated holes.
              inside_vals <- raster::values(aoi_mask)
              out_vals <- raster::values(cls_r)
              need_fill <- aoi_inside_need_fill(inside_vals, out_vals)
              if (any(need_fill)) {
                mode7 <- raster::focal(cls_r, w = matrix(1, 7, 7), fun = raster::modal, na.rm = TRUE, pad = TRUE, padValue = NA)
                out_vals[need_fill] <- raster::values(mode7)[need_fill]
                raster::values(cls_r) <- out_vals
              }
              # Final fallback: guarantee full inside-boundary classification by
              # assigning remaining NA cells to the nearest classified class.
              out_vals <- raster::values(cls_r)
              need_fill <- aoi_inside_need_fill(inside_vals, out_vals)
              if (any(need_fill)) {
                d1 <- raster::distance(raster::calc(cls_r, fun = function(x) ifelse(x == 1, 1, NA)))
                d2 <- raster::distance(raster::calc(cls_r, fun = function(x) ifelse(x == 2, 1, NA)))
                d3 <- raster::distance(raster::calc(cls_r, fun = function(x) ifelse(x == 3, 1, NA)))
                m <- cbind(raster::values(d1)[need_fill], raster::values(d2)[need_fill], raster::values(d3)[need_fill])
                m[!is.finite(m)] <- Inf
                ok <- apply(m, 1, function(v) any(is.finite(v)))
                cls_nn <- rep(NA_integer_, nrow(m))
                if (safe_any_true(ok)) {
                  cls_nn[ok] <- apply(m[ok, , drop = FALSE], 1, which.min)
                }
                out_vals[which(need_fill)[ok]] <- cls_nn[ok]
                raster::values(cls_r) <- out_vals
              }
              # Hard fallback: if any inside cells remain NA, assign dominant class.
              out_vals <- raster::values(cls_r)
              need_fill <- aoi_inside_need_fill(inside_vals, out_vals)
              if (any(need_fill)) {
                inside_cls_vals <- out_vals[!is.na(inside_vals) & !is.na(out_vals)]
                if (length(inside_cls_vals) > 0) {
                  dominant_cls <- as.numeric(names(which.max(table(inside_cls_vals))))
                  out_vals[need_fill] <- dominant_cls
                  raster::values(cls_r) <- out_vals
                }
              }
            }
          }
        }

        setProgress(0.78, detail = "Step 4/5: Mapping Similar/Transition/Dissimilar areas...")
        cls_plot <- raster::calc(cls_r, fun = function(x) {
          y <- suppressWarnings(as.integer(round(x)))
          y[!(y %in% c(1L, 2L, 3L))] <- NA_integer_
          as.numeric(y)
        })
        cls_r <- cls_plot
        cls_disp <- prepare_discrete_raster_for_leaflet(cls_r)
        adaptive_pal <- colorFactor(c("#2ECC71", "#F1C40F", "#E74C3C"), domain = sort(unique(stats::na.omit(raster::values(cls_disp)))))
        adaptive_similarity_raster(cls_r)
        leafletProxy("sampling_auto_map") %>%
          clearGroup("Adaptive Similarity Classes") %>%
          addRasterImage(cls_disp, colors = adaptive_pal, opacity = 0.8, group = "Adaptive Similarity Classes", project = FALSE) %>%
          removeControl("AdaptiveSimilarity_legend") %>%
          addLegend(position = "bottomright", colors = c("#2ECC71", "#F1C40F", "#E74C3C"),
                    labels = c("Similar", "Transition", "Dissimilar"),
                    title = "Adaptive Similarity Classes", layerId = "AdaptiveSimilarity_legend")
        render_auto_points_map(pts)

        setProgress(0.95, detail = "Step 5/5: Writing adaptive recommendation...")
        pt_cls <- suppressWarnings(as.integer(raster::extract(cls_r, as(pts_cov_crs, "Spatial"))))
        cls_tab <- as.data.frame(table(factor(pt_cls, levels = c(1, 2, 3))), stringsAsFactors = FALSE)
        names(cls_tab) <- c("class_id", "point_count")
        cls_tab$class_name <- c("Similar", "Transition", "Dissimilar")

        cls_vals_full <- raster::values(cls_r)
        cell_tab <- as.data.frame(table(factor(suppressWarnings(as.integer(cls_vals_full)), levels = c(1, 2, 3))), stringsAsFactors = FALSE)
        names(cell_tab) <- c("class_id", "cell_count")
        cell_tab$class_name <- c("Similar", "Transition", "Dissimilar")
        total_classified_cells <- sum(cell_tab$cell_count)
        cell_tab$field_share_pct <- if (total_classified_cells > 0) round(100 * (cell_tab$cell_count / total_classified_cells), 1) else 0
        unclassified_cells <- sum(is.na(cls_vals_full))
        unclassified_pct <- round(100 * (unclassified_cells / length(cls_vals_full)), 1)
        inside_unclassified_pct <- NA_real_
        if (isTRUE(use_boundary)) {
          boundary_sp <- tryCatch(as(boundary, "Spatial"), error = function(e) NULL)
          if (!is.null(boundary_sp)) {
            boundary_mask <- tryCatch(raster::rasterize(boundary_sp, cls_r, field = 1, background = NA), error = function(e) NULL)
            if (!is.null(boundary_mask)) {
              inside_idx <- !is.na(raster::values(boundary_mask))
              inside_total <- sum(inside_idx)
              if (inside_total > 0) {
                inside_unclassified_cells <- sum(is.na(cls_vals_full[inside_idx]))
                inside_unclassified_pct <- round(100 * (inside_unclassified_cells / inside_total), 1)
              }
            }
          }
        }

        total_point_n <- sum(cls_tab$point_count)
        cls_tab$point_share_pct <- if (total_point_n > 0) round(100 * (cls_tab$point_count / total_point_n), 1) else 0

        adaptive_recommendation_summary(list(
          n_points = nrow(pts),
          pca_pc12 = pc12,
          quantiles = c(q70 = q70, q90 = q90),
          used_layer_count = length(all_r),
          class_point_counts = cls_tab,
          class_field_counts = cell_tab,
          unclassified_pct = unclassified_pct,
          inside_unclassified_pct = inside_unclassified_pct,
          na_layer_top = na_layer_top
        ))
        adaptive_recommendation_hidden(FALSE)
        setProgress(1, detail = "Adaptive recommendation ready.")
        showNotification("Adaptive sampling recommendation ready: Similar/Transition/Dissimilar classes are now mapped.", type = "message", duration = 8)
      }, error = function(e) {
        showNotification(paste("Could not build adaptive recommendation:", e$message), type = "error", duration = 10)
      })
    })
  }, ignoreInit = TRUE)

  observeEvent(input$show_adaptive_similarity_classes, {
    zr <- adaptive_similarity_raster()
    if (is.null(zr)) return(invisible(NULL))
    zr <- raster::calc(zr, fun = function(x) {
      y <- suppressWarnings(as.integer(round(x)))
      y[!(y %in% c(1L, 2L, 3L))] <- NA_integer_
      as.numeric(y)
    })
    zr_disp <- prepare_discrete_raster_for_leaflet(zr)
    adaptive_pal <- colorFactor(c("#2ECC71", "#F1C40F", "#E74C3C"), domain = sort(unique(stats::na.omit(raster::values(zr_disp)))))
    if (isTRUE(input$show_adaptive_similarity_classes)) {
      leafletProxy("sampling_auto_map") %>%
        addRasterImage(zr_disp, colors = adaptive_pal, opacity = 0.8, group = "Adaptive Similarity Classes", project = FALSE) %>%
        removeControl("AdaptiveSimilarity_legend") %>%
        addLegend(position = "bottomright", colors = c("#2ECC71", "#F1C40F", "#E74C3C"),
                  labels = c("Similar", "Transition", "Dissimilar"),
                  title = "Adaptive Similarity Classes", layerId = "AdaptiveSimilarity_legend")
    } else {
      leafletProxy("sampling_auto_map") %>%
        clearGroup("Adaptive Similarity Classes") %>%
        removeControl("AdaptiveSimilarity_legend")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$close_adaptive_recommendation, {
    adaptive_recommendation_hidden(TRUE)
  }, ignoreInit = TRUE)

  clear_generated_sampling_outputs <- function() {
    unlock_generate_sample_buttons()
    unlock_summary_compute_buttons()
    sample_points(NULL)
    manual_points(NULL)
    zonal_cluster_summary(NULL)
    zonal_cluster_means(NULL)
    zonal_cluster_model(NULL)
    zonal_zone_raster(NULL)
    zonal_zone_count(NULL)
    clhs_similarity_zone(NULL)
    clhs_weak_gps_zone(NULL)
    clhs_similarity_threshold(NULL)
    clhs_similarity_polygon_count(NULL)
    clhs_weak_zone_area_ha(NULL)
    adaptive_recommendation_summary(NULL)
    adaptive_similarity_raster(NULL)
    adaptive_recommendation_hidden(TRUE)
    session$sendCustomMessage("clearLeafletDrawFeatures", list(mapId = "sampling_auto_map"))
    session$sendCustomMessage("clearLeafletDrawFeatures", list(mapId = "sampling_manual_map-map"))
    leafletProxy("sampling_auto_map") %>%
      clearGroup("Sample Points") %>%
      clearGroup("Adaptive Similarity Classes") %>%
      clearMarkers() %>%
      clearShapes() %>%
      removeControl("AdaptiveSimilarity_legend") %>%
      removeControl("CLHSSimilarity_legend")
    leafletProxy("sampling_manual_map-map") %>%
      clearMarkers() %>%
      clearShapes()
    manual_points_plot_df(NULL)
    showNotification("Generated sampling points removed. You can try another method/settings.", type = "message")
    release_geosampler_memory()
  }

  clear_sampling_after_comparison <- function() {
    sample_points(NULL)
    manual_points(NULL)
    zonal_cluster_summary(NULL)
    zonal_cluster_means(NULL)
    zonal_cluster_model(NULL)
    zonal_zone_raster(NULL)
    zonal_zone_count(NULL)
    clhs_similarity_zone(NULL)
    clhs_weak_gps_zone(NULL)
    clhs_similarity_threshold(NULL)
    clhs_similarity_polygon_count(NULL)
    clhs_weak_zone_area_ha(NULL)
    adaptive_recommendation_summary(NULL)
    adaptive_similarity_raster(NULL)
    adaptive_recommendation_hidden(TRUE)
    session$sendCustomMessage("clearLeafletDrawFeatures", list(mapId = "sampling_auto_map"))
    tryCatch({
      leafletProxy("sampling_auto_map") %>%
        clearGroup("Sample Points") %>%
        clearGroup("Sampling Zones") %>%
        clearGroup("Adaptive Similarity Classes") %>%
        clearMarkers() %>%
        clearShapes() %>%
        removeControl("SamplingZones_legend") %>%
        removeControl("AdaptiveSimilarity_legend") %>%
        removeControl("CLHSSimilarity_legend")
    }, error = function(e) invisible(NULL))
    release_geosampler_memory()
  }

  observeEvent(
    c(input$clear_generated_samples, input$clear_generated_samples_custom),
    clear_generated_sampling_outputs(),
    ignoreInit = TRUE
  )

  output$sampling_custom_actions_ui <- renderUI({
    if (!identical(input$sampling_method, "automatic")) return(NULL)
    res <- comparison_results()
    has_cmp <- !is.null(res) && !is.null(res$n_points)
    settings_differ <- has_cmp && !isTRUE(sampling_prefill_active())
    n_cur <- RECOMMENDED_GENERATION_REPEATS() + GENERATION_CURRENT_SETTINGS_EXTRA_REPEATS()
    gen_label <- if (settings_differ) {
      paste0("Generate sample points (current settings, ", n_cur, " reps, ≥60% spread)")
    } else {
      paste0("Generate sample points (", n_cur, " reps, ≥60% spread target)")
    }
    if (has_cmp && isTRUE(sampling_prefill_active())) {
      return(NULL)
    }
    tagList(
      if (settings_differ) {
        tags$div(
          style = "border-left:3px solid #6c9bd1; padding-left:12px; margin:8px 0 10px;",
          tags$p(
            strong("Current settings differ from the technique comparison snapshot."),
            style = "margin-bottom:6px; font-size:13px;"
          ),
          tags$p(
            class = "text-muted",
            style = "font-size:12px; margin-bottom:10px;",
            "You changed parameters after comparison. ",
            strong("Generate sample points (current settings)"),
            " runs ",
            RECOMMENDED_GENERATION_REPEATS() + GENERATION_CURRENT_SETTINGS_EXTRA_REPEATS(),
            " replicates of your chosen method and sample size (Explore more) and keeps the highest field coverage (≥60% when possible). Reset ",
            strong("Sampling Parameters"),
            " to match the recommendation card for identical comparison alignment."
          )
        )
      },
      tags$div(
        class = "action-toolbar",
        actionButton("generate_samples_custom", gen_label, class = "btn-primary btn-one-shot"),
        actionButton("clear_generated_samples_custom", "Clear sampling points", class = "btn-danger btn-sm")
      )
    )
  })
  
  observeEvent(input$sampling_auto_map_draw_new_feature, {
    showNotification("Point edit detected. Click 'Save Auto Point Edits' to apply changes to table/download.", type = "message", duration = 5)
  }, ignoreInit = TRUE)
  observeEvent(input$sampling_auto_map_draw_edited_features, {
    showNotification("Point edit detected. Click 'Save Auto Point Edits' to apply changes to table/download.", type = "message", duration = 5)
  }, ignoreInit = TRUE)
  observeEvent(input$sampling_auto_map_draw_deleted_features, {
    showNotification("Point edit detected. Click 'Save Auto Point Edits' to apply changes to table/download.", type = "message", duration = 5)
  }, ignoreInit = TRUE)
  
  observeEvent(sample_points(), {
    if (!is.null(sample_points()) && nrow(sample_points()) > 0L) {
      unlock_summary_compute_buttons()
    }
  }, ignoreNULL = FALSE)
  
  output$comparison_headline_ui <- renderUI({
    res <- comparison_results()
    has_res <- !is.null(res) && !is.null(res$table) && nrow(res$table) > 0

    winner_txt <- if (!has_res) {
      "Run Compare Sampling Techniques to see the winning method and recommended sample size."
    } else {
      buf <- if (!is.null(res$recommended_buffer_m)) {
        paste0(" Recommended buffer: ", res$recommended_buffer_m, " m.")
      } else ""
      spread <- if (!is.null(res$spread_pick_rep)) {
        fc <- res$spread_pick_field_coverage_pct
        fc_txt <- if (is.finite(fc)) paste0(round(fc, 1), "% field coverage") else NULL
        if (!is.null(fc_txt)) {
          paste0(
            " Last generate: replicate ", res$spread_pick_rep, "/", RECOMMENDED_GENERATION_REPEATS(),
            " (", fc_txt, ")."
          )
        } else if (is.finite(res$spread_pick_score)) {
          paste0(
            " Last generate: replicate ", res$spread_pick_rep, "/", RECOMMENDED_GENERATION_REPEATS(),
            " (spread index ", res$spread_pick_score, " / 1)."
          )
        } else ""
      } else ""
      paste0(res$winner, " at n = ", res$n_points, ".", buf, spread)
    }

    tags$ul(
      style = "margin:8px 0 10px 0; padding-left:20px; font-size:13px; line-height:1.45;",
      tags$li(tags$strong("Winner & recommended sample size: "), winner_txt)
    )
  })

  output$comparison_performance_ui <- renderUI({
    res <- comparison_results()
    has_curve <- !is.null(res) && !is.null(res$all_summary) && nrow(res$all_summary) > 0
    tagList(
      tags$p(style = "margin:8px 0 4px 0; font-size:13px; font-weight:700;", "Performance by sample size"),
      if (has_curve) {
        plotOutput("comparison_performance_curve", height = "420px")
      } else {
        tags$p(
          class = "text-muted",
          style = "font-size:12px; margin:0 0 12px 0;",
          "Score curves appear after comparison completes."
        )
      }
    )
  })

  output$comparison_podium_ui <- renderUI({
    res <- comparison_results()
    if (is.null(res) || is.null(res$table) || nrow(res$table) < 3) {
      return(tags$p(
        class = "text-muted",
        style = "font-size:12px; margin:4px 0 12px 0;",
        strong("Top 3 methods: "),
        "podium appears after comparison completes."
      ))
    }
    top3 <- res$table[1:3, c("Rank", "Method"), drop = FALSE]
    ord_suf <- function(n) {
      k <- n %% 100
      if (k >= 11L && k <= 13L) return("th")
      d <- n %% 10L
      if (d == 1L) return("st")
      if (d == 2L) return("nd")
      if (d == 3L) return("rd")
      "th"
    }
    get_card <- function(i, bg, height_px) {
      rk <- as.integer(top3$Rank[i])
      place_lbl <- if (rk == 1L) {
        paste0("\U0001F451 ", rk, ord_suf(rk), " place")
      } else {
        paste0(rk, ord_suf(rk), " place")
      }
      tags$div(
        style = paste0(
          "flex:1; min-width:180px; background:", bg,
          "; border-radius:12px; padding:10px; margin:6px; height:", height_px, "px;",
          "display:flex; flex-direction:column; justify-content:flex-end; text-align:center; color:#1f2d3d; box-shadow:0 6px 14px rgba(0,0,0,0.08);"
        ),
        tags$div(style = "font-size:14px; font-weight:700;", place_lbl),
        tags$div(style = "font-size:16px; font-weight:700; margin-top:4px;", top3$Method[i])
      )
    }
    tagList(
      tags$p(style = "margin:0 0 4px 0; font-weight:700; font-size:13px;", "Top 3 methods"),
      tags$div(
        style = "display:flex; align-items:flex-end; justify-content:center; flex-wrap:wrap; margin:0 0 14px 0;",
        get_card(2, "#d9e6f7", 120),
        get_card(1, "#ffe8a3", 150),
        get_card(3, "#eedfd6", 105)
      )
    )
  })

  output$comparison_performance_curve <- renderPlot({
    res <- comparison_results()
    req(!is.null(res), !is.null(res$all_summary), nrow(res$all_summary) > 0)
    p <- build_comparison_curve_plot(res)
    req(!is.null(p))
    print(p)
  })

  output$comparison_covariate_balance_status_ui <- renderUI({
    res <- comparison_results()
    if (is.null(res) || is.null(res$covariate_balance) || nrow(res$covariate_balance) < 1L) {
      return(tags$div(
        class = "text-muted",
        style = "margin:6px 0; font-size:12px;",
        "Run comparison to see how the winning method's best repeat differs from the population per covariate (stats rounded to 3 decimals; % diff from those rounded values)."
      ))
    }
    tags$div(
      class = "text-muted",
      style = "margin:6px 0; font-size:12px;",
      paste0(
        res$winner, " at n = ", res$n_points,
        " — best repeat vs population (3-decimal stats; % diff: positive = sample above population)."
      )
    )
  })

  output$comparison_covariate_balance_table <- DT::renderDataTable({
    res <- comparison_results()
    req(!is.null(res), !is.null(res$covariate_balance), nrow(res$covariate_balance) > 0)
    d <- res$covariate_balance
    colnms <- c(
      "Layer",
      "Min (population)", "Min (sample)", "% diff min",
      "Max (population)", "Max (sample)", "% diff max",
      "Mean (population)", "Mean (sample)", "% diff mean",
      "Median (population)", "Median (sample)", "% diff median"
    )
    colnames(d) <- colnms
    pop_samp_cols <- c(2, 3, 5, 6, 8, 9, 11, 12)
    pct_cols <- c(4, 7, 10, 13)
    dt <- DT::datatable(
      d,
      rownames = FALSE,
      options = list(dom = "ft", pageLength = 12, scrollX = TRUE),
      class = "compact stripe hover nowrap"
    ) %>%
      DT::formatRound(columns = pop_samp_cols, digits = POP_SAMPLE_STAT_DIGITS) %>%
      DT::formatRound(columns = pct_cols, digits = POP_SAMPLE_PCT_DIGITS)
    dt <- DT::formatStyle(dt, columns = c(2, 5, 8, 11), backgroundColor = "#e6f2fc", fontWeight = "600")
    dt <- DT::formatStyle(dt, columns = c(3, 6, 9, 12), backgroundColor = "#e8f7ec", fontWeight = "600")
    dt <- DT::formatStyle(dt, columns = c(4, 7, 10, 13), backgroundColor = "#fff4e6", fontWeight = "600")
    dt
  })

  output$comparison_methods_at_n_table <- DT::renderDataTable({
    res <- comparison_results()
    req(!is.null(res), !is.null(res$methods_at_n), nrow(res$methods_at_n) > 0)
    d <- compare_raw_metrics_display_df(order_sampling_methods_df(res$methods_at_n))
    num_cols <- which(sapply(d, is.numeric))
    dt <- DT::datatable(d, rownames = FALSE, options = list(pageLength = 6, scrollX = TRUE, dom = "tip"), class = "compact stripe hover")
    if (length(num_cols)) dt <- DT::formatRound(dt, columns = num_cols, digits = 3)
    dt
  })
  
  output$comparison_method_description_ui <- renderUI({
    tags$details(
      style = "margin:10px 0; border:1px solid #d8e6fb; border-radius:12px; background:linear-gradient(120deg,#f8fbff 0%,#eef6ff 100%); padding:8px 10px;",
      tags$summary(strong("How comparison was performed")),
      tags$ul(
        tags$li(strong("Simple Random:"), " draws points uniformly from valid covariate cells; baseline for unbiased spatial coverage."),
        tags$li(strong("Systematic Spread:"), " deterministic spatial spread: k-means on XY then one population cell nearest each cluster centroid (ties/rare empty clusters fall back to rank-ordered thinning)."),
        tags$li(strong("Spread + cLHS:"), " builds a spatially spread candidate pool (~3× n), then runs cLHS within that pool (same design as Sampling → Spread + cLHS (best coverage))."),
        tags$li(strong("cLHS:"), " optimizes point selection to match multivariate covariate distributions using conditioned Latin hypercube logic (fast screening mode in comparison)."),
        tags$li(strong("Zone-based:"), " k-means zones on standardized covariates; sample counts per zone use Neyman allocation (proportional to zone area × mean within-zone column variance), then random points within each zone."),
        tags$li(strong("Hybrid Zonal cLHS:"), " same zoning as zone-based; Neyman allocation across zones, then conditioned Latin hypercube within each zone."),
        tags$li(strong("Univariate Distribution Comparison:"), " Kolmogorov-Smirnov similarity per variable, averaged."),
        tags$li(strong("Multivariate Representativeness:"), " Mahalanobis-distance based similarity between sample and population centers."),
        tags$li(strong("PCA Coverage:"), " for PCs explaining 90% of population variance: sample span vs expected span at this ", strong("n"), " (robust 5th–95th population range), PC-space centering, and share of the population core (10th–90th) inside the sample envelope."),
        tags$li(strong("Range & quantile tails:"), " combines span across the population range with whether sample min/max reach the lower and upper tails (10th/90th percentiles, with depth into 5th/95th)."),
        tags$li(strong("Correlation Structure Preservation:"), " compares pairwise-correlation matrices between sample and population."),
        tags$li(strong("Spatial coverage:"), " average of (a) nearest-neighbor spacing vs a regular reference and (b) share of coarse grid cells in the AOI touched by sample points (higher = better geographic spread)."),
        tags$li(strong("Final score:"), " per repeat, the six design metrics are converted to rank scores across the six methods (rank 1 = best). Those feed a weighted sum with ", strong(COMPARE_WEIGHTS_LABEL), ". Repeats are averaged per method/size, then re-ranked for the curves."),
        tags$li(strong("Multiple Sample Sizes + Repeats:"), " methods are re-evaluated for each selected sample size across repeats; stochastic methods (especially cLHS) are run multiple times because you would pick the best configuration in practice."),
        tags$li(strong("Best Sample Size Rules:"), " elbow, threshold, minimum range coverage, and cost-vs-accuracy each suggest an ", strong("n"), " (rule-consensus included). Ties use a ", strong("fair pick"), " closest to the middle of evaluated sizes—not always the smallest or largest ", strong("n"), ". Final recommendation is the fair pick among sizes within 2% of the top final score."),
        tags$li(strong("Covariate grid:"), " layers are harmonized to the ", strong("coarsest"), " selected raster resolution (not resampled by AOI size)."),
        tags$li(strong("AOI buffer:"), " recommended negative buffer by AOI size: ≤10 ha → 15 m; 10–30 ha → 25 m; 30–100 ha → 40 m; >100 ha → 80 m (applied on ", strong("Generate using recommendation"), ")."),
        tags$li(strong("Final recommendation:"), " among methods with nearly the same final score at the recommended ", strong("n"), " the app prefers lower repeat-to-repeat variability, then stronger ", strong("range & quantile-tail"), " coverage, then spatial coverage."),
        tags$li(strong("Generate using recommendation"), " and ", strong("Generate sample points"), " (non–grid methods) both call the same engine: ", strong(as.character(RECOMMENDED_GENERATION_REPEATS())), " replicates on the full harmonized grid, pick by ", strong("field coverage spread %"), ", light polish; recommendation also syncs sidebar to the winner."),
        tags$li(strong("Interpretation tip:"), " compare smaller ", strong("n"), " where methods diverge, then larger ", strong("n"), " where covariate metrics converge; design methods with explicit zoning or systematic spread often win on map coverage. For logistics alone, SRS is simplest—use zone- or HZC-style designs when geographic coverage must be enforced."),
      )
    )
  })

  outputOptions(output, "comparison_performance_curve", suspendWhenHidden = TRUE)
  outputOptions(output, "comparison_headline_ui", suspendWhenHidden = TRUE)
  outputOptions(output, "comparison_podium_ui", suspendWhenHidden = TRUE)
  outputOptions(output, "comparison_performance_ui", suspendWhenHidden = TRUE)
  outputOptions(output, "comparison_methods_at_n_table", suspendWhenHidden = TRUE)
  outputOptions(output, "comparison_covariate_balance_table", suspendWhenHidden = TRUE)
  outputOptions(output, "comparison_covariate_balance_status_ui", suspendWhenHidden = TRUE)
  outputOptions(output, "comparison_method_description_ui", suspendWhenHidden = TRUE)
  
  output$sampling_best_recommendation_ui <- renderUI({
    res <- comparison_results()
    if (is.null(res) || is.null(res$table) || nrow(res$table) == 0) {
      return(
        tags$details(
          class = "sampling-rec-card sampling-rec-card-empty disclosure-card",
          style = "margin:8px 0; border:1px solid #e2ecf7; border-radius:10px; padding:6px 10px; background:#fbfdff;",
          tags$summary(class = "rec-card-title", "Recommended from Technique comparison"),
          tags$p(
            class = "text-muted",
            style = "margin:6px 0 0 0; font-size:11px;",
            "Run ", strong("Technique comparison"), " first. One-click generate uses the same ",
            RECOMMENDED_GENERATION_REPEATS(), "-replicate field-coverage pick as ",
            strong("Generate sample points"), "."
          )
        )
      )
    }
    pre <- isTRUE(sampling_prefill_active())
    actions <- tags$div(
      style = "margin-top:8px; display:flex; flex-wrap:wrap; gap:6px; align-items:center;",
      actionButton("generate_samples_recommended", "Generate using recommendation (one click)", class = "btn-primary btn-sm btn-one-shot"),
      actionButton("clear_generated_samples", "Clear sampling points", class = "btn-danger btn-sm"),
      downloadButton("download_samples", "Download sample points (GeoJSON)", class = "btn-sm")
    )
    note <- if (!pre) {
      tags$div(
        class = "text-muted",
        style = "margin:8px 0 0 0; font-size:11px;",
        tags$p(
          style = "margin:0;",
          "You changed sidebar settings. Use ",
          strong("Generate sample points"),
          " on Generate sample points, or reset parameters to match the recommendation."
        )
      )
    } else NULL
    tags$div(
      class = "sampling-rec-card",
      tags$div(class = "rec-card-title", "Recommended from Technique comparison"),
      tags$p(style = "margin:4px 0 0 0;", paste0("Best method: ", res$winner)),
      tags$p(style = "margin:2px 0 0 0;", paste0("Best sample size: ", res$n_points)),
      tags$p(
        class = "text-muted",
        style = "font-size:11px; margin:4px 0 0 0;",
        paste0(
          "One-click generate uses the same ", RECOMMENDED_GENERATION_REPEATS(),
          "-replicate field-coverage pick as Generate sample points."
        )
      ),
      actions,
      note
    )
  })

  output$sampling_quality_dashboard_ui <- renderUI({
    pts <- sample_points()
    if (is.null(pts) || nrow(pts) < 2) return(NULL)
    b <- digitized_features()
    spread_ctx <- sampling_spread_pick_context()
    qm <- compute_sampling_quality_metrics(
      pts, b, analysis_crs_string(), spread_grid_context = spread_ctx
    )
    zone_bal_txt <- if (is.finite(qm$zone_bal)) paste0(round(qm$zone_bal, 1), "%") else "NA"
    spread_txt <- if (is.finite(qm$spread_pct)) paste0(round(qm$spread_pct, 1), "%") else "NA"
    even_txt <- if (is.finite(qm$even_spread_idx)) round(qm$even_spread_idx, 3) else "NA"
    n_reps_lbl <- RECOMMENDED_GENERATION_REPEATS()
    fc_hint <- if (is.finite(qm$spread_pct) && qm$spread_pct < 60) {
      " For 60%+ coverage, prefer Systematic Spread or Spread + cLHS in technique comparison, or increase sample size."
    } else ""
    tags$details(
      open = "open",
      style = "margin:8px 0; border:1px solid #d8e6fb; border-radius:12px; background:linear-gradient(120deg,#f8fbff 0%,#eef6ff 100%); padding:8px 10px;",
      tags$summary(strong("Sampling Quality Dashboard")),
      tags$ul(
        tags$li(paste0("Typical point spacing: ", ifelse(is.finite(qm$nn_med), round(qm$nn_med, 2), NA), " m (higher means points are farther apart).")),
        tags$li(paste0("Zone allocation quality: ", zone_bal_txt, ifelse(identical(zone_bal_txt, "NA"), " (available for zone-labeled sampling only)", ""))),
        tags$li(paste0(
          "Field coverage spread: ", spread_txt,
          " (target 60%+; Generate picks the best of ", n_reps_lbl, " replicates on this metric).", fc_hint
        )),
        tags$li(paste0("Geographic spread index (", n_reps_lbl, "-rep pick): ", even_txt, " (0–1 auxiliary score)."))
      )
    )
  })
  
  output$sampling_comparison_how_ui <- renderUI({
    tags$details(
      style = "margin-bottom:10px; border:1px solid #d8e6fb; border-radius:12px; background:linear-gradient(120deg,#f8fbff 0%,#eef6ff 100%); padding:8px 10px;",
      tags$summary(strong("How Technique comparison was performed")),
      tags$ul(
        tags$li("Covariates = layers you select under automatic sampling (defaults to all loaded layers); harmonized to one grid."),
        tags$li("Methods compared: Simple Random, Systematic Spread, Spread + cLHS, cLHS, Zone-based (Neyman allocation across k-means zones), Hybrid Zonal cLHS (Neyman + within-zone cLHS)."),
        tags$li("Each selected sample size is repeated multiple times; curves show the mean final score across repeats (rank-based composite)."),
        tags$li("Metrics: KS/univariate, Mahalanobis, PCA coverage, range/quantile tails, correlation, and spatial/field coverage (modest weight to limit bias toward spread-only designs)."),
        tags$li(paste0("Per repeat: rank-based scoring across six methods, then weighted (", COMPARE_WEIGHTS_LABEL, "). Recommended ", strong("n"), " uses fair tie-breaking among near-best sizes.")),
        tags$li("Elbow, threshold, minimum range coverage, and cost-vs-accuracy each suggest an n (see recommendation table). The recommended n is a fair pick among top-scoring sizes—not automatically the minimum or maximum evaluated size.")
      )
    )
  })
  
  observeEvent(
    list(sample_points(), names(available_rasters())),
    {
      unlock_action_button("compute_variables_summary")
    },
    ignoreInit = TRUE
  )

  observeEvent(manual_points(), {
    mp <- manual_points()
    if (is.null(mp) || nrow(mp) < 1L) {
      manual_points_plot_df(NULL)
      return(invisible(NULL))
    }
    all_r <- available_rasters()
    if (length(all_r) == 0L) {
      manual_points_plot_df(NULL)
      return(invisible(NULL))
    }
    tryCatch({
      harmonized <- harmonize_covariate_layers(
        all_r,
        boundary_sf = digitized_features(),
        analysis_crs = analysis_crs_string(),
        harmonize_scale = deployment_safety_params()$harmonize_scale
      )
      if (length(harmonized) == 0L) {
        manual_points_plot_df(NULL)
        return(invisible(NULL))
      }
      extracted_vals <- raster::extract(stack(harmonized), mp, df = TRUE)
      manual_points_plot_df(cbind(st_drop_geometry(mp), extracted_vals[, -1, drop = FALSE]))
      rm(harmonized, extracted_vals)
      release_geosampler_memory()
    }, error = function(e) {
      manual_points_plot_df(NULL)
      showNotification(paste("Error extracting values for manual points:", e$message), type = "error")
    })
  }, ignoreNULL = FALSE)
  
  # --- Download Handlers (vegetation indices) ---
  for (vi_base in standard_vi_base_names()) {
    local({
      base_nm <- vi_base
      output[[vi_download_output_id("", base_nm)]] <- downloadHandler(
        filename = function() paste0(base_nm, ".tif"),
    content = function(file) {
          req(length(vi_rasters()) > 0, base_nm %in% names(vi_rasters()))
          writeRaster(vi_rasters()[[base_nm]], file, overwrite = TRUE)
    }
  )
      output[[vi_download_output_id("sentinel", base_nm)]] <- downloadHandler(
    filename = function() {
      suf <- if (identical(sentinel_retrieval_used(), "median")) "_median" else ""
          paste0("Sentinel_", base_nm, suf, ".tif")
    },
    content = function(file) {
          nm <- if (identical(sentinel_retrieval_used(), "median")) paste0(base_nm, "_median") else base_nm
          req(length(sentinel_vi_rasters()) > 0, nm %in% names(sentinel_vi_rasters()))
      writeRaster(sentinel_vi_rasters()[[nm]], file, overwrite = TRUE)
    }
  )
      output[[vi_download_output_id("ms", base_nm)]] <- downloadHandler(
        filename = function() paste0("MS_", base_nm, ".tif"),
    content = function(file) {
          req(length(ms_vi_rasters()) > 0, base_nm %in% names(ms_vi_rasters()))
          writeRaster(ms_vi_rasters()[[base_nm]], file, overwrite = TRUE)
        }
      )
    })
  }
  
  output$download_slope <- downloadHandler(filename = "Slope.tif", content = function(file) { writeRaster(elevation_aux_layers()[["Slope"]], file, overwrite = TRUE) })
  output$download_aspect <- downloadHandler(filename = "Aspect.tif", content = function(file) { writeRaster(elevation_aux_layers()[["Aspect"]], file, overwrite = TRUE) })
  output$download_tpi <- downloadHandler(filename = "TPI.tif", content = function(file) { writeRaster(elevation_aux_layers()[["TPI"]], file, overwrite = TRUE) })
  output$download_twi <- downloadHandler(filename = "TWI.tif", content = function(file) { writeRaster(elevation_aux_layers()[["TWI"]], file, overwrite = TRUE) })
  
  create_zip_download <- function(raster_list, zip_name) {
    downloadHandler(
      filename = function() { zip_name },
      content = function(file) {
        temp_dir <- tempdir()
        files <- c()
        for(name in names(raster_list)){
          path <- file.path(temp_dir, paste0(name, ".tif"))
          writeRaster(raster_list[[name]], path, overwrite=TRUE)
          files <- c(files, path)
        }
        zip::zip(zipfile=file, files=basename(files), root=temp_dir)
      }
    )
  }
  
  output$soil_download_ui <- renderUI({
    if (length(soil_layers()) > 0) {
      downloadButton("download_soil_zip", "Download Soil Layers (ZIP)")
    }
  })
  
  output$download_soil_zip <- create_zip_download(soil_layers(), "soil_layers.zip")
  
  output$other_download_ui <- renderUI({
    if (length(other_layers()) > 0) {
      downloadButton("download_other_zip", "Download Other Layers (ZIP)")
    }
  })
  
  output$download_other_zip <- create_zip_download(other_layers(), "other_layers.zip")
  
  output$elevation_download_ui <- renderUI({
    if (!is.null(uploaded_elevation_raster())) {
      downloadButton("download_elevation_tif", "Download Elevation GeoTIFF")
    }
  })
  
  output$download_samples <- downloadHandler(
    filename = function() { paste0("sample_points_", Sys.Date(), ".geojson") },
    content = function(file) { req(!is.null(sample_points())); suppressWarnings(st_write(sample_points(), file, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)) }
  )
  output$download_manual_points <- downloadHandler(
    filename = function() { paste0("manual_points_", Sys.Date(), ".geojson") },
    content = function(file) { req(!is.null(manual_points())); suppressWarnings(st_write(manual_points(), file, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)) }
  )
  
  output$individual_vi_calculator_ui <- renderUI({})
}

shinyApp(ui, server)