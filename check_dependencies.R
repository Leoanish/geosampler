required_pkgs <- c(
  "shiny", "bslib", "dplyr", "tibble", "sf", "leaflet",
  "leaflet.extras", "mapedit", "shinyjs", "httr", "jsonlite", "promises",
  "future", "later", "parallelly", "progress", "raster", "terra", "zip", "shinycssloaders",
  "elevatr", "clhs", "viridis", "DT", "planetR", "rstac", "ggplot2", "lubridate"
)

missing <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop(
    "Missing packages: ", paste(missing, collapse = ", "),
    "\nInstall with: install.packages(c(", paste0('"', missing, '"', collapse = ", "), "))"
  )
}

parse(file.path("app.R"))
cat("All", length(required_pkgs), "packages are installed and app.R parses OK.\n")
