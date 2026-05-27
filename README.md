# GeoSampler — Sentinel-2 Sampling Dashboard

> A modern geospatial sampling workspace for field planning, remote sensing analysis, and statistically defensible point design — built with R Shiny.

[![Live Demo](https://img.shields.io/badge/Live%20Demo-shinyapps.io-blue?logo=r)](https://recuga.shinyapps.io/sentinel2_trial/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![R](https://img.shields.io/badge/R-%3E%3D4.1-276DC3?logo=r)](https://cran.r-project.org/)
[![GitHub](https://img.shields.io/badge/GitHub-Leoanish%2Fgeosampler-181717?logo=github)](https://github.com/Leoanish/geosampler)

---

## What Is GeoSampler?

GeoSampler helps agronomists, ecologists, and field scientists move from **area definition → satellite imagery → statistically optimised sample locations** in one guided browser-based workflow — no GIS software required.

**Key capabilities:**

- Draw or upload an Area of Interest (AOI) polygon
- Retrieve Sentinel-2 or Planet multispectral imagery automatically, or upload your own raster stack
- Pull elevation data (DEM) and auto-compute terrain derivatives (slope, aspect, TWI, TPI)
- Add any supporting GeoTIFF predictors (soil maps, yield layers, etc.)
- Compare six sampling strategies (Simple Random, Systematic, Spread + cLHS, cLHS, Zone-based, Hybrid Zonal cLHS) with a built-in benchmark curve
- Generate export-ready GPS points (GeoJSON)
- Compare new sample locations against historical field visits (distribution plots + map)
- Estimate cost savings vs. a prior uniform/grid design
- Download a one-page PDF field-plan report

---

## Live Demo

**[https://recuga.shinyapps.io/sentinel2_trial/](https://recuga.shinyapps.io/sentinel2_trial/)**

No installation needed — open the link, follow the in-app workflow tree.

---

## Repository Structure

```
sentinel2_trial/
├── app.R                          # Main Shiny application (~15 000 lines)
├── check_dependencies.R           # Quick dependency checker — run before first launch
├── sentinel_workflow_functions.qmd# Standalone function API (script / future R package)
├── user_guide.qmd                 # Step-by-step user guide (render with Quarto)
├── www/
│   ├── geo_sampler.css            # Dashboard stylesheet
│   └── geo_sampler.js             # Client-side helpers
└── .gitignore
```

---

## Quick Start — Run Locally

### 1. Prerequisites

- **R ≥ 4.1** — [Download R](https://cran.r-project.org/)
- **RStudio** (recommended) — [Download RStudio](https://posit.co/downloads/)
- **Quarto** (only needed to render `user_guide.qmd`) — [Download Quarto](https://quarto.org/docs/get-started/)

### 2. Clone the repo

```bash
git clone https://github.com/Leoanish/geosampler.git
cd geosampler
```

### 3. Install R packages

Open R or RStudio and run:

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "tibble", "sf", "leaflet",
  "leaflet.extras", "mapedit", "shinyjs", "httr", "jsonlite",
  "promises", "future", "later", "parallelly", "progress",
  "raster", "terra", "elevatr", "zip", "shinycssloaders",
  "clhs", "viridis", "DT", "rstac", "ggplot2", "lubridate"
))
```

For Planet imagery support, install `planetR` from GitHub:

```r
# install.packages("remotes")
remotes::install_github("bevingtona/planetR")
```

Run the dependency checker to confirm everything is in place:

```r
source("check_dependencies.R")
# Expected: "All 27 packages are installed and app.R parses OK."
```

### 4. Launch the app

```r
shiny::runApp("app.R")
```

The dashboard opens in your browser. Follow the **workflow tree** on the Welcome tab from left to right.

---

## Dashboard Overview

| Tab | Purpose |
|---|---|
| **Dashboard** | Welcome screen, workflow tree, quick-start guide |
| **Boundary** | Draw or upload one AOI polygon; export GeoJSON |
| **Variables & derivatives** | Load imagery, elevation, and other raster predictors |
| **Sampling** | Compare techniques, generate GPS points, review distributions |
| **App vs prior** | Upload historical field points; compare against new design |
| **Cost** | Estimate cost savings vs. a prior uniform/grid design |
| **Report** | Preview and download one-page PDF field plan |

For a detailed step-by-step walkthrough of every control and expected output, see **[user_guide.qmd](user_guide.qmd)** (render with `quarto render user_guide.qmd`).

---

## Deploying to shinyapps.io

1. Create a free account at [shinyapps.io](https://www.shinyapps.io/)
2. In RStudio, install `rsconnect`:
   ```r
   install.packages("rsconnect")
   ```
3. Authorise your account (copy the token from the shinyapps.io dashboard):
   ```r
   rsconnect::setAccountInfo(name = "<account>", token = "<token>", secret = "<secret>")
   ```
4. Deploy:
   ```r
   rsconnect::deployApp(
     appDir = ".",
     appName = "geosampler",
     account = "<account>"
   )
   ```
5. The live URL will be `https://<account>.shinyapps.io/geosampler/`

> **Note:** The free tier has 25 active-hours/month. The app is configured to use sequential futures and lower memory limits automatically when it detects a hosted environment.

---

## Supported File Formats

| Input | Accepted formats |
|---|---|
| Boundary / vector | `.geojson`, `.json`, `.kml`, `.kmz`, `.zip` (shapefile bundle), `.gpkg` |
| Raster stack | `.tif` / `.tiff` (single file multiband or individual bands) |
| Elevation (DEM) | `.tif` / `.tiff` |
| Historical points | `.geojson`, `.kml`, `.kmz`, `.zip`, `.gpkg` |

GeoJSON is the **preferred format** for all vector inputs — it avoids sidecar-file headaches.

---

## Sampling Methods

| Method | Best for |
|---|---|
| Simple Random | Fast pilots; uniform spatial distribution |
| Systematic Spread | Even geographic coverage |
| Spread + cLHS | Spatial spread combined with covariate balance |
| cLHS | Covariate-representative design (Minasny & McBratney 2006) |
| Zone-based | k-means strata from selected predictors |
| Hybrid Zonal cLHS | Zone stratification + within-zone cLHS |

Use **Technique comparison** → **WSS Curve** → **Recommend Zones** to choose the right method and sample size before committing to a design.

---

## Workflow Functions (Scripting API)

`sentinel_workflow_functions.qmd` translates the Shiny reactive logic into plain R functions with explicit arguments and return values — useful if you want to:

- Run the same analysis from a script or R Markdown document
- Build a future R package (`R/*.R`) from the same function signatures
- Reproduce a specific sampling run without the UI

Render with:

```bash
quarto render sentinel_workflow_functions.qmd
```

---

## Contributing

Pull requests are welcome. For major changes please open an issue first.

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/my-feature`)
3. Commit your changes (`git commit -m "add: my feature"`)
4. Push the branch (`git push origin feature/my-feature`)
5. Open a Pull Request

---

## Citation

If you use GeoSampler in your research, please cite:

```
Bhattarai, A. (2025). GeoSampler: A Sentinel-2 Shiny Dashboard for
Statistically Defensible Field Sampling. GitHub.
https://github.com/Leoanish/geosampler
```

---

## License

MIT © Anish Bhattarai — see [LICENSE](LICENSE) for details.
