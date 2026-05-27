<div align="center">

# 🛰️ GeoSampler

### A Sentinel-2 Shiny Dashboard for Statistically Defensible Field Sampling

*From satellite imagery to export-ready GPS sample points — entirely in your browser.*

[![Live Demo](https://img.shields.io/badge/🚀%20Live%20Demo-shinyapps.io-2ea44f?style=for-the-badge)](https://recuga.shinyapps.io/sentinel2_trial/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg?style=for-the-badge)](LICENSE)
[![R](https://img.shields.io/badge/R-%3E%3D4.1-276DC3?style=for-the-badge&logo=r&logoColor=white)](https://cran.r-project.org/)
[![GitHub](https://img.shields.io/badge/GitHub-Leoanish%2Fgeosampler-181717?style=for-the-badge&logo=github)](https://github.com/Leoanish/geosampler)

</div>

---

## 🌍 What is GeoSampler?

GeoSampler is an interactive **R Shiny dashboard** that helps agronomists, ecologists, and field scientists design optimal soil or vegetation sampling plans using freely available satellite data.

Instead of guessing where to sample in a field, GeoSampler uses **Sentinel-2 multispectral imagery**, **digital elevation models**, and **statistical sampling algorithms** (including conditioned Latin Hypercube Sampling — cLHS) to ensure your sample points capture the full range of environmental variability in your study area.

> **No GIS software, no coding, no satellite data subscriptions required** to get started — just open the live demo link above.

---

## 🔁 End-to-End Workflow

```mermaid
flowchart LR
    A([🗺️ Define\nBoundary]) --> B([🛰️ Load\nImagery])
    B --> C([⛰️ Load\nElevation])
    C --> D([📦 Add Other\nLayers])
    D --> E([📊 Compare\nTechniques])
    E --> F([📍 Generate\nSample Points])
    F --> G([🔍 App vs\nPrior])
    G --> H([💰 Cost\nComparison])
    H --> I([📄 Download\nPDF Report])

    style A fill:#4CAF50,color:#fff,stroke:#388E3C
    style B fill:#2196F3,color:#fff,stroke:#1565C0
    style C fill:#FF9800,color:#fff,stroke:#E65100
    style D fill:#9C27B0,color:#fff,stroke:#6A1B9A
    style E fill:#F44336,color:#fff,stroke:#B71C1C
    style F fill:#009688,color:#fff,stroke:#00695C
    style G fill:#607D8B,color:#fff,stroke:#37474F
    style H fill:#795548,color:#fff,stroke:#4E342E
    style I fill:#455A64,color:#fff,stroke:#263238
```

---

## ✨ Key Features

| Feature | Description |
|---|---|
| 🗺️ **AOI Definition** | Draw a polygon directly on the map or upload GeoJSON / KML / Shapefile |
| 🛰️ **Sentinel-2 Retrieval** | Searches Microsoft Planetary Computer STAC — no account needed |
| 🌱 **Vegetation Indices** | NDVI, NDRE, GNDVI, EVI, SAVI auto-computed from retrieved imagery |
| ⛰️ **Terrain Derivatives** | DEM retrieval + slope, aspect, TWI, TPI computed in one click |
| 📊 **Technique Comparison** | Benchmark 6 methods across multiple sample sizes with a WSS elbow curve |
| 📍 **Smart Sampling** | cLHS, Zone-based, Hybrid Zonal cLHS, Simple Random, Systematic Spread |
| 🔍 **Historical Comparison** | Upload prior field GPS points and compare covariate distributions |
| 💰 **Cost Calculator** | Estimate savings vs. prior uniform / grid designs |
| 📄 **PDF Field Plan** | One-click downloadable report for field crews |
| 📥 **GeoJSON Export** | Export boundary and sample points for use in any GIS or GPS device |

---

## 🖥️ Dashboard Tabs — Step by Step

<details>
<summary><b>Tab 1 — Dashboard (Welcome)</b> &nbsp;🏠</summary>

The landing page contains a **clickable workflow tree** that mirrors the recommended tab order. It also includes:
- A **Quick Start** card (6-step summary)
- A **Sampling Methods** reference card
- A **Detailed Operating Guide** with tips on AOI size, cloud thresholds, and layer QA

Start here, read the overview, then follow the workflow tree left to right.

</details>

<details>
<summary><b>Tab 2 — Boundary</b> &nbsp;🗺️</summary>

Define your **Area of Interest (AOI)**. All subsequent tabs use this boundary.

**Two methods:**

**Option A — Draw on the map**
1. Select *"I want to digitize"* from the dropdown
2. Click **Use My Location** or enter lat/lon and click **Set Location**
3. Use the polygon tool on the map to draw your AOI
4. Click ✓ **Finish** → **Done** to save

**Option B — Upload a file**
1. Select *"I have my own boundary layer"*
2. Upload a `.geojson`, `.kml`, `.kmz`, `.zip` (shapefile), or `.gpkg` file
3. The boundary loads onto the map automatically

After loading, click **Download GeoJSON of Boundary** to save a copy, then **Next: Variables & derivatives** to continue.

> ⚠️ Only one AOI is active at a time. Clear and redraw if you need to replace it.

</details>

<details>
<summary><b>Tab 3 — Variables & Derivatives</b> &nbsp;🛰️</summary>

Four sub-tabs build up your covariate stack for sampling.

---

### 3.1 Imagery

Choose your imagery source:

| Source | When to use |
|---|---|
| **Sentinel-2 (retrieve)** | Free, no account — searches Planetary Computer STAC |
| **Planet (retrieve)** | Higher resolution; requires a Planet API key |
| **Upload raster stack** | You already have a multiband GeoTIFF |
| **Upload individual bands** | Separate NIR / Red / RedEdge / Blue / Green TIFFs |
| **Upload VI TIFF** | Pre-computed vegetation index raster |

**For Sentinel-2:**
1. Set a date range (broader = more scenes = better cloud-free composite)
2. Set maximum cloud cover (10% default)
3. Click **Search** → inspect the NDRE timeseries chart
4. Click **Retrieve** to download and build the median composite
5. A false-colour preview renders on the map

---

### 3.2 Elevation

1. Choose **Retrieve elevation** (uses AWS Terrain Tiles, free) or **Upload DEM GeoTIFF**
2. Click **Retrieve Elevation Data** — takes 10–60 s depending on AOI size
3. Check boxes for terrain derivatives you want: **Slope, Aspect, TWI, TPI**
4. Click **Compute derivatives** — each is added as a separate covariate

---

### 3.3 Other Layers

Upload any additional GeoTIFF predictors (soil EC, yield maps, clay content, etc.):
1. Click the file picker → select your `.tif`
2. Click **Add Layer** — appears on the map and enters the covariate pool
3. Repeat for as many layers as needed

---

### 3.4 Variable Summary

Click **Compute / refresh summary** to see a QA table of min / max / mean / median for every loaded layer inside the AOI. Fix any layers with anomalous values before proceeding to Sampling.

</details>

<details>
<summary><b>Tab 4 — Sampling</b> &nbsp;📍</summary>

Three sub-tabs guide you from method selection to final GPS points.

---

### 4.1 Technique Comparison *(recommended first step)*

Benchmarks **6 sampling strategies** across multiple sample sizes using your covariate stack:

| Method | Best for |
|---|---|
| Simple Random | Fast pilots; uniform spatial distribution |
| Systematic Spread | Even geographic coverage |
| Spread + cLHS | Spatial spread + covariate balance |
| **cLHS** | Covariate-representative design *(Minasny & McBratney 2006)* |
| Zone-based | k-means strata from your predictors |
| Hybrid Zonal cLHS | Zone stratification + within-zone cLHS |

**How to run:**
1. Select covariates (use a **Covariate preset** for quick selection: Lean / Scout / Fertility / Full)
2. Click **Recommend Zones** — runs a k-means WSS elbow analysis and suggests optimal k
3. Set **Repeats per method** (3–5 for stable results)
4. Click **Compare Sampling Techniques**
5. Read the benchmark chart — the highlighted point shows the recommended method + sample size

---

### 4.2 Generate Sample Points

1. Choose a **sampling method** from the dropdown
2. Set **Number of Sample Points** (use the value recommended by Technique Comparison)
3. Set **Negative Buffer** (e.g., 10 m to avoid field edges)
4. Set a **Random Seed** for reproducibility
5. Click **Generate** — points appear on the map
6. Click **Download GeoJSON of sample points** to export

---

### 4.3 Population vs Sample

Violin / density plots automatically show whether your sample points cover the full covariate range of the field. If the sample distribution misses parts of the field distribution, increase sample size or switch to cLHS.

</details>

<details>
<summary><b>Tab 5 — App vs Prior</b> &nbsp;🔍</summary>

Upload **historical GPS points** from a previous field season and compare them against the new app-generated design.

1. Upload your prior points file (same vector formats as boundary)
2. **Map tab** — shows old points (hollow) and new points (filled) together
3. **Distributions tab** → click **Compute distributions** — dual-violin plots for each covariate show where the two designs differ in covariate space

Use this to justify your new design: *"the app-generated points cover TWI ranges not represented in the prior grid."*

</details>

<details>
<summary><b>Tab 6 — Cost Comparison</b> &nbsp;💰</summary>

Estimates total sampling cost for your prior design vs. the app-recommended design.

1. Enter **prior sample count** (e.g., 40 grid points)
2. Enter **app sample count** (pre-filled from Technique Comparison)
3. Enter **cost per sample** (lab fees + travel + time)
4. Choose **currency** (USD, EUR, GBP, CAD, AUD, or custom)
5. A bar chart and **% savings summary** update automatically
6. These numbers flow directly into the PDF report

</details>

<details>
<summary><b>Tab 7 — Report (PDF)</b> &nbsp;📄</summary>

Generates a one-page PDF field plan containing:
- AOI area, selected method, sample count, and date
- Map with sample points overlaid
- Sample density distribution
- Population vs sample violin plots
- Historical vs app comparison (if prior points uploaded)
- Cost comparison chart
- Technique comparison chart

Click **Download PDF report** to save. For the full coordinate table, export GeoJSON from the Sampling tab.

</details>

---

## 🚀 Run Locally

### Prerequisites

- **R ≥ 4.1** → [Download R](https://cran.r-project.org/)
- **RStudio** (recommended) → [Download RStudio](https://posit.co/downloads/)

### 1 — Clone the repo

```bash
git clone https://github.com/Leoanish/geosampler.git
cd geosampler
```

### 2 — Install packages

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "tibble", "sf", "leaflet",
  "leaflet.extras", "mapedit", "shinyjs", "httr", "jsonlite",
  "promises", "future", "later", "parallelly", "progress",
  "raster", "terra", "elevatr", "zip", "shinycssloaders",
  "clhs", "viridis", "DT", "rstac", "ggplot2", "lubridate"
))

# For Planet imagery support:
remotes::install_github("bevingtona/planetR")
```

### 3 — Check dependencies

```r
source("check_dependencies.R")
# Expected: "All 27 packages are installed and app.R parses OK."
```

### 4 — Launch

```r
shiny::runApp("app.R")
```

---

## 📂 Repository Structure

```
geosampler/
├── app.R                           # Main Shiny application (~15,000 lines)
├── check_dependencies.R            # Dependency checker — run before first launch
├── sentinel_workflow_functions.qmd # Standalone R function API (scripting / future package)
├── user_guide.qmd                  # Full step-by-step user guide (render with Quarto)
├── www/
│   ├── geo_sampler.css             # Dashboard stylesheet
│   └── geo_sampler.js              # Client-side helpers
└── .gitignore
```

---

## ☁️ Deploy to shinyapps.io

```r
install.packages("rsconnect")

rsconnect::setAccountInfo(
  name   = "<your-account>",
  token  = "<your-token>",
  secret = "<your-secret>"
)

rsconnect::deployApp(appDir = ".", appName = "geosampler")
```

> The app auto-detects hosted environments and switches to memory-safe sequential processing. Free tier: 25 active-hours/month.

---

## 📁 Supported File Formats

| Input | Formats |
|---|---|
| Boundary / AOI | `.geojson` ✅ `.kml` `.kmz` `.zip` (shapefile) `.gpkg` |
| Raster stack | `.tif` `.tiff` (multiband or single-band) |
| Elevation (DEM) | `.tif` `.tiff` |
| Historical points | `.geojson` `.kml` `.kmz` `.zip` `.gpkg` |

> **GeoJSON is always the preferred format** — single file, no sidecars, human-readable.

---

## 🔧 Scripting API

`sentinel_workflow_functions.qmd` exposes the same logic as standalone R functions — useful for scripting, R Markdown reports, or building a future R package:

```r
# Example
aoi    <- read_boundary("my_field.geojson")
s2     <- retrieve_sentinel(aoi, start = "2024-04-01", end = "2024-09-30", cloud_pct = 10)
elev   <- retrieve_elevation(aoi)
pts    <- generate_sample_points(aoi, covariates = list(s2, elev), n = 20, method = "clhs")
```

Render the full function reference:
```bash
quarto render sentinel_workflow_functions.qmd
```

---

## 🤝 Contributing

Pull requests are welcome. For major changes please open an issue first.

```bash
git checkout -b feature/my-feature
git commit -m "add: my feature"
git push origin feature/my-feature
# → Open a Pull Request on GitHub
```

---

## 📖 Citation

If you use GeoSampler in your research, please cite:

```bibtex
@software{bhattarai2025geosampler,
  author  = {Bhattarai, Anish},
  title   = {GeoSampler: A Sentinel-2 Shiny Dashboard for Statistically Defensible Field Sampling},
  year    = {2025},
  url     = {https://github.com/Leoanish/geosampler}
}
```

---

## 📜 License

MIT © [Anish Bhattarai](https://github.com/Leoanish) — see [LICENSE](LICENSE) for details.

---

<div align="center">
  <sub>Built with ❤️ using R Shiny · Sentinel-2 data via Microsoft Planetary Computer · Sampling via cLHS</sub>
</div>
