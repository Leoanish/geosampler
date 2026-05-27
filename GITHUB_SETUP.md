# GitHub Repository Setup Guide

A step-by-step checklist to get GeoSampler published as a public GitHub repository.

---

## Phase 1 — Prepare the local folder

### 1.1 Things to do before the first commit

Work through this checklist before you `git init`:

- [ ] **Remove or add to `.gitignore` any sensitive files** — API keys, Planet credentials, or any `.Renviron` file with secrets should never be committed. The `.gitignore` already excludes `.Renviron` and `rsconnect/`.
- [ ] **Decide on a LICENSE** — for academic open-source use MIT is standard. Create a `LICENSE` file (GitHub can generate one for you during repo creation).
- [ ] **Replace placeholder URLs in README.md** — search for `<your-username>` in `README.md` and `user_guide.qmd` and substitute your actual GitHub username.
- [ ] **Confirm no large data files exist in the folder** — rasters (`.tif`) and large vector files should live outside the repo or in a separate data release. The `.gitignore` already excludes `*.tif` and `*.tiff`.

---

## Phase 2 — Create the GitHub repository

1. Go to <https://github.com/new>
2. Fill in:
   - **Repository name**: `geosampler` (or `sentinel2-geosampler`)
   - **Description**: "A Shiny dashboard for Sentinel-2 guided field sampling — boundary definition, imagery retrieval, cLHS point generation, and PDF reporting."
   - **Visibility**: Public
   - **Initialize this repository with**: do NOT check "Add a README" (you already have one)
   - **Add .gitignore**: None (you already have one)
   - **Add a license**: MIT License ← choose this here
3. Click **Create repository**
4. Copy the repository URL shown (e.g., `https://github.com/anishbhattarai/geosampler.git`)

---

## Phase 3 — Initialise Git and push from your computer

Open a **Terminal** (Mac: Applications → Utilities → Terminal), then run each command block in order.

### 3.1 Navigate to the project folder

```bash
cd "/Users/anishbhattarai/Library/CloudStorage/OneDrive-UniversityofGeorgia/01_Leo Lab/01_Research/14_sampling_dashboard_sentinel/sentinel2_trial"
```

### 3.2 Initialise git

```bash
git init
git branch -M main
```

### 3.3 Stage all files (respects .gitignore)

```bash
git add .
```

Check what will be committed — make sure no secrets or huge files appear:

```bash
git status
```

### 3.4 First commit

```bash
git commit -m "init: GeoSampler Shiny dashboard — app, guides, and CSS/JS"
```

### 3.5 Connect to GitHub and push

Replace the URL below with the one you copied in Phase 2:

```bash
git remote add origin https://github.com/<your-username>/geosampler.git
git push -u origin main
```

GitHub will prompt for your username and password. Use your GitHub username and a **Personal Access Token** (not your account password):

- Generate a token at <https://github.com/settings/tokens> → **Generate new token (classic)** → check `repo` scope → copy and use as password.

---

## Phase 4 — Verify on GitHub

1. Open `https://github.com/<your-username>/geosampler` in your browser.
2. Confirm the **README.md** renders on the front page with the live demo badge.
3. Confirm **user_guide.qmd** is visible in the file list.
4. Confirm the `rsconnect/` folder is **not** present (the `.gitignore` should have excluded it).

---

## Phase 5 — What still needs to be done in the app

Before calling the dashboard "complete" for publication, work through this checklist:

### App functionality
- [ ] **Planet API key handling** — currently users paste the key directly into the UI. Consider adding a note in the README warning that keys entered in a public shinyapps.io instance may be visible in browser dev tools. Recommend users run locally for Planet data.
- [ ] **Error handling review** — test edge cases: very small AOI (< 1 ha), AOI with no Sentinel-2 coverage, uploading a point layer as a boundary.
- [ ] **Mobile / narrow viewport** — test the sidebar layout on a tablet-width screen.

### Documentation
- [ ] **Replace `<your-username>` placeholders** in `README.md` and `user_guide.qmd` with your actual GitHub username.
- [ ] **Add a `LICENSE` file** — GitHub can auto-generate MIT during repo creation, but confirm it is present after pushing.
- [ ] **Render `user_guide.qmd` to HTML** and optionally publish it to GitHub Pages or Quarto Pub so users can read it without installing Quarto.

  ```bash
  quarto render user_guide.qmd
  # Outputs user_guide.html — open in browser to review
  ```

- [ ] **Screenshot / GIF in README** — a screenshot of the dashboard increases engagement on GitHub. Add one under the Live Demo section:

  ```markdown
  ![GeoSampler dashboard screenshot](docs/screenshot.png)
  ```

### Shinyapps.io
- [ ] **Set the app title** in the shinyapps.io dashboard to "GeoSampler" (currently `sentinel2_trial`).
- [ ] **Monitor usage** — the free tier allows 25 active-hours/month. If usage is high, consider upgrading or directing users to run locally.

---

## Phase 6 — Keeping the repo up to date

After each session of changes:

```bash
# From the project folder:
git add .
git commit -m "fix: short description of what changed"
git push
```

For re-deploying to shinyapps.io after code changes:

```r
rsconnect::deployApp(appDir = ".", appName = "sentinel2_trial", account = "recuga")
```

---

## Summary checklist

| Step | Done? |
|---|---|
| `.gitignore` reviewed — no secrets or large rasters | ☐ |
| `<your-username>` replaced in README and guide | ☐ |
| GitHub repo created (public, MIT license) | ☐ |
| `git init && git push` completed | ☐ |
| README renders with live demo badge on GitHub | ☐ |
| `rsconnect/` folder absent from GitHub | ☐ |
| `user_guide.qmd` rendered and reviewed | ☐ |
| Screenshot added to README | ☐ |
| Planet API key warning documented | ☐ |
