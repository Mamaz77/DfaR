# DfaR

**DfaR** is a Shiny application for dental fluctuating asymmetry (FA),
odontometric size and LEH analyses.

## Online deployment with Posit Connect Cloud

This repository is structured for deployment as a Shiny for R application.

### Files

- `app.R` — main Shiny application
- `manifest.json` — R/dependency manifest for Posit Connect Cloud
- `make_manifest.R` — optional script to regenerate `manifest.json` locally
- `.rscignore` — files excluded from deployment
- `.gitignore` — standard Git exclusions

### Publish

1. Create a public GitHub repository, e.g. `DfaR`.
2. Upload the files from this folder to the root of the repository.
3. Sign in to Posit Connect Cloud.
4. Click **Publish**.
5. Choose **Shiny**.
6. Select the GitHub repository.
7. Select `app.R` as the primary file.
8. Click **Publish**.

The app does not store uploaded Excel files permanently. Each uploaded file is
processed within the running Shiny session.

## Regenerating the manifest

If `app.R` or its R package dependencies are changed, regenerate the manifest
locally in R/RStudio:

```r
install.packages("rsconnect")
source("make_manifest.R")
```

Then commit the new `manifest.json` to GitHub.

## Main R packages

- shiny
- readxl
- dplyr
- tidyr
- tibble
- openxlsx
- DT
- ggplot2
- scales

## Output

The downloaded workbook includes the FA audit tables and an
`Individual_Summary` sheet containing:

`ID`, `Sex`, `AgeClass`, `Funerary`, `LEH_Present_Individual`,
`LEH_MaxBands`, `Composite_FA`, `N_FA_traits`,
`Composite_FA_ge2`, `Composite_FA_complete`.
