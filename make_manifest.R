# Rigenera manifest.json per Posit Connect Cloud.
# Eseguire dalla cartella DfaR.

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect", repos = "https://cloud.r-project.org")
}

rsconnect::writeManifest(
  appDir = getwd(),
  appFiles = c("app.R", "README.md"),
  appMode = "shiny"
)

message("manifest.json rigenerato.")
