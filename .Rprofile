# .Rprofile for ChugakuEigo Shiny App
# This file configures the R environment for both local development and shinyapps.io deployment

# Set CRAN mirror for consistent package installation
local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cran.rstudio.com/"
  options(repos = r)
})

# Configure googlesheets4 for non-interactive authentication
options(
  gargle_oauth_cache = FALSE,
  gargle_oauth_email = FALSE,
  gargle_oob_default = TRUE
)

# Suppress startup messages in production
if (Sys.getenv("SHINY_PORT") != "") {
  options(
    shiny.maxRequestSize = 30*1024^2, # 30MB upload limit
    warn = 1 # Show warnings immediately
  )
}

# Load required packages early to ensure proper initialization order
if (interactive()) {
  cat("Loading ChugakuEigo development environment...\n")
}

credentials_file <- "credentials/service-account-key.json"
if (file.exists(credentials_file)) {
  json_content <- paste(readLines(credentials_file), collapse = "")
  Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS_JSON = json_content)
  cat("Credentials loaded from file into environment variable\n")
}
