# ===========================
# DATA STORAGE SUBMODULE
# **Private Submodule of data_manager.R**
# Concrete implementation of data persistence (currently: Google Sheets)
# Function prefix: storage_*
# ===========================

library(googlesheets4)
source("constants.R")
source("utils.R")

# ===========================
# Authentication & Initialization
# ===========================

storage_setup_authentication <- function() {
  utils_safe_execute(
    operation = function() {
      env_result <- utils_detect_environment()
      if (!env_result$success) {
	stop("Failed to detect environment")
      }
      env_info <- env_result$data

      if (env_info$auth_method == "environment_variable") {
	json_content <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS_JSON")
	if (json_content == "" || json_content == "NA") {
	  stop("Environment variable GOOGLE_APPLICATION_CREDENTIALS_JSON not found in production environment")
	}
	temp_file <- tempfile(fileext = ".json")
	writeLines(json_content, temp_file)
	on.exit(unlink(temp_file), add = TRUE)
	gs4_auth(path = temp_file)

	return(paste("Environment variable authentication successful on", env_info$platform))
      } else {
	if (!file.exists(DATA$PATHS$SERVICE_ACCOUNT_KEY)) {
	  stop("Service account file not found in development environment")
	}
	gs4_auth(path = DATA$PATHS$SERVICE_ACCOUNT_KEY)
	return(paste("File-based authentication successful on", env_info$platform))
      }
    },
    success_message = "",
    error_message_prefix = "Authentication failed:",
    use_result_as_message = TRUE
  )
}
