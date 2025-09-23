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
      json_content <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS_JSON")
      if (json_content == "") {
	stop("GOOGLE_APPLICATION_CREDENTIALS_JSON environment variable not set")
      }

      temp_file <- tempfile(fileext = ".json")
      writeLines(json_content, temp_file)
      on.exit(unlink(temp_file), add = TRUE)
      gs4_auth(path = temp_file)

      return("Environment variable authentication successful")
    },
    success_message = "",
    error_message_prefix = "Authentication failed:",
    use_result_as_message = TRUE
  )
}

# ===========================
# Network Error Retry Functions
# ===========================

storage_safe_write_with_retry <- function(data, ss, sheet, max_retries = 3, base_delay = 1) {
  for (attempt in 1:max_retries) {
    tryCatch({
      sheet_write(data, ss = ss, sheet = sheet)
      return(list(
	success = TRUE,
	message = paste("Write successful on attempt", attempt),
	retry_count = attempt
      ))
    }, error = function(e) {
      error_msg <- toString(e$message)
      is_retryable <- grepl("timeout|429|5[0-9][0-9]|network|connection|quota", error_msg, ignore.case = TRUE)

      if(!is_retryable || attempt == max_retries) {
	return(list(
	  success = FALSE,
	  message = paste("Write failed after", attempt, "attempts:", error_msg),
	  retry_count = attempt
	))
      }

      delay <- base_delay * (2 ^ (attempt -1))
      Sys.sleep(delay)
    })
  }
}

storage_safe_append_with_retry <- function(data, ss, sheet, max_retries = 3, base_delay = 1) {
  for (attempt in 1:max_retries) {
    tryCatch({
      sheet_append(ss, data, sheet = sheet)
      return(list(
	success = TRUE,
	message = paste("Append successful on attempt", attempt),
	retry_count = attempt
      ))
    }, error = function(e) {
      error_msg <- toString(e$message)
      is_retryable <- grepl("timeout|429|5[0-9][0-9]|network|connection|quota", error_msg, ignore.case = TRUE)
      if(!is_retryable || attempt == max_retries) {
	return(list(
	  success = FALSE,
	  message = paste("Append failed after", attempt, "attempts:", error_msg),
	  retry_count = attempt
	))
      }

      delay <- base_delay * (2 ^ (attempt -1))
      Sys.sleep(delay)
    })
  }
}
