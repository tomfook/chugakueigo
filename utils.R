# ===========================
# UTILITY FUNCTIONS
# Common helper functions shared across all modules
# Function Prefix: utils_*
# This is NOT a module - it's infrastructure that can be sourced by any module
# ===========================

utils_safe_execute <- function(operation, success_message = "", error_message_prefix = "", fallback_data = NULL, use_result_as_message = FALSE) {
  tryCatch({
    result <- operation()

    if (use_result_as_message && is.character(result) && length(result) == 1) {
      message <- result
      data <- NULL
    } else {
      message <- success_message
      data <- result
    }

    return(list(success = TRUE, data = result, message = message))
  }, error = function(e) {
    return(list(success = FALSE, data = fallback_data, message = paste(error_message_prefix, e$message)))
  })
}

utils_detect_environment <- function() {
  utils_safe_execute(
    operation = function() {
      shinyapps_indicators <- c(
	"SHINY_PORT",
	"RSTUDIO_CONNECT",
	"SHINYAPPS_USER"
      )

      is_shinyapps <- any(sapply(shinyapps_indicators, function(var) {Sys.getenv(var, "") != ""}))

      environment_info <- list(
	platform = if (is_shinyapps) "shinyapps.io" else "local",
	is_production = is_shinyapps,
	auth_method = if (is_shinyapps) "environment_variable" else "file_based"
      )

      return(environment_info)
    },
    success_message = "Environment detection completed",
    error_message_prefix = "Environment detection failed:"
  )
}
