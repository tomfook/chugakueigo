# ===========================
# UTILITY FUNCTIONS
# Common helper functions shared across all modules
# This is NOT a module - it's infrastructure that can be sourced by any module
# ===========================

# ===========================
# Standardized Error Handling
# ===========================

utils_safe_execute <- function(operation, success_message = "", error_message_prefix = "", fallback_data = NULL, user_result_as_message = FALSE) {
  tryCatch({
    result <- operation()

    if (user_result_as_message && is.character(result) && length(result) == 1) {
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
