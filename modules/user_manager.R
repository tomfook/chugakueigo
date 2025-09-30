# ================================
# USER MANAGER MODULE
# Handles user account management: validation, creation, deletion, and state switching
# ================================
source("constants.R")

user_validate_username <- function(username) {
  if (is.null(username) || username == "" || trimws(username) == "") {
    return(list(valid = FALSE, message = "Username cannot be empty."))
  }
  if (nchar(username) > 50) {
    return(list(valid = FALSE, message = "Username too long (max 50 characters)."))
  }
  if (grepl("[^a-zA-Z0-9_-]", username)) {
    return(list(valid = FALSE, message = "Username can only contain letters, numbers, underscore, and hyphen."))
  }
  if (username == APP$DEFAULTS$USER) {
    return(list(valid = FALSE, message = "Username 'guest' is reserved."))
  }
  return(list(valid = TRUE, message = ""))
}

user_validate_deletion <- function(selected_user, current_user) {
  if (is.null(selected_user) || selected_user == "No users to delete") {
    return(list(valid = FALSE, message = "No user selected for deletion.", type = "warning"))
  }

  if (selected_user == current_user) {
    return(list(valid = FALSE, message = "Cannot delete currently selected user. Please switch to another user first.", type = "warning"))
  }

  return(list(valid = TRUE, message = "", type = ""))
}

user_add_new <- function(user_state, username) {
  validation <- user_validate_username(username)
  if (!validation$valid) {
    return(list(success = FALSE, message = validation$message))
  }

  user_names <- user_state$user_names
  if (username %in% user_names) {
    return(list(success = FALSE, message = "Your name has been already registered."))
  }

  user_state$user_names <- c(user_state$user_names, username)

  return(list(success = TRUE, message = paste("User", username, "was added.")))
}

user_remove <- function(username, user_state) {
  user_names <- user_state$user_names

  if (username == APP$DEFAULTS$USER) {
    return(list(success = FALSE, message = "Cannot remove guest user."))
  }

  if (!(username %in% user_names)) {
    return(list(success = FALSE, message = "User not found."))
  }

  tryCatch({
    user_state$user_names <- user_names[user_names != username]
    return(list(success = TRUE, message = paste("User", username, "was permanently removed.")))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error removing user:", e$message)))
  })
}
