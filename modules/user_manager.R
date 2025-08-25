validate_username <- function(username) {
  if (is.null(username) || username == "" || trimws(username) == "") {
    return(list(valid = FALSE, message = "Username cannot be empty."))
  }
  if (nchar(username) > 50) {
    return(list(valid = FALSE, message = "Username too long (max 50 characters)."))
  }
  if (grepl("[^a-zA-Z0-9_-]", username)) {
    return(list(valid = FALSE, message = "Username can only contain letters, numbers, underscore, and hyphen."))
  }
  if (username == "guest") {
    return(list(valid = FALSE, message = "Username 'guest' is reserved."))
  }
  return(list(valid = TRUE, message = ""))
}

create_user_scores <- function(username, current_scores, question_count){
  if (username %in% names(current_scores)) {
    return(list(success = FALSE, message = "Your name has been already registered."))
  }

  updated_scores <- current_scores
  updated_scores[[username]] <- rep(0L, question_count)

  return(list(
    success = TRUE,
    updated_scores = updated_scores,
    updated_namelist = names(updated_scores),
    message = paste("User", username, "was added.")
  ))
}

remove_user_from_scores <- function(username, current_scores){
  if (username == "guest") {
      return(list(success = FALSE, message = "Cannot remove guest user."))
  }

  if (!(username %in% names(current_scores))) {
    return(list(success = FALSE, message = "User not found."))
  }

  updated_scores <- current_scores
  updated_scores[[username]] <- NULL

  return(list(
    success = TRUE,
    updated_scores = updated_scores,
    updated_namelist = names(updated_scores),
    message = paste("User", username, "was permanently removed.")
  ))
}

