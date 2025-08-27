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
  if (username == DEFAULTS$USER) {
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

user_create_scores <- function(username, current_scores, question_count){
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
user_add_new <- function(username, qa_state, main_data) {
  validation <- user_validate_username(username)
  if (!validation$valid) {
    return(list(success = FALSE, message = validation$message))
  }

  result <- user_create_scores(username, qa_state$score.all, nrow(main_data))
  if (!result$success) {
    return(result)
  }

  qa_state$score.all <- result$updated_scores
  qa_state$namelist <- result$updated_namelist

  return(list(success = TRUE, message = result$message))
}

user_remove_from_scores <- function(username, current_scores){
  if (username == DEFAULTS$USER) {
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
user_remove <- function(username, qa_state) {
  result <- user_remove_from_scores(username, qa_state$score.all)
  if (!result$success) {
    return(result)
  }

  tryCatch({
    write.table(result$updated_scores, PATHS$SCORES, row.names = FALSE, sep = ",")

    qa_state$score.all <- result$updated_scores
    qa_state$namelist <- result$updated_namelist

    return(list(success = TRUE, message = result$message))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error removing user:", e$message)))
  })
}

user_switch_reset_state <- function(qa_state, new_username) {
  qa_state$trial <- 0L
  qa_state$ok <- 0L
  qa_state$start <- FALSE
  qa_state$user <- new_username
  qa_state$score <- qa_state$score.all[[new_username]]
  qa_state$index <- NULL
  qa_state$question <- ""
  qa_state$answer <- ""

  return(qa_state)
}

