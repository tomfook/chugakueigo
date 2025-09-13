# =============================
# DATA MANAGER MODULE
# Handles data file operations: reading, writing, and initialization
# =============================

library(dplyr)
library(googlesheets4)
source("constants.R")
gs4_auth(path = DATA$PATHS$SERVICE_ACCOUNT_KEY)


# =============================
# Core File Operations
# =============================

data_read_user_score <- function(username, qa_count) {
  tryCatch({
    if (username == UI$DEFAULTS$USER) {
      user_scores <- rep(0L, qa_count)
      return(list(success = TRUE, data = user_scores, message = "Guest user scores initialized"))
    }

    sheet_name <- paste0("user_", username)
    existing_sheets <- sheet_names(DATA$SHEETS$SCORES)

    if (!(sheet_name %in% existing_sheets)) {
      user_scores <- rep(0L, qa_count)
      return(list(success = TRUE, data = user_scores, message = paste("User worksheet not found, initialized with zeros:", username)))
    }

    user_data <- read_sheet(DATA$SHEETS$SCORES, sheet = sheet_name, col_types = "ii")

    if (nrow(user_data) < qa_count) {
      missing_rows <- qa_count - nrow(user_data)
      additional_data <- data.frame(
	question_id = seq(nrow(user_data) + 1, qa_count),
	score = rep(0L, missing_rows)
      )
      user_data <- bind_rows(user_data, additional_data)
    } else if (nrow(user_data) > qa_count) {
      user_data <- user_data[1:qa_count, ]
    }

    user_scores <- user_data$score
    return(list(success = TRUE, data = user_scores, message = paste("User scores loaded successfully:", username)))
  }, error = function(e) {
    user_scores <- rep(0L, qa_count)
    return(list(success = FALSE, data = user_scores, message = paste("Error reading user scores, using zeros:", e$message)))
  })
}

data_read_users_meta <- function() {
  tryCatch({
    users_meta <- read_sheet(DATA$SHEETS$USERS_META, sheet = "users_meta", col_types = "cDDd")

    return(list(success = TRUE, data = users_meta, message = "Users metadata loaded successfully"))
  }, error = function(e) {
    return(list(success = FALSE, data = NULL, message = paste("Error reading users_meta from Google Sheets:", e$message)))
  })
}

# ==============================
# Application Initialization
# ==============================

data_initialize <- function() {
  tryCatch({
    qa_data <- read.csv(DATA$PATHS$QUESTIONS, comment = "#", stringsAsFactors = FALSE) %>%
      filter(question != "", answer != "")
    qa_count <- nrow(qa_data)

    if (qa_count == 0) {
      return(list(success = FALSE, data = NULL, message = "No valid questions found in qlist.csv"))
    }

    users_meta_result <- data_read_users_meta()
    if (!users_meta_result$success) {
      return(list(success = FALSE, data = NULL, message = paste("Failed to initialize users_meta:", users_meta_result$message)))
    }

    guest_score_result <- data_read_user_score(UI$DEFAULTS$USER, qa_count)
    if (!guest_score_result$success) {
      return(list(success = FALSE, data = NULL, message = paste("Failed to initialize guest score:", guest_score_result$message)))
    }

    user_names <- users_meta_result$data$username

    return(list(
      success = TRUE,
      data = list(
	qa_data = qa_data,
	user_score = guest_score_result$data,
	user_names = user_names
      ),
      message = "Data initialized successfully")
    )
  }, error = function(e) {
    return(list(success = FALSE, data = NULL, message = paste("Failed to initialize data:", e$message)))
  })
}

# ===============================
# Data Persistence Operations
# ===============================

data_save_user_score <- function(username, current_user, user_scores, qa_count) {
  if (username != current_user) {
    return(list(
      success = FALSE,
      data = NULL,
      message = "You switched user account."
    ))
  }

  tryCatch({
    user_sheet_result <- data_write_user_score(username, user_scores)
    if (!user_sheet_result$success) {
      return(list(success = FALSE, data = NULL, message = user_sheet_result$message))
    }

    return(list(
      success = TRUE,
      data = NULL,
      message = paste("Score saved to user worksheet for user:", username)
      ))
  }, error = function(e) {
    return(list(
      success = FALSE,
      data = NULL,
      message = paste("Error saving to Google Sheets:", e$message)
    ))
  })
}

# ===========================
# Users Meta Management
# ===========================

data_add_user_to_meta <- function(username) {
  tryCatch({
    users_meta_result <- data_read_users_meta()
    if (!users_meta_result$success) {
      return(users_meta_result)
    }

    current_meta <- users_meta_result$data

    if (username %in% current_meta$username) {
      return(list(success = FALSE, message = "User already exists in users_meta"))
    }

    new_row <- data.frame(
      username = username,
      created_date = Sys.time(),
      last_updated = Sys.time(),
      question_count = 0,
      stringsAsFactors = FALSE
    )

    updated_meta <- rbind(current_meta, new_row)

    sheet_write(updated_meta, ss = DATA$SHEETS$USERS_META, sheet = "users_meta")

    return(list(success = TRUE, message = paste("User", username, "added to users_meta")))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error adding user to users_meta:", e$message)))
  })
}

data_remove_user_from_meta <- function(username) {
  tryCatch({
    users_meta_result <- data_read_users_meta()
    if (!users_meta_result$success) {
      return(users_meta_result)
    }

    current_meta <- users_meta_result$data

    if (!(username %in% current_meta$username)) {
      return(list(success = FALSE, message = "User not found in users_meta"))
    }

    if (username == UI$DEFAULTS$USER) {
      return(list(success = FALSE, message = "Cannot remove guest user from users_meta"))
    }

    updated_meta <- current_meta[current_meta$username != username,]

    sheet_write(updated_meta, ss = DATA$SHEETS$USERS_META, sheet = "users_meta")

    return(list(success = TRUE, message = paste("User", username, "removed from users_meta")))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error removing user from users_meta:", e$message)))
  })
}


# ===========================
# User Worksheet management
# ===========================

data_ensure_user_worksheet <- function(username, qa_count) {
  tryCatch({
    sheet_name <- paste0("user_", username)
    existing_sheets <- sheet_names(DATA$SHEETS$SCORES)

    if (!(sheet_name %in% existing_sheets)) {
      sheet_add(DATA$SHEETS$SCORES, sheet = sheet_name)

      initial_data <- data.frame(
	question_id = seq_len(qa_count),
	score = rep(0L, qa_count),
	stringsAsFactors = FALSE
      )
      sheet_write(initial_data, ss = DATA$SHEETS$SCORES, sheet = sheet_name)
    }

    return(list(success = TRUE, message = paste("User worksheet ensured for:", username)))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error ensuring user worksheet:", e$message)))
  })
}

data_write_user_score <- function(username, user_scores) {
  tryCatch({
    sheet_name <- paste0("user_", username)

    ensure_result <- data_ensure_user_worksheet(username, length(user_scores))
    if (!ensure_result$success) {
      return(ensure_result)
    }

    user_data <- data.frame(
      question_id = seq_along(user_scores),
      score = as.integer(user_scores),
      stringsAsFactors = FALSE
    )

    sheet_write(user_data, ss = DATA$SHEETS$SCORES, sheet = sheet_name)

    return(list(success = TRUE, message = paste("User score written to worksheet:", sheet_name)))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error writing user score:", e$message)))
  })
}

data_delete_user_worksheet <- function(username) {
  tryCatch({
    sheet_name <- paste0("user_", username)
    existing_sheets <- sheet_names(DATA$SHEETS$SCORES)

    if (sheet_name %in% existing_sheets) {
      sheet_delete(DATA$SHEETS$SCORES, sheet = sheet_name)
      return(list(success = TRUE, message = paste("User worksheet deleted:", sheet_name)))
    } else {
      return(list(success = TRUE, message = paste("User worksheet not found (already deleted):", sheet_name)))
    }
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error deleting user worksheet:", e$message)))
  })
}
