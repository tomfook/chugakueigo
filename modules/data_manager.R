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
data_read_score <- function(qa_count) {
  tryCatch({
    # Google Sheetsから読み込み
    score <- read_sheet(DATA$SHEETS$SCORES, col_types = "c")

    numeric_cols <- names(score)
    for(col in numeric_cols) {
      score[[col]] <- as.integer(score[[col]])
    }

    if (qa_count > nrow(score)) {
      missing_rows <- qa_count - nrow(score)
      padding_data <- list()
      for (col_name in names(score)) {
        padding_data[[col_name]] <- rep(0L, missing_rows)
      }
      padding_rows <- data.frame(padding_data)
      score <- bind_rows(score, padding_rows)
    }

    return(list(success = TRUE, data = score, message = "Score data loaded successfully"))
    }, error = function(e) {
      return(list(success = FALSE, data = NULL, message = paste("Error reading from Google Sheets:", e$message)))
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

    score_result <- data_read_score(qa_count = qa_count)
    if (!score_result$success) {
      return(list(success = FALSE, data = NULL, message = paste("Failed to initialize:", score_result$message)))
    }

    users_meta_result <- data_read_users_meta()
    if (!users_meta_result$success) {
      return(list(success = FALSE, data = NULL, message = paste("Failed to initialize users_meta:", users_meta_result$message)))
    }

    score_global <- score_result$data %>%
      mutate(guest = 0L) %>%
      select(guest, everything())

    if (qa_count != nrow(score_global)) {
      warning("Question and score data length mismatch. This has been automatically corrected.")
    }

    user_names <- users_meta_result$data$username

    return(list(
      success = TRUE,
      data = list(
	qa_data = qa_data,
	score_global = score_global,
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
    score_result <- data_read_score(qa_count = qa_count)
    if (!score_result$success) {
      return(list(success = FALSE, data = NULL, message = score_result$message))
    }
    existing_scores <- score_result$data
    existing_scores[[username]] <- user_scores
    sheet_write(existing_scores, ss = DATA$SHEETS$SCORES, sheet = "Sheet1")

    user_sheet_result <- data_write_user_score(username, user_scores)
    if (!user_sheet_result$success) {
      warning(paste("Failed to write to user worksheet:", user_sheet_result$message))
    }

    updated_scores <- existing_scores %>%
      mutate(guest = 0L) %>%
      select(guest, everything())

    return(list(
      success = TRUE,
      data = updated_scores,
      message = paste("Score saved to Google Sheets for user:", username)
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
