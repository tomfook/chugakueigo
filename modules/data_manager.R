# =============================
# DATA MANAGER MODULE
# Handles data file operations: reading, writing, and initialization
# Function prefix: data_*
# =============================

library(dplyr)
library(googlesheets4)
source("constants.R")
source("utils.R")
source("modules/data_manager/data_storage.R")

auth_result <- storage_setup_authentication()
if (!auth_result$success) {
  stop(paste("Google Sheets authentication failed:", auth_result$message))
}

# =============================
# Core File Operations
# =============================

data_read_user_score <- function(username, qa_count) {
  utils_safe_execute(
    operation = function() {
      if (username == UI$DEFAULTS$USER) {
	return(rep(0L, qa_count))
      }
      sheet_name <- paste0("user_", username)
      existing_sheets <- sheet_names(DATA$SHEETS$SCORES)

      if (!(sheet_name %in% existing_sheets)) {
	return(rep(0L, qa_count))
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
      return(user_scores)
    },
    success_message = paste("User scores loaded successfully:", username),
    error_message_prefix = "Error reading user scores, using zeros:",
    fallback_data = rep(0L, qa_count)
  )
}

data_read_users_meta <- function() {
  utils_safe_execute(
    operation = function() read_sheet(DATA$SHEETS$USERS_META, sheet = "users_meta", col_types = "cDDd"),
    success_message = "Users metadata loaded successfully",
    error_message_prefix = "Error reading users_meta from Google Sheets:"
  )
}

# ==============================
# Application Initialization
# ==============================

data_initialize <- function() {
  utils_safe_execute(
    operation = function() {
      qa_data <- read.csv(DATA$PATHS$QUESTIONS, comment ="#", stringsAsFactors = FALSE) %>%
	filter(question != "", answer != "")
      qa_count <- nrow(qa_data)
      if (qa_count == 0){
	stop("No valid questions found in qlist.csv")
      }

      users_meta_result <- data_read_users_meta()
      if (!users_meta_result$success) {
	stop(paste("Failed to initialize users_meta:", users_meta_result$message))
      }

      guest_score_result <- data_read_user_score(UI$DEFAULTS$USER, qa_count)
      if (!guest_score_result$success) {
	stop(paste("Failed to initialize guest score:", guest_score_result$message))
      }

    return(list(
	qa_data = qa_data,
	user_score = guest_score_result$data,
	user_names = users_meta_result$data$username
    ))
    },
    success_message = "Data initialized successfully",
    error_message_prefix = "Failed to initialize data:"
  )
}

# ===============================
# Data Persistence Operations
# ===============================

data_save_user_score <- function(username, current_user, user_scores, qa_count) {
  utils_safe_execute(
    operation = function() {
      if (username != current_user) {
	stop("You switched user account.")
      }

      user_sheet_result <- data_write_user_score(username, user_scores)
      if (!user_sheet_result$success) {
	stop(user_sheet_result$message)
      }

      return(NULL)
    },
    success_message = paste("Score saved to user worksheet for user:", username),
    error_message_prefix = "Error saving to Google Sheets:"
  )
}

# ===========================
# Users Meta Management
# ===========================

data_add_user_to_meta <- function(username) {
  utils_safe_execute(
    operation = function(){
      users_meta_result <- data_read_users_meta()
      if (!users_meta_result$success) {
	stop(users_meta_result$message)
      }

      current_meta <- users_meta_result$data
      if (username %in% current_meta$username) {
        stop("User already exists in users_meta")
      }
  
      new_row <- data.frame(
        username = username,
        created_date = Sys.time(),
        last_updated = Sys.time(),
        question_count = 0,
        stringsAsFactors = FALSE
      )

      append_result <- storage_safe_append_with_retry(new_row, DATA$SHEETS$USERS_META, sheet = "users_meta")
      if (!append_result$success) {
        stop(append_result$message)
      }

      return(NULL)
    },
    success_message = paste("User", username, "added to users_meta"),
    error_message_prefix = "Error adding user to users_meta:"
  )
}

data_remove_user_from_meta <- function(username) {
  utils_safe_execute(
    operation = function() {
      users_meta_result <- data_read_users_meta()
      if (!users_meta_result$success) {
	stop(users_meta_result$message)
      }

      current_meta <- users_meta_result$data

      if (!(username %in% current_meta$username)) {
	stop("User not found in users_meta")
      }

      if (username == UI$DEFAULTS$USER) {
        stop("Cannot remove guest user from users_meta")
      } 

      updated_meta <- current_meta[current_meta$username != username,,drop = FALSE]

      write_result <- storage_safe_write_with_retry(updated_meta, ss = DATA$SHEETS$USERS_META, sheet = "users_meta")
      if (!write_result$success) {
        stop(write_result$message)
      }

      return(NULL)
    },
    success_message = paste("User", username, "removed from users_meta"),
    error_message_prefix = "Error removing user from users_meta:"
  )
}


# ===========================
# User Worksheet management
# ===========================

data_ensure_user_worksheet <- function(username, qa_count) {
  utils_safe_execute(
    operation = function() {
      sheet_name <- paste0("user_", username)
      existing_sheets <- sheet_names(DATA$SHEETS$SCORES)

      if (!(sheet_name %in% existing_sheets)) {
        sheet_add(DATA$SHEETS$SCORES, sheet = sheet_name)
  
        initial_data <- data.frame(
  	question_id = seq_len(qa_count),
  	score = rep(0L, qa_count),
  	stringsAsFactors = FALSE
        )

        write_result <- storage_safe_write_with_retry(initial_data, ss = DATA$SHEETS$SCORES, sheet = sheet_name)
        if (!write_result$success) {
	  stop(write_result$message)
        }
      }
      return(NULL)
    },
    success_message = paste("User worksheet ensured for:", username),
    error_message_prefix = "Error ensuring user worksheet:"
  )
}

data_write_user_score <- function(username, user_scores) {
  utils_safe_execute(
    operation = function() {

      sheet_name <- paste0("user_", username)
  
      ensure_result <- data_ensure_user_worksheet(username, length(user_scores))
      if (!ensure_result$success) {
        stop(ensure_result$message)
      }
  
      user_data <- data.frame(
        question_id = seq_along(user_scores),
        score = as.integer(user_scores),
        stringsAsFactors = FALSE
      )
  
      write_result <- storage_safe_write_with_retry(user_data, ss = DATA$SHEETS$SCORES, sheet = sheet_name)
      if (!write_result$success) {
        stop(write_result$message)
      }

      return(NULL)
    },
    success_message = "User score written to worksheet",
    error_message_prefix = "Error writing user score:"
  )
}

data_delete_user_worksheet <- function(username) {
  utils_safe_execute(
    operation = function() {
      
      sheet_name <- paste0("user_", username)
      existing_sheets <- sheet_names(DATA$SHEETS$SCORES)

      if (sheet_name %in% existing_sheets) {
        sheet_delete(DATA$SHEETS$SCORES, sheet = sheet_name)
        return(paste("User worksheet deleted:", sheet_name))
      } else {
        return(paste("User worksheet not found (already deleted):", sheet_name))
      }
    },
    success_message = "",
    error_message_prefix = "Error deleting user worksheet:",
    use_result_as_message = TRUE
  )
}

