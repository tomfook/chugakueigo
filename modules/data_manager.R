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
# Cache Functions
# =============================

data_get_cached_data <- function(cache_key, ttl_seconds, data_fetcher) {
  if (!CACHE$ENABLED) {
    return(data_fetcher())
  }

  tryCatch({
    if (!exists("app_global_cache", envir = .GlobalEnv)) {
      assign("app_global_cache", list(
	sheet_names = list(data = NULL, timestamp = NULL, is_valid = FALSE),
	users_meta = list(data = NULL, timestamp = NULL, is_valid = FALSE)
	), envir = .GlobalEnv)
    }
    cache <- get("app_global_cache", envir = .GlobalEnv)
    current_time <- Sys.time()

    cached_entry <- .subset2(cache, cache_key)
    if (!is.null(cached_entry) &&
	!is.null(cached_entry$data) &&
	!is.null(cached_entry$timestamp) &&
	difftime(current_time, cached_entry$timestamp, units = "secs") < ttl_seconds) {
      return(cached_entry$data)
    }

    result <- data_fetcher()

    cached_entry$data <- result
    cached_entry$timestamp <- current_time
    cached_entry$is_valid <- TRUE

    cache[[cache_key]] <- cached_entry

    assign("app_global_cache", cache, envir = .GlobalEnv)

    return(result)
  }, error = function(e) {
    warning(paste("API call failed for", cache_key, ", using cached data:", e$message))
    return(data_fetcher())
  })
}
      
data_get_cached_sheet_names <- function() {
  data_get_cached_data("sheet_names", CACHE$SHEET_NAMES$TTL, function() {sheet_names(DATA$SHEETS$SCORES)})
}

data_get_cached_users_meta <- function() {
  data_get_cached_data("users_meta", CACHE$USERS_META$TTL, function() {data_read_users_meta()})
}

data_invalidate_cache <- function(cache_key) {
  if (!CACHE$ENABLED) return()

  if (!exists("app_global_cache", envir = .GlobalEnv)) {
    return()
  }

  cache <- get("app_global_cache", envir = .GlobalEnv)
  cache[[cache_key]]$data <- NULL
  cache[[cache_key]]$timestamp <- NULL
  cache[[cache_key]]$is_valid <- FALSE
  assign("app_global_cache", cache, envir = .GlobalEnv)
}



# =============================
# Core File Operations
# =============================

data_read_user_score <- function(username, qa_data) {
  utils_safe_execute(
    operation = function() {
      qa_count = nrow(qa_data)
      if (username == APP$DEFAULTS$USER) {
	return(setNames(rep(0L, qa_count), as.character(1:qa_count)))
      }
      sheet_name <- paste0("user_", username)
      existing_sheets <- data_get_cached_sheet_names()

      if (!(sheet_name %in% existing_sheets)) {
	return(setNames(rep(0L, qa_count), as.character(1:qa_count)))
      }

      user_data <- read_sheet(DATA$SHEETS$SCORES, sheet = sheet_name, col_types = "ii")

      required_ids <- qa_data$question_id
      existing_ids <- user_data$question_id
      missing_ids <- setdiff(required_ids, existing_ids)

      if (length(missing_ids) > 0) {
	additional_data <- data.frame(
	  question_id = missing_ids,
	  score = rep(0L, length(missing_ids))
	)
	user_data <- bind_rows(user_data, additional_data)
      }
      
      user_data <- user_data[user_data$question_id %in% required_ids, ]
      user_data <- user_data[order(user_data$question_id), ]

      user_scores <- setNames(user_data$score, as.character(user_data$question_id))
      return(user_scores)
    },
    success_message = paste("User scores loaded successfully:", username),
    error_message_prefix = "Error reading user scores, using zeros:",
    fallback_data = setNames(rep(0L, nrow(qa_data)), as.character(qa_data$question_id))
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
	mutate(is_active = if("is_active" %in% names(.)) is_active else TRUE) %>%
	filter(question != "", answer != "", is_active == TRUE)
      qa_count <- nrow(qa_data)
      if (qa_count == 0){
	stop("No valid questions found in qlist.csv")
      }

      users_meta_result <- data_get_cached_users_meta()
      if (!users_meta_result$success) {
	stop(paste("Failed to initialize users_meta:", users_meta_result$message))
      }

      guest_score_result <- data_read_user_score(APP$DEFAULTS$USER, qa_data)
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

data_save_user_score <- function(username, current_user, user_scores) {
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
      users_meta_result <- data_get_cached_users_meta()
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
      data_invalidate_cache("users_meta")
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
      users_meta_result <- data_get_cached_users_meta()
      if (!users_meta_result$success) {
	stop(users_meta_result$message)
      }

      current_meta <- users_meta_result$data

      if (!(username %in% current_meta$username)) {
	stop("User not found in users_meta")
      }

      if (username == APP$DEFAULTS$USER) {
        stop("Cannot remove guest user from users_meta")
      } 

      updated_meta <- current_meta[current_meta$username != username,,drop = FALSE]

      write_result <- storage_safe_write_with_retry(updated_meta, ss = DATA$SHEETS$USERS_META, sheet = "users_meta")
      data_invalidate_cache("users_meta")
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

data_ensure_user_worksheet <- function(username, question_ids) {
  utils_safe_execute(
    operation = function() {
      sheet_name <- paste0("user_", username)
      existing_sheets <- data_get_cached_sheet_names()

      if (!(sheet_name %in% existing_sheets)) {
        sheet_add(DATA$SHEETS$SCORES, sheet = sheet_name)
	data_invalidate_cache("sheet_names")
  
        initial_data <- data.frame(
       	  question_id = as.integer(question_ids),
      	  score = rep(0L, length(question_ids)),
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
  
      ensure_result <- data_ensure_user_worksheet(username, names(user_scores))
      if (!ensure_result$success) {
        stop(ensure_result$message)
      }
  
      user_data <- data.frame(
        question_id = as.integer(names(user_scores)),
        score = user_scores,
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
      existing_sheets <- data_get_cached_sheet_names()

      if (sheet_name %in% existing_sheets) {
        sheet_delete(DATA$SHEETS$SCORES, sheet = sheet_name)
	data_invalidate_cache("sheet_names")
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

