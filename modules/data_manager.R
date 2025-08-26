library(dplyr)

read_score <- function(qa, path = "data/score.csv") {
  if (!file.exists(path)) {
    warning("Score file not found, creating new one: ", path)
    score <- data.frame(guest = rep(0L, nrow(qa)))
    write.table(score, path, row.names = FALSE, sep = ",")
    return(score)
  }

  tryCatch({
    score <- read.csv(path, comment = "#", stringsAsFactors = FALSE)
  }, error = function(e) {
    stop("Error reading score file: ", e$message)
  })

  if (nrow(qa) > nrow(score)) {
    zero.score <- head(score, nrow(qa) - nrow(score))
    zero.score[] <- 0L
    score <- bind_rows(score, zero.score)
  }

  return(score)
}

initialize_data <- function() {
  tryCatch({
    main <- read.csv("data/qlist.csv", comment = "#", stringsAsFactors = FALSE) %>%
      filter(question != "", answer != "")

    if (nrow(main) == 0) {
      stop("No valid questions found in qlist.csv")
    }

    score_global <- read_score(qa = main, path = "data/score.csv") %>%
      mutate(guest = 0L) %>%
      select(guest, everything())

    if (nrow(main) != nrow(score_global)) {
      warning("Question and score data length mismatch. This has been automatically corrected.")
    }

    list(main = main, score_global = score_global)
  }, error = function(e) {
    stop("Failed to initialize data: ", e$message)
  })
}

