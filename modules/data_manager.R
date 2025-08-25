library(dplyr)

read_score_improved <- function(qa, path = "data/score.csv") {
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

read.score <- function(qa, path = "data/score.csv") {
  read_score_improved(qa, path)
}
