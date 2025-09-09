source("constants.R")
library(digest)

security_generate_salt <- function(length = 16) {
  raw_data <- paste0(Sys.time(), runif(1), sample(1000000, 1))
  substr(digest(raw_data, algo="md5", serialize=FALSE), 1, length)
}

security_hash_password <- function(password, salt) {
  digest(paste0(password, salt), algo = ADMIN$HASH_ALGORITHM, serialize=FALSE)
}

security_verify_password <- function(input_password, stored_hash, salt) {
  input_hash <- security_hash_password(input_password, salt)
  identical(input_hash, stored_hash)
}

security_setup_password <- function(password) {
  salt <- security_generate_salt()
  hash <- security_hash_password(password, salt)
  list(hash = hash, salt = salt)
}
