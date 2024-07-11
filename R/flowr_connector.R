library(jsonlite)
source("R/logger.R")

logger <- create_new_logger("flowr connection")
flowr_con <- NULL

create_connection <- function(host = "localhost", port = 1042) {
  if (!is.null(flowr_con) && isOpen(flowr_con)) {
    logger(INFO, "Reusing existing connection")
    return()
  }
  # TODO: Handl error if connection can't be established
  flowr_con <<- socketConnection(
    host = host,
    port = port,
    server = FALSE,
    blocking = TRUE
  )
  server_hello_raw <- readLines(flowr_con, n = 1)
  server_hello <- fromJSON(server_hello_raw)
  logger(
    INFO, "Connected to flowr %s as %s",
    server_hello$versions$flowr,
    server_hello$clientName
  )
}

close_connection <- function() {
  if (is.null(flowr_con) || !isOpen(flowr_con)) {
    logger(INFO, "No connection to close")
    return()
  }
  close(flowr_con)
}
