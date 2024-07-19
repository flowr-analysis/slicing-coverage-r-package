library(flowradapter)
library(jsonlite)
library(logger)

source("R/config.R")
source("R/logger.R")

logger <- create_new_logger("flowr")

fromRJSON <- function(x) {
  return(fromJSON(x,
    simplifyVector = FALSE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  ))
}

handle_res_error <- function(res) {
  if (res$type == "error") {
    logger(
      ERROR, "Error (%s): %s",
      ifelse(res$fatal, "fatal", "non fatal"),
      res$reason
    )
    return(TRUE)
  }
  return(FALSE)
}

get_connection <- function() {
  con_response <- connect(get_option("flowr_host"), get_option("flowr_port"))
  flowr_con <- con_response$connection
  flowr_hello <- fromJSON(con_response$hello)
  logger(
    INFO, "Connected to flowr %s as %s",
    flowr_hello$versions$flowr,
    flowr_hello$clientName
  )
  return(flowr_con)
}

get_filetoken <- function(filepath) {
  return(filepath)
}

initiate_file_analysis <- function(filepath, con = NULL) {
  if (is.null(con)) {
    con <- get_connection()
  }

  filepath <- normalizePath(filepath, mustWork = FALSE)
  filetoken <- get_filetoken(filepath)

  request <- fromRJSON(sprintf('{
    "type": "request-file-analysis",
    "filetoken": "%s",
    "filepath": "%s"
  }', filetoken, filepath))
  logger(INFO, "Requesting file analysis for %s", filepath)
  logger(DEBUG, "Request: %s", toJSON(request, pretty = TRUE))
  res <- send_request(con, request)

  if (handle_res_error(res)) {
    return(NULL)
  }
  logger(INFO, "Received file analysis for %s", filepath)
  return(list(filetoken = filetoken, res = res))
}

request_slice <- function(filepath, criteria) {
  con <- get_connection()
  file_ana_res <- initiate_file_analysis(filepath, con)
  if (is.null(file_ana_res)) {
    logger(ERROR, "Aborting slice request")
    return()
  }
  filetoken <- file_ana_res$filetoken

  request <- fromRJSON(sprintf('{
    "type": "request-slice",
    "filetoken": "%s",
    "criterion": %s
  }', filetoken, toJSON(criteria)))
  logger(INFO, "Requesting slice on %s for %s", filepath, criteria)
  logger(DEBUG, "Request: %s", toJSON(request, pretty = TRUE))
  res <- send_request(con, request)

  if (handle_res_error(res)) {
    return(NULL)
  }
  return(res)
}
