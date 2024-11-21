option_env <- new.env()
option_env$flowr_host <- "localhost"
option_env$flowr_port <- 1042
option_env$measure_time <- FALSE
option_env$return_covr_result <- FALSE
option_env$slicing_points <- FALSE
option_env$log_level <- "INFO"
option_env$unknown_locations <- FALSE

#' Configures various options for this package. If no value is given for any
#' option, it's default is used.
#'
#' @param flowr_host The host where the flowr server is running. Defaults to
#' "localhost".
#' @param flowr_port The port where the flowr server is running. Defaults to
#' 1042.
#' @param measure_time Whether to measure the execution time of different
#' calculations. Defaults to FALSE.
#' @param return_covr_result Whether to also return the coverage covr calculated
#' @param slicing_points Whether to return all slicing points we found and slices for
#' @param log_level The minimum level of logs that should be shown
#' @param unknown_locations Whether to return the number of source refs flowr did not
#' know about
#'
#' @export
configure <- function(flowr_host = NULL,
                      flowr_port = NULL,
                      measure_time = NULL,
                      return_covr_result = NULL,
                      slicing_points = NULL,
                      log_level = NULL,
                      unknown_locations = NULL) {
  if (!missing(flowr_host)) {
    option_env$flowr_host <- flowr_host
  }
  if (!missing(flowr_port)) {
    option_env$flowr_port <- flowr_port
  }
  if (!missing(measure_time)) {
    option_env$measure_time <- measure_time
  }
  if (!missing(return_covr_result)) {
    option_env$return_covr_result <- return_covr_result
  }
  if (!missing(slicing_points)) {
    option_env$slicing_points <- slicing_points
  }
  if (!missing(log_level)) {
    option_env$log_level <- log_level
    logger::log_threshold(level = log_level, namespace = "slicingCoverage")
    logger::log_formatter(logger::formatter_sprintf, namespace = "slicingCoverage")
  }
  if (!missing(unknown_locations)) {
    option_env$unknown_locations <- unknown_locations
  }
}


#' Retrieves the value of an option.
#'
#' @param option The option to retrieve.
#'
#' @export
get_option <- function(option) {
  return(option_env[[option]])
}
