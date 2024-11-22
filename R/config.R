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
#' @param list A list of options to set.
#'
#' @export
configure <- function(list = NULL, ...) {
  all_opts <- list()
  for (opt in ls(option_env)) {
    all_opts[[opt]] <- get_option(opt)
  }

  args <- if (is.list(list)) list else list(...)
  for (arg in names(args)) {
    if (!exists(arg, envir = option_env)) {
      warning(sprintf("Option '%s' does not exist", arg))
    }
    option_env[[arg]] <- args[[arg]]
    if (arg == "log_level") {
      logger::log_threshold(level = args[[arg]], namespace = "slicingCoverage")
      logger::log_formatter(logger::formatter_sprintf, namespace = "slicingCoverage")
    }
  }

  return(all_opts)
}


#' Retrieves the value of an option.
#'
#' @param option The option to retrieve.
#'
#' @export
get_option <- function(option) {
  return(option_env[[option]])
}

#' Executes an expression with the given options set and resets them afterwards.
#'
#' @param options A list of options to set.
#' @param expr The expression to execute
#'
#' @return The result of the expression.
#' @export
with_options <- function(options, expr) {
  prev_opts <- configure(options)
  on.exit(configure(list = prev_opts))
  return(expr)
}
