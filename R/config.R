option_env <- new.env()
option_env$flowr_host <- "localhost"
option_env$flowr_port <- 1042

#' Configures various options for this package. If no value is given for any
#' option, it's default is used.
#'
#' @param flowr_host The host where the flowr server is running. Defaults to
#' "localhost".
#' @param flowr_port The port where the flowr server is running. Defaults to
#' 1042.
#'
#' @export
configure <- function(flowr_host = NULL, flowr_port = NULL) {
  if (!missing(flowr_host)) {
    option_env$flowr_host <- flowr_host
  }
  if (!missing(flowr_port)) {
    option_env$flowr_port <- flowr_port
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
