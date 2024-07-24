library(covr)
library(logger)

logger <- create_new_logger("cov")

#' @export
file_coverage <- function(
    source_files,
    test_files,
    line_exclusions = NULL,
    function_exclusions = NULL) {
  if (!missing(line_exclusions)) {
    stop("line_exclusions is not supported")
  }
  if (!missing(function_exclusions)) {
    stop("function_exclusions is not supported")
  }

  request_slice(source_files[1], c("3@z"))

  covr::file_coverage(
    source_files = source_files,
    test_files = test_files,
    line_exclusions = line_exclusions,
    function_exclusions = function_exclusions
  )
}

#' @export
package_coverage <- function(line_exclusions = NULL,
                             function_exclusions = NULL,
                             ...,
                             exclusions) {
  if (!missing(line_exclusions)) {
    stop("line_exclusions is not supported")
  }
  if (!missing(function_exclusions)) {
    stop("function_exclusions is not supported")
  }
  if (!missing(exclusions)) {
    stop("exclusions is not supported")
  }

  covr::package_coverage(
    line_exclusions = line_exclusions,
    function_exclusions = function_exclusions,
    ...,
    exclusions = exclusions,
  )
}
