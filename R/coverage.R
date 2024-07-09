library(covr)

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
