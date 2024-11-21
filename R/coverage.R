logger::log_threshold(level = get_option("log_level"), namespace = "slicingCoverage")
logger::log_formatter(logger::formatter_sprintf, namespace = "slicingCoverage")

#' Calculate slicing coverage for a set of files
#'
#' @param source_files Character vector of source files with function definitions to measure coverage
#' @param test_files Character vector of test files with code to test the functions
#' @param line_exclusions Currently unsupported
#' @param function_exclusions Currently unsupported
#'
#' @export
file_coverage <- function(
    source_files,
    test_files,
    line_exclusions = NULL,
    function_exclusions = NULL) {
  stopifnot(missing(line_exclusions), missing(function_exclusions))

  logger::log_trace("Tracing coverage", namespace = "slicingCoverage")
  covr_measure <- measure(covr::file_coverage(
    source_files = source_files,
    test_files = test_files,
  ))

  return(give_me_covr_and_i_do_the_rest(covr_measure, source_files, test_files))
}

#' Calculate slicing coverage for a package
#'
#' @param path Path to the package
#'
#' @export
package_coverage <- function(path = ".") {
  sources <- get_pkg_source_files(path)
  tests <- get_pkg_test_files(path)

  logger::log_trace("Tracing coverage", namespace = "slicingCoverage")
  covr_measure <- measure(covr::package_coverage(path = path))

  test_path <- file.path(path, "tests")

  return(give_me_covr_and_i_do_the_rest(covr_measure, sources$files, tests$files, test_path = tests$path))
}
