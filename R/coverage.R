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

  return(give_me_covr_and_i_do_the_rest(covr_measure, sources$files, tests$files))
}

#' Calculate the maximum possible slicing coverage with the current assertions.
#' Considering this value can indicate whether the slicing coverage score is limited by
#' the number of assertions or the amount of covered code.
#'
#' @param f Function to calculate slicing coverage
#' @param ... Arguments to pass to the function
#'
#' @export
maximum_coverage <- function(f, ...) {
  cov <- with_options(list(return_annotated_cov = TRUE), f(...))
  max_cov <- recalculate_values(cov, new_value = function(row) {
    if (is.null(row$in_slice) || row$in_slice) {
      max(row$value, 1)
    } else {
      0
    }
  }) |> remove_slc_from_coverage()
  return(max_cov)
}
