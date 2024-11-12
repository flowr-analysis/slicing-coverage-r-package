add_ids_to_coverage <- function(coverage) {
  nodes <- lapply(get_all_nodes(), function(node) list(id = node$info$id, location = get_location(node)))
  nodes <- Filter(function(x) !is.null(x$location), nodes)

  for (node in nodes) {
    location <- node$location
    id <- node$id

    # TODO: this can definitely be optimized
    for (i in seq_along(coverage)) {
      elem <- coverage[[i]]
      srcref <- as.integer(elem$srcref) # line start, column start, line end, column end
      lines_match <- srcref[1] == location$first_line && srcref[3] == location$last_line
      cols_match <- srcref[2] == location$first_column && srcref[4] == location$last_column
      if (lines_match && cols_match) {
        elem$flowr_id <- id
        coverage[[i]] <- elem
      }
    }
  }

  return(coverage)
}

remove_ids_from_coverage <- function(coverage) {
  for (i in seq_along(coverage)) {
    elem <- coverage[[i]]
    elem$flowr_id <- NULL
    coverage[[i]] <- elem
  }
  return(coverage)
}

recalculate_values <- function(coverage, exec_and_slc_ids) {
  for (i in seq_along(coverage)) {
    elem <- coverage[[i]]
    elem$value <- if (elem$flowr_id %in% exec_and_slc_ids) 1 else 0
    coverage[[i]] <- elem
  }
  return(coverage)
}

gather_slicing_criteria <- function() {
  # FIXME: we only want to search in the test files, do we?
  check_function_ids <- get_check_function_ids()
  criteria <- lapply(check_function_ids, function(id) sprintf("$%s", id))
  return(criteria)
}

as_slicing_coverage <- function(coverage, slice) {
  coverage_with_ids <- add_ids_to_coverage(coverage)

  set_executed <- Filter(was_executed, coverage_with_ids) |>
    lapply(get_flowr_id) |>
    uneverything()
  set_slice <- unlist(slice)
  set_exec_and_slice <- intersect(set_executed, set_slice)

  slicing_coverage <- recalculate_values(coverage_with_ids, set_exec_and_slice) |> remove_ids_from_coverage()

  return(slicing_coverage)
}

retrieve_slice <- function(source_files, test_files) {
  criteria <- gather_slicing_criteria()

  if (length(criteria) == 0) {
    return(list(result = vector()))
  }

  result <- with_connection(function(con) {
    slc_res <- flowr::request_slice(con, get_filetoken(), criteria) |> verify_flowr_response()
    return(slc_res$res$results$slice)
  })

  return(result$result)
}

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

  coverage <- measure(covr::file_coverage(
    source_files = source_files,
    test_files = test_files,
    line_exclusions = line_exclusions,
    function_exclusions = function_exclusions
  ))

  slicing_coverage <- measure({
    init_analysis(c(source_files, test_files))
    slice <- retrieve_slice(source_files, test_files)
    as_slicing_coverage(coverage$result, slice)
  })

  coverage_time <- coverage$elapsed_time
  slicing_time <- slicing_coverage$elapsed_time

  if (get_option("measure_time")) {
    return(list(
      coverage_time = coverage_time,
      slicing_time = slicing_time,
      elapsed_time = coverage_time + slicing_time,
      coverage = slicing_coverage$result
    ))
  }

  return(slicing_coverage$result)
}

#' Calculate slicing coverage for code directly
#'
#' @param source_files Character vector of source files with function definitions to measure coverage
#' @param test_files Character vector of test files with code to test the functions
#'
#' @export
code_covergae <- function(source_files, test_files) {
  init_analysis(c(source_files, test_files))

  slice <- retrieve_slice(source_files, test_files)
  coverage <- covr::code_coverage(
    source_files = source_files,
    test_files = test_files
  )

  slicing_coverage <- as_slicing_coverage(coverage, slice)
  return(slicing_coverage)
}

#' Calculate slicing coverage for a package
#'
#' @param path Path to the package
#'
#' @export
package_coverage <- function(path = ".") {
  source_files <- get_pkg_source_files(path)
  test_files <- get_pkg_test_files(path)

  init_analysis(c(source_files, test_files))

  slice <- retrieve_slice(source_files, test_files)
  coverage <- covr::package_coverage(path = path)

  slicing_coverage <- as_slicing_coverage(coverage, slice)
  return(slicing_coverage)
}
