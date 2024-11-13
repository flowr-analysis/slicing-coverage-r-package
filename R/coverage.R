add_ids_to_coverage <- function(coverage) {
  if ("result" %in% names(coverage)) {
    coverage <- coverage$result
  }

  nodes <- lapply(get_all_nodes(), function(node) list(id = node$info$id, location = get_location(node)))
  nodes <- Filter(function(x) !is.null(x$location), nodes)

  location_to_id <- new.env()
  for (node in nodes) {
    location <- node$location
    # TODO: we also need to check the file name. Two files can have elements at the exact
    #       same location.
    key <- sprintf("%d-%d-%d-%d", location$first_line, location$first_column, location$last_line, location$last_column)
    if (exists(key, envir = location_to_id)) {
      cat("Duplicate location", key, "found\n")
    }
    location_to_id[[key]] <- node$id
  }

  for (i in seq_along(coverage)) {
    elem <- coverage[[i]]
    srcref <- as.integer(elem$srcref) # line start, column start, line end, column end
    id <- location_to_id[[sprintf("%d-%d-%d-%d", srcref[1], srcref[2], srcref[3], srcref[4])]]
    if (is.null(id)) {
      cat("No node found for", elem$srcref, "\n")
      next
    }
    elem$flowr_id <- id
    coverage[[i]] <- elem
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
    flowr_id <- elem$flowr_id
    if (is.null(flowr_id)) { # This should not happen (see add_ids_to_coverage)
      next
    }
    if (flowr_id %in% exec_and_slc_ids) { # No need to change anything as element is in the slice
      next
    }

    elem$value <- 0
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

  return(build_return_value(coverage, slicing_coverage))
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

  coverage <- measure(covr::package_coverage(path = path))

  slicing_coverage <- measure({
    init_analysis(c(source_files, test_files))
    slice <- retrieve_slice(source_files, test_files)
    as_slicing_coverage(coverage, slice)
  })

  return(build_return_value(coverage, slicing_coverage))
}
