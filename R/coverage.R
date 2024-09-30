get_location <- function(node) {
  if ("fullRange" %in% names(node$info)) {
    location <- node$info$fullRange
  } else if ("location" %in% names(node)) {
    location <- node$location
  } else {
    return(NULL)
  }
  return(list(
    first_line = location[1],
    first_column = location[2],
    last_line = location[3],
    last_column = location[4]
  ))
}

add_ids_to_cov <- function(coverage, ast) {
  flowr::visit_nodes(ast, function(node) {
    location <- get_location(node)
    if (is.null(location)) {
      return()
    }

    for (i in seq_along(coverage)) {
      elem <- coverage[[i]]
      srcref <- as.integer(elem$srcref) # line start, column start, line end, column end
      lines_match <- srcref[1] == location$first_line && srcref[3] == location$last_line
      cols_match <- srcref[2] == location$first_column && srcref[4] == location$last_column
      if (lines_match && cols_match) {
        elem$flowr_id <- node$info$id
        coverage[[i]] <<- elem
      }
    }
  })
  return(coverage)
}

remove_ids_from_cov <- function(coverage) {
  for (i in seq_along(coverage)) {
    elem <- coverage[[i]]
    elem$flowr_id <- NULL
    coverage[[i]] <- elem
  }
  return(coverage)
}

as_slicing_coverage <- function(coverage, exec_and_slc_ids) {
  for (i in seq_along(coverage)) {
    elem <- coverage[[i]]
    elem$value <- if (elem$flowr_id %in% exec_and_slc_ids) 1 else 0
    coverage[[i]] <- elem
  }
  return(coverage)
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

  result <- with_connection(function(con) {
    ana_res <- flowr::request_file_analysis(con, c(source_files, test_files))
    if (!is.null(ana_res$error)) {
      handle_flowr_error(ana_res$error)
    }

    criteria <- Reduce(function(a, v) {
      elem <- v[[2]]
      if (elem$tag != "function-call" || !is_assertion(elem, ana_res$filetoken, ana_res$res$results$normalize)) {
        return(a)
      }
      return(append(a, sprintf("$%s", elem$id)))
    }, ana_res$res$results$dataflow$graph$vertexInformation, vector())

    if (length(criteria) == 0) {
      return(list(
        ast = ana_res$res$results$normalize,
        slice = NULL
      ))
    }

    slc_res <- flowr::request_slice(con, ana_res$filetoken, criteria)
    if (!is.null(slc_res$error)) {
      handle_flowr_error(slc_res$error)
    }

    return(list(
      ast = ana_res$res$results$normalize,
      slice = slc_res$res$results$slice
    ))
  })

  coverage <- covr::file_coverage(
    source_files = source_files,
    test_files = test_files,
    line_exclusions = line_exclusions,
    function_exclusions = function_exclusions
  )

  coverage_with_ids <- add_ids_to_cov(coverage, result$ast)

  set_executed <- Filter(was_executed, coverage_with_ids) |> lapply(get_flowr_id) |> unname() |> unlist()
  set_slice <- if (!is.null(result$slice)) unlist(result$slice$result) else vector()
  set_exec_and_slice <- intersect(set_executed, set_slice)

  slicing_coverage <- as_slicing_coverage(coverage_with_ids, set_exec_and_slice) |> remove_ids_from_cov()

  # TODO: remove later for compatibility with covr (currently it's useful for 'debugging')
  return(list(
    original = coverage,
    slicing = slicing_coverage
  ))
}
