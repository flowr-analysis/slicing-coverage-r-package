with_connection <- function(f) {
  con <- get_connection()
  res <- f(con)
  return(res)
}

get_flowr_id <- function(coverage_info) {
  return(coverage_info$flowr_id)
}

was_executed <- function(coverage_info) {
  return(coverage_info$value == 1)
}

uneverything <- function(x) {
  return(unlist(x) |> unname())
}

get_pkg_source_files <- function(pkg) {
  path <- file.path(pkg, "R")
  files <- list.files(path, pattern = "\\.(R|r)$", full.names = TRUE) |> normalizePath()
  return(list(
    files = files,
    path = path
  ))
}

get_pkg_test_files <- function(pkg) {
  path <- file.path(pkg, "tests")
  files <- list.files(path, pattern = "\\.(R|r)$", full.names = TRUE, recursive = TRUE) |> normalizePath()
  return(list(
    files = files,
    path = path
  ))
}

get_location <- function(node) {
  res <- list()
  if ("fullRange" %in% names(node$info)) {
    location <- node$info$fullRange
    res$fullRange <- list(
      first_line = location[[1]],
      first_column = location[[2]],
      last_line = location[[3]],
      last_column = location[[4]]
    )
  }
  if ("location" %in% names(node)) {
    location <- node$location
    res$location <- list(
      first_line = location[[1]],
      first_column = location[[2]],
      last_line = location[[3]],
      last_column = location[[4]]
    )
  } else {
    return(NULL)
  }
  return(list(
    file = node$info$file,
    fullRange = res$fullRange,
    location = res$location
  ))
}

measure <- function(expr, only_time = FALSE) {
  s <- substitute(expr, parent.frame())
  # TODO: do we want to run gc here, or does it take too much time
  start_time <- proc.time()[[3]]
  res <- eval(s)
  end_time <- proc.time()[[3]]
  elapsed_time <- end_time - start_time
  if (only_time) {
    return(elapsed_time)
  }
  return(list(
    result = res,
    elapsed_time = elapsed_time
  ))
}

build_return_value <- function(covr, covr_time, slicing_coverage, slicing_points, ana_time, slicing_time, query_time, unknown_locations) {
  if (get_option("measure_time") || get_option("return_covr_result") || get_option("slicing_points")) {
    res <- list(coverage = slicing_coverage)
    if (get_option("measure_time")) {
      res$covr_time <- covr_time
      res$ana_time <- ana_time
      res$slicing_time <- slicing_time
      res$query_time <- query_time
      res$elapsed_time <- covr_time + ana_time + slicing_time + query_time
    }
    if (get_option("return_covr_result")) {
      res$covr <- covr
    }
    if (get_option("slicing_points")) {
      res$slicing_points <- slicing_points
    }
    if (get_option("unknown_locations")) {
      res$unknown_locations <- unknown_locations
    }
    return(res)
  }

  return(slicing_coverage)
}

build_loc2id_key <- function(file, location = NULL, srcref = NULL) {
  if (!missing(location)) {
    first_line <- location$first_line
    first_column <- location$first_column
    last_line <- location$last_line
    last_column <- location$last_column
  } else if (!missing(srcref)) {
    first_line <- srcref[1]
    first_column <- srcref[2]
    last_line <- srcref[3]
    last_column <- srcref[4]
  } else {
    stop("Either location or srcref must be provided")
  }

  return(sprintf("%s:%d:%d:%d:%d", file, first_line, first_column, last_line, last_column))
}

add_ids_to_coverage <- function(coverage) {
  logger::log_trace("Merging coverage and slice", namespace = "slicingCoverage")
  if ("result" %in% names(coverage)) {
    coverage <- coverage$result
  }

  nodes <- lapply(get_all_nodes(), function(node) list(id = node$info$id, location = get_location(node)))
  nodes <- Filter(function(x) !is.null(x$location), nodes)

  location_to_id <- new.env()
  for (node in nodes) {
    full_range <- node$location$fullRange
    location <- node$location$location
    file <- node$location$file
    file <- if (!is.null(file)) basename(file) else ""
    if (!is.null(full_range)) {
      key <- build_loc2id_key(file, location = full_range)
      location_to_id[[key]] <- c(location_to_id[[key]], node$id)
    }
    key <- build_loc2id_key(file, location = location)
    location_to_id[[key]] <- c(location_to_id[[key]], node$id)
  }

  unknown_locations <- list(r = 0, other = 0)
  for (file_and_srcref in names(coverage)) { # something like "file.R:4:3:4:7:3:7:4:4"
    elem <- coverage[[file_and_srcref]]
    srcref <- as.integer(elem$srcref) # line start, column start, line end, column end
    file <- strsplit(file_and_srcref, ":")[[1]][1]
    ids <- location_to_id[[build_loc2id_key(file, srcref = srcref)]]
    if (is.null(ids)) {
      ext <- tools::file_ext(file)
      if (ext == "r" || ext == "R") {
        unknown_locations$r <- unknown_locations$r + 1
      } else {
        unknown_locations$other <- unknown_locations$other + 1
      }
      next
    }
    elem$flowr_ids <- ids
    coverage[[file_and_srcref]] <- elem
  }

  for (type in names(unknown_locations)) {
    logger::log_warn("%d locations in %s files are not known to the slicer",
      unknown_locations[[type]],
      type,
      namespace = "slicingCoverage"
    )
  }

  return(list(
    coverage = coverage,
    unknown_locations = unknown_locations
  ))
}

remove_ids_from_coverage <- function(coverage) {
  for (i in seq_along(coverage)) {
    elem <- coverage[[i]]
    elem$flowr_ids <- NULL
    coverage[[i]] <- elem
  }
  return(coverage)
}

recalculate_values <- function(coverage, exec_and_slc_ids) {
  logger::log_trace("Adjusting coverage values", namespace = "slicingCoverage")
  for (i in seq_along(coverage)) {
    elem <- coverage[[i]]
    flowr_ids <- elem$flowr_ids
    if (is.null(flowr_ids)) { # This should not happen (see add_ids_to_coverage)
      next
    }
    if (any(flowr_ids %in% exec_and_slc_ids)) { # No need to change anything as element is in the slice
      next
    }

    elem$value <- 0
    coverage[[i]] <- elem
  }
  return(coverage)
}

as_slicing_coverage <- function(coverage, slice) {
  coverage_with_ids <- add_ids_to_coverage(coverage)
  unknown_locations <- coverage_with_ids$unknown_locations
  coverage_with_ids <- coverage_with_ids$coverage

  set_executed <- Filter(was_executed, coverage_with_ids) |>
    lapply(get_flowr_id) |>
    uneverything()
  set_slice <- unlist(slice)
  set_exec_and_slice <- intersect(set_executed, set_slice)

  slicing_coverage <- recalculate_values(coverage_with_ids, set_exec_and_slice) |> remove_ids_from_coverage()

  return(list(
    coverage = slicing_coverage,
    unknown_locations = unknown_locations
  ))
}

give_me_covr_and_i_do_the_rest <- function(covr_measure, sources, tests, test_path = NULL) {
  covr_time <- covr_measure$elapsed_time
  covr <- covr_measure$result

  ana_time <- measure(init_analysis(c(sources, tests)), only_time = TRUE)

  slicing_measure <- retrieve_slice(test_path)
  slicing_points <- slicing_measure$slicing_points
  slicing_time <- slicing_measure$slicing_time
  query_time <- slicing_measure$query_time
  slicing_coverage <- as_slicing_coverage(covr, slicing_measure)
  unknown_locations <- slicing_coverage$unknown_locations
  slicing_coverage <- slicing_coverage$coverage

  return(build_return_value(
    covr, covr_time,
    slicing_coverage, slicing_points, ana_time, slicing_time, query_time,
    unknown_locations
  ))
}
