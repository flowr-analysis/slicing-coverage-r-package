make_connection_funs <- function(con = NULL) {
  get_connection <- function() {
    if (is.null(con)) {
      host <- get_option("flowr_host")
      port <- get_option("flowr_port")
      logger::log_debug("Opening connection to flowr on %s:%d", host, port, namespace = "slicingCoverage")
      con_res <- flowr::connect(host, port)
      con <<- con_res$connection
    }
    return(con)
  }
  close_connection <- function() {
    if (is.null(con)) {
      return()
    }
    logger::log_debug("Closing connection to flowr", namespace = "slicingCoverage")
    close(con)
    con <<- NULL
  }
  return(list(
    get_connection = get_connection,
    close_connection = close_connection
  ))
}

connection_funs <- make_connection_funs()
get_connection <- connection_funs$get_connection
close_connection <- connection_funs$close_connection

with_connection <- function(f) {
  con <- get_connection()
  res <- f(con)
  return(res)
}

make_analysis_info_funs <- function(filetoken = NULL) {
  return(list(
    init_analysis = function(files) {
      logger::log_trace("Requesting analysis", namespace = "slicingCoverage")
      with_connection(function(con) {
        res <- flowr::request_file_analysis(con, files) |> verify_flowr_response()
        filetoken <<- res$filetoken
        return()
      })
    },
    get_filetoken = function() {
      if (is.null(filetoken)) {
        stop("analysis session is not initialized")
      }
      return(filetoken)
    }
  ))
}

analysis_info_funs <- make_analysis_info_funs()
init_analysis <- analysis_info_funs$init_analysis
get_filetoken <- analysis_info_funs$get_filetoken

get_check_function_ids <- function() {
  with_connection(function(con) {
    query <- list(list(
      type = "compound",
      query = "call-context",
      commonArguments = list(
        callTargets = "global"
      ),
      arguments = get_all_groups() |> combine_groups()
    ))

    res <- flowr::request_query(con, get_filetoken(), query) |> verify_flowr_response()
    call_context <- res$res$results[["call-context"]]
    ids <- lapply(call_context$kinds, function(kind) {
      lapply(kind$subkinds, function(subkind) {
        lapply(subkind, function(elem) elem$id)
      })
    }) |> uneverything()

    return(ids)
  })
}

get_all_nodes <- function() {
  with_connection(function(con) {
    query <- list(list(type = "id-map"))
    res <- flowr::request_query(con, get_filetoken(), query) |> verify_flowr_response()
    map <- res$res$results[["id-map"]]$idMap$k2v
    return(setNames(lapply(map, `[[`, 2), lapply(map, `[[`, 1)))
  })
}

handle_flowr_error <- function(err) {
  # TODO: or should we maybe fall back to covr's output if there's an error?
  logger::log_warn("Slicer returned an error: %s", err, namespace = "slicingCoverage")
  stop(err)
}

verify_flowr_response <- function(res) {
  error <- res$error
  if (!is.null(error)) {
    return(handle_flowr_error(error))
  }
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
  files <- list.files(path, pattern = "\\.R$", full.names = TRUE) |> normalizePath()
  return(files)
}

get_pkg_test_files <- function(pkg) {
  path <- file.path(pkg, "tests")
  files <- list.files(path, pattern = "\\.R$", full.names = TRUE, recursive = TRUE) |> normalizePath()
  return(files)
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

measure <- function(expr) {
  s <- substitute(expr, parent.frame())
  # TODO: do we want to run gc here, or does it take too much time
  start_time <- proc.time()[[3]]
  res <- eval(s)
  end_time <- proc.time()[[3]]
  elapsed_time <- end_time - start_time
  return(list(
    result = res,
    elapsed_time = elapsed_time
  ))
}

build_return_value <- function(coverage, slicing_coverage) {
  coverage_time <- coverage$elapsed_time
  slicing_time <- slicing_coverage$elapsed_time

  if (get_option("measure_time") || get_option("return_covr_result") || get_option("slicing_points")) {
    res <- list(coverage = slicing_coverage$result$coverage)
    if (get_option("measure_time")) {
      res$coverage_time <- coverage_time
      res$slicing_time <- slicing_time
      res$elapsed_time <- coverage_time + slicing_time
    }
    if (get_option("return_covr_result")) {
      res$covr <- coverage$result
    }
    if (get_option("slicing_points")) {
      res$slicing_points <- slicing_coverage$result$slicing_points
    }
    return(res)
  }

  return(slicing_coverage$result$coverage)
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
