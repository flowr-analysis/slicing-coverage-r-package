make_connection_funs <- function(con = NULL) {
  get_connection <- function() {
    if (is.null(con)) {
      con_res <- flowr::connect(get_option("flowr_host"), get_option("flowr_port"))
      con <<- con_res$connection
    }
    return(con)
  }
  close_connection <- function() {
    if (is.null(con)) {
      return()
    }
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
  if ("fullRange" %in% names(node$info)) {
    location <- node$info$fullRange
  } else if ("location" %in% names(node)) {
    location <- node$location
  } else {
    return(NULL)
  }
  return(list(
    first_line = location[[1]],
    first_column = location[[2]],
    last_line = location[[3]],
    last_column = location[[4]]
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

  if (get_option("measure_time") || get_option("return_covr_result")) {
    res <- list(coverage = slicing_coverage$result)
    if (get_option("measure_time")) {
      res$coverage_time <- coverage_time
      res$slicing_time <- slicing_time
      res$elapsed_time <- coverage_time + slicing_time
    }
    if (get_option("return_covr_result")) {
      res$covr <- coverage$result
    }
    return(res)
  }

  return(slicing_coverage$result)
}
