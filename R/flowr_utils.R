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

get_all_nodes <- function() {
  with_connection(function(con) {
    query <- list(list(type = "id-map"))
    res <- flowr::request_query(con, get_filetoken(), query) |> verify_flowr_response()
    map <- res$res$results[["id-map"]]$idMap$k2v
    return(stats::setNames(lapply(map, `[[`, 2), lapply(map, `[[`, 1)))
  })
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

retrieve_slice <- function(file_filter = NULL) {
  slicing_points_measure <- measure(gather_slicing_points(file_filter))
  query_time <- slicing_points_measure$elapsed_time
  slicing_points <- slicing_points_measure$result
  criteria <- slicing_points$criteria
  slicing_points <- slicing_points$ids

  if (length(criteria) == 0) {
    logger::log_debug("Slice is empty", namespace = "slicingCoverage")
    return(list(result = vector()))
  }

  slicice_ids <- list()
  batch_size <- 20
  slicing_time <- 0
  for (i in seq(1, length(criteria), by = batch_size)) {
    j <- min(i + batch_size, length(criteria))
    logger::log_trace("Requesting %d of %d slices",
      ceiling(i / batch_size),
      ceiling(length(criteria) / batch_size),
      namespace = "slicingCoverage"
    )
    result <- with_connection(function(con) {
      query <- list(list(
        type = "static-slice",
        noReconstruction = TRUE,
        criteria = criteria[i:j]
      ))

      slicing_measure <- measure(flowr::request_query(con, get_filetoken(), query)) |> verify_flowr_response()
      slicing_time <- slicing_time + slicing_measure$elapsed_time
      slc_res <- slicing_measure$result |> verify_flowr_response()
      static_slice <- slc_res$res$results[["static-slice"]]
      ids <- list()
      for (r in static_slice$results) {
        ids <- c(ids, r$slice$result)
      }
      return(ids)
    })
    slicice_ids <- c(slicice_ids, result)
  }

  return(list(
    slice = slicice_ids,
    slicing_points = slicing_points,
    slicing_time = slicing_time,
    query_time = query_time
  ))
}

get_check_function_ids <- function(file_filter = NULL) {
  if (!is.null(file_filter)) {
    logger::log_debug("Only searching for assertions in %s", file_filter, namespace = "slicingCoverage")
  }

  with_connection(function(con) {
    query <- list(list(
      type = "compound",
      query = "call-context",
      commonArguments = c(list(
        callTargets = "global"
      ), if (!is.null(file_filter)) {
        list(fileFilter = list(
          filter = file_filter,
          includeUndefinedFiles = TRUE
        ))
      }),
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

gather_slicing_points <- function(file_filter = NULL) {
  logger::log_trace("Searching for slicing points", namespace = "slicingCoverage")
  check_function_ids <- get_check_function_ids(file_filter)
  logger::log_debug("Found %d slicing points", length(check_function_ids), namespace = "slicingCoverage")
  criteria <- lapply(check_function_ids, function(id) sprintf("$%s", id))
  return(list(
    criteria = criteria,
    ids = check_function_ids
  ))
}
