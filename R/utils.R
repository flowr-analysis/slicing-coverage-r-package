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

get_check_function_ids <- function(filetoken) {
  with_connection(function(con) {
    query <- list(list(
      type = "compound",
      query = "call-context",
      commonArguments = list(
        kind = "check",
        callTargets = "global"
      ),
      arguments = list(
        list(
          type = "call-context",
          callName = "^expect_.*$",
          subkind = "except"
        )
      )
    ))

    res <- flowr::request_query(con, filetoken, query)
    if (!is.null(res$error)) {
      handle_flowr_error(res$error)
    }

    call_context <- res$res$results[["call-context"]]
    ids <- lapply(call_context$kinds, function(kind) {
      lapply(kind$subkinds, function(subkind) {
        lapply(subkind, function(elem) elem$id)
      })
    }) |> uneverything()

    return(ids)
  })
}

handle_flowr_error <- function(err) {
  # TODO: or should we maybe fall back to covr's output if there's an error?
  stop(err)
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
