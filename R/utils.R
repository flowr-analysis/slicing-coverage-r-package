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

is_assertion <- function(vertex, filetoken, ast) {
  name <- vertex$name
  if (startsWith(name, "expect_")) {
    return(TRUE)
  }
  return(FALSE)
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
