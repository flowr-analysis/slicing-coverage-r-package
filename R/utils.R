library(flowr)

make_get_connection <- function(con = NULL) {
  function() {
    if (is.null(con)) {
      con_res <- connect(get_option("flowr_host"), get_option("flowr_port"))
      con <<- con_res$connection
    }
    return(con)
  }
}
get_connection <- make_get_connection()

with_connection <- function(f) {
  con <- get_connection()
  res <- f(con)
  return(res)
}
