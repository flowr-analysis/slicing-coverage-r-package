cache <- new.env()

file_with_content <- function(content, ext = ".R") {
  hash <- rlang::hash(list(c = content, e = ext))
  if (exists(hash, envir = cache)) {
    return(get(hash, envir = cache))
  }
  tmp <- tempfile(pattern = "slccov-test-", fileext = ext)
  writeLines(content, tmp)
  assign(hash, tmp, envir = cache)
  return(tmp)
}

skip_if_no_flowr <- function() {
  tryCatch(get_connection(),
    warning = function(e) {
      testthat::skip(sprintf("flowr is not reachable under %s:%s", get_option("flowr_host"), get_option("flowr_port")))
    }
  )
}
