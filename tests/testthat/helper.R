file_with_content <- function(content, ext = ".R") {
  tmp <- tempfile(pattern = "slccov-test-", fileext = ext)
  writeLines(content, tmp)
  return(tmp)
}
