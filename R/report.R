library(covr)

#' @export
report <- function(x = package_coverage()) {
  covr::report(x = x)
}
