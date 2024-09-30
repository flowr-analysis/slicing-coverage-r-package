test_that("slicing coverage equals normal coverage for simple inputs", {
  file <- file_with_content("add <- function(a,b)a+b")
  test <- file_with_content("
    library(testthat)
    expect_equal(add(1,2), 3)
  ")

  x <- covr::file_coverage(c(file), c(test))
  y <- slicingCoverage::file_coverage(c(file), c(test))

  expect_equal(percent_coverage(x), percent_coverage(y$slicing))
})
