test_that("slicing coverage equals normal coverage for simple inputs", {
  file <- file_with_content("add <- function(a,b)a+b")
  test <- file_with_content("
    library(testthat)
    expect_equal(add(1,2), 3)
  ")

  x <- covr::file_coverage(c(file), c(test))
  y <- slicingCoverage::file_coverage(c(file), c(test))

  expect_equal(covr::percent_coverage(x), covr::percent_coverage(y))
})

test_that("slicing coverage does not equal normal coverage for simple inputs", {
  file <- file_with_content("
    add <- function(a,b) {
      x <<- 2;
      a + b
    }")
  test <- file_with_content("
    library(testthat)
    expect_equal(add(1,2), 3)
  ")

  x <- covr::file_coverage(c(file), c(test))
  y <- slicingCoverage::file_coverage(c(file), c(test))

  # Why tf does testthat not habe expect_not_equal?
  expect(covr::percent_coverage(x) != covr::percent_coverage(y), "coverage should not be equal")
})
