test_that("empty tests have n:wo coverage", {
  file <- file_with_content("add <- function(a,b) a+b")
  test <- file_with_content("")

  cov <- file_coverage(file, test)
  expect_equal(covr::percent_coverage(cov$coverage), 0)

  file <- file_with_content("")
  test <- file_with_content("")

  cov <- file_coverage(file, test)
  expect_equal(covr::percent_coverage(cov$coverage), NaN)

  file <- file_with_content("")
  test <- file_with_content("
    library(testthat)
    expect_true(TRUE)
  ")

  cov <- file_coverage(file, test)
  expect_equal(covr::percent_coverage(cov$coverage), NaN)
})

test_that("test without (real) assertion has zero slicing coverage", {
  file <- file_with_content("add <- function(a,b) a+b")
  test <- file_with_content("
    library(testthat)
    add(1,2)
  ")

  cov <- file_coverage(file, test)
  expect_equal(covr::percent_coverage(cov$coverage), 0)

  file <- file_with_content("add <- function(a,b) a+b")
  test <- file_with_content("
    library(testthat)
    add(1,2)
    expect_true(TRUE)
  ")

  cov <- file_coverage(file, test)
  expect_equal(covr::percent_coverage(cov$coverage), 0)
})

test_that("assertion in source file does is not sliced for", {
  file <- file_with_content("
    add <- function(a,b){
      testthat::expect_equal(1+2, 3)
      a+b
    }
  ")
  test <- file_with_content("add(1,2)")

  cov <- file_coverage(file, test)
  expect_equal(covr::percent_coverage(cov$coverage), 0)

  test <- file_with_content("
    library(testthat)
    expect_equal(add(1,2), 3)
  ")

  cov <- file_coverage(file, test)
  expect_equal(covr::percent_coverage(cov$coverage), 50)
})

test_that("slicing coverage can equal normal coverage", {
  file <- file_with_content("add <- function(a,b) a+b")
  test <- file_with_content("
    library(testthat)
    expect_equal(add(1,2), 3)
  ")

  cov <- file_coverage(file, test)
  expect_equal(covr::percent_coverage(cov$covr), covr::percent_coverage(cov$coverage))
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

  cov <- file_coverage(file, test)
  expect(covr::percent_coverage(cov$covr) != covr::percent_coverage(cov$coverage), "coverage should not be equal")
})

test_that("Coverage over multiple functions", {
  file <- file_with_content("
    add <- function(a,b) {
      x <- add_helper(a)
      y <- add_helper(b)
      x + y
    }

    add_helper <- function(n) {
      x <- 0
      while(x < n) {
        x <- x + 1
      }
      return(x)
    }
    ")
  test <- file_with_content("
    library(testthat)
    expect_equal(add(1,2), 3)
  ")

  cov <- file_coverage(file, test)
  expect_equal(covr::percent_coverage(cov$coverage), 100)

  file <- file_with_content("
    add <- function(a,b) {
      x <- add_helper(a)
      y <- add_helper(b)
      x + y
    }

    add_helper <- function(n) {
      x <- 0
      while(x < n) {
        x <- x + 1
      }
      return(x)
    }
    ")
  test <- file_with_content("
    library(testthat)
    expect_equal(add(1,2), 3)
  ")

  cov <- file_coverage(file, test)
  expect_equal(covr::percent_coverage(cov$coverage), 100)
})
