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

test_that("covr does not consider code outside of functions", {
  file <- file_with_content("
    x <- 2
    y <- x + 2
  ")
  test <- file_with_content("
    library(testthat)
    expect_true(x - 2 == 0)
  ")

  cov <- file_coverage(file, test)
  expect_equal(covr::percent_coverage(cov$covr), NaN)
  expect_equal(covr::percent_coverage(cov$coverage), NaN)
})

test_that("commented coded does not matter", {
  file <- file_with_content("
    # This is a long comment to
    # test if comments are ignored
    # Here's a joke:
    # Why did the math book look so sad? Because it had too many problems!
    # HAHAHAHAHAHAHA
    add <- function(a,b) a+b
  ")
  test <- file_with_content("
    library(testthat)
    expect_equal(add(1,2), 3)
  ")

  cov <- file_coverage(file, test)
  expect_equal(covr::percent_coverage(cov$coverage), 100)
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
    res <- 0
    add <- function(a,b) {
      add_helper(a)
      x <- res
      add_helper(b)
      y <- res
      x + y
    }

    add_helper <- function(n) {
      res <<- 0
      while(res < n) {
        res <<- res + 1
      }
    }
    ")
  test <- file_with_content("
    library(testthat)
    expect_equal(add(1,2), 3)
  ")

  cov <- file_coverage(file, test)
  # The slice is a bit weird here. It excludes `add_helper` and the calls to it
  # But 33.3 is the correct value for the given slice (independent of its correctness)
  expect_equal(covr::percent_coverage(cov$coverage), 33.3, tolerance = 0.1)

  file <- file_with_content("
    create_custom_list <- function(names, base_value) {
      custom_list <- setNames(
        lapply(seq_along(names), function(i) base_value * i),
        names
      )
      return(custom_list)
    }

    transform_list_elements <- function(input_list, filter_func, transform_func) {
      filtered_list <- input_list[sapply(input_list, filter_func)]
      transformed_list <- lapply(filtered_list, transform_func)
      return(transformed_list)
    }

    filtered_list <- function() transform_list_elements(
      create_custom_list(c('apple', 'banana', 'cherry'), 10),
      function(x) x > 15,
      function(x) x * 2
    )
  ")
  test <- file_with_content("
    library(testthat)
    expect_equal(length(filtered_list()), 2)
  ")

  cov <- file_coverage(file, test)
  expect_equal(covr::percent_coverage(cov$coverage), 100)
})

test_that("Coverage over multiple functions and files", {
  file1 <- file_with_content("
    add <- function(a,b) {
      x <- a+b+1
      return(sub_one(x))
    }
  ")
  file2 <- file_with_content("
    i_was_forgotten <- function() print('What a bummer')
    sub_one <- function(x) x-1
  ")
  test1 <- file_with_content("
    library(testthat)
    expect_equal(add(1,2), 3)
  ")
  test2 <- file_with_content("
    library(testthat)
    expect_equal(sub_one(0), -1)
  ")

  cov <- file_coverage(c(file1, file2), c(test1, test2))
  expect_equal(covr::percent_coverage(cov$coverage), 75)
})

test_that("We can find all assertions", {
  file <- file_with_content("")

  file_testthat <- file_with_content("
    library(testthat)
    expect_true(TRUE)
    expect_length(c(1,2,3), 3)
    tryCatch(fail(), error = function(e) NULL)
    expect(TRUE, 'When thats not true, I dont know what is')
  ")

  file_unitizer <- file_with_content("
    library(unitizer)
    unitizer_sect('test', {
      1+1
    })
  ")

  file_rlang <- file_with_content("
    library(rlang)
    tryCatch(warn('Warning'), warning = function(w) NULL)
    tryCatch(abort('Yeet'), error = function(e) NULL)
  ")

  file_xpectr <- file_with_content("
    library(xpectr)
    stop_if(FALSE)
    warn_if(FALSE)
  ")

  file_testit <- file_with_content("
    library(testit)
    assert(1+1 == 2)
  ")

  file_runit <- file_with_content("
    library(RUnit)
    checkEquals(2, 1+1)
    checkTrue(TRUE)
  ")

  file_r <- file_with_content("
    tryCatch(stop('Stopperino'), error = function(e) NULL)
    stopifnot(1+1 == 2)
  ")

  file_assertthat <- file_with_content("
    library(assertthat)
    assert_that(1+1 == 2)
  ")

  for (p in list(list("testthat", 4), list("unitizer", 1), list("rlang", 2), list("xpectr", 2), list("testit", 1), list("runit", 2), list("r", 2), list("assertthat", 1))) {
    pkg <- p[[1]]
    expected_assertions <- p[[2]]

    test_file <- get(paste0("file_", pkg))
    slicing_points <- file_coverage(file, test_file)$slicing_points

    test_that(paste("we can find all assertions in", pkg), {
      expect_length(slicing_points, expected_assertions)
    })
  }
})
