test_that("with_options correctly resets options after execution", {
  opts <- configure()
  with_options(list(flowr_host = "not.localhost", flowr_port = 1234), {
    expect_equal(get_option("flowr_host"), "not.localhost")
    expect_equal(get_option("flowr_port"), 1234)
  })
  expect_equal(configure(), opts)
})

test_that("reading config options set with 'configure' yields the set value", {
  with_options(list(flowr_host = "localhost", flowr_port = 1042, measure_time = FALSE, return_covr_result = FALSE), {
    expect_equal(get_option("flowr_host"), "localhost")
    expect_equal(get_option("flowr_port"), 1042)
    expect_equal(get_option("measure_time"), FALSE)
    expect_equal(get_option("return_covr_result"), FALSE)
  })
})

test_that("setting nonexistent options yields a warning", {
  expect_warning(configure(nonexistent = "value"))
})
