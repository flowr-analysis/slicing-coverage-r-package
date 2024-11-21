test_that("Reading config options set with 'configure' yields the set value", {
  configure(flowr_host = "test.host", flowr_port = 1234, measure_time = TRUE, return_covr_result = TRUE)

  expect_equal(get_option("flowr_host"), "test.host")
  expect_equal(get_option("flowr_port"), 1234)
  expect_equal(get_option("return_covr_result"), TRUE)
  expect_equal(get_option("measure_time"), TRUE)
})
