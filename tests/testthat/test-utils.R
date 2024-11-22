skip_if_offline()

test_that("we can find all pakage test files", {
  tmp_dir <- tempdir()
  dest_zip <- file.path(tmp_dir, "covr.zip")
  covr_dir <- file.path(tmp_dir, "covr-3.6.0")
  tryCatch(download.file(
    url = "https://github.com/r-lib/covr/archive/refs/tags/v3.6.0.zip",
    destfile = dest_zip,
    quiet = TRUE
  ), error = function(e) skip("could not download package"))
  unzip(dest_zip, exdir = tmp_dir)

  sources <- get_pkg_source_files(covr_dir)
  tests <- get_pkg_test_files(covr_dir)

  expect_equal(length(sources$files), 27)
  expect_equal(length(tests$files), 73)
})
