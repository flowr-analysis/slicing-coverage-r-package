configure(return_covr_result = TRUE, slicing_points = TRUE)

tryCatch(get_connection(),
  warning = function(e) {
    skip(sprintf("flowr is not reachable under %s:%s", get_option("flowr_host"), get_option("flowr_port")))
  }
)

withr::defer(close_connection(), teardown_env())
