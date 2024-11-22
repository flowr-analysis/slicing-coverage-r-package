configure(return_covr_result = TRUE, slicing_points = TRUE)

withr::defer(close_connection(), teardown_env())
