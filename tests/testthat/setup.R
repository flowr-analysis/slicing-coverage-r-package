skip_on_ci()

tryCatch(get_connection(),
  warning = function(e) {
    skip(sprintf("flowr is not reachable under %s:%s", get_option("flowr_host"), get_option("flowr_port")))
  }
)

withr::defer(close_connection(), teardown_env())
