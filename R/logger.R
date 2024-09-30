log_level <- Sys.getenv("LOG_LEVEL", unset = "INFO")

create_new_logger <- function(name) {
  layout <- function(...) {
    sprintf("[%s] %s", name, logger::layout_simple(...))
  }

  return(logger::logger(log_level, logger::formatter_sprintf, layout, logger::appender_console))
}
