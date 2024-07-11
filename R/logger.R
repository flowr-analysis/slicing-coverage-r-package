library(logger)

log_level <- Sys.getenv("LOG_LEVEL", unset = "INFO")

create_new_logger <- function(name) {
  layout <- function(...) {
    sprintf("[%s] %s", name, layout_simple(...))
  }

  return(logger(log_level, formatter_sprintf, layout, appender_console))
}
