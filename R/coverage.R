library(covr)
library(flowr)

get_location <- function(node) {
  if ("fullRange" %in% names(node$info)) {
    location <- node$info$fullRange
  } else if ("location" %in% names(node)) {
    location <- node$location
  } else {
    return(NULL)
  }
  return(list(
    first_line = location[1],
    first_column = location[2],
    last_line = location[3],
    last_column = location[4]
  ))
}

populate_corv_info_with_ids <- function(covr_info, ast) {
  covr_info[, "flowr_id"] <- NA
  visit_nodes(ast, function(node) {
    location <- get_location(node)
    if (is.null(location)) {
      return()
    }

    for (i in seq_len(nrow(covr_info))) {
      elem <- covr_info[i, ]
      lines_match <- elem$last_line == location$last_line && elem$first_line == location$first_line
      cols_match <- elem$first_column == location$first_column && elem$last_column == location$last_column
      if (lines_match && cols_match) {
        covr_info[i, "flowr_id"] <<- node$info$id
      }
    }
  })
  return(covr_info)
}

#' @export
file_coverage <- function(
    source_files,
    test_files,
    line_exclusions = NULL,
    function_exclusions = NULL) {
  stopifnot(missing(line_exclusions), missing(function_exclusions))

  result <- with_connection(function(con) {
    # TODO: Should be test_files[1]. But for this to work ,we need to tell flowr to also analyze files from source_files
    ana_res <- request_file_analysis(con, source_files[1])
    criteria <- vector()
    visit_nodes(ana_res$res$results$normalize, function(node) {
      if (node$type == "RFunctionCall" && node$named && node$functionName$content == "expect_equal") {
        criteria <<- append(criteria, sprintf("$%s", node$info$id))
      }
    })
    slc_res <- request_slice(con, ana_res$filetoken, criteria)
    return(list(
      ast = ana_res$res$results$normalize,
      slice = slc_res$res$results$slice
    ))
  })

  covr_res <- covr::file_coverage(
    source_files = source_files,
    test_files = test_files,
    line_exclusions = line_exclusions,
    function_exclusions = function_exclusions
  )

  df <- add_ids_to_corv_info(as.data.frame(covr_res), result$ast)

  set_executable <- df$flowr_id
  set_executed <- subset(df, value == 1)$flowr_id
  set_slice <- unlist(result$slice$result)
  set_exec_and_slice <- intersect(set_executed, set_slice)
  score <- length(set_exec_and_slice) / length(set_executable)

  print(set_executable)
  print(set_executed)
  print(set_slice)
  print(set_exec_and_slice)
  print(sprintf("%f%%", score * 100))
}
