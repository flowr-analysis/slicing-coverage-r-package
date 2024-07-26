library(covr)
library(flowradapter)

#' @export
file_coverage <- function(
    source_files,
    test_files,
    line_exclusions = NULL,
    function_exclusions = NULL) {
  stopifnot(missing(line_exclusions), missing(function_exclusions))

  slice <- with_connection(function(con) {
    ana_res <- request_file_analysis(con, test_files[1])
    criteria <- vector()
    visit_nodes(ana_res$res$results$normalize, function(node) {
      if (node$type == "RFunctionCall" && node$named && node$functionName$content == "expect_equal") {
        criteria <<- append(criteria, sprintf("$%s", node$info$id))
      }
    })
    slc_res <- request_slice(con, ana_res$filetoken, criteria)
    return(slc_res$res$results$slice)
  })

  ids_in_slice <- slice$result
  print(ids_in_slice)

  covr_res <- covr::file_coverage(
    source_files = source_files,
    test_files = test_files,
    line_exclusions = line_exclusions,
    function_exclusions = function_exclusions
  )

  print(as.data.frame(covr_res))
}

#' @export
package_coverage <- function(line_exclusions = NULL,
                             function_exclusions = NULL,
                             ...,
                             exclusions) {
  stopifnot(missing(line_exclusions), missing(function_exclusions), missing(exclusions))

  covr::package_coverage(
    line_exclusions = line_exclusions,
    function_exclusions = function_exclusions,
    ...,
    exclusions = exclusions,
  )
}
