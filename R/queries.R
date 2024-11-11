load_group <- function(name) {
  file_path <- system.file("queries", name, package = "slicingCoverage")
  if (!file.exists(file_path)) {
    stop("Query file ", file_path, " not found.")
  }

  group <- eval(parse(text = paste0("source('", file_path, "', local = TRUE)")))
  return(group$value)
}

normalize_group <- function(group) {
  return(group$parts |> lapply(function(part) {
    part$kind <- group$kind
    part$callNameExact <- TRUE
    part$includeAliases <- TRUE
    return(part)
  }))
}

combine_groups <- function(names) {
  groups <- lapply(names, load_group)
  return(groups |> lapply(normalize_group) |> unlist(recursive = FALSE))
}

get_all_groups <- function() {
  return(list.files(system.file("queries", package = "slicingCoverage")))
}
