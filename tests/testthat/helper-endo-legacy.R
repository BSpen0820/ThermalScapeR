legacy_dir <- function() testthat::test_path("fixtures", "legacy")

legacy_rename_map <- function() {
  utils::read.csv(file.path(legacy_dir(), "endo_rename_map.csv"),
                  stringsAsFactors = FALSE)
}

flatten_leaves <- function(x, prefix = "") {
  out <- list()
  for (nm in names(x)) {
    path <- if (nzchar(prefix)) paste(prefix, nm, sep = ".") else nm
    if (is.list(x[[nm]])) {
      out <- c(out, flatten_leaves(x[[nm]], path))
    } else {
      out[[path]] <- x[[nm]]
    }
  }
  out
}

legacy_to_new <- function(x, map = legacy_rename_map(), prefix = "") {
  lookup <- stats::setNames(map$new_path, map$old_path)
  out <- list()
  for (nm in names(x)) {
    old_path <- if (nzchar(prefix)) paste(prefix, nm, sep = ".") else nm
    if (is.list(x[[nm]])) {
      out[[nm]] <- legacy_to_new(x[[nm]], map, old_path)
    } else {
      new_path <- if (old_path %in% names(lookup)) lookup[[old_path]] else old_path
      new_leaf <- utils::tail(strsplit(new_path, ".", fixed = TRUE)[[1]], 1)
      out[[new_leaf]] <- x[[nm]]
    }
  }
  out
}
