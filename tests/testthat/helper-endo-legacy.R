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

endotherm_preset_template_for_test <- function() {
  tbl <- endotherm_species_presets
  tbl[["Test Sheep"]] <- NA_character_
  tbl[["Test Sheep"]][tbl$param == "__inherits_from__"] <- "Female Bighorn - Winter"
  tbl[["Test Sheep"]][tbl$param == "animal.body_mass"] <- "99"
  tbl
}

# Deliberate departures of the built-in Bighorn preset from the frozen legacy
# fixtures: tree sleep and huddling were wrongly enabled in the legacy script.
legacy_apply_fixes <- function(x) {
  x$thermoreg$tree_sleep_enabled <- "N"
  x$thermoreg$huddle_enabled <- "N"
  x
}

legacy_apply_fixes_endo_dat <- function(bytes) {
  txt <- rawToChar(bytes)
  fixed <- sub("\n'Y'(\t    85\\.0\t )'Y'(\t   3\t)", "\n'N'\\1'N'\\2", txt)
  stopifnot(!identical(fixed, txt))
  charToRaw(fixed)
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
