# Rebuilds data/endotherm_species_presets.rda from the hand-edited CSV.
# After editing the CSV: source this file, then devtools::load_all() / install().
pkgload::load_all(".")

tbl <- .endo_read_csv(file.path("data-raw", "endotherm_species_presets.csv"))
tbl[] <- lapply(tbl, .endo_clean_cells)

stopifnot(identical(tbl$param[1], .endo_inherit_row))
species_cols <- setdiff(names(tbl), .endo_reserved_cols)
stopifnot(is.na(tbl[[species_cols[1]]][1]))

endotherm_species_presets <- .endo_check_presets(tbl, builtin = tbl)
for (s in species_cols)
  invisible(.endo_resolve_preset(endotherm_species_presets, s, 12, .endo_default_juldays))

dir.create("data", showWarnings = FALSE)
save(endotherm_species_presets, file = file.path("data", "endotherm_species_presets.rda"),
     compress = "bzip2")
message(sprintf("saved %d params x %d species", nrow(endotherm_species_presets) - 1L,
                length(species_cols)))
