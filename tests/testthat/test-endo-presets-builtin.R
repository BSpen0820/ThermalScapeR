test_that("built-in table has the expected shape", {
  tbl <- endotherm_species_presets
  expect_identical(names(tbl)[1:3], c("param", "type", "by_julday"))
  expect_identical(tbl$param[1], "__inherits_from__")
  expect_true("Female Bighorn - Winter" %in% names(tbl))
  expect_true(is.na(tbl[["Female Bighorn - Winter"]][1]))
  expect_false(anyNA(tbl[["Female Bighorn - Winter"]][-1]))
})

test_that("built-in root species resolves to the renamed legacy defaults", {
  cases <- list(
    list(file = "defaults_j12.rds", julnum = 12, juldays = .endo_default_juldays),
    list(file = "defaults_j6.rds",  julnum = 6,  juldays = c(15, 74, 135, 196, 258, 319))
  )
  for (case in cases) {
    legacy <- readRDS(file.path(legacy_dir(), case$file))
    resolved <- .endo_resolve_preset(endotherm_species_presets, "Female Bighorn - Winter",
                                     case$julnum, case$juldays)
    expect_identical(resolved, legacy_to_new(legacy), info = case$file)
  }
})

test_that(".endo_baseline is the root species at the requested julnum", {
  expect_identical(.endo_baseline(),
                   .endo_resolve_preset(endotherm_species_presets, "Female Bighorn - Winter"))
  expect_length(.endo_baseline(6, c(15, 74, 135, 196, 258, 319))$diet$digestive_efficiency, 6)
})
