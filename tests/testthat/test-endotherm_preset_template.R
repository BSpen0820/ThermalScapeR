test_that("template appends one blank column that inherits from base_species", {
  tbl <- endotherm_preset_template("Elk - Winter", base_species = "Female Bighorn - Winter")
  expect_identical(names(tbl), c(names(endotherm_species_presets), "Elk - Winter"))
  expect_identical(tbl[["Elk - Winter"]][tbl$param == "__inherits_from__"], "Female Bighorn - Winter")
  expect_true(all(is.na(tbl[["Elk - Winter"]][tbl$param != "__inherits_from__"])))
  expect_identical(tbl$param, endotherm_species_presets$param)
})

test_that("base_species is matched case-insensitively and validated", {
  tbl <- endotherm_preset_template("Elk", base_species = "female bighorn - winter")
  expect_identical(tbl$Elk[1], "Female Bighorn - Winter")
  expect_error(endotherm_preset_template("Elk", base_species = "Moose"), "Unknown species 'Moose'")
})

test_that("base_species = NULL leaves the new column as a blank root", {
  tbl <- endotherm_preset_template("Elk")
  expect_true(all(is.na(tbl$Elk)))
})

test_that("new_species must be a single new name", {
  expect_error(endotherm_preset_template(c("a", "b")), "single non-empty string")
  expect_error(endotherm_preset_template(""), "single non-empty string")
  expect_error(endotherm_preset_template("Female Bighorn - Winter"), "already exists")
  expect_error(endotherm_preset_template("param"), "already exists")
  expect_error(endotherm_preset_template("__inherits_from__"), "cannot be used")
})

test_that("output_file writes a CSV that round-trips through get_endotherm_defaults", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  res <- withVisible(endotherm_preset_template("Elk - Winter", base_species = "Female Bighorn - Winter",
                                               output_file = path))
  expect_false(res$visible)
  expect_true(file.exists(path))
  expect_identical(get_endotherm_defaults("Elk - Winter", presets = path),
                   get_endotherm_defaults())
})

test_that("a filled-in template yields a species whose deltas apply on top of the parent", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  tbl <- endotherm_preset_template("Elk - Winter", base_species = "Female Bighorn - Winter")
  tbl[["Elk - Winter"]][tbl$param == "animal.body_mass"] <- "250"
  tbl[["Elk - Winter"]][tbl$param == "diet.digestive_efficiency"] <- "0.6"
  utils::write.csv(tbl, path, row.names = FALSE, na = "")
  d <- get_endotherm_defaults("Elk - Winter", presets = path)
  expect_identical(d$animal$body_mass, 250)
  expect_identical(d$animal$mass_by_julday, rep(250, 12))
  expect_identical(d$diet$digestive_efficiency, rep(0.6, 12))
  expect_identical(d$animal$body_fat_pct, get_endotherm_defaults()$animal$body_fat_pct)
})

test_that("extending a custom table (presets =) keeps its existing columns", {
  first <- endotherm_preset_template("Elk - Winter", base_species = "Female Bighorn - Winter")
  second <- endotherm_preset_template("Elk - Summer", base_species = "Elk - Winter", presets = first)
  expect_identical(names(second)[(ncol(second) - 1):ncol(second)], c("Elk - Winter", "Elk - Summer"))
  expect_identical(second[["Elk - Summer"]][1], "Elk - Winter")
})

test_that("new_species differing only by case from an existing column is refused", {
  expect_error(endotherm_preset_template("female bighorn - winter"),
               "differs only by case from existing species 'Female Bighorn - Winter'")
})
