test_that("get_endotherm_defaults returns the nine groups write_endotherm_inputs expects", {
  expect_named(
    get_endotherm_defaults(),
    c("model_settings", "animal", "fur", "physiology", "diet",
      "thermoreg", "flying_digging", "nest_shelter", "allometry")
  )
})

test_that("get_endotherm_defaults's model_settings matches the julnum/juldays arguments by default", {
  defaults <- get_endotherm_defaults()
  expect_identical(defaults$model_settings$julnum, 12)
  expect_identical(defaults$model_settings$juldays,
                   c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349))
  expect_length(defaults$animal$mass_by_julday, 12)
  expect_length(defaults$diet$digestive_efficiency, 12)
})

test_that("get_endotherm_defaults resizes julnum-dependent vectors for a custom julnum", {
  custom_days <- c(15, 45, 74, 105, 135, 166)
  defaults <- get_endotherm_defaults(julnum = 6, juldays = custom_days)
  expect_identical(defaults$model_settings$julnum, 6)
  expect_identical(defaults$model_settings$juldays, custom_days)
  expect_length(defaults$animal$mass_by_julday, 6)
  expect_length(defaults$fur$torso_hair_length_dorsal_by_julday, 6)
  expect_length(defaults$physiology$core_temp_target_by_julday, 6)
  expect_length(defaults$diet$digestive_efficiency, 6)
})

test_that("get_endotherm_defaults errors when juldays length doesn't match julnum", {
  expect_error(get_endotherm_defaults(julnum = 6, juldays = 1:12), "must have length")
})

test_that("get_endotherm_defaults's output round-trips through write_endotherm_inputs via do.call", {
  tmp_dir <- tempfile("endo_defaults_"); dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  log <- do.call(write_endotherm_inputs, c(list(output_dir = tmp_dir), get_endotherm_defaults()))
  expect_true(file.exists(file.path(tmp_dir, "endo.dat")))
  expect_true(file.exists(file.path(tmp_dir, "alomvars.dat")))
  expect_true(all(log$status == "success"))
})

test_that("species must be a single string", {
  expect_error(get_endotherm_defaults(12), "single string")
  expect_error(get_endotherm_defaults(c("a", "b")), "single string")
  expect_error(get_endotherm_defaults(NA_character_), "single string")
})

test_that("an unknown species lists the available ones", {
  expect_error(get_endotherm_defaults("Moose - Winter"),
               "Unknown species 'Moose - Winter'.*Female Bighorn - Winter")
})

test_that("species matching is case-insensitive", {
  expect_identical(get_endotherm_defaults("female bighorn - winter"), get_endotherm_defaults())
})

test_that("list_endotherm_species returns the built-in species", {
  expect_identical(list_endotherm_species(), "Female Bighorn - Winter")
})

test_that("presets accepts a CSV path, including Excel-style BOM, padding and blank rows", {
  tbl <- endotherm_preset_template_for_test()
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(tbl, path, row.names = FALSE, na = "")
  bytes <- readBin(path, "raw", file.size(path))
  writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), bytes, charToRaw(",,,,\n")), path)

  d <- get_endotherm_defaults("Test Sheep", presets = path)
  expect_identical(d$animal$body_mass, 99)
  expect_identical(d$animal$body_fat_pct,
                   get_endotherm_defaults()$animal$body_fat_pct)
})

test_that("an older custom table missing a built-in param loads with a warning", {
  tbl <- endotherm_preset_template_for_test()
  tbl <- tbl[tbl$param != "animal.body_density", ]
  expect_warning(d <- get_endotherm_defaults("Test Sheep", presets = tbl), "animal.body_density")
  expect_identical(d$animal$body_density, get_endotherm_defaults()$animal$body_density)
})

test_that("a custom table with a misspelled param is rejected", {
  tbl <- endotherm_preset_template_for_test()
  tbl$param[tbl$param == "animal.body_mass"] <- "animal.body_mas"
  expect_error(get_endotherm_defaults("Test Sheep", presets = tbl), "unknown param.*animal.body_mas")
})
