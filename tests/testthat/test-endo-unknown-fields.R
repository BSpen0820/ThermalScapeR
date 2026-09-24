new_endo_dir <- function() {
  d <- tempfile("endo_unknown_")
  dir.create(d)
  d
}

test_that("a pre-rename group field is an error naming it and the closest valid field", {
  d <- new_endo_dir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_error(write_endotherm_inputs(d, animal = list(mass = 60)),
               "animal.mass (did you mean animal.body_mass?)", fixed = TRUE)
  expect_false(file.exists(file.path(d, "endo.dat")))
})

test_that("a pre-rename nested part field is an error naming the dotted path", {
  d <- new_endo_dir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_error(write_endotherm_inputs(d, fur = list(parts = list(leg = list(diad = 1)))),
               "fur.parts.leg.diad", fixed = TRUE)
  expect_error(write_endotherm_inputs(d, allometry = list(parts = list(torso = list(len = 1)))),
               "allometry.parts.torso.len", fixed = TRUE)
  expect_error(write_endotherm_inputs(d, fur = list(parts = list(snout = list(hair_diameter_dorsal = 1)))),
               "fur.parts.snout", fixed = TRUE)
})

test_that("every unknown field across groups is reported in one error", {
  d <- new_endo_dir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  err <- tryCatch(
    write_endotherm_inputs(d, animal = list(mass = 60, fat = 1), diet = list(xyz = 1)),
    error = function(e) conditionMessage(e))
  expect_match(err, "write_endotherm_inputs()", fixed = TRUE)
  expect_match(err, "animal.mass", fixed = TRUE)
  expect_match(err, "animal.fat", fixed = TRUE)
  expect_match(err, "diet.xyz", fixed = TRUE)
})

test_that("unnamed group elements are an error", {
  d <- new_endo_dir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_error(write_endotherm_inputs(d, animal = list(60)), "animal")
})

test_that("valid nested and partial overrides still write files", {
  d <- new_endo_dir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_no_error(write_endotherm_inputs(
    d,
    animal = list(body_mass = 60),
    fur = list(parts = list(leg = list(hair_diameter_dorsal = 90)), hair_diameter_dorsal = 80),
    model_settings = list(julnum = 12, juldays = c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349))
  ))
  expect_true(file.exists(file.path(d, "endo.dat")))
  expect_true(file.exists(file.path(d, "alomvars.dat")))
})

test_that("a full defaults list and a chunk-prepared time-varying list pass the check", {
  d <- new_endo_dir(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_no_error(do.call(write_endotherm_inputs,
                          c(list(output_dir = d), get_endotherm_defaults())))

  d2 <- new_endo_dir(); on.exit(unlink(d2, recursive = TRUE), add = TRUE)
  n <- 30; idx <- 1:10
  tv <- endo_timevar_template()
  for (nm in names(endo_timevar_template())) tv[[nm]] <- rep(1, n)
  x <- get_endotherm_defaults()
  x$model_settings$julnum <- length(idx)
  x$model_settings$juldays <- seq_along(idx)
  x <- .endo_resize_static_fields(x, length(idx))
  x <- .endo_apply_timevar(x, tv, idx)
  expect_no_error(do.call(write_endotherm_inputs, c(list(output_dir = d2), x)))
})

test_that(".endo_validate_timevar_names rejects a pre-rename key with a hint", {
  tv <- endo_timevar_template()
  tv$mass2 <- rep(60, 5)
  expect_error(.endo_validate_timevar_names(tv),
               "mass2 (did you mean", fixed = TRUE)
  expect_true(.endo_validate_timevar_names(endo_timevar_template()))
  expect_true(.endo_validate_timevar_names(NULL))
  ok <- endo_timevar_template(); ok$mass_by_julday <- rep(60, 5)
  expect_true(.endo_validate_timevar_names(ok))
})

test_that("run_endo_big_nichemap rejects an unknown time_varying name before any I/O", {
  expect_error(
    run_endo_big_nichemap(
      tile_map = "x", valid_cells_mask = "x",
      dates = as.Date(c("2022-07-01", "2022-07-10")), microclim_dir = "x", dem = "x",
      refl_dir = "x", exe_path = "x", output_dir = tempdir(), wineprefix = tempdir(),
      time_varying = list(mass2 = rep(60, 10))
    ),
    "Unknown time_varying name(s): mass2", fixed = TRUE
  )
})
