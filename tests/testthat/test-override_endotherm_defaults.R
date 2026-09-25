test_that("scalar overrides replace exactly the named field", {
  d <- get_endotherm_defaults()
  out <- override_endotherm_defaults(d, animal.body_mass = 60, thermoreg.huddle_enabled = "N")
  expect_identical(out$animal$body_mass, 60)
  expect_identical(out$thermoreg$huddle_enabled, "N")
  out$animal$body_mass <- d$animal$body_mass
  out$thermoreg$huddle_enabled <- d$thermoreg$huddle_enabled
  expect_identical(out, d)
})

test_that("a scalar aimed at a per-julday field is repeated; an exact-length vector is kept", {
  d <- get_endotherm_defaults()
  out <- override_endotherm_defaults(d, diet.food_fat_frac = 0.05)
  expect_identical(out$diet$food_fat_frac, rep(0.05, 12))
  v <- seq(0.01, 0.12, length.out = 12)
  out <- override_endotherm_defaults(d, diet.food_fat_frac = v)
  expect_identical(out$diet$food_fat_frac, v)
})

test_that("nested part fields can be overridden", {
  d <- get_endotherm_defaults()
  out <- override_endotherm_defaults(d, fur.parts.leg.fur_depth_dorsal = 8)
  expect_identical(out$fur$parts$leg$fur_depth_dorsal, 8)
})

test_that("bad inputs give clear errors", {
  d <- get_endotherm_defaults()
  expect_error(override_endotherm_defaults(d, 60), "must be named")
  expect_error(override_endotherm_defaults(d, animal.body_mas = 60),
               "Unknown parameter path 'animal.body_mas'.*animal.body_mass")
  expect_error(override_endotherm_defaults(d, fur.parts = 1), "Unknown parameter path 'fur.parts'")
  expect_error(override_endotherm_defaults(d, model_settings.julnum = 6), "julnum")
  expect_error(override_endotherm_defaults(d, model_settings.juldays = 1), "juldays")
  expect_error(override_endotherm_defaults(d, animal.body_mass = "heavy"), "must be numeric")
  expect_error(override_endotherm_defaults(d, animal.body_mass = c(1, 2)), "length 1")
  expect_error(override_endotherm_defaults(d, diet.food_fat_frac = c(1, 2, 3)), "length 12")
})

test_that("no overrides returns the input unchanged", {
  d <- get_endotherm_defaults()
  expect_identical(override_endotherm_defaults(d), d)
})

test_that("overriding a smaller-julnum defaults object uses its own julnum", {
  d <- get_endotherm_defaults(julnum = 6, juldays = c(15, 74, 135, 196, 258, 319))
  out <- override_endotherm_defaults(d, diet.food_fat_frac = 0.1)
  expect_length(out$diet$food_fat_frac, 6)
})

test_that("duplicate override paths are an error", {
  d <- get_endotherm_defaults()
  expect_error(override_endotherm_defaults(d, animal.body_mass = 1, animal.body_mass = 2),
               "duplicate.*animal.body_mass")
})

test_that("non-finite numeric and NA character overrides are errors naming the path", {
  d <- get_endotherm_defaults()
  expect_error(override_endotherm_defaults(d, animal.body_mass = NA_real_),
               "animal.body_mass.*finite")
  expect_error(override_endotherm_defaults(d, animal.body_mass = Inf),
               "animal.body_mass.*finite")
  expect_error(override_endotherm_defaults(d, diet.food_fat_frac = c(rep(0.1, 11), NaN)),
               "diet.food_fat_frac.*finite")
  expect_error(override_endotherm_defaults(d, animal.species_label = NA_character_),
               "animal.species_label.*NA")
})
