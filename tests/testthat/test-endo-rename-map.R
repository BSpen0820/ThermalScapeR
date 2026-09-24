test_that("rename map covers every legacy leaf exactly once", {
  legacy <- readRDS(file.path(legacy_dir(), "defaults_j12.rds"))
  leaves <- setdiff(names(flatten_leaves(legacy)),
                    c("model_settings.julnum", "model_settings.juldays"))
  map <- legacy_rename_map()
  expect_setequal(leaves, map$old_path)
  expect_false(anyDuplicated(map$old_path) > 0)
  expect_false(anyDuplicated(map$new_path) > 0)
})

test_that("legacy_to_new renames nested lists and keeps element order", {
  legacy <- list(
    animal = list(mass = 5, mass2 = c(5, 5)),
    fur    = list(parts = list(leg = list(diad = 1)))
  )
  expect_identical(
    legacy_to_new(legacy),
    list(
      animal = list(body_mass = 5, mass_by_julday = c(5, 5)),
      fur    = list(parts = list(leg = list(hair_diameter_dorsal = 1)))
    )
  )
})
