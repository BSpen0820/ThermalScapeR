test_that("endo_timevar_template returns the expected field names, all NULL", {
  tv <- endo_timevar_template()
  expect_named(tv, c(
    "mass_by_julday", "body_fat_pct_by_julday", "core_temp_target_by_julday",
    "torso_hair_length_dorsal_by_julday", "torso_hair_length_ventral_by_julday",
    "torso_fur_depth_dorsal_by_julday", "torso_fur_depth_ventral_by_julday",
    "digestive_efficiency", "activity_basal_multiple", "reproduction_basal_multiple",
    "food_protein_frac", "food_fat_frac", "food_carb_frac", "food_dry_matter_frac",
    "diurnal_enabled", "nocturnal_enabled", "crepuscular_enabled",
    "hibernate_enabled", "hibernation_day_frac",
    "active_land_or_water", "inactive_land_or_water"
  ))
  expect_true(all(vapply(tv, is.null, logical(1))))
})
