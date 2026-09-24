test_that(".default_mc_overrides returns the fixed 5-group override table", {
  ov <- .default_mc_overrides()
  expect_named(ov, c("model_settings", "physiology", "diet", "thermoreg", "flying_digging"))
  expect_equal(ov$model_settings$output_file_enabled, "Y")
  expect_equal(ov$model_settings$microclimate_input_format, "CSV")
  expect_equal(ov$model_settings$output_file_format, "CSV")
  expect_equal(ov$model_settings$stored_heat_enabled, "N")
  expect_equal(ov$physiology$sweating_enabled, "N")
  expect_equal(ov$physiology$piloerection_enabled, "N")
  expect_equal(ov$diet$activity_basal_multiple, rep(1.0, 12))
  expect_equal(ov$diet$reproduction_basal_multiple, rep(0.0, 12))
  expect_equal(ov$thermoreg$active_in_shade_enabled, "Y")
  expect_equal(ov$thermoreg$shade_posture, "S")
  expect_equal(ov$flying_digging$flight_enabled, "N")
})

test_that(".mc_scenario_overrides sets standing scenarios active all day with real posture/temp", {
  endo_inputs <- get_endotherm_defaults()
  ov <- .mc_scenario_overrides("standing_variable", endo_inputs)
  expect_equal(ov$diet$diurnal_enabled, rep("Y", 12))
  expect_equal(ov$diet$nocturnal_enabled, rep("Y", 12))
  expect_equal(ov$diet$crepuscular_enabled, rep("Y", 12))
  expect_equal(ov$physiology, list())
  expect_equal(ov$allometry, list())
})

test_that(".mc_scenario_overrides sets curled scenarios inactive all day with forced posture 3", {
  endo_inputs <- get_endotherm_defaults()
  ov <- .mc_scenario_overrides("curled_variable", endo_inputs)
  expect_equal(ov$diet$diurnal_enabled, rep("N", 12))
  expect_equal(ov$diet$nocturnal_enabled, rep("N", 12))
  expect_equal(ov$diet$crepuscular_enabled, rep("N", 12))
  expect_equal(ov$allometry$posture_3_enabled, "Y")
  expect_equal(ov$allometry$posture_4_enabled, "N")
  expect_equal(ov$allometry$sleep_start_posture, 3)
  expect_equal(ov$allometry$shade_start_posture, 3)
  expect_equal(ov$allometry$inactive_end_posture, 3)
  expect_equal(ov$physiology, list())
})

test_that(".mc_scenario_overrides narrows core temp to core_temp_target +/- 0.1 for constant scenarios", {
  endo_inputs <- get_endotherm_defaults()
  tcreg <- endo_inputs$physiology$core_temp_target
  ov <- .mc_scenario_overrides("curled_constant", endo_inputs)
  expect_equal(ov$physiology$core_temp_max, tcreg + 0.1)
  expect_equal(ov$physiology$core_temp_min, tcreg - 0.1)

  ov2 <- .mc_scenario_overrides("standing_constant", endo_inputs)
  expect_equal(ov2$physiology$core_temp_max, tcreg + 0.1)
  expect_equal(ov2$physiology$core_temp_min, tcreg - 0.1)
  expect_equal(ov2$allometry, list())
})

test_that(".mc_scenario_overrides leaves core_temp_max/core_temp_min untouched for variable scenarios", {
  endo_inputs <- get_endotherm_defaults()
  ov <- .mc_scenario_overrides("standing_variable", endo_inputs)
  expect_null(ov$physiology$core_temp_max)
  expect_null(ov$physiology$core_temp_min)
})

test_that(".mc_target_rmr uses the user-supplied metabolic rate when user_metabolic_rate_enabled is Y", {
  endo_inputs <- get_endotherm_defaults()
  endo_inputs$animal$user_metabolic_rate_enabled <- "Y"
  endo_inputs$animal$metabolic_rate <- 42
  expect_equal(.mc_target_rmr(endo_inputs), 42)
})

test_that(".mc_target_rmr computes the allometric formula for a non-marsupial mammal", {
  endo_inputs <- get_endotherm_defaults()
  endo_inputs$animal$user_metabolic_rate_enabled <- "N"
  endo_inputs$animal$taxon_class <- "MAMMAL"
  endo_inputs$animal$is_marsupial <- "N"
  endo_inputs$animal$body_mass <- 56.6
  expect_equal(.mc_target_rmr(endo_inputs), (70 * 56.6 ^ 0.75) * (4.185 / (24 * 3.6)))
})

test_that(".mc_target_rmr computes the marsupial formula variant", {
  endo_inputs <- get_endotherm_defaults()
  endo_inputs$animal$user_metabolic_rate_enabled <- "N"
  endo_inputs$animal$taxon_class <- "MAMMAL"
  endo_inputs$animal$is_marsupial <- "Y"
  endo_inputs$animal$body_mass <- 10
  expect_equal(.mc_target_rmr(endo_inputs), (2187 * 10 ^ 0.737) * (4.185 / 3600))
})

test_that(".mc_target_rmr warns and returns NA for a non-MAMMAL class", {
  endo_inputs <- get_endotherm_defaults()
  endo_inputs$animal$user_metabolic_rate_enabled <- "N"
  endo_inputs$animal$taxon_class <- "BIRDIE"
  expect_warning(result <- .mc_target_rmr(endo_inputs), "MAMMAL")
  expect_true(is.na(result))
})

test_that(".mc_parse_dimensions extracts a 6-row body-part dimension table from a fake OUTPUT file", {
  tmp <- tempfile("OUTPUT_")
  lines <- rep("", 136)
  # Column 1 = label (ignored/overwritten for rows 4-6), columns 6:8 = the
  # three dimensions .mc_parse_dimensions keeps (dim_table[, c(1, 6:8)]).
  lines[131] <- "Head 0 0 0 0 0.10 0.11 0.12"
  lines[132] <- "Neck 0 0 0 0 0.20 0.21 0.22"
  lines[133] <- "Torso 0 0 0 0 0.30 0.31 0.32"
  lines[134] <- "Leg1 0 0 0 0 0.40 0.41 0.42"
  lines[135] <- "Leg2 0 0 0 0 0.50 0.51 0.52"
  lines[136] <- "Tail 0 0 0 0 0.60 0.61 0.62"
  # Column 1 = ignored, columns 11:16 = the six masses (t(mass_table[11:16])).
  lines[129] <- "x x x x x x x x x x 1.1 2.2 3.3 4.4 5.5 6.6"
  writeLines(lines, tmp)
  on.exit(unlink(tmp))

  dims <- .mc_parse_dimensions(tmp)

  expect_equal(nrow(dims), 6)
  expect_named(dims, c("Body Part", "Vertical or Side-to-Side Diameter (m)",
                        "Horizontal or Front-to-Back Diameter (m)", "Length (m)", "Mass (kg)"))
  expect_equal(dims[["Body Part"]], c("Head", "Neck", "Torso", "Front Legs",
                                       "Rear Legs", "6th Appendage (Tail/Proboscis)"))
  expect_equal(dims[["Vertical or Side-to-Side Diameter (m)"]], c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60))
  expect_equal(dims[["Mass (kg)"]], c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6))
})
