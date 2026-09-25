test_that("write_endotherm_inputs writes core-temperature columns in header order (core_temp_target, core_temp_max, core_temp_min)", {
  # endo.dat's row94 header reads "regul_T  max T  min T ..."; row96 must write
  # tcreg, tcmax, tcmin in that order to match. V3 of the upstream source script
  # wrote tcreg, tcmin, tcmax here (a mismatch with its own header) - V5 fixed
  # this, and this test locks in the fix.
  tmp_dir <- tempfile("endo_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  write_endotherm_inputs(output_dir = tmp_dir)

  # endo.dat has no trailing newline (ported verbatim from the source script);
  # suppress readLines' expected "incomplete final line" warning.
  lines <- suppressWarnings(readLines(file.path(tmp_dir, "endo.dat")))
  expect_match(lines[94], "regul_T\\s+max T\\s+min T")

  fields <- strsplit(trimws(lines[96]), "\t")[[1]]
  expect_equal(as.numeric(fields[1]), 38.6) # core_temp_target (default)
  expect_equal(as.numeric(fields[2]), 40.5) # core_temp_max (default), under "max T"
  expect_equal(as.numeric(fields[3]), 35.7) # core_temp_min (default), under "min T"
})

test_that("write_endotherm_inputs's core-temperature column order holds for custom overrides", {
  tmp_dir <- tempfile("endo_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  write_endotherm_inputs(
    output_dir = tmp_dir,
    physiology = list(core_temp_target = 38.0, core_temp_max = 39.0, core_temp_min = 35.0)
  )

  lines <- suppressWarnings(readLines(file.path(tmp_dir, "endo.dat")))
  fields <- strsplit(trimws(lines[96]), "\t")[[1]]
  expect_equal(as.numeric(fields[1]), 38.0) # core_temp_target
  expect_equal(as.numeric(fields[2]), 39.0) # core_temp_max, under "max T"
  expect_equal(as.numeric(fields[3]), 35.0) # core_temp_min, under "min T"
})
