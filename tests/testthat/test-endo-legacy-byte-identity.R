read_bytes <- function(path) readBin(path, "raw", file.size(path))

expect_same_files <- function(new_dir, ref_dir) {
  for (f in c("endo.dat", "alomvars.dat", "JULDAYS.DAT"))
    expect_identical(read_bytes(file.path(new_dir, f)), read_bytes(file.path(ref_dir, f)), info = f)
}

test_that("scenario A (all defaults) reproduces the legacy files byte-for-byte", {
  skip_if_not(.Platform$OS.type == "windows", "legacy fixtures were captured on Windows (CRLF)")
  tmp <- tempfile("endo_a_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  d <- get_endotherm_defaults()
  do.call(write_endotherm_inputs, c(list(output_dir = tmp), d))
  write_juldays_dat(tmp, model_settings = d$model_settings)
  expect_same_files(tmp, file.path(legacy_dir(), "scenario_a"))
})

test_that("scenario B (stress inputs, julnum = 6, partial groups) reproduces the legacy files", {
  skip_if_not(.Platform$OS.type == "windows", "legacy fixtures were captured on Windows (CRLF)")
  saved <- readRDS(file.path(legacy_dir(), "scenario_b_args.rds"))
  args <- legacy_to_new(saved$inputs)
  tmp <- tempfile("endo_b_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  do.call(write_endotherm_inputs, c(list(output_dir = tmp), args))
  write_juldays_dat(tmp, model_settings = args$model_settings, habitat_settings = saved$habitat)
  expect_same_files(tmp, file.path(legacy_dir(), "scenario_b"))
})
