test_that("run_metabolic_chamber errors on an invalid scenario ID", {
  expect_error(
    run_metabolic_chamber(get_endotherm_defaults(), exe_path = tempfile(), scenarios = "not_a_scenario"),
    "scenarios"
  )
})

test_that("run_metabolic_chamber errors when exe_path does not exist", {
  expect_error(
    run_metabolic_chamber(get_endotherm_defaults(), exe_path = tempfile("missing_exe_")),
    "exe_path"
  )
})

test_that("run_metabolic_chamber errors when save_dir does not exist", {
  fake_exe <- tempfile(fileext = ".exe")
  writeLines("not a real exe", fake_exe)
  on.exit(unlink(fake_exe))
  expect_error(
    run_metabolic_chamber(get_endotherm_defaults(), exe_path = fake_exe, save_dir = tempfile("missing_dir_")),
    "save_dir"
  )
})

test_that("run_metabolic_chamber errors when scenarios is character(0)", {
  fake_exe <- tempfile(fileext = ".exe")
  writeLines("not a real exe", fake_exe)
  on.exit(unlink(fake_exe))
  expect_error(
    run_metabolic_chamber(get_endotherm_defaults(), exe_path = fake_exe, scenarios = character(0)),
    "scenarios"
  )
})

test_that("run_metabolic_chamber errors when endo_inputs is missing required group(s)", {
  fake_exe <- tempfile(fileext = ".exe")
  writeLines("not a real exe", fake_exe)
  on.exit(unlink(fake_exe))
  bad_inputs <- get_endotherm_defaults()
  bad_inputs$flying_digging <- NULL
  expect_error(
    run_metabolic_chamber(bad_inputs, exe_path = fake_exe),
    "flying_digging"
  )
})

test_that("run_metabolic_chamber messages when endo_inputs$model_settings$julnum is not 12", {
  fake_exe <- tempfile(fileext = ".exe")
  writeLines("not a real exe", fake_exe)
  on.exit(unlink(fake_exe))

  testthat::local_mocked_bindings(
    run_endotherm_model = function(workspace_dir, exe_path, sysname, ...) {
      list(success = FALSE, message = "simulated failure")
    }
  )

  bad_inputs <- get_endotherm_defaults(julnum = 6, juldays = c(15, 45, 74, 105, 135, 166))
  expect_message(
    try(run_metabolic_chamber(bad_inputs, exe_path = fake_exe, scenarios = "standing_variable"), silent = TRUE),
    "julnum = 12"
  )
})

test_that("run_metabolic_chamber passes headless and wineprefix through to run_endotherm_model", {
  fake_exe <- tempfile(fileext = ".exe")
  writeLines("not a real exe", fake_exe)
  on.exit(unlink(fake_exe))

  seen <- list()
  testthat::local_mocked_bindings(
    run_endotherm_model = function(workspace_dir, exe_path, sysname,
                                   wineprefix = NULL, headless = FALSE) {
      seen <<- list(wineprefix = wineprefix, headless = headless)
      list(success = FALSE, message = "simulated failure")
    }
  )

  suppressWarnings(try(
    run_metabolic_chamber(get_endotherm_defaults(), exe_path = fake_exe,
                          scenarios = "standing_variable",
                          headless = TRUE, wineprefix = "/shared/prefix"),
    silent = TRUE
  ))

  expect_true(seen$headless)
  expect_equal(seen$wineprefix, "/shared/prefix")
})

test_that("run_metabolic_chamber computes target_rmr before running any scenario", {
  fake_exe <- tempfile(fileext = ".exe")
  writeLines("not a real exe", fake_exe)
  on.exit(unlink(fake_exe))

  bad_inputs <- get_endotherm_defaults()
  bad_inputs$animal$user_metabolic_rate_enabled <- "N"
  bad_inputs$animal$taxon_class <- "BIRDIE"

  events <- character(0)
  testthat::local_mocked_bindings(
    run_endotherm_model = function(workspace_dir, exe_path, sysname, ...) {
      events <<- c(events, "exe_invoked")
      list(success = FALSE, message = "simulated failure")
    }
  )

  withCallingHandlers(
    try(
      run_metabolic_chamber(bad_inputs, exe_path = fake_exe, scenarios = "standing_variable"),
      silent = TRUE
    ),
    warning = function(w) {
      if (grepl("MAMMAL", conditionMessage(w))) events <<- c(events, "mammal_warning")
      invokeRestart("muffleWarning")
    }
  )

  # the MAMMAL warning (from .mc_target_rmr, hoisted before the loop) must
  # fire before the first (mocked) exe invocation, not after 4 real runs.
  expect_equal(events, c("mammal_warning", "exe_invoked"))
})

# --- Mocked exe-free tests: exercise the warn+skip / stop-on-all-fail control
# flow without needing the real Endotherm exe. run_endotherm_model() is
# mocked; the fake "success" path writes minimal fake HOURPLOT.csv/OUTPUT
# files into workspace_dir itself so run_metabolic_chamber()'s downstream
# parsing has something valid to read.

.write_fake_hourplot <- function(path) {
  con <- file(path, "w")
  on.exit(close(con))
  writeLines("fake species comment line", con)
  writeLines("MET(W),TAIR", con)
  for (i in 1:96) writeLines(sprintf("%.1f,%.1f", 10 + i * 0.1, -40 + i), con)
}

.write_fake_output <- function(path) {
  lines <- rep("", 136)
  lines[131] <- "Head 0 0 0 0 0.10 0.11 0.12"
  lines[132] <- "Neck 0 0 0 0 0.20 0.21 0.22"
  lines[133] <- "Torso 0 0 0 0 0.30 0.31 0.32"
  lines[134] <- "Leg1 0 0 0 0 0.40 0.41 0.42"
  lines[135] <- "Leg2 0 0 0 0 0.50 0.51 0.52"
  lines[136] <- "Tail 0 0 0 0 0.60 0.61 0.62"
  lines[129] <- "x x x x x x x x x x 1.1 2.2 3.3 4.4 5.5 6.6"
  writeLines(lines, path)
}

test_that("run_metabolic_chamber stops when every requested scenario fails", {
  fake_exe <- tempfile(fileext = ".exe")
  writeLines("not a real exe", fake_exe)
  on.exit(unlink(fake_exe))

  testthat::local_mocked_bindings(
    run_endotherm_model = function(workspace_dir, exe_path, sysname, ...) {
      list(success = FALSE, message = "simulated failure")
    }
  )

  expect_error(
    run_metabolic_chamber(get_endotherm_defaults(), exe_path = fake_exe,
                           scenarios = c("standing_variable", "curled_variable")),
    "every requested scenario failed"
  )
})

test_that("run_metabolic_chamber warns and continues when one scenario fails but another succeeds", {
  fake_exe <- tempfile(fileext = ".exe")
  writeLines("not a real exe", fake_exe)
  on.exit(unlink(fake_exe))

  call_count <- 0
  testthat::local_mocked_bindings(
    run_endotherm_model = function(workspace_dir, exe_path, sysname, ...) {
      call_count <<- call_count + 1
      if (call_count == 1) {
        return(list(success = FALSE, message = "simulated failure"))
      }
      .write_fake_hourplot(file.path(workspace_dir, "HOURPLOT.csv"))
      .write_fake_output(file.path(workspace_dir, "OUTPUT"))
      list(success = TRUE, message = "Calculations completed.")
    }
  )

  expect_warning(
    result <- run_metabolic_chamber(get_endotherm_defaults(), exe_path = fake_exe,
                           scenarios = c("standing_variable", "curled_variable")),
    "standing_variable"
  )

  expect_s3_class(result, "metchamber_result")
  expect_length(result$hourplot, 1)
  expect_named(result$hourplot, "curled_variable")
  expect_equal(nrow(result$log), 2)
  expect_equal(result$log$success, c(FALSE, TRUE))
  expect_equal(nrow(result$dimensions), 6)
})

test_that("run_metabolic_chamber's save_dir option copies each scenario's endo.dat/alomvars.dat", {
  fake_exe <- tempfile(fileext = ".exe")
  writeLines("not a real exe", fake_exe)
  on.exit(unlink(fake_exe))
  save_dir <- tempfile("mc_save_")
  dir.create(save_dir)
  on.exit(unlink(save_dir, recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    run_endotherm_model = function(workspace_dir, exe_path, sysname, ...) {
      .write_fake_hourplot(file.path(workspace_dir, "HOURPLOT.csv"))
      .write_fake_output(file.path(workspace_dir, "OUTPUT"))
      list(success = TRUE, message = "Calculations completed.")
    }
  )

  run_metabolic_chamber(get_endotherm_defaults(), exe_path = fake_exe,
                         scenarios = "standing_variable", save_dir = save_dir)

  expect_true(file.exists(file.path(save_dir, "standing_variable_endo.dat")))
  expect_true(file.exists(file.path(save_dir, "standing_variable_alomvars.dat")))
})

test_that("run_metabolic_chamber cleans up all scenario temp directories, not just the last", {
  fake_exe <- tempfile(fileext = ".exe")
  writeLines("not a real exe", fake_exe)
  on.exit(unlink(fake_exe))

  testthat::local_mocked_bindings(
    run_endotherm_model = function(workspace_dir, exe_path, sysname, ...) {
      .write_fake_hourplot(file.path(workspace_dir, "HOURPLOT.csv"))
      .write_fake_output(file.path(workspace_dir, "OUTPUT"))
      list(success = TRUE, message = "Calculations completed.")
    }
  )

  before <- list.files(tempdir(), pattern = "^mc_")
  run_metabolic_chamber(get_endotherm_defaults(), exe_path = fake_exe,
                         scenarios = c("standing_variable", "curled_variable",
                                       "curled_constant", "standing_constant"))
  after <- list.files(tempdir(), pattern = "^mc_")

  expect_equal(after, before)
})

# --- Real-exe integration test, mirrors test-run_endotherm_model.R's pattern.
test_that("run_metabolic_chamber runs all 4 real scenarios end-to-end", {
  skip_on_os(c("linux", "mac"))
  fixtures_dir <- testthat::test_path("fixtures")
  exe_src <- file.path(fixtures_dir, "Endo2022a.exe")
  skip_if_not(file.exists(exe_src), "Endo2022a.exe fixture not present")

  result <- run_metabolic_chamber(get_endotherm_defaults(), exe_path = exe_src)

  expect_s3_class(result, "metchamber_result")
  expect_length(result$hourplot, 4)
  expect_setequal(names(result$hourplot), .mc_scenario_ids)
  expect_true(all(result$log$success))
  expect_equal(nrow(result$dimensions), 6)
  expect_true(is.numeric(result$target_rmr$trgt))
  for (hp in result$hourplot) {
    expect_equal(nrow(hp), 86) # length(c(2:23, 27:48, 52:73, 77:96))
    expect_true("MET.W." %in% names(hp))
    expect_true("TAIR" %in% names(hp))
  }
})
