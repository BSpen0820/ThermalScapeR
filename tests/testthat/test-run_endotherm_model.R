test_that("select native invocation on Windows via sysname works as expected", {
  # run_endotherm_model's native/Wine branch selection is exercised indirectly
  # by the integration test below (which runs on Windows); this test just
  # locks in the failure path, which is OS-independent.
  workspace <- tempfile("endo_ws_missing_")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE))

  result <- run_endotherm_model(workspace, exe_path = file.path(workspace, "Endo2022a.exe"), sysname = "Windows")

  expect_false(result$success)
  expect_match(result$message, "exe not found")
})

test_that("write_endotherm_inputs + write_juldays_dat + run_endotherm_model work end-to-end against the real exe", {
  skip_on_os(c("linux", "mac")) # this integration test only runs natively where the exe is a native binary
  fixtures_dir <- testthat::test_path("fixtures")
  skip_if_not(file.exists(file.path(fixtures_dir, "Endo2022a.exe")), "Endo2022a.exe fixture not present")
  workspace <- tempfile("endo_ws_")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE))

  file.copy(
    file.path(fixtures_dir, c("metout.csv", "shadmet.csv", "soil.csv", "shadsoil.csv")),
    workspace
  )

  write_endotherm_inputs(output_dir = workspace)
  write_juldays_dat(output_dir = workspace)

  result <- run_endotherm_model(workspace, exe_path = file.path(fixtures_dir, "Endo2022a.exe"), sysname = "Windows")

  expect_true(result$success)
  expect_match(result$message, "Calculations completed")
  expect_true(file.exists(file.path(workspace, "HOURPLOT.csv")))

  hourplot <- read.csv(file.path(workspace, "HOURPLOT.csv"), skip = 1)
  # MASS(KG) is an echo of the input mass (56.6, the write_endotherm_inputs default),
  # not a computed output - confirmed empirically against the real exe.
  expect_equal(hourplot[1, "MASS.KG."], 56.6, tolerance = 0.01)
})

test_that("a chunked sequence (varying juldays per chunk) runs successfully for two consecutive chunks", {
  skip_on_os(c("linux", "mac"))
  fixtures_dir <- testthat::test_path("fixtures")

  chunks <- chunk_days(total_days = 24, chunk_size = 12)
  expect_length(chunks, 2)

  for (chunk in chunks) {
    workspace <- tempfile("endo_chunk_ws_")
    dir.create(workspace)
    on.exit(unlink(workspace, recursive = TRUE), add = TRUE)

    file.copy(
      file.path(fixtures_dir, c("metout.csv", "shadmet.csv", "soil.csv", "shadsoil.csv")),
      workspace
    )

    write_endotherm_inputs(output_dir = workspace, model_settings = list(julnum = 12, juldays = chunk))
    write_juldays_dat(output_dir = workspace, model_settings = list(julnum = 12, juldays = chunk))

    result <- run_endotherm_model(workspace, exe_path = file.path(fixtures_dir, "Endo2022a.exe"), sysname = "Windows")
    expect_true(result$success)
  }
})

test_that("run_endotherm_model stops on non-Windows when headless = FALSE and DISPLAY is unset", {
  skip_on_os("windows")
  # exe_path must point at a file that exists, or run_endotherm_model()
  # returns early at its file.exists() check and this test's DISPLAY logic
  # never runs. Sys.which is mocked so the test doesn't depend on wine/
  # xvfb-run actually being installed on whatever machine runs it.
  testthat::local_mocked_bindings(
    Sys.which = function(x) stats::setNames(paste0("/usr/bin/", x), x)
  )
  old_display <- Sys.getenv("DISPLAY", unset = NA)
  Sys.unsetenv("DISPLAY")
  on.exit(if (!is.na(old_display)) Sys.setenv(DISPLAY = old_display), add = TRUE)

  workspace <- tempfile("endo_ws_nodisp_")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  fake_exe <- tempfile(fileext = ".exe")
  writeLines("not a real exe", fake_exe)
  on.exit(unlink(fake_exe), add = TRUE)

  expect_error(
    run_endotherm_model(workspace, exe_path = fake_exe, sysname = "Linux", headless = FALSE),
    "DISPLAY"
  )
})

test_that("run_endotherm_model stops when a shared wineprefix is not initialized", {
  skip_on_os("windows")
  testthat::local_mocked_bindings(
    Sys.which = function(x) stats::setNames(paste0("/usr/bin/", x), x)
  )
  workspace <- tempfile("endo_ws_uninit_")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  fake_exe <- tempfile(fileext = ".exe")
  writeLines("not a real exe", fake_exe)
  on.exit(unlink(fake_exe), add = TRUE)
  bad_prefix <- tempfile("uninit_prefix_")
  dir.create(bad_prefix)
  on.exit(unlink(bad_prefix, recursive = TRUE), add = TRUE)

  expect_error(
    run_endotherm_model(workspace, exe_path = fake_exe, sysname = "Linux",
                        wineprefix = bad_prefix, headless = TRUE),
    "init_wine_prefix"
  )
})

test_that("init_wine_prefix stops when wine is unavailable", {
  skip_on_os("windows")
  testthat::local_mocked_bindings(
    Sys.which = function(x) stats::setNames("", x)
  )
  expect_error(init_wine_prefix(tempfile("prefix_")), "wine")
})

test_that("init_wine_prefix stops when xvfb-run is unavailable but headless = TRUE", {
  skip_on_os("windows")
  testthat::local_mocked_bindings(
    Sys.which = function(x) if (identical(x, "wine")) "/usr/bin/wine" else stats::setNames("", x)
  )
  expect_error(init_wine_prefix(tempfile("prefix_"), headless = TRUE), "xvfb-run")
})

test_that("a native run leaves endo.dat intact and the exe reads it as ENDO.DAT", {
  skip_on_os("windows")
  ws <- file.path(tempfile("endo native ws "), "work dir")
  dir.create(ws, recursive = TRUE)
  on.exit(unlink(dirname(ws), recursive = TRUE))
  writeLines("endo contents", file.path(ws, "endo.dat"))

  res <- run_endotherm_model(ws, exe_path = .endo_fake_native_exe(ws))

  expect_true(res$success)
  expect_identical(readLines(file.path(ws, "endo.dat")), "endo contents")
  expect_identical(readLines(file.path(ws, "seen_ENDO.DAT")), "endo contents")
})

test_that("a stale ENDO.DAT on a case-sensitive filesystem is refreshed", {
  skip_on_os("windows")
  ws <- tempfile("endo_ws_")
  dir.create(ws)
  on.exit(unlink(ws, recursive = TRUE))
  writeLines("new", file.path(ws, "endo.dat"))
  writeLines("stale", file.path(ws, "ENDO.DAT"))
  skip_if(length(list.files(ws)) < 2L, "temp filesystem is case-insensitive")

  res <- run_endotherm_model(ws, exe_path = .endo_fake_native_exe(ws))

  expect_identical(readLines(file.path(ws, "seen_ENDO.DAT")), "new")
})

test_that("a failed native run reports the exe's console output", {
  skip_on_os("windows")
  ws <- tempfile("endo_ws_")
  dir.create(ws)
  on.exit(unlink(ws, recursive = TRUE))
  writeLines(character(0), file.path(ws, "endo.dat"))

  res <- run_endotherm_model(ws, exe_path = .endo_fake_native_exe(ws))

  expect_false(res$success)
  expect_match(res$message, "exit status 2")
  expect_match(res$message, "console: .*Fortran runtime error: End of file")
})

test_that("a native exe without execute permission fails with an explanation", {
  skip_on_os("windows")
  ws <- tempfile("endo_ws_")
  dir.create(ws)
  on.exit(unlink(ws, recursive = TRUE))
  writeLines("x", file.path(ws, "endo.dat"))
  exe <- .endo_fake_native_exe(ws)
  Sys.chmod(exe, "644")

  res <- run_endotherm_model(ws, exe_path = exe)

  expect_false(res$success)
  expect_match(res$message, "exit status 12[67]|[Pp]ermission denied")
})
