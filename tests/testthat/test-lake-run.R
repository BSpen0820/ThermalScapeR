fake_ler <- function(lake, meteo, start, stop, models, ...) {
  tm <- seq(start, stop, by = 3600)
  stats::setNames(lapply(models, function(m) data.frame(datetime = tm, temp = 10, ice_height = 0)), models)
}

setup_run <- function() {
  lk <- .test_square_lake(200)
  b  <- generate_lake_bathymetry(lk, max_depth = 4.572)
  dem <- terra::rast(terra::ext(b), res = 30, crs = terra::crs(b), vals = 60)
  lc <- terra::rast(terra::ext(b), res = 30, crs = terra::crs(b), vals = 11)
  list(p = prepare_lakes(b, dem, lakes = lk, id_col = "lake_id"), lc = lc,
       cd = .test_climdata(as.POSIXct("2019-10-01", tz = "UTC"), 24 * 400))
}

test_that("writes named outputs and a log", {
  local_mocked_bindings(.ler_run_lake = fake_ler)
  s <- setup_run(); od <- withr::local_tempdir()
  log <- run_lake_watertemp(s$p, s$cd, as.Date(c("2020-05-01", "2020-05-01")), s$lc, 11, od,
                            Kw = 1, models = c("FLake", "GLM"), spinup_clim = s$cd, study_area = "Kenai")
  f <- file.path(od, "Kenai_WaterTemp_20200501_to_20200501.nc")
  expect_true(file.exists(f))
  expect_true(file.exists(file.path(od, "Kenai_WaterTemp_Lakes_20200501_to_20200501.csv")))
  wt <- terra::rast(f, subds = "water_temp")
  expect_equal(terra::nlyr(wt), 31L * 24L)
  expect_equal(log$models_ok, "FLake,GLM")
  expect_false(dir.exists(file.path(od, "LER_runs")))
})

test_that("spin-up starts 1 Oct of prior year and is trimmed from output", {
  starts <- NULL
  local_mocked_bindings(.ler_run_lake = function(lake, meteo, start, stop, models, ...) {
    starts <<- start; fake_ler(lake, meteo, start, stop, models)
  })
  s <- setup_run()
  run_lake_watertemp(s$p, s$cd, as.Date(c("2020-05-01", "2020-05-01")), s$lc, 11,
                     withr::local_tempdir(), Kw = 1, models = "FLake", spinup_clim = s$cd)
  expect_equal(starts, as.POSIXct("2019-10-01", tz = "UTC"))
})

test_that("Kw missing for a lake stops naming it", {
  s <- setup_run()
  expect_error(run_lake_watertemp(s$p, s$cd, as.Date(c("2020-05-01", "2020-05-01")), s$lc, 11,
                                  withr::local_tempdir(), Kw = c(Z = 1)), "A")
})

test_that("no spin-up climate gives a message and starts at the period", {
  local_mocked_bindings(.ler_run_lake = fake_ler)
  s <- setup_run()
  expect_message(run_lake_watertemp(s$p, s$cd, as.Date(c("2020-05-01", "2020-05-01")), s$lc, 11,
                                    withr::local_tempdir(), Kw = 1, models = "FLake"), "spin-up")
})

test_that("a lake where every model fails stops", {
  local_mocked_bindings(.ler_run_lake = function(...) list())
  s <- setup_run()
  expect_error(run_lake_watertemp(s$p, s$cd, as.Date(c("2020-05-01", "2020-05-01")), s$lc, 11,
                                  withr::local_tempdir(), Kw = 1, models = "FLake"),
               "all models failed")
})

test_that("fewer than two successful models is warned and logged", {
  local_mocked_bindings(.ler_run_lake = function(lake, meteo, start, stop, models, ...)
    fake_ler(lake, meteo, start, stop, "FLake"))
  s <- setup_run()
  expect_warning(log <- run_lake_watertemp(s$p, s$cd, as.Date(c("2020-05-01", "2020-05-01")), s$lc, 11,
                                           withr::local_tempdir(), Kw = 1, models = c("FLake", "GLM"),
                                           spinup_clim = s$cd), "1 of 2")
  expect_equal(log$n_models, 1L)
  expect_equal(log$models_failed, "GLM")
})
