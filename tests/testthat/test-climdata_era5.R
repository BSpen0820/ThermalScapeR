test_that(".era5_raw_point reads the station cell for all variables and times", {
  d <- withr::local_tempdir()
  make_era5_fixture(d, n_hours = 48)
  raw <- .era5_raw_point(d, lon = -150.4, lat = 60.7)
  expect_equal(nrow(raw), 48)
  expect_equal(names(raw), c("time", .era5_vars))
  expect_equal(raw$t2m[1], 280)
  expect_identical(attr(raw$time, "tzone"), "UTC")
})

test_that(".era5_raw_point ignores non-.nc files and folders, merges files, drops duplicate times", {
  d <- withr::local_tempdir()
  make_era5_fixture(d, n_hours = 48, name = "a.nc")
  make_era5_fixture(d, n_hours = 48, start = "2020-05-02 00:00", name = "b.nc")  # 24 h overlap
  dir.create(file.path(d, "Downloads")); writeLines("x", file.path(d, "x.zip"))
  raw <- .era5_raw_point(d, -150.4, 60.7)
  expect_equal(nrow(raw), 72)
  expect_false(anyDuplicated(raw$time) > 0)
  expect_false(is.unsorted(raw$time))
})

test_that(".era5_raw_point errors for no files, outside grid, missing variable", {
  empty <- withr::local_tempdir()
  expect_error(.era5_raw_point(empty, -150.4, 60.7), "No ERA5")
  d <- withr::local_tempdir(); make_era5_fixture(d)
  expect_error(.era5_raw_point(d, -100, 40), "outside")
})

test_that(".era5_to_climdata converts units", {
  raw <- data.frame(time = as.POSIXct("2020-05-01 01:00", tz = "UTC"),
                    t2m = 283.15, d2m = 283.15, sp = 101325, u10 = 3, v10 = 4,
                    tp = 0.002, avg_sdlwrf = 310, fdir = 3600 * 200, ssrd = 3600 * 500)
  o <- .era5_to_climdata(raw)
  expect_equal(names(o), c("obs_time", "temp", "relhum", "pres", "swdown", "difrad",
                           "lwdown", "windspeed", "winddir", "precip"))
  expect_equal(o$temp, 10)
  expect_equal(o$relhum, 100)                       # dewpoint == temp
  expect_equal(o$pres, 101.325)
  expect_equal(o$swdown, 500)
  expect_equal(o$difrad, 300)                       # horizontal direct is NOT re-projected
  expect_equal(o$lwdown, 310)
  expect_equal(o$windspeed, 5 * 0.7477849)
  expect_equal(o$winddir, (180 + atan2(3, 4) * 180 / pi) %% 360)
  expect_equal(o$precip, 2)
})

test_that(".era5_to_climdata clamps difrad >= 0 and relhum <= 100", {
  raw <- data.frame(time = as.POSIXct("2020-05-01", tz = "UTC"), t2m = 273.15,
                    d2m = 275, sp = 1e5, u10 = 0, v10 = 0, tp = 0, avg_sdlwrf = 300,
                    fdir = 3600 * 10, ssrd = 0)
  o <- .era5_to_climdata(raw)
  expect_equal(o$difrad, 0); expect_equal(o$relhum, 100)
})
