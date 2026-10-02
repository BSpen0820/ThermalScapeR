make_station_crn <- function(n_hours = 30) {
  t <- seq(as.POSIXct("2020-05-01 00:05", tz = "UTC"), by = "5 min", length.out = n_hours * 12)
  data.frame(WBANNO = "26563", UTC_DATE = as.numeric(format(t, "%Y%m%d")),
             UTC_TIME = format(t, "%H%M"), LONGITUDE = -150.4, LATITUDE = 60.7,
             AIR_TEMPERATURE = 10, RELATIVE_HUMIDITY = 70, SOLAR_RADIATION = 100,
             SR_FLAG = 0L, WIND_1_5 = 2, PRECIPITATION = 0.1, stringsAsFactors = FALSE)
}

test_that("returns the ten climdata columns, complete, hourly UTC", {
  d <- withr::local_tempdir(); make_era5_fixture(d, n_hours = 48)
  out <- suppressMessages(build_climdata_crn_era5(make_station_crn(30), d))
  expect_equal(names(out), c("obs_time", "temp", "relhum", "pres", "swdown", "difrad",
                             "lwdown", "windspeed", "winddir", "precip", "filled"))
  expect_equal(nrow(out), 30)
  expect_false(anyNA(out[, setdiff(names(out), "filled")]))
  expect_identical(attr(out$obs_time, "tzone"), "UTC")
  expect_equal(as.numeric(diff(out$obs_time), units = "hours"), rep(1, 29))
})

test_that("flag_col = FALSE drops the filled column", {
  d <- withr::local_tempdir(); make_era5_fixture(d, n_hours = 48)
  out <- suppressMessages(build_climdata_crn_era5(make_station_crn(30), d, flag_col = FALSE))
  expect_false("filled" %in% names(out))
})

test_that("wind is scaled from wind_height to 2 m", {
  d <- withr::local_tempdir(); make_era5_fixture(d, n_hours = 48)
  a <- suppressMessages(build_climdata_crn_era5(make_station_crn(30), d, wind_height = 2))
  b <- suppressMessages(build_climdata_crn_era5(make_station_crn(30), d, wind_height = 1.5))
  expect_equal(a$windspeed[5], 2)
  expect_equal(b$windspeed[5], 2 * log(2 / 0.01) / log(1.5 / 0.01))
})

test_that("tme restricts the output period", {
  d <- withr::local_tempdir(); make_era5_fixture(d, n_hours = 48)
  tme <- seq(as.POSIXct("2020-05-01 05:00", tz = "UTC"), by = "hour", length.out = 10)
  out <- suppressMessages(build_climdata_crn_era5(make_station_crn(30), d, tme = tme))
  expect_equal(nrow(out), 10); expect_equal(out$obs_time[1], tme[1])
})

test_that("unfilled NA after merging is an error naming the columns", {
  d <- withr::local_tempdir(); make_era5_fixture(d, n_hours = 10)   # ERA5 ends early
  expect_error(suppressMessages(build_climdata_crn_era5(make_station_crn(30), d)),
               "NA.*pres")
})

test_that("input validation", {
  d <- withr::local_tempdir(); make_era5_fixture(d)
  crn <- make_station_crn(30); crn2 <- crn; crn2$WBANNO[1] <- "99999"
  expect_error(build_climdata_crn_era5(crn2, d), "more than one station")
  expect_error(build_climdata_crn_era5(crn, file.path(d, "nope")), "No ERA5")
})

test_that("low CRN coverage warns", {
  d <- withr::local_tempdir(); make_era5_fixture(d, n_hours = 48)
  crn <- make_station_crn(30); crn$AIR_TEMPERATURE[1:200] <- NA
  expect_warning(suppressMessages(build_climdata_crn_era5(crn, d)), "coverage")
})
