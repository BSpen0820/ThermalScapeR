t0 <- as.POSIXct("2020-05-01 00:00", tz = "UTC")

test_that("climdata frame is converted and subset", {
  m <- .lake_meteo(.test_climdata(t0, 48), -151, 60.5, t0 + 3600, t0 + 10 * 3600)
  expect_equal(nrow(m), 10L)
  expect_equal(m$datetime[1], t0 + 3600)
  expect_equal(m$Ten_Meter_Elevation_Wind_Speed_meterPerSecond[1], log(10/0.01) / log(2/0.01))
  expect_equal(m$Surface_Level_Barometric_Pressure_pascal[1], 1e5)
  expect_equal(m$Precipitation_millimeterPerDay[1], 24)
  expect_equal(names(m), c("datetime", "Ten_Meter_Elevation_Wind_Speed_meterPerSecond",
    "Air_Temperature_celsius", "Relative_Humidity_percent",
    "Shortwave_Radiation_Downwelling_wattPerMeterSquared",
    "Longwave_Radiation_Downwelling_wattPerMeterSquared",
    "Surface_Level_Barometric_Pressure_pascal", "Precipitation_millimeterPerDay"))
})

test_that("short gaps are interpolated, long gaps and short coverage stop", {
  cd <- .test_climdata(t0, 48); cd$temp[5:6] <- NA; cd$temp[4] <- 8; cd$temp[7] <- 14
  m <- .lake_meteo(cd, -151, 60.5, t0, t0 + 47 * 3600)
  expect_equal(m$Air_Temperature_celsius[5:6], c(10, 12))
  cd$temp[10:20] <- NA
  expect_error(.lake_meteo(cd, -151, 60.5, t0, t0 + 47 * 3600), "gap")
  expect_error(.lake_meteo(.test_climdata(t0, 48), -151, 60.5, t0, t0 + 100 * 3600), "2020-05-03 00:00")
})

test_that("packaged RDS is sampled at the nearest cell, wind unscaled", {
  f <- withr::local_tempfile(fileext = ".RDS")
  .test_packaged_clim(f, t0, 24)
  m <- .lake_meteo(f, -151.01, 60.51, t0, t0 + 23 * 3600)
  expect_equal(nrow(m), 24L)
  expect_equal(m$Air_Temperature_celsius[1], 1)
  expect_equal(m$Ten_Meter_Elevation_Wind_Speed_meterPerSecond[1], 1)
  expect_equal(m$Surface_Level_Barometric_Pressure_pascal[1], 1e5)
})

test_that("a missing first or last hour is filled from neighbours", {
  cd <- .test_climdata(t0, 48); cd$temp[1] <- NA; cd$temp[48] <- NA
  m <- .lake_meteo(cd, -151, 60.5, t0, t0 + 47 * 3600)
  expect_equal(m$Air_Temperature_celsius[c(1, 48)], c(10, 10))
  cd <- .test_climdata(t0, 48); cd$temp[10] <- NA
  m <- .lake_meteo(cd, -151, 60.5, t0 + 9 * 3600, t0 + 20 * 3600)
  expect_equal(m$Air_Temperature_celsius[1], 10)
})
