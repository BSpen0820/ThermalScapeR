make_crn <- function(start = "2020-05-01 00:05", n = 24, ...) {
  t <- seq(as.POSIXct(start, tz = "UTC"), by = "5 min", length.out = n)
  df <- data.frame(
    UTC_DATE = as.numeric(format(t, "%Y%m%d")),
    UTC_TIME = format(t, "%H%M"),
    AIR_TEMPERATURE = 5, RELATIVE_HUMIDITY = 80, SOLAR_RADIATION = 100,
    SR_FLAG = 0L, WIND_1_5 = 2, PRECIPITATION = 0.1, stringsAsFactors = FALSE
  )
  over <- list(...)
  for (nm in names(over)) df[[nm]] <- over[[nm]]
  df
}

test_that("5-min stamps in (H-1, H] are labelled H and aggregated", {
  h <- .crn_hourly(make_crn(n = 24))   # 00:05 .. 02:00 -> hours 01:00 and 02:00
  expect_equal(format(h$obs_time, "%H:%M"), c("01:00", "02:00"))
  expect_s3_class(h$obs_time, "POSIXct")
  expect_identical(attr(h$obs_time, "tzone"), "UTC")
  expect_equal(h$temp, c(5, 5))
  expect_equal(h$precip, c(1.2, 1.2))           # 12 x 0.1
  expect_equal(names(h), c("obs_time", "temp", "relhum", "swdown", "windspeed", "precip"))
})

test_that("hour with fewer than 9 valid values is NA", {
  temp <- rep(5, 24); temp[1:4] <- NA          # first hour: 8 valid
  h <- .crn_hourly(make_crn(AIR_TEMPERATURE = temp))
  expect_true(is.na(h$temp[1])); expect_equal(h$temp[2], 5)
})

test_that("precip is scaled up when 9-11 values are valid", {
  p <- rep(0.1, 24); p[1:2] <- NA              # first hour: 10 valid -> 10*0.1*12/10
  h <- .crn_hourly(make_crn(PRECIPITATION = p))
  expect_equal(h$precip[1], 1.2)
})

test_that("flagged shortwave and sentinels are missing", {
  h <- .crn_hourly(make_crn(SR_FLAG = c(rep(3L, 12), rep(0L, 12)),
                            AIR_TEMPERATURE = c(rep(-9999, 12), rep(5, 12))))
  expect_true(is.na(h$swdown[1])); expect_equal(h$swdown[2], 100)
  expect_true(is.na(h$temp[1]));   expect_equal(h$temp[2], 5)
})

test_that("missing 5-min rows do not shift hour bins", {
  d <- make_crn(n = 24)[-(3:6), ]              # drop 4 rows from first hour (8 left)
  h <- .crn_hourly(d)
  expect_equal(format(h$obs_time, "%H:%M"), c("01:00", "02:00"))
  expect_true(is.na(h$temp[1]))
})

test_that("missing columns error clearly", {
  expect_error(.crn_hourly(make_crn()[, c("UTC_DATE", "UTC_TIME")]), "missing column")
})

test_that("RH_FLAG and WIND_FLAG non-zero values are treated as missing", {
  h <- .crn_hourly(make_crn(RH_FLAG = c(rep(3L, 12), rep(0L, 12)),
                            WIND_FLAG = c(rep(0L, 12), rep(3L, 12))))
  expect_true(is.na(h$relhum[1])); expect_equal(h$relhum[2], 80)
  expect_equal(h$windspeed[1], 2); expect_true(is.na(h$windspeed[2]))
})
