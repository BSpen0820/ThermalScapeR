kenai_fixture <- function() testthat::test_path("fixtures", "crn_kenai_2024_sample.txt")

test_that(".crn_subhourly_cols has the 23 CRN sub-hourly names in order", {
  expect_length(.crn_subhourly_cols, 23L)
  expect_identical(.crn_subhourly_cols[c(1, 5, 9, 11, 23)],
                   c("WBANNO", "LST_TIME", "AIR_TEMPERATURE", "SOLAR_RADIATION", "WIND_FLAG"))
})

test_that(".crn_read_file keeps date/time columns as text and types the rest", {
  raw <- .crn_read_file(kenai_fixture())
  expect_equal(nrow(raw), 10L)
  expect_named(raw, .crn_subhourly_cols)
  expect_type(raw$LST_TIME, "character")
  expect_true("0005" %in% raw$UTC_TIME)          # zero padding survives
  expect_type(raw$WBANNO, "character")
  expect_type(raw$CRX_VN, "character")
  expect_type(raw$AIR_TEMPERATURE, "double")
  expect_type(raw$SR_FLAG, "integer")
  expect_equal(nrow(readr::problems(raw)), 0L)
})

test_that(".crn_read_file warns on malformed rows", {
  path <- withr::local_tempfile(fileext = ".txt")
  lines <- crn_test_lines("2024-01-01 00:05", "2024-01-01 00:20")
  lines[2] <- sub(" -5.0 ", " abc ", lines[2], fixed = TRUE)
  writeLines(lines, path)
  expect_warning(.crn_read_file(path), "Parsing problems")
})

test_that(".crn_lst_tz derives the Etc/GMT zone from the data", {
  raw <- .crn_read_file(kenai_fixture())
  expect_identical(.crn_lst_tz(raw), "Etc/GMT+9")

  p <- withr::local_tempfile(fileext = ".txt")
  crn_test_file(p, "2024-01-01 00:05", "2024-01-01 01:00", offset_h = 9)
  expect_identical(.crn_lst_tz(.crn_read_file(p)), "Etc/GMT-9")
  crn_test_file(p, "2024-01-01 00:05", "2024-01-01 01:00", offset_h = 0)
  expect_identical(.crn_lst_tz(.crn_read_file(p)), "Etc/GMT+0")
})

test_that("real Kenai rows parse to a valid Date_Time with correct LST midnight semantics", {
  raw <- .crn_read_file(kenai_fixture())
  tz <- .crn_lst_tz(raw)
  dt <- as.POSIXct(paste(raw$LST_DATE, raw$LST_TIME), format = "%Y%m%d %H%M", tz = tz)
  expect_false(anyNA(dt))
  # first file row: UTC 2024-01-01 00:05 -> LST 2023-12-31 15:05
  expect_identical(raw$LST_DATE[1], "20231231")
  expect_identical(format(dt[1], "%Y-%m-%d %H:%M"), "2023-12-31 15:05")
  # the 0000 row carries the NEW calendar date
  i <- which(raw$LST_TIME == "0000")
  expect_length(i, 1L)
  expect_identical(raw$LST_DATE[i], "20240101")
  expect_equal(dt[i], as.POSIXct("2024-01-01 00:00:00", tz = "Etc/GMT+9"))
  # ...and it directly follows 23:55 of the previous day
  expect_equal(as.numeric(difftime(dt[i], dt[i - 1], units = "mins")), 5)
})

test_that(".crn_mask_sentinels applies the per-column sentinel table", {
  p <- withr::local_tempfile(fileext = ".txt")
  crn_test_file(p, "2024-01-01 00:05", "2024-01-01 00:20",
                vals = list(AIR_TEMPERATURE = "-9999.0", SOLAR_RADIATION = "-99999",
                            SOIL_MOISTURE_5 = "-99.000", WIND_1_5 = "-99.000",
                            RELATIVE_HUMIDITY = "-9999", WETNESS = "-9999"))
  m <- .crn_mask_sentinels(as.data.frame(.crn_read_file(p)))
  for (col in c("AIR_TEMPERATURE", "SOLAR_RADIATION", "SOIL_MOISTURE_5", "WIND_1_5",
                "RELATIVE_HUMIDITY", "WETNESS"))
    expect_true(all(is.na(m[[col]])), info = col)
  expect_false(anyNA(m$PRECIPITATION))
  expect_false(anyNA(m$SURFACE_TEMPERATURE))
})

test_that(".crn_finalize keeps (start 00:00, end+1 00:00] with 288 rows per day", {
  p <- withr::local_tempfile(fileext = ".txt")
  crn_test_file(p, "2023-12-31 12:00", "2024-01-05 12:00")
  raw <- .crn_read_file(p)
  df <- .crn_finalize(raw, as.Date("2024-01-01"), as.Date("2024-01-03"),
                      "Etc/GMT+9", TRUE, "20240101_to_20240103")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 864L)
  expect_identical(format(min(df$Date_Time), "%Y-%m-%d %H:%M"), "2024-01-01 00:05")
  expect_identical(format(max(df$Date_Time), "%Y-%m-%d %H:%M"), "2024-01-04 00:00")
  expect_identical(attr(df$Date_Time, "tzone"), "Etc/GMT+9")
  expect_false(is.unsorted(df$Date_Time))
  expect_named(df, c(.crn_subhourly_cols, "Date_Time"))
})

test_that(".crn_finalize returns exactly 288 rows for a single-day period", {
  p <- withr::local_tempfile(fileext = ".txt")
  crn_test_file(p, "2024-01-01 12:00", "2024-01-04 12:00")
  df <- .crn_finalize(.crn_read_file(p), as.Date("2024-01-02"), as.Date("2024-01-02"),
                      "Etc/GMT+9", TRUE, "20240102_to_20240102")
  expect_equal(nrow(df), 288L)
  expect_identical(format(min(df$Date_Time), "%Y-%m-%d %H:%M"), "2024-01-02 00:05")
  expect_identical(format(max(df$Date_Time), "%Y-%m-%d %H:%M"), "2024-01-03 00:00")
})

test_that(".crn_finalize keeps sentinels when missing_to_na is FALSE", {
  p <- withr::local_tempfile(fileext = ".txt")
  crn_test_file(p, "2023-12-31 12:00", "2024-01-03 12:00",
                vals = list(AIR_TEMPERATURE = "-9999.0"))
  df <- .crn_finalize(.crn_read_file(p), as.Date("2024-01-01"), as.Date("2024-01-01"),
                      "Etc/GMT+9", FALSE, "x")
  expect_true(all(df$AIR_TEMPERATURE == -9999))
  df2 <- .crn_finalize(.crn_read_file(p), as.Date("2024-01-01"), as.Date("2024-01-01"),
                       "Etc/GMT+9", TRUE, "x")
  expect_true(all(is.na(df2$AIR_TEMPERATURE)))
})

test_that(".crn_finalize warns on a row shortfall and on all-NA sensor columns", {
  p <- withr::local_tempfile(fileext = ".txt")
  crn_test_file(p, "2024-01-01 06:00", "2024-01-04 00:00")
  expect_warning(
    .crn_finalize(.crn_read_file(p), as.Date("2024-01-01"), as.Date("2024-01-03"),
                  "Etc/GMT+9", TRUE, "20240101_to_20240103"),
    "expected 864")

  crn_test_file(p, "2023-12-31 12:00", "2024-01-03 12:00",
                vals = list(SOLAR_RADIATION = "-99999", WIND_1_5 = "-99.000"))
  expect_warning(
    .crn_finalize(.crn_read_file(p), as.Date("2024-01-01"), as.Date("2024-01-01"),
                  "Etc/GMT+9", TRUE, "20240101_to_20240101"),
    "SOLAR_RADIATION, WIND_1_5")
})

test_that(".crn_finalize stops when nothing falls inside the window", {
  p <- withr::local_tempfile(fileext = ".txt")
  crn_test_file(p, "2024-02-01 00:05", "2024-02-01 06:00")
  expect_error(
    .crn_finalize(.crn_read_file(p), as.Date("2024-01-01"), as.Date("2024-01-03"),
                  "Etc/GMT+9", TRUE, "20240101_to_20240103"),
    "no observations")
})
