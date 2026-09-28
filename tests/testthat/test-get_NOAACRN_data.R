# Mocks the three network seams. Year files are synthetic: each covers a fixed
# LST slab so tests stay small. `downloads` records every URL requested.
mock_crn_network <- function(env, listing_years = 2023:2025,
                             kenai_years = 2023:2025,
                             slabs = list(
                               "2023" = c("2023-12-29 12:00", "2023-12-31 15:00"),
                               "2024" = c("2023-12-31 15:05", "2024-01-12 12:00"),
                               "2025" = c("2025-01-01 00:05", "2025-01-02 00:00")),
                             vals = list(), .frame = parent.frame()) {
  env$urls <- character()
  env$dests <- character()
  testthat::local_mocked_bindings(
    .crn_stations = function() .crn_parse_stations(crn_stations_fixture()),
    .crn_year_listing = function(year) {
      if (year %in% kenai_years) c("AK_Kenai_29_ENE", "AK_Farther_9_S") else character()
    },
    .crn_download = function(url, dest, retries = 1) {
      env$urls <- c(env$urls, url)
      env$dests <- c(env$dests, dest)
      yr <- sub(".*/subhourly01/([0-9]{4})/.*", "\\1", url)
      slab <- slabs[[yr]]
      if (is.null(slab)) return("not_found")
      crn_test_file(dest, slab[1], slab[2], vals = vals)
      "ok"
    },
    .env = .frame,
    .package = "ThermalScapeR"
  )
}

test_that("a Date vector returns one data.frame with headers and Date_Time", {
  env <- new.env()
  mock_crn_network(env)
  out <- suppressMessages(get_NOAACRN_data(
    station = "AK_Kenai_29_ENE", dates = as.Date(c("2024-01-02", "2024-01-04"))))
  expect_s3_class(out, "data.frame")
  expect_false(inherits(out, "list") && !is.data.frame(out))
  expect_equal(nrow(out), 864L)
  expect_named(out, c(.crn_subhourly_cols, "Date_Time"))
  expect_identical(attr(out$Date_Time, "tzone"), "Etc/GMT+9")
  expect_identical(format(max(out$Date_Time), "%Y-%m-%d %H:%M"), "2024-01-05 00:00")
})

test_that("a data.frame of dates returns a named list, even for one row", {
  env <- new.env()
  mock_crn_network(env)
  d2 <- data.frame(Start_Dates = as.Date(c("2024-01-02", "2024-01-06")),
                   End_Dates   = as.Date(c("2024-01-03", "2024-01-07")))
  out <- suppressMessages(get_NOAACRN_data(station = "Kenai", dates = d2))
  expect_type(out, "list")
  expect_named(out, c("20240102_to_20240103", "20240106_to_20240107"))
  expect_equal(vapply(out, nrow, integer(1), USE.NAMES = FALSE), c(576L, 576L))

  d1 <- d2[1, ]
  out1 <- suppressMessages(get_NOAACRN_data(station = "Kenai", dates = d1))
  expect_type(out1, "list")
  expect_false(is.data.frame(out1))
  expect_named(out1, "20240102_to_20240103")
})

test_that("each year file is downloaded once across periods, and cleaned up", {
  env <- new.env()
  mock_crn_network(env)
  d2 <- data.frame(Start_Dates = as.Date(c("2024-01-02", "2024-01-06")),
                   End_Dates   = as.Date(c("2024-01-03", "2024-01-07")))
  suppressMessages(get_NOAACRN_data(station = "Kenai", dates = d2))
  file_urls <- grep("CRNS0101-05", env$urls, value = TRUE)
  expect_length(file_urls, 1L)
  expect_false(dir.exists(dirname(env$dests[1])))
})

test_that("a Dec 31 end pulls the next UTC-year file and is gap-free across it", {
  env <- new.env()
  mock_crn_network(env)
  out <- suppressMessages(get_NOAACRN_data(
    station = "AK_Kenai_29_ENE", dates = as.Date(c("2023-12-30", "2023-12-31"))))
  expect_equal(nrow(out), 576L)
  expect_identical(format(min(out$Date_Time), "%Y-%m-%d %H:%M"), "2023-12-30 00:05")
  expect_identical(format(max(out$Date_Time), "%Y-%m-%d %H:%M"), "2024-01-01 00:00")
  steps <- as.numeric(diff(out$Date_Time), units = "mins")
  expect_true(all(steps == 5))
  expect_true(any(grepl("/2024/CRNS0101-05-2024-", env$urls)))
})

test_that("a missing padding-only year is silent", {
  env <- new.env()
  mock_crn_network(env, slabs = list(
    "2023" = c("2023-12-29 12:00", "2024-01-01 12:00")))
  expect_no_warning(suppressMessages(get_NOAACRN_data(
    station = "AK_Kenai_29_ENE", dates = as.Date(c("2023-12-30", "2023-12-31")))))
})

test_that("coords picks the nearest station with data and messages the choice", {
  env <- new.env()
  mock_crn_network(env)
  expect_message(
    out <- get_NOAACRN_data(coords = c(-150.44, 60.72),
                            dates = as.Date(c("2024-01-02", "2024-01-02"))),
    "AK_Kenai_29_ENE")
  expect_equal(nrow(out), 288L)
})

test_that("coords warns when the chosen station is beyond max_dist_km", {
  env <- new.env()
  mock_crn_network(env)
  expect_warning(
    suppressMessages(get_NOAACRN_data(coords = c(-100, 40),
                                      dates = as.Date(c("2024-01-02", "2024-01-02")),
                                      max_dist_km = 100)),
    "km away")
})

test_that("a station present in only some requested years warns; in none it stops", {
  env <- new.env()
  mock_crn_network(env, kenai_years = 2023)
  expect_warning(
    suppressMessages(get_NOAACRN_data(
      station = "AK_Kenai_29_ENE", dates = as.Date(c("2023-12-30", "2024-01-02")))),
    "2024")

  mock_crn_network(env, kenai_years = integer())
  expect_error(
    suppressMessages(get_NOAACRN_data(
      station = "AK_Kenai_29_ENE", dates = as.Date(c("2024-01-02", "2024-01-03")))),
    "no sub-hourly file")
})

test_that("input validation errors are clear", {
  d <- as.Date(c("2024-01-02", "2024-01-03"))
  expect_error(get_NOAACRN_data(dates = d), "exactly one")
  expect_error(get_NOAACRN_data(station = "Kenai", coords = c(-150, 60), dates = d),
               "exactly one")
  expect_error(get_NOAACRN_data(station = c("a", "b"), dates = d), "single character")
  expect_error(get_NOAACRN_data(station = "Kenai", dates = c("2024-01-02", "2024-01-03")),
               "class Date")
})

test_that("out_dir writes one CSV per period with local-time Date_Time, replacing old files", {
  env <- new.env()
  mock_crn_network(env)
  root <- withr::local_tempdir()
  out_dir <- file.path(root, "with space", "nested dir")
  d2 <- data.frame(Start_Dates = as.Date(c("2024-01-02", "2024-01-06")),
                   End_Dates   = as.Date(c("2024-01-03", "2024-01-07")))
  res <- suppressMessages(get_NOAACRN_data(station = "Kenai", dates = d2, out_dir = out_dir))
  files <- sort(list.files(out_dir))
  expect_identical(files, c("AK_Kenai_29_ENE_20240102_to_20240103.csv",
                            "AK_Kenai_29_ENE_20240106_to_20240107.csv"))

  path <- file.path(out_dir, files[1])
  csv <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
                         progress = FALSE)
  expect_equal(nrow(csv), 576L)
  expect_named(csv, c(.crn_subhourly_cols, "Date_Time"))
  expect_identical(csv$Date_Time[1], "2024-01-02 00:05:00")
  expect_false(any(grepl("Z$", csv$Date_Time)))
  expect_identical(csv$UTC_TIME[1], format(as.POSIXct("2024-01-02 09:05", tz = "UTC"), "%H%M"))

  # Re-running replaces the file (same row count, not appended)
  writeLines("stale", path)
  suppressMessages(get_NOAACRN_data(station = "Kenai", dates = d2, out_dir = out_dir))
  expect_equal(nrow(readr::read_csv(path, col_types = readr::cols(.default = "c"),
                                    progress = FALSE)), 576L)
})

test_that("temp files are removed even when the call errors", {
  env <- new.env()
  mock_crn_network(env, slabs = list("2024" = c("2024-02-01 00:05", "2024-02-01 06:00")))
  expect_error(
    suppressMessages(get_NOAACRN_data(
      station = "AK_Kenai_29_ENE", dates = as.Date(c("2024-01-02", "2024-01-03")))),
    "no observations")
  expect_false(dir.exists(dirname(env$dests[1])))
})

test_that("missing_to_na = FALSE keeps sentinels", {
  env <- new.env()
  mock_crn_network(env, vals = list(AIR_TEMPERATURE = "-9999.0"))
  out <- suppressMessages(get_NOAACRN_data(
    station = "Kenai", dates = as.Date(c("2024-01-02", "2024-01-02")),
    missing_to_na = FALSE))
  expect_true(all(out$AIR_TEMPERATURE == -9999))
})

test_that("get_NOAACRN_data downloads real Kenai data across a Dec 31/Jan 1 crossing (network)", {
  skip_on_cran()
  skip_if_not_installed("curl")
  skip_if_offline("www.ncei.noaa.gov")
  # Kenai has no soil sensors in this period, so the all-NA warning is expected
  expect_warning(
    d <- suppressMessages(get_NOAACRN_data(
      station = "AK_Kenai_29_ENE", dates = as.Date(c("2023-12-31", "2024-01-01")))),
    "SOIL_MOISTURE_5, SOIL_TEMPERATURE_5")
  expect_equal(nrow(d), 576L)
  expect_identical(attr(d$Date_Time, "tzone"), "Etc/GMT+9")
  expect_false(anyNA(d$Date_Time))
  expect_identical(format(range(d$Date_Time), "%Y-%m-%d %H:%M"),
                   c("2023-12-31 00:05", "2024-01-02 00:00"))
})

test_that("in a multi-period call, an empty period is skipped with a warning and the rest are kept", {
  env <- new.env()
  mock_crn_network(env, kenai_years = c(2020, 2024),
                   slabs = list("2024" = c("2023-12-31 15:05", "2024-01-12 12:00")))
  root <- withr::local_tempdir()
  d2 <- data.frame(Start_Dates = as.Date(c("2020-06-01", "2024-01-02")),
                   End_Dates   = as.Date(c("2020-06-02", "2024-01-03")))
  msgs <- character()
  res <- withCallingHandlers(
    suppressMessages(get_NOAACRN_data(station = "AK_Kenai_29_ENE", dates = d2, out_dir = root)),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_named(res, c("20200601_to_20200602", "20240102_to_20240103"))
  expect_null(res[[1]])
  expect_equal(nrow(res[[2]]), 576L)
  expect_true(any(grepl("20200601_to_20200602.*skipped", msgs)))
  expect_identical(list.files(root), "AK_Kenai_29_ENE_20240102_to_20240103.csv")
})

test_that("when every period fails the call stops", {
  env <- new.env()
  mock_crn_network(env, kenai_years = c(2020, 2021), slabs = list())
  d2 <- data.frame(Start_Dates = as.Date(c("2020-06-01", "2021-06-01")),
                   End_Dates   = as.Date(c("2020-06-02", "2021-06-02")))
  expect_error(
    suppressWarnings(suppressMessages(
      get_NOAACRN_data(station = "AK_Kenai_29_ENE", dates = d2))),
    "All periods failed")
})
