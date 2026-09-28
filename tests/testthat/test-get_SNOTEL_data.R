wteq_only_json <- function(triplet, url) {
  sprintf('[{"stationTriplet":"%s","data":[{"stationElement":{"elementCode":"WTEQ","ordinal":1},"values":[{"date":"2024-01-01","value":6.2,"qcFlag":"V"},{"date":"2024-01-02","value":6.1,"qcFlag":"V"}]}]}]',
         triplet)
}

test_that("a Date vector returns one data.frame, by station name", {
  env <- new.env()
  mock_snotel_network(env, data_fn = wteq_only_json)
  out <- suppressMessages(get_SNOTEL_data(
    station = "Banner Summit", dates = as.Date(c("2024-01-01", "2024-01-02"))))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
  expect_true("WTEQ" %in% names(out))
  expect_s3_class(out$Date_Time, "Date")
})

test_that("a data.frame of dates returns a named list, even for one row", {
  env <- new.env()
  mock_snotel_network(env, data_fn = wteq_only_json)
  d2 <- data.frame(Start_Dates = as.Date(c("2024-01-01", "2024-02-01")),
                   End_Dates   = as.Date(c("2024-01-02", "2024-02-02")))
  out <- suppressMessages(get_SNOTEL_data(station = "Banner Summit", dates = d2))
  expect_type(out, "list")
  expect_named(out, c("20240101_to_20240102", "20240201_to_20240202"))

  d1 <- d2[1, ]
  out1 <- suppressMessages(get_SNOTEL_data(station = "Banner Summit", dates = d1))
  expect_type(out1, "list")
  expect_false(is.data.frame(out1))
})

test_that("explicit multi-depth elements are requested as :*:*, not bare", {
  env <- new.env()
  data_fn <- function(triplet, url) {
    expect_match(url, "STO:\\*:\\*")
    expect_match(url, "SMS:\\*:\\*")
    sprintf('[{"stationTriplet":"%s","data":[{"stationElement":{"elementCode":"STO","ordinal":1,"heightDepth":-20},"values":[{"date":"2024-01-01","value":28.0,"qcFlag":"V"}]}]}]',
           triplet)
  }
  mock_snotel_network(env, data_fn = data_fn)
  out <- suppressMessages(get_SNOTEL_data(
    station = "Banner Summit", elements = c("STO", "SMS"),
    dates = as.Date(c("2024-01-01", "2024-01-01"))))
  expect_true("STO_N0508" %in% names(out))
})

test_that("coords resolves the nearest station with data and messages the choice", {
  env <- new.env()
  data_fn <- function(triplet, url) {
    if (triplet == "801:CO:SNTL") wteq_only_json(triplet, url) else NULL
  }
  mock_snotel_network(env, data_fn = data_fn)
  # "Late Start" sits exactly at the query point (nearest) but has no data in
  # this mock, so a "trying the next-nearest station" warning is expected
  # before falling back to "Nearby Peak" (801:CO:SNTL); suppress it here since
  # this test is about the eventual resolution, not the fallback warning.
  expect_message(
    out <- suppressWarnings(get_SNOTEL_data(coords = c(-106.50, 39.50),
                                            dates = as.Date(c("2024-01-01", "2024-01-02")),
                                            network = "SNTL")),
    "801:CO:SNTL")
  expect_equal(nrow(out), 2L)
})

test_that("coords warns when the chosen station is beyond max_dist_km", {
  env <- new.env()
  mock_snotel_network(env, data_fn = wteq_only_json)
  # Offset slightly from "Late Start"'s exact coordinates so the resolved
  # distance is a small nonzero value, not literally 0 (which could never
  # exceed max_dist_km); still nearest to "Late Start" than any other fixture
  # station.
  expect_warning(
    suppressMessages(get_SNOTEL_data(coords = c(-106.501, 39.501),
                                     dates = as.Date(c("2024-01-01", "2024-01-01")),
                                     network = "SNTL", max_dist_km = 0.001)),
    "km away")
})

test_that("a named station with zero data across every period stops", {
  env <- new.env()
  mock_snotel_network(env, data_fn = function(triplet, url) NULL)
  expect_error(
    suppressMessages(get_SNOTEL_data(
      station = "Banner Summit", dates = as.Date(c("2024-01-01", "2024-01-02")))),
    "no data")
})

test_that("in a multi-period call, an empty period is skipped with a warning and the rest are kept", {
  env <- new.env()
  data_fn <- function(triplet, url) {
    if (grepl("beginDate=2024-02", url, fixed = TRUE)) wteq_only_json(triplet, url) else NULL
  }
  mock_snotel_network(env, data_fn = data_fn)
  d2 <- data.frame(Start_Dates = as.Date(c("2024-01-01", "2024-02-01")),
                   End_Dates   = as.Date(c("2024-01-02", "2024-02-02")))
  msgs <- character()
  res <- withCallingHandlers(
    suppressMessages(get_SNOTEL_data(station = "Banner Summit", dates = d2)),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_named(res, c("20240101_to_20240102", "20240201_to_20240202"))
  expect_null(res[[1]])
  expect_equal(nrow(res[[2]]), 2L)
  expect_true(any(grepl("20240101_to_20240102.*skipped", msgs)))
})

test_that("when every period fails the call stops", {
  env <- new.env()
  mock_snotel_network(env, data_fn = function(triplet, url) NULL)
  d2 <- data.frame(Start_Dates = as.Date(c("2024-01-01", "2024-02-01")),
                   End_Dates   = as.Date(c("2024-01-02", "2024-02-02")))
  expect_error(
    suppressWarnings(suppressMessages(get_SNOTEL_data(station = "Banner Summit", dates = d2))),
    "All periods failed")
})

test_that("HOURLY on a station with no recorded time zone stops with a clear error", {
  env <- new.env()
  no_tz_json <- withr::local_tempfile(fileext = ".json")
  writeLines(
    '[{"stationTriplet":"999:ZZ:COOP","stationId":"999","stateCode":"ZZ","networkCode":"COOP","name":"No TZ Site","elevation":1000,"latitude":40.0,"longitude":-100.0,"dataTimeZone":null,"beginDate":"1990-01-01 00:00","endDate":"2100-01-01 00:00"}]',
    no_tz_json)
  mock_snotel_network(env, stations_json = no_tz_json, data_fn = wteq_only_json)
  expect_error(
    suppressMessages(get_SNOTEL_data(station = "No TZ Site", network = NULL, duration = "HOURLY",
                                     dates = as.Date(c("2024-01-01", "2024-01-01")))),
    "time zone")
  # DAILY doesn't need a time zone, so the same station works fine
  out <- suppressMessages(get_SNOTEL_data(station = "No TZ Site", network = NULL,
                                          dates = as.Date(c("2024-01-01", "2024-01-02"))))
  expect_equal(nrow(out), 2L)
})

test_that("input validation errors are clear", {
  d <- as.Date(c("2024-01-01", "2024-01-02"))
  expect_error(get_SNOTEL_data(dates = d), "exactly one")
  expect_error(get_SNOTEL_data(station = "Banner Summit", coords = c(-106.5, 39.5), dates = d),
               "exactly one")
  expect_error(get_SNOTEL_data(station = c("a", "b"), dates = d), "single character")
  expect_error(get_SNOTEL_data(station = "Banner Summit", dates = d, duration = "WEEKLY"),
               "DAILY.*HOURLY")
  expect_error(get_SNOTEL_data(station = "Banner Summit", dates = c("2024-01-01", "2024-01-02")),
               "class Date")
})

test_that("out_dir writes one CSV per period with local-time Date_Time, replacing old files", {
  env <- new.env()
  mock_snotel_network(env, data_fn = wteq_only_json)
  out_dir <- withr::local_tempdir()
  d2 <- data.frame(Start_Dates = as.Date(c("2024-01-01", "2024-02-01")),
                   End_Dates   = as.Date(c("2024-01-02", "2024-02-02")))
  suppressMessages(get_SNOTEL_data(station = "Banner Summit", dates = d2, out_dir = out_dir))
  files <- sort(list.files(out_dir))
  expect_identical(files, c("312_20240101_to_20240102.csv", "312_20240201_to_20240202.csv"))

  csv <- readr::read_csv(file.path(out_dir, files[1]),
                        col_types = readr::cols(.default = readr::col_character()),
                        progress = FALSE)
  expect_identical(csv$Date_Time[1], "2024-01-01")

  writeLines("stale", file.path(out_dir, files[1]))
  suppressMessages(get_SNOTEL_data(station = "Banner Summit", dates = d2, out_dir = out_dir))
  expect_equal(nrow(readr::read_csv(file.path(out_dir, files[1]),
                                    col_types = readr::cols(.default = "c"),
                                    progress = FALSE)), 2L)
})

test_that("get_SNOTEL_data pulls real Banner Summit data (network)", {
  skip_on_cran()
  skip_if_not_installed("curl")
  skip_if_offline("wcc.sc.egov.usda.gov")
  d <- suppressMessages(get_SNOTEL_data(
    station = "Banner Summit", dates = as.Date(c("2024-01-01", "2024-01-03"))))
  expect_equal(nrow(d), 3L)
  expect_true("WTEQ" %in% names(d))
  expect_false(anyNA(d$Date_Time))

  h <- suppressMessages(get_SNOTEL_data(
    station = "Banner Summit", elements = "WTEQ", duration = "HOURLY",
    dates = as.Date(c("2024-01-01", "2024-01-01"))))
  expect_equal(nrow(h), 24L)
  expect_s3_class(h$Date_Time, "POSIXct")
})
