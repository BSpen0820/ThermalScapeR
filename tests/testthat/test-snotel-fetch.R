test_that(".snotel_elements_param builds the :*:* qualified list, or passes '*' through", {
  expect_identical(.snotel_elements_param("*"), "*")
  expect_identical(.snotel_elements_param("WTEQ"), "WTEQ:*:*")
  expect_identical(.snotel_elements_param(c("WTEQ", "STO")), "WTEQ:*:*,STO:*:*")
})

test_that(".snotel_duration_bounds builds DAILY and HOURLY request bounds", {
  b <- .snotel_duration_bounds(as.Date("2024-01-01"), as.Date("2024-01-03"), "DAILY")
  expect_identical(b, list(begin = "2024-01-01", end = "2024-01-03"))
  b <- .snotel_duration_bounds(as.Date("2024-01-01"), as.Date("2024-01-01"), "HOURLY")
  expect_identical(b, list(begin = "2024-01-01 00:00", end = "2024-01-01 23:00"))
})

test_that(".snotel_fetch requests :*:*-qualified elements and parses a real-shaped response", {
  fixture <- testthat::test_path("fixtures", "snotel_data_sample.json")
  seen_url <- NULL
  testthat::local_mocked_bindings(
    .snotel_download = function(url, dest, retries = 1) {
      seen_url <<- url
      file.copy(fixture, dest, overwrite = TRUE)
      invisible(NULL)
    }
  )
  df <- .snotel_fetch("312:ID:SNTL", c("WTEQ", "STO"), "DAILY",
                      as.Date("2024-01-01"), as.Date("2024-01-02"))
  expect_match(seen_url, "WTEQ:\\*:\\*,STO:\\*:\\*")
  expect_true(all(c("WTEQ", "STO") %in% df$elementCode))
  expect_true(all(is.na(df$heightDepth[df$elementCode == "WTEQ"])))
  expect_true(all(!is.na(df$heightDepth[df$elementCode == "STO"])))
  expect_equal(df$heightDepth[df$elementCode == "STO"][1], -20)
})

test_that(".snotel_fetch returns an empty data.frame (not an error) when the API returns []", {
  testthat::local_mocked_bindings(
    .snotel_download = function(url, dest, retries = 1) {
      writeLines("[]", dest)
      invisible(NULL)
    }
  )
  df <- .snotel_fetch("999:ZZ:SNTL", "*", "DAILY", as.Date("2024-01-01"), as.Date("2024-01-01"))
  expect_equal(nrow(df), 0L)
  expect_named(df, c("Date_Time", "elementCode", "heightDepth", "ordinal", "value",
                     "qcFlag", "qaFlag"))
})

test_that(".snotel_depth_label builds column stems per the naming rules", {
  expect_identical(.snotel_depth_label("WTEQ", NA_real_, 1L), "WTEQ")
  expect_identical(.snotel_depth_label("SNWD", 0, 1L), "SNWD_P0000")
  expect_identical(.snotel_depth_label("STO", -20, 1L), "STO_N0508")
  expect_identical(.snotel_depth_label("STO", -8, 1L), "STO_N0203")
  expect_identical(.snotel_depth_label("STO", -2, 1L), "STO_N0051")
  expect_identical(.snotel_depth_label("PTEMP", 8, 1L), "PTEMP_P0203")
  expect_identical(.snotel_depth_label("STO", -20, 2L), "STO_N0508_o2")
})

test_that(".snotel_pivot_wide builds a full DAILY skeleton with element and depth columns", {
  long_df <- data.frame(
    Date_Time = c("2024-01-01", "2024-01-02", "2024-01-01", "2024-01-02"),
    elementCode = c("WTEQ", "WTEQ", "STO", "STO"),
    heightDepth = c(NA, NA, -20, -20),
    ordinal = c(1L, 1L, 1L, 1L),
    value = c(6.2, 6.1, 28.0, 27.5),
    qcFlag = c("V", "V", "V", "V"),
    qaFlag = c(NA_character_, NA_character_, NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  out <- .snotel_pivot_wide(long_df, as.Date("2024-01-01"), as.Date("2024-01-03"),
                            "DAILY", "Etc/GMT+8")
  expect_equal(nrow(out), 3L)
  expect_s3_class(out$Date_Time, "Date")
  expect_identical(out$WTEQ, c(6.2, 6.1, NA))
  expect_identical(out$STO_N0508, c(28.0, 27.5, NA))
  expect_true("WTEQ_qcFlag" %in% names(out))
  expect_false("WTEQ_qaFlag" %in% names(out))
})

test_that(".snotel_pivot_wide adds a qaFlag column only when present, and builds an HOURLY skeleton", {
  long_df <- data.frame(
    Date_Time = c("2024-01-01 00:00", "2024-01-01 01:00"),
    elementCode = c("WTEQ", "WTEQ"), heightDepth = c(NA, NA), ordinal = c(1L, 1L),
    value = c(6.2, 6.1), qcFlag = c("V", "V"), qaFlag = c("A", NA_character_),
    stringsAsFactors = FALSE
  )
  out <- .snotel_pivot_wide(long_df, as.Date("2024-01-01"), as.Date("2024-01-01"),
                            "HOURLY", "Etc/GMT+8")
  expect_equal(nrow(out), 24L)
  expect_s3_class(out$Date_Time, "POSIXct")
  expect_identical(attr(out$Date_Time, "tzone"), "Etc/GMT+8")
  expect_true("WTEQ_qaFlag" %in% names(out))
  expect_identical(out$WTEQ_qaFlag[1:2], c("A", NA_character_))
})

test_that(".snotel_station_elements lists unique element codes for one duration", {
  fixture <- testthat::test_path("fixtures", "snotel_station_elements_sample.json")
  testthat::local_mocked_bindings(
    .snotel_download = function(url, dest, retries = 1) {
      expect_match(url, "returnStationElements=true")
      expect_match(url, "activeOnly=false")
      file.copy(fixture, dest, overwrite = TRUE)
      invisible(NULL)
    }
  )
  expect_identical(.snotel_station_elements("312:ID:SNTL", "DAILY"), c("WTEQ", "STO"))
  expect_identical(.snotel_station_elements("312:ID:SNTL", "HOURLY"), c("WTEQ", "PTEMP"))
})

test_that(".snotel_fetch resolves elements = '*' via the station's own element list, not a bare wildcard", {
  elems_fixture <- testthat::test_path("fixtures", "snotel_station_elements_sample.json")
  data_fixture <- testthat::test_path("fixtures", "snotel_data_sample.json")
  seen_urls <- character()
  testthat::local_mocked_bindings(
    .snotel_download = function(url, dest, retries = 1) {
      seen_urls <<- c(seen_urls, url)
      if (grepl("returnStationElements", url, fixed = TRUE)) {
        file.copy(elems_fixture, dest, overwrite = TRUE)
      } else {
        file.copy(data_fixture, dest, overwrite = TRUE)
      }
      invisible(NULL)
    }
  )
  df <- .snotel_fetch("312:ID:SNTL", "*", "DAILY", as.Date("2024-01-01"), as.Date("2024-01-02"))
  data_url <- seen_urls[!grepl("returnStationElements", seen_urls, fixed = TRUE)]
  expect_length(data_url, 1L)
  expect_match(data_url, "WTEQ:\\*:\\*")
  expect_match(data_url, "STO:\\*:\\*")
  expect_false(grepl("elements=\\*&", data_url))
})

test_that(".snotel_fetch returns empty (not an error) when the station has no elements for this duration", {
  elems_fixture <- testthat::test_path("fixtures", "snotel_station_elements_sample.json")
  testthat::local_mocked_bindings(
    .snotel_download = function(url, dest, retries = 1) {
      file.copy(elems_fixture, dest, overwrite = TRUE)
      invisible(NULL)
    }
  )
  df <- .snotel_fetch("312:ID:SNTL", "*", "MONTHLY", as.Date("2024-01-01"), as.Date("2024-01-02"))
  expect_equal(nrow(df), 0L)
})
