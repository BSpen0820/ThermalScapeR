test_that(".snotel_nearest_station filters by network and period-of-record, then picks nearest", {
  st <- .snotel_parse_stations(snotel_stations_fixture())
  periods <- data.frame(start = as.Date("2020-01-01"), end = as.Date("2020-01-02"))
  always_has_data <- function(cand) TRUE
  got <- .snotel_nearest_station(st, -106.50, 39.50, periods, "SNTL", always_has_data)
  # "Late Start" (beginDate 2023) is excluded for a 2020 period; nearest
  # remaining SNTL candidate is "Nearby Peak"
  expect_identical(got$stationTriplet, "801:CO:SNTL")
})

test_that(".snotel_nearest_station drops a candidate that returns no data and tries the next", {
  st <- .snotel_parse_stations(snotel_stations_fixture())
  periods <- data.frame(start = as.Date("2020-01-01"), end = as.Date("2020-01-02"))
  tried <- character()
  try_fn <- function(cand) {
    tried <<- c(tried, cand$stationTriplet)
    cand$stationTriplet == "802:CO:SNTL"
  }
  expect_warning(
    got <- .snotel_nearest_station(st, -106.50, 39.50, periods, "SNTL", try_fn),
    "801:CO:SNTL"
  )
  expect_identical(got$stationTriplet, "802:CO:SNTL")
  expect_identical(tried, c("801:CO:SNTL", "802:CO:SNTL"))
})

test_that(".snotel_nearest_station stops when every candidate is empty", {
  st <- .snotel_parse_stations(snotel_stations_fixture())
  periods <- data.frame(start = as.Date("2020-01-01"), end = as.Date("2020-01-02"))
  suppressWarnings(
    expect_error(.snotel_nearest_station(st, -106.50, 39.50, periods, "SNTL",
                                         function(cand) FALSE),
                "No candidate station returned data")
  )
})

test_that(".snotel_nearest_station rejects unknown network names", {
  st <- .snotel_parse_stations(snotel_stations_fixture())
  periods <- data.frame(start = as.Date("2020-01-01"), end = as.Date("2020-01-02"))
  expect_error(.snotel_nearest_station(st, -106.5, 39.5, periods, "sntl", function(x) TRUE),
               "Unknown network")
})

test_that(".snotel_nearest_station's period-of-record filter requires overlap with every requested period", {
  st <- .snotel_parse_stations(snotel_stations_fixture())
  periods <- data.frame(start = as.Date(c("2010-01-01", "2024-01-01")),
                        end = as.Date(c("2010-01-02", "2024-01-02")))
  # "Late Start" (begins 2023-10-01) covers the 2024 period but not the 2010
  # one, so it must be excluded even though it would otherwise be nearest
  got <- .snotel_nearest_station(st, -106.50, 39.50, periods, "SNTL", function(x) TRUE)
  expect_identical(got$stationTriplet, "801:CO:SNTL")
})
