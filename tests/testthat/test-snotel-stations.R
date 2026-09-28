test_that(".snotel_parse_stations parses station JSON with typed dates", {
  st <- .snotel_parse_stations(snotel_stations_fixture())
  expect_equal(nrow(st), 11L)
  expect_s3_class(st$beginDate, "Date")
  expect_s3_class(st$endDate, "Date")
  expect_equal(st$beginDate[st$stationTriplet == "312:ID:SNTL"], as.Date("1979-10-01"))
  expect_equal(st$endDate[st$stationTriplet == "803:CO:SNTL"], as.Date("2015-09-30"))
  expect_type(st$latitude, "double")
  expect_type(st$dataTimeZone, "double")
})

test_that(".snotel_resolve_station matches exact and partial names case-insensitively", {
  st <- .snotel_parse_stations(snotel_stations_fixture())
  expect_identical(.snotel_resolve_station(st, "Banner Summit")$stationTriplet, "312:ID:SNTL")
  expect_identical(.snotel_resolve_station(st, "banner summit")$stationTriplet, "312:ID:SNTL")
  expect_identical(.snotel_resolve_station(st, "Banner")$stationTriplet, "312:ID:SNTL")
})

test_that(".snotel_resolve_station errors on an ambiguous name and disambiguates with state", {
  st <- .snotel_parse_stations(snotel_stations_fixture())
  expect_error(.snotel_resolve_station(st, "Trail Creek"), "700:ID:SNTL")
  expect_error(.snotel_resolve_station(st, "Trail Creek"), "701:MT:SNTL")
  expect_identical(.snotel_resolve_station(st, "Trail Creek", state = "MT")$stationTriplet,
                   "701:MT:SNTL")
})

test_that(".snotel_resolve_station uses `network` to disambiguate a same-name, same-state collision", {
  st <- .snotel_parse_stations(snotel_stations_fixture())
  # "Trail Creek" also exists as a SNOW course in ID (704:ID:SNOW); state
  # alone cannot disambiguate it from the SNTL station (700:ID:SNTL), but
  # `network` can.
  expect_identical(
    .snotel_resolve_station(st, "Trail Creek", state = "ID", network = c("SNTL", "SNTLT"))$stationTriplet,
    "700:ID:SNTL")
})

test_that(".snotel_resolve_station still errors, listing every candidate, when network cannot disambiguate", {
  st <- .snotel_parse_stations(snotel_stations_fixture())
  expect_error(.snotel_resolve_station(st, "Trail Creek", state = "ID", network = NULL), "700:ID:SNTL")
  expect_error(.snotel_resolve_station(st, "Trail Creek", state = "ID", network = NULL), "704:ID:SNOW")
  # network given but every candidate is outside it: still ambiguous, not silently empty
  expect_error(.snotel_resolve_station(st, "Trail Creek", state = "ID", network = "SCAN"), "700:ID:SNTL")
})

test_that(".snotel_resolve_station errors clearly on no match", {
  st <- .snotel_parse_stations(snotel_stations_fixture())
  expect_error(.snotel_resolve_station(st, "Nowhere At All"), "No station matches")
  expect_error(.snotel_resolve_station(st, "Banner Summit", state = "MT"), "No station matches")
})
