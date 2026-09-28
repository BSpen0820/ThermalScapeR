test_that(".crn_parse_stations builds STATION_DIR, dedupes, and keeps NA-like WBANs", {
  st <- .crn_parse_stations(crn_stations_fixture())
  expect_equal(nrow(st), 10L)
  expect_false(anyDuplicated(st$STATION_DIR) > 0L)

  # Duplicate Bethel rows collapse to the numeric-WBAN row
  bethel <- st[st$STATION_DIR == "AK_Bethel_87_WNW", ]
  expect_equal(nrow(bethel), 1L)
  expect_identical(bethel$WBAN, "26656")

  # Non-ASCII name is transliterated to match the NOAA directory name
  expect_true("AK_Utqiagvik_formerly_Barrow_4_ENE" %in% st$STATION_DIR)

  # A WBAN of literal "NA" stays text, not missing
  sterling <- st[st$STATION_DIR == "VA_Sterling_0_N", ]
  expect_identical(sterling$WBAN, "NA")

  # Types: numeric coordinates, Date columns with NA for blanks
  expect_type(st$LATITUDE, "double")
  expect_type(st$LONGITUDE, "double")
  expect_s3_class(st$COMMISSIONING, "Date")
  expect_true(is.na(sterling$COMMISSIONING))
  expect_equal(sterling$CLOSING, as.Date("2008-02-20"))
  expect_equal(st$COMMISSIONING[st$STATION_DIR == "AK_Kenai_29_ENE"], as.Date("2011-09-12"))
})

test_that(".crn_resolve_station matches exact names case/underscore-insensitively", {
  st <- .crn_parse_stations(crn_stations_fixture())
  expect_identical(.crn_resolve_station(st, "AK_Kenai_29_ENE")$STATION_DIR, "AK_Kenai_29_ENE")
  expect_identical(.crn_resolve_station(st, "ak kenai 29 ene")$STATION_DIR, "AK_Kenai_29_ENE")
})

test_that(".crn_resolve_station accepts a unique partial match", {
  st <- .crn_parse_stations(crn_stations_fixture())
  expect_identical(.crn_resolve_station(st, "Kenai")$STATION_DIR, "AK_Kenai_29_ENE")
  # duplicated Bethel rows do not cause a false 'ambiguous' error
  expect_identical(.crn_resolve_station(st, "Bethel")$STATION_DIR, "AK_Bethel_87_WNW")
  expect_identical(.crn_resolve_station(st, "Utqiagvik")$STATION_DIR,
                   "AK_Utqiagvik_formerly_Barrow_4_ENE")
})

test_that(".crn_resolve_station errors on ambiguous and unknown names", {
  st <- .crn_parse_stations(crn_stations_fixture())
  expect_error(.crn_resolve_station(st, "Newton"), "GA_Newton_8_W")
  expect_error(.crn_resolve_station(st, "Newton"), "MS_Newton_5_ENE")
  expect_error(.crn_resolve_station(st, "Nowhere"), "No station")
})

test_that(".crn_parse_listing extracts station directories for the right year", {
  html <- c(
    '<td><a href="CRNS0101-05-2024-AK_Kenai_29_ENE.txt">CRNS0101-05-2024-AK_Kenai_29_ENE.txt</a></td>',
    '<td><a href="CRNS0101-05-2024-AK_Bethel_87_WNW.txt">CRNS0101-05-2024-AK_Bethel_87_WNW.txt</a></td>',
    '<td><a href="CRNS0101-05-2023-AK_Farther_9_S.txt">CRNS0101-05-2023-AK_Farther_9_S.txt</a></td>',
    '<td><a href="HEADERS.txt">HEADERS.txt</a></td>'
  )
  expect_identical(.crn_parse_listing(html, 2024), c("AK_Bethel_87_WNW", "AK_Kenai_29_ENE"))
  expect_identical(.crn_parse_listing(html, 2023), "AK_Farther_9_S")
  expect_identical(.crn_parse_listing(character(), 2024), character())
})

test_that(".crn_check_station_years stops, warns, or stays silent", {
  listings <- list("2023" = c("A", "B"), "2024" = "A")
  expect_silent(.crn_check_station_years("A", 2023:2024, listings))
  expect_warning(.crn_check_station_years("B", 2023:2024, listings), "2024")
  expect_error(.crn_check_station_years("C", 2023:2024, listings), "no sub-hourly file")
})

test_that(".crn_nearest_station picks the closest station that has data", {
  st <- .crn_parse_stations(crn_stations_fixture())
  # A point sitting on the synthetic 'Nearer' station
  lon <- -150.4; lat <- 60.75

  both <- list("2024" = c("AK_Nearer_1_N", "AK_Kenai_29_ENE", "AK_Farther_9_S"))
  got <- .crn_nearest_station(st, lon, lat, 2024L, "USCRN", both)
  expect_identical(got$STATION_DIR, "AK_Nearer_1_N")
  expect_equal(got$dist_km, 0, tolerance = 1e-6)

  # Nearer has no file that year: dropped, next closest (Kenai) is used
  no_nearer <- list("2024" = c("AK_Kenai_29_ENE", "AK_Farther_9_S"))
  got <- .crn_nearest_station(st, lon, lat, 2024L, "USCRN", no_nearer)
  expect_identical(got$STATION_DIR, "AK_Kenai_29_ENE")
  expect_gt(got$dist_km, 0)
})

test_that(".crn_nearest_station requires data in every requested year", {
  st <- .crn_parse_stations(crn_stations_fixture())
  listings <- list("2023" = c("AK_Farther_9_S"),
                   "2024" = c("AK_Kenai_29_ENE", "AK_Farther_9_S"))
  got <- .crn_nearest_station(st, -150.44, 60.72, 2023:2024, "USCRN", listings)
  expect_identical(got$STATION_DIR, "AK_Farther_9_S")
})

test_that(".crn_nearest_station filters by network", {
  st <- .crn_parse_stations(crn_stations_fixture())
  listings <- list("2024" = c("NM_Dulce_1_NW", "AK_Kenai_29_ENE"))
  # Standing on Dulce (USRCRN): default network excludes it
  got <- .crn_nearest_station(st, -107, 36.93, 2024L, "USCRN", listings)
  expect_identical(got$STATION_DIR, "AK_Kenai_29_ENE")
  got <- .crn_nearest_station(st, -107, 36.93, 2024L, NULL, listings)
  expect_identical(got$STATION_DIR, "NM_Dulce_1_NW")
  got <- .crn_nearest_station(st, -107, 36.93, 2024L, c("USCRN", "USRCRN"), listings)
  expect_identical(got$STATION_DIR, "NM_Dulce_1_NW")
})

test_that(".crn_nearest_station errors clearly when nothing is eligible", {
  st <- .crn_parse_stations(crn_stations_fixture())
  expect_error(.crn_nearest_station(st, -150.44, 60.72, 2024L, "USCRN", list("2024" = character())),
               "USCRN.*2024")
})
