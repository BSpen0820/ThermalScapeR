test_that(".crn_download returns 'ok' when the transport succeeds", {
  dest <- withr::local_tempfile()
  testthat::local_mocked_bindings(
    .crn_http_get = function(url, dest) writeLines("x", dest)
  )
  expect_identical(.crn_download("https://example.org/a", dest), "ok")
  expect_true(file.exists(dest))
})

test_that(".crn_download reports 'not_found' for a 404 without retrying", {
  dest <- withr::local_tempfile()
  n <- 0L
  testthat::local_mocked_bindings(
    .crn_http_get = function(url, dest) {
      n <<- n + 1L
      warning("cannot open URL 'x': HTTP status was '404 Not Found'")
      stop("cannot open URL 'x'")
    }
  )
  expect_identical(.crn_download("https://example.org/a", dest), "not_found")
  expect_equal(n, 1L)
})

test_that(".crn_download retries a non-404 failure once, then succeeds or stops", {
  dest <- withr::local_tempfile()
  calls <- 0L
  testthat::local_mocked_bindings(
    .crn_http_get = function(url, dest) {
      calls <<- calls + 1L
      if (calls == 1L) stop("Timeout was reached")
      writeLines("x", dest)
    }
  )
  expect_identical(.crn_download("https://example.org/a", dest), "ok")
  expect_equal(calls, 2L)

  calls <- 0L
  testthat::local_mocked_bindings(
    .crn_http_get = function(url, dest) {
      calls <<- calls + 1L
      writeLines("partial", dest)
      stop("connection reset")
    }
  )
  expect_error(.crn_download("https://example.org/a", dest), "Failed to download")
  expect_equal(calls, 2L)
  expect_false(file.exists(dest))
})

test_that(".crn_download raises the timeout locally and restores it", {
  dest <- withr::local_tempfile()
  seen <- NULL
  before <- getOption("timeout")
  testthat::local_mocked_bindings(
    .crn_http_get = function(url, dest) {
      seen <<- getOption("timeout")
      writeLines("x", dest)
    }
  )
  .crn_download("https://example.org/a", dest)
  expect_gte(seen, 600)
  expect_identical(getOption("timeout"), before)
})

test_that(".crn_download_year builds the URL, caches, and maps 404 to NULL", {
  tmp <- withr::local_tempdir()
  urls <- character()
  testthat::local_mocked_bindings(
    .crn_download = function(url, dest, retries = 1) {
      urls <<- c(urls, url)
      writeLines("x", dest)
      "ok"
    }
  )
  p1 <- .crn_download_year("AK_Kenai_29_ENE", 2024L, tmp)
  expect_identical(basename(p1), "CRNS0101-05-2024-AK_Kenai_29_ENE.txt")
  expect_identical(normalizePath(dirname(p1)), normalizePath(tmp))
  expect_identical(urls,
    "https://www.ncei.noaa.gov/pub/data/uscrn/products/subhourly01/2024/CRNS0101-05-2024-AK_Kenai_29_ENE.txt")

  p2 <- .crn_download_year("AK_Kenai_29_ENE", 2024L, tmp)
  expect_identical(p1, p2)
  expect_length(urls, 1L)

  testthat::local_mocked_bindings(
    .crn_download = function(url, dest, retries = 1) "not_found"
  )
  expect_null(.crn_download_year("AK_Kenai_29_ENE", 2031L, tmp))
})

test_that(".crn_year_listing parses the directory page and tolerates a missing year", {
  testthat::local_mocked_bindings(
    .crn_download = function(url, dest, retries = 1) {
      writeLines(c(
        '<a href="CRNS0101-05-2024-AK_Kenai_29_ENE.txt">CRNS0101-05-2024-AK_Kenai_29_ENE.txt</a>',
        '<a href="CRNS0101-05-2024-AK_Bethel_87_WNW.txt">CRNS0101-05-2024-AK_Bethel_87_WNW.txt</a>'),
        dest)
      "ok"
    }
  )
  expect_identical(.crn_year_listing(2024L), c("AK_Bethel_87_WNW", "AK_Kenai_29_ENE"))

  testthat::local_mocked_bindings(
    .crn_download = function(url, dest, retries = 1) "not_found"
  )
  expect_identical(.crn_year_listing(2031L), character())
})

test_that(".crn_stations downloads, parses, and removes its temp file", {
  seen_dest <- NULL
  testthat::local_mocked_bindings(
    .crn_download = function(url, dest, retries = 1) {
      seen_dest <<- dest
      expect_match(url, "stations\\.tsv$")
      file.copy(crn_stations_fixture(), dest)
      "ok"
    }
  )
  st <- .crn_stations()
  expect_equal(nrow(st), 10L)
  expect_true("STATION_DIR" %in% names(st))
  expect_false(file.exists(seen_dest))
})

test_that(".crn_stations stops if the station table cannot be found", {
  testthat::local_mocked_bindings(
    .crn_download = function(url, dest, retries = 1) "not_found"
  )
  expect_error(.crn_stations(), "station")
})

test_that(".crn_download does not mistake '404' inside a byte count for a missing file", {
  dest <- withr::local_tempfile()
  calls <- 0L
  testthat::local_mocked_bindings(
    .crn_http_get = function(url, dest) {
      calls <<- calls + 1L
      warning("downloaded length 1404123 != reported length 14230080")
      stop("cannot open URL 'x'")
    }
  )
  expect_error(.crn_download("https://example.org/a", dest), "Failed to download")
  expect_equal(calls, 2L)
})
