test_that(".snotel_download returns invisibly when the transport succeeds", {
  dest <- withr::local_tempfile()
  testthat::local_mocked_bindings(
    .snotel_http_get = function(url, dest) writeLines("x", dest)
  )
  expect_null(.snotel_download("https://example.org/a", dest))
  expect_true(file.exists(dest))
})

test_that(".snotel_download retries once on failure, then succeeds or stops", {
  dest <- withr::local_tempfile()
  calls <- 0L
  testthat::local_mocked_bindings(
    .snotel_http_get = function(url, dest) {
      calls <<- calls + 1L
      if (calls == 1L) stop("Timeout was reached")
      writeLines("x", dest)
    }
  )
  expect_null(.snotel_download("https://example.org/a", dest))
  expect_equal(calls, 2L)

  calls <- 0L
  testthat::local_mocked_bindings(
    .snotel_http_get = function(url, dest) {
      calls <<- calls + 1L
      writeLines("partial", dest)
      stop("connection reset")
    }
  )
  expect_error(.snotel_download("https://example.org/a", dest), "Failed to download")
  expect_equal(calls, 2L)
  expect_false(file.exists(dest))
})

test_that(".snotel_download raises the timeout locally and restores it", {
  dest <- withr::local_tempfile()
  seen <- NULL
  before <- getOption("timeout")
  testthat::local_mocked_bindings(
    .snotel_http_get = function(url, dest) {
      seen <<- getOption("timeout")
      writeLines("x", dest)
    }
  )
  .snotel_download("https://example.org/a", dest)
  expect_gte(seen, 600)
  expect_identical(getOption("timeout"), before)
})

test_that(".snotel_download does not leak a warning to the console on a recovered failure", {
  dest <- withr::local_tempfile()
  calls <- 0L
  testthat::local_mocked_bindings(
    .snotel_http_get = function(url, dest) {
      calls <<- calls + 1L
      if (calls == 1L) {
        warning("cannot open URL 'x': HTTP status was '500 Internal Server Error'")
        stop("cannot open URL 'x'")
      }
      writeLines("x", dest)
    }
  )
  expect_no_warning(.snotel_download("https://example.org/a", dest))
  expect_equal(calls, 2L)
})

test_that(".snotel_download does not leak a warning to the console on a permanent failure", {
  dest <- withr::local_tempfile()
  testthat::local_mocked_bindings(
    .snotel_http_get = function(url, dest) {
      warning("cannot open URL 'x': HTTP status was '404 Not Found'")
      stop("cannot open URL 'x'")
    }
  )
  expect_no_warning(expect_error(.snotel_download("https://example.org/a", dest), "Failed to download"))
})

test_that(".snotel_stations always requests activeOnly=false and parses the result", {
  seen_url <- NULL
  testthat::local_mocked_bindings(
    .snotel_download = function(url, dest, retries = 1) {
      seen_url <<- url
      file.copy(snotel_stations_fixture(), dest, overwrite = TRUE)
      invisible(NULL)
    }
  )
  st <- .snotel_stations()
  expect_match(seen_url, "activeOnly=false")
  expect_equal(nrow(st), 11L)
})
