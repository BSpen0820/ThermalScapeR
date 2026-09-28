snotel_stations_fixture <- function() {
  testthat::test_path("fixtures", "snotel_stations_sample.json")
}

# Mocks the download seam. `stations_json` is served for the /stations call;
# `data_fn(triplet, elements, duration, begin, end)` returns the JSON text to
# serve for a /data call, or NULL to make it look like [] (no data).
mock_snotel_network <- function(env, stations_json = snotel_stations_fixture(),
                                data_fn = NULL, .frame = parent.frame()) {
  env$urls <- character()
  testthat::local_mocked_bindings(
    .snotel_download = function(url, dest, retries = 1) {
      env$urls <- c(env$urls, url)
      if (grepl("/stations", url, fixed = TRUE)) {
        file.copy(stations_json, dest, overwrite = TRUE)
      } else if (!is.null(data_fn)) {
        triplet <- sub(".*stationTriplets=([^&]+).*", "\\1", url)
        txt <- data_fn(triplet, url)
        writeLines(if (is.null(txt)) "[]" else txt, dest)
      } else {
        writeLines("[]", dest)
      }
      invisible(NULL)
    },
    .env = .frame,
    .package = "ThermalScapeR"
  )
}
