snotel_stations_fixture <- function() {
  testthat::test_path("fixtures", "snotel_stations_sample.json")
}

# A generic, broad-enough stub for the returnStationElements=true metadata
# call that .snotel_fetch() makes to resolve elements = "*" (added after the
# spec review found a bare "*" silently drops depth/ordinal>1 sensors). Its
# element codes only have to be *some* real-looking codes -- most data_fn
# implementations below ignore the elements query param and return fixed
# JSON regardless, so this just needs to make elements = "*" resolve to a
# non-empty set for both durations.
.mock_snotel_station_elements_json <- '[{"stationTriplet":"x","stationElements":[
  {"elementCode":"WTEQ","durationName":"DAILY"},
  {"elementCode":"WTEQ","durationName":"HOURLY"},
  {"elementCode":"STO","heightDepth":-20,"durationName":"DAILY"},
  {"elementCode":"SMS","heightDepth":-20,"durationName":"DAILY"}
]}]'

# Mocks the download seam. `stations_json` is served for the plain /stations
# list call; a returnStationElements=true call gets a fixed element-metadata
# stub (see above); `data_fn(triplet, elements, duration, begin, end)`
# returns the JSON text to serve for a /data call, or NULL to make it look
# like [] (no data).
mock_snotel_network <- function(env, stations_json = snotel_stations_fixture(),
                                data_fn = NULL, .frame = parent.frame()) {
  env$urls <- character()
  testthat::local_mocked_bindings(
    .snotel_download = function(url, dest, retries = 1) {
      env$urls <- c(env$urls, url)
      if (grepl("returnStationElements=true", url, fixed = TRUE)) {
        writeLines(.mock_snotel_station_elements_json, dest)
      } else if (grepl("/stations", url, fixed = TRUE)) {
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
