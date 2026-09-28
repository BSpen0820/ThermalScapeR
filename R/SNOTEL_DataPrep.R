# --------------------------------------------------------------------------- #
#  NRCS SNOTEL (AWDB REST API) station data
# --------------------------------------------------------------------------- #

.snotel_parse_stations <- function(path) {
  st <- jsonlite::fromJSON(path)
  for (col in c("beginDate", "endDate"))
    st[[col]] <- as.Date(substr(st[[col]], 1L, 10L), format = "%Y-%m-%d")
  st
}

.snotel_norm_name <- function(x) tolower(trimws(x))

.snotel_resolve_station <- function(stations, station, state = NULL) {
  cand <- stations
  if (!is.null(state))
    cand <- cand[cand$stateCode == state, , drop = FALSE]
  key <- .snotel_norm_name(cand$name)
  q <- .snotel_norm_name(station)
  exact <- which(key == q)
  if (length(exact) == 1L) return(cand[exact, , drop = FALSE])
  hits <- which(grepl(q, key, fixed = TRUE))
  if (length(hits) == 0L)
    stop(sprintf("No station matches '%s'%s.", station,
                if (is.null(state)) "" else sprintf(" in state '%s'", state)))
  if (length(hits) > 1L)
    stop(sprintf(
      "'%s' matches several stations: %s. Use `state` to disambiguate.",
      station,
      paste(sprintf("%s (%s, %s)", cand$name[hits], cand$stateCode[hits],
                    cand$stationTriplet[hits]), collapse = "; ")))
  cand[hits, , drop = FALSE]
}

.snotel_base_url <- "https://wcc.sc.egov.usda.gov/awdbRestApi/services/v1"

.snotel_http_get <- function(url, dest) {
  utils::download.file(url, dest, mode = "wb", quiet = TRUE)
}

# Any non-2xx response is a real failure here: unlike CRN, "no data" comes
# back as HTTP 200 with an empty JSON array, never a 404.
.snotel_download <- function(url, dest, retries = 1) {
  old <- options(timeout = max(600, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  res <- NULL
  for (attempt in seq_len(retries + 1L)) {
    res <- tryCatch(.snotel_http_get(url, dest), error = function(e) e)
    if (!inherits(res, "error")) return(invisible(NULL))
    unlink(dest)
  }
  stop(sprintf("Failed to download %s: %s", url, conditionMessage(res)))
}

.snotel_stations <- function() {
  dest <- tempfile(fileext = ".json")
  on.exit(unlink(dest), add = TRUE)
  url <- sprintf("%s/stations?activeOnly=false", .snotel_base_url)
  .snotel_download(url, dest)
  .snotel_parse_stations(dest)
}
