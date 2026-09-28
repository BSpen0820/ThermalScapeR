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
