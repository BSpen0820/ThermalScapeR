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

.snotel_elements_param <- function(elements) {
  if (identical(elements, "*")) return("*")
  paste(sprintf("%s:*:*", elements), collapse = ",")
}

.snotel_duration_bounds <- function(start, end, duration) {
  if (identical(duration, "HOURLY")) {
    list(begin = sprintf("%s 00:00", format(start, "%Y-%m-%d")),
         end   = sprintf("%s 23:00", format(end, "%Y-%m-%d")))
  } else {
    list(begin = format(start, "%Y-%m-%d"), end = format(end, "%Y-%m-%d"))
  }
}

.snotel_empty_fetch <- function() {
  data.frame(Date_Time = character(), elementCode = character(),
            heightDepth = numeric(), ordinal = integer(), value = numeric(),
            qcFlag = character(), qaFlag = character(), stringsAsFactors = FALSE)
}

.snotel_fetch <- function(triplet, elements, duration, start, end) {
  bounds <- .snotel_duration_bounds(start, end, duration)
  url <- sprintf(
    "%s/data?stationTriplets=%s&elements=%s&duration=%s&beginDate=%s&endDate=%s&returnFlags=true",
    .snotel_base_url, triplet, .snotel_elements_param(elements), duration,
    utils::URLencode(bounds$begin), utils::URLencode(bounds$end))
  dest <- tempfile(fileext = ".json")
  on.exit(unlink(dest), add = TRUE)
  .snotel_download(url, dest)
  raw <- jsonlite::fromJSON(dest, simplifyVector = FALSE)
  if (length(raw) == 0L || length(raw[[1]]$data) == 0L)
    return(.snotel_empty_fetch())
  rows <- lapply(raw[[1]]$data, function(el) {
    se <- el$stationElement
    hd <- if (is.null(se$heightDepth)) NA_real_ else as.numeric(se$heightDepth)
    ord <- if (is.null(se$ordinal)) 1L else as.integer(se$ordinal)
    vals <- el$values
    data.frame(
      Date_Time = vapply(vals, function(v) v$date, character(1)),
      elementCode = se$elementCode,
      heightDepth = hd,
      ordinal = ord,
      value = vapply(vals, function(v) if (is.null(v$value)) NA_real_ else as.numeric(v$value), numeric(1)),
      qcFlag = vapply(vals, function(v) if (is.null(v$qcFlag)) NA_character_ else v$qcFlag, character(1)),
      qaFlag = vapply(vals, function(v) if (is.null(v$qaFlag)) NA_character_ else v$qaFlag, character(1)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

.snotel_depth_label <- function(element_code, height_depth, ordinal = 1L) {
  stem <- if (is.na(height_depth)) {
    element_code
  } else {
    sign <- if (height_depth < 0) "N" else "P"
    mm <- sprintf("%04d", round(abs(height_depth) * 25.4))
    sprintf("%s_%s%s", element_code, sign, mm)
  }
  if (!is.na(ordinal) && ordinal != 1L) stem <- sprintf("%s_o%d", stem, ordinal)
  stem
}

.snotel_pivot_wide <- function(long_df, start, end, duration, tz) {
  if (identical(duration, "HOURLY")) {
    skeleton <- seq(as.POSIXct(sprintf("%s 00:00:00", format(start, "%Y-%m-%d")), tz = tz),
                    as.POSIXct(sprintf("%s 23:00:00", format(end, "%Y-%m-%d")), tz = tz),
                    by = "hour")
    parsed_dt <- as.POSIXct(long_df$Date_Time, format = "%Y-%m-%d %H:%M", tz = tz)
  } else {
    skeleton <- seq(start, end, by = "day")
    parsed_dt <- as.Date(long_df$Date_Time)
  }
  out <- data.frame(Date_Time = skeleton)

  if (nrow(long_df) > 0L) {
    long_df$Date_Time <- parsed_dt
    long_df$col <- mapply(.snotel_depth_label, long_df$elementCode, long_df$heightDepth,
                          long_df$ordinal)
    for (col in unique(long_df$col)) {
      sub <- long_df[long_df$col == col, , drop = FALSE]
      out[[col]] <- sub$value[match(out$Date_Time, sub$Date_Time)]
      out[[paste0(col, "_qcFlag")]] <- sub$qcFlag[match(out$Date_Time, sub$Date_Time)]
      if (any(!is.na(sub$qaFlag)))
        out[[paste0(col, "_qaFlag")]] <- sub$qaFlag[match(out$Date_Time, sub$Date_Time)]
    }
  }
  out
}
