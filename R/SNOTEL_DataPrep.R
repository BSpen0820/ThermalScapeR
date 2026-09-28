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

.snotel_nearest_station <- function(stations, lon, lat, periods, network, try_fn) {
  cand <- stations
  if (!is.null(network)) {
    unknown <- setdiff(network, unique(stations$networkCode))
    if (length(unknown) > 0L)
      stop(sprintf("Unknown network(s): %s. Valid values: %s.",
                   paste(unknown, collapse = ", "),
                   paste(sort(unique(stations$networkCode)), collapse = ", ")))
    cand <- cand[cand$networkCode %in% network, , drop = FALSE]
  }
  overlaps <- vapply(seq_len(nrow(cand)), function(i)
    all(cand$beginDate[i] <= periods$end & cand$endDate[i] >= periods$start), logical(1))
  cand <- cand[overlaps, , drop = FALSE]
  if (nrow(cand) == 0L)
    stop(sprintf(
      "No station in network(s) %s has a period of record covering every requested period.",
      if (is.null(network)) "<any>" else paste(network, collapse = ", ")))
  cand$dist_km <- .station_haversine(lon, lat, cand$longitude, cand$latitude)
  cand <- cand[order(cand$dist_km), , drop = FALSE]
  for (i in seq_len(nrow(cand))) {
    if (try_fn(cand[i, , drop = FALSE])) return(cand[i, , drop = FALSE])
    warning(sprintf("Station %s (%s) returned no data; trying the next-nearest station.",
                    cand$name[i], cand$stationTriplet[i]))
  }
  stop("No candidate station returned data for the requested period(s).")
}

.snotel_write_period_csv <- function(df, out_dir, station_id, label) {
  out <- df
  fmt <- if (inherits(out$Date_Time, "POSIXct")) "%Y-%m-%d %H:%M:%S" else "%Y-%m-%d"
  out$Date_Time <- format(out$Date_Time, fmt)
  path <- file.path(out_dir, sprintf("%s_%s.csv", station_id, label))
  readr::write_csv(out, path)
  invisible(path)
}

#' Download NRCS SNOTEL station data
#'
#' Downloads daily or hourly Snow Telemetry (SNOTEL) network observations,
#' via the NRCS AWDB REST API, for a named station, or for the nearest
#' station with data to a coordinate, for one or more date periods. Every
#' element the station reports is returned by default. Data is returned
#' wide, one column per element (or per element/depth combination), with a
#' \code{Date_Time} column.
#'
#' @details
#' \strong{Data source.} NRCS AWDB REST API
#' (\url{https://wcc.sc.egov.usda.gov/awdbRestApi}). Unlike
#' \code{\link{get_NOAACRN_data}}, there are no year files to download and
#' delete: each request returns exactly the requested station, elements,
#' and date range, so nothing raw is ever written to disk beyond a
#' temporary station-table fetch.
#'
#' \strong{Elements.} \code{elements = "*"} (the default) returns every
#' element the resolved station reports; an explicit character vector
#' (e.g. \code{c("WTEQ", "SNWD")}) is automatically qualified as
#' \code{"WTEQ:*:*,SNWD:*:*"} when querying the API, so elements with
#' multiple sensor depths (e.g. soil temperature/moisture) are not silently
#' dropped. Which elements exist varies by station.
#'
#' \strong{Wide-format columns.} An element with no sensor depth (e.g.
#' \code{WTEQ}) gets a plain column name. An element reported at one or
#' more depths (e.g. \code{STO} for soil temperature) gets one column per
#' depth, named \code{ELEMENT_N####} (below the surface) or
#' \code{ELEMENT_P####} (at or above it), where \code{####} is the depth in
#' millimetres, zero-padded. A second sensor at the same element and depth
#' (rare) appends \code{_oN}. Each element column is followed by a
#' \code{_qcFlag} column, and a \code{_qaFlag} column when any value in the
#' period has one (common for 2022 and later data).
#'
#' \strong{Station selection.} With \code{station}, an exact name (e.g.
#' \code{"Banner Summit"}) or a unique partial match is used; \code{state}
#' (a 2-letter code) disambiguates a name that exists in more than one
#' state. With \code{coords}, stations in \code{network} are ranked by
#' great-circle distance; only stations whose period of record covers
#' \emph{every} requested period are considered, and the nearest one that
#' actually returns data is used, trying progressively farther candidates
#' otherwise. All periods in one call use the same station. \code{network}
#' defaults to \code{c("SNTL", "SNTLT")}, the two automated SNOTEL
#' networks; other values in the station table (\code{SCAN}, \code{SNOW},
#' \code{COOP}, \code{USGS}, \code{BOR}, and others) are different networks
#' with different sensors and are excluded unless requested.
#'
#' \strong{Time.} \code{Date_Time} is a \code{Date} for \code{duration =
#' "DAILY"} or a \code{POSIXct} in the station's own fixed UTC offset for
#' \code{duration = "HOURLY"} (SNOTEL timestamps do not observe daylight
#' saving). Every day (or hour) in the requested period appears as a row,
#' even when every element is \code{NA} that day.
#'
#' \strong{Failures.} For a length-2 \code{Date} vector, a failed period
#' stops the call. For a \code{data.frame} of dates, a period that fails
#' (e.g. the station has no data at all in that window) is skipped with a
#' warning and its list element is \code{NULL}, so the other periods are
#' still returned and written; the call stops only if every period fails.
#'
#' \strong{Files.} If \code{out_dir} is given, one CSV per period is
#' written as \code{{stationId}_{period_label}.csv}; existing files are
#' replaced. \code{Date_Time} is written as local-time text.
#'
#' Unlike most functions in this package, the data is returned visibly.
#'
#' @param station Character. Station name, e.g. \code{"Banner Summit"}.
#'   Supply exactly one of \code{station} or \code{coords}.
#' @param state Optional 2-letter USPS state/province code to disambiguate
#'   a \code{station} name that exists in more than one state. Ignored
#'   when \code{coords} is used.
#' @param coords Location used to find the nearest station: a numeric
#'   \code{c(lon, lat)} in WGS84, an \code{sf}/\code{sfc} object or
#'   \code{SpatVector} (reprojected to WGS84; the centroid is used for
#'   non-point geometry), or a \code{SpatRaster} (centroid of its extent).
#' @param dates Either a length-2 \code{Date} vector (one period) or a
#'   \code{data.frame} with \code{Date} columns \code{Start_Dates} and
#'   \code{End_Dates} (one row per period). Day-exact.
#' @param elements Character vector of AWDB element codes, or \code{"*"}
#'   (default) for every element the station reports.
#' @param duration Character. \code{"DAILY"} (default) or \code{"HOURLY"}.
#' @param network Character vector of AWDB networks considered when
#'   resolving \code{coords}: default \code{c("SNTL", "SNTLT")}, or
#'   \code{NULL} for all networks in the station table.
#' @param out_dir Optional directory. If given (created if missing), one
#'   CSV per period is written there.
#' @param max_dist_km Numeric. A warning is issued when the station chosen
#'   from \code{coords} is farther than this many kilometres. Default 100.
#'
#' @return For a length-2 \code{Date} vector, a \code{data.frame} with one
#'   row per day (or hour) and one column per element/depth. For a
#'   \code{data.frame} of dates, a named list of such data.frames, one per
#'   row, named by period label (\code{YYYYMMDD_to_YYYYMMDD}), even when
#'   there is one row.
#'
#' @seealso \url{https://www.nrcs.usda.gov/resources/data-and-reports/air-water-database}
#'   for the AWDB REST API, and \code{\link{get_NOAACRN_data}} for the
#'   parallel USCRN function.
#'
#' @examples
#' \dontrun{
#' # One period, by station name, every element
#' banner <- get_SNOTEL_data(
#'   station = "Banner Summit",
#'   dates = as.Date(c("2024-01-01", "2024-03-31"))
#' )
#'
#' # Nearest station to a point, hourly snow water equivalent only
#' pt <- get_SNOTEL_data(coords = c(-115.23, 44.30),
#'                       dates = as.Date(c("2024-01-01", "2024-01-07")),
#'                       elements = "WTEQ", duration = "HOURLY")
#' }
#' @export
get_SNOTEL_data <- function(station = NULL, state = NULL, coords = NULL, dates,
                            elements = "*", duration = "DAILY",
                            network = c("SNTL", "SNTLT"), out_dir = NULL,
                            max_dist_km = 100) {
  if (is.null(station) == is.null(coords))
    stop("Supply exactly one of `station` or `coords`.")
  if (!is.null(station) &&
      (!is.character(station) || length(station) != 1L || is.na(station)))
    stop("`station` must be a single character string.")
  if (!duration %in% c("DAILY", "HOURLY"))
    stop("`duration` must be \"DAILY\" or \"HOURLY\".")
  per <- .station_normalize_dates(dates)
  periods <- per$periods

  stations <- .snotel_stations()

  if (!is.null(station)) {
    st <- .snotel_resolve_station(stations, station, state)
    message(sprintf("Using station %s (%s, %s).", st$name, st$stationTriplet, st$networkCode))
  } else {
    ll <- .station_coords_to_lonlat(coords)
    try_fn <- function(cand) {
      df <- .snotel_fetch(cand$stationTriplet, elements, duration,
                          periods$start[1L], periods$end[1L])
      nrow(df) > 0L
    }
    st <- .snotel_nearest_station(stations, ll[["lon"]], ll[["lat"]], periods, network, try_fn)
    message(sprintf("Using station %s (%s, %s), %.1f km from the requested location.",
                    st$name, st$stationTriplet, st$networkCode, st$dist_km))
    if (st$dist_km > max_dist_km)
      warning(sprintf("Nearest station with data is %.1f km away (max_dist_km = %s).",
                      st$dist_km, format(max_dist_km)))
  }

  if (!is.null(out_dir))
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  tz <- sprintf("Etc/GMT%+d", -round(st$dataTimeZone))

  run_period <- function(i) {
    start <- periods$start[i]
    end <- periods$end[i]
    label <- periods$label[i]
    long_df <- .snotel_fetch(st$stationTriplet, elements, duration, start, end)
    if (nrow(long_df) == 0L)
      stop(sprintf("Period %s: no data returned for station %s.", label, st$stationTriplet))
    df <- .snotel_pivot_wide(long_df, start, end, duration, tz)
    if (!is.null(out_dir))
      .snotel_write_period_csv(df, out_dir, st$stationId, label)
    df
  }

  results <- vector("list", nrow(periods))
  names(results) <- periods$label
  for (i in seq_len(nrow(periods))) {
    label <- periods$label[i]
    df <- if (per$is_df) {
      tryCatch(run_period(i), error = function(e) {
        warning(sprintf("Period %s skipped: %s", label,
                        sub("^Period [^:]+: ", "", conditionMessage(e))),
                call. = FALSE)
        NULL
      })
    } else {
      run_period(i)
    }
    results[i] <- list(df)
  }

  if (per$is_df && all(vapply(results, is.null, logical(1))))
    stop("All periods failed; see the warnings above.")

  if (per$is_df) results else results[[1L]]
}
