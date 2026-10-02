# --------------------------------------------------------------------------- #
#  NOAA USCRN sub-hourly station data
# --------------------------------------------------------------------------- #

.crn_year <- function(d) as.integer(format(d, "%Y"))

.crn_period_label <- function(start, end) {
  sprintf("%s_to_%s", format(start, "%Y%m%d"), format(end, "%Y%m%d"))
}

# Required years are those the period touches. Padding years cover the
# end-of-interval stamps that spill into the neighbouring UTC-year file.
.crn_years_needed <- function(start, end, lon) {
  required <- seq(.crn_year(start), .crn_year(end))
  padding <- integer()
  if (lon <= 0 && format(end, "%m-%d") == "12-31")
    padding <- .crn_year(end) + 1L
  if (lon > 0 && format(start, "%m-%d") == "01-01")
    padding <- c(.crn_year(start) - 1L, padding)
  list(required = required, padding = padding)
}

.crn_ascii <- function(x) {
  y <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  y[is.na(y)] <- x[is.na(y)]
  y
}

.crn_norm_name <- function(x) {
  tolower(gsub("[[:space:]_]+", "_", trimws(.crn_ascii(x))))
}

.crn_parse_stations <- function(path) {
  st <- readr::read_tsv(path,
                        col_types = readr::cols(.default = readr::col_character()),
                        na = character(), quote = "", progress = FALSE)
  st <- as.data.frame(st, stringsAsFactors = FALSE)
  for (col in c("COMMISSIONING", "CLOSING")) {
    x <- substr(st[[col]], 1L, 10L)
    x[!nzchar(x)] <- NA_character_
    st[[col]] <- as.Date(x, format = "%Y-%m-%d")
  }
  for (col in c("LATITUDE", "LONGITUDE", "ELEVATION"))
    st[[col]] <- suppressWarnings(as.numeric(st[[col]]))
  st$STATION_DIR <- gsub("[[:space:]]+", "_",
                         paste(trimws(st$STATE), trimws(.crn_ascii(st$LOCATION)),
                               trimws(st$VECTOR), sep = "_"))
  st <- st[order(!grepl("^[0-9]+$", st$WBAN)), , drop = FALSE]
  st <- st[!duplicated(st$STATION_DIR), , drop = FALSE]
  rownames(st) <- NULL
  st
}

.crn_resolve_station <- function(stations, station) {
  key <- .crn_norm_name(stations$STATION_DIR)
  q <- .crn_norm_name(station)
  exact <- which(key == q)
  if (length(exact) == 1L) return(stations[exact, , drop = FALSE])
  hits <- which(grepl(q, key, fixed = TRUE))
  if (length(hits) == 0L)
    stop(sprintf("No station matches '%s'.", station))
  if (length(hits) > 1L)
    stop(sprintf("'%s' matches several stations: %s. Use a full station name.",
                 station, paste(stations$STATION_DIR[hits], collapse = ", ")))
  stations[hits, , drop = FALSE]
}

.crn_parse_listing <- function(lines, year) {
  prefix <- sprintf("CRNS0101-05-%d-", as.integer(year))
  pat <- sprintf("%s[^\"<>/ ]+\\.txt", prefix)
  m <- unlist(regmatches(lines, gregexpr(pat, lines)))
  if (length(m) == 0L) return(character())
  sort(unique(sub("\\.txt$", "", substring(m, nchar(prefix) + 1L))))
}

.crn_check_station_years <- function(station_dir, years, listings) {
  has <- vapply(as.character(years),
                function(y) station_dir %in% listings[[y]], logical(1))
  if (!any(has))
    stop(sprintf("Station %s has no sub-hourly file for any requested year (%s).",
                 station_dir, paste(years, collapse = ", ")))
  if (!all(has))
    warning(sprintf("Station %s has no sub-hourly file for year(s): %s.",
                    station_dir, paste(years[!has], collapse = ", ")))
  invisible(NULL)
}

.crn_nearest_station <- function(stations, lon, lat, years, network, listings) {
  cand <- stations
  if (!is.null(network)) {
    unknown <- setdiff(network, unique(stations$NETWORK))
    if (length(unknown) > 0L)
      stop(sprintf("Unknown network(s): %s. Valid values: %s.",
                   paste(unknown, collapse = ", "),
                   paste(sort(unique(stations$NETWORK)), collapse = ", ")))
    cand <- cand[cand$NETWORK %in% network, , drop = FALSE]
  }
  has_all <- vapply(cand$STATION_DIR, function(d)
    all(vapply(as.character(years), function(y) d %in% listings[[y]], logical(1))),
    logical(1))
  cand <- cand[has_all, , drop = FALSE]
  if (nrow(cand) == 0L)
    stop(sprintf("No station in network(s) %s has sub-hourly data for every year of %s.",
                 if (is.null(network)) "<any>" else paste(network, collapse = ", "),
                 paste(years, collapse = ", ")))
  cand$dist_km <- .station_haversine(lon, lat, cand$LONGITUDE, cand$LATITUDE)
  cand[which.min(cand$dist_km), , drop = FALSE]
}

.crn_base_url <- "https://www.ncei.noaa.gov/pub/data/uscrn/products"

.crn_http_get <- function(url, dest) {
  utils::download.file(url, dest, mode = "wb", quiet = TRUE)
}

# Returns "ok" or "not_found"; retries other failures, then stops.
.crn_download <- function(url, dest, retries = 1) {
  old <- options(timeout = max(600, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  res <- NULL
  for (attempt in seq_len(retries + 1L)) {
    msgs <- character()
    res <- tryCatch(
      withCallingHandlers(
        .crn_http_get(url, dest),
        warning = function(w) {
          msgs <<- c(msgs, conditionMessage(w))
          invokeRestart("muffleWarning")
        }),
      error = function(e) e)
    if (!inherits(res, "error")) return("ok")
    unlink(dest)
    if (grepl("HTTP status was '404", paste(c(msgs, conditionMessage(res)), collapse = " "), fixed = TRUE))
      return("not_found")
  }
  stop(sprintf("Failed to download %s: %s", url, conditionMessage(res)))
}

.crn_download_year <- function(station_dir, year, tmp) {
  fname <- sprintf("CRNS0101-05-%d-%s.txt", as.integer(year), station_dir)
  dest <- file.path(tmp, fname)
  if (file.exists(dest)) return(dest)
  url <- sprintf("%s/subhourly01/%d/%s", .crn_base_url, as.integer(year), fname)
  if (identical(.crn_download(url, dest), "not_found")) return(NULL)
  dest
}

.crn_year_listing <- function(year) {
  dest <- tempfile(fileext = ".html")
  on.exit(unlink(dest), add = TRUE)
  url <- sprintf("%s/subhourly01/%d/", .crn_base_url, as.integer(year))
  if (identical(.crn_download(url, dest), "not_found")) return(character())
  .crn_parse_listing(readLines(dest, warn = FALSE), year)
}

.crn_stations <- function() {
  dest <- tempfile(fileext = ".tsv")
  on.exit(unlink(dest), add = TRUE)
  url <- sprintf("%s/stations.tsv", .crn_base_url)
  if (identical(.crn_download(url, dest), "not_found"))
    stop("Could not find the USCRN station table at ", url)
  .crn_parse_stations(dest)
}

.crn_subhourly_cols <- c(
  "WBANNO", "UTC_DATE", "UTC_TIME", "LST_DATE", "LST_TIME", "CRX_VN",
  "LONGITUDE", "LATITUDE", "AIR_TEMPERATURE", "PRECIPITATION",
  "SOLAR_RADIATION", "SR_FLAG", "SURFACE_TEMPERATURE", "ST_TYPE", "ST_FLAG",
  "RELATIVE_HUMIDITY", "RH_FLAG", "SOIL_MOISTURE_5", "SOIL_TEMPERATURE_5",
  "WETNESS", "WET_FLAG", "WIND_1_5", "WIND_FLAG"
)

.crn_col_types <- function() {
  readr::cols(
    WBANNO = readr::col_character(), UTC_DATE = readr::col_character(),
    UTC_TIME = readr::col_character(), LST_DATE = readr::col_character(),
    LST_TIME = readr::col_character(), CRX_VN = readr::col_character(),
    LONGITUDE = readr::col_double(), LATITUDE = readr::col_double(),
    AIR_TEMPERATURE = readr::col_double(), PRECIPITATION = readr::col_double(),
    SOLAR_RADIATION = readr::col_double(), SR_FLAG = readr::col_integer(),
    SURFACE_TEMPERATURE = readr::col_double(), ST_TYPE = readr::col_character(),
    ST_FLAG = readr::col_integer(), RELATIVE_HUMIDITY = readr::col_double(),
    RH_FLAG = readr::col_integer(), SOIL_MOISTURE_5 = readr::col_double(),
    SOIL_TEMPERATURE_5 = readr::col_double(), WETNESS = readr::col_double(),
    WET_FLAG = readr::col_integer(), WIND_1_5 = readr::col_double(),
    WIND_FLAG = readr::col_integer()
  )
}

.crn_sentinels <- c(
  AIR_TEMPERATURE = -9999, PRECIPITATION = -9999, SURFACE_TEMPERATURE = -9999,
  SOIL_TEMPERATURE_5 = -9999, RELATIVE_HUMIDITY = -9999, WETNESS = -9999,
  SOLAR_RADIATION = -99999, SOIL_MOISTURE_5 = -99, WIND_1_5 = -99
)

.crn_sensor_cols <- c("SOLAR_RADIATION", "SURFACE_TEMPERATURE", "RELATIVE_HUMIDITY",
                      "SOIL_MOISTURE_5", "SOIL_TEMPERATURE_5", "WIND_1_5")

.crn_read_file <- function(path) {
  df <- suppressWarnings(
    readr::read_table(path, col_names = .crn_subhourly_cols,
                      col_types = .crn_col_types(), progress = FALSE))
  if (nrow(readr::problems(df)) > 0L)
    warning(sprintf("Parsing problems in %s; see readr::problems().", basename(path)))
  df
}

.crn_lst_tz <- function(df) {
  fmt <- "%Y%m%d %H%M"
  utc <- as.POSIXct(paste(df$UTC_DATE, df$UTC_TIME), format = fmt, tz = "UTC")
  lst <- as.POSIXct(paste(df$LST_DATE, df$LST_TIME), format = fmt, tz = "UTC")
  off <- round(as.numeric(difftime(lst, utc, units = "hours")))
  off <- off[!is.na(off)]
  if (length(off) == 0L)
    stop("Cannot derive the local standard time offset from the data.")
  mode_off <- as.integer(names(which.max(table(off))))
  if (abs(mode_off) > 14L)
    stop(sprintf("Implausible local standard time offset: %d hours.", mode_off))
  sprintf("Etc/GMT%+d", -mode_off)
}

.crn_mask_sentinels <- function(df) {
  for (col in intersect(names(.crn_sentinels), names(df))) {
    x <- df[[col]]
    x[!is.na(x) & x == .crn_sentinels[[col]]] <- NA
    df[[col]] <- x
  }
  df
}

.crn_finalize <- function(df, start, end, tz, missing_to_na, label) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df$Date_Time <- as.POSIXct(paste(df$LST_DATE, df$LST_TIME),
                             format = "%Y%m%d %H%M", tz = tz)
  lo <- as.POSIXct(sprintf("%s 00:00:00", format(start, "%Y-%m-%d")), tz = tz)
  hi <- as.POSIXct(sprintf("%s 00:00:00", format(end + 1, "%Y-%m-%d")), tz = tz)
  df <- df[!is.na(df$Date_Time) & df$Date_Time > lo & df$Date_Time <= hi, , drop = FALSE]
  if (nrow(df) == 0L)
    stop(sprintf("Period %s: no observations in the requested window.", label))
  df <- df[order(df$Date_Time), , drop = FALSE]
  rownames(df) <- NULL

  masked <- .crn_mask_sentinels(df)
  n_expected <- 288L * (as.integer(end - start) + 1L)
  if (nrow(df) < n_expected)
    warning(sprintf("Period %s: expected %d observations but found %d (%d missing).",
                    label, n_expected, nrow(df), n_expected - nrow(df)))
  all_na <- .crn_sensor_cols[vapply(.crn_sensor_cols,
                                    function(cn) all(is.na(masked[[cn]])), logical(1))]
  if (length(all_na) > 0L)
    warning(sprintf("Period %s: no valid data for %s.", label,
                    paste(all_na, collapse = ", ")))
  if (missing_to_na) masked else df
}

.crn_fetch_years <- function(station_dir, yrs, tmp, label) {
  fetch <- function(y) .crn_download_year(station_dir, y, tmp)
  req <- lapply(yrs$required, fetch)
  missing <- yrs$required[vapply(req, is.null, logical(1))]
  if (length(missing) > 0L)
    warning(sprintf("Period %s: no data file for year(s) %s.", label,
                    paste(missing, collapse = ", ")))
  pad <- lapply(yrs$padding, fetch)
  files <- Filter(Negate(is.null), c(req, pad))
  if (length(files) == 0L)
    stop(sprintf("Period %s: no data files could be downloaded for station %s.",
                 label, station_dir))
  unlist(files, use.names = FALSE)
}

.crn_write_period_csv <- function(df, out_dir, station_dir, label) {
  out <- df
  out$Date_Time <- format(out$Date_Time, "%Y-%m-%d %H:%M:%S")
  path <- file.path(out_dir, sprintf("%s_%s.csv", station_dir, label))
  readr::write_csv(out, path)
  invisible(path)
}

#' Download NOAA USCRN sub-hourly station data
#'
#' Downloads 5-minute U.S. Climate Reference Network (USCRN) observations for a
#' named station, or for the nearest station with data to a coordinate, for one
#' or more date periods. Column names are attached, a local-standard-time
#' \code{Date_Time} column is added, and each period is trimmed to the
#' requested days. Raw NOAA files are downloaded to a temporary directory and
#' always deleted; only the final, header-amended data is returned or written.
#'
#' @details
#' \strong{Data source.} NOAA NCEI \code{subhourly01} product
#' (\url{https://www.ncei.noaa.gov/pub/data/uscrn/products/subhourly01/}). Year
#' files are organised by UTC year; the function fetches only the year files
#' each period needs, plus the neighbouring year when a period ends on 31 Dec
#' (stations west of Greenwich) or starts on 1 Jan (stations east of
#' Greenwich). Each year file is downloaded once per call and shared across
#' periods.
#'
#' \strong{Station selection.} With \code{station}, an exact directory name
#' (e.g. \code{"AK_Kenai_29_ENE"}) or a unique partial match (e.g.
#' \code{"Kenai"}) is used; matching ignores case and treats spaces and
#' underscores alike. With \code{coords}, stations in \code{network} are ranked
#' by great-circle distance and the closest one that has a data file for every
#' year the periods touch is used; stations without data are skipped. All
#' periods in one call use the same station. The station table has three
#' \code{NETWORK} values: \code{"USCRN"} (the core network with the full sensor
#' suite), \code{"USRCRN"} and \code{"Alabama-USRCRN"}, which lack most
#' sensors.
#'
#' \strong{Time.} \code{Date_Time} is \code{POSIXct} in Local Standard Time
#' (no daylight saving), with the zone derived from the station's own
#' \code{LST} and \code{UTC} columns (e.g. \code{Etc/GMT+9} for Alaska Kenai).
#' It marks the \emph{end} of each 5-minute interval, so a \code{00:00} row
#' belongs to the previous day's data and \code{as.Date(Date_Time)} places it
#' in the next day. Each period is trimmed to
#' \code{(start 00:00, end + 1 day 00:00]}, giving 288 rows per day. A warning
#' is issued when fewer rows are found.
#'
#' \strong{Dates are day-exact.} Unlike \code{\link{run_micro_big_nichemap}} and
#' \code{\link{package_climate}}, which snap periods to whole months, the exact
#' start and end days are kept, so period labels look like
#' \code{20240101_to_20240331}.
#'
#' \strong{Missing values.} With \code{missing_to_na = TRUE}, NOAA sentinels
#' become \code{NA}: -9999 for air, surface and soil temperature, precipitation,
#' relative humidity and wetness; -99999 for solar radiation; -99 for soil
#' moisture and wind speed. QC flag columns are kept, but values are not
#' masked by flag (a flag of 3 does not always come with a sentinel). The text
#' column \code{CRX_VN} (datalogger version) also uses \code{-9.000} for
#' missing; it is left as-is. A warning
#' names any sensor column with no valid data in a period.
#'
#' \strong{Failures.} For a length-2 \code{Date} vector, a failed period stops
#' the call. For a \code{data.frame} of dates, a period that fails (for
#' example, the station has no file for that year) is skipped with a warning
#' and its list element is \code{NULL}, so the other periods are still
#' returned and written; the call stops only if every period fails.
#'
#' \strong{Files.} If \code{out_dir} is given, one CSV per period is written
#' as \code{{STATION_DIR}_{period_label}.csv}; existing files are replaced.
#' \code{Date_Time} is written as local-time text (\code{\%Y-\%m-\%d \%H:\%M:\%S}).
#' The date and time columns are text with leading zeros (e.g. \code{"0005"}),
#' so read the CSV back with those columns as character.
#'
#' Unlike most functions in this package, the data is returned visibly.
#'
#' @param station Character. Station name, e.g. \code{"AK_Kenai_29_ENE"} or
#'   \code{"Kenai"}. Supply exactly one of \code{station} or \code{coords}. No
#'   \code{network} filter is applied to an explicit name.
#' @param coords Location used to find the nearest station: a numeric
#'   \code{c(lon, lat)} in WGS84, an \code{sf}/\code{sfc} object or
#'   \code{SpatVector} (reprojected to WGS84; the centroid is used for
#'   non-point geometry), or a \code{SpatRaster} (centroid of its extent).
#' @param dates Either a length-2 \code{Date} vector (one period) or a
#'   \code{data.frame} with \code{Date} columns \code{Start_Dates} and
#'   \code{End_Dates} (one row per period). Day-exact.
#' @param network Character vector of networks considered when resolving
#'   \code{coords}: \code{"USCRN"} (default), \code{"USRCRN"},
#'   \code{"Alabama-USRCRN"}, several of these, or \code{NULL} for all.
#' @param out_dir Optional directory. If given (created if missing), one CSV per
#'   period is written there.
#' @param missing_to_na Logical. Convert NOAA missing-value sentinels to
#'   \code{NA}. Default \code{TRUE}.
#' @param max_dist_km Numeric. A warning is issued when the station chosen from
#'   \code{coords} is farther than this many kilometres. Default 100.
#'
#' @return For a length-2 \code{Date} vector, a \code{data.frame} with the 23 CRN
#'   sub-hourly columns plus \code{Date_Time}. For a \code{data.frame} of dates,
#'   a named list of such data.frames, one per row, named by period label
#'   (\code{YYYYMMDD_to_YYYYMMDD}), even when there is one row.
#'
#' @seealso \url{https://www.ncei.noaa.gov/access/crn/} for the CRN network
#'   documentation, and \code{\link{package_climate}} for the AORC-based climate
#'   pipeline.
#'
#' @examples
#' \dontrun{
#' # One period, by station name
#' kenai <- get_NOAACRN_data(
#'   station = "AK_Kenai_29_ENE",
#'   dates = as.Date(c("2024-01-01", "2024-03-31"))
#' )
#'
#' # Several periods, nearest station to a point, saved as CSVs
#' periods <- data.frame(
#'   Start_Dates = as.Date(c("2023-10-01", "2024-10-01")),
#'   End_Dates   = as.Date(c("2024-03-31", "2025-03-31"))
#' )
#' res <- get_NOAACRN_data(coords = c(-150.44, 60.72), dates = periods,
#'                         out_dir = "D:/Data/NOAA_CRN")
#' names(res)
#' }
#' @export
get_NOAACRN_data <- function(station = NULL, coords = NULL, dates,
                             network = "USCRN", out_dir = NULL,
                             missing_to_na = TRUE, max_dist_km = 100) {
  if (is.null(station) == is.null(coords))
    stop("Supply exactly one of `station` or `coords`.")
  if (!is.null(station) &&
      (!is.character(station) || length(station) != 1L || is.na(station)))
    stop("`station` must be a single character string.")
  per <- .station_normalize_dates(dates)
  periods <- per$periods

  tmp <- tempfile("crn_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  stations <- .crn_stations()
  req_years <- sort(unique(unlist(lapply(seq_len(nrow(periods)), function(i)
    seq(.crn_year(periods$start[i]), .crn_year(periods$end[i]))))))
  listings <- stats::setNames(lapply(req_years, .crn_year_listing),
                              as.character(req_years))

  if (!is.null(station)) {
    st <- .crn_resolve_station(stations, station)
    .crn_check_station_years(st$STATION_DIR, req_years, listings)
    message(sprintf("Using station %s (%s).", st$STATION_DIR, st$NETWORK))
  } else {
    ll <- .station_coords_to_lonlat(coords)
    st <- .crn_nearest_station(stations, ll[["lon"]], ll[["lat"]], req_years,
                               network, listings)
    message(sprintf("Using station %s (%s), %.1f km from the requested location.",
                    st$STATION_DIR, st$NETWORK, st$dist_km))
    if (st$dist_km > max_dist_km)
      warning(sprintf("Nearest station with data is %.1f km away (max_dist_km = %s).",
                      st$dist_km, format(max_dist_km)))
  }
  station_dir <- st$STATION_DIR
  station_lon <- st$LONGITUDE

  if (!is.null(out_dir))
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  results <- vector("list", nrow(periods))
  names(results) <- periods$label
  run_period <- function(i) {
    start <- periods$start[i]
    end <- periods$end[i]
    label <- periods$label[i]
    yrs <- .crn_years_needed(start, end, station_lon)
    files <- .crn_fetch_years(station_dir, yrs, tmp, label)
    raw <- do.call(rbind, lapply(files, .crn_read_file))
    tz <- .crn_lst_tz(raw)
    df <- .crn_finalize(raw, start, end, tz, missing_to_na, label)
    if (!is.null(out_dir))
      .crn_write_period_csv(df, out_dir, station_dir, label)
    rm(raw)
    gc()
    df
  }

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

# 5-min CRN stamps are interval ENDS, so stamps in (H-1:00, H:00] are labelled
# H:00 -- the same convention as ERA5 accumulated fluxes. An hour needs >= 9 of
# 12 valid values; precip is scaled by 12 / n_valid for partly missing hours.
.crn_hourly <- function(crn, min_valid = 9L) {
  need <- c("UTC_DATE", "UTC_TIME", "AIR_TEMPERATURE", "RELATIVE_HUMIDITY",
            "SOLAR_RADIATION", "SR_FLAG", "WIND_1_5", "PRECIPITATION")
  miss <- setdiff(need, names(crn))
  if (length(miss) > 0L)
    stop(sprintf("crn is missing column(s): %s", paste(miss, collapse = ", ")))

  crn <- .crn_mask_sentinels(as.data.frame(crn, stringsAsFactors = FALSE))
  t5 <- as.POSIXct(
    sprintf("%.0f %04d", as.numeric(crn$UTC_DATE), as.integer(crn$UTC_TIME)),
    format = "%Y%m%d %H%M", tz = "UTC")
  sr <- crn$SOLAR_RADIATION
  sr[!is.na(crn$SR_FLAG) & crn$SR_FLAG != 0] <- NA
  rh <- crn$RELATIVE_HUMIDITY
  if ("RH_FLAG" %in% names(crn)) rh[!is.na(crn$RH_FLAG) & crn$RH_FLAG != 0] <- NA
  wind <- crn$WIND_1_5
  if ("WIND_FLAG" %in% names(crn)) wind[!is.na(crn$WIND_FLAG) & crn$WIND_FLAG != 0] <- NA

  keep <- !is.na(t5)
  key  <- ceiling(as.numeric(t5[keep]) / 3600) * 3600

  hours <- sort(unique(key))
  agg <- function(x, fun) {
    vapply(split(x[keep], factor(key, levels = hours)), function(v) {
      n <- sum(!is.na(v))
      if (n < min_valid) NA_real_ else fun(v, n)
    }, numeric(1))
  }
  mean_fun <- function(v, n) mean(v, na.rm = TRUE)
  sum_fun  <- function(v, n) sum(v, na.rm = TRUE) * 12 / n

  data.frame(
    obs_time  = as.POSIXct(hours, origin = "1970-01-01", tz = "UTC"),
    temp      = unname(agg(crn$AIR_TEMPERATURE, mean_fun)),
    relhum    = unname(agg(rh, mean_fun)),
    swdown    = unname(agg(sr, mean_fun)),
    windspeed = unname(agg(wind, mean_fun)),
    precip    = unname(agg(crn$PRECIPITATION, sum_fun)),
    stringsAsFactors = FALSE
  )
}

.era5_vars <- c("t2m", "d2m", "sp", "u10", "v10", "tp", "avg_sdlwrf", "fdir", "ssrd")

.era5_raw_point <- function(era5_dir, lon, lat) {
  files <- list.files(era5_dir, pattern = "\\.nc$", full.names = TRUE)
  if (length(files) == 0L)
    stop(sprintf("No ERA5 .nc files found in %s", era5_dir))
  pts <- matrix(c(lon, lat), ncol = 2)

  per_file <- lapply(files, function(f) {
    cols <- lapply(.era5_vars, function(v) {
      r <- tryCatch(terra::rast(f, subds = v), error = function(e)
        stop(sprintf("ERA5 file %s has no variable '%s' (expected CDS names: %s)",
                     basename(f), v, paste(.era5_vars, collapse = ", ")), call. = FALSE))
      e <- terra::ext(r)
      if (lon < e$xmin || lon > e$xmax || lat < e$ymin || lat > e$ymax)
        stop(sprintf("Station (lon %.4f, lat %.4f) is outside the ERA5 grid in %s",
                     lon, lat, basename(f)), call. = FALSE)
      list(time = terra::time(r),
           val  = as.numeric(unlist(terra::extract(r, pts)[1, ])))
    })
    tm <- cols[[1]]$time
    for (cc in cols[-1])
      if (!isTRUE(all.equal(as.numeric(cc$time), as.numeric(tm))))
        stop(sprintf("Variables in %s have different time axes", basename(f)))
    out <- data.frame(time = as.POSIXct(as.numeric(tm), origin = "1970-01-01", tz = "UTC"))
    for (i in seq_along(.era5_vars)) out[[.era5_vars[i]]] <- cols[[i]]$val
    out
  })

  raw <- do.call(rbind, per_file)
  if (all(is.na(raw$t2m)))
    stop(sprintf("Station (lon %.4f, lat %.4f) is outside the ERA5 grid in %s",
                 lon, lat, era5_dir))
  raw <- raw[!duplicated(raw$time), , drop = FALSE]
  raw <- raw[order(raw$time), , drop = FALSE]
  rownames(raw) <- NULL
  raw
}

.era5_to_climdata <- function(raw) {
  es <- function(tc) 0.6108 * exp(17.27 * tc / (tc + 237.3))
  tc <- raw$t2m - 273.15
  sw <- raw$ssrd / 3600
  data.frame(
    obs_time  = raw$time,
    temp      = tc,
    relhum    = pmin(100, 100 * es(raw$d2m - 273.15) / es(tc)),
    pres      = raw$sp / 1000,
    swdown    = sw,
    difrad    = pmax(sw - raw$fdir / 3600, 0),
    lwdown    = raw$avg_sdlwrf,
    windspeed = sqrt(raw$u10^2 + raw$v10^2) * 0.7477849,
    winddir   = (180 + atan2(raw$u10, raw$v10) * 180 / pi) %% 360,
    precip    = raw$tp * 1000,
    stringsAsFactors = FALSE
  )
}

.bias_coef <- function(crn, era5, type = c("add", "ratio"), min_era5 = 0) {
  type <- match.arg(type)
  neutral <- if (type == "add") 0 else 1
  ok <- !is.na(crn) & !is.na(era5)
  if (type == "ratio") ok <- ok & era5 > min_era5
  n <- sum(ok)
  if (n < 24L) {
    warning(sprintf("Only %d overlapping CRN/ERA5 hours for bias fitting (< 24); using neutral correction.", n))
    return(list(value = neutral, n = n))
  }
  val <- if (type == "add") mean(crn[ok] - era5[ok]) else {
    den <- sum(era5[ok]); if (den == 0) 1 else sum(crn[ok]) / den
  }
  list(value = val, n = n)
}

.merge_fill <- function(crn_h, era5_c, grid, bias_correct = TRUE, wind_factor = 1) {
  m <- merge(data.frame(obs_time = grid), crn_h, by = "obs_time", all.x = TRUE)
  m <- merge(m, era5_c, by = "obs_time", all.x = TRUE, suffixes = c("", ".e"))
  m <- m[order(m$obs_time), , drop = FALSE]
  m$windspeed <- m$windspeed * wind_factor

  spec <- list(
    temp      = list(type = "add"),
    relhum    = list(type = "add"),
    swdown    = list(type = "ratio", min = 10),
    windspeed = list(type = "ratio", min = 0.1),
    precip    = list(type = NA)
  )
  filled <- character(nrow(m))
  coefs  <- list()
  for (v in names(spec)) {
    x <- m[[v]]; e <- m[[paste0(v, ".e")]]; s <- spec[[v]]
    if (!is.na(s$type) && bias_correct) {
      cf <- .bias_coef(x, e, s$type, if (is.null(s$min)) 0 else s$min)
      e <- if (s$type == "add") e + cf$value else e * cf$value
      coefs[[v]] <- data.frame(variable = v, type = s$type, value = cf$value, n = cf$n)
    }
    fill <- is.na(x) & !is.na(e)
    x[fill] <- e[fill]
    filled[fill] <- ifelse(nzchar(filled[fill]), paste(filled[fill], v, sep = ","), v)
    m[[v]] <- x
  }
  m$relhum    <- pmin(pmax(m$relhum, 0), 100)
  m$swdown    <- pmax(m$swdown, 0)
  m$windspeed <- pmax(m$windspeed, 0)
  m$precip    <- pmax(m$precip, 0)
  frac        <- ifelse(!is.na(m$swdown.e) & m$swdown.e > 1,
                        pmin(pmax(m$difrad / m$swdown.e, 0), 1), 1)
  m$difrad    <- m$swdown * frac
  m$filled    <- filled

  cols <- c("obs_time", "temp", "relhum", "pres", "swdown", "difrad", "lwdown",
            "windspeed", "winddir", "precip", "filled")
  out <- m[, cols]
  rownames(out) <- NULL
  list(data = out, coefs = if (length(coefs)) do.call(rbind, coefs) else NULL)
}

#' Build a microclimf Climate Data Frame from CRN and ERA5
#'
#' Merges hourly-aggregated USCRN station data with ERA5 reanalysis from the
#' station's grid cell into the hourly \code{climdata}-format data frame used by
#' the \code{microclimfPara} data-frame workflow (e.g. \code{runmicro_big()}
#' with a \code{micropoint} data frame). Intended for areas outside AORC
#' coverage (e.g. Alaska) or small areas where one station represents the domain.
#'
#' @details CRN is the primary source for \code{temp}, \code{relhum},
#'   \code{swdown}, \code{windspeed} and \code{precip}. Five-minute values are
#'   aggregated to hourly UTC (stamps in (H-1:00, H:00] are labelled H:00, the
#'   ERA5 flux convention); an hour needs at least 9 of 12 valid values, and
#'   precipitation is scaled for partly missing hours. Solar radiation with
#'   \code{SR_FLAG}, \code{RH_FLAG} or \code{WIND_FLAG != 0} is treated as
#'   missing. ERA5 supplies \code{pres}, \code{lwdown} and \code{winddir}, and
#'   fills CRN gaps. \code{difrad} applies ERA5's diffuse fraction (total minus
#'   direct-horizontal shortwave, over total) to the output \code{swdown}. When \code{bias_correct = TRUE}, filled
#'   values are corrected using hours where both sources exist: an additive offset
#'   for temperature and humidity, a multiplicative ratio for daytime shortwave
#'   and wind speed, and none for precipitation. CRN wind is scaled from
#'   \code{wind_height} to the 2 m climdata reference with a log profile
#'   (z0 = 0.01 m). ERA5 files are read directly with current CDS variable names
#'   (\code{avg_sdlwrf}, \code{ssrd}, ...), so no patching of
#'   \code{microclimdata} is needed. The function stops if any value remains
#'   \code{NA}, because \code{microclimf} cannot run with missing weather.
#'
#' @param crn A data.frame from \code{\link{get_NOAACRN_data}} for a single
#'   station and period
#' @param era5_dir Directory containing the ERA5 \code{.nc} files downloaded by
#'   \code{microclimdata::era5_download()}. Non-\code{.nc} entries are ignored
#' @param tme Optional hourly POSIXct vector; output is restricted to its
#'   range. Default NULL uses the full hourly CRN period
#' @param bias_correct Logical. Bias-correct ERA5 values used to fill CRN gaps.
#'   Default TRUE
#' @param wind_height Numeric. CRN anemometer height in metres. Default 1.5
#' @param flag_col Logical. Add a \code{filled} column naming the variables
#'   filled from ERA5 in each hour. Default TRUE
#'
#' @return A data.frame with columns \code{obs_time} (hourly POSIXct, UTC),
#'   \code{temp}, \code{relhum}, \code{pres}, \code{swdown}, \code{difrad},
#'   \code{lwdown}, \code{windspeed}, \code{winddir}, \code{precip}, plus
#'   \code{filled} when \code{flag_col = TRUE}
#' @seealso \code{\link{get_NOAACRN_data}}, \code{microclimdata::era5_download}
#' @export
build_climdata_crn_era5 <- function(crn,
                                    era5_dir,
                                    tme = NULL,
                                    bias_correct = TRUE,
                                    wind_height = 1.5,
                                    flag_col = TRUE) {

  if (length(unique(crn$WBANNO)) > 1L)
    stop("crn contains more than one station (WBANNO); supply a single station")
  lon <- stats::median(crn$LONGITUDE, na.rm = TRUE)
  lat <- stats::median(crn$LATITUDE, na.rm = TRUE)

  crn_h <- .crn_hourly(crn)
  era5_c <- .era5_to_climdata(.era5_raw_point(era5_dir, lon, lat))

  if (is.null(tme)) {
    lo <- max(min(crn_h$obs_time), min(era5_c$obs_time))
    hi <- min(max(crn_h$obs_time), max(era5_c$obs_time))
    if (hi < lo)
      stop("CRN and ERA5 data have no overlapping period; check the ERA5 download dates")
    n_drop <- sum(crn_h$obs_time < lo | crn_h$obs_time > hi)
    if (n_drop > 0L)
      message(sprintf("%d CRN hour(s) outside ERA5 coverage dropped (period set to %s - %s UTC).",
                      n_drop, format(lo, "%Y-%m-%d %H:%M"), format(hi, "%Y-%m-%d %H:%M")))
    grid <- seq(lo, hi, by = "hour")
  } else {
    tme <- as.POSIXct(tme)
    attr(tme, "tzone") <- "UTC"
    grid <- seq(min(tme), max(tme), by = "hour")
  }

  cov <- mean(!is.na(crn_h$temp[match(grid, crn_h$obs_time)]))
  if (cov < 0.8)
    warning(sprintf("CRN coverage is only %.0f%% of the requested hours; ERA5 fills the rest.",
                    100 * cov))

  wind_factor <- if (wind_height == 2) 1 else log(2 / 0.01) / log(wind_height / 0.01)
  res <- .merge_fill(crn_h, era5_c, grid, bias_correct, wind_factor)
  out <- res$data

  value_cols <- setdiff(names(out), c("obs_time", "filled"))
  na_n <- vapply(out[value_cols], function(x) sum(is.na(x)), integer(1))
  if (any(na_n > 0L))
    stop(sprintf("NA values remain after merging CRN and ERA5 (does ERA5 cover %s to %s? Restrict the period with `tme` if not): %s",
                 format(min(grid), "%Y-%m-%d %H:%M"), format(max(grid), "%Y-%m-%d %H:%M"),
                 paste(sprintf("%s = %d", names(na_n)[na_n > 0L], na_n[na_n > 0L]),
                       collapse = ", ")))

  cat(sprintf("Built climdata: %d hours, %s to %s UTC (station lon %.4f, lat %.4f)\n",
              nrow(out), format(min(grid), "%Y-%m-%d %H:%M"),
              format(max(grid), "%Y-%m-%d %H:%M"), lon, lat))
  for (v in c("temp", "relhum", "swdown", "windspeed", "precip"))
    cat(sprintf("  %-9s filled from ERA5: %d hours\n", v, sum(grepl(v, out$filled))))
  if (!is.null(res$coefs))
    for (i in seq_len(nrow(res$coefs)))
      cat(sprintf("  bias %-9s %-5s %.3f (n = %d)\n", res$coefs$variable[i],
                  res$coefs$type[i], res$coefs$value[i], res$coefs$n[i]))

  if (!flag_col) out$filled <- NULL
  out
}
