# --------------------------------------------------------------------------- #
#  NOAA USCRN sub-hourly station data
# --------------------------------------------------------------------------- #

.crn_year <- function(d) as.integer(format(d, "%Y"))

.crn_period_label <- function(start, end) {
  sprintf("%s_to_%s", format(start, "%Y%m%d"), format(end, "%Y%m%d"))
}

.crn_haversine <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  dlat <- (lat2 - lat1) * rad
  dlon <- (lon2 - lon1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  2 * 6371.0088 * asin(pmin(1, sqrt(a)))
}

.crn_normalize_dates <- function(dates) {
  if (is.data.frame(dates)) {
    if (!all(c("Start_Dates", "End_Dates") %in% names(dates)))
      stop("`dates` data.frame must have columns Start_Dates and End_Dates.")
    start <- dates$Start_Dates
    end <- dates$End_Dates
    is_df <- TRUE
  } else {
    if (length(dates) != 2L)
      stop("`dates` must be a length-2 Date vector or a data.frame with Start_Dates/End_Dates.")
    start <- dates[1L]
    end <- dates[2L]
    is_df <- FALSE
  }
  if (!inherits(start, "Date") || !inherits(end, "Date"))
    stop("`dates` must be of class Date.")
  if (length(start) == 0L)
    stop("`dates` must not be empty.")
  if (anyNA(start) || anyNA(end))
    stop("`dates` must not contain NA.")
  if (any(end < start))
    stop("An end date is before start date in `dates`.")
  label <- vapply(seq_along(start),
                  function(i) .crn_period_label(start[i], end[i]), character(1))
  list(periods = data.frame(start = start, end = end, label = label,
                            stringsAsFactors = FALSE),
       is_df = is_df)
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

.crn_coords_to_lonlat <- function(coords) {
  if (is.numeric(coords)) {
    if (length(coords) != 2L || anyNA(coords))
      stop("Numeric `coords` must be c(lon, lat).")
    lon <- coords[[1L]]
    lat <- coords[[2L]]
  } else {
    geom <- if (inherits(coords, "SpatRaster")) {
      sf::st_as_sf(terra::as.polygons(terra::ext(coords), crs = terra::crs(coords)))
    } else if (inherits(coords, "SpatVector")) {
      sf::st_as_sf(coords)
    } else if (inherits(coords, c("sf", "sfc"))) {
      coords
    } else {
      stop("`coords` must be c(lon, lat), an sf/sfc object, a SpatVector, or a SpatRaster.")
    }
    g <- sf::st_geometry(geom)
    if (is.na(sf::st_crs(g)))
      stop("Spatial `coords` must have a CRS.")
    g <- sf::st_transform(g, 4326)
    ctr <- sf::st_coordinates(sf::st_centroid(sf::st_union(g)))
    lon <- unname(ctr[1L, "X"])
    lat <- unname(ctr[1L, "Y"])
  }
  if (abs(lat) > 90 || abs(lon) > 180)
    stop("`coords` are out of range (lon within +/-180, lat within +/-90).")
  c(lon = lon, lat = lat)
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
  if (!is.null(network))
    cand <- cand[cand$NETWORK %in% network, , drop = FALSE]
  has_all <- vapply(cand$STATION_DIR, function(d)
    all(vapply(as.character(years), function(y) d %in% listings[[y]], logical(1))),
    logical(1))
  cand <- cand[has_all, , drop = FALSE]
  if (nrow(cand) == 0L)
    stop(sprintf("No station in network(s) %s has sub-hourly data for every year of %s.",
                 if (is.null(network)) "<any>" else paste(network, collapse = ", "),
                 paste(years, collapse = ", ")))
  cand$dist_km <- .crn_haversine(lon, lat, cand$LONGITUDE, cand$LATITUDE)
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
    if (grepl("404", paste(c(msgs, conditionMessage(res)), collapse = " "), fixed = TRUE))
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
  for (col in names(.crn_sentinels)) {
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
