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
