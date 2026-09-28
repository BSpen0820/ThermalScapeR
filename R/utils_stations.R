# --------------------------------------------------------------------------- #
#  Generic station-resolution helpers shared by get_NOAACRN_data() and
#  get_SNOTEL_data(). Moved from R/Weather_DataPrep.R's .crn_* originals;
#  bodies unchanged.
# --------------------------------------------------------------------------- #

.station_haversine <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  dlat <- (lat2 - lat1) * rad
  dlon <- (lon2 - lon1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  2 * 6371.0088 * asin(pmin(1, sqrt(a)))
}

# .crn_period_label() stays in R/Weather_DataPrep.R (CRN-specific file/year
# naming logic elsewhere depends on it); this generic function calls it
# across the file boundary, which is fine -- all package functions share one
# namespace regardless of which file defines them.
.station_normalize_dates <- function(dates) {
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

.station_coords_to_lonlat <- function(coords) {
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
