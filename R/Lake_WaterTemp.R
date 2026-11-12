# --------------------------------------------------------------------------- #
#  Lake surface water temperature (LakeEnsemblR ensemble)
# --------------------------------------------------------------------------- #

.stop_if_longlat <- function(x, what) {
  ll <- if (inherits(x, "SpatRaster")) terra::is.lonlat(x) else isTRUE(sf::st_is_longlat(x))
  if (ll) stop(sprintf("%s must be in a projected CRS with metre units, not longitude/latitude", what))
  invisible(TRUE)
}

#' Generate Synthetic Lake Bathymetry from Lake Polygons
#'
#' Builds a fine-resolution water-depth raster for lakes without surveyed
#' bathymetry, using a distance-to-shore rule: depth reaches \code{ref_depth}
#' at \code{ref_dist} metres from shore and approaches \code{max_depth}
#' exponentially toward the lake interior.
#'
#' @details Depth at distance \eqn{d} (m) from the nearest shoreline is
#'   \deqn{z = z_{max} (1 - e^{-k d}), \quad k = -\log(1 - z_{ref}/z_{max}) / d_{ref}}
#'   so \eqn{z(d_{ref}) = z_{ref}} and \eqn{z \to z_{max}} offshore.
#'   Distances are computed from each cell centre to the polygon boundary,
#'   so \code{lakes} must be in a projected CRS with metre units.
#'
#'   Per-lake guidelines can be supplied as columns of \code{lakes} named
#'   \code{ref_dist}, \code{ref_depth}, and \code{max_depth}; a non-\code{NA}
#'   value in such a column overrides the corresponding argument for that
#'   lake.
#'
#'   The result follows the bathymetry convention used by
#'   \code{\link{prepare_lakes}}: positive values are water depth in metres,
#'   \code{NA} outside lakes. It is interchangeable with a surveyed
#'   bathymetry raster (e.g. ADF&G maps) and the two can be combined with
#'   \code{terra::merge()}.
#'
#' @param lakes An \code{sf} polygon object in a projected (metre) CRS, one
#'   row per lake.
#' @param res Numeric. Output resolution in metres. Default 1. Must be fine
#'   enough to resolve \code{ref_dist}.
#' @param ref_dist Numeric. Reference distance from shore in metres.
#'   Default 5.
#' @param ref_depth Numeric. Water depth in metres at \code{ref_dist}.
#'   Default 1. Must be less than \code{max_depth}.
#' @param max_depth Numeric. Maximum (asymptotic) depth in metres. Default
#'   4.572 (15 ft).
#' @param out_file Optional file path; if given the raster is written there.
#'
#' @return Invisibly, a single-layer \code{SpatRaster} named \code{depth}.
#' @seealso \code{\link{prepare_lakes}}, \code{\link{run_lake_watertemp}}
#' @export
generate_lake_bathymetry <- function(lakes,
                                     res       = 1,
                                     ref_dist  = 5,
                                     ref_depth = 1,
                                     max_depth = 4.572,
                                     out_file  = NULL) {
  if (!inherits(lakes, "sf")) stop("lakes must be an sf polygon object")
  .stop_if_longlat(lakes, "lakes")

  n <- nrow(lakes)
  pick <- function(col, default) {
    if (col %in% names(lakes)) {
      v <- as.numeric(lakes[[col]])
      ifelse(is.na(v), default, v)
    } else {
      rep(default, n)
    }
  }
  rd <- pick("ref_dist", ref_dist)
  rp <- pick("ref_depth", ref_depth)
  md <- pick("max_depth", max_depth)

  bad <- which(rp >= md | rp <= 0 | rd <= 0)
  if (length(bad) > 0)
    stop(sprintf("ref_depth must be > 0 and less than max_depth, and ref_dist > 0 (lake row(s): %s)",
                 paste(bad, collapse = ", ")))

  rs <- lapply(seq_len(n), function(i) {
    v    <- terra::vect(sf::st_geometry(lakes[i, ]))
    e0   <- as.vector(terra::ext(v))
    e    <- terra::ext(floor(e0[1] / res) * res, ceiling(e0[2] / res) * res,
                       floor(e0[3] / res) * res, ceiling(e0[4] / res) * res)
    tmpl <- terra::rast(e, resolution = res, crs = terra::crs(v))
    d    <- terra::distance(tmpl, terra::as.lines(v))
    k    <- -log(1 - rp[i] / md[i]) / rd[i]
    z    <- md[i] * (1 - exp(-k * d))
    terra::mask(z, terra::rasterize(v, tmpl))
  })

  out <- if (n == 1) rs[[1]] else terra::merge(terra::sprc(rs))
  names(out) <- "depth"

  if (!is.null(out_file)) terra::writeRaster(out, out_file, overwrite = TRUE)
  invisible(out)
}

#' Prepare Lakes for Water Temperature Modeling
#'
#' Derives the per-lake inputs LakeEnsemblR needs from a bathymetry raster:
#' a depth-area hypsograph, maximum depth (raised to a minimum modeling
#' depth), basin length/width and wind fetch, centroid coordinates, and
#' surface elevation.
#'
#' @details Lake outlines come from \code{lakes} when supplied, otherwise
#'   from connected (8-neighbour) patches of \code{bathy > 0}, named
#'   \code{lake_001}, \code{lake_002}, ... in patch order.
#'
#'   The hypsograph lists the surface area at depth 0 (polygon area) and,
#'   for each depth \code{z} in \code{seq(0, max_depth, by = dz)} plus
#'   \code{max_depth}, the summed area of bathymetry cells at least
#'   \code{z} deep (never less than one cell).
#'
#'   Very shallow water cannot be run by every model in the ensemble
#'   (LakeEnsemblR builds GOTM and Simstrat grids as
#'   \code{round(depth / 0.5)} layers, which is 0 at or below 0.25 m). A lake
#'   shallower than \code{depth_floor} is therefore modeled at
#'   \code{model_depth = depth_floor}, with its hypsograph depths stretched
#'   proportionally. The temperature is reported at
#'   \code{out_depth = min(0.5, model_depth / 2)}. Floored ponds carry
#'   extra thermal mass and respond more slowly than the real water body.
#'
#'   Basin length and width are the long and short sides of the minimum
#'   rotated rectangle around each lake; fetch is set to the basin length.
#'
#' @param bathy A \code{SpatRaster} (or file path) of water depth in metres,
#'   positive downward, \code{NA} or 0 outside water, in a projected (metre)
#'   CRS. Real (e.g. ADF&G) or generated by
#'   \code{\link{generate_lake_bathymetry}}.
#' @param dem A \code{SpatRaster} (or file path) of elevation in metres,
#'   used for lake surface elevation at the centroid.
#' @param lakes Optional \code{sf} polygons of the lakes. Other columns (e.g.
#'   \code{Kw}) are carried through.
#' @param id_col Optional name of the column in \code{lakes} holding lake
#'   identifiers. If \code{NULL}, lakes are named \code{lake_001}, ... by row.
#' @param depth_floor Numeric. Minimum modeled depth in metres. Default 0.3.
#' @param dz Numeric. Hypsograph depth step in metres. Default 0.1.
#'
#' @return Invisibly, an \code{sf} object (CRS of \code{bathy}) with one row
#'   per lake and columns \code{lake_id}, \code{area_m2}, \code{max_depth},
#'   \code{model_depth}, \code{out_depth}, \code{lon}, \code{lat} (WGS84
#'   centroid), \code{elevation}, \code{bsn_len}, \code{bsn_wid},
#'   \code{fetch}, and list-column \code{hypsograph} (data.frames with
#'   \code{Depth_meter}, \code{Area_meterSquared}).
#' @seealso \code{\link{generate_lake_bathymetry}}, \code{\link{run_lake_watertemp}}
#' @export
prepare_lakes <- function(bathy,
                          dem,
                          lakes       = NULL,
                          id_col      = NULL,
                          depth_floor = 0.3,
                          dz          = 0.1) {
  if (!inherits(bathy, "SpatRaster")) bathy <- terra::rast(bathy)
  if (!inherits(dem, "SpatRaster"))   dem   <- terra::rast(dem)
  bathy <- bathy[[1]]
  .stop_if_longlat(bathy, "bathy")

  if (is.null(lakes)) {
    pt    <- terra::patches(terra::ifel(bathy > 0, 1, NA), directions = 8)
    lakes <- sf::st_as_sf(terra::as.polygons(pt))
    lakes <- lakes[order(lakes[[1]]), ]
    lakes$lake_id <- sprintf("lake_%03d", seq_len(nrow(lakes)))
    lakes <- lakes[, "lake_id"]
  } else {
    if (!inherits(lakes, "sf")) stop("lakes must be an sf polygon object")
    lakes <- sf::st_transform(lakes, terra::crs(bathy))
    lakes$lake_id <- if (is.null(id_col)) {
      sprintf("lake_%03d", seq_len(nrow(lakes)))
    } else {
      if (!id_col %in% names(lakes)) stop(sprintf("id_col '%s' not found in lakes", id_col))
      as.character(lakes[[id_col]])
    }
  }
  if (nrow(lakes) == 0) stop("no lakes found")
  if (anyDuplicated(lakes$lake_id)) stop("lake identifiers must be unique")

  cell_area <- prod(terra::res(bathy))

  props <- lapply(seq_len(nrow(lakes)), function(i) {
    g  <- sf::st_geometry(lakes[i, ])
    z  <- terra::extract(bathy, terra::vect(g), ID = FALSE)[[1]]
    z  <- z[!is.na(z) & z > 0]
    if (length(z) == 0)
      stop(sprintf("lake %s has no bathymetry cells with depth > 0", lakes$lake_id[i]))

    area <- as.numeric(sf::st_area(g))
    zmax <- max(z)
    deps <- unique(c(seq(0, zmax, by = dz), zmax))
    a    <- vapply(deps, function(d) {
      if (d == 0) area else max(sum(z >= d) * cell_area, cell_area)
    }, numeric(1))
    a <- cummin(a)

    model_depth <- max(zmax, depth_floor)
    if (model_depth > zmax) deps <- deps * model_depth / zmax

    rect  <- sf::st_minimum_rotated_rectangle(g)
    co    <- sf::st_coordinates(rect)[1:3, 1:2]
    sides <- sqrt(rowSums(diff(co)^2))

    cen  <- sf::st_centroid(g)
    ll   <- sf::st_coordinates(sf::st_transform(cen, 4326))
    elev <- terra::extract(dem, terra::vect(sf::st_transform(cen, terra::crs(dem))),
                           ID = FALSE)[[1]]
    if (is.na(elev)) warning(sprintf("lake %s: DEM has no value at the centroid", lakes$lake_id[i]))

    list(
      area_m2     = area,
      max_depth   = zmax,
      model_depth = model_depth,
      out_depth   = min(0.5, model_depth / 2),
      lon         = ll[1, 1],
      lat         = ll[1, 2],
      elevation   = elev,
      bsn_len     = max(sides),
      bsn_wid     = min(sides),
      fetch       = max(sides),
      hypsograph  = data.frame(Depth_meter = deps, Area_meterSquared = a)
    )
  })

  keep <- setdiff(names(lakes), c("lake_id", attr(lakes, "sf_column"), names(props[[1]])))
  out  <- sf::st_sf(
    lake_id     = lakes$lake_id,
    as.data.frame(lakes)[, keep, drop = FALSE],
    area_m2     = vapply(props, `[[`, numeric(1), "area_m2"),
    max_depth   = vapply(props, `[[`, numeric(1), "max_depth"),
    model_depth = vapply(props, `[[`, numeric(1), "model_depth"),
    out_depth   = vapply(props, `[[`, numeric(1), "out_depth"),
    lon         = vapply(props, `[[`, numeric(1), "lon"),
    lat         = vapply(props, `[[`, numeric(1), "lat"),
    elevation   = vapply(props, `[[`, numeric(1), "elevation"),
    bsn_len     = vapply(props, `[[`, numeric(1), "bsn_len"),
    bsn_wid     = vapply(props, `[[`, numeric(1), "bsn_wid"),
    fetch       = vapply(props, `[[`, numeric(1), "fetch"),
    geometry    = sf::st_geometry(lakes)
  )
  out$hypsograph <- lapply(props, `[[`, "hypsograph")
  invisible(out)
}

.ler_meteo_cols <- c(
  windspeed = "Ten_Meter_Elevation_Wind_Speed_meterPerSecond",
  temp      = "Air_Temperature_celsius",
  relhum    = "Relative_Humidity_percent",
  swdown    = "Shortwave_Radiation_Downwelling_wattPerMeterSquared",
  lwdown    = "Longwave_Radiation_Downwelling_wattPerMeterSquared",
  pres      = "Surface_Level_Barometric_Pressure_pascal",
  precip    = "Precipitation_millimeterPerDay"
)

.fmt_hour <- function(x) format(x, "%Y-%m-%d %H:%M", tz = "UTC")

.lake_meteo <- function(clim, lon, lat, start, stop, max_gap = 6) {
  vars <- names(.ler_meteo_cols)

  if (is.data.frame(clim)) {
    miss <- setdiff(c("obs_time", vars), names(clim))
    if (length(miss) > 0)
      stop(sprintf("climate data.frame is missing column(s): %s", paste(miss, collapse = ", ")))
    df <- data.frame(datetime = as.POSIXct(clim$obs_time, tz = "UTC"), clim[vars])
    wind_fac <- log(10 / 0.01) / log(2 / 0.01)
  } else {
    cl  <- if (is.character(clim)) readr::read_rds(clim) else clim
    get <- function(v) {
      if (is.null(cl[[v]])) stop(sprintf("packaged climate is missing '%s'", v))
      if (inherits(cl[[v]], "PackedSpatRaster")) terra::unwrap(cl[[v]]) else cl[[v]]
    }
    r1  <- get("temp")
    pt  <- terra::project(terra::vect(cbind(lon, lat), crs = "EPSG:4326"), terra::crs(r1))
    cell <- terra::cellFromXY(r1, terra::crds(pt))
    if (is.na(cell))
      stop(sprintf("lake centroid (%.4f, %.4f) lies outside the packaged climate extent", lon, lat))
    df <- data.frame(datetime = as.POSIXct(terra::time(r1), tz = "UTC"))
    for (v in vars) df[[v]] <- as.numeric(unlist(get(v)[cell]))
    wind_fac <- 1
  }

  df <- df[!duplicated(df$datetime), , drop = FALSE]
  tg <- seq(start, stop, by = 3600)
  ix <- match(as.numeric(tg), as.numeric(df$datetime))
  if (anyNA(ix)) {
    gone <- tg[is.na(ix)]
    stop(sprintf("climate input does not cover %s to %s (%d hour(s) missing)",
                 .fmt_hour(min(gone)), .fmt_hour(max(gone)), length(gone)))
  }
  df <- df[ix, , drop = FALSE]

  for (v in vars) {
    x <- zoo::na.approx(df[[v]], maxgap = max_gap, na.rm = FALSE)
    if (anyNA(x)) {
      bad <- df$datetime[is.na(x)]
      stop(sprintf("climate variable '%s' has a gap longer than %d hours starting %s",
                   v, max_gap, .fmt_hour(min(bad))))
    }
    df[[v]] <- x
  }

  df$windspeed <- df$windspeed * wind_fac
  df$pres      <- df$pres * 1000
  df$precip    <- df$precip * 24

  out <- data.frame(datetime = tg)
  for (v in vars) out[[.ler_meteo_cols[[v]]]] <- df[[v]]
  out <- out[, c("datetime", unname(.ler_meteo_cols))]
  rownames(out) <- NULL
  out
}

.map_lakes_to_grid <- function(lakes, landcover, water_classes, max_match_dist = 60) {
  lc    <- landcover[[1]]
  lakes <- sf::st_transform(lakes, terra::crs(lc))
  n     <- nrow(lakes)

  water <- terra::values(lc, mat = FALSE) %in% water_classes

  v      <- terra::vect(sf::st_geometry(lakes))
  v$.row <- seq_len(n)
  idx    <- terra::values(terra::rasterize(v, lc, field = ".row", touches = TRUE), mat = FALSE)
  idx[!water] <- NA

  un <- which(water & is.na(idx))
  if (length(un) > 0) {
    pts <- sf::st_as_sf(as.data.frame(terra::xyFromCell(lc, un)),
                        coords = c("x", "y"), crs = sf::st_crs(lakes))
    nf  <- sf::st_nearest_feature(pts, lakes)
    dd  <- as.numeric(sf::st_distance(pts, lakes[nf, ], by_element = TRUE))
    ok  <- dd <= max_match_dist
    idx[un[ok]] <- nf[ok]
    if (any(!ok))
      warning(sprintf("%d water cell(s) farther than %g m from any modeled lake left NA",
                      sum(!ok), max_match_dist))
  }

  counts <- tabulate(idx[!is.na(idx)], nbins = n)
  for (i in which(counts == 0))
    warning(sprintf("lake %s matched no landcover water cells; it will not appear in the raster",
                    lakes$lake_id[i]))

  out <- terra::rast(lc)
  terra::values(out) <- as.integer(idx)
  names(out) <- "lake_index"
  out
}

.write_watertemp_nc <- function(lake_idx, ens, out_path) {
  tm <- ens[[1]]$datetime
  for (e in ens)
    if (!identical(as.numeric(e$datetime), as.numeric(tm)))
      stop("all lakes must share the same hourly time axis")

  v  <- terra::values(lake_idx, mat = FALSE)
  mt <- do.call(rbind, lapply(ens, function(e) e$water_temp))
  mi <- do.call(rbind, lapply(ens, function(e) e$ice_frac))

  fill <- function(m) {
    r <- terra::rast(lake_idx, nlyrs = length(tm))
    terra::values(r) <- m[v, , drop = FALSE]
    terra::time(r) <- tm
    r
  }
  wt <- fill(mt)
  ic <- fill(mi)

  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  ds <- terra::sds(wt, ic)
  names(ds) <- c("water_temp", "ice_frac")
  terra::longnames(ds) <- c("Ensemble-mean near-surface water temperature",
                            "Fraction of ensemble models with ice cover")
  terra::units(ds) <- c("degC", "1")
  terra::writeCDF(ds, out_path,
                  varname  = c("water_temp", "ice_frac"),
                  longname = c("Ensemble-mean near-surface water temperature",
                               "Fraction of ensemble models with ice cover"),
                  unit     = c("degC", "1"),
                  overwrite = TRUE)
  invisible(out_path)
}
