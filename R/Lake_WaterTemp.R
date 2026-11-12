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

.ensemble_lake <- function(model_out, start, stop) {
  if (length(model_out) == 0) stop("all models failed; no ensemble can be formed")
  tg  <- seq(start, stop, by = 3600)
  out <- data.frame(datetime = tg)
  for (m in names(model_out)) {
    d  <- model_out[[m]]
    ix <- match(as.numeric(tg), as.numeric(d$datetime))
    if (anyNA(ix))
      stop(sprintf("model %s output does not cover %s to %s", m, .fmt_hour(start), .fmt_hour(stop)))
    out[[sprintf("%s_temp", m)]] <- d$temp[ix]
    out[[sprintf("%s_ice", m)]]  <- d$ice_height[ix]
  }
  tcols <- sprintf("%s_temp", names(model_out))
  icols <- sprintf("%s_ice", names(model_out))
  out$water_temp <- rowMeans(out[tcols], na.rm = TRUE)
  out$ice_frac   <- rowMeans(out[icols] > 0, na.rm = TRUE)
  out[, c("datetime", "water_temp", "ice_frac", as.vector(rbind(tcols, icols)))]
}

.ler_require <- function() {
  if (!requireNamespace("LakeEnsemblR", quietly = TRUE))
    stop(paste0(
      "LakeEnsemblR is required for lake water temperature modeling. Install with:\n",
      "  remotes::install_github('GLEON/rLakeAnalyzer')\n",
      "  remotes::install_github('aemon-j/GLM3r', ref = 'v3.1.1')\n",
      "  remotes::install_github('USGS-R/glmtools', ref = 'ggplot_overhaul')\n",
      "  remotes::install_github('aemon-j/FLakeR', ref = 'inflow')\n",
      "  remotes::install_github('aemon-j/GOTMr'); remotes::install_github('aemon-j/gotmtools')\n",
      "  remotes::install_github('aemon-j/SimstratR'); remotes::install_github('aemon-j/MyLakeR')\n",
      "  remotes::install_github('aemon-j/LakeEnsemblR')"))
  invisible(TRUE)
}

.ler_write_yaml <- function(path, lake, start, stop, Kw) {
  fmt <- function(x) format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  txt <- c(
    "location:",
    sprintf("   name: %s", lake$lake_id),
    sprintf("   latitude: %.6f", lake$lat),
    sprintf("   longitude: %.6f", lake$lon),
    sprintf("   elevation: %.2f", lake$elevation),
    sprintf("   depth: %.4f", lake$model_depth),
    "   hypsograph: LakeEnsemblR_bathymetry_standard.csv",
    sprintf("   init_depth: %.4f", lake$model_depth),
    "time:",
    sprintf("   start: %s", fmt(start)),
    sprintf("   stop: %s", fmt(stop)),
    "   time_step: 3600.0",
    "config_files:",
    "   GOTM: GOTM/gotm.yaml",
    "   GLM: GLM/glm3.nml",
    "   Simstrat: Simstrat/simstrat.par",
    "   FLake: FLake/flake.nml",
    "   MyLake: MyLake/mylake.Rdata",
    "observations:",
    "   temperature:",
    "      file: NULL",
    "   ice_height:",
    "      file: NULL",
    "   water_level:",
    "      file: NULL",
    "input:",
    "   init_temp_profile:",
    "      file: LakeEnsemblR_init_temp_profile.csv",
    "   meteo:",
    "      file: LakeEnsemblR_meteo_standard.csv",
    "   light:",
    "      Kw:",
    sprintf("         all: %.4f", Kw),
    "   ice:",
    "      use: true",
    "inflows:",
    "   use: false",
    "   file: NULL",
    "   number_inflows: 0",
    "   fix_wlvl: false",
    "outflows:",
    "   use: false",
    "   file: NULL",
    "   number_outflows: 0",
    "   outflow_lvl: -1",
    "output:",
    "   file: ensemble_output",
    "   format: netcdf",
    sprintf("   depths: %.4f", lake$out_depth),
    "   compression: 4",
    "   max_members: 25",
    "   time_unit: hour",
    "   time_step: 1",
    "   time_method: mean",
    "   variables:",
    "      - temp",
    "      - ice_height",
    "scaling_factors:",
    "   all:",
    "      wind_speed: 1.0",
    "      swr: 1.0",
    "model_parameters:",
    "   FLake:",
    sprintf("      fetch_lk: %.2f", lake$fetch),
    "   GLM:",
    sprintf("      bsn_len: %.2f", lake$bsn_len),
    sprintf("      bsn_wid: %.2f", lake$bsn_wid),
    "   GOTM:",
    "      k_min: 3.6E-6",
    "   Simstrat:",
    "   MyLake:"
  )
  writeLines(txt, path)
  invisible(path)
}

.ler_set_ice_init <- function(model, ice_init) {
  if (ice_init <= 0) return(invisible(FALSE))
  if (model == "GOTM") {
    f <- file.path("GOTM", "gotm.yaml")
    y <- readLines(f, warn = FALSE)
    i <- grep("^[[:space:]]*ice:", y)[1]
    j <- i + grep("^[[:space:]]*H:", y[(i + 1):length(y)])[1]
    y[j] <- sub("H:[[:space:]]*[-0-9.eE+]+", sprintf("H: %.4f", ice_init), y[j])
    writeLines(y, f)
    return(invisible(TRUE))
  }
  if (model == "GLM") {
    f <- file.path("GLM", "glm3.nml")
    y <- readLines(f, warn = FALSE)
    i <- grep("^[[:space:]]*&init_profiles", y)[1]
    y <- append(y, sprintf("   blue_ice_thickness = %.4f", ice_init), after = i)
    writeLines(y, f)
    return(invisible(TRUE))
  }
  warning(sprintf("ice_init is not supported for %s; it starts ice-free", model))
  invisible(FALSE)
}

.ler_run_one <- function(wd, lake, meteo, start, stop, model, Kw, init_temp, ice_init) {
  old <- setwd(wd)
  on.exit(setwd(old), add = TRUE)

  met <- meteo
  met$datetime <- format(met$datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  utils::write.csv(met, "LakeEnsemblR_meteo_standard.csv", row.names = FALSE, quote = FALSE)
  utils::write.csv(lake$hypsograph[[1]], "LakeEnsemblR_bathymetry_standard.csv",
                   row.names = FALSE, quote = FALSE)
  zs <- unique(c(seq(0, lake$model_depth, length.out = 5)))
  utils::write.csv(data.frame(Depth_meter = zs, Water_Temperature_celsius = init_temp),
                   "LakeEnsemblR_init_temp_profile.csv", row.names = FALSE, quote = FALSE)
  .ler_write_yaml("LakeEnsemblR.yaml", lake, start, stop, Kw)

  utils::capture.output({
    LakeEnsemblR::export_config("LakeEnsemblR.yaml", model = model)
    .ler_set_ice_init(model, ice_init)
    LakeEnsemblR::run_ensemble("LakeEnsemblR.yaml", model = model)
  })

  nc  <- file.path("output", "ensemble_output.nc")
  tl  <- LakeEnsemblR::load_var(nc, "temp", return = "list", print = FALSE)[[model]]
  il  <- LakeEnsemblR::load_var(nc, "ice_height", return = "list", print = FALSE)[[model]]
  if (is.null(tl)) stop(sprintf("no temperature output from %s", model))

  zc  <- grep("^wtr_", names(tl), value = TRUE)
  zv  <- as.numeric(sub("^wtr_", "", zc))
  col <- zc[which.min(abs(zv - lake$out_depth))]
  dt  <- as.POSIXct(format(tl$datetime, "%Y-%m-%d %H:%M:%S"), tz = "UTC")
  ice <- if (is.null(il)) rep(0, nrow(tl)) else as.numeric(il$ice_height)

  # Models leave the first output step NA; MyLake reports daily only, so its
  # values are interpolated to hours (no diurnal cycle).
  gap <- if (model == "MyLake") 24 else 3
  if (model == "MyLake")
    warning("MyLake reports daily values; interpolated to hourly without a diurnal cycle")
  edge_fill <- function(x) zoo::na.approx(x, maxgap = gap, rule = 2, na.rm = FALSE)
  temp <- edge_fill(as.numeric(tl[[col]]))
  ice  <- edge_fill(ice)
  ice[is.na(ice)] <- 0

  data.frame(datetime = dt, temp = temp, ice_height = ice)
}

.ler_run_lake <- function(lake, meteo, start, stop, models, Kw,
                          init_temp = 4, ice_init = 0, run_dir) {
  .ler_require()
  # LakeEnsemblR's netCDF writer looks up its lazy-loaded `lake_var_dic`
  # dataset unqualified, which only resolves when the package is attached.
  if (!"package:LakeEnsemblR" %in% search()) {
    attachNamespace("LakeEnsemblR")
    on.exit(detach("package:LakeEnsemblR", character.only = TRUE), add = TRUE)
  }
  # Some models (e.g. Simstrat) stop writing output a day before `stop`.
  # Run one extra day on repeated final-day forcing, then trim it off.
  pad <- utils::tail(meteo, 24)
  pad$datetime <- pad$datetime + 86400
  met_pad  <- rbind(meteo, pad)
  stop_pad <- stop + 86400

  res  <- list()
  for (m in models) {
    wd <- file.path(run_dir, lake$lake_id, m)
    dir.create(wd, recursive = TRUE, showWarnings = FALSE)
    out <- tryCatch(
      .ler_run_one(wd, lake, met_pad, start, stop_pad, m, Kw, init_temp, ice_init),
      error = function(e) {
        warning(sprintf("model %s failed for lake %s: %s", m, lake$lake_id, conditionMessage(e)))
        NULL
      })
    if (!is.null(out)) res[[m]] <- out[out$datetime >= start & out$datetime <= stop, , drop = FALSE]
  }
  res
}

.resolve_lake_clim <- function(x, period_label, study_area) {
  if (is.data.frame(x)) return(x)
  if (!is.character(x)) stop("climate input must be a data.frame, an RDS file path, or a directory")
  path <- .resolve_rds_path(x, "Climate", period_label, study_area)
  readr::read_rds(path)
}

.month_end_hour <- function(d) {
  last_day <- seq(d, by = "1 month", length.out = 2)[2] - 1
  as.POSIXct(sprintf("%s 23:00:00", format(last_day, "%Y-%m-%d")), tz = "UTC")
}

#' Model Lake Surface Water Temperature with a LakeEnsemblR Ensemble
#'
#' Runs one-dimensional lake models (via LakeEnsemblR) for each prepared
#' lake, driven by the same weather forcing as the microclimate model, and
#' writes the hourly ensemble-mean near-surface water temperature onto the
#' landcover-water cells of the microclimate grid.
#'
#' @details
#' \strong{Weather.} \code{clim} (and \code{spinup_clim}) is either the
#' packaged climate used by \code{\link{run_micro_big_nichemap}} (an RDS from
#' \code{\link{package_climate}}, or a directory of them resolved per period
#' as \code{{study_area}_Climate_{period_label}.RDS}) or an hourly
#' \code{climdata} data.frame from \code{\link{build_climdata_crn_era5}}.
#' Packaged climate is sampled at the cell nearest each lake centroid; a
#' data.frame forces every lake identically. Wind from a climdata
#' data.frame (2 m) is scaled to the 10 m LakeEnsemblR expects with a log
#' profile (z0 = 0.01 m). Gaps up to 6 h are interpolated; longer gaps or
#' missing hours stop with an error.
#'
#' \strong{Spin-up.} Lake models start from a uniform \code{init_temp}
#' profile. To let ice-out emerge from real weather, supply
#' \code{spinup_clim} covering \code{spinup_start} (default 1 October of the
#' year before each period) up to the period start; only the period is kept.
#' Without \code{spinup_clim} the run starts at the period start, optionally
#' with \code{ice_init} metres of starting ice (GLM and GOTM only).
#'
#' \strong{Ensemble.} Each chosen model runs in its own folder with inflows
#' and outflows off. Temperature is taken at each lake's \code{out_depth}
#' (\code{min(0.5, model_depth / 2)}). \code{water_temp} is the mean across
#' models that ran; a failing model is dropped with a warning, and the run
#' stops if all models fail for a lake. During ice cover each model reports
#' its under-ice water temperature; \code{ice_frac} is the share of models
#' with ice that hour. MyLake reports daily values only (interpolated to
#' hours without a diurnal cycle), so it is not in the default \code{models}.
#'
#' \strong{Grid.} Every \code{landcover} cell whose class is in
#' \code{water_classes} takes the series of the lake it overlaps or touches,
#' else the nearest lake within \code{max_match_dist} metres; other cells
#' are \code{NA}.
#'
#' \strong{Known limitations.} Ponds shallower than their
#' \code{depth_floor} (see \code{\link{prepare_lakes}}) carry extra thermal
#' mass and respond more slowly than reality. Several models fail on very
#' shallow water, so ponds near the floor may be run by only some models.
#' One value per lake ignores warmer near-shore shallows.
#'
#' @param lakes Output of \code{\link{prepare_lakes}}.
#' @param clim Period weather: packaged climate RDS path or directory, or a
#'   climdata data.frame.
#' @param dates A data.frame with \code{Start_Dates}/\code{End_Dates}, or a
#'   length-2 Date vector. Day components are ignored; whole months are run.
#' @param landcover A \code{SpatRaster} (or path) on the microclimate grid.
#' @param water_classes Landcover value(s) that denote water (e.g. 11 for
#'   NLCD open water).
#' @param out_dir Output directory.
#' @param Kw Light extinction coefficient (m^-1): a single number, a named
#'   numeric vector by \code{lake_id}, or \code{NULL} to use a \code{Kw}
#'   column in \code{lakes}. Required.
#' @param models Lake models to run; any of \code{"FLake"}, \code{"GLM"},
#'   \code{"GOTM"}, \code{"Simstrat"}, \code{"MyLake"}. Default all but MyLake.
#' @param spinup_clim Optional spin-up weather, same forms as \code{clim}.
#'   A directory is resolved with the label from \code{spinup_start} to the
#'   month before the period start.
#' @param spinup_start Optional Date for the spin-up start. Default 1
#'   October of the year before each period start.
#' @param init_temp Initial uniform water temperature (degC). Default 4.
#' @param ice_init Optional initial ice thickness (m). Default 0.
#' @param max_match_dist Maximum distance (m) for matching a water cell to
#'   the nearest lake. Default 60.
#' @param study_area Optional study area name, used as file prefix and to
#'   resolve climate files.
#' @param keep_runs Logical. Keep the per-model LakeEnsemblR run folders
#'   (\code{out_dir/LER_runs}). Default \code{FALSE}.
#'
#' @return Invisibly, a log data.frame with columns \code{period_label},
#'   \code{lake_id}, \code{models_ok}, \code{models_failed} (comma-joined),
#'   \code{n_cells}, and \code{nc_file}. Writes
#'   \code{{study_area}_WaterTemp_{period_label}.nc} (variables
#'   \code{water_temp} in degC and \code{ice_frac}) and
#'   \code{{study_area}_WaterTemp_Lakes_{period_label}.csv} (per-lake
#'   ensemble and per-model series).
#' @seealso \code{\link{generate_lake_bathymetry}}, \code{\link{prepare_lakes}},
#'   \code{\link{package_climate}}, \code{\link{build_climdata_crn_era5}}
#' @export
run_lake_watertemp <- function(lakes,
                               clim,
                               dates,
                               landcover,
                               water_classes,
                               out_dir,
                               Kw,
                               models         = c("FLake", "GLM", "GOTM", "Simstrat"),
                               spinup_clim    = NULL,
                               spinup_start   = NULL,
                               init_temp      = 4,
                               ice_init       = 0,
                               max_match_dist = 60,
                               study_area     = NULL,
                               keep_runs      = FALSE) {
  models <- match.arg(models, c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"), several.ok = TRUE)
  if (!inherits(lakes, "sf") || !"hypsograph" %in% names(lakes))
    stop("lakes must be the output of prepare_lakes()")
  if (!inherits(landcover, "SpatRaster")) landcover <- terra::rast(landcover)
  n <- nrow(lakes)

  kw <- if (is.null(Kw)) {
    if (!"Kw" %in% names(lakes)) stop("Kw is NULL and lakes has no Kw column")
    lakes$Kw
  } else if (length(Kw) == 1 && is.null(names(Kw))) {
    rep(Kw, n)
  } else {
    miss <- setdiff(lakes$lake_id, names(Kw))
    if (length(miss) > 0)
      stop(sprintf("Kw has no value for lake(s): %s", paste(miss, collapse = ", ")))
    unname(Kw[lakes$lake_id])
  }
  if (anyNA(kw) || any(kw <= 0)) stop("Kw must be positive for every lake")

  if (is.data.frame(dates)) {
    if (!all(c("Start_Dates", "End_Dates") %in% names(dates)))
      stop("dates data.frame must contain columns 'Start_Dates' and 'End_Dates'")
    starts <- as.Date(dates$Start_Dates); ends <- as.Date(dates$End_Dates)
  } else {
    if (length(dates) != 2) stop("dates must be a data.frame or a length-2 Date vector")
    starts <- as.Date(dates[1]); ends <- as.Date(dates[2])
  }

  prefix <- if (is.null(study_area)) "" else sprintf("%s_", study_area)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  lake_idx <- .map_lakes_to_grid(lakes, landcover, water_classes, max_match_dist)
  n_cells  <- tabulate(terra::values(lake_idx, mat = FALSE), nbins = n)

  log <- list()
  for (k in seq_along(starts)) {
    p      <- .normalize_period(starts[k], ends[k])
    label  <- p$period_label
    pstart <- as.POSIXct(format(p$start_date, "%Y-%m-%d"), tz = "UTC")
    pstop  <- .month_end_hour(p$end_date)
    cat(sprintf("Period %s\n", label))

    clim_p <- .resolve_lake_clim(clim, label, study_area)

    if (is.null(spinup_clim)) {
      message(sprintf("No spin-up climate supplied for %s; spin-up skipped, run starts at the period start", label))
      run_start <- pstart
      clim_s    <- NULL
    } else {
      ss <- if (is.null(spinup_start)) {
        as.Date(sprintf("%d-10-01", as.integer(format(p$start_date, "%Y")) - 1L))
      } else {
        as.Date(spinup_start)
      }
      if (ss >= p$start_date) stop("spinup_start must be before the period start")
      prev_month <- seq(p$start_date, by = "-1 month", length.out = 2)[2]
      clim_s     <- .resolve_lake_clim(spinup_clim, .normalize_period(ss, prev_month)$period_label,
                                       study_area)
      run_start  <- as.POSIXct(format(ss, "%Y-%m-%d"), tz = "UTC")
    }

    run_dir <- file.path(out_dir, "LER_runs", label)
    ens <- vector("list", n)
    for (i in seq_len(n)) {
      lake <- lakes[i, ]
      met  <- .lake_meteo(clim_p, lake$lon, lake$lat, pstart, pstop)
      if (!is.null(clim_s))
        met <- rbind(.lake_meteo(clim_s, lake$lon, lake$lat, run_start, pstart - 3600), met)

      mo <- .ler_run_lake(lake, met, run_start, pstop, models, Kw = kw[i],
                          init_temp = init_temp, ice_init = ice_init, run_dir = run_dir)
      if (length(mo) == 0)
        stop(sprintf("all models failed for lake %s (period %s)", lake$lake_id, label))
      ens[[i]] <- .ensemble_lake(mo, pstart, pstop)

      log[[length(log) + 1]] <- data.frame(
        period_label  = label,
        lake_id       = lake$lake_id,
        models_ok     = paste(names(mo), collapse = ","),
        models_failed = paste(setdiff(models, names(mo)), collapse = ","),
        n_cells       = n_cells[i],
        nc_file       = file.path(out_dir, sprintf("%sWaterTemp_%s.nc", prefix, label)),
        stringsAsFactors = FALSE)
    }

    nc_file <- file.path(out_dir, sprintf("%sWaterTemp_%s.nc", prefix, label))
    .write_watertemp_nc(lake_idx, lapply(ens, function(e) e[, c("datetime", "water_temp", "ice_frac")]),
                        nc_file)

    all_cols <- unique(unlist(lapply(ens, names)))
    tab <- do.call(rbind, lapply(seq_len(n), function(i) {
      e <- ens[[i]]
      for (cc in setdiff(all_cols, names(e))) e[[cc]] <- NA_real_
      cbind(lake_id = lakes$lake_id[i], e[, all_cols])
    }))
    tab$datetime <- format(tab$datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    utils::write.csv(tab, file.path(out_dir, sprintf("%sWaterTemp_Lakes_%s.csv", prefix, label)),
                     row.names = FALSE)

    if (!keep_runs) unlink(run_dir, recursive = TRUE)
    rm(ens, tab, clim_p, clim_s); invisible(gc())
  }

  runs_root <- file.path(out_dir, "LER_runs")
  if (!keep_runs && dir.exists(runs_root) && length(list.files(runs_root)) == 0)
    unlink(runs_root, recursive = TRUE)

  invisible(do.call(rbind, log))
}
