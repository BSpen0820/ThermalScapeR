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
