.parse_meminfo_bytes <- function(lines) {
  hit <- grep("^MemTotal:", lines, value = TRUE)
  if (length(hit) == 0L) stop("could not find MemTotal in /proc/meminfo")
  as.numeric(gsub("[^0-9]", "", hit[1])) * 1024
}

.parse_single_number <- function(x) {
  x <- trimws(x)
  x <- x[nzchar(x)]
  if (length(x) == 0L) return(NA_real_)
  suppressWarnings(as.numeric(x[1]))
}

.windows_total_ram_kb <- function() {
  ps_cmd <- "\"(Get-CimInstance -ClassName Win32_OperatingSystem).TotalVisibleMemorySize\""
  for (exe in unname(Sys.which(c("powershell", "pwsh")))) {
    if (!nzchar(exe)) next
    out <- tryCatch(
      suppressWarnings(system2(exe, c("-NoProfile", "-NonInteractive", "-Command", ps_cmd),
                               stdout = TRUE, stderr = FALSE)),
      error = function(e) character(0)
    )
    kb <- .parse_single_number(out)
    if (is.finite(kb)) return(kb)
  }
  wmic <- unname(Sys.which("wmic"))
  if (nzchar(wmic)) {
    out <- tryCatch(
      suppressWarnings(system2(wmic, c("OS", "get", "TotalVisibleMemorySize", "/Value"),
                               stdout = TRUE, stderr = FALSE)),
      error = function(e) character(0)
    )
    kb <- as.numeric(gsub("[^0-9]", "", out[grepl("=", out)]))[1]
    if (is.finite(kb)) return(kb)
  }
  NA_real_
}

.get_total_ram <- function(sysname = Sys.info()[["sysname"]]) {
  bytes <- switch(
    sysname,
    Windows = .windows_total_ram_kb() * 1024,
    Darwin  = .parse_single_number(tryCatch(
      suppressWarnings(system2("sysctl", c("-n", "hw.memsize"), stdout = TRUE, stderr = FALSE)),
      error = function(e) character(0)
    )),
    if (file.exists("/proc/meminfo")) .parse_meminfo_bytes(readLines("/proc/meminfo")) else NA_real_
  )
  if (!is.finite(bytes) || bytes <= 0)
    stop(sprintf("Unable to determine total RAM on this platform (%s)", sysname))
  bytes
}

# --------------------------------------------------------------------------- #
#  Grid-matching validation helpers
# --------------------------------------------------------------------------- #

.check_grid_match <- function(ref, target, ref_label, target_label,
                               action = c("stop", "warn"), tol = 1e-6) {
  action <- match.arg(action)
  ref_res    <- terra::res(ref)
  target_res <- terra::res(target)
  ref_origin    <- terra::origin(ref)
  target_origin <- terra::origin(target)

  res_ok    <- isTRUE(all.equal(ref_res, target_res, tolerance = tol))
  crs_ok    <- terra::same.crs(ref, target)
  origin_ok <- isTRUE(all.equal(ref_origin, target_origin, tolerance = tol))

  if (res_ok && crs_ok && origin_ok) return(invisible(TRUE))

  msg <- sprintf(
    "%s and %s do not share the same grid:\n  %s resolution: %s\n  %s resolution: %s\n  %s origin: %s\n  %s origin: %s\n  CRS match: %s",
    ref_label, target_label,
    ref_label, paste(signif(ref_res, 10), collapse = " x "),
    target_label, paste(signif(target_res, 10), collapse = " x "),
    ref_label, paste(signif(ref_origin, 10), collapse = " x "),
    target_label, paste(signif(target_origin, 10), collapse = " x "),
    crs_ok
  )

  if (action == "stop") stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  invisible(FALSE)
}

.first_spatraster <- function(x) {
  if (inherits(x, "SpatRaster")) return(x)
  if (inherits(x, "PackedSpatRaster")) return(terra::unwrap(x))
  if (is.list(x)) {
    for (el in x) {
      r <- tryCatch(.first_spatraster(el), error = function(e) NULL)
      if (!is.null(r)) return(r)
    }
  }
  stop("No SpatRaster found")
}

#' Create Tiles for Large Raster Processing
#'
#' Creates a tile raster for processing large rasters in memory-efficient chunks.
#' When \code{tile_dims} is \code{NULL} (default), automatically selects the
#' largest tile size whose peak in-memory footprint stays within \code{mem_fraction}
#' of total system RAM. Peak usage accounts for \code{mout} (10 arrays),
#' optionally \code{smod} (6 arrays) when \code{snow_modeling = TRUE}, and
#' \code{micropointa} (one element per coarse cell) -- all sized to the buffered
#' tile extent (\code{tiles_proc}).
#'
#' @param coarse_dem A \code{SpatRaster} at the weather-data resolution used as
#'   the tile grid. Must match the resolution of climate inputs passed to
#'   downstream micropoint model functions.
#' @param fine_dem A \code{SpatRaster} at the fine (e.g. 30 m) resolution.
#'   Used to compute the fine-to-coarse scale ratio for memory estimation and
#'   to resample the output tile ID raster.
#' @param dates Length-2 vector of class \code{Date} giving the start and end
#'   of the modeling period e.g. \code{as.Date(c("2020-01-01", "2020-12-31"))}.
#'   Used to compute the number of hourly time steps for memory estimation.
#' @param tile_dims Either \code{NULL} (default), a single numeric value, or a
#'   vector of two numeric values specifying \code{c(nrow, ncol)} of each tile
#'   in coarse DEM cells. When \code{NULL} the memory-based auto-sizing is used.
#'   When provided, a warning is issued if the tile exceeds the memory budget but
#'   processing continues. Each dimension must be at least
#'   \code{2 * buffer_size + 1} to ensure at least one non-buffer cell per
#'   dimension. Tile dimensions need not divide evenly into the DEM; edge tiles
#'   will be slightly smaller.
#' @param buffer_size Integer. Number of coarse DEM cells added as buffer around
#'   each tile for \code{tiles_proc}. Default is 1. Memory estimation uses the
#'   buffered tile size as peak usage occurs on \code{tiles_proc}, not
#'   \code{tiles_core}.
#' @param snow_modeling Logical. If \code{TRUE}, the memory estimate includes
#'   \code{smod} (6 arrays) in addition to \code{mout} (10 arrays). Default is
#'   \code{FALSE}.
#' @param mem_fraction Numeric. Fraction of total system RAM to target for peak
#'   tile memory usage. Default is 0.7.
#' @param return_tiles_rast Logical. If \code{TRUE}, includes the fine-resolution
#'   tile ID raster (\code{tiles_rast}) in the returned list. Default is
#'   \code{FALSE}.
#' @param output_path Character. File path to save the fine-resolution tile ID
#'   raster. If \code{NULL} (default), not saved.
#'
#' @return A list containing:
#'   \item{tiles_proc}{Matrix of tile extents including buffer for processing}
#'   \item{tiles_core}{Matrix of tile extents without buffer (core areas)}
#'   \item{tiles_rast}{Fine-resolution \code{SpatRaster} with tile IDs (only if
#'     \code{return_tiles_rast = TRUE})}
#'
#' @details
#' Memory estimation formula per tile:
#' \code{n_eff_coarse * n_hours * (8 * n_arrays * ratio_r * ratio_c + 257)}
#' where \code{n_eff_coarse = (tile_nrow + 2*buffer_size) * (tile_ncol + 2*buffer_size)},
#' \code{n_arrays} is 16 (snow) or 10 (no snow), \code{ratio_r}/\code{ratio_c}
#' are the fine-to-coarse row/column ratios, and 257 bytes per coarse cell per
#' hour is an empirical estimate for \code{micropointa} overhead.
#'
#' \code{coarse_dem} and \code{fine_dem} are expected to share the same CRS
#' and to have a whole-number fine-to-coarse resolution ratio; a mismatch on
#' either produces a warning (not an error) since \code{create_tiles} only
#' uses the ratio for memory estimation and tile-ID resampling.
#'
#' @examples
#' \dontrun{
#'   # Memory-based auto sizing (recommended)
#'   tiles <- create_tiles(coarse_dem, fine_dem,
#'                         dates = as.Date(c("2020-01-01", "2020-12-31")))
#'
#'   # With snow modeling -- more conservative tile size
#'   tiles <- create_tiles(coarse_dem, fine_dem,
#'                         dates = as.Date(c("2020-10-01", "2021-03-31")),
#'                         snow_modeling = TRUE)
#'
#'   # Manual override with 2-cell buffer, save fine-res tile raster
#'   tiles <- create_tiles(coarse_dem, fine_dem,
#'                         dates = as.Date(c("2020-01-01", "2020-12-31")),
#'                         tile_dims = c(4, 4),
#'                         buffer_size = 2,
#'                         output_path = "./Data/TileRast.tif")
#' }
#'
#' @export
create_tiles <- function(coarse_dem,
                         fine_dem,
                         dates,
                         tile_dims         = NULL,
                         buffer_size       = 1,
                         snow_modeling     = FALSE,
                         mem_fraction      = 0.7,
                         return_tiles_rast = FALSE,
                         output_path       = NULL) {

  dates   <- as.Date(dates)
  n_hours <- as.integer(difftime(dates[2], dates[1], units = "days") + 1L) * 24L
  min_dim <- 2L * buffer_size + 1L

  n_arrays   <- if (snow_modeling) 16L else 10L
  mem_budget <- .get_total_ram() * mem_fraction

  ratio_r <- nrow(fine_dem) / nrow(coarse_dem)
  ratio_c <- ncol(fine_dem) / ncol(coarse_dem)

  if (!terra::same.crs(coarse_dem, fine_dem)) {
    warning("coarse_dem and fine_dem have different CRS; tile extents may not align correctly.")
  }
  if (abs(ratio_r - round(ratio_r)) > 1e-6 || abs(ratio_c - round(ratio_c)) > 1e-6) {
    warning(sprintf(
      "fine_dem resolution is not a whole-number multiple of coarse_dem resolution (row ratio = %.4f, col ratio = %.4f); tile boundaries may not align to whole fine-DEM cells.",
      ratio_r, ratio_c))
  }

  bytes_per_eff_coarse <- n_hours * (8 * n_arrays * ratio_r * ratio_c + 257)
  max_eff_coarse       <- mem_budget / bytes_per_eff_coarse

  .est_mem_gb <- function(tnr, tnc) {
    n_eff <- (tnr + 2 * buffer_size) * (tnc + 2 * buffer_size)
    n_eff * bytes_per_eff_coarse / 1024^3
  }

  if (is.null(tile_dims)) {

    nr <- nrow(coarse_dem)
    nc <- ncol(coarse_dem)
    max_splits_r <- floor(nr / min_dim)
    max_splits_c <- floor(nc / min_dim)

    best_score <- 0
    n_tiles_r  <- NA_integer_
    n_tiles_c  <- NA_integer_

    for (sr in seq_len(max_splits_r)) {
      tile_r <- ceiling(nr / sr)
      for (sc in seq_len(max_splits_c)) {
        tile_c <- ceiling(nc / sc)
        n_eff  <- (tile_r + 2 * buffer_size) * (tile_c + 2 * buffer_size)
        if (n_eff <= max_eff_coarse) {
          score <- tile_r * tile_c * (min(tile_r, tile_c) / max(tile_r, tile_c))
          if (score > best_score) {
            best_score <- score
            n_tiles_r  <- sr
            n_tiles_c  <- sc
          }
        }
      }
    }

    if (is.na(n_tiles_r)) {
      stop(sprintf(
        "Even a %dx%d coarse tile (buffered to %dx%d) exceeds %.0f%% of RAM (%.1f GB).\n  Reduce mem_fraction, use a shorter date range, or add more RAM.",
        min_dim, min_dim,
        min_dim + 2 * buffer_size, min_dim + 2 * buffer_size,
        mem_fraction * 100,
        mem_budget / 1024^3
      ))
    }

    tile_nrow <- ceiling(nr / n_tiles_r)
    tile_ncol <- ceiling(nc / n_tiles_c)

    cat(sprintf(
      "Auto tile size: %d x %d coarse cells (%d x %d grid, ~%.2f GB estimated peak memory)\n",
      tile_nrow, tile_ncol, n_tiles_r, n_tiles_c,
      .est_mem_gb(tile_nrow, tile_ncol)
    ))

  } else if (length(tile_dims) == 1) {
    tile_nrow <- tile_dims
    tile_ncol <- tile_dims
  } else if (length(tile_dims) == 2) {
    tile_nrow <- tile_dims[1]
    tile_ncol <- tile_dims[2]
  } else {
    stop("tile_dims must be NULL, a single numeric value, or a vector of length 2.")
  }

  if (!is.null(tile_dims)) {
    if (tile_nrow < min_dim || tile_ncol < min_dim) {
      stop(sprintf(
        "Tile dimensions (%d, %d) are below the minimum (%d x %d) required for buffer_size = %d.",
        tile_nrow, tile_ncol, min_dim, min_dim, buffer_size
      ))
    }
    est_gb <- .est_mem_gb(tile_nrow, tile_ncol)
    if (est_gb > mem_budget / 1024^3) {
      warning(sprintf(
        "Specified tile (%dx%d) estimated at %.2f GB, exceeding %.0f%% RAM budget (%.1f GB). Proceeding anyway.",
        tile_nrow, tile_ncol, est_gb, mem_fraction * 100, mem_budget / 1024^3
      ))
    } else {
      cat(sprintf(
        "Tile size: %d x %d coarse cells (~%.2f GB estimated peak memory)\n",
        tile_nrow, tile_ncol, est_gb
      ))
    }
    n_tiles_r <- ceiling(nrow(coarse_dem) / tile_nrow)
    n_tiles_c <- ceiling(ncol(coarse_dem) / tile_ncol)
  }

  tiles_rast_coarse <- terra::rast(
    nrow   = n_tiles_r,
    ncol   = n_tiles_c,
    extent = terra::ext(coarse_dem),
    crs    = terra::crs(coarse_dem),
    vals   = seq_len(n_tiles_r * n_tiles_c)
  )

  tiles_proc <- terra::getTileExtents(coarse_dem, tiles_rast_coarse, buffer = buffer_size, extend = TRUE)
  tiles_core <- terra::getTileExtents(coarse_dem, tiles_rast_coarse, buffer = 0,           extend = TRUE)

  rownames(tiles_proc) <- seq_len(nrow(tiles_proc))
  rownames(tiles_core) <- seq_len(nrow(tiles_core))

  tiles_rast <- terra::rast(fine_dem)
  terra::values(tiles_rast) <- NA_integer_
  for (i in seq_len(nrow(tiles_core))) {
    ext_i    <- terra::ext(tiles_core[i, "xmin"], tiles_core[i, "xmax"],
                            tiles_core[i, "ymin"], tiles_core[i, "ymax"])
    cell_ids <- terra::cells(tiles_rast, ext_i)
    tiles_rast[cell_ids] <- i
  }

  names(tiles_rast) <- "tile_id"

  cat(sprintf("Created %d tile(s) (%d x %d grid).\n", n_tiles_r * n_tiles_c, n_tiles_r, n_tiles_c))

  if (!is.null(output_path)) {
    terra::writeRaster(tiles_rast, output_path, overwrite = TRUE)
    cat(sprintf("Tile raster saved to: %s\n", output_path))
  }

  result <- list(tiles_proc = tiles_proc, tiles_core = tiles_core)
  if (return_tiles_rast) result$tiles_rast <- tiles_rast

  return(invisible(result))
}

# --------------------------------------------------------------------------- #
#  trim_tile_buffer -- exported
# --------------------------------------------------------------------------- #

#' Trim buffer rows and columns from microclimf tile output arrays
#'
#' Removes the spatial buffer from every 3D array in an \code{mout} or
#' \code{smod} list by comparing the extents of the buffered processing
#' raster (\code{dem_proc}) and the core raster (\code{dem_core}).
#' Non-array elements (e.g., \code{$tme}) are passed through unchanged.
#'
#' @param data List. An \code{mout} or \code{smod} object whose 3D arrays
#'   have spatial dimensions matching the extent of \code{dem_proc}.
#' @param dem_proc \code{SpatRaster} covering the buffered tile extent used
#'   during modeling (i.e., the \code{tiles_proc} raster). Its resolution
#'   and extent define the outer bounds of the arrays.
#' @param dem_core \code{SpatRaster} covering the core tile extent to retain
#'   (i.e., the \code{tiles_core} raster). Its extent must be contained
#'   within that of \code{dem_proc}.
#'
#' @return A list with the same structure as \code{data}: all 3D arrays are
#'   spatially trimmed to the \code{dem_core} footprint; non-3D elements
#'   are returned unchanged.
#'
#' @seealso \code{\link{create_tiles}} for generating \code{tiles_proc} and
#'   \code{tiles_core} extents. \code{\link{write_tile}} for writing trimmed
#'   tiles to disk.
#'
#' @export
trim_tile_buffer <- function(data, dem_proc, dem_core) {
  e_proc <- terra::ext(dem_proc)
  e_core <- terra::ext(dem_core)
  res_x  <- terra::res(dem_proc)[1]
  res_y  <- terra::res(dem_proc)[2]

  row_top  <- as.integer(round((as.numeric(e_proc$ymax) - as.numeric(e_core$ymax)) / res_y))
  row_bot  <- as.integer(round((as.numeric(e_core$ymin) - as.numeric(e_proc$ymin)) / res_y))
  col_left <- as.integer(round((as.numeric(e_core$xmin) - as.numeric(e_proc$xmin)) / res_x))
  col_rght <- as.integer(round((as.numeric(e_proc$xmax) - as.numeric(e_core$xmax)) / res_x))

  if (any(c(row_top, row_bot, col_left, col_rght) < 0L))
    stop("dem_core extent exceeds dem_proc on at least one side -- check raster inputs")

  is_3d <- vapply(data, function(x) length(dim(x)) == 3L, logical(1L))
  if (!any(is_3d)) stop("No 3D arrays found in data")

  first <- data[[which(is_3d)[1L]]]
  nr    <- dim(first)[1L]
  nc    <- dim(first)[2L]

  if (nr - row_top - row_bot < 1L || nc - col_left - col_rght < 1L)
    stop("Computed trim exceeds array dimensions -- dem_proc and dem_core may not match the data arrays")

  row_idx <- seq.int(row_top  + 1L, nr - row_bot)
  col_idx <- seq.int(col_left + 1L, nc - col_rght)

  result <- lapply(data, function(x) {
    if (length(dim(x)) == 3L) x[row_idx, col_idx, , drop = FALSE] else x
  })
  names(result) <- names(data)
  result
}

# --------------------------------------------------------------------------- #
#  Metadata lookup lists for write_tile / stitch_tiles
# --------------------------------------------------------------------------- #

.mout_meta <- list(
  Tz        = list(units = "degrees_C", long_name = "Air/soil temperature at model height"),
  tleaf     = list(units = "degrees_C", long_name = "Leaf temperature"),
  relhum    = list(units = "percent",   long_name = "Relative humidity"),
  soilm     = list(units = "m3 m-3",   long_name = "Volumetric soil moisture"),
  windspeed = list(units = "m s-1",    long_name = "Wind speed at model height"),
  Rdirdown  = list(units = "W m-2",    long_name = "Downward direct shortwave radiation"),
  Rdifdown  = list(units = "W m-2",    long_name = "Downward diffuse shortwave radiation"),
  Rlwdown   = list(units = "W m-2",    long_name = "Downward longwave radiation"),
  Rswup     = list(units = "W m-2",    long_name = "Upward shortwave radiation"),
  Rlwup     = list(units = "W m-2",    long_name = "Upward longwave radiation")
)

.smod_meta <- list(
  Tc              = list(units = "degrees_C", long_name = "Canopy air temperature"),
  Tg              = list(units = "degrees_C", long_name = "Ground surface temperature"),
  groundsnowdepth = list(units = "m",         long_name = "Snow depth on ground"),
  totalSWE        = list(units = "mm",        long_name = "Total snow water equivalent"),
  snowden         = list(units = "kg m-3",    long_name = "Snow density"),
  umu             = list(units = "",          long_name = "umu")
)

.mout_blw_meta <- list(
  Tz = list(units = "degrees_C", long_name = "Soil temperature")
)

.detect_data_type <- function(data) {
  nm <- names(data)
  if ("Tz" %in% nm) return("mout")
  if ("Tc" %in% nm) return("smod")
  stop("Cannot determine data type: names must include 'Tz' (mout) or 'Tc' (smod)")
}

# --------------------------------------------------------------------------- #
#  HDF5 write helper
# --------------------------------------------------------------------------- #

.write_h5 <- function(data_list, var_meta, tme, out_path, dtm, data_type,
                      compression) {
  if (file.exists(out_path)) file.remove(out_path)
  rhdf5::h5createFile(out_path)

  nrow_  <- dim(data_list[[1]])[1]
  ncol_  <- dim(data_list[[1]])[2]
  ntime_ <- dim(data_list[[1]])[3]
  chunk  <- c(nrow_, ncol_, min(ntime_, 24L))

  fid <- rhdf5::H5Fopen(out_path)
  tryCatch({
    for (vn in names(var_meta)) {
      rhdf5::h5createDataset(fid, vn,
                             dims         = c(nrow_, ncol_, ntime_),
                             storage.mode = "double",
                             chunk        = chunk,
                             level        = compression)
      rhdf5::h5write(data_list[[vn]], fid, vn)
      did <- rhdf5::H5Dopen(fid, vn)
      rhdf5::h5writeAttribute(var_meta[[vn]]$units,     did, "units")
      rhdf5::h5writeAttribute(var_meta[[vn]]$long_name, did, "long_name")
      rhdf5::H5Dclose(did)
    }

    if (!is.null(tme)) {
      time_str <- format(as.POSIXct(tme), "%Y-%m-%dT%H:%M:%S", tz = "UTC")
      rhdf5::h5write(time_str, fid, "time")
    }

    gid <- rhdf5::H5Gopen(fid, "/")
    tryCatch({
      if (!is.null(dtm)) {
        e <- terra::ext(dtm)
        rhdf5::h5writeAttribute(as.numeric(e$xmin),               gid, "xmin")
        rhdf5::h5writeAttribute(as.numeric(e$xmax),               gid, "xmax")
        rhdf5::h5writeAttribute(as.numeric(e$ymin),               gid, "ymin")
        rhdf5::h5writeAttribute(as.numeric(e$ymax),               gid, "ymax")
        rhdf5::h5writeAttribute(terra::res(dtm)[1],               gid, "res_x")
        rhdf5::h5writeAttribute(terra::res(dtm)[2],               gid, "res_y")
        rhdf5::h5writeAttribute(as.integer(terra::nrow(dtm)),     gid, "nrow")
        rhdf5::h5writeAttribute(as.integer(terra::ncol(dtm)),     gid, "ncol")
        rhdf5::h5writeAttribute(terra::crs(dtm, proj = FALSE),    gid, "crs_wkt")
      } else {
        warning("dtm is NULL: spatial metadata will not be written to HDF5 file.")
      }
      rhdf5::h5writeAttribute(data_type, gid, "data_type")
      if (!is.null(tme)) {
        rhdf5::h5writeAttribute(
          format(as.POSIXct(tme[1]),          "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
          gid, "period_start")
        rhdf5::h5writeAttribute(
          format(as.POSIXct(tme[length(tme)]), "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
          gid, "period_end")
      }
    }, finally = rhdf5::H5Gclose(gid))
  }, finally = {
    rhdf5::H5Fclose(fid)
    rhdf5::H5close()
  })
  invisible(NULL)
}

# --------------------------------------------------------------------------- #
#  HDF5 below-ground write helpers
# --------------------------------------------------------------------------- #

.write_h5_blw <- function(data_list, var_meta, tme, out_path, dtm,
                          depth_label, compression) {
  rhdf5::h5createFile(out_path)

  nrow_  <- dim(data_list[["Tz"]])[1]
  ncol_  <- dim(data_list[["Tz"]])[2]
  ntime_ <- dim(data_list[["Tz"]])[3]
  chunk  <- c(nrow_, ncol_, min(ntime_, 24L))

  fid <- rhdf5::H5Fopen(out_path)
  tryCatch({
    rhdf5::h5createGroup(fid, depth_label)
    ds_path <- sprintf("%s/Tz", depth_label)
    rhdf5::h5createDataset(fid, ds_path,
                           dims         = c(nrow_, ncol_, ntime_),
                           storage.mode = "double",
                           chunk        = chunk,
                           level        = compression)
    rhdf5::h5write(data_list[["Tz"]], fid, ds_path)
    did <- rhdf5::H5Dopen(fid, ds_path)
    rhdf5::h5writeAttribute(var_meta$Tz$units,     did, "units")
    rhdf5::h5writeAttribute(var_meta$Tz$long_name, did, "long_name")
    rhdf5::H5Dclose(did)

    if (!is.null(tme)) {
      time_str <- format(as.POSIXct(tme), "%Y-%m-%dT%H:%M:%S", tz = "UTC")
      rhdf5::h5write(time_str, fid, "time")
    }

    gid <- rhdf5::H5Gopen(fid, "/")
    tryCatch({
      if (!is.null(dtm)) {
        e <- terra::ext(dtm)
        rhdf5::h5writeAttribute(as.numeric(e$xmin),               gid, "xmin")
        rhdf5::h5writeAttribute(as.numeric(e$xmax),               gid, "xmax")
        rhdf5::h5writeAttribute(as.numeric(e$ymin),               gid, "ymin")
        rhdf5::h5writeAttribute(as.numeric(e$ymax),               gid, "ymax")
        rhdf5::h5writeAttribute(terra::res(dtm)[1],               gid, "res_x")
        rhdf5::h5writeAttribute(terra::res(dtm)[2],               gid, "res_y")
        rhdf5::h5writeAttribute(as.integer(terra::nrow(dtm)),     gid, "nrow")
        rhdf5::h5writeAttribute(as.integer(terra::ncol(dtm)),     gid, "ncol")
        rhdf5::h5writeAttribute(terra::crs(dtm, proj = FALSE),    gid, "crs_wkt")
      } else {
        warning("dtm is NULL: spatial metadata will not be written to HDF5 file.")
      }
      rhdf5::h5writeAttribute("mout_blw", gid, "data_type")
      if (!is.null(tme)) {
        rhdf5::h5writeAttribute(
          format(as.POSIXct(tme[1]),          "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
          gid, "period_start")
        rhdf5::h5writeAttribute(
          format(as.POSIXct(tme[length(tme)]), "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
          gid, "period_end")
      }
    }, finally = rhdf5::H5Gclose(gid))
  }, finally = {
    rhdf5::H5Fclose(fid)
    rhdf5::H5close()
  })
  invisible(NULL)
}

.append_h5_blw <- function(data_list, var_meta, out_path, depth_label,
                           compression) {
  nrow_  <- dim(data_list[["Tz"]])[1]
  ncol_  <- dim(data_list[["Tz"]])[2]
  ntime_ <- dim(data_list[["Tz"]])[3]
  chunk  <- c(nrow_, ncol_, min(ntime_, 24L))

  fid <- rhdf5::H5Fopen(out_path)
  tryCatch({
    rhdf5::h5createGroup(fid, depth_label)
    ds_path <- sprintf("%s/Tz", depth_label)
    rhdf5::h5createDataset(fid, ds_path,
                           dims         = c(nrow_, ncol_, ntime_),
                           storage.mode = "double",
                           chunk        = chunk,
                           level        = compression)
    rhdf5::h5write(data_list[["Tz"]], fid, ds_path)
    did <- rhdf5::H5Dopen(fid, ds_path)
    rhdf5::h5writeAttribute(var_meta$Tz$units,     did, "units")
    rhdf5::h5writeAttribute(var_meta$Tz$long_name, did, "long_name")
    rhdf5::H5Dclose(did)
  }, finally = {
    rhdf5::H5Fclose(fid)
    rhdf5::H5close()
  })
  invisible(NULL)
}

# --------------------------------------------------------------------------- #
#  NetCDF write helper
# --------------------------------------------------------------------------- #

.write_nc <- function(data_list, var_meta, tme, out_path, dtm, data_type,
                      compression) {

  nrow_  <- dim(data_list[[1]])[1]
  ncol_  <- dim(data_list[[1]])[2]
  ntime_ <- dim(data_list[[1]])[3]

  if (!is.null(dtm)) {

    x_vals <- terra::xFromCol(dtm, seq_len(ncol_))
    y_vals <- terra::yFromRow(dtm, seq_len(nrow_))

    is_lonlat <- terra::is.lonlat(dtm)

  } else {

    warning("dtm is NULL: coordinate variables will use integer indices.")

    x_vals <- seq_len(ncol_)
    y_vals <- seq_len(nrow_)
    is_lonlat <- FALSE
  }

  t_origin <- if (!is.null(tme)) {
    format(as.POSIXct(tme[1], tz = "UTC"),
           "%Y-%m-%dT%H:%M:%S")
  } else {
    "1970-01-01T00:00:00"
  }

  t_vals <- if (!is.null(tme)) {
    as.numeric(
      difftime(
        as.POSIXct(tme, tz = "UTC"),
        as.POSIXct(tme[1], tz = "UTC"),
        units = "hours"
      )
    )
  } else {
    seq_len(ntime_) - 1L
  }

  if (is_lonlat) {

    dim_x <- ncdf4::ncdim_def(
      "lon",
      "degrees_east",
      x_vals,
      longname = "longitude",
      create_dimvar = TRUE
    )

    dim_y <- ncdf4::ncdim_def(
      "lat",
      "degrees_north",
      y_vals,
      longname = "latitude",
      create_dimvar = TRUE
    )

  } else {

    dim_x <- ncdf4::ncdim_def(
      "x",
      "m",
      x_vals,
      longname = "x coordinate",
      create_dimvar = TRUE
    )

    dim_y <- ncdf4::ncdim_def(
      "y",
      "m",
      y_vals,
      longname = "y coordinate",
      create_dimvar = TRUE
    )
  }

  dim_time <- ncdf4::ncdim_def(
    "time",
    sprintf("hours since %s UTC", t_origin),
    t_vals,
    unlim = TRUE,
    longname = "time",
    calendar = "standard"
  )

  var_crs <- ncdf4::ncvar_def(
    "crs",
    "",
    list(),
    prec = "integer",
    longname = "CRS definition"
  )

  data_vars <- lapply(names(var_meta), function(vn) {

    ncdf4::ncvar_def(
      vn,
      var_meta[[vn]]$units,
      list(dim_x, dim_y, dim_time),
      missval     = -9999,
      longname    = var_meta[[vn]]$long_name,
      compression = compression,
      prec        = "double"
    )

  })

  names(data_vars) <- names(var_meta)

  nc <- ncdf4::nc_create(
    out_path,
    c(list(var_crs), data_vars)
  )

  on.exit(ncdf4::nc_close(nc), add = TRUE)

  ncdf4::ncvar_put(nc, var_crs, 0L)

  if (!is.null(dtm)) {

    crs_wkt <- terra::crs(dtm, proj = FALSE)

    ncdf4::ncatt_put(nc, "crs", "crs_wkt", crs_wkt)

    if (is_lonlat) {

      ncdf4::ncatt_put(
        nc,
        "crs",
        "grid_mapping_name",
        "latitude_longitude"
      )

    } else {

      ncdf4::ncatt_put(
        nc,
        "crs",
        "grid_mapping_name",
        "projected_coordinate_system"
      )
    }
  }

  if (is_lonlat) {

    ncdf4::ncatt_put(nc, "lon", "standard_name", "longitude")
    ncdf4::ncatt_put(nc, "lon", "axis", "X")

    ncdf4::ncatt_put(nc, "lat", "standard_name", "latitude")
    ncdf4::ncatt_put(nc, "lat", "axis", "Y")

  } else {

    ncdf4::ncatt_put(nc, "x", "standard_name",
                     "projection_x_coordinate")
    ncdf4::ncatt_put(nc, "x", "axis", "X")

    ncdf4::ncatt_put(nc, "y", "standard_name",
                     "projection_y_coordinate")
    ncdf4::ncatt_put(nc, "y", "axis", "Y")
  }

  for (vn in names(var_meta)) {

    arr <- aperm(data_list[[vn]], c(2L, 1L, 3L))

    arr[is.na(arr)] <- -9999

    ncdf4::ncvar_put(
      nc,
      data_vars[[vn]],
      arr
    )

    ncdf4::ncatt_put(
      nc,
      vn,
      "units",
      var_meta[[vn]]$units
    )

    ncdf4::ncatt_put(
      nc,
      vn,
      "long_name",
      var_meta[[vn]]$long_name
    )

    ncdf4::ncatt_put(
      nc,
      vn,
      "grid_mapping",
      "crs"
    )

    ncdf4::ncatt_put(
      nc,
      vn,
      "coordinates",
      if (is_lonlat) "lon lat" else "x y"
    )
  }

  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.8")

  ncdf4::ncatt_put(
    nc,
    0,
    "data_type",
    data_type
  )

  ncdf4::ncatt_put(
    nc,
    0,
    "history",
    sprintf(
      "Created %s by R %s / ThermalScapeR",
      format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      paste(R.version$major, R.version$minor, sep = ".")
    )
  )

  invisible(NULL)
}

# --------------------------------------------------------------------------- #
#  NetCDF below-ground write helpers
# --------------------------------------------------------------------------- #

.write_nc_blw <- function(data_list, var_meta, tme, out_path, dtm,
                          depth_label, compression) {

  nrow_  <- dim(data_list[["Tz"]])[1]
  ncol_  <- dim(data_list[["Tz"]])[2]
  ntime_ <- dim(data_list[["Tz"]])[3]

  if (!is.null(dtm)) {
    x_vals    <- terra::xFromCol(dtm, seq_len(ncol_))
    y_vals    <- terra::yFromRow(dtm, seq_len(nrow_))
    is_lonlat <- terra::is.lonlat(dtm)
  } else {
    warning("dtm is NULL: coordinate variables will use integer indices.")
    x_vals    <- seq_len(ncol_)
    y_vals    <- seq_len(nrow_)
    is_lonlat <- FALSE
  }

  t_origin <- if (!is.null(tme)) {
    format(as.POSIXct(tme[1], tz = "UTC"), "%Y-%m-%dT%H:%M:%S")
  } else {
    "1970-01-01T00:00:00"
  }

  t_vals <- if (!is.null(tme)) {
    as.numeric(difftime(as.POSIXct(tme, tz = "UTC"),
                        as.POSIXct(tme[1], tz = "UTC"),
                        units = "hours"))
  } else {
    seq_len(ntime_) - 1L
  }

  if (is_lonlat) {
    dim_x <- ncdf4::ncdim_def("lon", "degrees_east", x_vals,
                              longname = "longitude", create_dimvar = TRUE)
    dim_y <- ncdf4::ncdim_def("lat", "degrees_north", y_vals,
                              longname = "latitude", create_dimvar = TRUE)
  } else {
    dim_x <- ncdf4::ncdim_def("x", "m", x_vals,
                              longname = "x coordinate", create_dimvar = TRUE)
    dim_y <- ncdf4::ncdim_def("y", "m", y_vals,
                              longname = "y coordinate", create_dimvar = TRUE)
  }

  dim_time <- ncdf4::ncdim_def("time",
                               sprintf("hours since %s UTC", t_origin),
                               t_vals, unlim = TRUE,
                               longname = "time", calendar = "standard")

  var_crs <- ncdf4::ncvar_def("crs", "", list(), prec = "integer",
                              longname = "CRS definition")

  vn <- sprintf("Tz_%s", depth_label)
  depth_var <- ncdf4::ncvar_def(vn, var_meta$Tz$units,
                                list(dim_x, dim_y, dim_time),
                                missval = -9999,
                                longname = var_meta$Tz$long_name,
                                compression = compression, prec = "double")

  nc <- ncdf4::nc_create(out_path, c(list(var_crs), list(depth_var)))
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  ncdf4::ncvar_put(nc, var_crs, 0L)

  if (!is.null(dtm)) {
    crs_wkt <- terra::crs(dtm, proj = FALSE)
    ncdf4::ncatt_put(nc, "crs", "crs_wkt", crs_wkt)
    if (is_lonlat) {
      ncdf4::ncatt_put(nc, "crs", "grid_mapping_name", "latitude_longitude")
    } else {
      ncdf4::ncatt_put(nc, "crs", "grid_mapping_name",
                       "projected_coordinate_system")
    }
  }

  if (is_lonlat) {
    ncdf4::ncatt_put(nc, "lon", "standard_name", "longitude")
    ncdf4::ncatt_put(nc, "lon", "axis", "X")
    ncdf4::ncatt_put(nc, "lat", "standard_name", "latitude")
    ncdf4::ncatt_put(nc, "lat", "axis", "Y")
  } else {
    ncdf4::ncatt_put(nc, "x", "standard_name", "projection_x_coordinate")
    ncdf4::ncatt_put(nc, "x", "axis", "X")
    ncdf4::ncatt_put(nc, "y", "standard_name", "projection_y_coordinate")
    ncdf4::ncatt_put(nc, "y", "axis", "Y")
  }

  arr <- aperm(data_list[["Tz"]], c(2L, 1L, 3L))
  arr[is.na(arr)] <- -9999
  ncdf4::ncvar_put(nc, depth_var, arr)

  ncdf4::ncatt_put(nc, vn, "units",     var_meta$Tz$units)
  ncdf4::ncatt_put(nc, vn, "long_name", var_meta$Tz$long_name)
  ncdf4::ncatt_put(nc, vn, "grid_mapping", "crs")
  ncdf4::ncatt_put(nc, vn, "coordinates",
                   if (is_lonlat) "lon lat" else "x y")

  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.8")
  ncdf4::ncatt_put(nc, 0, "data_type",   "mout_blw")
  ncdf4::ncatt_put(nc, 0, "history",
                   sprintf("Created %s by R %s / ThermalScapeR",
                           format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                           paste(R.version$major, R.version$minor, sep = ".")))

  invisible(NULL)
}

.append_nc_blw <- function(data_list, var_meta, out_path, depth_label,
                           compression) {
  nc <- ncdf4::nc_open(out_path, write = TRUE)

  x_nm <- if ("lon" %in% names(nc$dim)) "lon" else "x"
  y_nm <- if ("lat" %in% names(nc$dim)) "lat" else "y"

  dim_x    <- nc$dim[[x_nm]]
  dim_y    <- nc$dim[[y_nm]]
  dim_time <- nc$dim[["time"]]

  vn <- sprintf("Tz_%s", depth_label)
  new_var <- ncdf4::ncvar_def(vn, var_meta$Tz$units,
                              list(dim_x, dim_y, dim_time),
                              missval = -9999,
                              longname = var_meta$Tz$long_name,
                              compression = compression, prec = "double")

  nc <- ncdf4::ncvar_add(nc, new_var)

  arr <- aperm(data_list[["Tz"]], c(2L, 1L, 3L))
  arr[is.na(arr)] <- -9999
  ncdf4::ncvar_put(nc, vn, arr)

  is_lonlat <- x_nm == "lon"
  ncdf4::ncatt_put(nc, vn, "units",        var_meta$Tz$units)
  ncdf4::ncatt_put(nc, vn, "long_name",    var_meta$Tz$long_name)
  ncdf4::ncatt_put(nc, vn, "grid_mapping", "crs")
  ncdf4::ncatt_put(nc, vn, "coordinates",
                   if (is_lonlat) "lon lat" else "x y")

  ncdf4::nc_close(nc)
  invisible(NULL)
}

# --------------------------------------------------------------------------- #
#  write_tile -- exported
# --------------------------------------------------------------------------- #

#' Write a microclimf tile output to HDF5 or NetCDF
#'
#' Serializes a single \code{mout} or \code{smod} list returned by
#' \code{microclimf} spatial model functions to an HDF5 or NetCDF-4 file with
#' full variable metadata, spatial coordinates, and CRS information.
#'
#' When \code{depth_label} is non-\code{NULL}, below-ground mode is activated:
#' only the \code{Tz} variable is written, and multiple depths can be
#' accumulated into a single file by calling \code{write_tile} repeatedly with
#' different \code{depth_label} values and the same \code{out_path}. The first
#' call creates the file; subsequent calls append a new depth group (HDF5) or
#' variable (NetCDF).
#'
#' @param data List. Either an \code{mout} object (must contain element
#'   \code{"Tz"}) or an \code{smod} object (must contain element \code{"Tc"}).
#'   All data arrays must have dimensions \code{[nrow, ncol, ntime]}.
#'   \code{mout} lists may include a \code{$tme} POSIXct element which is
#'   used automatically when \code{tme = NULL}.
#' @param out_path Character. Output file path. The extension is ignored; format
#'   is controlled by \code{file_fmt}.
#' @param dtm \code{terra::SpatRaster} at tile resolution. Used to embed spatial
#'   extent, CRS, and cell coordinates. If \code{NULL}, spatial metadata is
#'   omitted with a warning.
#' @param tme POSIXct vector of length \code{ntime}. Required for \code{smod}
#'   objects (which have no \code{$tme} element). For \code{mout} objects,
#'   defaults to \code{data$tme} when \code{NULL}.
#' @param file_fmt Character. Output format: \code{"h5"} (HDF5 via
#'   \code{rhdf5}) or \code{"nc"} (NetCDF-4 via \code{ncdf4}). Default
#'   \code{"h5"}.
#' @param compression Integer 0-9. Gzip compression level applied to each data
#'   variable. 0 = no compression; 9 = maximum compression. Default 4.
#' @param depth_label Character or \code{NULL}. When non-\code{NULL} (e.g.
#'   \code{"BlwGrd_0000"}), activates below-ground mode: only the \code{Tz}
#'   array is written. In HDF5, each depth becomes a group
#'   (\code{/BlwGrd_XXXX/Tz}); in NetCDF, each depth becomes a variable
#'   (\code{Tz_BlwGrd_XXXX}). If \code{out_path} already exists, the new depth
#'   is appended; otherwise, the file is created with spatial and time metadata.
#'   Default \code{NULL} (standard above-ground / smod behaviour).
#'
#' @return Invisibly, a one-row \code{data.frame} with columns \code{file_path}
#'   and \code{data_type}.
#'
#' @details
#' **HDF5 layout** (\code{file_fmt = "h5"}, requires \code{rhdf5}):
#' One dataset per variable at \code{[nrow, ncol, ntime]} plus a \code{time}
#' dataset (ISO 8601 character strings). Spatial and period metadata are stored
#' as HDF5 attributes on the root group \code{"/"}. Per-variable \code{units}
#' and \code{long_name} attributes are written on each dataset.
#'
#' **Below-ground HDF5 layout** (when \code{depth_label} is set):
#' One HDF5 group per depth label, each containing a single \code{Tz} dataset.
#' Root attributes carry spatial metadata, time, and
#' \code{data_type = "mout_blw"}.
#'
#' **NetCDF layout** (\code{file_fmt = "nc"}, requires \code{ncdf4}):
#' CF-1.8 compliant. Dimensions \code{x}, \code{y}, \code{time} with
#' cell-centre coordinate variables. A scalar \code{crs} variable carries
#' the WKT string. All data variables carry \code{grid_mapping = "crs"}.
#' Array dimensions are permuted from R's \code{[nrow, ncol, ntime]} to
#' NetCDF's \code{(x, y, time)} storage order before writing.
#'
#' **Below-ground NetCDF layout** (when \code{depth_label} is set):
#' Each depth is a separate 3D variable named \code{Tz_<depth_label>} sharing
#' the same \code{x}, \code{y}, \code{time} dimensions.
#'
#' Both formats apply gzip compression at the level set by \code{compression}.
#' HDF5 uses chunk dimensions \code{[nrow, ncol, min(ntime, 24)]} (time slabs).
#'
#' @seealso \code{\link{stitch_tiles}} to combine tile files into a single
#'   domain-wide file. \code{\link{create_tiles}} for generating tile extents.
#'
#' @export
write_tile <- function(data, out_path, dtm = NULL, tme = NULL,
                       file_fmt = "h5", compression = 4L,
                       depth_label = NULL) {
  file_fmt    <- match.arg(file_fmt, c("h5", "nc"))
  compression <- as.integer(compression)
  if (compression < 0L || compression > 9L)
    stop("compression must be an integer between 0 and 9")

  pkg <- if (file_fmt == "h5") "rhdf5" else "ncdf4"
  if (!requireNamespace(pkg, quietly = TRUE))
    stop(sprintf("Package '%s' is required. Install with: %s", pkg,
                 if (pkg == "rhdf5") 'BiocManager::install("rhdf5")'
                 else sprintf('install.packages("%s")', pkg)))

  # --- Below-ground mode (depth_label set) ------------------------------------
  if (!is.null(depth_label)) {
    if (!"Tz" %in% names(data))
      stop("Below-ground mode requires data to contain 'Tz'")

    var_meta  <- .mout_blw_meta
    data_list <- list(Tz = round(data[["Tz"]], 3))

    if (is.null(tme) && !is.null(data$tme)) tme <- data$tme

    if (file.exists(out_path)) {
      if (file_fmt == "h5") {
        .append_h5_blw(data_list, var_meta, out_path, depth_label, compression)
      } else {
        .append_nc_blw(data_list, var_meta, out_path, depth_label, compression)
      }
    } else {
      if (is.null(tme))
        warning("tme is NULL. Time metadata will be omitted from newly created below-ground file.")
      if (file_fmt == "h5") {
        .write_h5_blw(data_list, var_meta, tme, out_path, dtm,
                      depth_label, compression)
      } else {
        .write_nc_blw(data_list, var_meta, tme, out_path, dtm,
                      depth_label, compression)
      }
    }

    return(invisible(data.frame(file_path = out_path, data_type = "mout_blw",
                                stringsAsFactors = FALSE)))
  }

  # --- Standard above-ground / smod mode (unchanged) -------------------------
  data_type <- .detect_data_type(data)
  var_meta  <- if (data_type == "mout") .mout_meta else .smod_meta

  if (is.null(tme)) {
    if (data_type == "mout" && !is.null(data$tme)) {
      tme <- data$tme
    } else {
      warning("tme is NULL and could not be extracted from data. Time metadata will be omitted.")
    }
  }

  data_list <- lapply(data[names(var_meta)], round, digits = 3)

  if (file_fmt == "h5") {
    .write_h5(data_list, var_meta, tme, out_path, dtm, data_type, compression)
  } else {
    .write_nc(data_list, var_meta, tme, out_path, dtm, data_type, compression)
  }

  invisible(data.frame(file_path = out_path, data_type = data_type,
                       stringsAsFactors = FALSE))
}

# --------------------------------------------------------------------------- #
#  Stitch helpers
# --------------------------------------------------------------------------- #

.read_h5_tile_attrs <- function(path) {
  attrs <- rhdf5::h5readAttributes(path, "/")
  rhdf5::H5close()
  attrs
}

.read_nc_tile_attrs <- function(path) {
  nc   <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  x_nm <- if ("lon" %in% names(nc$dim)) "lon" else "x"
  y_nm <- if ("lat" %in% names(nc$dim)) "lat" else "y"
  x_v  <- nc$dim[[x_nm]]$vals
  y_v  <- nc$dim[[y_nm]]$vals
  res_x <- abs(diff(x_v))[1]
  res_y <- abs(diff(y_v))[1]
  list(
    xmin  = min(x_v) - res_x / 2,
    xmax  = max(x_v) + res_x / 2,
    ymin  = min(y_v) - res_y / 2,
    ymax  = max(y_v) + res_y / 2,
    res_x = res_x,
    res_y = res_y,
    nrow  = nc$dim[[y_nm]]$len,
    ncol  = nc$dim[[x_nm]]$len,
    ntime = nc$dim$time$len
  )
}

# lapply, optionally parallelized via future.apply when workers > 1.
# Mirrors the workers / future::plan(multisession) / on.exit pattern used in
# R/Microclimf_DataPrep.R (e.g. download_aorc()).
.lapply_workers <- function(x, fn, workers = 1) {
  if (workers > 1) {
    future::plan(future::multisession, workers = workers)
    on.exit(future::plan(future::sequential), add = TRUE)
    future.apply::future_lapply(x, fn, future.seed = TRUE)
  } else {
    lapply(x, fn)
  }
}

.rel_path <- function(target, from_file) {
  t_norm  <- normalizePath(target,    mustWork = FALSE)
  f_dir   <- dirname(normalizePath(from_file, mustWork = FALSE))
  t_parts <- strsplit(t_norm, "[/\\\\]")[[1]]
  f_parts <- strsplit(f_dir,  "[/\\\\]")[[1]]
  t_parts <- t_parts[nchar(t_parts) > 0L]
  f_parts <- f_parts[nchar(f_parts) > 0L]
  n_common <- 0L
  for (i in seq_len(min(length(t_parts), length(f_parts)))) {
    if (tolower(t_parts[i]) == tolower(f_parts[i])) n_common <- i else break
  }
  ups   <- rep("..", length(f_parts) - n_common)
  downs <- t_parts[seq(n_common + 1L, length(t_parts))]
  paste(c(ups, downs), collapse = "/")
}

.stitch_h5_extlinks <- function(tile_files, out_file, var_names) {
  rhdf5::h5createFile(out_file)
  fid_out <- rhdf5::H5Fopen(out_file)
  on.exit({ rhdf5::H5Fclose(fid_out); rhdf5::H5close() }, add = TRUE)
  for (tf in tile_files) {
    rel   <- .rel_path(tf, out_file)
    tname <- tools::file_path_sans_ext(basename(tf))
    rhdf5::H5Lcreate_external(rel, "/", fid_out, tname)
  }
  invisible(NULL)
}

.stitch_h5_vds <- function(tile_files, out_file, var_names, full_nrow, full_ncol,
                            ntime, full_xmin, full_ymax, res_x, res_y,
                            fill_value, python_path) {
  if (!is.null(python_path))
    reticulate::use_python(python_path, required = TRUE)

  if (!reticulate::py_module_available("h5py")) {
    warning("h5py not found; falling back to HDF5 external-links master file.")
    return(.stitch_h5_extlinks(tile_files, out_file, var_names))
  }

  h5py <- reticulate::import("h5py")
  np   <- reticulate::import("numpy")
  bi   <- reticulate::import("builtins")

  fv <- if (is.na(fill_value)) np$nan else fill_value

  fout <- h5py$File(out_file, "w")
  on.exit(fout$close(), add = TRUE)

  out_dir <- dirname(normalizePath(out_file, mustWork = FALSE))

  # rhdf5 reverses dim order: R array [nrow,ncol,ntime] -> HDF5 (ntime,ncol,nrow)
  # h5py sees (ntime,ncol,nrow), so VDS layout must match that convention.
  for (vn in var_names) {
    layout <- h5py$VirtualLayout(
      shape = reticulate::tuple(as.integer(ntime),
                                as.integer(full_ncol),
                                as.integer(full_nrow)),
      dtype = np$float64
    )
    for (tf in tile_files) {
      attrs   <- .read_h5_tile_attrs(tf)
      row_off <- as.integer(round((full_ymax - as.numeric(attrs$ymax)) / res_y))
      col_off <- as.integer(round((as.numeric(attrs$xmin) - full_xmin) / res_x))
      t_nrow  <- as.integer(attrs$nrow)
      t_ncol  <- as.integer(attrs$ncol)
      rel_path <- .rel_path(tf, out_file)
      vsource <- h5py$VirtualSource(
        path_or_dataset = rel_path,
        name            = vn,
        shape           = reticulate::tuple(as.integer(ntime), t_ncol, t_nrow)
      )
      t_sl <- bi$slice(0L, as.integer(ntime))
      c_sl <- bi$slice(col_off, col_off + t_ncol)
      r_sl <- bi$slice(row_off, row_off + t_nrow)
      idx  <- reticulate::tuple(t_sl, c_sl, r_sl)
      layout$`__setitem__`(idx, vsource)
    }
    fout$create_virtual_dataset(vn, layout, fillvalue = fv)
  }
}

.stitch_nc_vrt <- function(tile_files, out_file, var_names,
                           data_type = NULL, var_meta = NULL, workers = 1) {
  if (!requireNamespace("sf",    quietly = TRUE))
    stop('Package \'sf\' is required for VRT stitching. Install with: install.packages("sf")')
  if (!requireNamespace("ncdf4", quietly = TRUE))
    stop('Package \'ncdf4\' is required. Install with: install.packages("ncdf4")')

  out_stem  <- tools::file_path_sans_ext(out_file)
  abs_tiles <- normalizePath(tile_files, mustWork = TRUE)

  if (is.null(data_type))
    data_type <- if ("Tz" %in% var_names) "mout" else "smod"
  if (is.null(var_meta)) {
    var_meta <- if (data_type == "mout") .mout_meta else .smod_meta
  }

  # Read time axis from first tile
  nc0        <- ncdf4::nc_open(tile_files[1])
  time_units <- nc0$dim$time$units
  time_vals  <- nc0$dim$time$vals
  ncdf4::nc_close(nc0)
  origin_str   <- trimws(sub("hours since\\s+", "", time_units))
  origin_str   <- sub("\\s+UTC$", "", origin_str)
  origin_posix <- as.POSIXct(origin_str, tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
  tme_iso      <- format(origin_posix + time_vals * 3600,
                         "%Y-%m-%dT%H:%M:%S", tz = "UTC")

  # Forward-slash versions of abs paths for matching in GDAL-generated VRT XML
  abs_fwd <- gsub("\\\\", "/", abs_tiles)

  .build_one_vrt <- function(vn) {
    vrt_path  <- sprintf("%s_%s.vrt", out_stem, vn)
    gdal_srcs <- sprintf('NETCDF:"%s":%s', abs_tiles, vn)

    # Build VRT -- GDAL requires absolute paths to open sources and read metadata
    sf::gdal_utils(util        = "buildvrt",
                   source      = gdal_srcs,
                   destination = vrt_path,
                   quiet       = TRUE)

    # --- Post-process VRT: relative paths + metadata -------------------------
    vm <- if (!is.null(var_meta[[vn]])) {
      var_meta[[vn]]
    } else if (grepl("^Tz_BlwGrd_", vn) && !is.null(var_meta$Tz)) {
      var_meta$Tz
    } else {
      list(units = "", long_name = vn)
    }
    vrt_text <- paste(readLines(vrt_path, warn = FALSE), collapse = "\n")

    # 1. Swap absolute source paths -> paths relative to the VRT file.
    #    GDAL on Windows may embed paths with backslashes or forward slashes;
    #    try both to ensure a match regardless of platform.
    for (k in seq_along(abs_tiles)) {
      rel_fwd <- gsub("\\\\", "/", .rel_path(abs_tiles[k], vrt_path))
      for (abs_variant in unique(c(abs_tiles[k], abs_fwd[k]))) {
        vrt_text <- gsub(
          paste0('relativeToVRT="0">NETCDF:"', abs_variant, '":'),
          paste0('relativeToVRT="1">NETCDF:"', rel_fwd,     '":'),
          vrt_text, fixed = TRUE
        )
      }
    }

    # 2. Insert dataset-level metadata block after the opening VRTDataset tag
    meta_block <- paste0(
      "  <Metadata>\n",
      sprintf('    <MDI key="data_type">%s</MDI>\n', data_type),
      sprintf('    <MDI key="varname">%s</MDI>\n',   vn),
      sprintf('    <MDI key="units">%s</MDI>\n',     vm$units),
      sprintf('    <MDI key="long_name">%s</MDI>\n', vm$long_name),
      "  </Metadata>"
    )
    vrt_text <- sub("(<VRTDataset[^>]*>\n)",
                    paste0("\\1", meta_block, "\n"),
                    vrt_text, perl = TRUE)

    # 3. Add per-band <Description> (ISO8601 time) -- fully vectorized:
    #    split on "<VRTRasterBand ", insert description after the first newline
    #    in each part (= after the band opening tag), then rejoin
    parts <- strsplit(vrt_text, "<VRTRasterBand ", fixed = TRUE)[[1L]]
    if (length(parts) > 1L) {
      band_parts <- parts[-1L]
      nl_pos     <- regexpr("\n", band_parts, fixed = TRUE)
      band_parts <- paste0(
        substr(band_parts, 1L, nl_pos),
        "    <Description>", tme_iso, "</Description>\n",
        substr(band_parts, nl_pos + 1L, nchar(band_parts))
      )
      vrt_text <- paste0(parts[1L],
                         paste(paste0("<VRTRasterBand ", band_parts), collapse = ""))
    }

    writeLines(strsplit(vrt_text, "\n", fixed = TRUE)[[1L]], vrt_path)
    vrt_path
  }

  vrt_results <- .lapply_workers(var_names, .build_one_vrt, workers)
  vrt_paths   <- stats::setNames(unlist(vrt_results, use.names = FALSE), var_names)

  cat(sprintf("Created %d VRT file(s) with stem: %s\n", length(vrt_paths), out_stem))
  invisible(vrt_paths)
}

# --------------------------------------------------------------------------- #
#  stitch_tiles -- exported
# --------------------------------------------------------------------------- #

#' Stitch microclimf tile output files into a single domain-wide file
#'
#' Discovers all per-tile HDF5 or NetCDF files produced by
#' \code{\link{write_tile}} in \code{tile_dir}, reads their embedded spatial
#' metadata to determine each tile's position within the full domain, and
#' assembles them into either an HDF5 virtual dataset (VDS) or a set of
#' per-variable GDAL VRT files.
#'
#' @param tile_dir Character. Directory containing tile files written by
#'   \code{\link{write_tile}}.
#' @param out_file Character. Output path (including stem) for the assembled
#'   file(s). For \code{file_fmt = "vrt"}, the extension is stripped and each
#'   variable is written as \code{<stem>_<varname>.vrt}.
#' @param data_type Character. \code{"mout"}, \code{"smod"}, or
#'   \code{"mout_blw"}. Controls which files in \code{tile_dir} are selected
#'   and which variables are stitched. \code{"mout"} matches above-ground
#'   microclimate files (containing \code{"MicroclimModel"} but not
#'   \code{"BlwGrd"}). \code{"mout_blw"} matches below-ground files
#'   (containing \code{"BlwGrd"}). \code{"smod"} matches snow model files
#'   (containing \code{"SnowModel"}).
#' @param file_fmt Character. \code{"h5"} builds an HDF5 Virtual Dataset
#'   (requires h5py in the active Python environment, or falls back to external
#'   links); \code{"vrt"} creates one GDAL VRT per variable referencing the
#'   tile NC files via NETCDF subdatasets -- no data is copied and tile files
#'   must remain at their original paths (requires \code{ncdf4} and
#'   \code{terra}). Default \code{"h5"}.
#' @param dtm Optional \code{terra::SpatRaster} covering the full domain. When
#'   supplied, it defines the authoritative full-domain extent, row count, and
#'   column count. When \code{NULL}, these are inferred from the union of all
#'   tile spatial attrs embedded by \code{write_tile}.
#' @param python_path Character. Path to a Python executable that has
#'   \code{h5py} installed. Only used when \code{file_fmt = "h5"}. Defaults to
#'   \code{reticulate}'s currently configured Python.
#' @param fill_value Numeric. Value used to initialise cells not covered by any
#'   tile. Only used when \code{file_fmt = "h5"} (becomes the VDS fill value;
#'   NaN when \code{NA_real_}). Default \code{NA_real_}.
#' @param compression Integer 0-9. Unused; retained for compatibility.
#'   Default \code{4L}.
#' @param workers Integer. Number of parallel workers (via
#'   \code{future.apply}) used for (1) resolving the full-domain extent from
#'   per-tile metadata when \code{dtm = NULL}, and (2) — only when
#'   \code{file_fmt = "vrt"} — building each variable's \code{.vrt} file.
#'   Default \code{1} (sequential). Has no effect on the \code{file_fmt =
#'   "h5"} per-variable stitch step, which always runs sequentially because
#'   all variables share one output HDF5 file handle; a \code{warning} is
#'   raised if \code{workers > 1} is combined with \code{file_fmt = "h5"}.
#'
#' @return Invisibly, a \code{data.frame} with columns \code{tile_file} and
#'   \code{status}.
#'
#' @details
#' **HDF5 VDS path** (\code{file_fmt = "h5"}): Uses \code{reticulate} and
#' \code{h5py} to create a virtual dataset of shape
#' \code{[full_nrow, full_ncol, ntime]} per variable, mapping each tile into
#' its correct spatial position without copying data. Tile files must remain at
#' their original paths for the VDS to remain readable. If h5py is unavailable,
#' falls back to a master file with HDF5 external links.
#'
#' **VRT path** (\code{file_fmt = "vrt"}): For each variable, writes a
#' \code{.vrt} file using \code{sf::gdal_utils("buildvrt", ...)} with GDAL
#' \code{NETCDF:filepath:varname} subdataset references. The VRT mosaics all
#' tile extents spatially; each hourly time step is one band. Tile NC files
#' must stay in place.
#'
#' **Below-ground stitching** (\code{data_type = "mout_blw"}): Depth variable
#' names are enumerated from the first tile file (HDF5 groups matching
#' \code{BlwGrd_*} or NetCDF variables matching \code{Tz_BlwGrd_*}). Each
#' depth is stitched independently.
#'
#' Tile files are discovered by token-based filename matching:
#' \code{"mout"} matches files containing \code{"MicroclimModel"} but not
#' \code{"BlwGrd"}; \code{"mout_blw"} matches files containing
#' \code{"BlwGrd"}; \code{"smod"} matches files containing
#' \code{"SnowModel"}.
#'
#' @seealso \code{\link{write_tile}} for writing individual tile files.
#'   \code{\link{create_tiles}} for generating tile extents.
#'
#' @export
stitch_tiles <- function(tile_dir, out_file, data_type = "mout", file_fmt = "h5",
                         dtm = NULL, python_path = NULL, fill_value = NA_real_,
                         compression = 4L, workers = 1) {
  data_type   <- match.arg(data_type, c("mout", "smod", "mout_blw"))
  file_fmt    <- match.arg(file_fmt, c("h5", "vrt"))
  compression <- as.integer(compression)

  if (file_fmt == "h5" && workers > 1)
    warning("workers is ignored for file_fmt = 'h5'; all variables share one ",
             "HDF5 output file and must be written sequentially. Use ",
             "file_fmt = 'vrt' to parallelize the per-variable stitch.")

  # --- Discover tile files using token-based matching -------------------------
  ext_pat    <- if (file_fmt == "h5") "\\.h5$" else "\\.nc$"
  all_files  <- list.files(tile_dir, full.names = TRUE)
  all_files  <- all_files[grepl(ext_pat, all_files)]

  if (data_type == "mout") {
    tile_files <- all_files[
      grepl("MicroclimModel", basename(all_files), ignore.case = TRUE) &
      !grepl("BlwGrd", basename(all_files), ignore.case = TRUE)
    ]
  } else if (data_type == "mout_blw") {
    tile_files <- all_files[
      grepl("BlwGrd", basename(all_files), ignore.case = TRUE)
    ]
  } else {
    tile_files <- all_files[
      grepl("SnowModel", basename(all_files), ignore.case = TRUE)
    ]
  }

  if (length(tile_files) == 0)
    stop(sprintf("No %s files matching data_type '%s' found in: %s",
                 if (file_fmt == "h5") "HDF5" else "NetCDF", data_type, tile_dir))

  # --- Resolve var_names and var_meta -----------------------------------------
  if (data_type == "mout_blw") {
    if (file_fmt == "h5") {
      if (!requireNamespace("rhdf5", quietly = TRUE))
        stop('Package \'rhdf5\' is required. Install with: BiocManager::install("rhdf5")')
      h5_contents <- rhdf5::h5ls(tile_files[1], recursive = FALSE)
      rhdf5::H5close()
      depth_groups <- h5_contents$name[h5_contents$otype == "H5I_GROUP" &
                                       grepl("^BlwGrd_", h5_contents$name)]
      if (length(depth_groups) == 0)
        stop("No BlwGrd_* groups found in: ", tile_files[1])
      var_names <- sprintf("%s/Tz", sort(depth_groups))
    } else {
      if (!requireNamespace("ncdf4", quietly = TRUE))
        stop('Package \'ncdf4\' is required. Install with: install.packages("ncdf4")')
      nc0 <- ncdf4::nc_open(tile_files[1])
      nc_vars <- names(nc0$var)
      ncdf4::nc_close(nc0)
      var_names <- sort(nc_vars[grepl("^Tz_BlwGrd_", nc_vars)])
      if (length(var_names) == 0)
        stop("No Tz_BlwGrd_* variables found in: ", tile_files[1])
    }
    var_meta <- .mout_blw_meta
  } else {
    var_meta  <- if (data_type == "mout") .mout_meta else .smod_meta
    var_names <- names(var_meta)
  }

  # --- Resolve full domain ---------------------------------------------------
  if (!is.null(dtm)) {
    e         <- terra::ext(dtm)
    full_xmin <- e$xmin
    full_xmax <- e$xmax
    full_ymin <- e$ymin
    full_ymax <- e$ymax
    res_x     <- terra::res(dtm)[1]
    res_y     <- terra::res(dtm)[2]
    full_nrow <- terra::nrow(dtm)
    full_ncol <- terra::ncol(dtm)
  } else {
    if (file_fmt == "h5") {
      if (!requireNamespace("rhdf5", quietly = TRUE))
        stop('Package \'rhdf5\' is required. Install with: BiocManager::install("rhdf5")')
      all_attrs <- .lapply_workers(tile_files, .read_h5_tile_attrs, workers)
    } else {
      if (!requireNamespace("ncdf4", quietly = TRUE))
        stop('Package \'ncdf4\' is required. Install with: install.packages("ncdf4")')
      all_attrs <- .lapply_workers(tile_files, .read_nc_tile_attrs, workers)
    }
    full_xmin <- min(sapply(all_attrs, function(a) as.numeric(a$xmin)))
    full_xmax <- max(sapply(all_attrs, function(a) as.numeric(a$xmax)))
    full_ymin <- min(sapply(all_attrs, function(a) as.numeric(a$ymin)))
    full_ymax <- max(sapply(all_attrs, function(a) as.numeric(a$ymax)))
    res_x     <- as.numeric(all_attrs[[1]]$res_x)
    res_y     <- as.numeric(all_attrs[[1]]$res_y)
    full_nrow <- as.integer(round((full_ymax - full_ymin) / res_y))
    full_ncol <- as.integer(round((full_xmax - full_xmin) / res_x))
  }

  # --- Get ntime from first tile file ----------------------------------------
  if (file_fmt == "h5") {
    if (!requireNamespace("rhdf5", quietly = TRUE))
      stop('Package \'rhdf5\' is required. Install with: BiocManager::install("rhdf5")')
    ntime <- length(rhdf5::h5read(tile_files[1], "time"))
  } else {
    if (!requireNamespace("ncdf4", quietly = TRUE))
      stop('Package \'ncdf4\' is required. Install with: install.packages("ncdf4")')
    nc0   <- ncdf4::nc_open(tile_files[1])
    ntime <- nc0$dim$time$len
    ncdf4::nc_close(nc0)
  }

  # --- Dispatch ---------------------------------------------------------------
  if (file_fmt == "h5") {
    .stitch_h5_vds(tile_files, out_file, var_names,
                   full_nrow, full_ncol, ntime,
                   full_xmin, full_ymax, res_x, res_y,
                   fill_value, python_path)
  } else {
    .stitch_nc_vrt(tile_files, out_file, var_names,
                   data_type = data_type, var_meta = var_meta, workers = workers)
  }

  invisible(data.frame(tile_file = tile_files, status = "stitched",
                       stringsAsFactors = FALSE))
}

#' Stitch Microclimate Tile Files from a \code{run_micro_big_nichemap} Output Tree
#'
#' Wraps \code{\link{stitch_tiles}} with automatic directory discovery driven
#' by the output structure produced by \code{\link{run_micro_big_nichemap}}.
#' The same \code{output_dir}, \code{study_area}, and \code{dates} passed to
#' the modeling run are used to locate tile directories and assemble one
#' stitched file per period x model-type combination.
#'
#' @param output_dir Character. Root output directory passed to
#'   \code{\link{run_micro_big_nichemap}}.
#' @param dates Either a \code{data.frame} with columns \code{Start_Dates} and
#'   \code{End_Dates} (one row per modeling period), or a length-2 \code{Date}
#'   vector defining a single period.  Must match the argument used in the
#'   original modeling run.
#' @param study_area Character or \code{NULL}.  Same value passed to
#'   \code{\link{run_micro_big_nichemap}}. When non-\code{NULL}, tile files are
#'   sought under \code{output_dir/study_area/} and assembled output files are
#'   prefixed with \code{study_area}. Default \code{NULL}.
#' @param stitch_dir Character or \code{NULL}. Directory where assembled files
#'   are written.  Defaults to \code{output_dir/\{study_area\}/Stitched/} (or
#'   \code{output_dir/Stitched/} when \code{study_area} is \code{NULL}).
#'   Per-period subdirectories are created automatically.
#' @param file_fmt Character. \code{"h5"} or \code{"nc"}; must match the
#'   \code{file_fmt} used in the modeling run.  \code{"h5"} produces an HDF5
#'   Virtual Dataset; \code{"nc"} produces one GDAL VRT per variable pointing
#'   to the tile NetCDF files (output files carry a \code{.vrt} extension
#'   derived from the \code{.nc}-named stem). Default \code{"h5"}.
#' @param snow Logical. If \code{TRUE}, also stitch \code{Snow_Models}
#'   directories.  Default \code{FALSE}.
#' @param dtm Optional \code{terra::SpatRaster} covering the full domain.
#'   Passed to \code{\link{stitch_tiles}}. When \code{NULL} the domain extent
#'   is inferred from tile metadata. Default \code{NULL}.
#' @param python_path Character or \code{NULL}. Path to a Python executable
#'   with \code{h5py} installed.  Only used when \code{file_fmt = "h5"}.
#'   Passed to \code{\link{stitch_tiles}}. Default \code{NULL}.
#' @param fill_value Numeric. Fill value for cells not covered by any tile.
#'   Passed to \code{\link{stitch_tiles}}. Default \code{NA_real_}.
#' @param compression Integer 0--9. Retained for compatibility; passed to
#'   \code{\link{stitch_tiles}}. Default \code{4L}.
#' @param workers Integer. Passed unchanged to each \code{\link{stitch_tiles}}
#'   call (see its \code{workers} argument); parallelizes work inside a
#'   single stitch (domain-extent resolution, and the per-variable VRT build
#'   when \code{file_fmt = "nc"}). The period / data-type loop in this
#'   function itself remains sequential -- each \code{\link{stitch_tiles}}
#'   call still runs one at a time. Default \code{1} (sequential).
#'
#' @return Invisibly, a \code{data.frame} with columns \code{period},
#'   \code{data_type}, \code{tile_file}, and \code{status} — one row per tile
#'   file processed by each \code{\link{stitch_tiles}} call.  A zero-row
#'   data.frame is returned when all directories are skipped.  Failed stitches
#'   are recorded with \code{status = "failed: <message>"} rather than
#'   stopping, so partial results are preserved when some periods or model
#'   types are incomplete.
#'
#' @details
#' Period labels are derived from \code{dates} using the same normalisation as
#' \code{\link{run_micro_big_nichemap}}: both \code{Start_Dates} and
#' \code{End_Dates} are snapped to the first of their respective months before
#' formatting as \code{YYYYMM01_to_YYYYMM01}.  This ensures the labels match
#' the on-disk directory names exactly.
#'
#' Tile files must not be moved after stitching.  HDF5 VDS files embed
#' relative paths from the assembled file to each tile; VRT files embed
#' absolute paths.  Either way, relocating tile files breaks the virtual
#' dataset.  When \code{file_fmt = "h5"} and \code{stitch_dir} is on a
#' different Windows drive letter than \code{output_dir}, a warning is emitted
#' because cross-drive relative paths are invalid.
#'
#' Directories that do not exist or are empty are skipped with a message.
#' Directories that exist but contain no files matching the expected token
#' pattern trigger a warning and a failed row in the log without stopping the
#' remaining periods.
#'
#' @seealso \code{\link{run_micro_big_nichemap}} for the modeling run that
#'   produces the tile files this function assembles.
#'   \code{\link{stitch_tiles}} for the underlying stitching logic.
#'
#' @export
stitch_tiles_runmicro <- function(output_dir, dates, study_area = NULL,
                                  stitch_dir  = NULL,
                                  file_fmt    = c("h5", "nc"),
                                  snow        = FALSE,
                                  dtm         = NULL,
                                  python_path = NULL,
                                  fill_value  = NA_real_,
                                  compression = 4L,
                                  workers     = 1) {

  file_fmt   <- match.arg(file_fmt)
  stitch_fmt <- if (file_fmt == "h5") "h5" else "vrt"
  ext        <- if (file_fmt == "h5") ".h5" else ".nc"

  if (!dir.exists(output_dir))
    stop("output_dir does not exist: ", output_dir)
  if (!is.logical(snow) || length(snow) != 1L)
    stop("snow must be a single logical value")

  # --- dates normalisation (mirrors run_micro_big_nichemap lines 1734-1746) ---
  if (is.data.frame(dates)) {
    if (!all(c("Start_Dates", "End_Dates") %in% names(dates)))
      stop("dates data.frame must contain columns 'Start_Dates' and 'End_Dates'")
    date_ranges <- dates
  } else if (inherits(dates, "Date") && length(dates) == 2) {
    date_ranges <- data.frame(Start_Dates      = as.Date(dates[1]),
                               End_Dates        = as.Date(dates[2]),
                               stringsAsFactors = FALSE)
  } else {
    stop("dates must be a data.frame with Start_Dates/End_Dates columns, or a length-2 Date vector")
  }

  period_labels <- vapply(seq_len(nrow(date_ranges)), function(i) {
    s <- as.Date(sprintf("%s-01", format(as.Date(date_ranges$Start_Dates[i]), "%Y-%m")))
    e <- as.Date(sprintf("%s-01", format(as.Date(date_ranges$End_Dates[i]),   "%Y-%m")))
    sprintf("%s_to_%s", format(s, "%Y%m%d"), format(e, "%Y%m%d"))
  }, character(1))

  # --- root paths --------------------------------------------------------------
  sa_root <- if (!is.null(study_area)) file.path(output_dir, study_area) else output_dir
  if (is.null(stitch_dir)) stitch_dir <- file.path(sa_root, "Stitched")

  if (file_fmt == "h5") {
    out_drive  <- toupper(substring(normalizePath(output_dir, mustWork = FALSE), 1, 2))
    stch_drive <- toupper(substring(normalizePath(stitch_dir, mustWork = FALSE), 1, 2))
    if (grepl("^[A-Z]:$", out_drive) && out_drive != stch_drive)
      warning(
        "stitch_dir is on a different drive than output_dir. ",
        "HDF5 VDS files embed relative paths to tile files; cross-drive relative ",
        "paths are invalid on Windows. Use file_fmt = \"nc\" or place stitch_dir ",
        "on the same drive as output_dir."
      )
  }

  mc_root   <- file.path(sa_root, "Microclim_Models")
  smod_root <- file.path(sa_root, "Snow_Models")
  prefix    <- if (!is.null(study_area)) study_area else "Model"

  log_df <- data.frame(period    = character(),
                       data_type = character(),
                       tile_file = character(),
                       status    = character(),
                       stringsAsFactors = FALSE)

  .do_stitch <- function(tile_dir, out_file, data_type, period) {
    if (!dir.exists(tile_dir) || length(list.files(tile_dir)) == 0L) {
      message(sprintf("[SKIP] %s dir missing or empty: %s", data_type, tile_dir))
      return(NULL)
    }
    dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
    message(sprintf("  Stitching %s -> %s", data_type, out_file))
    tryCatch({
      res <- stitch_tiles(tile_dir, out_file, data_type = data_type,
                          file_fmt = stitch_fmt, dtm = dtm,
                          python_path = python_path, fill_value = fill_value,
                          compression = compression, workers = workers)
      cbind(data.frame(period    = period,
                       data_type = data_type,
                       stringsAsFactors = FALSE), res)
    }, error = function(e) {
      warning(sprintf("stitch_tiles failed [%s | %s]: %s",
                      period, data_type, conditionMessage(e)), call. = FALSE)
      data.frame(period    = period,
                 data_type = data_type,
                 tile_file = NA_character_,
                 status    = paste0("failed: ", conditionMessage(e)),
                 stringsAsFactors = FALSE)
    })
  }

  for (i in seq_along(period_labels)) {
    period <- period_labels[i]
    message(sprintf("\n-- Period: %s --", period))

    abv_dir <- file.path(mc_root, period, "AbvGrd")
    out_abv <- file.path(stitch_dir, period,
                         sprintf("%s_AbvGrd_MicroclimModel_%s%s", prefix, period, ext))
    res <- .do_stitch(abv_dir, out_abv, "mout", period)
    if (!is.null(res)) log_df <- rbind(log_df, res)

    blw_dir <- file.path(mc_root, period, "BlwGrd")
    out_blw <- file.path(stitch_dir, period,
                         sprintf("%s_BlwGrd_MicroclimModel_%s%s", prefix, period, ext))
    res <- .do_stitch(blw_dir, out_blw, "mout_blw", period)
    if (!is.null(res)) log_df <- rbind(log_df, res)

    if (snow) {
      smod_dir <- file.path(smod_root, period)
      out_smod <- file.path(stitch_dir, period,
                            sprintf("%s_SnowModel_%s%s", prefix, period, ext))
      res <- .do_stitch(smod_dir, out_smod, "smod", period)
      if (!is.null(res)) log_df <- rbind(log_df, res)
    }
  }

  invisible(log_df)
}

# --------------------------------------------------------------------------- #
#  Internal helpers for run_micro_big_nichemap
# --------------------------------------------------------------------------- #

# Height -> label: positive = above ground, zero/negative = soil depth in mm
.hgt_label <- function(h) {
  if (h > 0) "AbvGrd"
  else sprintf("BlwGrd_%04d", as.integer(round(abs(h) * 1000)))
}

# Suppress cat(), messages, and warnings from noisy model calls
.quiet_run <- function(expr) {
  local({
    invisible(utils::capture.output(
      suppressMessages(suppressWarnings(r <- expr)),
      type = "output"
    ))
    r
  })
}

.interp_nan_layers <- function(clim_list, tme, tile_id, period_label,
                               max_gap_hours = 48L) {
  probe  <- terra::unwrap(clim_list[[1]])
  ncells <- terra::ncell(probe)
  nlyr   <- terra::nlyr(probe)

  nan_count <- terra::global(probe, function(x) sum(is.nan(x)))[[1]]
  nan_idx   <- which(nan_count == ncells)

  if (length(nan_idx) == 0L) return(clim_list)

  gaps    <- split(nan_idx, cumsum(c(1, diff(nan_idx) != 1)))
  max_run <- max(vapply(gaps, length, integer(1)))
  if (max_run > max_gap_hours)
    stop(sprintf("Tile %d | %s: climate NaN gap of %d hours exceeds %d-hour limit",
                 tile_id, period_label, max_run, max_gap_hours))

  gap_dates <- unique(as.Date(tme[nan_idx]))
  warning(sprintf(
    "Tile %d | %s: interpolating %d NaN climate hours (%s)",
    tile_id, period_label, length(nan_idx),
    paste(gap_dates, collapse = ", ")), call. = FALSE)

  clamp_bounds <- list(
    precip = c(0, Inf), swdown = c(0, Inf), lwdown = c(0, Inf),
    difrad = c(0, Inf), windspeed = c(0, Inf), pres = c(0, Inf),
    relhum = c(0, 100), winddir = c(0, 360)
  )

  for (nm in names(clim_list)) {
    r    <- terra::unwrap(clim_list[[nm]])
    vals <- terra::values(r)
    vals[is.nan(vals)] <- NA

    for (cell in seq_len(nrow(vals))) {
      vals[cell, ] <- zoo::na.spline(vals[cell, ], maxgap = max_gap_hours,
                                     na.rm = FALSE)
    }

    bounds <- clamp_bounds[[nm]]
    if (!is.null(bounds)) {
      vals[vals < bounds[1]] <- bounds[1]
      if (is.finite(bounds[2])) vals[vals > bounds[2]] <- bounds[2]
    }

    terra::values(r) <- vals
    clim_list[[nm]] <- terra::wrap(r)
  }

  clim_list
}

# Resolve an RDS input that may be a file path or a directory.
# When a directory, constructs the expected filename from stem + period_label + study_area.
.resolve_rds_path <- function(input, stem, period_label, study_area) {
  if (dir.exists(input)) {
    fname <- if (!is.null(study_area)) {
      sprintf("%s_%s_%s.RDS", study_area, stem, period_label)
    } else {
      sprintf("%s_%s.RDS", stem, period_label)
    }
    path <- file.path(input, fname)
    if (!file.exists(path))
      stop(sprintf("%s file not found for period %s:\n  %s", stem, period_label, path))
    return(path)
  }
  if (!file.exists(input))
    stop(sprintf("%s file not found:\n  %s", stem, input))
  input
}

# Normalizes a date range to first-of-month start/end and builds the
# "YYYYMMDD_to_YYYYMMDD" period label. Shared by the pre-flight grid
# validation pass and the main per-task loop in run_micro_big_nichemap().
.normalize_period <- function(start, end) {
  start_date <- as.Date(sprintf("%s-01", format(as.Date(start), "%Y-%m")))
  end_date   <- as.Date(sprintf("%s-01", format(as.Date(end),   "%Y-%m")))
  period_label <- sprintf("%s_to_%s",
                          format(start_date, "%Y%m%d"),
                          format(end_date,   "%Y%m%d"))
  list(start_date = start_date, end_date = end_date, period_label = period_label)
}

# --------------------------------------------------------------------------- #
#  run_micro_big_nichemap -- exported
# --------------------------------------------------------------------------- #

#' Run Full Microclimate Pipeline for Large Tiled Domains
#'
#' Executes the complete NicheMapR-microclimf microclimate pipeline -- micropoint
#' models, an optional snow model, and microclimate models -- across all tiles
#' and date periods for a large spatial domain.  Terrain features (slope,
#' aspect, topographic wetness index, horizon angles, sky-view factor, and wind
#' shelter) are derived from the full fine DEM once before the tile loop and
#' cropped per tile, avoiding redundant full-domain computation.  SLURM array
#' job distribution is provided via hidden \code{...} arguments.
#'
#' @param tiles List returned by \code{\link{create_tiles}} containing
#'   \code{tiles_proc} (buffered tile extents) and \code{tiles_core} (core tile
#'   extents without buffer).
#' @param clim Character. File path to a packaged climate \code{.RDS}, or a
#'   directory containing one \code{.RDS} per period following the naming
#'   convention \code{{study_area}_Climate_{period_label}.RDS}.
#' @param dates Either a \code{data.frame} with columns \code{Start_Dates} and
#'   \code{End_Dates} (one row per modeling period), or a length-2 \code{Date}
#'   vector defining a single period.  Only year and month matter; the day
#'   component is ignored.  Dates are normalised internally to the first of
#'   each month, so period labels always take the form
#'   \code{YYYYMM01_to_YYYYMM01} and the hourly time sequence spans from the
#'   first hour of the start month through the last hour of the end month.
#' @param dtm_fine \code{SpatRaster}. Fine-resolution DEM (e.g. 30 m) covering
#'   the full domain.  Must share the CRS and extent assumed by
#'   \code{\link{create_tiles}}.
#' @param dtm_coarse \code{SpatRaster}. Coarse-resolution DEM matching the
#'   weather-data grid (e.g. the AORC grid).
#' @param vegp Character. File path to a packaged vegetation parameters
#'   \code{.RDS}, or a directory containing one \code{.RDS} per period following
#'   \code{{study_area}_VegPara_{period_label}.RDS}.
#' @param soilc Character. File path to a packaged soil parameters \code{.RDS},
#'   or a directory containing one \code{.RDS} per period following
#'   \code{{study_area}_SoilPara_{period_label}.RDS}.
#' @param output_dir Character. Root output directory.  Sub-directories are
#'   created automatically (see Details).
#' @param reqhgt Numeric. Above-ground model height in metres.  Must be
#'   positive.  Default \code{2}.
#' @param zref Numeric. Reference height (m) for input air temperature and
#'   humidity.  Default \code{2}.
#' @param windhgt Numeric. Height (m) at which wind speed is measured.  Default
#'   \code{10}.
#' @param matemp Numeric or \code{NA}. Mean annual temperature
#'   (\eqn{^\circ}C) used to initialise soil temperature.  \code{NA} lets
#'   microclimf estimate it internally (default).
#' @param snow Logical. If \code{TRUE}, a snow model is run for each
#'   (tile, period) combination after the above-ground micropoint model, and
#'   its output is passed to the microclimate model.  Default \code{FALSE}.
#' @param snowenv Character. Snow environment type passed to
#'   \code{microclimfPara::runsnowmodel}.  Default \code{"Taiga"}.
#' @param Dynreqhgt Logical. If \code{TRUE}, \code{reqhgt} is adjusted
#'   dynamically for canopy height inside
#'   \code{microclimfPara::runmicro}.  Default \code{FALSE}.
#' @param altcorrect Integer. Altitude correction method passed to
#'   \code{microclimfPara::runsnowmodel} and \code{microclimfPara::runmicro}.
#'   \code{0} = no correction (default).
#' @param parallel Logical. If \code{TRUE}, \code{microclimfPara::runmicro} is
#'   run in parallel.  Default \code{FALSE}.
#' @param ncores Integer. Number of cores to use when \code{parallel = TRUE}.
#'   Default \code{2}.
#' @param study_area Character or \code{NULL}.  When provided, input RDS files
#'   are resolved with this prefix and output files are written under
#'   \code{output_dir/study_area/}.  Default \code{NULL}.
#' @param file_fmt Character.  Output format for microclimate and snow model
#'   results: \code{"h5"} (HDF5 via \code{rhdf5}) or \code{"nc"} (NetCDF-4 via
#'   \code{ncdf4}).  Micropoint model outputs are always written as \code{.RDS}.
#'   Default \code{"h5"}.
#' @param compression Integer 0-9. Gzip compression level applied to HDF5/NetCDF
#'   output files.  Default \code{4L}.
#' @param ... Hidden SLURM array arguments:
#'   \describe{
#'     \item{\code{clust_array_arg}}{Integer.  Value of
#'       \code{$SLURM_ARRAY_TASK_ID} for this node (1-based).}
#'     \item{\code{clust_array_size}}{Integer.  Total number of array tasks.
#'       Required when \code{clust_array_arg} is set.}
#'   }
#'   When supplied, the full set of \code{(tile, period)} tasks is distributed
#'   round-robin across \code{clust_array_size} nodes and only this node's
#'   subset is processed.
#'
#' @return Invisibly, a \code{data.frame} with one row per completed or failed
#'   step and columns:
#'   \describe{
#'     \item{tile_id}{Integer tile index.}
#'     \item{period_label}{Character period string, e.g.
#'       \code{"20200101_to_20201231"}.}
#'     \item{height_label}{Character height label: \code{"AbvGrd"} or
#'       \code{"BlwGrd_XXXX"} where \code{XXXX} is depth in zero-padded
#'       millimetres (e.g. 1.5 cm -> \code{"BlwGrd_0015"}).}
#'     \item{step}{One of \code{"micropoint"}, \code{"snow"}, or
#'       \code{"microclimate"}.}
#'     \item{status}{\code{"success"} or \code{"error: <message>"}.}
#'     \item{file_path}{Path of the output file attempted.}
#'     \item{timestamp}{ISO 8601 wall-clock time of the step.}
#'   }
#'
#' @details
#' **Heights modeled.**  The above-ground height (\code{reqhgt}, labelled
#' \code{"AbvGrd"}) and ten soil depths -- 0, 1.5, 5, 10, 15, 20, 30, 50, 100,
#' and 200 cm -- are always modeled.  Depth labels use the \code{BlwGrd_XXXX}
#' convention where \code{XXXX} is the depth in zero-padded millimetres
#' (ground surface = \code{BlwGrd_0000}).
#'
#' **Output directory layout.**
#' \preformatted{
#' output_dir/
#'   {study_area}/                      # omitted when study_area = NULL
#'     Micropoint_Models/{period}/{hgt}/
#'       Tile_NNN_{prefix}_MicropointModel_{period}.RDS
#'     Snow_Models/{period}/
#'       Tile_NNN_{prefix}_SnowModel_{period}.{ext}
#'     Microclim_Models/{period}/AbvGrd/
#'       Tile_NNN_{prefix}_AbvGrd_MicroclimModel_{period}.{ext}
#'     Microclim_Models/{period}/BlwGrd/
#'       Tile_NNN_{prefix}_BlwGrd_MicroclimModel_{period}.{ext}
#' }
#' Below-ground microclimate outputs are combined into a single file per tile,
#' with each soil depth stored as an HDF5 group (\code{/BlwGrd_XXXX/Tz}) or a
#' NetCDF variable (\code{Tz_BlwGrd_XXXX}).  Only the \code{Tz} variable is
#' retained for below-ground depths.
#'
#' **Processing order per (tile, period) task.**
#' \enumerate{
#'   \item Load and crop climate, vegetation, and soil inputs to the buffered
#'     tile extent (\code{tiles_proc}).
#'   \item Run \code{microclimfPara::runpointmodela} for every height and save
#'     each result as an \code{.RDS}.
#'   \item If \code{snow = TRUE}, run \code{microclimfPara::runsnowmodel} using
#'     the above-ground micropoint output, trim the tile buffer with
#'     \code{\link{trim_tile_buffer}}, and write to disk.
#'   \item Run \code{microclimfPara::runmicro} for every height, trim the
#'     buffer, and write to disk with \code{\link{write_tile}}.
#' }
#' Each step is wrapped in \code{tryCatch}: a failure is logged and a warning
#' issued, but processing continues for the remaining heights and tiles.
#'
#' **SLURM usage.**  In an SBATCH array script, pass
#' \code{clust_array_arg = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))} and
#' \code{clust_array_size = <total tasks>}.  All \code{(tile, period)}
#' combinations are enumerated 1-N and distributed round-robin so each node
#' receives an equal share.  Tile index cycles fastest (all tiles for period 1
#' before period 2, etc.).
#'
#' **Terrain pre-computation.**  Slope, aspect, topographic wetness index (TWI),
#' 24-direction horizon angles (at 15-degree intervals), sky-view factor, and wind
#' shelter arrays are computed from \code{dtm_fine} once before the tile loop
#' using internal microclimf helpers (\code{microclimf:::.topidx},
#' \code{microclimf:::.horizon}, \code{microclimf:::.windsheltera}).  Per-tile
#' subsets are extracted by row/column index, avoiding repeated full-domain
#' computations.
#'
#' **Input grid validation.**  Before terrain features are computed, every
#' period's resolved \code{clim}, \code{vegp}, and \code{soilc} files are
#' checked against \code{dtm_coarse} (\code{clim}) and \code{dtm_fine}
#' (\code{vegp}, \code{soilc}) for matching resolution, CRS, and grid origin/alignment. A mismatch
#' stops the function immediately, before any terrain or model computation.
#'
#' @seealso \code{\link{create_tiles}} to generate \code{tiles} input.
#'   \code{\link{trim_tile_buffer}} for buffer removal.
#'   \code{\link{write_tile}} for output file format details.
#'   \code{\link{stitch_tiles}} to assemble tile outputs into a single file.
#'   \code{\link{package_climate}} and \code{\link{package_veg_soil}} to
#'   produce the \code{clim}, \code{vegp}, and \code{soilc} inputs.
#'
#' @export
run_micro_big_nichemap <- function(tiles,        # tile object from create_tiles
                                   clim,         # file path or directory for packaged climate RDS
                                   dates,        # data.frame(Start_Dates, End_Dates) or length-2 Date vector
                                   dtm_fine,     # fine DEM SpatRaster or file path
                                   dtm_coarse,   # coarse DEM SpatRaster or file path
                                   vegp,         # file path or directory for packaged veg params RDS
                                   soilc,        # file path or directory for packaged soil params RDS
                                   output_dir,   # root output folder
                                   reqhgt       = 2,
                                   zref         = 2,
                                   windhgt      = 10,
                                   matemp       = NA,
                                   snow         = FALSE,
                                   snowenv      = "Taiga",
                                   Dynreqhgt    = FALSE,
                                   altcorrect   = 0,
                                   parallel     = FALSE,
                                   ncores       = 2,
                                   study_area   = NULL,
                                   file_fmt     = c("h5", "nc"),
                                   compression  = 4L,
                                   ...) {

  file_fmt <- match.arg(file_fmt)

  # ... SLURM hidden args -------------------------------------------------------
  dots    <- list(...)
  allowed <- c("clust_array_arg", "clust_array_size")
  unknown <- setdiff(names(dots), allowed)
  if (length(unknown) > 0)
    stop("Unknown argument(s): ", paste(unknown, collapse = ", "))

  clust_array_arg  <- dots$clust_array_arg
  clust_array_size <- dots$clust_array_size

  if (!is.null(clust_array_arg) &&
      (!is.numeric(clust_array_arg) || length(clust_array_arg) != 1))
    stop("clust_array_arg must be a single numeric value or NULL")
  if (!is.null(clust_array_size) &&
      (!is.numeric(clust_array_size) || length(clust_array_size) != 1))
    stop("clust_array_size must be a single numeric value or NULL")
  if (!is.null(clust_array_arg) && is.null(clust_array_size))
    stop("clust_array_size must be provided when clust_array_arg is set")
  if (!is.null(clust_array_arg) &&
      (clust_array_arg < 1 || clust_array_arg > clust_array_size))
    stop("clust_array_arg must be between 1 and clust_array_size")

  if (!is.numeric(reqhgt) || length(reqhgt) != 1 || is.na(reqhgt) || reqhgt <= 0)
    stop("reqhgt must be a single positive numeric value")

  # --- normalise dates ---------------------------------------------------------
  if (is.data.frame(dates)) {
    if (!all(c("Start_Dates", "End_Dates") %in% names(dates)))
      stop("dates data.frame must contain columns 'Start_Dates' and 'End_Dates'")
    date_ranges <- dates
  } else if (inherits(dates, "Date") && length(dates) == 2) {
    date_ranges <- data.frame(
      Start_Dates      = as.Date(dates[1]),
      End_Dates        = as.Date(dates[2]),
      stringsAsFactors = FALSE
    )
  } else {
    stop("dates must be a data.frame with Start_Dates/End_Dates columns, or a length-2 Date vector")
  }

  # --- validate clim / vegp / soilc inputs ------------------------------------
  for (.inp in list(list(clim, "clim"), list(vegp, "vegp"), list(soilc, "soilc"))) {
    if (!file.exists(.inp[[1]]) && !dir.exists(.inp[[1]]))
      stop(sprintf("'%s' does not exist as a file or directory:\n  %s", .inp[[2]], .inp[[1]]))
  }

  # --- validate clim / vegp / soilc grids against dtm_coarse / dtm_fine ------
  cat("--- run_micro_big_nichemap: validating input grids ---\n")
  for (.i in seq_len(nrow(date_ranges))) {
    .p <- .normalize_period(date_ranges$Start_Dates[.i], date_ranges$End_Dates[.i])

    .clim_path  <- .resolve_rds_path(clim,  "Climate",  .p$period_label, study_area)
    .vegp_path  <- .resolve_rds_path(vegp,  "VegPara",  .p$period_label, study_area)
    .soilc_path <- .resolve_rds_path(soilc, "SoilPara", .p$period_label, study_area)

    .clim_r <- .first_spatraster(readr::read_rds(.clim_path))
    .check_grid_match(dtm_coarse, .clim_r, "dtm_coarse",
                       sprintf("clim (%s)", .p$period_label), action = "stop")
    rm(.clim_r)

    .vegp_r <- .first_spatraster(readr::read_rds(.vegp_path))
    .check_grid_match(dtm_fine, .vegp_r, "dtm_fine",
                       sprintf("vegp (%s)", .p$period_label), action = "stop")
    rm(.vegp_r)

    .soilc_r <- .first_spatraster(readr::read_rds(.soilc_path))
    .check_grid_match(dtm_fine, .soilc_r, "dtm_fine",
                       sprintf("soilc (%s)", .p$period_label), action = "stop")
    rm(.soilc_r)

    invisible(gc())
  }
  cat("  All input grids match.\n")

  # --- heights -----------------------------------------------------------------
  sdepth     <- c(0, 2.5, 5, 10, 15, 20, 30, 50, 100, 200) / -100
  allheights <- c(reqhgt, sdepth)

  out_ext <- if (file_fmt == "h5") ".h5" else ".nc"

  # --- terrain features (full DEM, computed once) ------------------------------
  cat(sprintf("\n--- run_micro_big_nichemap: computing terrain features ---\n"))

  slope <- terra::terrain(dtm_fine, v = "slope")
  aspect <- terra::terrain(dtm_fine, v = "aspect")
  twi   <- microclimfPara:::.topidx(dtm_fine)

  hor <- array(NA, dim = c(dim(dtm_fine)[1:2], 24))
  for (i in 1:24) hor[,,i] <- microclimfPara:::.horizon(dtm_fine, (i - 1) * 15)

  msl  <- tan(apply(atan(hor), c(1, 2), mean))
  svfa <- 0.5 * cos(2 * msl) + 0.5
  wsa  <- microclimfPara:::.windsheltera(dtm_fine, 2, 1)
  rm(msl); invisible(gc())

  # --- offload terrain to temp files to free RAM during modeling --------------
  # The full-DEM terrain features are only ever used as small per-tile crops.
  # Write them to a per-process temp dir and drop them from RAM; each loop
  # iteration reloads and crops, then frees the full object before the heavy
  # model runs. Slower, but keeps peak memory off the full-DEM objects.
  terr_dir <- tempfile("micro_terrain_")
  dir.create(terr_dir)
  on.exit(unlink(terr_dir, recursive = TRUE), add = TRUE)

  slope_path  <- file.path(terr_dir, "slope.tif")
  aspect_path <- file.path(terr_dir, "aspect.tif")
  twi_path    <- file.path(terr_dir, "twi.tif")
  hor_path    <- file.path(terr_dir, "hor.rds")
  svfa_path   <- file.path(terr_dir, "svfa.rds")
  wsa_path    <- file.path(terr_dir, "wsa.rds")

  terra::writeRaster(slope,  slope_path,  overwrite = TRUE)
  terra::writeRaster(aspect, aspect_path, overwrite = TRUE)
  terra::writeRaster(twi,    twi_path,    overwrite = TRUE)
  readr::write_rds(hor,  hor_path)
  readr::write_rds(svfa, svfa_path)
  readr::write_rds(wsa,  wsa_path)

  rm(slope, aspect, twi, hor, svfa, wsa); invisible(gc())

  # --- flat task table: all (tile, date) combinations --------------------------
  n_tiles <- nrow(tiles$tiles_proc)
  n_dates <- nrow(date_ranges)

  all_combos <- expand.grid(
    tile_idx = seq_len(n_tiles),
    date_idx = seq_len(n_dates),
    KEEP.OUT.ATTRS = FALSE
  )

  all_combos$node <- if (is.null(clust_array_size)) 1L else
    rep(seq_len(clust_array_size), length.out = nrow(all_combos))

  task_combos <- if (is.null(clust_array_arg)) all_combos else
    all_combos[all_combos$node == clust_array_arg, ]

  cat(sprintf("Tasks this node: %d of %d total (tile x period combinations)\n",
              nrow(task_combos), nrow(all_combos)))

  # --- log data.frame ----------------------------------------------------------
  log_df <- data.frame(
    tile_id      = integer(),
    period_label = character(),
    height_label = character(),
    step         = character(),
    status       = character(),
    file_path    = character(),
    timestamp    = character(),
    stringsAsFactors = FALSE
  )

  .log_row <- function(tile_id, period_label, height_label, step, status, file_path) {
    data.frame(
      tile_id      = tile_id,
      period_label = period_label,
      height_label = height_label,
      step         = step,
      status       = status,
      file_path    = file_path,
      timestamp    = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      stringsAsFactors = FALSE
    )
  }

  # --- flat loop over (tile, date) tasks ---------------------------------------
  current_d      <- NULL
  clim_resolved  <- NULL
  vegp_resolved  <- NULL
  soilc_resolved <- NULL

  for (k in seq_len(nrow(task_combos))) {

    tile_i <- task_combos$tile_idx[k]
    d      <- task_combos$date_idx[k]

    tile_rn <- rownames(tiles$tiles_proc)[tile_i]
    tile_id <- if (!is.null(tile_rn)) as.integer(tile_rn) else tile_i

    tile_proc <- tiles$tiles_proc[tile_i, ]
    tile_core <- tiles$tiles_core[tile_i, ]

    .p           <- .normalize_period(date_ranges$Start_Dates[d], date_ranges$End_Dates[d])
    start_date   <- .p$start_date
    end_date     <- .p$end_date
    period_label <- .p$period_label

    if (is.null(current_d) || d != current_d) {
      clim_resolved  <- .resolve_rds_path(clim,  "Climate",  period_label, study_area)
      vegp_resolved  <- .resolve_rds_path(vegp,  "VegPara",  period_label, study_area)
      soilc_resolved <- .resolve_rds_path(soilc, "SoilPara", period_label, study_area)
      current_d <- d
    }

    cat(sprintf("\n=== Task %d/%d | Tile %d | Period: %s ===\n",
                k, nrow(task_combos), tile_id, period_label))

    dem_coarse_tile <- terra::crop(dtm_coarse, terra::ext(tile_proc))
    dem_fine_tile   <- terra::crop(dtm_fine,   dem_coarse_tile, snap = "out")
    dem_fine_core   <- terra::crop(dtm_fine,   terra::ext(tile_core))

    slope_src   <- terra::rast(slope_path)
    slope_tile  <- terra::crop(slope_src,  dem_fine_tile)
    aspect_src  <- terra::rast(aspect_path)
    aspect_tile <- terra::crop(aspect_src, dem_fine_tile)
    twi_src     <- terra::rast(twi_path)
    twi_tile    <- terra::crop(twi_src,    dem_fine_tile)
    rm(slope_src, aspect_src, twi_src)

    crop_cells  <- terra::cellFromXY(
      dtm_fine,
      terra::xyFromCell(dem_fine_tile,
                        seq_len(terra::nrow(dem_fine_tile) * terra::ncol(dem_fine_tile)))
    )
    crop_rowcol <- terra::rowColFromCell(dtm_fine, crop_cells)
    row_idx <- unique(crop_rowcol[, 1])
    col_idx <- unique(crop_rowcol[, 2])

    hor       <- readr::read_rds(hor_path)
    hor_tile  <- hor [row_idx, col_idx, , drop = FALSE]
    rm(hor)
    svfa      <- readr::read_rds(svfa_path)
    svfa_tile <- svfa[row_idx, col_idx,   drop = FALSE]
    rm(svfa)
    wsa       <- readr::read_rds(wsa_path)
    wsa_tile  <- wsa [row_idx, col_idx, , drop = FALSE]
    rm(wsa)
    invisible(gc())

    tme <- as.POSIXlt(
      seq(as.POSIXct(sprintf("%s 00:00:00", start_date), tz = "UTC"),
          as.POSIXct(sprintf("%s 23:00:00", (end_date + months(1) - lubridate::days(1))),   tz = "UTC"),
          by = "1 hour")
    )

    # --- load and crop inputs --------------------------------------------------
    cat(sprintf("  Loading and cropping inputs...\n"))

    soilc.d   <- readr::read_rds(soilc_resolved)
    soil.crop <- lapply(soilc.d, function(x) terra::wrap(terra::crop(terra::unwrap(x), dem_fine_tile)))
    attr(soil.crop, "class") <- attr(soilc.d, "class")
    rm(soilc.d); invisible(gc())

    vegp.d   <- readr::read_rds(vegp_resolved)
    veg.crop <- lapply(vegp.d, function(x) terra::wrap(terra::crop(terra::unwrap(x), dem_fine_tile)))
    attr(veg.crop, "class") <- attr(vegp.d, "class")
    rm(vegp.d); invisible(gc())

    climdatag <- readr::read_rds(clim_resolved)
    clim.crop <- lapply(climdatag, function(x) terra::wrap(terra::crop(terra::unwrap(x), dem_coarse_tile)))
    rm(climdatag); invisible(gc())

    clim.crop <- .interp_nan_layers(clim.crop, tme, tile_id, period_label)

    # Coverage check: soil/veg rasters can have NA wedges at domain corners from
    # reprojection. Skip rather than run models on all-NA inputs.
    soil_chk <- Find(function(el) inherits(el, "PackedSpatRaster"), soil.crop)
    if (!is.null(soil_chk) &&
        terra::global(terra::unwrap(soil_chk), "notNA")[[1]] == 0L) {
      cat(sprintf("  Tile %d: outside soil/veg data coverage (reprojection edge) -- skipping.\n",
                  tile_id))
      log_df <<- rbind(log_df,
        .log_row(tile_id, period_label, "", "micropoint",
                 "skipped: outside data coverage", ""))
      rm(soil_chk, soil.crop, veg.crop, clim.crop, tme,
         dem_coarse_tile, dem_fine_tile, dem_fine_core,
         slope_tile, aspect_tile, twi_tile, hor_tile, svfa_tile, wsa_tile)
      invisible(gc())
      next
    }
    rm(soil_chk)

    # --- micropoint models for all heights -------------------------------------
    for (h in allheights) {

      hgt_lbl  <- .hgt_label(h)
      prefix   <- if (!is.null(study_area)) sprintf("%s_%s", study_area, hgt_lbl) else hgt_lbl

      mp_parts <- c(output_dir)
      if (!is.null(study_area)) mp_parts <- c(mp_parts, study_area)
      mp_parts <- c(mp_parts, "Micropoint_Models", period_label, hgt_lbl)
      mp_dir   <- do.call(file.path, as.list(mp_parts))
      dir.create(mp_dir, recursive = TRUE, showWarnings = FALSE)

      mp_file <- file.path(mp_dir,
                           sprintf("Tile_%03d_%s_MicropointModel_%s.RDS",
                                   tile_id, prefix, period_label))

      cat(sprintf("  [Tile %d | %s | %s] Running micropoint model...",
                  tile_id, period_label, hgt_lbl))

      tryCatch({
        pointa <- .quiet_run(
          microclimfPara::runpointmodela(
            climarrayr = clim.crop, tme = tme, reqhgt = h,
            dtm = dem_fine_tile, vegp = veg.crop, soilc = soil.crop,
            matemp = matemp, zref = zref, windhgt = windhgt
          )
        )
        readr::write_rds(pointa, mp_file)
        rm(pointa); invisible(gc())
        cat(" done.\n")
        log_df <<- rbind(log_df, .log_row(tile_id, period_label, hgt_lbl,
                                          "micropoint", "success", mp_file))
      }, error = function(e) {
        cat(" FAILED.\n")
        stop(sprintf("Tile %d | %s | %s | micropoint: %s",
                     tile_id, period_label, hgt_lbl, conditionMessage(e)),
             call. = FALSE)
      })
    }

    # --- snow model (once per tile+period, uses AbvGrd micropoint) -------------
    smod <- NULL

    if (snow) {

      abv_lbl    <- .hgt_label(reqhgt)
      abv_prefix <- if (!is.null(study_area)) sprintf("%s_%s", study_area, abv_lbl) else abv_lbl

      abv_parts <- c(output_dir)
      if (!is.null(study_area)) abv_parts <- c(abv_parts, study_area)
      abv_parts <- c(abv_parts, "Micropoint_Models", period_label, abv_lbl)
      abv_dir   <- do.call(file.path, as.list(abv_parts))

      abv_file <- file.path(abv_dir,
                            sprintf("Tile_%03d_%s_MicropointModel_%s.RDS",
                                    tile_id, abv_prefix, period_label))

      smod_parts <- c(output_dir)
      if (!is.null(study_area)) smod_parts <- c(smod_parts, study_area)
      smod_parts <- c(smod_parts, "Snow_Models", period_label)
      smod_dir   <- do.call(file.path, as.list(smod_parts))
      dir.create(smod_dir, recursive = TRUE, showWarnings = FALSE)

      smod_prefix <- if (!is.null(study_area)) study_area else "SnowModel"
      smod_file <- file.path(smod_dir,
                             sprintf("Tile_%03d_%s_SnowModel_%s%s",
                                     tile_id, smod_prefix, period_label, out_ext))

      cat(sprintf("  [Tile %d | %s] Running snow model...", tile_id, period_label))

      tryCatch({
        pointa_abv <- readr::read_rds(abv_file)

        smod <- .quiet_run(
          microclimfPara::runsnowmodel(
            weather    = clim.crop,   micropoint = pointa_abv,
            vegp       = veg.crop,    soilc      = soil.crop,
            dtm        = dem_fine_tile, dtmc     = dem_coarse_tile,
            tme        = tme,         altcorrect = altcorrect,
            snowenv    = snowenv,     method     = "slow",
            zref       = zref,        windhgt    = windhgt,
            parallel   = parallel,    ncores     = ncores
          )
        )

        smod.trim <- trim_tile_buffer(smod, dem_proc = dem_fine_tile, dem_core = dem_fine_core)
        write_tile(smod.trim, out_path = smod_file, tme = tme,
                   compression = compression, file_fmt = file_fmt, dtm = dem_fine_core)
        rm(smod.trim, pointa_abv); invisible(gc())
        cat(" done.\n")
        log_df <<- rbind(log_df, .log_row(tile_id, period_label, "",
                                          "snow", "success", smod_file))
      }, error = function(e) {
        cat(" FAILED.\n")
        stop(sprintf("Tile %d | %s | snow: %s",
                     tile_id, period_label, conditionMessage(e)),
             call. = FALSE)
      })
    }

    # --- above-ground microclimate model ----------------------------------------
    {
      h       <- reqhgt
      hgt_lbl <- .hgt_label(h)
      prefix  <- if (!is.null(study_area)) sprintf("%s_%s", study_area, hgt_lbl) else hgt_lbl

      mp_parts <- c(output_dir)
      if (!is.null(study_area)) mp_parts <- c(mp_parts, study_area)
      mp_parts <- c(mp_parts, "Micropoint_Models", period_label, hgt_lbl)
      mp_dir   <- do.call(file.path, as.list(mp_parts))

      mp_file <- file.path(mp_dir,
                           sprintf("Tile_%03d_%s_MicropointModel_%s.RDS",
                                   tile_id, prefix, period_label))

      mc_parts <- c(output_dir)
      if (!is.null(study_area)) mc_parts <- c(mc_parts, study_area)
      mc_parts <- c(mc_parts, "Microclim_Models", period_label, hgt_lbl)
      mc_dir   <- do.call(file.path, as.list(mc_parts))
      dir.create(mc_dir, recursive = TRUE, showWarnings = FALSE)

      mc_file <- file.path(mc_dir,
                           sprintf("Tile_%03d_%s_MicroclimModel_%s%s",
                                   tile_id, prefix, period_label, out_ext))

      cat(sprintf("  [Tile %d | %s | %s] Running microclimate model...",
                  tile_id, period_label, hgt_lbl))

      tryCatch({
        pointa <- readr::read_rds(mp_file)

        if (snow && !is.null(smod)) {
          mout <- .quiet_run(
            microclimfPara::runmicro(
              micropoint = pointa,       reqhgt     = h,
              vegp       = veg.crop,     soilc      = soil.crop,
              dtm        = dem_fine_tile, dtmc      = dem_coarse_tile,
              altcorrect = altcorrect,   snow       = snow,
              snowmod    = smod,         runchecks  = FALSE,
              slr        = slope_tile,   apr        = aspect_tile,
              hor        = hor_tile,     twi        = twi_tile,
              wsa        = wsa_tile,     svf        = svfa_tile,
              Dynreqhgt  = Dynreqhgt,   parallel   = parallel,
              ncores     = ncores
            )
          )
        } else {
          mout <- .quiet_run(
            microclimfPara::runmicro(
              micropoint = pointa,       reqhgt     = h,
              vegp       = veg.crop,     soilc      = soil.crop,
              dtm        = dem_fine_tile, dtmc      = dem_coarse_tile,
              altcorrect = altcorrect,   snow       = snow,
              runchecks  = FALSE,         slr        = slope_tile,
              apr        = aspect_tile,  hor        = hor_tile,
              twi        = twi_tile,     wsa        = wsa_tile,
              svf        = svfa_tile,    Dynreqhgt  = FALSE,
              parallel   = parallel,     ncores     = ncores
            )
          )
        }

        mout.trim <- trim_tile_buffer(mout, dem_proc = dem_fine_tile, dem_core = dem_fine_core)
        write_tile(mout.trim, out_path = mc_file, tme = tme,
                   compression = compression, file_fmt = file_fmt, dtm = dem_fine_core)
        rm(mout, mout.trim, pointa); invisible(gc())
        cat(" done.\n")
        log_df <<- rbind(log_df, .log_row(tile_id, period_label, hgt_lbl,
                                          "microclimate", "success", mc_file))
      }, error = function(e) {
        cat(" FAILED.\n")
        stop(sprintf("Tile %d | %s | %s | microclimate: %s",
                     tile_id, period_label, hgt_lbl, conditionMessage(e)),
             call. = FALSE)
      })
    }

    # --- below-ground microclimate models (combined file per tile) -------------
    {
      blw_prefix <- if (!is.null(study_area)) sprintf("%s_BlwGrd", study_area) else "BlwGrd"

      blw_parts <- c(output_dir)
      if (!is.null(study_area)) blw_parts <- c(blw_parts, study_area)
      blw_parts <- c(blw_parts, "Microclim_Models", period_label, "BlwGrd")
      blw_dir   <- do.call(file.path, as.list(blw_parts))
      dir.create(blw_dir, recursive = TRUE, showWarnings = FALSE)

      blw_file <- file.path(blw_dir,
                            sprintf("Tile_%03d_%s_MicroclimModel_%s%s",
                                    tile_id, blw_prefix, period_label, out_ext))

      # On HPC restarts, a partial file from a previous run would cause
      # append errors (duplicate groups/variables). Start fresh each time.
      if (file.exists(blw_file)) unlink(blw_file)

      for (h in sdepth) {
        hgt_lbl <- .hgt_label(h)
        prefix  <- if (!is.null(study_area)) sprintf("%s_%s", study_area, hgt_lbl) else hgt_lbl

        mp_parts <- c(output_dir)
        if (!is.null(study_area)) mp_parts <- c(mp_parts, study_area)
        mp_parts <- c(mp_parts, "Micropoint_Models", period_label, hgt_lbl)
        mp_dir   <- do.call(file.path, as.list(mp_parts))

        mp_file <- file.path(mp_dir,
                             sprintf("Tile_%03d_%s_MicropointModel_%s.RDS",
                                     tile_id, prefix, period_label))

        cat(sprintf("  [Tile %d | %s | %s] Running microclimate model...",
                    tile_id, period_label, hgt_lbl))

        tryCatch({
          pointa <- readr::read_rds(mp_file)

          if (snow && !is.null(smod)) {
            mout <- .quiet_run(
              microclimfPara::runmicro(
                micropoint = pointa,       reqhgt     = h,
                vegp       = veg.crop,     soilc      = soil.crop,
                dtm        = dem_fine_tile, dtmc      = dem_coarse_tile,
                altcorrect = altcorrect,   snow       = snow,
                snowmod    = smod,         runchecks  = FALSE,
                slr        = slope_tile,   apr        = aspect_tile,
                hor        = hor_tile,     twi        = twi_tile,
                wsa        = wsa_tile,     svf        = svfa_tile,
                Dynreqhgt  = FALSE,   parallel   = parallel,
                ncores     = ncores
              )
            )
          } else {
            mout <- .quiet_run(
              microclimfPara::runmicro(
                micropoint = pointa,       reqhgt     = h,
                vegp       = veg.crop,     soilc      = soil.crop,
                dtm        = dem_fine_tile, dtmc      = dem_coarse_tile,
                altcorrect = altcorrect,   snow       = snow,
                runchecks  = FALSE,        slr        = slope_tile,
                apr        = aspect_tile,  hor        = hor_tile,
                twi        = twi_tile,     wsa        = wsa_tile,
                svf        = svfa_tile,    Dynreqhgt  = FALSE,
                parallel   = parallel,     ncores     = ncores
              )
            )
          }

          mout.trim <- trim_tile_buffer(mout, dem_proc = dem_fine_tile, dem_core = dem_fine_core)
          write_tile(mout.trim, out_path = blw_file, tme = tme,
                     compression = compression, file_fmt = file_fmt,
                     dtm = dem_fine_core, depth_label = hgt_lbl)
          rm(mout, mout.trim, pointa); invisible(gc())
          cat(" done.\n")
          log_df <<- rbind(log_df, .log_row(tile_id, period_label, hgt_lbl,
                                            "microclimate", "success", blw_file))
        }, error = function(e) {
          cat(" FAILED.\n")
          stop(sprintf("Tile %d | %s | %s | microclimate: %s",
                       tile_id, period_label, hgt_lbl, conditionMessage(e)),
               call. = FALSE)
        })
      }
    }

    if (!is.null(smod)) rm(smod)
    invisible(gc())
  }

  cat(sprintf("\n--- run_micro_big_nichemap complete: %d tile(s), %d period(s) ---\n",
              n_tiles, n_dates))
  invisible(log_df)
}
