#' Define Area of Interest for GEE and Local Analysis
#'
#' Accepts an sf object, SpatVector, or SpatRaster, buffers it, optionally
#' reprojects it, optionally writes the result to disk, and returns a named
#' list containing a GEE ee$Geometry rectangle and the CRS string.
#'
#' @param aoi An sf object, SpatVector, or SpatRaster defining the study area
#' @param buffer_dist Buffer distance in meters. Default is 1000
#' @param crs_epsg Optional EPSG code as a string e.g. "EPSG:26911". If NULL,
#'   the CRS of the input object is used
#' @param write_shp Logical. Whether to write the AOI to disk. Default is FALSE
#' @param out_path File path for the output shapefile. Required if write_shp = TRUE
#'
#' @return A named list with elements:
#'   \item{geometry}{A GEE ee$Geometry$Rectangle object}
#'   \item{crs}{The CRS string used for the geometry}
#' @export
define_aoi <- function(aoi,
                       buffer_dist = 1000,
                       crs_epsg = NULL,
                       write_shp = FALSE,
                       out_path = NULL) {

  # Convert input to sf
  if (inherits(aoi, "SpatVector")) {
    aoi <- sf::st_as_sf(aoi)
  } else if (inherits(aoi, "SpatRaster")) {
    aoi <- sf::st_as_sf(terra::as.polygons(terra::ext(aoi), crs = terra::crs(aoi)))
  } else if (!inherits(aoi, "sf")) {
    stop("aoi must be an sf object, SpatVector, or SpatRaster")
  }

  # Use input CRS if none specified
  if (is.null(crs_epsg)) {
    crs_epsg <- paste0("EPSG:", sf::st_crs(aoi)$epsg)
  }

  range_buf <- sf::st_buffer(aoi, dist = buffer_dist)
  ee.ext <- sf::st_bbox(range_buf)
  bbox_wgs84 <- as.numeric(sf::st_bbox(sf::st_transform(range_buf, 4326)))
  out <- sf::st_as_sfc(ee.ext)
  out <- sf::st_transform(out, crs_epsg)

  if (write_shp) {
    if (is.null(out_path)) stop("out_path must be provided if write_shp = TRUE")
    sf::st_write(out, out_path, delete_dsn = TRUE)
  }

  ee_aoi <- ee$Geometry$Rectangle(
    coords = as.numeric(ee.ext),
    proj = crs_epsg,
    geodesic = FALSE
  )

  return(list(geometry = ee_aoi, crs = crs_epsg, bbox_wgs84 = bbox_wgs84))
}


#' Poll Google Drive and Download Files as They Appear
#'
#' Polls the GEE_Exports folder on Google Drive at a set interval, downloading
#' files as they appear. Keeps a running count of expected vs downloaded files.
#' Issues a warning if the timeout is reached before all files are downloaded.
#'
#' @param out_dir Local directory path to save downloaded files
#' @param n_expected Integer. Number of files expected to download
#' @param poll_interval Polling interval in seconds. Default is 30
#' @param timeout Maximum wait time in seconds. NULL means poll indefinitely.
#'   Default is NULL
#'
#' @return Invisibly returns a named list with elements:
#'   \item{downloaded}{Number of files successfully downloaded}
#'   \item{expected}{Number of files expected}
#'   \item{timed_out}{Logical, whether the timeout was reached}
#' @export
poll_drive <- function(out_dir,
                       n_expected,
                       poll_interval = 30,
                       timeout = NULL) {

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  downloaded <- c()
  n_downloaded <- 0
  start_time <- Sys.time()

  cat("\n--- GEE Export Download Manager ---\n")

  repeat {

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    elapsed_fmt <- sprintf("%dm %ds", floor(elapsed / 60), round(elapsed %% 60))

    # Check Drive folder
    files <- tryCatch(
      googledrive::drive_ls("GEE_Exports/"),
      error = function(e) {
        warning("Could not access GEE_Exports on Google Drive: ", e$message)
        return(NULL)
      }
    )

    if (!is.null(files) && nrow(files) > 0) {

      # Filter to only new files not yet downloaded
      new_files <- files[!files$name %in% downloaded, ]

      if (nrow(new_files) > 0) {
        for (i in seq_len(nrow(new_files))) {

          file_name <- new_files$name[i]
          local_file <- file.path(out_dir, file_name)

          # Warn if overwriting
          if (file.exists(local_file)) {
            warning("File already exists and will be overwritten: ", local_file)
          }

          # Download and delete from Drive
          googledrive::drive_download(
            googledrive::as_id(new_files$id[i]),
            path = local_file,
            overwrite = TRUE
          )
          googledrive::drive_rm(googledrive::as_id(new_files$id[i]))

          downloaded <- c(downloaded, file_name)
          n_downloaded <- length(downloaded)

          cat(sprintf("Downloaded: %s\n", file_name))
        }
      }
    }

    # Progress message
    cat(sprintf(
      "Polling GEE_Exports... (elapsed: %s)\nFiles expected: %d | Downloaded: %d | Remaining: %d\nChecking again in %d seconds.\n\n",
      elapsed_fmt, n_expected, n_downloaded, n_expected - n_downloaded, poll_interval
    ))

    # Check if all files downloaded
    if (n_downloaded >= n_expected) {
      cat("All expected files downloaded successfully.\n")
      return(invisible(list(downloaded = n_downloaded,
                            expected = n_expected,
                            timed_out = FALSE)))
    }

    # Check timeout
    if (!is.null(timeout) && elapsed >= timeout) {
      warning(sprintf(
        "Timeout reached. Downloaded %d of %d expected files. You can re-run poll_drive() manually to resume.",
        n_downloaded, n_expected
      ))
      return(invisible(list(downloaded = n_downloaded,
                            expected = n_expected,
                            timed_out = TRUE)))
    }

    Sys.sleep(poll_interval)
  }
}


#' Download a Digital Elevation Model from Google Earth Engine
#'
#' Exports the Copernicus GLO-30 DEM clipped to the area of interest from GEE
#' to Google Drive, then downloads it locally using poll_drive().
#'
#' @param aoi A named list returned by define_aoi() containing elements
#'   \code{geometry} (ee$Geometry) and \code{crs} (CRS string)
#' @param out_dir Local directory path to save the DEM. Default is "./Data/DEM"
#' @param scale Spatial resolution in meters. Default is 30
#' @param timeout Polling timeout in seconds. Default is 120 (2 minutes)
#' @param poll_interval Polling interval in seconds. Default is 30
#'
#' @return Invisibly returns the result of poll_drive()
#' @export
download_dem <- function(aoi,
                         out_dir = "./Data/DEM",
                         scale = 30,
                         timeout = 120,
                         poll_interval = 30) {

  if (!all(c("geometry", "crs") %in% names(aoi))) {
    stop("aoi must be a named list with 'geometry' and 'crs' elements, as returned by define_aoi()")
  }

  ee_aoi <- aoi$geometry
  crs <- aoi$crs

  # Load and process DEM
  cat("Submitting DEM export task to GEE...\n")

  dem <- ee$ImageCollection("COPERNICUS/DEM/GLO30")$mosaic()
  dem_clipped <- dem$select("DEM")$clip(ee_aoi)

  task <- ee$batch$Export$image$toDrive(
    image = dem_clipped,
    description = "DEM_GLO30_AOI",
    folder = "GEE_Exports",
    fileNamePrefix = "DEM_GLO30",
    region = ee_aoi,
    scale = scale,
    crs = crs,
    maxPixels = 1e13
  )

  task$start()
  cat("GEE export task started. Polling Drive for output...\n")

  result <- poll_drive(
    out_dir = out_dir,
    n_expected = 1,
    poll_interval = poll_interval,
    timeout = timeout
  )

  return(invisible(result))
}

#' Downscale MODIS LAI to 30m Resolution Using NDVI
#'
#' Iterates over each date provided, loading multispectral imagery and
#' coarse MODIS LAI, and downscales LAI to 30m resolution using the
#' lai_fromndvi() function from the microclimdata package.
#'
#' @param dates Vector of class Date specifying year-month combinations to process.
#'   The day component is ignored; only year and month are used.
#'   E.g. as.Date(c("2020-10-01", "2020-11-01"))
#' @param img_dir Directory containing multispectral imagery files (from
#'   download_hls() or download_s2())
#' @param lai_dir Directory containing coarse MODIS LAI files
#' @param out_dir Directory to save downscaled LAI outputs
#' @param study_area Optional character string identifying the study area e.g. "GMU1".
#'   If provided, only files containing this string will be processed
#'
#' @return Invisibly returns a data frame logging the status of each
#'   year/month combination processed
#' @export
downscale_lai <- function(dates,
                          img_dir,
                          lai_dir,
                          out_dir,
                          study_area = NULL) {

  # Extract year and month from dates
  dates <- as.Date(dates)
  years <- lubridate::year(dates)
  months <- lubridate::month(dates)

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # List files in both directories
  img_files <- list.files(img_dir, pattern = "\\.tif$", full.names = TRUE)
  lai_files <- list.files(lai_dir, pattern = "\\.tif$", full.names = TRUE)

  # Optionally filter by study area
  if (!is.null(study_area)) {
    img_files <- img_files[grepl(study_area, img_files)]
    lai_files <- lai_files[grepl(study_area, lai_files)]
  }

  img_df <- .extract_ym(img_files)
  lai_df <- .extract_ym(lai_files)

  # Build log from requested year/month combos
  log <- data.frame(year = years, month = months, stringsAsFactors = FALSE)
  log$ym <- sprintf("%d_%02d", log$year, log$month)
  log$status <- NA_character_

  cat(sprintf("\n--- LAI Downscaling: %d date combinations ---\n\n", nrow(log)))

  for (i in seq_len(nrow(log))) {

    y  <- log$year[i]
    mo <- log$month[i]
    ym <- log$ym[i]

    cat(sprintf("Processing year: %d | month: %02d\n", y, mo))

    # Match files by YYYY_MM
    img_match <- img_df$file[img_df$ym == ym]
    lai_match <- lai_df$file[lai_df$ym == ym]

    # Check files exist
    if (length(img_match) == 0) {
      stop(sprintf("Imagery not found for %s in:\n  %s", ym, img_dir))
    }
    if (length(lai_match) == 0) {
      stop(sprintf("MODIS LAI not found for %s in:\n  %s", ym, lai_dir))
    }
    if (length(img_match) > 1) {
      stop(sprintf("Multiple imagery files found for %s in:\n  %s", ym, img_dir))
    }
    if (length(lai_match) > 1) {
      stop(sprintf("Multiple MODIS LAI files found for %s in:\n  %s", ym, lai_dir))
    }

    # Load imagery
    img <- terra::rast(img_match)

    # Rescale each band to 0-1

    if(any(terra::values(img) > 1 | terra::values(img) < 0)){

          img <- terra::sapp(img, function(x) {
      rng <- range(terra::values(x), na.rm = TRUE)
      (x - rng[1]) / (rng[2] - rng[1])
    })

    }

    # Build rgb and cir stacks
    rgb <- c(img$red, img$green, img$blue)
    cir <- c(img$nir, img$red, img$green)

    # Load coarse LAI
    lai <- terra::rast(lai_match)

    # Downscale
    lai_fine <- microclimdata::lai_fromndvi(rgb, cir, lai)

    out_name <- if (!is.null(study_area)) {
      sprintf("LAI_fromNDVI_%s_%s.tif", study_area, ym)
    } else {
      sprintf("LAI_fromNDVI_%s.tif", ym)
    }

    out_file <- file.path(out_dir, out_name)
    terra::writeRaster(lai_fine, out_file, overwrite = TRUE)

    cat(sprintf("  Saved: %s\n", basename(out_file)))
    log$status[i] <- "success"
  }

  cat(sprintf("\nDone. %d/%d combinations processed successfully.\n",
              sum(log$status == "success", na.rm = TRUE), nrow(log)))

  return(invisible(log))
}

#' Convert NLCD Land Cover to CORINE-Like Classification
#'
#' Reclassifies a National Land Cover Database (NLCD) raster to CORINE Land
#' Cover class codes. Optionally reprojects and/or crops the output to a
#' template raster.
#'
#' @param lc A SpatRaster containing NLCD land cover codes
#' @param new_crs Optional CRS string to reproject the output e.g. "EPSG:26911".
#'   If NULL no reprojection is applied
#' @param crop_template Optional SpatRaster to use as a crop and reproject
#'   template. If provided, output will be cropped and reprojected to match
#'   this raster's CRS and extent
#'
#' @return A SpatRaster with CORINE-like land cover codes and labels
#' @export
NLCD_2_CORINE <- function(lc, new_crs = NULL, crop_template = NULL) {

  # CORINE land cover code lookup table
  corine_lcm <- data.frame(
    value = c(
      111, 112, 121, 122, 123, 124, 131, 132, 133, 141, 142,
      211, 212, 213, 221, 222, 223, 231, 241, 242, 243, 244,
      311, 312, 313, 321, 322, 323, 324, 331, 332, 333, 334, 335,
      411, 412, 421, 422, 423,
      511, 512, 521, 522, 523
    ),
    label = c(
      "Continuous urban fabric", "Discontinuous urban fabric",
      "Industrial or commercial units", "Road and rail networks",
      "Port areas", "Airports", "Mineral extraction sites", "Dump sites",
      "Construction sites", "Green urban areas", "Sport and leisure facilities",
      "Non-irrigated arable land", "Permanently irrigated land", "Rice fields",
      "Vineyards", "Fruit trees and berry plantations", "Olive groves",
      "Pastures", "Annual crops with permanent crops",
      "Complex cultivation patterns", "Agriculture with natural vegetation",
      "Agro-forestry areas", "Broad-leaved forest", "Coniferous forest",
      "Mixed forest", "Natural grasslands", "Moors and heathland",
      "Sclerophyllous vegetation", "Transitional woodland-shrub",
      "Beaches, dunes, sands", "Bare rocks", "Sparsely vegetated areas",
      "Burnt areas", "Glaciers and perpetual snow", "Inland marshes",
      "Peat bogs", "Salt marshes", "Salines", "Intertidal flats",
      "Water courses", "Water bodies", "Coastal lagoons", "Estuaries",
      "Sea and ocean"
    ),
    stringsAsFactors = FALSE
  )

  # NLCD to CORINE crosswalk
  nlcd_to_corine <- data.frame(
    from = c(11, 12, 21, 22, 23, 24, 31, 41, 42, 43, 52, 71, 81, 82, 90, 95),
    to   = c(512, 335, 141, 112, 112, 111, 333, 311, 312, 313, 324, 321, 231, 211, 411, 411)
  )

  # Reclassify
  corine_rast <- terra::classify(lc, nlcd_to_corine, include.lowest = TRUE)
  levels(corine_rast) <- corine_lcm

  # Handle reprojection and cropping
  if (is.null(new_crs) && is.null(crop_template)) {
    message("No new CRS or crop template provided, returning CORINE-like raster in original CRS and extent.")
    return(corine_rast)

  } else if (is.null(new_crs) && !is.null(crop_template)) {
    message("Crop template provided, reprojecting and cropping to template CRS and extent.")
    return(terra::crop(terra::project(corine_rast, terra::crs(crop_template), method = "near"), crop_template))

  } else if (!is.null(new_crs) && is.null(crop_template)) {
    message("New CRS provided, reprojecting to new CRS without cropping.")
    return(terra::project(corine_rast, new_crs, method = "near"))

  } else {
    message("Both new CRS and crop template provided, cropping to template then reprojecting to new CRS.")
    cropped <- terra::crop(terra::project(corine_rast, terra::crs(crop_template), method = "near"), crop_template)
    return(terra::project(cropped, new_crs, method = "near"))
  }
}

#' Convert LANDFIRE Vegetation Height Raster to Numeric
#'
#' Extracts numeric height values from LANDFIRE vegetation height raster class
#' names, converts the raster to numeric, and optionally crops and reprojects
#' to a template raster.
#'
#' @param rast A categorical SpatRaster of LANDFIRE vegetation height
#' @param crop_template Optional SpatRaster to use as a crop and reproject
#'   template. If provided, output will be cropped and reprojected to match
#'   this raster's CRS and extent
#' @param no_match_value Value to assign when no numeric value can be extracted
#'   from a class name. Default is 0
#'
#' @return A numeric SpatRaster of vegetation heights
#' @export
LandfireVegHght_AsNumeric <- function(rast,
                                           crop_template = NULL,
                                           no_match_value = 0) {

  lv <- levels(rast)

  m <- regexpr("[0-9]+\\.?[0-9]*", lv[[1]]$CLASSNAMES)
  numbers <- as.numeric(regmatches(lv[[1]]$CLASSNAMES, m))
  numbers <- c(m[m == -1], numbers)
  numbers[numbers == -1] <- no_match_value

  lv <- data.frame(lv)
  lv$height <- numbers

  rast <- terra::classify(rast, rcl = as.matrix(lv[, c("VALUE", "height")]))

  if (!is.null(crop_template)) {
    rast <- terra::crop(
      terra::project(rast, terra::crs(crop_template), method = "bilinear"),
      crop_template
    )
  }

  return(rast)
}

#' Download MODIS Albedo Data from Google Earth Engine
#'
#' Iterates over each date provided, exporting monthly median MODIS shortwave
#' black-sky albedo images from GEE to Google Drive, then optionally downloads
#' them locally using poll_drive().
#'
#' @param aoi A named list returned by define_aoi() containing elements
#'   \code{geometry} (ee$Geometry) and \code{crs} (CRS string)
#' @param dates Vector of class Date specifying year-month combinations to process.
#'   The day component is ignored; only year and month are used.
#'   E.g. as.Date(c("2020-10-01", "2020-11-01"))
#' @param out_dir Local directory path to save downloaded albedo files
#' @param study_area Optional character string identifying the study area e.g. "GMU1".
#'   If provided, used as a prefix in output file names
#' @param scale Spatial resolution in meters. Default is 500
#' @param poll Logical. Whether to poll Google Drive and download files after
#'   all tasks are submitted. Default is TRUE
#' @param poll_interval Polling interval in seconds. Default is 30
#' @param timeout Polling timeout in seconds. Default is 300 (5 minutes)
#'
#' @return Invisibly returns a named list with elements:
#'   \item{submitted}{Number of GEE tasks successfully submitted}
#'   \item{skipped}{A data frame of skipped year/month combos}
#'   \item{poll_result}{Result of poll_drive() if poll = TRUE, otherwise NULL}
#' @export
download_albedo <- function(aoi,
                           dates,
                           out_dir,
                           study_area = NULL,
                           scale = 500,
                           poll = TRUE,
                           poll_interval = 30,
                           timeout = 300) {

  # Extract year and month from dates
  dates <- as.Date(dates)
  years <- lubridate::year(dates)
  months <- lubridate::month(dates)

  if (!all(c("geometry", "crs") %in% names(aoi))) {
    stop("aoi must be a named list with 'geometry' and 'crs' elements, as returned by define_aoi()")
  }

  ee_aoi <- aoi$geometry
  crs    <- aoi$crs

  n_submitted <- 0
  skipped     <- data.frame(year = integer(), month = integer())

  cat(sprintf("\n--- MODIS Albedo Export: %d date combinations ---\n\n", length(dates)))

  for (i in seq_along(dates)) {
    y <- years[i]
    mo <- months[i]

      start_date <- sprintf("%d-%02d-01", y, mo)
      end_date   <- as.character(lubridate::ceiling_date(as.Date(start_date), unit = "month"))

      modis_ic <- ee$ImageCollection("MODIS/061/MCD43A3")$
        filterDate(start_date, end_date)$
        filterBounds(ee_aoi)$
        select("Albedo_BSA_shortwave")

      # Scale to 0-1
      scaled_ic <- modis_ic$map(function(img) {
        img$multiply(0.001)
      })

      n <- scaled_ic$size()$getInfo()
      cat(sprintf("Year %d, Month %02d: %d images found\n", y, mo, n))

      # Skip if no images found
      if (n == 0) {
        warning(sprintf("No images found for year %d month %02d - skipping.", y, mo))
        skipped <- rbind(skipped, data.frame(year = y, month = mo))
        next
      }

      # Monthly median composite
      clim_img <- scaled_ic$median()$clip(ee_aoi)

      # Build file name
      fname <- if (!is.null(study_area)) {
        sprintf("MODIS_Albedo_%s_%d_%02d", study_area, y, mo)
      } else {
        sprintf("MODIS_Albedo_%d_%02d", y, mo)
      }

      task <- ee$batch$Export$image$toDrive(
        image          = clim_img,
        description    = fname,
        folder         = "GEE_Exports",
        fileNamePrefix = fname,
        region         = ee_aoi,
        scale          = scale,
        crs            = crs,
        maxPixels      = 1e13
      )

      task$start()
      n_submitted <- n_submitted + 1
      cat(sprintf("  Task submitted: %s\n", fname))
    
  }

  cat(sprintf("\n%d task(s) submitted. %d combo(s) skipped.\n", n_submitted, nrow(skipped)))

  # Poll and download
  poll_result <- NULL
  if (poll && n_submitted > 0) {
    cat(sprintf(
      "\nPolling Google Drive for %d file(s). This may take a while depending on the number of images.\n",
      n_submitted
    ))
    poll_result <- poll_drive(
      out_dir       = out_dir,
      n_expected    = n_submitted,
      poll_interval = poll_interval,
      timeout       = timeout
    )
  } else if (poll && n_submitted == 0) {
    cat("No tasks were submitted, skipping Drive polling.\n")
  }

  return(invisible(list(
    submitted   = n_submitted,
    skipped     = skipped,
    poll_result = poll_result
  )))
}

# Internal helpers --------------------------------------------------------

# Output bands carried through compositing and export
.hls_out_bands <- c("red", "green", "blue", "nir")

# Per-scene cloud/shadow mask. Returns a closure (over the threshold list `mp`)
# that masks one HLS image, keeping only red/green/blue/nir. Cloud detection
# combines Fmask QA bits with a cirrus-band test (high/thin cloud is bright at
# 1.38 um), a visible+SWIR brightness gate, and a physical reflectance ceiling.
#
# Snow vs. cloud cannot be separated spectrally: any bright surface (snow OR
# cloud) shows low SWIR and an elevated cirrus band, so the same signature means
# cloud in summer but snow in winter. The discriminator is therefore seasonal.
# When `mp$protect_snow` is TRUE (snow-season months) a snow override (high NDSI,
# low SWIR, bright green) rescues genuine snow from every cloud rule, so snowy
# scenes are never removed. When FALSE (snow-free months) the cloud rules apply
# in full and residual cloud is removed, to be backfilled from other years.
.make_hls_masker <- function(mp) {
  function(img) {
    img$select(.hls_out_bands)$toFloat()$updateMask(.hls_spectral_keep(img, mp))
  }
}

# Spectral keep-mask (1 = keep, 0 = drop) shared by the spectral and Cloud Score+
# maskers. Combines Fmask QA bits (cloud / adjacent / shadow), a 1.38 um cirrus
# test, a visible+SWIR brightness gate, and a physical reflectance ceiling. When
# mp$protect_snow is TRUE a snow override (high NDSI, low SWIR, bright green)
# rescues genuine snow from every cloud rule (including the >1 ceiling, since snow
# on lit slopes can legitimately exceed 1 at low winter sun angles).
.hls_spectral_keep <- function(img, mp) {
  fmask       <- img$select("Fmask")
  fmask_cloud <- fmask$bitwiseAnd(2L)$neq(0)   # bit 1: cloud
  adjacent    <- fmask$bitwiseAnd(4L)$neq(0)   # bit 2: adjacent to cloud/shadow
  shadow      <- fmask$bitwiseAnd(8L)$neq(0)   # bit 3: cloud shadow

  swir1  <- img$select("swir1")
  green  <- img$select("green")
  cirrus <- img$select("cirrus")

  cirrus_cloud <- cirrus$gt(mp$cirrus_thresh)
  swir_cloud   <- green$gt(mp$vis_thresh)$And(swir1$gt(mp$swir_cloud_thresh))
  over_bright  <- img$select(.hls_out_bands)$reduce(ee$Reducer$max())$gt(mp$refl_max)

  bad <- fmask_cloud$Or(adjacent)$Or(shadow)$
    Or(cirrus_cloud)$Or(swir_cloud)$Or(over_bright)

  if (isTRUE(mp$protect_snow)) {
    ndsi <- img$normalizedDifference(c("green", "swir1"))
    snow <- ndsi$gte(mp$ndsi_thresh)$
      And(swir1$lt(mp$swir_snow_max))$
      And(green$gt(mp$green_thresh))
    bad <- bad$And(snow$Not())
  }

  bad$Not()
}

# Cloud Score+ masker for S30 images carrying a linked `qa_band` (cs_cdf). Per
# pixel: where the CS+ score is present, keep pixels with cs >= clear_threshold;
# where it is masked (an S30 scene with no CS+ partner - e.g. before 2015-06-27 -
# or a per-pixel sensor gap) fall back to the spectral keep-mask. Cloud Score+
# separates snow from cloud, so matched snow is preserved without the spectral
# snow override.
.make_hls_masker_cs <- function(mp, qa_band, clear_threshold) {
  function(img) {
    cs        <- img$select(qa_band)
    avail     <- cs$mask()$gt(0)                       # 1 where CS+ score present
    cs_keep   <- cs$gte(clear_threshold)$unmask(0)     # CS+ decision, 0 where absent
    spec_keep <- .hls_spectral_keep(img, mp)
    keep <- avail$multiply(cs_keep)$add(avail$Not()$multiply(spec_keep))$gt(0)

    # Snow rescue. Cloud Score+ can score bright fresh / early-season snow as
    # occluded (verified: a Teton peak pixel at NDSI 0.75, SWIR 0.15 - unmistakable
    # snow - scored cs_cdf 0.12 and was dropped by the 0.60 gate, leaving a hole that
    # was then backfilled snowless from other years). Where a pixel is unambiguous
    # snow (high NDSI, low SWIR, bright green) keep it regardless of the CS+ score.
    # Cloud is excluded by the low-SWIR gate - cloud is bright in SWIR, snow is dark.
    # Unlike the spectral snow override this is NOT seasonally gated: Cloud Score+
    # already removes cloud, so the rescue is safe year-round and recovers snow in
    # shoulder months that fall outside snow_months.
    swir1 <- img$select("swir1")
    ndsi  <- img$normalizedDifference(c("green", "swir1"))
    snow  <- ndsi$gte(mp$ndsi_thresh)$
      And(swir1$lt(mp$swir_snow_max))$
      And(img$select("green")$gt(mp$green_thresh))
    keep <- keep$Or(snow)

    img$select(.hls_out_bands)$toFloat()$updateMask(keep)
  }
}

# MGRS tiles overlapping the AOI, from Sentinel-2 L1C (correct footprints). HLS
# tiles near the antimeridian carry corrupt globe-wrapping footprints that
# spuriously match filterBounds(AOI): for a Teton AOI, filterBounds returned 84
# HLSS30 scenes of which only 3 were the real T12TVP/T12TWP tiles - the other 81 were
# UTM-zone-01/60 junk (verified: 0 valid pixels over the AOI) that inflated every
# collection ~28x and dominated export time. A fixed mid-archive year is enough
# since MGRS tile coverage is a fixed geographic grid.
.hls_aoi_tiles <- function(ee_aoi) {
  ee$ImageCollection("COPERNICUS/S2_HARMONIZED")$
    filterBounds(ee_aoi)$
    filterDate("2021-01-01", "2021-12-31")$
    aggregate_array("MGRS_TILE")$
    distinct()
}

# Filter an HLS collection to the AOI tiles, removing antimeridian-footprint junk.
# The tile is parsed from system:index ("T{TILE}_{DATETIME}", leading "T" dropped)
# rather than the MGRS_TILE_ID property because HLSL30 leaves that property null -
# filtering L30 on it would silently drop every Landsat scene (verified). The
# system:index prefix is populated for both HLSS30 and HLSL30.
.hls_filter_tiles <- function(ic, tiles) {
  ic$
    map(function(img) {
      img$set("aoi_tile", ee$String(img$get("system:index"))$slice(1L)$split("_")$get(0))
    })$
    filter(ee$Filter$inList("aoi_tile", tiles))
}

# Per-sensor selectors: filter + select/rename to red/green/blue/nir/swir1/cirrus/Fmask.
.s30_select <- function(ee_aoi, ee_filter, s30_bands, band_rename, tiles) {
  .hls_filter_tiles(
    ee$ImageCollection("NASA/HLS/HLSS30/v002")$filter(ee_filter)$filterBounds(ee_aoi),
    tiles
  )$select(s30_bands, band_rename)
}

.l30_select <- function(ee_aoi, ee_filter, l30_bands, band_rename, tiles) {
  .hls_filter_tiles(
    ee$ImageCollection("NASA/HLS/HLSL30/v002")$filter(ee_filter)$filterBounds(ee_aoi),
    tiles
  )$select(l30_bands, band_rename)
}

# S30 with the Cloud Score+ `qa_band` linked by an exact tile+sensing-datetime
# key, then selected+renamed (keeping qa_band). HLSS30 system:index is
# "{TILE}_{SENSINGDT}"; CS+ system:index is "{SENSINGDT}_{PROCDT}_{TILE}", so
# reconstructing the former from the latter gives an exact 1:1 match (no time
# tolerance). Unmatched S30 scenes still receive a fully-masked qa_band (verified),
# which the masker resolves via spectral fallback.
.s30_link_cs <- function(ee_aoi, ee_filter, s30_bands, band_rename, qa_band, tiles) {
  s30 <- .hls_filter_tiles(
    ee$ImageCollection("NASA/HLS/HLSS30/v002")$filter(ee_filter)$filterBounds(ee_aoi),
    tiles
  )$map(function(img) img$set("match_key", img$get("system:index")))

  cs <- ee$ImageCollection("GOOGLE/CLOUD_SCORE_PLUS/V1/S2_HARMONIZED")$
    filter(ee_filter)$filterBounds(ee_aoi)$
    map(function(img) {
      parts <- ee$String(img$get("system:index"))$split("_")
      img$set("match_key", ee$String(parts$get(2))$cat("_")$cat(ee$String(parts$get(0))))
    })

  s30$linkCollection(cs, ee$List(list(qa_band)), ee$List(list()), "match_key")$
    select(c(s30_bands, qa_band), c(band_rename, qa_band))
}

# Build the masked, merged S30+L30 collection for one EE filter. The L30 half
# always uses the spectral masker (Cloud Score+ is Sentinel-2 only). With
# use_cloud_score the S30 half is Cloud Score+ masked (spectral fallback where the
# score is absent); otherwise it too uses the spectral masker. Masking is per
# image, so masking each half then merging equals merging then masking.
.hls_build_masked <- function(ee_aoi, ee_filter, s30_bands, l30_bands, band_rename,
                              mp, use_cloud_score, qa_band, clear_threshold, tiles) {
  l30 <- .l30_select(ee_aoi, ee_filter, l30_bands, band_rename, tiles)$map(.make_hls_masker(mp))

  if (isTRUE(use_cloud_score)) {
    s30 <- .s30_link_cs(ee_aoi, ee_filter, s30_bands, band_rename, qa_band, tiles)$
      map(.make_hls_masker_cs(mp, qa_band, clear_threshold))
  } else {
    s30 <- .s30_select(ee_aoi, ee_filter, s30_bands, band_rename, tiles)$map(.make_hls_masker(mp))
  }

  s30$merge(l30)
}

# Medoid composite: for each pixel select the actual observation whose spectrum
# is closest (least squared distance) to the per-pixel median. Preserves real
# spectra (no band-mixing, no cloud-edge bright pixels). `ic` must already be
# masked. Pixels with fewer than `min_obs` clear observations are left masked
# (treated as gaps for downstream filling). Returns NULL for an empty collection.
.hls_medoid <- function(ic, min_obs = 1L) {
  if (ic$size()$getInfo() == 0) return(NULL)

  med <- ic$select(.hls_out_bands)$median()
  scored <- ic$map(function(img) {
    dist <- img$select(.hls_out_bands)$subtract(med)$pow(2)$
      reduce(ee$Reducer$sum())$multiply(-1)$rename("medoid_score")
    img$addBands(dist)
  })
  mosaic <- scored$qualityMosaic("medoid_score")$select(.hls_out_bands)

  if (min_obs > 1L) {
    cnt <- ic$select("red")$count()
    mosaic <- mosaic$updateMask(cnt$gte(min_obs))
  }
  mosaic
}

.focalFillMasked <- function(img, radius = 10) {
  kernel <- ee$Kernel$square(radius = radius)
  focal  <- img$focal_median(kernel = kernel, iterations = 1)
  img$unmask(focal)
}

.float32_cast <- function(img) {
  img$cast(ee$Dictionary(list(
    red   = "float",
    green = "float",
    blue  = "float",
    nir   = "float"
  )))
}

# First multi-year gap-fill level: medoid of clear observations in the same month
# across the `near` year window (target year +/- near_span). Returns NULL if no
# imagery exists in that window.
.hls_fill_near <- function(ee_aoi, target_year, month, near_span,
                           s30_bands, l30_bands, band_rename,
                           mp, use_cloud_score, qa_band, clear_threshold, tiles) {
  near_filter <- ee$Filter$And(
    ee$Filter$calendarRange(target_year - near_span, target_year + near_span, "year"),
    ee$Filter$calendarRange(month, month, "month")
  )
  .hls_medoid(
    .hls_build_masked(ee_aoi, near_filter, s30_bands, l30_bands, band_rename,
                      mp, use_cloud_score, qa_band, clear_threshold, tiles),
    min_obs = 1L
  )
}

# Second (last-resort) gap-fill level: medoid of clear observations in the same
# month across ALL available years (climatology). This is the heaviest graph -
# the full archive can be thousands of scenes - so the caller only builds it when
# gaps remain after the near fill. Returns NULL if no imagery exists.
.hls_fill_clim <- function(ee_aoi, month, s30_bands, l30_bands, band_rename,
                           mp, use_cloud_score, qa_band, clear_threshold, tiles) {
  month_filter <- ee$Filter$calendarRange(month, month, "month")
  .hls_medoid(
    .hls_build_masked(ee_aoi, month_filter, s30_bands, l30_bands, band_rename,
                      mp, use_cloud_score, qa_band, clear_threshold, tiles),
    min_obs = 1L
  )
}

# TRUE if `img` has any masked pixel inside `ee_aoi` (i.e. unfilled gaps remain).
# `tile_scale` (1-16) splits the reduceRegion into smaller tiles so the check
# survives heavy graphs (e.g. a multi-year medoid) without exceeding the
# interactive getInfo memory limit.
.hls_has_gaps <- function(img, ee_aoi, scale, tile_scale = 4) {
  res <- img$select("red")$mask()$Not()$reduceRegion(
    reducer   = ee$Reducer$anyNonZero(),
    geometry  = ee_aoi,
    scale     = scale,
    maxPixels = 1e13,
    tileScale = tile_scale
  )$getInfo()
  isTRUE(as.logical(unlist(res)[1]))
}

# Sentinel-2 + Cloud Score+ helpers ---------------------------------------

# Sentinel-2 SR source bands and their output names (shared with the HLS path).
.s2_src_bands <- c("B4", "B3", "B2", "B8")  # red, green, blue, nir
.s2_out_bands <- .hls_out_bands             # c("red", "green", "blue", "nir")

# Filter the S2 surface-reflectance collection for `ee_filter` and attach the
# Cloud Score+ QA band via linkCollection (matched by system:index), so every
# image carries `qa_band` alongside B4/B3/B2/B8 for downstream masking.
.s2_select_link <- function(ee_aoi, ee_filter, qa_band) {
  cs_plus <- ee$ImageCollection("GOOGLE/CLOUD_SCORE_PLUS/V1/S2_HARMONIZED")
  ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
    filter(ee_filter)$filterBounds(ee_aoi)$
    linkCollection(cs_plus, list(qa_band))
}

# Per-scene Cloud Score+ mask. Returns a closure (over `qa_band` and
# `clear_threshold`) that keeps only pixels whose CS+ score is >= the threshold,
# selects+renames red/green/blue/nir, and scales SR DN to 0-1. Cloud Score+
# (cs_cdf) scores persistent bright surfaces (snow) as clear and transient cloud
# as occluded, so no spectral snow override is needed. The HARMONIZED collection
# already applies the post-2022 -1000 offset, so only divide by 10000 here.
.make_s2_masker <- function(qa_band, clear_threshold) {
  function(img) {
    clear <- img$select(qa_band)$gte(clear_threshold)
    img$select(.s2_src_bands, .s2_out_bands)$
      divide(10000)$
      clamp(0, 1)$
      updateMask(clear)$
      toFloat()
  }
}

# Multi-year, same-calendar-month gap fill for the S2 path. Analogous to
# .hls_multiyear_fill: medoid composites of every clear observation in the same
# month across the `near` year window (target year +/- near_span) and across all
# available years (climatology). Returns list(near, clim); either may be NULL.
.s2_multiyear_fill <- function(ee_aoi, target_year, month, near_span, qa_band, masker) {
  month_filter <- ee$Filter$calendarRange(month, month, "month")

  near_filter <- ee$Filter$And(
    ee$Filter$calendarRange(target_year - near_span, target_year + near_span, "year"),
    month_filter
  )
  near_ic <- .s2_select_link(ee_aoi, near_filter, qa_band)
  clim_ic <- .s2_select_link(ee_aoi, month_filter, qa_band)

  list(
    near = .hls_medoid(near_ic$map(masker), min_obs = 1L),
    clim = .hls_medoid(clim_ic$map(masker), min_obs = 1L)
  )
}

# Main function -----------------------------------------------------------

#' Download Cloud-Free Sentinel-2 Imagery via Cloud Score+ from Google Earth Engine
#'
#' Iterates over each year-month provided, building a cloud-free, snow-preserving
#' surface-reflectance composite from \code{COPERNICUS/S2_SR_HARMONIZED}
#' (Sentinel-2 L2A) imagery masked with Google's \code{GOOGLE/CLOUD_SCORE_PLUS/V1/S2_HARMONIZED}
#' product. This is an alternative to \code{\link{download_hls}} with identical
#' inputs and outputs (4-band red/green/blue/nir, 0-1 reflectance, monthly
#' composites); only the masking and tuning parameters differ.
#'
#' \strong{Why Cloud Score+ instead of a spectral mask.} Snow and cloud cannot be
#' separated spectrally - any bright surface shows low SWIR and an elevated cirrus
#' band - so the spectral cloud rules in \code{download_hls} either leak cloud or
#' misclassify snow as cloud. Cloud Score+ instead grades each pixel's clarity on
#' a continuous 0-1 scale. The \code{cs_cdf} band ranks an observation against the
#' temporal distribution of scores at that location, so a \emph{persistent} bright
#' surface (snow) is scored clear while \emph{transient} bright pixels (cloud) are
#' scored occluded. This dissolves the snow-vs-cloud tie, so \strong{no seasonal
#' snow override is needed} and snow is preserved without leaking cloud.
#'
#' The compositing strategy is:
#' \enumerate{
#'   \item Per-scene masking (\code{.make_s2_masker}): keep pixels whose Cloud
#'         Score+ \code{qa_band} value is >= \code{clear_threshold}; scale SR to
#'         0-1 and clamp to \code{[0, 1]}.
#'   \item Per-pixel \emph{medoid} composite (\code{.hls_medoid}) of the target
#'         month's clear observations - the real observation closest to the
#'         per-pixel median, preserving physically valid spectra.
#'   \item Gap filling from the \strong{same calendar month in other years}
#'         (\code{.s2_multiyear_fill}) - nearest years first (\code{near_span}),
#'         then all available years.
#'   \item Focal median as an absolute last resort for any pixel never clear in
#'         any year.
#' }
#'
#' \strong{Temporal coverage.} Global Sentinel-2 L2A and Cloud Score+ are only
#' reliably available from roughly 2019 onward (L2A global coverage begins late
#' 2018; earlier scenes are sparse or absent). For study windows that predate this,
#' prefer \code{\link{download_hls}}; \code{download_s2} emits a warning for any
#' requested year before 2019.
#'
#' Output reflectance is scaled 0-1, matching \code{download_hls} so the same
#' downstream functions (\code{downscale_lai}, \code{compute_albedo}) consume it
#' unchanged.
#'
#' @param aoi A named list returned by define_aoi() containing elements
#'   \code{geometry} (ee$Geometry) and \code{crs} (CRS string)
#' @param dates Vector of class Date specifying year-month combinations to process.
#'   The day component is ignored; only year and month are used.
#'   E.g. as.Date(c("2020-10-01", "2020-11-01"))
#' @param out_dir Local directory path to save downloaded imagery files
#' @param study_area Optional character string identifying the study area e.g. "GMU1".
#'   If provided, used as a prefix in output file names
#' @param scale Spatial resolution in meters. Default is 30
#' @param qa_band Cloud Score+ QA band used for masking. \code{"cs_cdf"} (a
#'   temporal-CDF ranking, less sensitive to terrain shading) or \code{"cs"} (an
#'   instantaneous spectral-distance score). Default is \code{"cs_cdf"}
#' @param clear_threshold Minimum Cloud Score+ value for a pixel to be kept;
#'   pixels below are masked as cloud/shadow. Values between 0.50 and 0.65
#'   generally work well, with higher values removing more thin cloud, haze and
#'   cirrus shadow. Default is 0.60
#' @param near_span Number of years either side of the target year searched first
#'   (same calendar month) when filling gaps, before falling back to the full
#'   archive. Default is 2
#' @param focal_radius Kernel radius in pixels for the last-resort focal median
#'   that fills any pixel left masked after multi-year filling. Default is 10
#' @param min_obs Minimum number of clear observations required for a target-month
#'   pixel to be kept; pixels with fewer are treated as gaps and filled from other
#'   years. Default is 1
#' @param poll Logical. Whether to poll Google Drive and download files after
#'   all tasks are submitted. Default is TRUE
#' @param poll_interval Polling interval in seconds. Default is 30
#' @param timeout Polling timeout in seconds. Default is 300 (5 minutes)
#'
#' @return Invisibly returns a named list with elements:
#'   \item{submitted}{Number of GEE tasks successfully submitted}
#'   \item{skipped}{A data frame of year/month combos with no imagery in any year}
#'   \item{filled}{A data frame of year/month combos that required multi-year gap filling}
#'   \item{poll_result}{Result of poll_drive() if poll = TRUE, otherwise NULL}
#' @seealso \code{\link{download_hls}}, \code{\link{define_aoi}},
#'   \code{\link{poll_drive}}, \code{\link{compute_albedo}},
#'   \code{\link{compute_reflectance}}
#' @export
download_s2 <- function(aoi,
                        dates,
                        out_dir,
                        study_area = NULL,
                        scale = 30,
                        qa_band = "cs_cdf",
                        clear_threshold = 0.60,
                        near_span = 2,
                        focal_radius = 10,
                        min_obs = 1,
                        poll = TRUE,
                        poll_interval = 30,
                        timeout = 300) {

  # Extract year and month from dates
  dates  <- as.Date(dates)
  years  <- lubridate::year(dates)
  months <- lubridate::month(dates)

  if (!all(c("geometry", "crs") %in% names(aoi))) {
    stop("aoi must be a named list with 'geometry' and 'crs' elements, as returned by define_aoi()")
  }

  ee_aoi <- aoi$geometry
  crs    <- aoi$crs

  # Cloud Score+ handles cloud vs. snow on its own, so the masker is built once
  # and reused for every month (no seasonal snow toggle).
  masker <- .make_s2_masker(qa_band, clear_threshold)

  n_submitted <- 0
  skipped     <- data.frame(year = integer(), month = integer())
  filled      <- data.frame(year = integer(), month = integer())

  cat(sprintf("\n--- Sentinel-2 Imagery Export: %d date combinations ---\n\n", length(dates)))

  for (i in seq_along(dates)) {
    y  <- years[i]
    mo <- months[i]

    if (y < 2019) {
      warning(sprintf(paste0("Year %d predates reliable global Sentinel-2 L2A / Cloud Score+ ",
                             "coverage (~2019); results may be sparse. Consider download_hls() ",
                             "for this window."), y))
    }

    month_filter <- ee$Filter$calendarRange(mo, mo, "month")
    year_filter  <- ee$Filter$calendarRange(y, y, "year")

    target_raw <- .s2_select_link(ee_aoi, ee$Filter$And(year_filter, month_filter), qa_band)
    n <- target_raw$size()$getInfo()
    cat(sprintf("Year %d, Month %02d: %d images found (S2_SR_HARMONIZED)\n", y, mo, n))

    # Target-month medoid (may be NULL or contain gaps).
    combined <- if (n > 0) .hls_medoid(target_raw$map(masker), min_obs = min_obs) else NULL

    has_gaps <- is.null(combined) || .hls_has_gaps(combined, ee_aoi, scale)

    if (has_gaps) {
      cat("  Gaps detected, filling from the same month in other years...\n")
      fills <- .s2_multiyear_fill(ee_aoi, y, mo, near_span, qa_band, masker)

      for (lvl in c("near", "clim")) {
        f <- fills[[lvl]]
        if (is.null(f)) next
        combined <- if (is.null(combined)) f else combined$unmask(f)
      }

      if (is.null(combined)) {
        warning(sprintf("No Sentinel-2 imagery found for month %02d in any year - skipping %d-%02d.",
                        mo, y, mo))
        skipped <- rbind(skipped, data.frame(year = y, month = mo))
        next
      }

      filled <- rbind(filled, data.frame(year = y, month = mo))

      # Absolute last resort: focal median over any pixel never clear in any year.
      combined <- .focalFillMasked(combined, radius = focal_radius)
    }

    combined <- combined$select(.s2_out_bands)$toFloat()$clip(ee_aoi)

    fname <- if (!is.null(study_area)) {
      sprintf("S2_RGBNIR_%s_%d_%02d", study_area, y, mo)
    } else {
      sprintf("S2_RGBNIR_%d_%02d", y, mo)
    }

    task <- ee$batch$Export$image$toDrive(
      image          = combined,
      description    = fname,
      folder         = "GEE_Exports",
      fileNamePrefix = fname,
      region         = ee_aoi,
      scale          = scale,
      crs            = crs,
      maxPixels      = 1e13
    )

    task$start()
    n_submitted <- n_submitted + 1
    cat(sprintf("  Task submitted: %s\n", fname))
  }

  cat(sprintf("\n%d task(s) submitted. %d combo(s) skipped. %d combo(s) used multi-year gap filling.\n",
              n_submitted, nrow(skipped), nrow(filled)))

  # Poll and download
  poll_result <- NULL
  if (poll && n_submitted > 0) {
    cat(sprintf(
      "\nPolling Google Drive for %d file(s). This may take a while depending on the number of images.\n",
      n_submitted
    ))
    poll_result <- poll_drive(
      out_dir       = out_dir,
      n_expected    = n_submitted,
      poll_interval = poll_interval,
      timeout       = timeout
    )
  } else if (poll && n_submitted == 0) {
    cat("No tasks were submitted, skipping Drive polling.\n")
  }

  return(invisible(list(
    submitted   = n_submitted,
    skipped     = skipped,
    filled      = filled,
    poll_result = poll_result
  )))
}

# Main function -----------------------------------------------------------

#' Download Cloud-Free HLS Imagery from Google Earth Engine
#'
#' Iterates over each year-month provided, building a cloud-free, snow-preserving
#' surface-reflectance composite from merged HLS S30 (Sentinel-2) and L30
#' (Landsat 8/9) imagery. Merging both collections nearly doubles observation
#' frequency (~2-3 day revisit). Each composite is a per-pixel \emph{medoid} -
#' the real observation whose spectrum is closest to the per-pixel median -
#' which preserves physically valid spectra and avoids the band-mixing and
#' cloud-edge artefacts of distance-weighted mosaics.
#'
#' The compositing strategy is:
#' \enumerate{
#'   \item Per-scene masking: by default (\code{use_cloud_score = TRUE}) the S30
#'         (Sentinel-2) half is masked with Cloud Score+ (see below) and the L30
#'         (Landsat) half with the spectral mask (\code{.make_hls_masker}); when
#'         \code{use_cloud_score = FALSE} both halves use the spectral mask: Fmask
#'         QA bits (cloud / adjacent / shadow), a 1.38 um cirrus-band test for thin
#'         cloud, a visible + SWIR brightness gate, and a physical reflectance
#'         ceiling.
#'   \item Per-pixel medoid composite of the target month's clear observations.
#'   \item Gap filling from the \strong{same calendar month in other years} -
#'         nearest years first (\code{near_span}); only if gaps still remain is the
#'         heavier full same-month climatology (all available years) built and
#'         applied - so completeness is guaranteed without blending across seasons,
#'         and the expensive full-archive pass is skipped when the nearby-years
#'         fill already completes the image.
#'   \item Focal median as an absolute last resort for any pixel never clear in
#'         any year.
#' }
#'
#' \strong{Cloud Score+ masking (S30).} When \code{use_cloud_score = TRUE}, each
#' HLSS30 scene is linked by exact tile + sensing-time to its
#' \code{GOOGLE/CLOUD_SCORE_PLUS/V1/S2_HARMONIZED} partner and masked where the
#' \code{qa_band} (default \code{cs_cdf}) is below \code{clear_threshold}. Cloud
#' Score+ ranks each pixel against the temporal distribution of clarity scores, so
#' persistent bright surfaces (snow) score clear while transient cloud scores
#' occluded - dissolving the snow-vs-cloud tie that the spectral mask cannot. Cloud
#' Score+ is Sentinel-2-only, so the L30 (Landsat) half keeps the spectral mask.
#' Any S30 scene with no Cloud Score+ partner (e.g. before the Cloud Score+ archive
#' start of 2015-06-27) falls back per-pixel to the spectral mask, so coverage is
#' never reduced. See \code{\link{download_s2}} for a Sentinel-2-only equivalent.
#'
#' \strong{Snow rescue on the Cloud Score+ path.} Cloud Score+ can score bright
#' fresh or early-season snow as occluded and drop it (verified: a Teton peak at
#' NDSI 0.75 / SWIR 0.15 scored \code{cs_cdf} 0.12). To prevent this, a pixel that
#' is unambiguous snow (NDSI \eqn{\ge} \code{ndsi_thresh}, SWIR1 \eqn{<}
#' \code{swir_snow_max}, green \eqn{>} \code{green_thresh}) is kept regardless of
#' its Cloud Score+ value. Cloud is excluded by the low-SWIR gate (cloud is bright
#' in SWIR, snow is dark), and unlike the spectral snow override this rescue is not
#' seasonally gated - Cloud Score+ already removes cloud, so it is safe year-round
#' and recovers snow in shoulder months outside \code{snow_months}.
#'
#' \strong{Antimeridian tile filter.} HLS tiles near 180 deg longitude carry
#' corrupt globe-wrapping footprints that spuriously match \code{filterBounds(aoi)}
#' (for a Teton AOI, 81 of 84 HLSS30 scenes were UTM-zone-01/60 junk with zero
#' pixels over the AOI). Each HLS collection is therefore additionally filtered to
#' the MGRS tiles that actually overlap the AOI - derived from Sentinel-2 L1C, whose
#' footprints are correct - which removes the junk and greatly speeds up exports.
#' The tile is parsed from each scene's \code{system:index} (\code{"T\{TILE\}_..."})
#' rather than the \code{MGRS_TILE_ID} property, because HLSL30 leaves that property
#' null and a property filter would silently drop every Landsat scene.
#'
#' \strong{Seasonal snow handling.} Snow and cloud cannot be separated
#' spectrally: any bright surface (snow or cloud) shows low SWIR and an elevated
#' cirrus band, so an identical signature means cloud in summer but snow in
#' winter. The split is therefore made by month. In \code{snow_months} a snow
#' override (high NDSI, low SWIR, bright green) protects snow from every cloud
#' rule, so snowy scenes are preserved at the cost of some residual cloud. In all
#' other months the cloud rules apply in full and residual cloud is converted to
#' gaps and backfilled from clear same-month observations in other years, rather
#' than selecting a "least-bad but still cloudy" pixel.
#'
#' Output reflectance is scaled 0-1 (already provided that way by GEE for HLS
#' v2.0).
#'
#' @param aoi A named list returned by define_aoi() containing elements
#'   \code{geometry} (ee$Geometry) and \code{crs} (CRS string)
#' @param dates Vector of class Date specifying year-month combinations to process.
#'   The day component is ignored; only year and month are used.
#'   E.g. as.Date(c("2020-10-01", "2020-11-01"))
#' @param out_dir Local directory path to save downloaded imagery files
#' @param study_area Optional character string identifying the study area e.g. "GMU1".
#'   If provided, used as a prefix in output file names
#' @param scale Spatial resolution in meters. Default is 30
#' @param use_cloud_score Logical. If TRUE (default), mask the S30 (Sentinel-2)
#'   half with Cloud Score+ and the L30 (Landsat) half with the spectral mask;
#'   S30 scenes lacking a Cloud Score+ partner fall back to the spectral mask. If
#'   FALSE, both halves use the spectral mask (the original behavior).
#' @param qa_band Cloud Score+ QA band used to mask S30 scenes. \code{"cs_cdf"} (a
#'   temporal-CDF ranking, robust to terrain shading) or \code{"cs"} (an
#'   instantaneous spectral-distance score). Only used when
#'   \code{use_cloud_score = TRUE}. Default is \code{"cs_cdf"}
#' @param clear_threshold Minimum Cloud Score+ value for an S30 pixel to be kept;
#'   pixels below are masked as cloud/shadow. Values between 0.50 and 0.65
#'   generally work well, with higher values removing more thin cloud, haze and
#'   cirrus shadow. Only used when \code{use_cloud_score = TRUE}. Default is 0.60
#' @param near_span Number of years either side of the target year searched first
#'   (same calendar month) when filling gaps, before falling back to the full
#'   archive. Default is 2
#' @param focal_radius Kernel radius in pixels for the last-resort focal median
#'   that fills any pixel left masked after multi-year filling. Default is 10
#' @param snow_months Integer vector of calendar months (1-12) in which the snow
#'   override is active, i.e. the snow season for the study area. In these months
#'   snow is protected from cloud masking (preserving snow at the cost of some
#'   residual cloud); in all other months cloud is masked aggressively and
#'   backfilled from other years. Tune to the study area's elevation and climate.
#'   Default is c(11, 12, 1, 2, 3, 4)
#' @param ndsi_thresh Minimum NDSI = (green - SWIR1)/(green + SWIR1) for the snow
#'   override. Pixels above this (and meeting the SWIR and green gates) are treated
#'   as snow. Governs both the seasonal spectral override (only in
#'   \code{snow_months}) and the year-round Cloud Score+ snow rescue. Default is 0.4
#' @param green_thresh Minimum green reflectance for the snow override. Prevents
#'   dark surfaces from being mistaken for snow. Default is 0.3
#' @param swir_snow_max Maximum SWIR1 reflectance for the snow override. Snow
#'   absorbs SWIR (low values); cloud reflects it. Also the SWIR ceiling for the
#'   Cloud Score+ snow rescue. Raised from 0.12 to capture early-season / slightly
#'   aged mountain snow (SWIR up to ~0.17) while staying well below cloud SWIR
#'   (~0.3+). Default is 0.18
#' @param swir_cloud_thresh SWIR1 reflectance above which a bright-in-visible
#'   pixel is treated as cloud. Default is 0.2
#' @param vis_thresh Green reflectance above which a pixel is "bright in the
#'   visible" for the SWIR cloud gate. Default is 0.25
#' @param cirrus_thresh Cirrus-band (1.38 um) reflectance above which a pixel is
#'   treated as thin cloud. Snow-safe because the surface is dark at 1.38 um.
#'   Default is 0.01
#' @param refl_max Maximum physically plausible reflectance; pixels with any band
#'   above this are masked as residual cloud (except snow in \code{snow_months},
#'   which can exceed 1 on lit slopes at low winter sun). Default is 1.05
#' @param min_obs Minimum number of clear observations required for a target-month
#'   pixel to be kept; pixels with fewer are treated as gaps and filled from other
#'   years. Default is 1
#' @param poll Logical. Whether to poll Google Drive and download files after
#'   all tasks are submitted. Default is TRUE
#' @param poll_interval Polling interval in seconds. Default is 30
#' @param timeout Polling timeout in seconds. Default is 300 (5 minutes)
#'
#' @return Invisibly returns a named list with elements:
#'   \item{submitted}{Number of GEE tasks successfully submitted}
#'   \item{skipped}{A data frame of year/month combos with no imagery in any year}
#'   \item{filled}{A data frame of year/month combos that required multi-year gap filling}
#'   \item{poll_result}{Result of poll_drive() if poll = TRUE, otherwise NULL}
#' @seealso \code{\link{download_s2}}, \code{\link{define_aoi}},
#'   \code{\link{poll_drive}}, \code{\link{compute_albedo}},
#'   \code{\link{compute_reflectance}}
#' @export
download_hls <- function(aoi,
                        dates,
                        out_dir,
                        study_area = NULL,
                        scale = 30,
                        use_cloud_score = TRUE,
                        qa_band = "cs_cdf",
                        clear_threshold = 0.60,
                        near_span = 2,
                        focal_radius = 10,
                        snow_months = c(11, 12, 1, 2, 3, 4),
                        ndsi_thresh = 0.4,
                        green_thresh = 0.3,
                        swir_snow_max = 0.18,
                        swir_cloud_thresh = 0.2,
                        vis_thresh = 0.25,
                        cirrus_thresh = 0.01,
                        refl_max = 1.05,
                        min_obs = 1,
                        poll = TRUE,
                        poll_interval = 30,
                        timeout = 300) {

  # Extract year and month from dates
  dates  <- as.Date(dates)
  years  <- lubridate::year(dates)
  months <- lubridate::month(dates)

  if (!all(c("geometry", "crs") %in% names(aoi))) {
    stop("aoi must be a named list with 'geometry' and 'crs' elements, as returned by define_aoi()")
  }

  ee_aoi <- aoi$geometry
  crs    <- aoi$crs

  # MGRS tiles overlapping the AOI, computed once. Used to strip antimeridian-
  # footprint junk from every HLS collection (see .hls_aoi_tiles / .hls_filter_tiles).
  tiles <- .hls_aoi_tiles(ee_aoi)

  # HLS band mappings (S30=Sentinel-2, L30=Landsat 8/9). swir1 and cirrus are
  # used only for masking and dropped before export.
  s30_bands   <- c("B4", "B3", "B2", "B8", "B11", "B10", "Fmask")
  l30_bands   <- c("B4", "B3", "B2", "B5", "B6",  "B9",  "Fmask")
  band_rename <- c("red", "green", "blue", "nir", "swir1", "cirrus", "Fmask")

  mp <- list(
    ndsi_thresh       = ndsi_thresh,
    green_thresh      = green_thresh,
    swir_snow_max     = swir_snow_max,
    swir_cloud_thresh = swir_cloud_thresh,
    vis_thresh        = vis_thresh,
    cirrus_thresh     = cirrus_thresh,
    refl_max          = refl_max
  )

  n_submitted <- 0
  skipped     <- data.frame(year = integer(), month = integer())
  filled      <- data.frame(year = integer(), month = integer())

  cat(sprintf("\n--- HLS Imagery Export: %d date combinations ---\n\n", length(dates)))

  for (i in seq_along(dates)) {
    y  <- years[i]
    mo <- months[i]

    # Seasonal spectral snow protection (keep snow, allow some residual cloud) only
    # in snow-season months; snow-free months mask cloud aggressively and backfill
    # from other years. Snow and cloud share a spectral signature, so this seasonal
    # split governs the L30 half and any S30 pixels that fall back to the spectral
    # mask. The Cloud Score+ S30 path has its OWN snow rescue (in .make_hls_masker_cs)
    # that runs year-round - it can afford to, since Cloud Score+ has already removed
    # cloud - so it does not depend on this toggle.
    mp$protect_snow <- mo %in% snow_months

    month_filter <- ee$Filter$calendarRange(mo, mo, "month")
    year_filter  <- ee$Filter$calendarRange(y, y, "year")

    target_masked <- .hls_build_masked(
      ee_aoi, ee$Filter$And(year_filter, month_filter),
      s30_bands, l30_bands, band_rename,
      mp, use_cloud_score, qa_band, clear_threshold, tiles
    )
    n <- target_masked$size()$getInfo()
    mode_lbl <- if (use_cloud_score) {
      " [S30 CloudScore+ / L30 spectral]"
    } else if (mp$protect_snow) {
      " [snow-protected]"
    } else {
      " [aggressive cloud removal]"
    }
    cat(sprintf("Year %d, Month %02d: %d images found (S30+L30)%s\n", y, mo, n, mode_lbl))

    # Target-month medoid (may be NULL or contain gaps).
    combined <- if (n > 0) .hls_medoid(target_masked, min_obs = min_obs) else NULL

    has_gaps <- is.null(combined) || .hls_has_gaps(combined, ee_aoi, scale)

    if (has_gaps) {
      cat("  Gaps detected, filling from nearby years (same month)...\n")
      near <- .hls_fill_near(
        ee_aoi, y, mo, near_span,
        s30_bands, l30_bands, band_rename,
        mp, use_cloud_score, qa_band, clear_threshold, tiles
      )
      if (!is.null(near)) {
        combined <- if (is.null(combined)) near else combined$unmask(near)
      }

      # Only build the full same-month climatology medoid - by far the heaviest
      # graph (the whole archive can be thousands of scenes) - if gaps remain
      # after the nearby-years fill, short-circuiting the expensive path for months
      # the near fill already completes. The probe runs at a coarse scale (~10x the
      # working scale) with a high tileScale purely to survive the interactive
      # getInfo memory limit on the heavy near medoid - a 30 m probe exceeds it.
      # anyNonZero aggregates sub-pixels, so even a single 30 m gap is still
      # detected (verified): clim is built whenever ANY gap remains, so fill
      # behavior is identical to the original - only clim-when-already-complete is
      # skipped. If even the coarse probe is too heavy to evaluate, fall back to
      # building the climatology (the original always-on behavior).
      gap_scale <- max(focal_radius * scale, scale)
      still_gaps <- if (is.null(combined)) {
        TRUE
      } else {
        tryCatch(
          .hls_has_gaps(combined, ee_aoi, gap_scale, tile_scale = 16),
          error = function(e) {
            cat("  (gap probe too heavy to evaluate; building climatology fill)\n")
            TRUE
          }
        )
      }

      if (still_gaps) {
        cat("  Gaps remain, filling from the full same-month climatology...\n")
        clim <- .hls_fill_clim(
          ee_aoi, mo,
          s30_bands, l30_bands, band_rename,
          mp, use_cloud_score, qa_band, clear_threshold, tiles
        )
        if (!is.null(clim)) {
          combined <- if (is.null(combined)) clim else combined$unmask(clim)
        }
      }

      if (is.null(combined)) {
        warning(sprintf("No HLS imagery found for month %02d in any year - skipping %d-%02d.",
                        mo, y, mo))
        skipped <- rbind(skipped, data.frame(year = y, month = mo))
        next
      }

      filled <- rbind(filled, data.frame(year = y, month = mo))

      # Absolute last resort: focal median over any pixel never clear in any year.
      combined <- .focalFillMasked(combined, radius = focal_radius)
    }

    combined <- combined$select(.hls_out_bands)$toFloat()$clip(ee_aoi)

    fname <- if (!is.null(study_area)) {
      sprintf("HLS_RGBNIR_%s_%d_%02d", study_area, y, mo)
    } else {
      sprintf("HLS_RGBNIR_%d_%02d", y, mo)
    }

    task <- ee$batch$Export$image$toDrive(
      image          = combined,
      description    = fname,
      folder         = "GEE_Exports",
      fileNamePrefix = fname,
      region         = ee_aoi,
      scale          = scale,
      crs            = crs,
      maxPixels      = 1e13
    )

    task$start()
    n_submitted <- n_submitted + 1
    cat(sprintf("  Task submitted: %s\n", fname))
  }

  cat(sprintf("\n%d task(s) submitted. %d combo(s) skipped. %d combo(s) used multi-year gap filling.\n",
              n_submitted, nrow(skipped), nrow(filled)))

  # Poll and download
  poll_result <- NULL
  if (poll && n_submitted > 0) {
    cat(sprintf(
      "\nPolling Google Drive for %d file(s). This may take a while depending on the number of images.\n",
      n_submitted
    ))
    poll_result <- poll_drive(
      out_dir       = out_dir,
      n_expected    = n_submitted,
      poll_interval = poll_interval,
      timeout       = timeout
    )
  } else if (poll && n_submitted == 0) {
    cat("No tasks were submitted, skipping Drive polling.\n")
  }

  return(invisible(list(
    submitted   = n_submitted,
    skipped     = skipped,
    filled      = filled,
    poll_result = poll_result
  )))
}

# Internal helpers --------------------------------------------------------
.generate_month_sequence <- function(start_date, end_date) {
  start_date  <- as.Date(start_date)
  end_date    <- as.Date(end_date)
  start_year  <- lubridate::year(start_date)
  start_month <- lubridate::month(start_date)
  end_year    <- lubridate::year(end_date)
  end_month   <- lubridate::month(end_date)

  months_list <- list()
  for (y in start_year:end_year) {
    m_start <- if (y == start_year) start_month else 1
    m_end   <- if (y == end_year)   end_month   else 12
    months_list[[length(months_list) + 1]] <- data.frame(
      year  = y,
      month = m_start:m_end,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, months_list)
}

.maskMODIS_LAI <- function(img) {
  qc <- img$select("FparLai_QC")

  # Bit 0: MODLAND QC - mask backup/fill
  bad_quality  <- qc$bitwiseAnd(2^0)$neq(0)

  # Bits 3-4: CloudState - mask clouds present (1) and mixed (2)
  cloud_state  <- qc$rightShift(3)$bitwiseAnd(3)
  cloud        <- cloud_state$eq(1)
  mixed_cloud  <- cloud_state$eq(2)

  # Bits 5-7: SCF_QC - mask pixel not produced (4)
  scf_qc       <- qc$rightShift(5)$bitwiseAnd(7)
  not_produced <- scf_qc$eq(4)

  mask <- bad_quality$Or(cloud)$Or(mixed_cloud)$Or(not_produced)$Not()
  img$updateMask(mask)
}

.scaleMODIS_LAI <- function(img) {
  img$select("Lai")$multiply(0.1)$rename("lai")
}

.extract_ym <- function(files) {
  matches <- regmatches(files, regexpr("[0-9]{4}_[0-9]{2}", files))
  data.frame(file = files, ym = matches, stringsAsFactors = FALSE)
}

.extract_year <- function(files) {
  matches <- regmatches(files, regexpr("[0-9]{4}", files))
  data.frame(file = files, year = as.integer(matches), stringsAsFactors = FALSE)
}

# Main function -----------------------------------------------------------

#' Download MODIS LAI Data from Google Earth Engine
#'
#' Iterates over each date provided, applying QC masking and scaling to MODIS
#' MCD15A3H LAI imagery before exporting monthly median composites from GEE to
#' Google Drive, then optionally downloading them locally using poll_drive().
#'
#' The masking strategy applies the following QC filters:
#' \enumerate{
#'   \item Bit 0: Masks backup/fill quality pixels
#'   \item Bits 3-4: Masks cloudy and mixed cloud pixels
#'   \item Bits 5-7: Masks pixels that could not be produced
#' }
#' Remaining gaps are filled using a focal median, with unmasked raw values
#' as a final fallback.
#'
#' @param aoi A named list returned by define_aoi() containing elements
#'   \code{geometry} (ee$Geometry) and \code{crs} (CRS string)
#' @param dates Vector of class Date specifying year-month combinations to process.
#'   The day component is ignored; only year and month are used.
#'   E.g. as.Date(c("2020-10-01", "2020-11-01"))
#' @param out_dir Local directory path to save downloaded LAI files
#' @param study_area Optional character string identifying the study area e.g. "GMU1".
#'   If provided, used as a prefix in output file names
#' @param scale Spatial resolution in meters. Default is 500
#' @param focal_radius Kernel radius in pixels for focal median gap filling.
#'   Default is 10 (21x21 pixel window)
#' @param poll Logical. Whether to poll Google Drive and download files after
#'   all tasks are submitted. Default is TRUE
#' @param poll_interval Polling interval in seconds. Default is 30
#' @param timeout Polling timeout in seconds. Default is 300 (5 minutes)
#'
#' @return Invisibly returns a named list with elements:
#'   \item{submitted}{Number of GEE tasks successfully submitted}
#'   \item{skipped}{A data frame of skipped year/month combos}
#'   \item{poll_result}{Result of poll_drive() if poll = TRUE, otherwise NULL}
#' @export
download_modis_lai <- function(aoi,
                              dates,
                              out_dir,
                              study_area = NULL,
                              scale = 500,
                              focal_radius = 10,
                              poll = TRUE,
                              poll_interval = 30,
                              timeout = 300) {

  # Extract year and month from dates
  dates <- as.Date(dates)
  years <- lubridate::year(dates)
  months <- lubridate::month(dates)

  if (!all(c("geometry", "crs") %in% names(aoi))) {
    stop("aoi must be a named list with 'geometry' and 'crs' elements, as returned by define_aoi()")
  }

  ee_aoi <- aoi$geometry
  crs    <- aoi$crs

  n_submitted <- 0
  skipped     <- data.frame(year = integer(), month = integer())

  cat(sprintf("\n--- MODIS LAI Export: %d date combinations ---\n\n", length(dates)))

  for (i in seq_along(dates)) {
    y <- years[i]
    mo <- months[i]

      start_date <- sprintf("%d-%02d-01", y, mo)
      end_date   <- as.character(lubridate::ceiling_date(as.Date(start_date), unit = "month"))

      modis_ic_raw <- ee$ImageCollection("MODIS/061/MCD15A3H")$
        filterDate(start_date, end_date)$
        filterBounds(ee_aoi)$
        select(c("Lai", "FparLai_QC"))

      n <- modis_ic_raw$size()$getInfo()

      if (n == 0) {
        warning(sprintf("No images found for year %d month %02d - skipping.", y, mo))
        skipped <- rbind(skipped, data.frame(year = y, month = mo))
        next
      }

      cat(sprintf("Year %d, Month %02d: %d images found\n", y, mo, n))

      # Apply QC mask, scale, and compute median
      lai_masked <- modis_ic_raw$map(.maskMODIS_LAI)$map(.scaleMODIS_LAI)$median()$clip(ee_aoi)
      lai_raw    <- modis_ic_raw$map(.scaleMODIS_LAI)$median()$clip(ee_aoi)

      # Focal fill then fallback to raw
      lai_filled <- .focalFillMasked(lai_masked, radius = focal_radius)$unmask(lai_raw)

      # Build file name
      fname <- if (!is.null(study_area)) {
        sprintf("MODIS_LAI_%s_%d_%02d", study_area, y, mo)
      } else {
        sprintf("MODIS_LAI_%d_%02d", y, mo)
      }

      task <- ee$batch$Export$image$toDrive(
        image          = lai_filled,
        description    = fname,
        folder         = "GEE_Exports",
        fileNamePrefix = fname,
        region         = ee_aoi,
        scale          = scale,
        crs            = crs,
        maxPixels      = 1e13
      )

      task$start()
      n_submitted <- n_submitted + 1
      cat(sprintf("  Task submitted: %s\n", fname))
    }

  cat(sprintf("\n%d task(s) submitted. %d combo(s) skipped.\n", n_submitted, nrow(skipped)))

  # Poll and download
  poll_result <- NULL
  if (poll && n_submitted > 0) {
    cat(sprintf(
      "\nPolling Google Drive for %d file(s). This may take a while depending on the number of images.\n",
      n_submitted
    ))
    poll_result <- poll_drive(
      out_dir       = out_dir,
      n_expected    = n_submitted,
      poll_interval = poll_interval,
      timeout       = timeout
    )
  } else if (poll && n_submitted == 0) {
    cat("No tasks were submitted, skipping Drive polling.\n")
  }

  return(invisible(list(
    submitted   = n_submitted,
    skipped     = skipped,
    poll_result = poll_result
  )))
}

#' Compute and Adjust Albedo from Sentinel-2 Imagery
#'
#' Iterates over each date provided, computing photographic albedo from
#' Sentinel-2 imagery using \code{microclimdata::albedo_fromaerial()} and
#' adjusting it to MODIS broadband albedo using \code{microclimdata::albedo_adjust()}.
#' Default band wavelength parameters are specific to the Sentinel-2 (HLSS30 /
#' download_s2()) product. If using a different sensor, adjust the band wavelength
#' arguments accordingly. For HLS band specifications see:
#' \url{https://lpdaac.usgs.gov/documents/1698/HLS_User_Guide_V2.pdf}
#'
#' @param dates Vector of class Date specifying year-month combinations to process.
#'   The day component is ignored; only year and month are used.
#'   E.g. as.Date(c("2020-10-01", "2020-11-01"))
#' @param modis_dir Directory containing MODIS albedo files
#' @param img_dir Directory containing multispectral imagery files (from
#'   download_hls() or download_s2())
#' @param out_dir Directory to save output albedo files
#' @param study_area Optional character string identifying the study area e.g. "GMU1".
#'   If provided, used to filter input files and as a prefix in output file names
#' @param rgb_band_mins Numeric vector of RGB band minimum wavelengths in nm.
#'   Default is c(640, 530, 450) for Sentinel-2
#' @param rgb_band_maxs Numeric vector of RGB band maximum wavelengths in nm.
#'   Default is c(670, 590, 510) for Sentinel-2
#' @param cir_band_mins Numeric vector of CIR band minimum wavelengths in nm.
#'   Default is c(780, 640, 530) for Sentinel-2
#' @param cir_band_maxs Numeric vector of CIR band maximum wavelengths in nm.
#'   Default is c(880, 670, 590) for Sentinel-2
#'
#' @return Invisibly returns a data frame logging the status of each
#'   year/month combination processed
#' @export
compute_albedo <- function(dates,
                          modis_dir,
                          img_dir,
                          out_dir,
                          study_area = NULL,
                          rgb_band_mins = c(640, 530, 450),
                          rgb_band_maxs = c(670, 590, 510),
                          cir_band_mins = c(780, 640, 530),
                          cir_band_maxs = c(880, 670, 590)) {

  # Extract year and month from dates
  dates <- as.Date(dates)
  years <- lubridate::year(dates)
  months <- lubridate::month(dates)

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # List files in both directories
  modis_files <- list.files(modis_dir, pattern = "\\.tif$", full.names = TRUE)
  img_files   <- list.files(img_dir,   pattern = "\\.tif$", full.names = TRUE)

  # Optionally filter by study area
  if (!is.null(study_area)) {
    modis_files <- modis_files[grepl(study_area, modis_files)]
    img_files   <- img_files[grepl(study_area, img_files)]
  }

  modis_df <- .extract_ym(modis_files)
  img_df   <- .extract_ym(img_files)

  # Build log from requested year/month combos
  log <- data.frame(year = years, month = months, stringsAsFactors = FALSE)
  log$ym     <- sprintf("%d_%02d", log$year, log$month)
  log$status <- NA_character_

  cat(sprintf("\n--- Albedo Computation: %d date combinations ---\n\n", nrow(log)))

  for (i in seq_len(nrow(log))) {

    y  <- log$year[i]
    mo <- log$month[i]
    ym <- log$ym[i]

    cat(sprintf("Processing year: %d | month: %02d\n", y, mo))

    # Match files by YYYY_MM
    modis_match <- modis_df$file[modis_df$ym == ym]
    img_match   <- img_df$file[img_df$ym == ym]

    # Check files exist and are unambiguous
    if (length(modis_match) == 0) {
      stop(sprintf("MODIS albedo file not found for %s in:\n  %s", ym, modis_dir))
    }
    if (length(img_match) == 0) {
      stop(sprintf("Imagery file not found for %s in:\n  %s", ym, img_dir))
    }
    if (length(modis_match) > 1) {
      stop(sprintf("Multiple MODIS albedo files found for %s in:\n  %s", ym, modis_dir))
    }
    if (length(img_match) > 1) {
      stop(sprintf("Multiple imagery files found for %s in:\n  %s", ym, img_dir))
    }

    # Load imagery
    s_rast <- terra::rast(img_match)
    m_rast <- terra::rast(modis_match)

    # Rescale each band to 0-1

    if(any(terra::values(s_rast) > 1 | terra::values(s_rast) < 0)){

      s_rast <- terra::sapp(s_rast, function(x) {
        rng <- range(terra::values(x), na.rm = TRUE)
        (x - rng[1]) / (rng[2] - rng[1])
      })

    }

    # Build rgb and cir stacks
    rgb <- c(s_rast$red, s_rast$green, s_rast$blue)
    cir <- c(s_rast$nir, s_rast$red, s_rast$green)
    rm(s_rast)

    # Clamp and scale to 0-250
    rgb <- rgb * 250
    cir <- cir * 250

    # Compute photographic albedo
    albphoto <- microclimdata::albedo_fromaerial(
      rgb,
      cir,
      RGBbandmins = rgb_band_mins,
      RGBbandmaxs = rgb_band_maxs,
      CIRbandmins = cir_band_mins,
      CIRbandmaxs = cir_band_maxs
    )

    # Clamp to valid range
    albphoto <- terra::clamp(albphoto, lower = 1e-6, upper = 1 - 1e-6)
    m_rast <- terra::clamp(m_rast, lower = 1e-6, upper = 1 - 1e-6)

    # Adjust to MODIS broadband albedo
    albadjusted <- microclimdata::albedo_adjust(albphoto, m_rast)

    # Build output file name, carrying the imagery source prefix (the token
    # before "_RGBNIR", e.g. "HLS" or "S2"). Falls back to "Img" if the input
    # filename does not follow the *_RGBNIR_* convention.
    src <- sub("_RGBNIR.*", "", basename(img_match))
    if (identical(src, basename(img_match))) src <- "Img"

    out_name <- if (!is.null(study_area)) {
      sprintf("%s_Albedo_%s_%s.tif", src, study_area, ym)
    } else {
      sprintf("%s_Albedo_%s.tif", src, ym)
    }

    terra::writeRaster(
      albadjusted,
      file.path(out_dir, out_name),
      overwrite = TRUE
    )

    cat(sprintf("  Saved: %s\n", out_name))
    log$status[i] <- "success"
  }

  cat(sprintf("\nDone. %d/%d combinations processed successfully.\n",
              sum(log$status == "success", na.rm = TRUE), nrow(log)))

  return(invisible(log))
}

#' Compute Leaf and Ground Reflectance from LAI, Albedo and Land Cover
#'
#' Iterates over each date provided, computing leaf and ground reflectance
#' using \code{microclimdata::reflectance_calc()}. Land cover based x values are
#' computed annually (directory mode) or once (static mode) and reused across
#' all months. LAI and albedo are processed monthly.
#'
#' @param dates Vector of class Date specifying year-month combinations to process.
#'   The day component is ignored; only year and month are used.
#'   E.g. as.Date(c("2020-10-01", "2020-11-01"))
#' @param landcover Either (a) a directory path containing annual land cover
#'   \code{.tif} files (one per year, matched by a four-digit year in the file
#'   name), or (b) a file path to a single land cover raster, or (c) a
#'   \code{SpatRaster} object. In cases (b) and (c) the same raster is used
#'   across all years (static mode).
#' @param lai_dir Directory containing fine resolution LAI files
#' @param alb_dir Directory containing HLS albedo files
#' @param out_dir_lref Directory to save leaf reflectance output files
#' @param out_dir_gref Directory to save ground reflectance output files
#' @param xcalc_dir Optional directory to save x calculation rasters. In
#'   directory mode rasters are saved with a year suffix; in static mode the
#'   raster is saved without a year suffix. If NULL x rasters are kept in
#'   memory only. Default is NULL
#' @param study_area Optional character string identifying the study area e.g. "GMU1".
#'   If provided, used to filter input files and as a prefix in output file names
#' @param lctype Land cover classification type passed to \code{microclimdata::x_calc()}.
#'   Must be either "CORINE" or "ESA". Default is "CORINE"
#'
#' @return Invisibly returns a data frame logging the status of each
#'   year/month combination processed
#' @export
compute_reflectance <- function(dates,
                               landcover,
                               lai_dir,
                               alb_dir,
                               out_dir_lref,
                               out_dir_gref,
                               xcalc_dir = NULL,
                               study_area = NULL,
                               lctype = "CORINE") {

  dates  <- as.Date(dates)
  years  <- lubridate::year(dates)
  months <- lubridate::month(dates)

  if (!lctype %in% c("CORINE", "ESA")) stop("lctype must be either 'CORINE' or 'ESA'")

  dir.create(out_dir_lref, recursive = TRUE, showWarnings = FALSE)
  dir.create(out_dir_gref, recursive = TRUE, showWarnings = FALSE)
  if (!is.null(xcalc_dir)) dir.create(xcalc_dir, recursive = TRUE, showWarnings = FALSE)

  # TRUE when landcover is a SpatRaster or a path to a single file (not a dir)
  is_static <- inherits(landcover, "SpatRaster") ||
    (is.character(landcover) && file.exists(landcover) && !dir.exists(landcover))

  lai_files <- list.files(lai_dir, pattern = "\\.tif$", full.names = TRUE)
  alb_files <- list.files(alb_dir, pattern = "\\.tif$", full.names = TRUE)

  if (!is.null(study_area)) {
    lai_files <- lai_files[grepl(study_area, lai_files)]
    alb_files <- alb_files[grepl(study_area, alb_files)]
  }

  lai_df <- .extract_ym(lai_files)
  alb_df <- .extract_ym(alb_files)

  # Directory mode: index per-year land cover files up front
  lc_df <- NULL
  if (!is_static) {
    lc_files <- list.files(landcover, pattern = "\\.tif$", full.names = TRUE)
    if (!is.null(study_area)) lc_files <- lc_files[grepl(study_area, lc_files)]
    lc_df <- .extract_year(lc_files)
  }

  log        <- data.frame(year = years, month = months, stringsAsFactors = FALSE)
  log$ym     <- sprintf("%d_%02d", log$year, log$month)
  log$status <- NA_character_

  cat(sprintf("\n--- Reflectance Computation: %d date combinations ---\n\n", nrow(log)))

  current_year <- NULL
  x            <- NULL
  com_ext      <- NULL

  # Static mode: compute x_calc once before the loop
  if (is_static) {
    cat("Computing x values from static land cover...\n")
    lc <- if (inherits(landcover, "SpatRaster")) landcover else terra::rast(landcover)
    x  <- microclimdata::x_calc(lc, lctype = lctype)

    first_ym     <- log$ym[1]
    lai_ref_file <- lai_df$file[lai_df$ym == first_ym]
    if (length(lai_ref_file) == 0) stop(sprintf("Reference LAI not found for %s", first_ym))
    lai_template <- terra::rast(lai_ref_file)
    com_ext      <- terra::intersect(terra::ext(lai_template), terra::ext(x))
    x            <- terra::crop(x, com_ext)
    x            <- terra::resample(x, lai_template)
    rm(lai_template)

    if (!is.null(xcalc_dir)) {
      x_out_name <- if (!is.null(study_area)) {
        sprintf("X_Calc_%s.tif", study_area)
      } else {
        "X_Calc.tif"
      }
      terra::writeRaster(x, file.path(xcalc_dir, x_out_name), overwrite = TRUE)
      cat(sprintf("  Saved: %s\n", x_out_name))
    }
  }

  for (i in seq_len(nrow(log))) {

    y  <- log$year[i]
    mo <- log$month[i]
    ym <- log$ym[i]

    if (is.null(current_year) || y != current_year) {
      if (!is_static) {
        # Directory mode: recompute x_calc for the new year
        cat(sprintf("Year %d: computing x values from land cover...\n", y))

        lc_match <- lc_df$file[lc_df$year == y]
        if (length(lc_match) == 0) stop(sprintf("Land cover file not found for year %d in:\n  %s", y, landcover))
        if (length(lc_match) > 1) stop(sprintf("Multiple land cover files found for year %d in:\n  %s", y, landcover))

        lc <- terra::rast(lc_match)
        x  <- microclimdata::x_calc(lc, lctype = lctype)

        first_ym_for_year <- log$ym[log$year == y][1]
        lai_ref <- lai_df$file[lai_df$ym == first_ym_for_year]
        if (length(lai_ref) == 0) stop(sprintf("Reference LAI not found for year %d", y))

        lai_template <- terra::rast(lai_ref)
        com_ext      <- terra::intersect(terra::ext(lai_template), terra::ext(x))
        x            <- terra::crop(x, com_ext)
        x            <- terra::resample(x, lai_template)
        rm(lai_template)

        if (!is.null(xcalc_dir)) {
          x_out_name <- if (!is.null(study_area)) {
            sprintf("X_Calc_%s_%d.tif", study_area, y)
          } else {
            sprintf("X_Calc_%d.tif", y)
          }
          terra::writeRaster(x, file.path(xcalc_dir, x_out_name), overwrite = TRUE)
          cat(sprintf("  Saved: %s\n", x_out_name))
        }
      } else {
        cat(sprintf("Year %d:\n", y))
      }
      current_year <- y
    }

    cat(sprintf("  Processing month: %02d\n", mo))

    lai_match <- lai_df$file[lai_df$ym == ym]
    alb_match <- alb_df$file[alb_df$ym == ym]

    if (length(lai_match) == 0) stop(sprintf("LAI file not found for %s in:\n  %s", ym, lai_dir))
    if (length(alb_match) == 0) stop(sprintf("Albedo file not found for %s in:\n  %s", ym, alb_dir))
    if (length(lai_match) > 1) stop(sprintf("Multiple LAI files found for %s in:\n  %s", ym, lai_dir))
    if (length(alb_match) > 1) stop(sprintf("Multiple albedo files found for %s in:\n  %s", ym, alb_dir))

    lai <- terra::rast(lai_match)
    alb <- terra::rast(alb_match)

    lai <- terra::crop(lai, com_ext)
    alb <- terra::crop(alb, com_ext)

    if (!isTRUE(all.equal(terra::ext(x), terra::ext(lai)))  ||
        !isTRUE(all.equal(terra::ext(x), terra::ext(alb)))  ||
        !isTRUE(all.equal(terra::res(x), terra::res(lai)))  ||
        !isTRUE(all.equal(terra::res(x), terra::res(alb)))) {
      stop(sprintf(
        "Extent or resolution of x, alb, and lai did not match for %s. Check preprocessing steps.", ym
      ))
    }

    refldata <- microclimdata::reflectance_calc(alb, lai, x, plotprogress = FALSE)

    lref_name <- if (!is.null(study_area)) {
      sprintf("LF_Refl_%s_%s.tif", study_area, ym)
    } else {
      sprintf("LF_Refl_%s.tif", ym)
    }

    gref_name <- if (!is.null(study_area)) {
      sprintf("GF_Refl_%s_%s.tif", study_area, ym)
    } else {
      sprintf("GF_Refl_%s.tif", ym)
    }

    terra::writeRaster(refldata$lref, file.path(out_dir_lref, lref_name), overwrite = TRUE)
    terra::writeRaster(refldata$gref, file.path(out_dir_gref, gref_name), overwrite = TRUE)

    cat(sprintf("    Saved: %s\n", lref_name))
    cat(sprintf("    Saved: %s\n", gref_name))

    log$status[log$ym == ym] <- "success"
  }

  cat(sprintf("\nDone. %d/%d combinations processed successfully.\n",
              sum(log$status == "success", na.rm = TRUE), nrow(log)))

  return(invisible(log))
}

#' Download and Downscale Soil Data
#'
#' Downloads coarse resolution soil data using \code{microclimdata::soildata_download()}
#' and downscales it to fine resolution using \code{microclimdata::soildata_downscale()}.
#' Requires a Digital Elevation Model and a single static land cover raster.
#'
#' @param dtm A SpatRaster or file path to a Digital Elevation Model raster
#' @param landcover A SpatRaster or file path to a land cover raster. Should
#'   represent a single static year
#' @param out_dir Directory to save soil data outputs
#' @param study_area Optional character string identifying the study area e.g. "GMU1".
#'   If provided, used as a prefix in output file names
#' @param water Integer. CORINE or ESA land cover code for water bodies passed
#'   to \code{soildata_downscale()}. Default is 512 for CORINE water class
#' @param delete_tmp Logical. Whether to delete temporary files after processing.
#'   Default is TRUE
#'
#' @return Invisibly returns a named list with elements:
#'   \item{coarse}{The coarse resolution soil SpatRaster}
#'   \item{fine}{The fine resolution soil SpatRaster}
#' @export
download_soil <- function(dtm,
                          landcover,
                          out_dir,
                          study_area = NULL,
                          water = 512,
                          delete_tmp = TRUE) {

  # Accept file path or SpatRaster for dtm
  if (is.character(dtm)) {
    dtm <- terra::rast(dtm)
  } else if (!inherits(dtm, "SpatRaster")) {
    stop("dtm must be a SpatRaster or a file path to a raster file")
  }

  if (is.character(landcover)) {
    landcover <- terra::rast(landcover)
  } else if (!inherits(landcover, "SpatRaster")) {
    stop("landcover must be a SpatRaster or a file path to a raster file")
  }

  # Create output and temp directories
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  tmp_dir <- file.path(out_dir, "tmp")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  # Project DTM to WGS84 for soildata_download
  dtm_wgs84 <- terra::project(dtm, "epsg:4326")

  # Download coarse soil data
  cat("Downloading coarse soil data...\n")
  soildata <- microclimdata::soildata_download(
    dtm_wgs84,
    pathdir     = tmp_dir,
    deletefiles = !delete_tmp
  )

  # Build coarse output file name
  coarse_name <- if (!is.null(study_area)) {
    sprintf("SoilData_Coarse_%s.tif", study_area)
  } else {
    "SoilData_Coarse.tif"
  }

  terra::writeRaster(soildata, file.path(out_dir, coarse_name), overwrite = TRUE)
  cat(sprintf("  Saved: %s\n", coarse_name))

  # Project land cover to WGS84 for soildata_downscale
  lc_wgs84 <- terra::project(landcover, "epsg:4326")

  # Downscale soil data
  cat("Downscaling soil data...\n")
  soildata_fine <- microclimdata::soildata_downscale(soildata, lc_wgs84, water = water)

  # Reproject back to original DTM CRS
  soildata_fine <- terra::project(soildata_fine, dtm)

  # Build fine output file name
  fine_name <- if (!is.null(study_area)) {
    sprintf("SoilData_Fine_%s.tif", study_area)
  } else {
    "SoilData_Fine.tif"
  }

  terra::writeRaster(soildata_fine, file.path(out_dir, fine_name), overwrite = TRUE)
  cat(sprintf("  Saved: %s\n", fine_name))

  # Clean up temp directory
  if (delete_tmp) {
    unlink(tmp_dir, recursive = TRUE)
    cat("  Temporary files removed.\n")
  }

  return(invisible(list(
    coarse = soildata,
    fine   = soildata_fine
  )))
}

# Shared aoi -> WGS84 bbox extraction, used by download_aorc() and
# download_arcgis_landcover(). terra::ext() indexes as (xmin, xmax, ymin,
# ymax) -- NOT the (xmin, ymin, xmax, ymax) order sf::st_bbox() uses -- so
# the SpatRaster/SpatVector branch is careful not to mix the two.
.aoi_to_wgs84_bbox <- function(aoi) {
  if (is.list(aoi) && all(c("geometry", "crs") %in% names(aoi)) && !is.null(aoi$bbox_wgs84)) {
    bb <- aoi$bbox_wgs84
    xmin <- as.numeric(bb[1])
    ymin <- as.numeric(bb[2])
    xmax <- as.numeric(bb[3])
    ymax <- as.numeric(bb[4])
  } else if (inherits(aoi, "SpatRaster") || inherits(aoi, "SpatVector")) {
    e <- terra::ext(terra::project(aoi, "epsg:4326"))
    xmin <- as.numeric(e[1])
    xmax <- as.numeric(e[2])
    ymin <- as.numeric(e[3])
    ymax <- as.numeric(e[4])
  } else if (inherits(aoi, "sf") || inherits(aoi, "sfc")) {
    bb <- sf::st_bbox(sf::st_transform(aoi, 4326))
    xmin <- as.numeric(bb["xmin"])
    ymin <- as.numeric(bb["ymin"])
    xmax <- as.numeric(bb["xmax"])
    ymax <- as.numeric(bb["ymax"])
  } else {
    stop("aoi must be a SpatRaster, SpatVector, sf object, or named list from define_aoi()")
  }
  c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
}

#' Download NOAA AORC Climate Data
#'
#' Downloads hourly AORC v1.1 climate data from NOAA's S3 bucket for a given
#' area of interest and time period. Data is downloaded variable by variable
#' to limit RAM usage and saved as NetCDF files organized by year and month.
#'
#' @details Requires the following Python packages to be installed in your
#'   conda environment: xarray, s3fs, zarr, netCDF4. These can be installed
#'   via conda: \code{conda install -c conda-forge xarray s3fs zarr netCDF4}
#'   If not found, the function will attempt to install them via
#'   \code{reticulate::py_install()}.
#'
#'   Downloaded files are returned in the native WGS84 (EPSG:4326) coordinate
#'   system of the AORC dataset. Reprojection to a different CRS is intentionally
#'   not performed here, as \code{estimate_diffuse_rad()} requires WGS84 to
#'   correctly compute solar geometry. Reprojection of all AORC and diffuse
#'   radiation outputs should be performed after \code{estimate_diffuse_rad()}
#'   has been run.
#'
#' @param aoi A SpatRaster, SpatVector, sf object, or named list returned by
#'   \code{define_aoi()}. Used to derive the bounding box for download
#' @param dates Vector of class Date specifying year-month combinations to process.
#'   The day component is ignored; only year and month are used.
#'   E.g. as.Date(c("2020-10-01", "2020-11-01"))
#' @param out_dir Base directory to save downloaded AORC files. Files are
#'   organized as \code{out_dir/study_area/year/month/} if study_area is
#'   provided, otherwise \code{out_dir/year/month/}
#' @param study_area Optional character string identifying the study area e.g. "GMU1".
#'   If provided, used as a prefix in output file names and added to directory
#'   structure
#' @param overwrite Logical. Whether to overwrite existing files. If FALSE
#'   existing files are skipped allowing the function to resume interrupted
#'   downloads. Default is FALSE
#' @param workers Integer. Number of parallel workers for month-level
#'   parallelization via \code{future.apply}. Default is 1 (sequential)
#' @param python_path Optional path to Python executable or conda environment.
#'   If NULL uses the currently configured Python environment. Default is NULL
#' @param retry_log Optional data frame returned by a previous call to
#'   \code{download_aorc()}. When provided, only year/month combinations that
#'   had at least one failed variable in that log are retried. Variables that
#'   already downloaded successfully are skipped via the \code{overwrite = FALSE}
#'   default. This allows resuming a partially failed run without re-downloading
#'   completed files
#'
#' @return Invisibly returns a data frame logging the status of each
#'   year/month/variable combination processed
#' @export
download_aorc <- function(aoi,
                         dates,
                         out_dir,
                         study_area = NULL,
                         overwrite = FALSE,
                         workers = 1,
                         python_path = NULL,
                         retry_log = NULL) {

  # Extract year and month from dates
  dates <- as.Date(dates)
  years <- lubridate::year(dates)
  months <- lubridate::month(dates)

  # Optionally set Python environment
  if (!is.null(python_path)) {
    reticulate::use_python(python_path, required = TRUE)
  }

  # Check and install required Python packages
  required_pkgs <- c("xarray", "s3fs", "zarr", "netCDF4")
  missing_pkgs  <- required_pkgs[!sapply(required_pkgs, reticulate::py_module_available)]

  if (length(missing_pkgs) > 0) {
    cat(sprintf("Installing missing Python packages: %s\n", paste(missing_pkgs, collapse = ", ")))
    reticulate::py_install(missing_pkgs)
  }

  # Extract extent from aoi in WGS84
  bb <- .aoi_to_wgs84_bbox(aoi)
  lon_min <- unname(bb["xmin"])
  lon_max <- unname(bb["xmax"])
  lat_min <- unname(bb["ymin"])
  lat_max <- unname(bb["ymax"])

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # Build list of specific year/month pairs from dates
  combos <- data.frame(year = years, month = months, stringsAsFactors = FALSE)

  # If a retry log is provided, restrict to combos that had failures
  if (!is.null(retry_log)) {
    if (!all(c("year", "month", "status") %in% names(retry_log))) {
      stop("retry_log must be a data frame with columns 'year', 'month', and 'status', as returned by download_aorc()")
    }
    failed_combos <- unique(retry_log[grepl("failed", retry_log$status), c("year", "month")])
    if (nrow(failed_combos) == 0) {
      cat("No failed combinations found in retry_log. Nothing to retry.\n")
      return(invisible(retry_log))
    }
    combos <- merge(combos, failed_combos, by = c("year", "month"))
    cat(sprintf("Retrying %d year/month combination(s) with previous failures.\n", nrow(combos)))
  }

  cat(sprintf("\n--- AORC Download: %d date combinations ---\n\n", nrow(combos)))

  # Set up parallel plan
  if (workers > 1) {
    future::plan(future::multisession, workers = workers)
    on.exit(future::plan(future::sequential), add = TRUE)
  }

  # Process each year/month combo
  results <- future.apply::future_lapply(seq_len(nrow(combos)), function(i) {

    y <- combos$year[i]
    m <- combos$month[i]

    combo_log <- data.frame(
      year     = integer(),
      month    = integer(),
      variable = character(),
      status   = character(),
      stringsAsFactors = FALSE
    )

    # Set Python environment in each worker
    if (!is.null(python_path)) {
      reticulate::use_python(python_path, required = TRUE)
    }

    # Re-import Python modules in each worker
    xr       <- reticulate::import("xarray")
    builtins <- reticulate::import("builtins")

    # Build month directory
    month_dir <- if (!is.null(study_area)) {
      file.path(out_dir, study_area, y, m)
    } else {
      file.path(out_dir, y, m)
    }
    dir.create(month_dir, recursive = TRUE, showWarnings = FALSE)

    start <- sprintf("%d-%02d-01", y, m)
    end   <- as.character(lubridate::ceiling_date(as.Date(start), unit = "month") - 1)

    # Open yearly Zarr store
    ds_url <- sprintf("https://noaa-nws-aorc-v1-1-1km.s3.amazonaws.com/%d.zarr", y)
    cat(sprintf("Opening %s\n", ds_url))

    ds <- tryCatch(
      xr$open_zarr(ds_url, consolidated = TRUE),
      error = function(e) {
        warning(sprintf("Failed to open Zarr store for year %d: %s", y, e$message))
        return(NULL)
      }
    )

    if (is.null(ds)) {
      combo_log <- rbind(combo_log, data.frame(
        year = y, month = m, variable = "ALL",
        status = "failed - could not open zarr",
        stringsAsFactors = FALSE
      ))
      return(combo_log)
    }

    vars <- reticulate::py_to_r(builtins$list(ds$data_vars$keys()))

    for (varname in vars) {

      # Build output file name
      out_file <- if (!is.null(study_area)) {
        file.path(month_dir, sprintf("AORC_%s_%d_%02d_%s.nc", study_area, y, m, varname))
      } else {
        file.path(month_dir, sprintf("AORC_%d_%02d_%s.nc", y, m, varname))
      }

      if (file.exists(out_file) && !overwrite) {
        cat(sprintf("  Skipping %s (already exists)\n", basename(out_file)))
        combo_log <- rbind(combo_log, data.frame(
          year = y, month = m, variable = varname, status = "skipped",
          stringsAsFactors = FALSE
        ))
        next
      }

      cat(sprintf("  Year %d, Month %02d, Variable: %s\n", y, m, varname))

      tryCatch({
        var        <- ds[[varname]]
        time_slice <- builtins$slice(start, end)
        lat_slice  <- builtins$slice(lat_min, lat_max)
        lon_slice  <- builtins$slice(lon_min, lon_max)

        sub_var <- var$sel(
          time      = time_slice,
          longitude = lon_slice,
          latitude  = lat_slice
        )

        sub_var$to_netcdf(out_file)
        cat(sprintf("    Saved %s\n", basename(out_file)))

        combo_log <- rbind(combo_log, data.frame(
          year = y, month = m, variable = varname, status = "success",
          stringsAsFactors = FALSE
        ))
      },
      error = function(e) {
        warning(sprintf("Failed %s: %s", basename(out_file), e$message))
        combo_log <<- rbind(combo_log, data.frame(
          year = y, month = m, variable = varname,
          status = paste("failed -", e$message),
          stringsAsFactors = FALSE
        ))
      })
    }

    return(combo_log)
  }, future.seed = TRUE)

  # Combine logs from all workers
  log <- do.call(rbind, results)

  # Summary
  n_success <- sum(log$status == "success",  na.rm = TRUE)
  n_skipped <- sum(log$status == "skipped",  na.rm = TRUE)
  n_failed  <- sum(grepl("failed", log$status), na.rm = TRUE)

  cat(sprintf("\nDone. %d downloaded, %d skipped, %d failed.\n",
              n_success, n_skipped, n_failed))

  if (n_failed > 0) {
    cat("Failed downloads:\n")
    print(log[grepl("failed", log$status), ])
  }

  return(invisible(log))
}

# service_url default and known-layer table for download_arcgis_landcover();
# all 4 Kenai layers share one underlying polygon schema (confirmed against
# the live service), so the table is only consulted when service_url matches
# this default -- a different service always requires an explicit numeric
# layer and field.
.arcgis_kenai_service_url <- "https://apps.fs.usda.gov/fsgisx02/rest/services/r10/KenaiVegVectorMapService2019/MapServer"

.arcgis_kenai_layer_table <- function() {
  data.frame(
    name        = c("DominanceType", "TreeCanopyCover", "TreeSizeClass", "TallShrubCanopyCover"),
    id          = c(0L, 1L, 2L, 3L),
    field       = c("DT_Code", "TreeCC", "TreeSize", "TallShrubCC"),
    label_field = c("Dominance_Type", NA_character_, NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
}

.arcgis_resolve_layer <- function(service_url, layer, field) {
  tbl <- .arcgis_kenai_layer_table()
  row <- if (identical(service_url, .arcgis_kenai_service_url)) {
    if (is.numeric(layer)) tbl[tbl$id == layer, ] else tbl[tbl$name == layer, ]
  } else {
    tbl[0, ]
  }

  if (nrow(row) == 1) {
    fld <- if (!is.null(field)) field else row$field
    # only attach the built-in descriptive label when the default field was kept
    label_field <- if (identical(fld, row$field)) row$label_field else NA_character_
    return(list(id = as.integer(row$id), field = fld, label_field = label_field))
  }

  if (is.null(field)) {
    stop(sprintf(
      "Unknown layer '%s' for service_url '%s'. Supply 'field' explicitly, or use one of the built-in Kenai layer names: %s",
      layer, service_url, paste(tbl$name, collapse = ", ")
    ))
  }
  if (!is.numeric(layer)) {
    stop("layer must be a numeric layer id when it is not one of the built-in Kenai layer names")
  }
  list(id = as.integer(layer), field = field, label_field = NA_character_)
}

.arcgis_aoi_bbox_sf <- function(aoi) {
  sf::st_as_sfc(sf::st_bbox(.aoi_to_wgs84_bbox(aoi), crs = sf::st_crs(4326)))
}

.arcgis_query_layer <- function(service_url, layer_id, fields, filter_geom) {
  furl <- arcgislayers::arc_open(sprintf("%s/%d", service_url, layer_id))
  arcgislayers::arc_select(furl, fields = fields, filter_geom = filter_geom)
}

# The template grid is built from query_extent (the AOI bbox), not from the
# extent of the returned features -- a single large polygon that merely
# touches the AOI (e.g. a lake or big forest stand) would otherwise blow the
# raster out well past the study area.
.arcgis_rasterize <- function(sf_result, field, res, query_extent, crop_template = NULL) {
  v <- terra::vect(sf_result)
  qext <- terra::ext(terra::project(terra::vect(query_extent), terra::crs(v)))
  template <- terra::rast(qext, resolution = res, crs = terra::crs(v))
  r <- terra::rasterize(v, template, field = field)

  if (!is.null(crop_template)) {
    r <- terra::crop(terra::project(r, terra::crs(crop_template)), crop_template)
  }
  r
}

# Attaches a descriptive label column (e.g. Dominance_Type) to a categorical
# raster's factor levels table, alongside the coded field it was rasterized
# from (e.g. DT_Code).
.arcgis_attach_label <- function(r, sf_result, field, label_field) {
  df <- unique(sf::st_drop_geometry(sf_result)[, c(field, label_field)])
  lv <- terra::levels(r)[[1]]
  lv <- merge(lv, df, by = field, all.x = TRUE)
  # merge() puts the join key first; set.cats() requires the numeric ID column first
  lv <- lv[order(lv$ID), c("ID", setdiff(names(lv), "ID"))]
  levels(r) <- lv  # terra registers an S3 method for base `levels<-`; it has no terra::-qualified export
  r
}

#' Download and Rasterize Landcover/Vegetation Polygons from an ArcGIS REST Feature Service
#'
#' Queries one or more layers of an ArcGIS REST MapServer/FeatureServer feature
#' service, clips to an area of interest, and rasterizes the result. Defaults
#' target the USDA Forest Service Kenai Peninsula vegetation map service, but
#' any ArcGIS REST feature service can be used by supplying \code{service_url},
#' a numeric \code{layer} id, and \code{field}.
#'
#' @details Despite being described as "raster layers" by the Kenai service
#'   metadata, the layers are polygon feature layers; this function queries
#'   the polygons via \code{arcgislayers} and rasterizes them with
#'   \code{terra::rasterize()}, which automatically produces a categorical
#'   (factor) raster when \code{field} is a character column, or a numeric
#'   raster otherwise.
#'
#'   Built-in Kenai layer names and their default fields:
#'   \code{"DominanceType"} (\code{DT_Code}), \code{"TreeCanopyCover"}
#'   (\code{TreeCC}), \code{"TreeSizeClass"} (\code{TreeSize}), and
#'   \code{"TallShrubCanopyCover"} (\code{TallShrubCC}). \code{field} can be
#'   used to override the default field for a built-in layer (e.g.
#'   \code{"Dominance_Type"} for the human-readable label instead of the
#'   code), and is required when \code{service_url} is not the built-in
#'   default or \code{layer} is not one of these names.
#'
#' @param aoi A SpatRaster, SpatVector, sf object, or named list returned by
#'   \code{define_aoi()}. Used to derive the bounding box for the query
#' @param out_dir Directory to save output raster files
#' @param service_url Base URL of an ArcGIS REST MapServer/FeatureServer.
#'   Default is the USDA Forest Service Kenai Peninsula vegetation map service
#' @param layer Character or numeric vector of layers to download. For the
#'   default \code{service_url}, one or more of \code{"DominanceType"},
#'   \code{"TreeCanopyCover"}, \code{"TreeSizeClass"},
#'   \code{"TallShrubCanopyCover"}, or their numeric layer ids. For any other
#'   service, numeric layer id(s). Default is \code{"DominanceType"}
#' @param field Character vector of attribute field(s) to rasterize, recycled
#'   against \code{layer}. \code{NULL} (default) uses the built-in Kenai
#'   default field for each recognized layer name; required for layers not in
#'   the built-in table
#' @param res Output raster resolution in meters, in the service's native CRS.
#'   Default is 30
#' @param crop_template Optional SpatRaster. If provided, each output raster
#'   is reprojected and cropped to match it, as in \code{NLCD_2_CORINE()} and
#'   \code{LandfireVegHght_AsNumeric()}
#' @param study_area Optional character string identifying the study area e.g. "Kenai1".
#'   If provided, used as a prefix in output file names
#' @param overwrite Logical. Whether to overwrite existing output files. If
#'   FALSE, existing files are skipped (no re-query). Default is FALSE
#'
#' @return Invisibly returns a list with \code{rasters} (a named list of
#'   SpatRaster objects, one per successfully processed layer) and \code{log}
#'   (a data frame logging the status of each requested layer)
#' @seealso \code{\link{NLCD_2_CORINE}}, \code{\link{LandfireVegHght_AsNumeric}}
#' @export
download_arcgis_landcover <- function(aoi,
                                       out_dir,
                                       service_url = "https://apps.fs.usda.gov/fsgisx02/rest/services/r10/KenaiVegVectorMapService2019/MapServer",
                                       layer = "DominanceType",
                                       field = NULL,
                                       res = 30,
                                       crop_template = NULL,
                                       study_area = NULL,
                                       overwrite = FALSE) {

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  filter_geom <- .arcgis_aoi_bbox_sf(aoi)
  prefix <- if (!is.null(study_area)) paste0(study_area, "_") else ""

  results <- list()
  log_rows <- vector("list", length(layer))

  for (i in seq_along(layer)) {
    lyr <- layer[[i]]
    lyr_label <- as.character(lyr)
    fld <- if (!is.null(field)) field[[min(i, length(field))]] else NULL
    out_file <- file.path(out_dir, sprintf("%s%s.tif", prefix, lyr_label))

    if (file.exists(out_file) && !overwrite) {
      cat(sprintf("Skipping %s (exists): %s\n", lyr_label, out_file))
      results[[lyr_label]] <- terra::rast(out_file)
      log_rows[[i]] <- data.frame(layer = lyr_label, n_features = NA_integer_,
                                   path = out_file, status = "skipped_existing",
                                   stringsAsFactors = FALSE)
      next
    }

    tryCatch({
      resolved <- .arcgis_resolve_layer(service_url, lyr, fld)
      fields_needed <- c(resolved$field, if (!is.na(resolved$label_field)) resolved$label_field)
      sf_result <- .arcgis_query_layer(service_url, resolved$id, fields_needed, filter_geom)

      if (nrow(sf_result) == 0) {
        warning(sprintf("No features returned for layer '%s' in this AOI; skipping.", lyr_label))
        log_rows[[i]] <- data.frame(layer = lyr_label, n_features = 0L,
                                     path = NA_character_, status = "no_features",
                                     stringsAsFactors = FALSE)
      } else {
        r <- .arcgis_rasterize(sf_result, resolved$field, res, filter_geom, crop_template)
        if (!is.na(resolved$label_field) && resolved$label_field %in% names(sf_result)) {
          r <- .arcgis_attach_label(r, sf_result, resolved$field, resolved$label_field)
        }
        terra::writeRaster(r, out_file, overwrite = TRUE)
        cat(sprintf("  Saved: %s (%d features)\n", out_file, nrow(sf_result)))

        results[[lyr_label]] <- r
        log_rows[[i]] <- data.frame(layer = lyr_label, n_features = nrow(sf_result),
                                     path = out_file, status = "ok",
                                     stringsAsFactors = FALSE)
      }
    }, error = function(e) {
      warning(sprintf("Layer '%s' failed: %s", lyr_label, conditionMessage(e)))
      log_rows[[i]] <<- data.frame(layer = lyr_label, n_features = NA_integer_,
                                    path = NA_character_,
                                    status = paste0("failed: ", conditionMessage(e)),
                                    stringsAsFactors = FALSE)
    })
  }

  return(invisible(list(rasters = results, log = do.call(rbind, log_rows))))
}

#' Estimate Diffuse Solar Radiation from AORC Shortwave Radiation
#'
#' Estimates hourly diffuse solar radiation from AORC downward shortwave
#' radiation (DSWRF) using the clearness index method. Solar geometry is
#' computed per latitude using the Michalsky method via the solaR package.
#' Timestamps are converted to mean solar time using the centroid longitude
#' of the AORC raster via \code{solaR::local2Solar()} to ensure correct
#' alignment of solar position calculations with local sunrise/sunset.
#' Output files are saved alongside their corresponding AORC DSWRF files.
#'
#' @details The diffuse fraction is estimated using the Erbs et al. (1982)
#'   piecewise model:
#'   \itemize{
#'     \item k <= 0: f = 0 (night)
#'     \item 0 < k <= 0.22: f = 1 - 0.09k
#'     \item 0.22 < k <= 0.80: f = 0.9511 - 0.1604k + 4.388k^2 - 16.638k^3 + 12.336k^4
#'     \item k > 0.80: f = 0.165
#'   }
#'
#'   Output files are returned in WGS84 (EPSG:4326) matching the native AORC
#'   coordinate system. This is intentional, reprojection prior to diffuse
#'   radiation estimation introduces empty edge cells due to projection warping
#'   which corrupts the solar geometry calculations. Reprojection of outputs
#'   should be performed after this function has been run.
#'
#'   Requires the \code{solaR} package available on CRAN and GitHub.
#'
#' @param dates Vector of class Date specifying year-month combinations to process.
#'   The day component is ignored; only year and month are used.
#'   E.g. as.Date(c("2020-10-01", "2020-11-01"))
#' @param aorc_dir Base directory containing AORC data organized as
#'   \code{aorc_dir/study_area/year/month/} or \code{aorc_dir/year/month/}
#' @param study_area Optional character string identifying the study area e.g. "GMU1".
#'   If provided, used to locate input files and prefix output file names
#' @param workers Integer. Number of parallel workers via \code{future.apply}.
#'   Default is 1 (sequential)
#'
#' @return Invisibly returns a data frame logging the status of each
#'   year/month combination processed
#' @export
estimate_diffuse_rad <- function(dates,
                                aorc_dir,
                                study_area = NULL,
                                workers = 1) {

  # Extract year and month from dates
  dates <- as.Date(dates)
  years <- lubridate::year(dates)
  months <- lubridate::month(dates)

  # Build list of all year/month combos
  combos <- data.frame(year = years, month = months, stringsAsFactors = FALSE)

  cat(sprintf("\n--- Diffuse Radiation Estimation: %d date combinations ---\n\n", nrow(combos)))

  # Set up parallel plan
  if (workers > 1) {
    future::plan(future::multisession, workers = workers)
    on.exit(future::plan(future::sequential), add = TRUE)
  }

  results <- future.apply::future_lapply(seq_len(nrow(combos)), function(i) {

    y <- combos$year[i]
    m <- combos$month[i]

    cat(sprintf("Working on %d %02d\n", y, m))

    # Build input directory
    month_dir <- if (!is.null(study_area)) {
      file.path(aorc_dir, study_area, y, m)
    } else {
      file.path(aorc_dir, y, m)
    }

    # Build DSWRF file path
    dswrf_file <- if (!is.null(study_area)) {
      file.path(month_dir, sprintf("AORC_%s_%d_%02d_DSWRF_surface.nc",
                                   study_area, y, m))
    } else {
      file.path(month_dir, sprintf("AORC_%d_%02d_DSWRF_surface.nc", y, m))
    }

    # Flag missing files and continue
    if (!file.exists(dswrf_file)) {
      warning(sprintf("DSWRF file not found for %d-%02d, skipping:\n  %s",
                      y, m, dswrf_file))
      return(data.frame(year = y, month = m,
                        status = "skipped - file not found",
                        stringsAsFactors = FALSE))
    }

    tryCatch({

      # Save and restore system timezone to avoid side effects
      old_tz <- Sys.getenv("TZ")
      Sys.setenv(TZ = "UTC")
      on.exit(Sys.setenv(TZ = old_tz), add = TRUE)

      AORC <- terra::rast(dswrf_file)

      ## Time handling
      AORC_time <- terra::time(AORC)
      attr(AORC_time, "tzone") <- "UTC"
      BTi_local <- lubridate::with_tz(AORC_time, "America/Denver")

      ## Extract centroid longitude from raster
      lon <- terra::xFromCol(AORC, col = round(terra::ncol(AORC) / 2))

      ## Convert to mean solar time using centroid longitude
      BTi <- solaR::local2Solar(BTi_local, lon = lon)

      ## Daily sequence for fSolD in solar time
      BTd <- as.POSIXct(unique(lubridate::date(BTi)), tz = "UTC")

      ## Latitude per cell
      coords      <- terra::crds(AORC, df = FALSE)
      lat_vals    <- coords[, 2]
      unique_lats <- sort(unique(round(lat_vals, 4)))
      lat_index   <- match(round(lat_vals, 4), unique_lats)

      ## Bo0 matrix [lat x time]
      n_lat   <- length(unique_lats)
      n_time  <- length(BTi)
      Bo0_mat <- matrix(NA_real_, n_lat, n_time)

      for (j in seq_len(n_lat)) {
        solD <- solaR::fSolD(
          lat    = unique_lats[j],
          BTd    = BTd,
          method = "michalsky"
        )
        solI <- solaR::fSolI(
          solD       = solD,
          BTi        = BTi,
          sample     = "hour",
          EoT        = TRUE,
          keep.night = TRUE
        )
        Bo0_mat[j, ] <- zoo::coredata(solI$Bo0)
      }

      ## Build Bo0 raster stack
      Bo_rast    <- terra::rast(AORC)
      bo0_values <- Bo0_mat[lat_index, ]
      terra::values(Bo_rast) <- bo0_values

      ## Clearness index
      k.out <- AORC / Bo_rast
      k.out[is.na(k.out)] <- 0

      ## Diffuse fraction function
      Diff.frac.calc <- function(k) {
        f <- numeric(length(k))
        f[k <= 0]               <- 0
        f[k > 0 & k <= 0.22]   <- 1 - 0.09 * k[k > 0 & k <= 0.22]
        f[k > 0.22 & k <= 0.8] <- 0.9511 - 0.1604 * k[k > 0.22 & k <= 0.8] +
          4.388  * k[k > 0.22 & k <= 0.8]^2 -
          16.638 * k[k > 0.22 & k <= 0.8]^3 +
          12.336 * k[k > 0.22 & k <= 0.8]^4
        f[k > 0.8]              <- 0.165
        f <- pmin(pmax(f, 0), 1)
        return(f)
      }

      ## Apply diffuse fraction
      f.out    <- terra::app(k.out, Diff.frac.calc)
      Diff.out <- f.out * AORC
      terra::time(Diff.out) <- terra::time(AORC)
      names(Diff.out) <- sprintf("DiffuseRad_%s",
                                 format(terra::time(Diff.out), "%Y%m%d_%H"))

      ## Write output alongside DSWRF file
      out_file <- if (!is.null(study_area)) {
        file.path(month_dir, sprintf("SolaR_%s_%d_%02d_DifRad_surface.tif",
                                     study_area, y, m))
      } else {
        file.path(month_dir, sprintf("SolaR_%d_%02d_DifRad_surface.tif", y, m))
      }

      terra::writeRaster(Diff.out, out_file, overwrite = TRUE)
      cat(sprintf("  Saved: %s\n", basename(out_file)))

      rm(AORC, Bo_rast, Diff.out, Bo0_mat)
      gc()

      return(data.frame(year = y, month = m, status = "success",
                        stringsAsFactors = FALSE))

    }, error = function(e) {
      warning(sprintf("Failed %d-%02d: %s", y, m, e$message))
      return(data.frame(year = y, month = m,
                        status = paste("failed -", e$message),
                        stringsAsFactors = FALSE))
    })

  }, future.seed = TRUE)

  # Combine logs
  log <- do.call(rbind, results)

  # Summary
  n_success <- sum(log$status == "success",      na.rm = TRUE)
  n_skipped <- sum(grepl("skipped", log$status), na.rm = TRUE)
  n_failed  <- sum(grepl("failed",  log$status), na.rm = TRUE)

  cat(sprintf("\nDone. %d processed, %d skipped, %d failed.\n",
              n_success, n_skipped, n_failed))

  if (n_failed > 0) {
    cat("Failed combinations:\n")
    print(log[grepl("failed", log$status), ])
  }

  return(invisible(log))
}

.raster_has_na <- function(r) {
  # terra::app() vectorizes across cells (not per-cell) when nlyr == 1, so an
  # any(is.na(v)) reducer only behaves correctly for nlyr > 1 -- handle the
  # single-layer case directly instead.
  m <- if (terra::nlyr(r) == 1) {
    is.na(r) * 1
  } else {
    terra::app(r, fun = function(v) as.numeric(any(is.na(v))))
  }
  names(m) <- "na"
  m
}

#' Check for unexpected NA values in a list of packed SpatRasters
#'
#' Iterates over every packed SpatRaster in a list (and every layer within
#' each one), flags NA cells, and cross-references them against a landcover
#' raster's water class so you can tell NAs caused by water from NAs caused
#' by something else (truncated I/O, a failed model cell, edge effects, etc).
#'
#' NA cells are split into three buckets per raster/layer:
#'   - n_na_water          : NA in the layer, and landcover == water_value there
#'   - n_na_landcover_na   : NA in the layer, but landcover itself is NA there
#'                           (ambiguous -- can't confirm water, flagged separately
#'                           so it isn't silently swept into "unexplained")
#'   - n_na_unexplained    : NA in the layer, landcover is known and != water
#'                           (this is the "real problem" bucket)
#'
#' @param packed_list list of PackedSpatRaster (output of terra::wrap()).
#'   Plain SpatRaster elements are tolerated too. Optionally named -- those
#'   names are used in the report.
#' @param landcover SpatRaster or PackedSpatRaster of landcover classes.
#' @param water_value numeric. Landcover class value representing water.
#'   Default 80.
#' @param names_list optional character vector of names for packed_list,
#'   used if packed_list itself isn't named.
#' @param check_geom logical. If TRUE (default), checks each raster shares
#'   the landcover's grid (extent/res/crs) before comparing, and resamples
#'   the water mask (nearest neighbor) if it doesn't -- with a message.
#' @param verbose logical. Print progress as it goes.
#'
#' @return a list with:
#'   summary  - data.frame, one row per raster x layer with NA counts/pct
#'   flagged  - named list (raster :: layer) of cell numbers for combos with
#'              unexplained NA cells, for follow-up (e.g. terra::xyFromCell())
#'
#' @examples
#' \dontrun{
#' out <- check_na_vs_water(packed_results, landcover, water_value = 80)
#' out$summary
#' subset(out$summary, n_na_unexplained > 0)
#' out$flagged[["run_04 :: Tmin"]]
#' }
#' @export
check_na_vs_water <- function(packed_list,
                              landcover,
                              water_value = 80,
                              names_list = NULL,
                              check_geom = TRUE,
                              verbose = TRUE) {

  stopifnot(is.list(packed_list), length(packed_list) > 0)

  if (inherits(landcover, "PackedSpatRaster")) {
    landcover <- terra::unwrap(landcover)
  }
  stopifnot(inherits(landcover, "SpatRaster"))

  water_mask <- landcover == water_value
  names(water_mask) <- "water"

  if (is.null(names_list)) {
    names_list <- names(packed_list)
    if (is.null(names_list)) names_list <- paste0("raster_", seq_along(packed_list))
  }
  names_list[names_list == ""] <- paste0("raster_", seq_along(packed_list))[names_list == ""]
  stopifnot(length(names_list) == length(packed_list))

  summary_rows <- list()
  flagged <- list()

  for (i in seq_along(packed_list)) {

    item <- packed_list[[i]]
    r <- if (inherits(item, "PackedSpatRaster")) terra::unwrap(item) else item
    stopifnot(inherits(r, "SpatRaster"))

    item_name <- names_list[i]
    nl <- terra::nlyr(r)

    if (verbose) {
      message(sprintf("[%d/%d] %s -- %d layer(s)", i, length(packed_list), item_name, nl))
    }

    wm <- water_mask
    if (check_geom && !isTRUE(terra::compareGeom(r, wm, stopOnError = FALSE))) {
      if (verbose) {
        message(sprintf("  grid mismatch for %s -- resampling water mask to match (nearest neighbor)", item_name))
      }
      wm <- terra::resample(wm, r, method = "near")
    }

    lyr_names <- names(r)
    if (is.null(lyr_names) || any(lyr_names == "")) lyr_names <- paste0("layer_", seq_len(nl))

    for (j in seq_len(nl)) {

      lyr <- r[[j]]
      lyr_name <- lyr_names[j]

      na_mask <- is.na(lyr)                    # TRUE/FALSE, never NA itself
      water_na_mask <- na_mask & wm             # NA in layer AND water
      unk_lc_mask <- na_mask & is.na(wm)         # NA in layer, landcover unknown there
      unexplained_mask <- na_mask & !wm          # NA in layer, landcover known & != water

      n_cells <- terra::ncell(lyr)
      n_na <- as.numeric(terra::global(na_mask, "sum", na.rm = TRUE)[1, 1])
      n_na_water <- as.numeric(terra::global(water_na_mask, "sum", na.rm = TRUE)[1, 1])
      n_na_lc_unk <- as.numeric(terra::global(unk_lc_mask, "sum", na.rm = TRUE)[1, 1])
      n_na_unexplained <- as.numeric(terra::global(unexplained_mask, "sum", na.rm = TRUE)[1, 1])

      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        raster = item_name,
        layer = lyr_name,
        n_cells = n_cells,
        n_na = n_na,
        n_na_water = n_na_water,
        n_na_landcover_na = n_na_lc_unk,
        n_na_unexplained = n_na_unexplained,
        pct_unexplained = if (n_cells > 0) 100 * n_na_unexplained / n_cells else NA_real_,
        stringsAsFactors = FALSE
      )

      if (n_na_unexplained > 0) {
        key <- paste(item_name, lyr_name, sep = " :: ")
        vals <- terra::values(unexplained_mask, mat = FALSE)
        flagged[[key]] <- which(vals == 1)
        if (verbose) {
          message(sprintf("  ! %s -- %d unexplained NA cell(s)", lyr_name, n_na_unexplained))
        }
      }
    }
  }

  summary_df <- do.call(rbind, summary_rows)
  rownames(summary_df) <- NULL

  if (verbose) {
    n_flagged_combos <- sum(summary_df$n_na_unexplained > 0)
    message(sprintf("Done. %d of %d raster/layer combination(s) have unexplained NA cells.",
                    n_flagged_combos, nrow(summary_df)))
  }

  list(summary = summary_df, flagged = flagged)
}

#' Package Vegetation and Soil Parameters for Microclimate Modeling
#'
#' Iterates over date ranges, assembling land cover, vegetation height, LAI, soil
#' data and reflectance into vegetation and soil parameter grids using
#' \code{microclimdata::create_veggrid()} and
#' \code{microclimdata::create_soilgrid()}. Reflectance data is averaged
#' across user specified snow free months. Outputs are saved as RDS files.
#'
#' @details After the inputs are aligned to a common grid, any outer ring of
#'   NA cells present in \emph{any} of the aligned inputs (land cover, veg
#'   height, soil, ground/leaf reflectance, LAI) is trimmed via
#'   \code{\link[terra]{trim}} before building the vegetation/soil grids --
#'   otherwise \code{create_veggrid()}/\code{create_soilgrid()} mask to the
#'   intersection of all inputs and that edge NA would propagate into every
#'   output layer. After each grid is built, \code{\link{check_na_vs_water}}
#'   cross-references its NA cells against \code{landcover == water} and
#'   issues a \code{warning()} if any are unexplained by water; the counts
#'   are also recorded in the returned log (see \code{@return}).
#'
#' @param dates A data.frame with columns \code{Start_Dates} and \code{End_Dates},
#'   where each row defines a date range to process. Alternatively, a vector of
#'   length 2 with \code{as.Date()} values where first is start date and last is
#'   end date. The day component is ignored; all months between start and end
#'   (inclusive) are processed in chronological order.
#'   E.g. data.frame with columns Start_Dates and End_Dates, or as.Date(c("2020-10-01", "2021-03-01"))
#' @param snow_free_months Integer vector of months to use for reflectance
#'   averaging e.g. 6:8
#' @param landcover Either (a) a directory path containing annual land cover
#'   files named with a 4-digit year (one file per year, matched via
#'   \code{.extract_year()}), (b) a path to a single land cover file, or (c) a
#'   \code{SpatRaster}. Cases (b) and (c) are static mode -- the same land cover
#'   is reused across all periods without per-year indexing
#' @param veg_height Either (a) a directory path containing annual vegetation
#'   height files named with a 4-digit year (one file per year, matched via
#'   \code{.extract_year()}), (b) a path to a single vegetation height file,
#'   or (c) a \code{SpatRaster}. Cases (b) and (c) are static mode -- the same
#'   raster is reused across all periods without per-year indexing
#' @param soil_path File path to the fine resolution soil data raster
#' @param lai_dir Directory containing fine resolution LAI files
#' @param refl_dir Directory containing leaf and ground reflectance files
#'   organized as \code{refl_dir/Lref/} and \code{refl_dir/Gref/}
#' @param vegpara_dir Directory to save vegetation parameter RDS outputs
#' @param soilpara_dir Directory to save soil parameter RDS outputs
#' @param study_area Optional character string identifying the study area e.g. "GMU1".
#'   If provided, used to filter input files and prefix output file names
#' @param lctype Land cover classification type passed to
#'   \code{microclimdata::create_veggrid()} and
#'   \code{microclimdata::create_soilgrid()}. Must be either "CORINE" or "ESA".
#'   Default is "CORINE"
#' @param water Integer. Land cover code for water bodies passed to
#'   \code{microclimdata::create_soilgrid()}. Default is 512
#' @param dtm_fine Optional \code{SpatRaster}. Fine-resolution DEM (e.g. the
#'   \code{dtm_fine} passed to \code{\link{run_micro_big_nichemap}}) used only
#'   for validation: if supplied, a warning is issued when the packaged
#'   vegetation/soil grid's resolution, CRS, or origin does not match it. Default
#'   \code{NULL} skips this check.
#'
#' @return Invisibly returns a data frame logging the status of each date
#'   range processed, with columns \code{period}, \code{status},
#'   \code{na_unexplained_veg}, and \code{na_unexplained_soil} (the last two
#'   are \code{NA} for failed periods; see \code{@details})
#' @seealso \code{\link{check_na_vs_water}}
#' @export
package_veg_soil <- function(dates,
                             snow_free_months,
                             landcover,
                             veg_height,
                             soil_path,
                             lai_dir,
                             refl_dir,
                             vegpara_dir,
                             soilpara_dir,
                             study_area = NULL,
                             lctype = "CORINE",
                             water = 512,
                             dtm_fine = NULL) {

  # Process dates input
  if (is.data.frame(dates)) {
    if (!all(c("Start_Dates", "End_Dates") %in% names(dates))) {
      stop("dates data.frame must contain columns 'Start_Dates' and 'End_Dates'")
    }
    date_ranges <- dates
  } else if (is.vector(dates) && length(dates) == 2) {
    date_ranges <- data.frame(
      Start_Dates = as.Date(dates[1]),
      End_Dates = as.Date(dates[2]),
      stringsAsFactors = FALSE
    )
  } else {
    stop("dates must be either a data.frame with Start_Dates and End_Dates columns, or a vector of length 2")
  }

  # Validate lctype
  if (!lctype %in% c("CORINE", "ESA")) {
    stop("lctype must be either 'CORINE' or 'ESA'")
  }

  dir.create(vegpara_dir,  recursive = TRUE, showWarnings = FALSE)
  dir.create(soilpara_dir, recursive = TRUE, showWarnings = FALSE)

  is_static <- inherits(landcover, "SpatRaster") ||
    (is.character(landcover) && file.exists(landcover) && !dir.exists(landcover))

  lc_static <- if (is_static) {
    if (inherits(landcover, "SpatRaster")) landcover else terra::rast(landcover)
  } else NULL

  is_static_vh <- inherits(veg_height, "SpatRaster") ||
    (is.character(veg_height) && file.exists(veg_height) && !dir.exists(veg_height))

  vh_static <- if (is_static_vh) {
    if (inherits(veg_height, "SpatRaster")) veg_height else terra::rast(veg_height)
  } else NULL

  # Load soil data once - does not change by date range
  if (!file.exists(soil_path)) stop(sprintf("Soil file not found:\n  %s", soil_path))
  SD <- terra::rast(soil_path)

  log <- data.frame(
    period = character(0),
    status = character(0),
    na_unexplained_veg = integer(0),
    na_unexplained_soil = integer(0),
    stringsAsFactors = FALSE
  )

  cat(sprintf("\n--- Vegetation and Soil Parameter Packaging: %d period(s) ---\n\n",
              nrow(date_ranges)))

  for (i in seq_len(nrow(date_ranges))) {

    start_date <- as.Date(date_ranges$Start_Dates[i])
    end_date <- as.Date(date_ranges$End_Dates[i])
    period_label <- sprintf("%s_to_%s",
                            format(start_date, "%Y%m%d"),
                            format(end_date, "%Y%m%d"))

    cat(sprintf("Processing period: %s\n", period_label))

    tryCatch({

      # Generate month sequence for this period
      month_seq <- .generate_month_sequence(start_date, end_date)

      # List input files
      lai_files <- list.files(lai_dir, pattern = "\\.tif$", full.names = TRUE)
      if (!is.null(study_area)) lai_files <- lai_files[grepl(study_area, lai_files)]
      lai_df <- .extract_ym(lai_files)

      # Collect all unique years in the period
      years_in_period <- unique(month_seq$year)
      first_year      <- years_in_period[1]

      if (is_static) {
        lc <- lc_static
      } else {
        lc_files <- list.files(landcover, pattern = "\\.tif$", full.names = TRUE)
        if (!is.null(study_area)) lc_files <- lc_files[grepl(study_area, lc_files)]
        lc_df <- .extract_year(lc_files)
        for (y in years_in_period) {
          if (!(y %in% lc_df$year)) stop(sprintf("Land cover file not found for year %d", y))
        }
        lc_match <- lc_df$file[lc_df$year == first_year]
        lc <- terra::rast(lc_match)
      }

      if (is_static_vh) {
        vght <- vh_static
      } else {
        vh_files <- list.files(veg_height, pattern = "\\.tif$", full.names = TRUE)
        if (!is.null(study_area)) vh_files <- vh_files[grepl(study_area, vh_files)]
        vh_df <- .extract_year(vh_files)
        for (y in years_in_period) {
          if (!(y %in% vh_df$year)) stop(sprintf("Vegetation height file not found for year %d", y))
        }
        vh_match <- vh_df$file[vh_df$year == first_year]
        vght <- terra::rast(vh_match)
      }

      # Load LAI files for all months in the period
      yms_needed <- sprintf("%d_%02d", month_seq$year, month_seq$month)
      lai_match <- lai_df$file[lai_df$ym %in% yms_needed]
      lai_match <- lai_match[order(match(
        regmatches(lai_match, regexpr("[0-9]{4}_[0-9]{2}", lai_match)), yms_needed
      ))]

      if (length(lai_match) != length(yms_needed)) {
        stop(sprintf("Expected %d LAI files for period, found %d",
                     length(yms_needed), length(lai_match)))
      }

      lai <- terra::rast(lai_match)

      # Collect reflectance files for snow-free months across all years
      sf_pattern <- paste(sprintf("%02d", snow_free_months), collapse = "|")
      lf_files <- list.files(file.path(refl_dir, "Lref"),
                             pattern = "\\.tif$", full.names = TRUE)
      gf_files <- list.files(file.path(refl_dir, "Gref"),
                             pattern = "\\.tif$", full.names = TRUE)

      if (!is.null(study_area)) {
        lf_files <- lf_files[grepl(study_area, lf_files)]
        gf_files <- gf_files[grepl(study_area, gf_files)]
      }

      # Filter to years in this period and snow-free months
      lf_files <- lf_files[
        (grepl(sprintf("_(%s)[^0-9]", paste(years_in_period, collapse = "|")), lf_files)) &
        grepl(sf_pattern, lf_files)
      ]
      gf_files <- gf_files[
        (grepl(sprintf("_(%s)[^0-9]", paste(years_in_period, collapse = "|")), gf_files)) &
        grepl(sf_pattern, gf_files)
      ]

      if (length(lf_files) == 0) stop("No leaf reflectance files found for period")
      if (length(gf_files) == 0) stop("No ground reflectance files found for period")

      refldata <- list(
        gref = terra::mean(terra::rast(gf_files)),
        lref = terra::mean(terra::rast(lf_files))
      )

      gc()

      # --- Align geometries ---
      all_rasters <- c(list(lai = lai, lc = lc, vght = vght, SD = SD), refldata)

      if (!terra::compareGeom(lai,
                              list(SD, vght, refldata$gref, refldata$lref, lc),
                              stopOnError = FALSE)) {

        cat("  Geometries differ: cropping and resampling to common extent...\n")

        common_ext <- Reduce(terra::intersect, lapply(all_rasters, terra::ext))
        template   <- terra::crop(lai, common_ext)

        lai_rs  <- terra::resample(terra::crop(lai,  common_ext), template, method = "bilinear")
        vght_rs <- terra::resample(terra::crop(vght, common_ext), template, method = "bilinear")
        SD_rs   <- terra::resample(terra::crop(SD,   common_ext), template, method = "bilinear")
        lc_rs   <- terra::resample(terra::crop(lc,   common_ext), template, method = "near")

        refldata_rs <- lapply(refldata, function(r) {
          terra::resample(terra::crop(r, common_ext), template, method = "bilinear")
        })

      } else {

        cat("  Geometries match: no resampling needed.\n")
        lai_rs      <- lai
        vght_rs     <- vght
        SD_rs       <- SD
        lc_rs       <- lc
        refldata_rs <- refldata

      }

      if (!is.null(dtm_fine)) {
        .check_grid_match(dtm_fine, lai_rs, "dtm_fine",
                          sprintf("packaged veg/soil (%s)", period_label), action = "warn")
      }

      gc()

      # --- Trim NA edge cells ---
      # Even when geometries already match, individual inputs can carry their
      # own edge no-data ring (e.g. from GEE export tiling or a slightly
      # smaller source extent). create_veggrid()/create_soilgrid() mask to the
      # intersection of all inputs, so any edge NA propagates into every
      # output layer unless it's cropped away first.
      cat("  Trimming NA edge cells...\n")

      combined_na <- .raster_has_na(lc_rs) + .raster_has_na(vght_rs) +
        .raster_has_na(SD_rs) + .raster_has_na(refldata_rs$gref) +
        .raster_has_na(refldata_rs$lref) + .raster_has_na(lai_rs)

      valid_mask <- terra::ifel(combined_na > 0, NA, 1)
      trim_ext   <- terra::ext(terra::trim(valid_mask))

      lai_rs      <- terra::crop(lai_rs,  trim_ext)
      vght_rs     <- terra::crop(vght_rs, trim_ext)
      SD_rs       <- terra::crop(SD_rs,   trim_ext)
      lc_rs       <- terra::crop(lc_rs,   trim_ext)
      refldata_rs <- lapply(refldata_rs, terra::crop, y = trim_ext)

      rm(combined_na, valid_mask)
      gc()

      # --- Name LAI layers by month ---
      names(lai_rs) <- sprintf("month_%04d%02d", month_seq$year, month_seq$month)

      # --- Create vegetation parameter grid ---
      cat("  Building vegetation parameter grids...\n")

      vegp.list <- lapply(seq_len(terra::nlyr(lai_rs)), function(x) {
        microclimdata::create_veggrid(
          landcover = lc_rs,
          vhgt      = vght_rs,
          lai       = lai_rs[[x]],
          refldata  = refldata_rs,
          lctype    = lctype
        )
      })

      vegp <- vegp.list[[1]]

      if (terra::nlyr(lai_rs) > 1) {
        for (j in 2:terra::nlyr(lai_rs)) {
          pai   <- vegp.list[[j]]$pai
          clump <- vegp.list[[j]]$clump
          vegp$pai   <- terra::wrap(c(terra::unwrap(vegp$pai),   terra::unwrap(pai)))
          vegp$clump <- terra::wrap(c(terra::unwrap(vegp$clump), terra::unwrap(clump)))
        }
      }

      gc()

      # --- QC: check vegetation parameters for unexplained NA ---
      veg_na_check <- check_na_vs_water(vegp, landcover = lc_rs,
                                        water_value = water, verbose = FALSE)
      n_na_veg <- sum(veg_na_check$summary$n_na_unexplained)
      if (n_na_veg > 0) {
        warning(sprintf(
          "Period %s: %d unexplained NA cell(s) in vegetation parameters (not water) -- see check_na_vs_water() for details",
          period_label, n_na_veg))
      }

      # --- Save vegetation parameters ---
      veg_out <- file.path(vegpara_dir, if (!is.null(study_area)) {
        sprintf("%s_VegPara_%s.RDS", study_area, period_label)
      } else {
        sprintf("VegPara_%s.RDS", period_label)
      })
      readr::write_rds(vegp, file = veg_out)
      cat(sprintf("  Saved: %s\n", basename(veg_out)))

      gc()

      # --- Create soil parameter grid ---
      cat("  Building soil parameter grid...\n")

      soilc <- microclimdata::create_soilgrid(
        soildata  = SD_rs,
        refldata  = refldata_rs,
        landcover = lc_rs,
        water     = water
      )

      # --- QC: check soil parameters for unexplained NA ---
      soil_na_check <- check_na_vs_water(soilc, landcover = lc_rs,
                                         water_value = water, verbose = FALSE)
      n_na_soil <- sum(soil_na_check$summary$n_na_unexplained)
      if (n_na_soil > 0) {
        warning(sprintf(
          "Period %s: %d unexplained NA cell(s) in soil parameters (not water) -- see check_na_vs_water() for details",
          period_label, n_na_soil))
      }

      soil_out <- file.path(soilpara_dir, if (!is.null(study_area)) {
        sprintf("%s_SoilPara_%s.RDS", study_area, period_label)
      } else {
        sprintf("SoilPara_%s.RDS", period_label)
      })
      readr::write_rds(soilc, file = soil_out)
      cat(sprintf("  Saved: %s\n", basename(soil_out)))

      log <- rbind(log, data.frame(period = period_label, status = "success",
                                   na_unexplained_veg = n_na_veg,
                                   na_unexplained_soil = n_na_soil,
                                   stringsAsFactors = FALSE))

    }, error = function(e) {
      warning(sprintf("Failed period %s: %s", period_label, e$message))
      log <<- rbind(log, data.frame(period = period_label,
                                    status = paste("failed -", e$message),
                                    na_unexplained_veg = NA_integer_,
                                    na_unexplained_soil = NA_integer_,
                                    stringsAsFactors = FALSE))
    })

    gc()
  }

  cat(sprintf("\nDone. %d/%d periods processed successfully.\n",
              sum(log$status == "success", na.rm = TRUE), nrow(log)))

  return(invisible(log))
}

#' Compute Multi-Year Climate Normals from AORC and Diffuse Radiation Data
#'
#' For each month in the provided date vector, stacks a given hour across all
#' available years and computes user-specified summary statistics. Output is
#' one multi-layer GeoTIFF per variable per month per statistic, with layers
#' ordered chronologically representing each hour of the month. The representative
#' hour sequence is derived from the most recent non-leap year.
#'
#' @details AORC variables processed are: APCP_surface, DLWRF_surface,
#'   DSWRF_surface, PRES_surface, SPFH_2maboveground, TMP_2maboveground,
#'   UGRD_10maboveground, VGRD_10maboveground, and DifRad_surface.
#'   Input files must follow the directory structure produced by
#'   \code{download_aorc()} and \code{estimate_diffuse_rad()}.
#'   Missing files for individual years are skipped and logged.
#'
#' @param dates Vector of class Date specifying year-month combinations to process.
#'   The day component is ignored; only the unique months and years are used.
#'   E.g. as.Date(c("2020-03-01", "2021-03-01", "2022-03-01")) to use March
#'   across years 2020-2022.
#' @param aorc_dir Base directory containing AORC data organized as
#'   \code{aorc_dir/study_area/year/month/} or \code{aorc_dir/year/month/}
#' @param out_dir Base directory for output files organized as
#'   \code{out_dir/month/}
#' @param stats Character vector of summary statistics to compute. Must include
#'   at least one of: "mean", "median", "mode", "sd", "min", "max"
#' @param study_area Optional character string identifying the study area e.g. "GMU1".
#'   If provided, used to locate input files and prefix output file names
#' @param workers Integer. Number of parallel workers via \code{future.apply}.
#'   Default is 1 (sequential)
#'
#' @return Invisibly returns a data frame logging the status of each
#'   month/variable combination processed
#' @export
summarize_climate_normals <- function(dates,
                                     aorc_dir,
                                     out_dir,
                                     stats = c("mean", "median", "mode", "sd", "min", "max"),
                                     study_area = NULL,
                                     workers = 1) {

  # Extract year and month from dates
  dates <- as.Date(dates)
  years <- unique(lubridate::year(dates))
  months <- unique(lubridate::month(dates))

  # Validate stats
  valid_stats <- c("mean", "median", "mode", "sd", "min", "max")
  invalid     <- stats[!stats %in% valid_stats]
  if (length(stats) == 0) stop("At least one stat must be specified.")
  if (length(invalid) > 0) {
    stop(sprintf("Invalid stat(s): %s. Must be one of: %s",
                 paste(invalid, collapse = ", "),
                 paste(valid_stats, collapse = ", ")))
  }

  # AORC variables
  aorc_vars <- c("APCP_surface", "DLWRF_surface", "DSWRF_surface",
                 "PRES_surface", "SPFH_2maboveground", "TMP_2maboveground",
                 "UGRD_10maboveground", "VGRD_10maboveground")

  # Find most recent non-leap year for reference hour sequence
  ref_year <- max(years[!lubridate::leap_year(years)])
  cat(sprintf("Using %d as reference year for hour sequence.\n", ref_year))

  # Build month/variable combos
  combos <- expand.grid(
    month = months,
    var   = c(aorc_vars, "DifRad_surface"),
    stringsAsFactors = FALSE
  )

  # Build log
  log <- data.frame(
    month    = combos$month,
    variable = combos$var,
    status   = NA_character_,
    stringsAsFactors = FALSE
  )

  cat(sprintf("\n--- Climate Normals: %d year(s), %d unique month(s), %d month/variable combos ---\n\n",
              length(years), length(months), nrow(combos)))

  # Mode helper
  rast_mode <- function(x) {
    ux <- unique(x[!is.na(x)])
    if (length(ux) == 0) return(NA_real_)
    ux[which.max(tabulate(match(x, ux)))]
  }

  # Set up parallel plan
  if (workers > 1) {
    future::plan(future::multisession, workers = workers)
    on.exit(future::plan(future::sequential), add = TRUE)
  }

  results <- future.apply::future_lapply(seq_len(nrow(combos)), function(i) {

    m   <- combos$month[i]
    var <- combos$var[i]

    is_difrad <- var == "DifRad_surface"

    cat(sprintf("Processing month: %02d | variable: %s\n", m, var))

    # Output directory
    month_out <- file.path(out_dir, sprintf("%02d", m))
    dir.create(month_out, recursive = TRUE, showWarnings = FALSE)

    # Reference hour sequence for this month
    ref_start <- as.POSIXct(sprintf("%d-%02d-01 00:00:00", ref_year, m),
                            tz = "UTC")
    ref_end   <- lubridate::add_with_rollback(ref_start, months(1), roll_to_first = T) -
      lubridate::hours(1)

    dtime     <- seq.POSIXt(ref_start, ref_end, by = "1 hour")

    # Missing file log
    missing_log <- character(0)

    # For each reference hour, stack across years
    hour_means   <- if ("mean"   %in% stats) vector("list", length(dtime)) else NULL
    hour_medians <- if ("median" %in% stats) vector("list", length(dtime)) else NULL
    hour_modes   <- if ("mode"   %in% stats) vector("list", length(dtime)) else NULL
    hour_sds     <- if ("sd"     %in% stats) vector("list", length(dtime)) else NULL
    hour_mins    <- if ("min"    %in% stats) vector("list", length(dtime)) else NULL
    hour_maxs    <- if ("max"    %in% stats) vector("list", length(dtime)) else NULL

    for (t_idx in seq_along(dtime)) {

      t <- dtime[t_idx]

      # Build file pattern for this month across all years
      year_layers <- vector("list", length(years))

      for (y_idx in seq_along(years)) {

        y <- years[y_idx]

        month_dir <- if (!is.null(study_area)) {
          file.path(aorc_dir, study_area, y, m)
        } else {
          file.path(aorc_dir, y, m)
        }

        if (!is.null(study_area)) {
          if (is_difrad) {
            fname <- sprintf("SolaR_%s_%d_%02d_DifRad_surface.tif", study_area, y, m)
          } else {
            fname <- sprintf("AORC_%s_%d_%02d_%s.nc", study_area, y, m, var)
          }
        } else {
          if (is_difrad) {
            fname <- sprintf("SolaR_%d_%02d_DifRad_surface.tif", y, m)
          } else {
            fname <- sprintf("AORC_%d_%02d_%s.nc", y, m, var)
          }
        }

        fpath <- file.path(month_dir, fname)

        if (!file.exists(fpath)) {
          missing_log <- c(missing_log, fpath)
          next
        }

        r <- tryCatch(terra::rast(fpath), error = function(e) NULL)
        if (is.null(r)) {
          missing_log <- c(missing_log, fpath)
          next
        }

        # Match layer by month-day-hour
        t_match <- format(t, "%m-%d %H")
        r_times <- terra::time(r)
        lay     <- which(format(r_times, "%m-%d %H") == t_match)

        if (length(lay) == 0) {
          missing_log <- c(missing_log, sprintf("%s [hour not found: %s]", fpath, t_match))
          next
        }

        year_layers[[y_idx]] <- r[[lay]]
        rm(r)
      }

      # Remove NULLs
      year_layers <- Filter(Negate(is.null), year_layers)

      if (length(year_layers) == 0) next

      # Stack across years
      year_stack <- do.call(c, year_layers)

      # Compute requested stats
      if ("mean"   %in% stats) hour_means[[t_idx]]   <- terra::mean(year_stack,   na.rm = TRUE)
      if ("median" %in% stats) hour_medians[[t_idx]] <- terra::median(year_stack, na.rm = TRUE)
      if ("sd"     %in% stats) hour_sds[[t_idx]]     <- terra::stdev(year_stack,  na.rm = TRUE)
      if ("min"    %in% stats) hour_mins[[t_idx]]     <- min(year_stack,           na.rm = TRUE)
      if ("max"    %in% stats) hour_maxs[[t_idx]]     <- max(year_stack,           na.rm = TRUE)
      if ("mode"   %in% stats) {
        hour_modes[[t_idx]] <- terra::app(year_stack, rast_mode)
      }

      rm(year_stack)
    }

    # Build base name without year
    base_name <- if (!is.null(study_area)) {
      if (is_difrad) {
        sprintf("SolaR_%s_%02d_DifRad_surface", study_area, m)
      } else {
        sprintf("AORC_%s_%02d_%s", study_area, m, var)
      }
    } else {
      if (is_difrad) {
        sprintf("SolaR_%02d_DifRad_surface", m)
      } else {
        sprintf("AORC_%02d_%s", m, var)
      }
    }

    # Helper to assemble and write a stat raster
    write_stat <- function(hour_list, stat_name) {
      valid <- Filter(Negate(is.null), hour_list)
      if (length(valid) == 0) return(invisible(NULL))
      stack <- do.call(c, valid)
      stack <- stack[[order(terra::time(stack))]]
      out_file <- file.path(month_out,
                            sprintf("%s_%s.tif", base_name, stat_name))
      terra::writeRaster(stack, out_file, overwrite = TRUE)
      cat(sprintf("  Saved: %s\n", basename(out_file)))
    }

    if ("mean"   %in% stats) write_stat(hour_means,   "mean")
    if ("median" %in% stats) write_stat(hour_medians, "median")
    if ("sd"     %in% stats) write_stat(hour_sds,     "sd")
    if ("min"    %in% stats) write_stat(hour_mins,    "min")
    if ("max"    %in% stats) write_stat(hour_maxs,    "max")
    if ("mode"   %in% stats) write_stat(hour_modes,   "mode")

    # Log missing files
    if (length(missing_log) > 0) {
      warning(sprintf("Month %02d | %s: %d file(s) missing or unreadable:\n%s",
                      m, var, length(missing_log),
                      paste(" ", missing_log, collapse = "\n")))
    }

    return(data.frame(
      month    = m,
      variable = var,
      status   = "success",
      missing  = length(missing_log),
      stringsAsFactors = FALSE
    ))

  }, future.seed = TRUE)

  # Combine logs
  log <- do.call(rbind, results)

  # Summary
  n_success <- sum(log$status == "success", na.rm = TRUE)
  n_missing <- sum(log$missing,             na.rm = TRUE)

  cat(sprintf("\nDone. %d/%d combos processed. %d file(s) skipped due to missing data.\n",
              n_success, nrow(combos), n_missing))

  return(invisible(log))
}

#' Package AORC Climate Data for Microclimate Modeling
#'
#' Loads AORC climate data, converts units, computes derived variables
#' (relative humidity, wind speed, wind direction), reprojects and crops
#' to a template raster, and assembles everything into a microclimf-ready
#' named list. Outputs one RDS per date range concatenating all months in
#' chronological order. Monthly RDS files are saved to a temporary
#' subdirectory and optionally cleaned up after concatenation.
#'
#' @details Unit conversions applied:
#'   \itemize{
#'     \item Air temperature: Kelvin to Celsius (subtract 273.15)
#'     \item Pressure: Pa to kPa (divide by 1000)
#'     \item Specific humidity + pressure converted to relative humidity
#'     \item U and V wind components converted to wind speed and direction
#'   }
#'   Diffuse radiation layers are aligned to match shortwave radiation
#'   timestamps, with any unmatched layers dropped.
#'
#'   A warning is issued reminding the user that the template raster should
#'   be representative of vegetation or soil parameter outputs.
#'
#'   The packaged climate raster is reprojected to \code{template}'s
#'   CRS and cropped to its extent, but is not resampled to \code{template}'s
#'   resolution. A resolution or origin mismatch against \code{template} triggers a
#'   one-time warning (checked on the first month/period for which packaged
#'   climate data is successfully loaded) rather
#'   than an error, since \code{package_climate} does not know what
#'   downstream DEM the output will ultimately be cropped against.
#'
#' @param dates A data.frame with columns \code{Start_Dates} and \code{End_Dates},
#'   where each row defines a date range to process. Alternatively, a vector of
#'   length 2 with \code{as.Date()} values where first is start date and last is
#'   end date. The day component is ignored; all months between start and end
#'   (inclusive) are processed in chronological order.
#'   E.g. data.frame with columns Start_Dates and End_Dates, or as.Date(c("2020-10-01", "2021-03-01"))
#' @param aorc_dir Base directory containing AORC data organized as
#'   \code{aorc_dir/year/month/} matching the structure produced by
#'   \code{download_aorc()}
#' @param out_dir Directory to save final output RDS files.
#'   Temporary monthly RDS files are saved to \code{out_dir/date_range/}
#'   subdirectories
#' @param template A SpatRaster used as the CRS and extent template for
#'   reprojection and cropping. Should be representative of vegetation or
#'   soil parameter outputs
#' @param study_area Optional character string identifying the study area
#'   e.g. "GMU1". If provided, used to filter input files and prefix output
#'   file names
#' @param keep_monthly Logical. Whether to keep the temporary monthly RDS
#'   files after the final RDS has been created. If FALSE the temporary
#'   period subdirectory and its contents are deleted after concatenation.
#'   Default is FALSE
#'
#' @return Invisibly returns a named list of output file paths keyed by period
#' @export
package_climate <- function(dates,
                            aorc_dir,
                            out_dir,
                            template,
                            study_area = NULL,
                            keep_monthly = FALSE) {

  if (!inherits(template, "SpatRaster")) {
    stop("template must be a SpatRaster")
  }

  warning("Ensure the template raster is representative of vegetation or soil parameter outputs for correct spatial alignment.")

  # Process dates input
  if (is.data.frame(dates)) {
    if (!all(c("Start_Dates", "End_Dates") %in% names(dates))) {
      stop("dates data.frame must contain columns 'Start_Dates' and 'End_Dates'")
    }
    date_ranges <- dates
  } else if (is.vector(dates) && length(dates) == 2) {
    date_ranges <- data.frame(
      Start_Dates = as.Date(dates[1]),
      End_Dates = as.Date(dates[2]),
      stringsAsFactors = FALSE
    )
  } else {
    stop("dates must be either a data.frame with Start_Dates and End_Dates columns, or a vector of length 2")
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


  # Helper to load, sort, reproject and crop a variable raster
  load_var <- function(files, template) {
    r <- terra::rast(files)
    names(r) <- ifelse(
      grepl("\\d{2}:\\d{2}", names(r)),
      names(r),
      paste0(names(r), " 00:00:00")
    )
    t_r <- as.POSIXct(names(r), format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
    if (all(is.na(t_r))) t_r <- terra::time(r)
    r   <- r[[order(t_r)]]
    terra::time(r) <- t_r[order(t_r)]
    r <- terra::project(r, terra::crs(template), method = "near", threads = TRUE)
    r <- terra::crop(r, terra::ext(template))
    return(r)
  }

  # Build log
  log <- list()
  .grid_checked <- FALSE

  for (i in seq_len(nrow(date_ranges))) {

    start_date <- as.Date(date_ranges$Start_Dates[i])
    end_date <- as.Date(date_ranges$End_Dates[i])
    period_label <- sprintf("%s_to_%s",
                            format(start_date, "%Y%m%d"),
                            format(end_date, "%Y%m%d"))

    cat(sprintf("\n--- Packaging climate for period: %s ---\n", period_label))

    month_seq <- .generate_month_sequence(start_date, end_date)

    # Create temp directory for monthly RDS files
    temp_dir <- file.path(out_dir, period_label)
    dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

    month_rds_paths <- list()

    for (j in seq_len(nrow(month_seq))) {

      y <- month_seq$year[j]
      m <- month_seq$month[j]

      cat(sprintf("  Processing month: %04d-%02d\n", y, m))

      month_dir <- if (!is.null(study_area)) {
        file.path(aorc_dir, study_area, y, m)
      } else {
        file.path(aorc_dir, y, m)
      }

      if (!dir.exists(month_dir)) {
        warning(sprintf("Directory not found for year %d month %02d:\n  %s", y, m, month_dir))
        next
      }

      clim_files <- list.files(month_dir, full.names = TRUE, recursive = FALSE)

      # Filter by study area if provided
      if (!is.null(study_area)) {
        clim_files <- clim_files[grepl(study_area, basename(clim_files))]
      }

      names(clim_files) <- basename(clim_files)

      if (length(clim_files) == 0) {
        warning(sprintf("No climate files found for year %d month %02d", y, m))
        next
      }

      # --- Shortwave radiation ---
      sw_f <- clim_files[grepl("DSWRF", names(clim_files))]
      if (length(sw_f) == 0) stop(sprintf("No DSWRF files found for year %d month %02d", y, m))
      r_sw <- load_var(sw_f, template)

      if (!.grid_checked) {
        .check_grid_match(template, r_sw, "template", "packaged climate", action = "warn")
        .grid_checked <- TRUE
      }

      sw_t <- terra::time(r_sw)
      r_sw <- terra::wrap(r_sw)

      # --- Diffuse radiation - align to shortwave timestamps ---
      df_f <- clim_files[grepl("DifRad|DiffRad", names(clim_files))]
      if (length(df_f) == 0) {
        warning(sprintf("No diffuse radiation files found for year %d month %02d", y, m))
        r_df <- NULL
      } else {
        r_df              <- terra::rast(df_f)
        names(sw_t)       <- format(sw_t, "%m%d%H")
        r_df_times        <- terra::time(r_df)
        names(r_df_times) <- format(r_df_times, "%m%d%H")
        names_only_in_r   <- setdiff(names(r_df_times), names(sw_t))
        r_df              <- r_df[[!(names(r_df_times) %in% names_only_in_r)]]
        r_df_times        <- terra::time(r_df)
        names(r_df_times) <- format(r_df_times, "%m%d%H")
        sw_t_aligned      <- sw_t[names(r_df_times)]
        terra::time(r_df) <- sw_t_aligned
        names(r_df) <- ifelse(
          grepl("\\d{2}:\\d{2}", as.character(terra::time(r_df))),
          as.character(terra::time(r_df)),
          paste0(terra::time(r_df), " 00:00:00")
        )
        t_r               <- as.POSIXct(names(r_df), format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
        r_df              <- r_df[[order(t_r)]]
        terra::time(r_df) <- t_r[order(t_r)]
        r_df <- terra::project(r_df, terra::crs(template), method = "near", threads = TRUE)
        r_df <- terra::crop(r_df, terra::ext(template))
        r_df <- terra::wrap(r_df)
      }

      # --- Longwave radiation ---
      lw_f <- clim_files[grepl("DLWRF", names(clim_files))]
      if (length(lw_f) == 0) stop(sprintf("No DLWRF files found for year %d month %02d", y, m))
      r_lw <- terra::wrap(load_var(lw_f, template))

      # --- Precipitation ---
      pr_f <- clim_files[grepl("APCP", names(clim_files))]
      if (length(pr_f) == 0) stop(sprintf("No APCP files found for year %d month %02d", y, m))
      r_pr <- terra::wrap(load_var(pr_f, template))

      # --- Air temperature: K to C ---
      at_f <- clim_files[grepl("TMP", names(clim_files))]
      if (length(at_f) == 0) stop(sprintf("No TMP files found for year %d month %02d", y, m))
      r_at_k <- load_var(at_f, template)
      at_c   <- r_at_k - 273.15
      r_at   <- terra::wrap(at_c)
      rm(r_at_k)

      # --- Pressure: Pa to kPa ---
      pa_f <- clim_files[grepl("PRES", names(clim_files))]
      if (length(pa_f) == 0) stop(sprintf("No PRES files found for year %d month %02d", y, m))
      r_pa_raw <- load_var(pa_f, template)
      pa_pa    <- r_pa_raw
      r_pa     <- terra::wrap(r_pa_raw / 1000)
      rm(r_pa_raw)

      # --- Relative humidity from specific humidity + pressure ---
      sh_f <- clim_files[grepl("SPFH", names(clim_files))]
      if (length(sh_f) == 0) stop(sprintf("No SPFH files found for year %d month %02d", y, m))
      r_sh     <- load_var(sh_f, template)
      e        <- (r_sh * pa_pa) / (0.622 + 0.378 * r_sh)
      es_water <- 611.2 * exp((17.67 * at_c) / (at_c + 243.5))
      es       <- es_water
      RH       <- 100 * (e / es)
      RH       <- terra::clamp(RH, lower = 0, upper = 100)
      r_rh     <- terra::wrap(RH)
      rm(e, es, es_water, RH, at_c, pa_pa, r_sh)

      # --- Wind speed and direction from U and V components ---
      uw_f <- clim_files[grepl("UGRD", names(clim_files))]
      vw_f <- clim_files[grepl("VGRD", names(clim_files))]
      if (length(uw_f) == 0) stop(sprintf("No UGRD files found for year %d month %02d", y, m))
      if (length(vw_f) == 0) stop(sprintf("No VGRD files found for year %d month %02d", y, m))
      r_u  <- load_var(uw_f, template)
      r_v  <- load_var(vw_f, template)
      ws   <- sqrt(r_u^2 + r_v^2)
      wd   <- (180 + terra::atan2(r_u, r_v) * 180 / pi) %% 360
      r_ws <- terra::wrap(ws)
      r_wd <- terra::wrap(wd)
      rm(ws, wd, r_u, r_v)

      gc()

      # --- Assemble output list ---
      out <- list(
        precip    = r_pr,
        temp      = r_at,
        relhum    = r_rh,
        lwdown    = r_lw,
        swdown    = r_sw,
        pres      = r_pa,
        windspeed = r_ws,
        winddir   = r_wd,
        difrad    = r_df
      )

      # --- Save monthly RDS to temp directory ---
      month_file <- if (!is.null(study_area)) {
        file.path(temp_dir, sprintf("%s_Climate_%04d_%02d.RDS", study_area, y, m))
      } else {
        file.path(temp_dir, sprintf("Climate_%04d_%02d.RDS", y, m))
      }

      readr::write_rds(out, file = month_file)
      cat(sprintf("    Saved temp: %s\n", basename(month_file)))
      month_rds_paths[[length(month_rds_paths) + 1]] <- month_file

      rm(out, r_pr, r_at, r_rh, r_lw, r_sw, r_pa, r_ws, r_wd, r_df)
      gc()
    }

    # --- Concatenate months in chronological order ---
    if (length(month_rds_paths) >= 1) {

      cat(sprintf("  Concatenating months for period %s in chronological order...\n", period_label))

      # Load first month as base
      int    <- readr::read_rds(month_rds_paths[[1]])
      vnames <- names(int)

      # Concatenate remaining months if more than one
      if (length(month_rds_paths) > 1) {
        for (k in seq_along(month_rds_paths)[-1]) {
          out <- readr::read_rds(month_rds_paths[[k]])
          int <- lapply(vnames, function(x) {
            var1 <- terra::rast(int[[x]])
            var2 <- terra::rast(out[[x]])
            terra::wrap(c(var1, var2))
          })
          names(int) <- vnames
          rm(out)
          gc()
        }
      }

      # --- Save period RDS to base output directory ---
      period_file <- if (!is.null(study_area)) {
        file.path(out_dir, sprintf("%s_Climate_%s.RDS", study_area, period_label))
      } else {
        file.path(out_dir, sprintf("Climate_%s.RDS", period_label))
      }

      readr::write_rds(int, file = period_file)
      cat(sprintf("  Saved: %s\n", basename(period_file)))
      log[[period_label]] <- period_file

      rm(int)
      gc()

      # --- Clean up temp directory if keep_monthly is FALSE ---
      if (!keep_monthly) {
        unlink(temp_dir, recursive = TRUE)
        cat(sprintf("  Temporary monthly files removed for period %s.\n", period_label))
      } else {
        cat(sprintf("  Temporary monthly files kept in: %s\n", temp_dir))
      }
    }
  }

  cat("\nDone.\n")
  return(invisible(log))
}
