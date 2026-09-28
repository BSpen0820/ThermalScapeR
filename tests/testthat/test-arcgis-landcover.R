mock_arcgis_layer_sf <- function() {
  pts <- data.frame(
    x = c(300000, 305000, 300000, 305000),
    y = c(6650000, 6650000, 6655000, 6655000),
    DT_Code = c("SS", "SS", "BS", "OW"),
    Dominance_Type = c("Sitka Spruce", "Sitka Spruce", "Black Spruce", "Open Water"),
    TreeCC = c(10, 40, 70, 0)
  )
  v <- terra::vect(pts, geom = c("x", "y"), crs = "epsg:32606")
  poly <- terra::buffer(v, 2000)
  sf::st_as_sf(poly)
}

mock_kenai_query_extent <- function() {
  sf::st_as_sfc(sf::st_bbox(
    c(xmin = 298000, ymin = 6648000, xmax = 307000, ymax = 6662000), crs = 32606
  ))
}

# .arcgis_resolve_layer ------------------------------------------------------

test_that(".arcgis_resolve_layer resolves a known Kenai layer by name, with its default field and label", {
  res <- .arcgis_resolve_layer(.arcgis_kenai_service_url, "DominanceType", NULL)
  expect_identical(res, list(id = 0L, field = "DT_Code", label_field = "Dominance_Type"))
})

test_that(".arcgis_resolve_layer resolves a known Kenai layer by numeric id, with no label for non-DominanceType layers", {
  res <- .arcgis_resolve_layer(.arcgis_kenai_service_url, 1L, NULL)
  expect_identical(res, list(id = 1L, field = "TreeCC", label_field = NA_character_))
})

test_that(".arcgis_resolve_layer drops the built-in label when an explicit field overrides the default", {
  res <- .arcgis_resolve_layer(.arcgis_kenai_service_url, "DominanceType", "Dominance_Type")
  expect_identical(res, list(id = 0L, field = "Dominance_Type", label_field = NA_character_))
})

test_that(".arcgis_resolve_layer errors on an unrecognized layer with no field supplied", {
  expect_error(
    .arcgis_resolve_layer(.arcgis_kenai_service_url, 9L, NULL),
    "field"
  )
})

test_that(".arcgis_resolve_layer works for a non-Kenai service when a numeric layer and field are supplied", {
  res <- .arcgis_resolve_layer("https://example.com/arcgis/rest/services/Foo/MapServer", 2L, "LC_CLASS")
  expect_identical(res, list(id = 2L, field = "LC_CLASS", label_field = NA_character_))
})

test_that(".arcgis_resolve_layer errors for a non-Kenai service given a character layer name, even with field supplied", {
  expect_error(
    .arcgis_resolve_layer("https://example.com/arcgis/rest/services/Foo/MapServer", "DominanceType", "SomeField"),
    "numeric"
  )
})

# .arcgis_aoi_bbox_sf ---------------------------------------------------------

test_that(".arcgis_aoi_bbox_sf builds a WGS84 bbox polygon from an sf object", {
  poly <- sf::st_as_sfc(sf::st_bbox(
    c(xmin = -150.5, ymin = 59.5, xmax = -149.5, ymax = 60.5), crs = 4326
  ))
  bb <- .arcgis_aoi_bbox_sf(poly)
  expect_s3_class(bb, "sfc")
  expect_identical(sf::st_crs(bb), sf::st_crs(4326))
  b <- sf::st_bbox(bb)
  expect_equal(as.numeric(b["xmin"]), -150.5, tolerance = 1e-6)
  expect_equal(as.numeric(b["xmax"]), -149.5, tolerance = 1e-6)
})

test_that(".arcgis_aoi_bbox_sf builds the correct bbox from a SpatRaster (terra ext order xmin,xmax,ymin,ymax)", {
  r <- terra::rast(xmin = -150, xmax = -149, ymin = 60, ymax = 61, crs = "epsg:4326")
  bb <- .arcgis_aoi_bbox_sf(r)
  b <- sf::st_bbox(bb)
  expect_equal(as.numeric(b["xmin"]), -150, tolerance = 1e-6)
  expect_equal(as.numeric(b["xmax"]), -149, tolerance = 1e-6)
  expect_equal(as.numeric(b["ymin"]), 60, tolerance = 1e-6)
  expect_equal(as.numeric(b["ymax"]), 61, tolerance = 1e-6)
})

test_that(".arcgis_aoi_bbox_sf reads bbox_wgs84 directly from a define_aoi()-style list", {
  aoi <- list(geometry = "not-used", crs = "epsg:4326", bbox_wgs84 = c(-150, 60, -149, 61))
  bb <- .arcgis_aoi_bbox_sf(aoi)
  b <- sf::st_bbox(bb)
  expect_equal(as.numeric(b["xmin"]), -150, tolerance = 1e-6)
  expect_equal(as.numeric(b["ymax"]), 61, tolerance = 1e-6)
})

# .arcgis_rasterize -----------------------------------------------------------

test_that(".arcgis_rasterize builds a categorical raster from a character field, bounded by query_extent", {
  sf_poly <- mock_arcgis_layer_sf()
  r <- .arcgis_rasterize(sf_poly, field = "DT_Code", res = 500, query_extent = mock_kenai_query_extent())
  expect_true(terra::is.factor(r))
  lv <- terra::levels(r)[[1]]
  expect_true(all(c("SS", "BS", "OW") %in% lv$DT_Code))
})

test_that(".arcgis_rasterize bounds the grid to query_extent, not the (larger) union of touching features", {
  # A single huge polygon that only touches a small AOI
  huge <- sf::st_as_sf(terra::buffer(
    terra::vect(data.frame(x = 300000, y = 6650000, cc = 1), geom = c("x", "y"), crs = "epsg:32606"),
    50000
  ))
  small_extent <- sf::st_as_sfc(sf::st_bbox(
    c(xmin = 295000, ymin = 6645000, xmax = 305000, ymax = 6655000), crs = 32606
  ))
  r <- .arcgis_rasterize(huge, field = "cc", res = 500, query_extent = small_extent)
  e <- terra::ext(r)
  expect_lt(as.numeric(e[2]) - as.numeric(e[1]), 20000)
  expect_lt(as.numeric(e[4]) - as.numeric(e[3]), 20000)
})

test_that(".arcgis_rasterize builds a numeric raster from a numeric field, and crop_template reprojects/crops to its extent", {
  pts <- data.frame(x = c(300000, 305000, 300000, 305000),
                     y = c(6650000, 6650000, 6660000, 6660000), cc = c(10, 90, 50, 20))
  v <- terra::vect(pts, geom = c("x", "y"), crs = "epsg:32606")
  sf_poly <- sf::st_as_sf(terra::buffer(v, 1500))

  # A template covering only part of the queried extent
  template <- terra::rast(terra::ext(299000, 306000, 6649000, 6653000),
                           resolution = 250, crs = "epsg:32606")
  r <- .arcgis_rasterize(sf_poly, field = "cc", res = 500,
                         query_extent = mock_kenai_query_extent(), crop_template = template)

  expect_false(terra::is.factor(r))
  expect_true(terra::same.crs(r, template))
  expect_true(as.numeric(terra::ext(r)[4]) <= as.numeric(terra::ext(template)[4]) + 1e-6)
})

# .arcgis_attach_label ---------------------------------------------------------

test_that(".arcgis_attach_label adds a descriptive label column to a raster's factor levels", {
  sf_poly <- mock_arcgis_layer_sf()
  r <- .arcgis_rasterize(sf_poly, field = "DT_Code", res = 500, query_extent = mock_kenai_query_extent())
  r <- .arcgis_attach_label(r, sf_poly, "DT_Code", "Dominance_Type")
  lv <- terra::cats(r)[[1]]
  expect_true("Dominance_Type" %in% names(lv))
  expect_equal(lv$Dominance_Type[lv$DT_Code == "SS"], "Sitka Spruce")
  expect_equal(lv$Dominance_Type[lv$DT_Code == "OW"], "Open Water")
})

# download_arcgis_landcover ---------------------------------------------------

test_that("download_arcgis_landcover queries, rasterizes, and writes one file per requested layer, attaching Dominance_Type label", {
  out_dir <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    .arcgis_query_layer = function(service_url, layer_id, fields, filter_geom) mock_arcgis_layer_sf()
  )
  aoi <- list(geometry = "unused", crs = "epsg:32606", bbox_wgs84 = c(-150.7, 59.95, -150.6, 60.0))

  out <- download_arcgis_landcover(aoi, out_dir, layer = c("DominanceType", "TreeCanopyCover"), res = 500)

  expect_named(out$rasters, c("DominanceType", "TreeCanopyCover"))
  expect_true(all(file.exists(file.path(out_dir, c("DominanceType.tif", "TreeCanopyCover.tif")))))
  expect_equal(out$log$status, c("ok", "ok"))

  lv <- terra::cats(out$rasters$DominanceType)[[1]]
  expect_true("Dominance_Type" %in% names(lv))
})

test_that("download_arcgis_landcover records a per-layer failure without aborting the other layers", {
  out_dir <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    .arcgis_query_layer = function(service_url, layer_id, fields, filter_geom) {
      if (layer_id == 0L) stop("service unavailable")
      mock_arcgis_layer_sf()
    }
  )
  aoi <- list(geometry = "unused", crs = "epsg:32606", bbox_wgs84 = c(-150.7, 59.95, -150.6, 60.0))

  expect_warning(
    out <- download_arcgis_landcover(aoi, out_dir, layer = c("DominanceType", "TreeCanopyCover"), res = 500),
    "service unavailable"
  )
  expect_equal(out$log$status, c("failed: service unavailable", "ok"))
  expect_false(file.exists(file.path(out_dir, "DominanceType.tif")))
  expect_true(file.exists(file.path(out_dir, "TreeCanopyCover.tif")))
})

test_that("download_arcgis_landcover logs no_features and still processes later layers when a query returns zero rows", {
  out_dir <- withr::local_tempdir()
  empty_sf <- mock_arcgis_layer_sf()[0, ]
  testthat::local_mocked_bindings(
    .arcgis_query_layer = function(service_url, layer_id, fields, filter_geom) {
      if (layer_id == 0L) empty_sf else mock_arcgis_layer_sf()
    }
  )
  aoi <- list(geometry = "unused", crs = "epsg:32606", bbox_wgs84 = c(-150.7, 59.95, -150.6, 60.0))

  expect_warning(
    out <- download_arcgis_landcover(aoi, out_dir, layer = c("DominanceType", "TreeCanopyCover"), res = 500),
    "No features"
  )
  expect_equal(out$log$status, c("no_features", "ok"))
  expect_false(file.exists(file.path(out_dir, "DominanceType.tif")))
  expect_true(file.exists(file.path(out_dir, "TreeCanopyCover.tif")))
})

test_that("download_arcgis_landcover skips an existing file when overwrite = FALSE, without re-querying", {
  out_dir <- withr::local_tempdir()
  call_count <- 0
  testthat::local_mocked_bindings(
    .arcgis_query_layer = function(service_url, layer_id, fields, filter_geom) {
      call_count <<- call_count + 1
      mock_arcgis_layer_sf()
    }
  )
  aoi <- list(geometry = "unused", crs = "epsg:32606", bbox_wgs84 = c(-150.7, 59.95, -150.6, 60.0))

  download_arcgis_landcover(aoi, out_dir, layer = "DominanceType", res = 500)
  expect_equal(call_count, 1)

  out2 <- download_arcgis_landcover(aoi, out_dir, layer = "DominanceType", res = 500, overwrite = FALSE)
  expect_equal(call_count, 1)
  expect_equal(out2$log$status, "skipped_existing")
})
