test_that(".aoi_to_wgs84_bbox reads bbox_wgs84 directly from a define_aoi()-style list", {
  aoi <- list(geometry = "not-used", crs = "epsg:4326", bbox_wgs84 = c(-150, 60, -149, 61))
  bb <- .aoi_to_wgs84_bbox(aoi)
  expect_equal(unname(bb["xmin"]), -150, tolerance = 1e-6)
  expect_equal(unname(bb["ymin"]), 60, tolerance = 1e-6)
  expect_equal(unname(bb["xmax"]), -149, tolerance = 1e-6)
  expect_equal(unname(bb["ymax"]), 61, tolerance = 1e-6)
})

test_that(".aoi_to_wgs84_bbox builds the correct bbox from an sf object", {
  poly <- sf::st_as_sfc(sf::st_bbox(
    c(xmin = -150.5, ymin = 59.5, xmax = -149.5, ymax = 60.5), crs = 4326
  ))
  bb <- .aoi_to_wgs84_bbox(poly)
  expect_equal(unname(bb["xmin"]), -150.5, tolerance = 1e-6)
  expect_equal(unname(bb["xmax"]), -149.5, tolerance = 1e-6)
})

test_that(".aoi_to_wgs84_bbox does not swap longitude and latitude for a SpatRaster (terra ext order xmin,xmax,ymin,ymax)", {
  r <- terra::rast(xmin = -150, xmax = -149, ymin = 60, ymax = 61, crs = "epsg:4326")
  bb <- .aoi_to_wgs84_bbox(r)
  expect_equal(unname(bb["xmin"]), -150, tolerance = 1e-6)
  expect_equal(unname(bb["xmax"]), -149, tolerance = 1e-6)
  expect_equal(unname(bb["ymin"]), 60, tolerance = 1e-6)
  expect_equal(unname(bb["ymax"]), 61, tolerance = 1e-6)
})

test_that(".aoi_to_wgs84_bbox does not swap longitude and latitude for a SpatVector", {
  v <- terra::vect(terra::ext(-150, -149, 60, 61), crs = "epsg:4326")
  bb <- .aoi_to_wgs84_bbox(v)
  expect_equal(unname(bb["xmin"]), -150, tolerance = 1e-6)
  expect_equal(unname(bb["xmax"]), -149, tolerance = 1e-6)
  expect_equal(unname(bb["ymin"]), 60, tolerance = 1e-6)
  expect_equal(unname(bb["ymax"]), 61, tolerance = 1e-6)
})

test_that(".aoi_to_wgs84_bbox errors on an unsupported aoi type", {
  expect_error(.aoi_to_wgs84_bbox(1:3), "aoi must be")
})
