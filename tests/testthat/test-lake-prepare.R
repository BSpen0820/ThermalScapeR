test_that("hypsograph and geometry from a generated lake", {
  lk <- .test_square_lake(200)
  b  <- generate_lake_bathymetry(lk)
  dem <- terra::rast(terra::ext(b), res = 30, crs = terra::crs(b), vals = 60)
  p  <- prepare_lakes(b, dem, lakes = lk, id_col = "lake_id")
  expect_equal(nrow(p), 1L); expect_equal(p$lake_id, "A")
  expect_equal(p$area_m2, 40000, tolerance = 0.01)
  expect_equal(p$max_depth, 4.572, tolerance = 0.01)
  expect_equal(p$out_depth, 0.5)
  expect_equal(p$elevation, 60)
  expect_equal(p$bsn_len, 200, tolerance = 0.01)
  h <- p$hypsograph[[1]]
  expect_named(h, c("Depth_meter", "Area_meterSquared"))
  expect_equal(h$Depth_meter[1], 0)
  expect_true(all(diff(h$Area_meterSquared) <= 0))
  expect_true(all(h$Area_meterSquared > 0))
})

test_that("shallow pond is raised to the depth floor", {
  lk <- .test_square_lake(20, id = "P")
  b  <- generate_lake_bathymetry(lk, ref_dist = 2, ref_depth = 0.05, max_depth = 0.1)
  dem <- terra::rast(terra::ext(b), res = 30, crs = terra::crs(b), vals = 60)
  p  <- prepare_lakes(b, dem, lakes = lk, id_col = "lake_id")
  expect_equal(p$model_depth, 0.3)
  expect_equal(p$out_depth, 0.15)
  expect_equal(max(p$hypsograph[[1]]$Depth_meter), 0.3)
})

test_that("lakes derived from the raster when no polygons given", {
  b <- terra::merge(generate_lake_bathymetry(.test_square_lake(100)),
                    generate_lake_bathymetry(.test_square_lake(100, x0 = 501000, id = "B")))
  dem <- terra::rast(terra::ext(b), res = 30, crs = terra::crs(b), vals = 60)
  p <- prepare_lakes(b, dem)
  expect_equal(p$lake_id, c("lake_001", "lake_002"))
})

test_that("lon/lat bathymetry is rejected", {
  b <- terra::rast(nrows = 2, ncols = 2, vals = 1)
  expect_error(prepare_lakes(b, b), "projected")
})

test_that("bathymetry in a feet CRS is rejected", {
  b <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2,
                   crs = "EPSG:2232", vals = 1)
  expect_error(prepare_lakes(b, b), "metre")
})
