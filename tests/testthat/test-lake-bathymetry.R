test_that("depth follows the shoreline rule and caps at max_depth", {
  lk <- .test_square_lake(200)
  b <- generate_lake_bathymetry(lk)
  expect_s4_class(b, "SpatRaster")
  expect_equal(terra::res(b), c(1, 1))
  v5 <- terra::extract(b, cbind(500000 + 5.5, 6700000 + 100))[1, 1]
  expect_equal(v5, 1, tolerance = 0.15)
  vc <- terra::extract(b, cbind(500100.5, 6700100.5))[1, 1]
  expect_equal(vc, 4.572, tolerance = 0.01)
  expect_lte(max(terra::values(b), na.rm = TRUE), 4.572)
  expect_true(is.na(terra::extract(b, cbind(499990, 6700100))[1, 1]))
})

test_that("per-lake override columns are honoured", {
  lk <- .test_square_lake(200); lk$max_depth <- 2
  b <- generate_lake_bathymetry(lk)
  expect_lte(max(terra::values(b), na.rm = TRUE), 2)
})

test_that("lon/lat lakes and ref_depth >= max_depth are rejected", {
  expect_error(generate_lake_bathymetry(sf::st_transform(.test_square_lake(), 4326)), "projected")
  expect_error(generate_lake_bathymetry(.test_square_lake(), ref_depth = 5), "ref_depth")
})
