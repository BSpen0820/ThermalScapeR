test_that("depth follows the shoreline rule and caps at max_depth", {
  lk <- .test_square_lake(200)
  b <- generate_lake_bathymetry(lk, method = "shoreline")
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
  b <- generate_lake_bathymetry(lk, method = "shoreline")
  expect_lte(max(terra::values(b), na.rm = TRUE), 2)
})

test_that("lon/lat lakes and ref_depth >= max_depth are rejected", {
  expect_error(generate_lake_bathymetry(sf::st_transform(.test_square_lake(), 4326)), "projected")
  expect_error(generate_lake_bathymetry(.test_square_lake(), method = "shoreline", ref_depth = 5), "ref_depth")
})

test_that("projected CRS in feet is rejected", {
  ft <- sf::st_transform(.test_square_lake(), 2232)
  expect_error(generate_lake_bathymetry(ft), "metre")
})

test_that("cone is the default and peaks at max_depth at the pole", {
  b <- generate_lake_bathymetry(.test_square_lake(200), max_depth = 3)
  expect_equal(names(b), "depth")
  v <- terra::values(b, mat = FALSE)
  expect_equal(max(v, na.rm = TRUE), 3)
  expect_true(all(v[!is.na(v)] > 0))
  expect_false(any(is.nan(v)))
  expect_lt(min(v, na.rm = TRUE), 3 * 1 / 99)
  z <- attr(b, "zmax")
  expect_equal(z$zmax_source, "argument"); expect_equal(z$zmax, 3)
})

test_that("max_depth column beats the argument; NA without dem stops naming the lake", {
  lk <- rbind(.test_square_lake(200, id = "A"), .test_square_lake(200, x0 = 501000, id = "B"))
  lk$max_depth <- c(2, NA)
  b <- generate_lake_bathymetry(lk, max_depth = 5)
  expect_equal(attr(b, "zmax")$zmax, c(2, 5))
  expect_equal(attr(b, "zmax")$zmax_source, c("column", "argument"))
  expect_error(generate_lake_bathymetry(lk), "B")
})

test_that("tiny pond gives finite positive depths", {
  b <- generate_lake_bathymetry(.test_square_lake(1.5), max_depth = 0.5)
  v <- terra::values(b, mat = FALSE); v <- v[!is.na(v)]
  expect_true(length(v) > 0 && all(is.finite(v)) && all(v > 0))
})

test_that("shoreline method reproduces the previous rule", {
  b <- generate_lake_bathymetry(.test_square_lake(200), method = "shoreline")
  expect_equal(terra::extract(b, cbind(500005.5, 6700100))[1, 1], 1, tolerance = 0.15)
  expect_equal(attr(b, "zmax")$zmax, 4.572)
})
