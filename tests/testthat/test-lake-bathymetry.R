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

test_that("Hollister estimate = tan(median buffer slope) * Dmax", {
  lk <- .test_square_lake(200)
  b <- generate_lake_bathymetry(lk, dem = .test_plane_dem(0.1))
  z <- attr(b, "zmax")
  expect_equal(z$zmax_source, "hollister2011")
  dmax <- max(terra::values(.lake_dist(sf::st_geometry(lk), 1)$d, mat = FALSE), na.rm = TRUE)
  expect_equal(z$zmax, 0.1 * dmax, tolerance = 0.02)
  b2 <- generate_lake_bathymetry(lk, dem = .test_plane_dem(0.1), correct_factor = 2)
  expect_equal(attr(b2, "zmax")$zmax, 2 * z$zmax, tolerance = 1e-6)
})

test_that("DEM in another CRS gives the same estimate", {
  lk <- .test_square_lake(200)
  d1 <- attr(generate_lake_bathymetry(lk, dem = .test_plane_dem(0.1)), "zmax")$zmax
  d2 <- attr(generate_lake_bathymetry(lk, dem = terra::project(.test_plane_dem(0.1), "EPSG:3338")), "zmax")$zmax
  expect_equal(d2, d1, tolerance = 0.02)
})

test_that("rings exclude neighbouring lakes' water", {
  dem <- .test_plane_dem(0.1)
  dem[terra::cellFromXY(dem, cbind(500250, 6700100))] <- 1e4
  lk <- rbind(.test_square_lake(200, id = "A"), .test_square_lake(100, x0 = 500220, id = "B"))
  z <- attr(generate_lake_bathymetry(lk, dem = dem), "zmax")
  expect_equal(z$zmax[1], attr(generate_lake_bathymetry(lk[1, ], dem = .test_plane_dem(0.1)), "zmax")$zmax,
               tolerance = 0.02)
})

test_that("flat terrain warns, never NaN, and all-zero slope stops naming the lake", {
  lk <- .test_square_lake(200)
  expect_warning(b <- generate_lake_bathymetry(lk, dem = .test_plane_dem(0.005)), "A")
  expect_false(any(is.nan(terra::values(b))))
  expect_error(suppressWarnings(generate_lake_bathymetry(lk, dem = .test_plane_dem(0))), "A")
})

test_that("powerlaw peaks at Zmax and alpha = 1 equals cone; alpha column used", {
  lk <- .test_square_lake(200)
  p <- generate_lake_bathymetry(lk, method = "powerlaw", max_depth = 3)
  expect_equal(max(terra::values(p), na.rm = TRUE), 3)
  c1 <- generate_lake_bathymetry(lk, method = "cone", max_depth = 3)
  p1 <- generate_lake_bathymetry(lk, method = "powerlaw", alpha = 1, max_depth = 3)
  expect_equal(terra::values(p1), terra::values(c1))
  lk$alpha <- 2
  p2 <- generate_lake_bathymetry(lk, method = "powerlaw", alpha = 0.5, max_depth = 3)
  v <- terra::extract(p2, cbind(500050.5, 6700100.5))[1, 1]
  expect_equal(v, 3 * (50 / 99.5)^2, tolerance = 0.03)
})

test_that("tin is 0 near shore, Zmax at the pole, finite inside", {
  skip_if_not_installed("interp")
  t <- generate_lake_bathymetry(.test_square_lake(200), method = "tin", max_depth = 3)
  v <- terra::values(t, mat = FALSE); v <- v[!is.na(v)]
  expect_equal(max(v), 3, tolerance = 1e-6)
  expect_true(all(is.finite(v)) && all(v >= 0))
  expect_lt(terra::extract(t, cbind(500000.5, 6700100.5))[1, 1], 0.1)
})
