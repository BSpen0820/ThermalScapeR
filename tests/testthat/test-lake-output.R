grid30 <- function() {
  terra::rast(xmin = 499910, xmax = 500390, ymin = 6699910, ymax = 6700390,
              res = 30, crs = "EPSG:32605", vals = 42)  # 42 = forest
}

test_that("water cells map to overlapping, touching, or nearby lakes", {
  lc <- grid30()
  lk <- rbind(.test_square_lake(200, id = "A"),
              .test_square_lake(8, x0 = 500302, y0 = 6700302, id = "P"))  # pond inside cell 500300-500330, misses its centre
  lc[terra::cellFromXY(lc, cbind(500100, 6700100))] <- 11
  lc[terra::cellFromXY(lc, cbind(500235, 6700100))] <- 11   # just off lake A
  lc[terra::cellFromXY(lc, cbind(500310, 6700310))] <- 11   # holds pond P
  lc[terra::cellFromXY(lc, cbind(499925, 6700375))] <- 11   # far from all lakes
  expect_warning(idx <- .map_lakes_to_grid(lk, lc, 11), "1 water cell")
  expect_equal(idx[terra::cellFromXY(lc, cbind(500100, 6700100))][1, 1], 1L)
  expect_equal(idx[terra::cellFromXY(lc, cbind(500235, 6700100))][1, 1], 1L)
  expect_equal(idx[terra::cellFromXY(lc, cbind(500310, 6700310))][1, 1], 2L)
  expect_true(is.na(idx[terra::cellFromXY(lc, cbind(499925, 6700375))][1, 1]))
  expect_true(is.na(idx[terra::cellFromXY(lc, cbind(500000, 6700000))][1, 1]))
})

test_that("a lake with no water cells is warned about by id", {
  lc <- grid30()
  expect_warning(.map_lakes_to_grid(.test_square_lake(200, id = "A"), lc, 11), "A")
})

test_that("NetCDF round-trips values and time", {
  lc <- grid30(); idx <- lc * NA; idx[1] <- 1L; idx[2] <- 2L
  tm <- as.POSIXct("2020-05-01", tz = "UTC") + 3600 * 0:2
  ens <- list(data.frame(datetime = tm, water_temp = c(5, 6, 7), ice_frac = c(1, 0.5, 0)),
              data.frame(datetime = tm, water_temp = c(8, 9, 10), ice_frac = 0))
  f <- withr::local_tempfile(fileext = ".nc")
  .write_watertemp_nc(idx, ens, f)
  wt <- terra::rast(f, subds = "water_temp")
  expect_equal(terra::nlyr(wt), 3L)
  expect_equal(as.numeric(wt[1]), c(5, 6, 7))
  expect_equal(as.numeric(wt[2]), c(8, 9, 10))
  expect_true(all(is.na(as.numeric(wt[3]))))
  expect_equal(as.numeric(terra::time(wt)), as.numeric(tm))
  expect_equal(as.numeric(terra::rast(f, subds = "ice_frac")[1]), c(1, 0.5, 0))
})

test_that("NetCDF is cropped to water cells on the same grid alignment", {
  lc <- grid30(); idx <- lc * NA
  cell <- terra::cellFromXY(lc, cbind(500300, 6700300)); idx[cell] <- 1L
  tm <- as.POSIXct("2020-05-01", tz = "UTC") + 3600 * 0:1
  f <- withr::local_tempfile(fileext = ".nc")
  .write_watertemp_nc(idx, list(data.frame(datetime = tm, water_temp = c(5, 6), ice_frac = 0)), f)
  wt <- terra::rast(f, subds = "water_temp")
  expect_equal(terra::ncell(wt), 9L)
  expect_equal(terra::res(wt), c(30, 30))
  expect_equal(as.numeric(unlist(terra::extract(wt, cbind(500300, 6700300)))), c(5, 6))
})
