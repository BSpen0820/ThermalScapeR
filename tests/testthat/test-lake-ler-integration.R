test_that("FLake + GLM run on a synthetic 3 m lake", {
  skip_if_not_installed("LakeEnsemblR"); skip_on_cran()
  lk <- .test_square_lake(300)
  b  <- generate_lake_bathymetry(lk, max_depth = 3)
  dem <- terra::rast(terra::ext(b), res = 30, crs = terra::crs(b), vals = 60)
  p  <- prepare_lakes(b, dem, lakes = lk, id_col = "lake_id")
  t0 <- as.POSIXct("2020-05-01", tz = "UTC"); t1 <- t0 + 14 * 86400 - 3600
  met <- .lake_meteo(.test_climdata(t0, 14 * 24), p$lon, p$lat, t0, t1)
  out <- .ler_run_lake(p[1, ], met, t0, t1, c("FLake", "GLM"), Kw = 1,
                       run_dir = withr::local_tempdir())
  expect_named(out, c("FLake", "GLM"), ignore.order = TRUE)
  expect_equal(nrow(out$FLake), 14L * 24L)
  expect_true(all(out$GLM$temp > -1 & out$GLM$temp < 30))
})
