tm <- as.POSIXct("2020-05-01", tz = "UTC") + 3600 * 0:2
mo <- list(FLake = data.frame(datetime = tm, temp = c(2, 4, 6), ice_height = c(0.2, 0, 0)),
           GLM   = data.frame(datetime = tm, temp = c(4, 6, 8), ice_height = c(0.1, 0.1, 0)))

test_that("ensemble mean and ice fraction", {
  e <- .ensemble_lake(mo, tm[1], tm[3])
  expect_equal(e$water_temp, c(3, 5, 7))
  expect_equal(e$ice_frac, c(1, 0.5, 0))
  expect_true(all(c("FLake_temp", "GLM_temp", "FLake_ice", "GLM_ice") %in% names(e)))
})

test_that("window is trimmed to [start, stop] and empty input stops", {
  expect_equal(nrow(.ensemble_lake(mo, tm[2], tm[3])), 2L)
  expect_error(.ensemble_lake(list(), tm[1], tm[3]), "all models failed")
})

test_that("a model with missing hours or long NA runs is dropped with a warning", {
  ok <- data.frame(datetime = tm, temp = 1, ice_height = 0)
  expect_identical(.check_model_output(ok, tm[1], tm[3], "GLM", "A"), ok)
  expect_warning(r <- .check_model_output(ok[1:2, ], tm[1], tm[3], "GLM", "A"), "GLM.*A")
  expect_null(r)
  bad <- ok; bad$temp[2] <- NA
  expect_warning(r <- .check_model_output(bad, tm[1], tm[3], "FLake", "A"), "FLake")
  expect_null(r)
})
