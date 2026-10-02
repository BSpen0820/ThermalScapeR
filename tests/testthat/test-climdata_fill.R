mk_pair <- function(n = 48, crn_temp = 10, era_temp = 8) {
  tm <- seq(as.POSIXct("2020-05-01 01:00", tz = "UTC"), by = "hour", length.out = n)
  crn <- data.frame(obs_time = tm, temp = crn_temp, relhum = 70, swdown = 100,
                    windspeed = 2, precip = 0.5)
  era <- data.frame(obs_time = tm, temp = era_temp, relhum = 60, pres = 99, swdown = 50,
                    difrad = 20, lwdown = 300, windspeed = 1, winddir = 90, precip = 0.2)
  list(crn = crn, era = era, grid = tm)
}

test_that("coefficients: additive offset and ratio on overlap", {
  expect_equal(.bias_coef(rep(10, 30), rep(8, 30), "add")$value, 2)
  expect_equal(.bias_coef(rep(100, 30), rep(50, 30), "ratio", min_era5 = 10)$value, 2)
})

test_that("too few overlapping hours gives neutral coefficient and warning", {
  expect_warning(r <- .bias_coef(rep(10, 5), rep(8, 5), "add"), "overlap")
  expect_equal(r$value, 0)
  expect_warning(r <- .bias_coef(rep(10, 5), rep(8, 5), "ratio"), "overlap")
  expect_equal(r$value, 1)
})

test_that("all-night / zero ERA5 overlap gives ratio 1 without NaN", {
  r <- suppressWarnings(.bias_coef(rep(0, 30), rep(0, 30), "ratio", min_era5 = 10))
  expect_equal(r$value, 1)
})

test_that("CRN gap is filled with bias-corrected ERA5 and flagged", {
  p <- mk_pair(); p$crn$temp[10:12] <- NA
  r <- .merge_fill(p$crn, p$era, p$grid)
  expect_equal(r$data$temp[10:12], rep(10, 3))        # era 8 + offset 2
  expect_equal(r$data$filled[10:12], rep("temp", 3))
  expect_equal(r$data$filled[1], "")
})

test_that("multiple filled variables are comma-joined; precip is not bias corrected", {
  p <- mk_pair(); p$crn$temp[5] <- NA; p$crn$precip[5] <- NA
  r <- .merge_fill(p$crn, p$era, p$grid)
  expect_equal(r$data$filled[5], "temp,precip")
  expect_equal(r$data$precip[5], 0.2)
})

test_that("bias_correct = FALSE uses raw ERA5", {
  p <- mk_pair(); p$crn$temp[10] <- NA
  r <- .merge_fill(p$crn, p$era, p$grid, bias_correct = FALSE)
  expect_equal(r$data$temp[10], 8)
})

test_that("pres, difrad, lwdown, winddir always come from ERA5; difrad <= swdown", {
  p <- mk_pair(); p$crn$swdown <- 10                  # less than ERA5 difrad of 20
  r <- .merge_fill(p$crn, p$era, p$grid)
  expect_equal(unique(r$data$pres), 99); expect_equal(unique(r$data$lwdown), 300)
  expect_equal(unique(r$data$winddir), 90)
  expect_true(all(r$data$difrad <= r$data$swdown))
})

test_that("wind_factor scales CRN wind; clamps hold", {
  p <- mk_pair(); p$crn$relhum <- 120; p$crn$swdown[1] <- -5
  r <- .merge_fill(p$crn, p$era, p$grid, wind_factor = 1.1)
  expect_equal(r$data$windspeed[1], 2.2)
  expect_true(all(r$data$relhum <= 100)); expect_true(all(r$data$swdown >= 0))
})

test_that("grid hours missing from CRN are filled; hours missing from ERA5 stay NA", {
  p <- mk_pair()
  grid <- c(p$grid[1] - 3600, p$grid, p$grid[length(p$grid)] + 3600)
  r <- .merge_fill(p$crn, p$era, grid)
  expect_equal(nrow(r$data), length(grid))
  expect_true(is.na(r$data$pres[1]) && is.na(r$data$temp[1]))
})
