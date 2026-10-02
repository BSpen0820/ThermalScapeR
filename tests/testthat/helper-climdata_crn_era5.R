make_era5_fixture <- function(dir, n_hours = 48, vals = list(),
                              start = "2020-05-01 00:00", name = "MRC_fixture.nc") {
  vars <- c("t2m", "d2m", "sp", "u10", "v10", "tp", "avg_sdlwrf", "fdir", "ssrd")
  default <- list(t2m = 280, d2m = 276, sp = 100000, u10 = 1, v10 = 0, tp = 0.0001,
                  avg_sdlwrf = 300, fdir = 3600 * 100, ssrd = 3600 * 150)
  default[names(vals)] <- vals
  tm <- seq(as.POSIXct(start, tz = "UTC"), by = "hour", length.out = n_hours)
  mk <- function(v) {
    r <- terra::rast(nrows = 2, ncols = 2, xmin = -150.625, xmax = -150.125,
                     ymin = 60.375, ymax = 60.875, nlyrs = n_hours,
                     crs = "EPSG:4326", vals = default[[v]])
    terra::time(r) <- tm
    names(r) <- paste0(v, "_", seq_len(n_hours))
    r
  }
  s <- terra::sds(lapply(vars, mk)); names(s) <- vars
  f <- file.path(dir, name)
  terra::writeCDF(s, f, overwrite = TRUE)
  f
}
