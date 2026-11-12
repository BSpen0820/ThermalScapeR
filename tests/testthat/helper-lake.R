.test_square_lake <- function(side = 200, x0 = 500000, y0 = 6700000,
                              crs = "EPSG:32605", id = "A") {
  m <- matrix(c(x0, y0, x0 + side, y0, x0 + side, y0 + side, x0, y0 + side, x0, y0),
              ncol = 2, byrow = TRUE)
  sf::st_sf(lake_id = id, geometry = sf::st_sfc(sf::st_polygon(list(m)), crs = crs))
}

.test_climdata <- function(start, n_hours) {
  data.frame(obs_time = start + 3600 * (seq_len(n_hours) - 1),
             temp = 10, relhum = 70, pres = 100, swdown = 200, difrad = 50,
             lwdown = 300, windspeed = 1, winddir = 180, precip = 1)
}

# 2 x 2 cells of 0.1 deg around (-151, 60.5); temp cell values 1..4 by row
# (top-left = 1), constant otherwise.
.test_packaged_clim <- function(path, start, n_hours) {
  mk <- function(vals) {
    r <- terra::rast(nrows = 2, ncols = 2, xmin = -151.1, xmax = -150.9,
                     ymin = 60.4, ymax = 60.6, crs = "EPSG:4326", nlyrs = n_hours)
    terra::values(r) <- matrix(rep(vals, length.out = 4), nrow = 4, ncol = n_hours)
    terra::time(r) <- start + 3600 * (seq_len(n_hours) - 1)
    terra::wrap(r)
  }
  cl <- list(precip = mk(1), temp = mk(1:4), relhum = mk(70), lwdown = mk(300),
             swdown = mk(200), pres = mk(100), windspeed = mk(1), winddir = mk(180),
             difrad = mk(50))
  saveRDS(cl, path)
  invisible(path)
}
