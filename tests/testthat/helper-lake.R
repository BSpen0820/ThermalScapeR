.test_square_lake <- function(side = 200, x0 = 500000, y0 = 6700000,
                              crs = "EPSG:32605", id = "A") {
  m <- matrix(c(x0, y0, x0 + side, y0, x0 + side, y0 + side, x0, y0 + side, x0, y0),
              ncol = 2, byrow = TRUE)
  sf::st_sf(lake_id = id, geometry = sf::st_sfc(sf::st_polygon(list(m)), crs = crs))
}
