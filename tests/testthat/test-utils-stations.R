test_that(".station_haversine matches known distances", {
  expect_equal(.station_haversine(-150.44, 60.72, -150.44, 60.72), 0)
  expect_equal(.station_haversine(0, 0, 0, 1), 111.195, tolerance = 1e-3)
  d <- .station_haversine(-150.44, 60.72, c(-150.44, -150.10), c(60.72, 60.20))
  expect_length(d, 2L)
  expect_equal(d[1], 0)
  expect_gt(d[2], 50)
  expect_lt(d[2], 70)
})

test_that(".station_normalize_dates treats a length-2 Date vector as one period", {
  p <- .station_normalize_dates(as.Date(c("2024-01-01", "2024-01-03")))
  expect_false(p$is_df)
  expect_equal(nrow(p$periods), 1L)
  expect_equal(p$periods$start, as.Date("2024-01-01"))
  expect_equal(p$periods$end, as.Date("2024-01-03"))
  expect_identical(p$periods$label, "20240101_to_20240103")
})

test_that(".station_normalize_dates accepts a data.frame with one row per period", {
  df <- data.frame(Start_Dates = as.Date(c("2024-01-01", "2024-06-15")),
                   End_Dates   = as.Date(c("2024-01-31", "2024-06-15")))
  p <- .station_normalize_dates(df)
  expect_true(p$is_df)
  expect_equal(nrow(p$periods), 2L)
  expect_identical(p$periods$label, c("20240101_to_20240131", "20240615_to_20240615"))
})

test_that(".station_normalize_dates accepts a one-row data.frame and a single-day vector", {
  p1 <- .station_normalize_dates(data.frame(Start_Dates = as.Date("2024-01-01"),
                                            End_Dates = as.Date("2024-01-02")))
  expect_true(p1$is_df)
  expect_equal(nrow(p1$periods), 1L)
  p2 <- .station_normalize_dates(as.Date(c("2024-02-10", "2024-02-10")))
  expect_identical(p2$periods$label, "20240210_to_20240210")
})

test_that(".station_normalize_dates rejects malformed input with clear messages", {
  expect_error(.station_normalize_dates(c("2024-01-01", "2024-01-03")), "class Date")
  expect_error(.station_normalize_dates(as.Date(c("2024-01-01", "2024-01-03", "2024-01-05"))),
               "length-2")
  expect_error(.station_normalize_dates(as.Date(c("2024-01-03", "2024-01-01"))),
               "before start")
  expect_error(.station_normalize_dates(as.Date(c("2024-01-01", NA))), "NA")
  expect_error(.station_normalize_dates(data.frame(a = 1)), "Start_Dates")
  expect_error(.station_normalize_dates(data.frame(Start_Dates = as.Date(character()),
                                                   End_Dates = as.Date(character()))),
               "empty")
  bad <- data.frame(Start_Dates = as.Date(c("2024-01-01", "2024-03-10")),
                    End_Dates   = as.Date(c("2024-01-31", "2024-03-01")))
  expect_error(.station_normalize_dates(bad), "before start")
  chr <- data.frame(Start_Dates = "2024-01-01", End_Dates = "2024-01-31")
  expect_error(.station_normalize_dates(chr), "class Date")
})

test_that(".station_coords_to_lonlat handles numeric input and validates it", {
  expect_equal(.station_coords_to_lonlat(c(-150.44, 60.72)), c(lon = -150.44, lat = 60.72))
  expect_error(.station_coords_to_lonlat(c(1, 2, 3)), "c\\(lon, lat\\)")
  expect_error(.station_coords_to_lonlat(c(NA, 2)), "c\\(lon, lat\\)")
  expect_error(.station_coords_to_lonlat(c(-150, 95)), "range")
  expect_error(.station_coords_to_lonlat("Kenai"), "must be")
})

test_that(".station_coords_to_lonlat reprojects sf points from a projected CRS", {
  pt <- sf::st_sfc(sf::st_point(c(-150.44, 60.72)), crs = 4326)
  pt_utm <- sf::st_transform(pt, 32605)
  out <- .station_coords_to_lonlat(pt_utm)
  expect_equal(unname(out), c(-150.44, 60.72), tolerance = 1e-4)
  expect_named(out, c("lon", "lat"))
})

test_that(".station_coords_to_lonlat takes the centroid of sf polygons, SpatVector and SpatRaster", {
  poly <- sf::st_sfc(sf::st_polygon(list(rbind(c(-151, 60), c(-150, 60), c(-150, 61),
                                               c(-151, 61), c(-151, 60)))), crs = 4326)
  out <- .station_coords_to_lonlat(sf::st_sf(id = 1, geometry = poly))
  expect_equal(unname(out), c(-150.5, 60.5), tolerance = 1e-2)

  v <- terra::vect(cbind(-150.44, 60.72), crs = "EPSG:4326")
  expect_equal(unname(.station_coords_to_lonlat(v)), c(-150.44, 60.72), tolerance = 1e-6)

  r <- terra::rast(xmin = -151, xmax = -150, ymin = 60, ymax = 61,
                   resolution = 0.1, crs = "EPSG:4326")
  expect_equal(unname(.station_coords_to_lonlat(r)), c(-150.5, 60.5), tolerance = 1e-2)
})

test_that(".station_coords_to_lonlat requires a CRS on spatial input", {
  pt <- sf::st_sfc(sf::st_point(c(-150.44, 60.72)))
  expect_error(.station_coords_to_lonlat(pt), "CRS")
})
