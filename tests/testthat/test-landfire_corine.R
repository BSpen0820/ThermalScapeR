lut <- ThermalScapeR::landfire_evt_corine

test_that("every EVT class maps to a valid CLC level-3 code; nodata -> NA", {
  nod <- lut$VALUE == -9999
  expect_equal(sum(nod), 1L)
  expect_true(all(is.na(lut$clc_code[nod])))
  expect_false(anyNA(lut$clc_code[!nod]))
  expect_false(anyDuplicated(lut$VALUE) > 0)
  expect_true(all(lut$clc_code[!nod] %in% ThermalScapeR:::.corine_classes()$clc_code))
  expect_false(any(grepl("^fallback", lut$rule)))
  expect_false(anyNA(lut$rationale[!nod]))
  # grid_code is consistent with the CLC 1-44 ordering
  cls <- ThermalScapeR:::.corine_classes()
  expect_equal(lut$grid_code[!nod], cls$grid_code[match(lut$clc_code[!nod], cls$clc_code)])
})

test_that("all CLC codes used exist in microclimdata's corinetable", {
  skip_if_not_installed("microclimdata")
  e <- new.env()
  utils::data("corinetable", package = "microclimdata", envir = e)
  expect_true(all(stats::na.omit(lut$clc_code) %in% e$corinetable$Code))
  # the 1-44 grid index follows corinetable's row order
  expect_equal(ThermalScapeR:::.corine_classes()$clc_code, as.integer(e$corinetable$Code[1:44]))
})

test_that("spot checks", {
  f <- function(v) LandfireEVT_2_CORINE(v)
  expect_equal(f(7052), 312L)  # Southern Rocky Mountain Mesic Montane Mixed Conifer Forest
  expect_equal(f(7047), 312L)  # Northern Rocky Mountain Mesic Montane Mixed Conifer Forest
  expect_equal(f(7125), 322L)  # Inter-Mountain Basins Big Sagebrush Steppe
  expect_equal(f(7292), 512L)  # Open Water
  expect_equal(f(7298), 111L)  # Developed-High Intensity
  expect_equal(f(7297), 112L)  # Developed-Medium Intensity
  expect_equal(f(7299), 122L)  # Developed-Roads
  expect_equal(f(7300), 141L)  # Developed-Open Space
  expect_equal(f(7755), 211L)  # Cultivated Crops and Irrigated Agriculture
  expect_equal(f(7754), 231L)  # Pasture and Hay
  expect_equal(f(7961), 221L)  # Vineyard
  expect_equal(f(7960), 222L)  # Orchard
  expect_equal(f(7735), 335L)  # Glacier and Ice Field
  expect_equal(f(7295), 131L)  # Quarries-Strip Mines
  expect_equal(f(7195), 334L)  # Recently Burned-Herb and Grass Cover
  expect_equal(f(9308), 321L)  # Introduced Annual Grassland (cheatgrass)
  expect_equal(f(9055), 412L)  # Boreal-Laurentian Bog
  expect_equal(f(9016), 332L)  # Rocky Mountain Alpine Bedrock and Scree
  expect_equal(f(9012), 311L)  # N Rocky Mtn Lower Montane Riparian Woodland
  expect_equal(f(7161), 312L)  # N Rocky Mtn Conifer Swamp
  expect_equal(f(9197), 421L)  # Tidal Salt Marsh
  expect_equal(f(-9999), NA_integer_)
  expect_equal(f(c(7052, NA, 7292)), c(312L, NA, 512L))
  # aspen (hardwood) and conifer-hardwood
  hw <- lut$VALUE[lut$EVT_PHYS == "Hardwood" & grepl("Aspen", lut$EVT_NAME)][1]
  expect_equal(f(hw), 311L)
  ch <- lut$VALUE[lut$EVT_PHYS == "Conifer-Hardwood"][1]
  expect_equal(f(ch), 313L)
})

test_that("grid code output", {
  expect_equal(LandfireEVT_2_CORINE(c(7052, 7292, -9999), code = "grid"),
               c(24L, 41L, NA))
})

test_that("unmatched values warn and become NA", {
  expect_warning(out <- LandfireEVT_2_CORINE(c(7052, 123456)), "not in the crosswalk")
  expect_equal(out, c(312L, NA))
})

test_that("raster input preserves geometry and NA, and is categorical", {
  r <- terra::rast(nrows = 3, ncols = 3, xmin = 0, xmax = 90, ymin = 0, ymax = 90,
                   crs = "EPSG:26911",
                   vals = c(7052, 7125, 7292, 7297, 7755, NA, -9999, 7300, 7735))
  out <- LandfireEVT_2_CORINE(r)
  expect_s4_class(out, "SpatRaster")
  expect_true(terra::compareGeom(out, r))
  expect_true(terra::is.factor(out))
  v <- terra::values(out, mat = FALSE)
  expect_equal(v, c(312, 322, 512, 112, 211, NA, NA, 141, 335))
  lv <- terra::levels(out)[[1]]
  expect_equal(lv$label[lv$value == 312], "Coniferous forest")

  g <- LandfireEVT_2_CORINE(r, code = "grid")
  expect_equal(terra::values(g, mat = FALSE), c(24, 27, 41, 2, 12, NA, NA, 10, 34))
})

test_that("raster with attached EVT categories is classified on raw values", {
  r <- terra::rast(nrows = 1, ncols = 3, vals = c(7052, 7292, 7125))
  terra::set.cats(r, layer = 1,
                  value = data.frame(VALUE = c(7052, 7125, 7292),
                                     EVT_NAME = c("conifer", "sage", "water")))
  expect_true(terra::is.factor(r))
  out <- LandfireEVT_2_CORINE(r)
  expect_equal(terra::values(out, mat = FALSE), c(312, 512, 322))
})

test_that("user-supplied evt_table re-applies the rules", {
  tbl <- data.frame(
    value = c(-9999, 1, 2, 3, 4),
    evt_name = c("Fill-NoData", "Some Pine Forest", "Big Sagebrush Steppe ",
                 "Open Water", "Mystery Herb"),
    evt_lf = c("Fill-NoData", "Tree", "Shrub", "Water", "Herb"),
    evt_phys = c("Fill-NoData", "Conifer", "Shrubland", "Open Water", "Grassland")
  )
  expect_equal(LandfireEVT_2_CORINE(c(1, 2, 3, 4, -9999), evt_table = tbl),
               c(312L, 322L, 512L, 321L, NA))
  # the shipped table, re-run through the rules, reproduces the built-in lookup
  csv <- testthat::test_path("..", "..", "data-raw", "LF2025_EVT.csv")
  skip_if_not(file.exists(csv), "data-raw not available (installed package)")
  rebuilt <- ThermalScapeR:::.evt_to_clc_rules(csv)
  expect_equal(rebuilt$clc_code, lut$clc_code)
})

test_that("output works end-to-end in microclimdata::x_calc", {
  skip_if_not_installed("microclimdata")
  skip_if_not_installed("withr")
  # x_calc() finds `corinetable` on the search path, so microclimdata must be
  # attached (its lazy data is not visible from microclimdata::x_calc alone)
  withr::local_package("microclimdata")
  r <- terra::rast(nrows = 2, ncols = 3, crs = "EPSG:26911",
                   xmin = 0, xmax = 90, ymin = 0, ymax = 60,
                   vals = c(7052, 7125, 7292, 7297, 7755, NA))
  out <- LandfireEVT_2_CORINE(r)
  e <- new.env()
  utils::data("corinetable", package = "microclimdata", envir = e)
  ct <- e$corinetable
  expected <- ct$x[match(c(312, 322, 512, 112, 211), ct$Code)]
  # with categories attached
  x1 <- suppressWarnings(microclimdata::x_calc(out, lctype = "CORINE"))
  expect_equal(terra::values(x1, mat = FALSE)[1:5], expected)
  # and as a plain numeric raster
  plain <- terra::deepcopy(out)
  terra::set.cats(plain, layer = 1, value = NULL)
  expect_false(terra::is.factor(plain))
  x2 <- suppressWarnings(microclimdata::x_calc(plain, lctype = "CORINE"))
  expect_equal(terra::values(x2, mat = FALSE)[1:5], expected)
})
