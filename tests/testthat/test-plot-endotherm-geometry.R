.geom_png <- function() {
  path <- tempfile(fileext = ".png")
  grDevices::png(path, width = 1000, height = 500)
  path
}

test_that("plot_endotherm_geometry draws both built-in presets to a png", {
  for (sp in c("Female Alaskan Moose - Winter", "Female Bighorn - Winter")) {
    ei <- get_endotherm_defaults(sp, julnum = 12)
    path <- .geom_png()
    out <- plot_endotherm_geometry(ei, main = sp)
    grDevices::dev.off()
    expect_true(file.exists(path))
    expect_gt(file.size(path), 0)
    expect_named(out, c("parts", "ref_parts", "shoulder_height", "side_xlim", "front_xlim", "ylim"))
    expect_null(out$ref_parts)
    p <- ei$allometry$parts
    expect_equal(out$shoulder_height, (p$front_leg$length + p$torso$diameter_vertical) / 100)
    expect_equal(out$parts["torso", "fd"], p$torso$fur_depth_dorsal_adjusted / 1000)
  }
})

test_that("plot_endotherm_geometry overlays a reference and restores par()", {
  ei  <- get_endotherm_defaults("Female Alaskan Moose - Winter", julnum = 12)
  ref <- get_endotherm_defaults("Female Bighorn - Winter", julnum = 12)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  mfrow <- graphics::par("mfrow"); mar <- graphics::par("mar")
  out <- plot_endotherm_geometry(ei, ref = ref)
  expect_identical(graphics::par("mfrow"), mfrow)
  expect_identical(graphics::par("mar"), mar)
  expect_s3_class(out$ref_parts, "data.frame")
  expect_equal(out$ref_parts["torso", "len"], ref$allometry$parts$torso$length / 100)
})

test_that("plot_endotherm_geometry errors on bad input", {
  ei <- get_endotherm_defaults("Female Alaskan Moose - Winter", julnum = 12)
  expect_error(plot_endotherm_geometry(list()), "allometry$parts", fixed = TRUE)
  expect_error(plot_endotherm_geometry("moose"), "allometry$parts", fixed = TRUE)
  no_torso <- ei
  no_torso$allometry$parts$torso <- NULL
  expect_error(plot_endotherm_geometry(no_torso), "missing part(s): torso", fixed = TRUE)
  bad_len <- ei
  bad_len$allometry$parts$neck$length <- "long"
  expect_error(plot_endotherm_geometry(bad_len), "parts$neck' needs single numeric value(s) for: length", fixed = TRUE)
  expect_error(plot_endotherm_geometry(ei, ref = list()), "'ref'")
})

test_that("plot_endotherm_geometry handles other head geometries, a tail and no front legs", {
  ei <- get_endotherm_defaults("Female Bighorn - Winter", julnum = 12)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  base <- plot_endotherm_geometry(ei)
  expect_lt(base$side_xlim[1], 0)   # padding only, no tail
  for (g in 1:4) {
    eg <- ei
    eg$allometry$parts$head$geometry <- g
    expect_no_error(plot_endotherm_geometry(eg))
  }
  et <- ei
  et$allometry$parts$tail[c("diameter_vertical", "diameter_horizontal", "length")] <- list(5, 5, 30)
  et$allometry$parts$tail$geometry <- 1
  tailed <- plot_endotherm_geometry(et)
  expect_lt(tailed$side_xlim[1], base$side_xlim[1] - 0.25)
  eb <- ei
  eb$allometry$parts$front_leg$geometry <- 0
  biped <- plot_endotherm_geometry(eb)
  expect_equal(biped$shoulder_height,
               (ei$allometry$parts$rear_leg$length + ei$allometry$parts$torso$diameter_vertical) / 100)
})
