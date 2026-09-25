toy_presets <- function() {
  data.frame(
    param     = c("__inherits_from__", "a.x", "a.y", "b.parts.p.z", "b.v"),
    type      = c(NA, "numeric", "character", "numeric", "numeric"),
    by_julday = c(NA, "FALSE", "FALSE", "FALSE", "TRUE"),
    Root      = c(NA, "1", "hello", "3", "0.5"),
    Child     = c("Root", "10", NA, NA, NA),
    Grand     = c("Child", NA, "bye", NA, "0.9"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

test_that(".endo_resolve_values resolves a root with type coercion and by_julday expansion", {
  r <- .endo_resolve_values(toy_presets(), "Root", julnum = 3)
  expect_identical(
    r,
    list(a = list(x = 1, y = "hello"),
         b = list(parts = list(p = list(z = 3)), v = c(0.5, 0.5, 0.5)))
  )
})

test_that(".endo_resolve_values layers children over parents, root to leaf", {
  r <- .endo_resolve_values(toy_presets(), "Grand", julnum = 2)
  expect_identical(r$a$x, 10)
  expect_identical(r$a$y, "bye")
  expect_identical(r$b$parts$p$z, 3)
  expect_identical(r$b$v, c(0.9, 0.9))
})

test_that("species matching is exact first, then case-insensitive", {
  expect_identical(.endo_resolve_values(toy_presets(), "grand", 2)$a$x, 10)
})

test_that(".endo_resolve_values reports bad inheritance and bad cells", {
  t <- toy_presets()
  expect_error(.endo_resolve_values(t, "Moose", 3),
               "Unknown species 'Moose'.*Root.*Child.*Grand")

  cyc <- toy_presets(); cyc$Root[1] <- "Grand"
  expect_error(.endo_resolve_values(cyc, "Grand", 3), "cycle")

  unk <- toy_presets(); unk$Child[1] <- "Nope"
  expect_error(.endo_resolve_values(unk, "Child", 3), "unknown species 'Nope'")

  bad <- toy_presets(); bad$Root[2] <- "1,5"
  expect_error(.endo_resolve_values(bad, "Root", 3),
               "Cannot read '1,5' as numeric for param 'a.x'")

  hole <- toy_presets(); hole$Root[3] <- NA
  expect_error(.endo_resolve_values(hole, "Root", 3), "no value for: a.y")
})

test_that(".endo_check_presets trims cells and drops blank rows and columns", {
  t <- toy_presets()
  t$Child[1] <- " Root "
  t$extra <- NA_character_
  names(t)[names(t) == "extra"] <- ""
  t[nrow(t) + 1, ] <- NA
  out <- .endo_check_presets(t, toy_presets())
  expect_identical(names(out), c("param", "type", "by_julday", "Root", "Child", "Grand"))
  expect_identical(out$Child[1], "Root")
  expect_identical(nrow(out), 5L)
})

test_that(".endo_check_presets rejects unknown params and bad headers", {
  t <- toy_presets()
  extra <- data.frame(param = "zzz", type = "numeric", by_julday = "FALSE",
                      Root = "1", Child = NA, Grand = NA, stringsAsFactors = FALSE)
  expect_error(.endo_check_presets(rbind(t, extra), toy_presets()), "unknown param.*zzz")

  dup <- toy_presets(); names(dup)[6] <- "Child"
  expect_error(.endo_check_presets(dup, toy_presets()), "duplicate column names")

  reserved <- toy_presets(); names(reserved)[6] <- "__inherits_from__"
  expect_error(.endo_check_presets(reserved, toy_presets()), "inherits_from")
})

test_that(".endo_check_presets fills params missing from an older custom table with a warning", {
  old <- toy_presets()[toy_presets()$param != "a.y", ]
  expect_warning(out <- .endo_check_presets(old, toy_presets()), "a.y")
  row <- out[out$param == "a.y", ]
  expect_identical(row$Root, "hello")
  expect_true(is.na(row$Child))
  expect_identical(out$param, toy_presets()$param)
})

test_that(".endo_read_csv reads a UTF-8 BOM file written by Excel", {
  plain <- tempfile(fileext = ".csv")
  on.exit(unlink(plain), add = TRUE)
  utils::write.csv(toy_presets(), plain, row.names = FALSE, na = "")
  bom <- tempfile(fileext = ".csv")
  on.exit(unlink(bom), add = TRUE)
  writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), readBin(plain, "raw", file.size(plain))), bom)
  tbl <- .endo_read_csv(bom)
  expect_identical(names(tbl)[1], "param")
  expect_true(is.na(tbl$Root[1]))
})

write_cp1252_preset_csv <- function(path) {
  sp <- "Élan - Winter"
  tbl <- endotherm_preset_template(sp, base_species = "Female Bighorn - Winter")
  tbl[[sp]][tbl$param == "animal.species_label"] <- "Élan"
  tbl[[sp]][tbl$param == "animal.body_mass"] <- "123"
  utf8 <- tempfile(fileext = ".csv")
  on.exit(unlink(utf8), add = TRUE)
  utils::write.csv(tbl, utf8, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  lines <- readLines(utf8, warn = FALSE, encoding = "UTF-8")
  bytes <- iconv(paste0(paste(lines, collapse = "\r\n"), "\r\n"),
                 from = "UTF-8", to = "CP1252", toRaw = TRUE)[[1]]
  writeBin(bytes, path)
  sp
}

test_that("a CP1252 (Excel 'CSV', not 'CSV UTF-8') presets file warns and loses no data", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  sp <- write_cp1252_preset_csv(path)
  expect_false(all(validUTF8(readLines(path, warn = FALSE))))
  expect_warning(d <- get_endotherm_defaults(sp, presets = path), "UTF-8")
  expect_equal(d$animal$body_mass, 123)
  expect_equal(d$animal$species_label, "Élan")
  expect_warning(sps <- list_endotherm_species(presets = path), "UTF-8")
  expect_true(sp %in% sps)
})

test_that("a UTF-8 presets file with a non-ASCII name reads with no warning", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  sp <- "Élan - Winter"
  tbl <- endotherm_preset_template(sp, base_species = "Female Bighorn - Winter")
  tbl[[sp]][tbl$param == "animal.body_mass"] <- "123"
  utils::write.csv(tbl, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  expect_no_warning(d <- get_endotherm_defaults(sp, presets = path))
  expect_equal(d$animal$body_mass, 123)
})

test_that(".endo_set_path builds nested lists and .endo_insert_after keeps order", {
  expect_identical(.endo_set_path(list(), c("a", "b", "c"), 1), list(a = list(b = list(c = 1))))
  expect_identical(.endo_insert_after(list(x = 1, z = 3), "x", "y", 2), list(x = 1, y = 2, z = 3))
  expect_identical(.endo_insert_after(list(x = 1), "x", "y", 2), list(x = 1, y = 2))
})

test_that(".endo_add_derived inserts derived fields in the legacy positions", {
  torso <- list(hair_diameter_dorsal = 1, hair_diameter_ventral = 2,
                hair_length_dorsal = 3, hair_length_ventral = 4,
                fur_depth_dorsal = 5, fur_depth_ventral = 6,
                hair_density_dorsal = 7, hair_density_ventral = 8,
                reflectivity_dorsal = 9, reflectivity_ventral = 10)
  out <- list(
    animal = list(body_mass = 50, mass_by_julday_enabled = 0,
                  body_fat_pct = 10, body_fat_pct_by_julday_enabled = 0),
    fur = list(per_part_fur_enabled = 1, parts = list(torso = torso),
               torso_fur_by_julday_enabled = 0),
    physiology = list(core_temp_target = 38, core_temp_by_julday_enabled = 0,
                      core_skin_temp_diff_min = 0.5)
  )
  res <- .endo_add_derived(out, 2)
  expect_identical(names(res$animal),
                   c("body_mass", "mass_by_julday_enabled", "mass_by_julday",
                     "body_fat_pct", "body_fat_pct_by_julday_enabled", "body_fat_pct_by_julday"))
  expect_identical(res$animal$mass_by_julday, c(50, 50))
  expect_identical(names(res$fur),
                   c(.endo_torso_whole_fields, "per_part_fur_enabled", "parts",
                     "torso_fur_by_julday_enabled",
                     "torso_hair_length_dorsal_by_julday", "torso_hair_length_ventral_by_julday",
                     "torso_fur_depth_dorsal_by_julday", "torso_fur_depth_ventral_by_julday"))
  expect_identical(res$fur$torso_fur_depth_dorsal_by_julday, c(5, 5))
  expect_identical(res$fur$hair_density_ventral, 8)
  expect_identical(names(res$physiology),
                   c("core_temp_target", "core_temp_by_julday_enabled",
                     "core_temp_target_by_julday", "core_skin_temp_diff_min"))
})

test_that("a case-insensitive match to several species is reported as ambiguous", {
  t <- toy_presets()
  t$grand <- NA_character_
  expect_identical(.endo_match_species("Grand", setdiff(names(t), .endo_reserved_cols)), "Grand")
  expect_error(.endo_match_species("GRAND", setdiff(names(t), .endo_reserved_cols)),
               "ambiguous.*'Grand'.*'grand'")
})
