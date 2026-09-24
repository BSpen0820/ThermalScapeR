# Endotherm species presets: table reader/validator, resolver, public helpers ----

.endo_default_juldays <- c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
.endo_reserved_cols   <- c("param", "type", "by_julday")
.endo_inherit_row     <- "__inherits_from__"
.endo_groups <- c("model_settings", "animal", "fur", "physiology", "diet",
                  "thermoreg", "flying_digging", "nest_shelter", "allometry")
.endo_torso_whole_fields <- c(
  "hair_diameter_dorsal", "hair_diameter_ventral", "hair_length_dorsal",
  "hair_length_ventral", "fur_depth_dorsal", "fur_depth_ventral",
  "hair_density_dorsal", "hair_density_ventral", "reflectivity_dorsal",
  "reflectivity_ventral"
)

.endo_clean_cells <- function(x) {
  x <- trimws(as.character(x))
  x[!is.na(x) & !nzchar(x)] <- NA_character_
  x
}

.endo_read_csv <- function(path) {
  tbl <- utils::read.csv(path, check.names = FALSE, colClasses = "character",
                         na.strings = c("", "NA"), stringsAsFactors = FALSE,
                         fileEncoding = "UTF-8-BOM")
  tbl
}

.endo_check_presets <- function(tbl, builtin) {
  if (!is.data.frame(tbl))
    stop("'presets' must be NULL, a CSV path, or a data.frame")
  names(tbl) <- trimws(names(tbl))
  keep_cols <- nzchar(names(tbl))
  if (anyDuplicated(names(tbl)[keep_cols]))
    stop("presets table has duplicate column names")
  tbl <- tbl[, keep_cols, drop = FALSE]
  tbl[] <- lapply(tbl, .endo_clean_cells)

  if (!all(.endo_reserved_cols %in% names(tbl)))
    stop(sprintf("presets table must have columns: %s",
                 paste(.endo_reserved_cols, collapse = ", ")))
  sp <- setdiff(names(tbl), .endo_reserved_cols)
  if (length(sp) == 0L)
    stop("presets table has no species columns")
  if (.endo_inherit_row %in% sp)
    stop(sprintf("'%s' cannot be used as a species name", .endo_inherit_row))

  tbl <- tbl[!is.na(tbl$param), , drop = FALSE]
  if (anyDuplicated(tbl$param))
    stop(sprintf("presets table has duplicate param(s): %s",
                 paste(unique(tbl$param[duplicated(tbl$param)]), collapse = ", ")))
  if (!.endo_inherit_row %in% tbl$param)
    stop(sprintf("presets table must contain a '%s' row", .endo_inherit_row))

  ref  <- builtin[builtin$param != .endo_inherit_row, , drop = FALSE]
  body <- tbl[tbl$param != .endo_inherit_row, , drop = FALSE]
  inh  <- tbl[tbl$param == .endo_inherit_row, , drop = FALSE]
  inh$type <- NA_character_
  inh$by_julday <- NA_character_

  extra <- setdiff(body$param, ref$param)
  if (length(extra))
    stop(sprintf("presets table has unknown param(s): %s", paste(extra, collapse = ", ")))

  missing <- setdiff(ref$param, body$param)
  if (length(missing)) {
    warning(sprintf(
      "presets table lacks %d built-in param(s); filling them from the built-in root species: %s",
      length(missing), paste(missing, collapse = ", ")))
    ref_root  <- setdiff(names(builtin), .endo_reserved_cols)[1]
    root_cols <- sp[is.na(unlist(inh[1, sp]))]
    idx <- match(missing, ref$param)
    add <- ref[idx, c("param", "type", "by_julday"), drop = FALSE]
    for (s in sp) add[[s]] <- NA_character_
    for (s in root_cols) add[[s]] <- ref[[ref_root]][idx]
    body <- rbind(body[, names(add), drop = FALSE], add)
  }

  body <- body[match(ref$param, body$param), , drop = FALSE]
  body$type <- ref$type
  body$by_julday <- ref$by_julday
  out <- rbind(inh[, names(body), drop = FALSE], body)
  rownames(out) <- NULL
  out
}

.endo_match_species <- function(species, sp_cols) {
  if (!is.character(species) || length(species) != 1L || is.na(species))
    stop("'species' must be a single string")
  hit <- which(sp_cols == species)
  if (length(hit) == 0L) hit <- which(tolower(sp_cols) == tolower(species))
  if (length(hit) != 1L)
    stop(sprintf("Unknown species '%s'. Available: %s", species,
                 paste(sprintf("'%s'", sp_cols), collapse = ", ")))
  sp_cols[hit]
}

.endo_set_path <- function(lst, path, value) {
  if (length(path) == 1L) {
    lst[[path]] <- value
    return(lst)
  }
  child <- if (is.null(lst[[path[1]]])) list() else lst[[path[1]]]
  lst[[path[1]]] <- .endo_set_path(child, path[-1], value)
  lst
}

.endo_insert_after <- function(lst, after, name, value) {
  i <- match(after, names(lst))
  new <- stats::setNames(list(value), name)
  c(lst[seq_len(i)], new, lst[seq_len(length(lst) - i) + i])
}

.endo_resolve_values <- function(tbl, species, julnum) {
  sp_cols <- setdiff(names(tbl), .endo_reserved_cols)
  species <- .endo_match_species(species, sp_cols)
  inh  <- tbl[tbl$param == .endo_inherit_row, , drop = FALSE]
  body <- tbl[tbl$param != .endo_inherit_row, , drop = FALSE]

  chain <- species
  repeat {
    parent <- inh[[chain[1]]]
    if (is.na(parent)) break
    if (!parent %in% sp_cols)
      stop(sprintf("Species '%s' inherits from unknown species '%s'", chain[1], parent))
    if (parent %in% chain)
      stop(sprintf("Inheritance cycle: %s", paste(c(parent, chain), collapse = " -> ")))
    chain <- c(parent, chain)
  }

  vals <- rep(NA_character_, nrow(body))
  for (s in chain) {
    v <- body[[s]]
    has <- !is.na(v)
    vals[has] <- v[has]
  }
  if (anyNA(vals))
    stop(sprintf("Species '%s' (root '%s') has no value for: %s", species, chain[1],
                 paste(body$param[is.na(vals)], collapse = ", ")))

  out <- list()
  for (i in seq_len(nrow(body))) {
    v <- vals[i]
    if (identical(body$type[i], "numeric")) {
      num <- suppressWarnings(as.numeric(v))
      if (is.na(num))
        stop(sprintf("Cannot read '%s' as numeric for param '%s' (species '%s')",
                     v, body$param[i], species))
      v <- num
    }
    if (identical(body$by_julday[i], "TRUE")) v <- rep(v, julnum)
    out <- .endo_set_path(out, strsplit(body$param[i], ".", fixed = TRUE)[[1]], v)
  }
  out
}

.endo_add_derived <- function(out, julnum) {
  torso <- out$fur$parts$torso
  whole <- torso[.endo_torso_whole_fields]
  tor_vecs <- list(
    torso_hair_length_dorsal_by_julday  = rep(torso$hair_length_dorsal, julnum),
    torso_hair_length_ventral_by_julday = rep(torso$hair_length_ventral, julnum),
    torso_fur_depth_dorsal_by_julday    = rep(torso$fur_depth_dorsal, julnum),
    torso_fur_depth_ventral_by_julday   = rep(torso$fur_depth_ventral, julnum)
  )
  out$fur <- c(whole, out$fur, tor_vecs)
  out$animal <- .endo_insert_after(out$animal, "mass_by_julday_enabled",
                                   "mass_by_julday", rep(out$animal$body_mass, julnum))
  out$animal <- .endo_insert_after(out$animal, "body_fat_pct_by_julday_enabled",
                                   "body_fat_pct_by_julday", rep(out$animal$body_fat_pct, julnum))
  out$physiology <- .endo_insert_after(out$physiology, "core_temp_by_julday_enabled",
                                       "core_temp_target_by_julday",
                                       rep(out$physiology$core_temp_target, julnum))
  out
}

.endo_resolve_preset <- function(tbl, species, julnum = 12, juldays = .endo_default_juldays) {
  out <- .endo_resolve_values(tbl, species, julnum)
  out <- .endo_add_derived(out, julnum)
  out$model_settings <- c(list(julnum = julnum, juldays = juldays), out$model_settings)
  out[.endo_groups]
}

.endo_builtin_presets <- function() ThermalScapeR::endotherm_species_presets

.endo_baseline <- function(julnum = 12, juldays = .endo_default_juldays) {
  tbl <- .endo_builtin_presets()
  root <- setdiff(names(tbl), .endo_reserved_cols)[1]
  .endo_resolve_preset(tbl, root, julnum, juldays)
}

.endo_read_presets <- function(presets = NULL) {
  builtin <- .endo_builtin_presets()
  if (is.null(presets)) return(builtin)
  if (is.character(presets) && length(presets) == 1L) {
    if (!file.exists(presets))
      stop(sprintf("'presets' file does not exist:\n  %s", presets))
    presets <- .endo_read_csv(presets)
  }
  .endo_check_presets(presets, builtin)
}

#' Get the default parameter set for write_endotherm_inputs()
#'
#' Returns the parameter set for one animal preset (species/season) as the
#' nine-group named list that \code{write_endotherm_inputs()} takes, so it can
#' be inspected, edited (see \code{\link{override_endotherm_defaults}}), and
#' passed back in.
#'
#' @param species Name of the preset, exactly as it appears in
#'   \code{\link{list_endotherm_species}} (matching is case-insensitive).
#'   Default \code{"Female Bighorn - Winter"}.
#' @param julnum Number of julian days in the model run. Defaults to \code{12}
#'   (one per month). Per-julian-day fields are sized to this value.
#' @param juldays Numeric vector of julian day numbers, length \code{julnum}.
#'   Defaults to the 12 monthly midpoints.
#' @param presets \code{NULL} (default) uses the package's built-in
#'   \code{\link{endotherm_species_presets}}. Otherwise a data.frame, or the path
#'   to a CSV, with the same layout (see \code{\link{endotherm_preset_template}}).
#'
#' @return A named \code{list()} with nine elements - \code{model_settings,
#'   animal, fur, physiology, diet, thermoreg, flying_digging, nest_shelter,
#'   allometry} - using the same names as \code{write_endotherm_inputs()}'s
#'   arguments, suitable for
#'   \code{do.call(write_endotherm_inputs, c(list(output_dir = ...), defaults))}.
#'
#' @details
#' Values come from a preset table: one row per parameter, one column per
#' species. A species column can name a parent (\code{__inherits_from__}) and
#' only fill in the parameters that differ; blank cells inherit. Fields that are
#' derived from others (whole-body fur from the torso, the \code{*_by_julday}
#' twins of \code{body_mass}, \code{body_fat_pct} and \code{core_temp_target})
#' are computed here and are not stored in the table.
#'
#' @examples
#' \dontrun{
#'   list_endotherm_species()
#'   defaults <- get_endotherm_defaults("Female Bighorn - Winter")
#'   str(defaults$animal)
#'   do.call(write_endotherm_inputs, c(list(output_dir = "working_dir"), defaults))
#' }
#'
#' @seealso \code{\link{list_endotherm_species}},
#'   \code{\link{override_endotherm_defaults}},
#'   \code{\link{endotherm_preset_template}}, \code{\link{write_endotherm_inputs}}
#' @export
get_endotherm_defaults <- function(species = "Female Bighorn - Winter",
                                   julnum = 12,
                                   juldays = .endo_default_juldays,
                                   presets = NULL) {
  .chk_vec_len(juldays, julnum, "juldays")
  tbl <- .endo_read_presets(presets)
  .endo_resolve_preset(tbl, species, julnum, juldays)
}

#' List the available Endotherm species presets
#'
#' @param presets \code{NULL} (default) for the built-in table, or a
#'   data.frame / CSV path in the same layout (see
#'   \code{\link{get_endotherm_defaults}}).
#'
#' @return A character vector of preset names, the valid values of
#'   \code{get_endotherm_defaults(species = )}.
#'
#' @details Preset names are the species column headers of the preset table.
#'
#' @examples
#' list_endotherm_species()
#'
#' @seealso \code{\link{get_endotherm_defaults}}, \code{\link{endotherm_preset_template}}
#' @export
list_endotherm_species <- function(presets = NULL) {
  setdiff(names(.endo_read_presets(presets)), .endo_reserved_cols)
}
