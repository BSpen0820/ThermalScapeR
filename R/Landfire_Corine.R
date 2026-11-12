# LANDFIRE Existing Vegetation Type (EVT) -> CORINE Land Cover (CLC) crosswalk.
#
# The CLC classes produced here are consumed downstream by
# microclimdata::x_calc() / create_veggrid() (lctype = "CORINE"), which look up
# 3-digit CLC codes in microclimdata's `corinetable` (columns Code, x, gsmax,
# leafd), and by soildata_downscale() via download_soil(water = 512). So the
# default output is 3-digit CLC level-3 codes. Only x (leaf angle), gsmax
# (max stomatal conductance) and leafd (leaf dimension) differ between classes
# downstream, so ties between plausible CLC classes are broken using those
# parameters (see the rationale strings below).

# The 44 CLC level-3 classes, in official order. `grid_code` is the 1-44
# GRID_CODE index used by the CLC raster product (same order as
# microclimdata's corinetable).
.corine_classes <- function() {
  data.frame(
    clc_code = c(
      111L, 112L, 121L, 122L, 123L, 124L, 131L, 132L, 133L, 141L, 142L,
      211L, 212L, 213L, 221L, 222L, 223L, 231L, 241L, 242L, 243L, 244L,
      311L, 312L, 313L, 321L, 322L, 323L, 324L, 331L, 332L, 333L, 334L, 335L,
      411L, 412L, 421L, 422L, 423L,
      511L, 512L, 521L, 522L, 523L
    ),
    grid_code = 1:44,
    clc_label = c(
      "Continuous urban fabric", "Discontinuous urban fabric",
      "Industrial or commercial units", "Road and rail networks and associated land",
      "Port areas", "Airports", "Mineral extraction sites", "Dump sites",
      "Construction sites", "Green urban areas", "Sport and leisure facilities",
      "Non-irrigated arable land", "Permanently irrigated land", "Rice fields",
      "Vineyards", "Fruit trees and berry plantations", "Olive groves",
      "Pastures", "Annual crops associated with permanent crops",
      "Complex cultivation patterns",
      "Agriculture with significant areas of natural vegetation",
      "Agro-forestry areas", "Broad-leaved forest", "Coniferous forest",
      "Mixed forest", "Natural grasslands", "Moors and heathland",
      "Sclerophyllous vegetation", "Transitional woodland-shrub",
      "Beaches, dunes, sands", "Bare rocks", "Sparsely vegetated areas",
      "Burnt areas", "Glaciers and perpetual snow", "Inland marshes",
      "Peat bogs", "Salt marshes", "Salines", "Intertidal flats",
      "Water courses", "Water bodies", "Coastal lagoons", "Estuaries",
      "Sea and ocean"
    ),
    stringsAsFactors = FALSE
  )
}

# Normalise a user/LANDFIRE EVT attribute table: upper-case column names,
# trimmed character columns, required columns present.
.evt_normalise_table <- function(evt_table) {
  if (is.character(evt_table) && length(evt_table) == 1L) {
    if (!file.exists(evt_table)) stop("evt_table file not found: ", evt_table)
    evt_table <- utils::read.csv(evt_table, stringsAsFactors = FALSE,
                                 check.names = FALSE)
  }
  if (!is.data.frame(evt_table)) {
    stop("evt_table must be a data.frame or the path to a LANDFIRE EVT CSV")
  }
  evt_table <- as.data.frame(evt_table, stringsAsFactors = FALSE)
  names(evt_table) <- toupper(trimws(names(evt_table)))
  # some LANDFIRE distributions (e.g. RAT exports) call the class name CLASSNAMES
  if (!"EVT_NAME" %in% names(evt_table) && "CLASSNAMES" %in% names(evt_table)) {
    evt_table$EVT_NAME <- evt_table$CLASSNAMES
  }
  req <- c("VALUE", "EVT_NAME", "EVT_LF", "EVT_PHYS")
  miss <- setdiff(req, names(evt_table))
  if (length(miss)) {
    stop("evt_table is missing required column(s): ", paste(miss, collapse = ", "))
  }
  for (nm in c("EVT_NAME", "EVT_LF", "EVT_PHYS", "EVT_GP_N", "EVT_ORDER",
               "EVT_CLASS", "EVT_SBCLS")) {
    if (!nm %in% names(evt_table)) evt_table[[nm]] <- ""
    v <- as.character(evt_table[[nm]])
    v[is.na(v)] <- ""
    evt_table[[nm]] <- trimws(v)
  }
  evt_table$VALUE <- as.numeric(evt_table$VALUE)
  evt_table
}

# Rule-based crosswalk. Takes an EVT attribute table, returns one row per EVT
# VALUE with the assigned CLC code, the rule that fired, a rationale and an
# `ambiguous` flag for judgement calls worth reviewing. Rules are applied in
# order; the first matching rule wins.
.evt_to_clc_rules <- function(evt_table) {
  evt <- .evt_normalise_table(evt_table)
  n <- nrow(evt)

  lo    <- function(v) tolower(v)
  name  <- lo(evt$EVT_NAME)
  phys  <- lo(evt$EVT_PHYS)
  lf    <- lo(evt$EVT_LF)
  gp    <- lo(evt$EVT_GP_N)
  sbcls <- lo(evt$EVT_SBCLS)
  txt   <- paste(name, gp)           # name + group name for keyword tests
  has   <- function(pattern, x = txt) grepl(pattern, x, perl = TRUE)

  code      <- rep(NA_integer_, n)
  rule      <- rep(NA_character_, n)
  rationale <- rep(NA_character_, n)
  ambiguous <- rep(FALSE, n)
  done      <- rep(FALSE, n)

  assign_rule <- function(cond, clc, id, why, amb = FALSE) {
    cond <- cond & !done
    cond[is.na(cond)] <- FALSE
    if (!any(cond)) return(invisible())
    code[cond]      <<- as.integer(if (length(clc) == 1L) clc else clc[cond])
    rule[cond]      <<- id
    rationale[cond] <<- why
    ambiguous[cond] <<- amb
    done[cond]      <<- TRUE
  }

  # leaf type of a tree class from name/group keywords (riparian, exotic and
  # developed tree classes carry no Conifer/Hardwood physiognomy)
  conifer_kw  <- "pine|spruce|\\bfirs?\\b|hemlock|cedar|cypress|juniper|tamarack|larch|redwood|conifer|sequoia|yew"
  hardwood_kw <- "hardwood|\\boaks?\\b|maple|aspen|cottonwood|birch|alder|willow|\\bash\\b|\\bgum\\b|sweetgum|poplar|mangrove|\\belm\\b|hammock|baygall|bayhead|mesquite|bosque|deciduous|pond-apple|popash|tupelo|sycamore|walnut|hickory|beech|basswood"
  # Priority: keywords in EVT_NAME, then deciduous EVT_SBCLS (-> broadleaf),
  # then keywords in EVT_GP_N, then default broadleaf. EVT_SBCLS "Mixed
  # evergreen-deciduous" means mixed leaf *phenology* (e.g. evergreen and
  # deciduous broadleaves in Caribbean or SE swamp forests), not conifer +
  # broadleaf, so it is not used to infer 313.
  kw_leaf <- function(x) {
    mixed <- grepl("conifer-hardwood|hardwood-conifer|mixed forest", x, perl = TRUE)
    con   <- grepl(conifer_kw, x, perl = TRUE)
    hw    <- grepl(hardwood_kw, x, perl = TRUE)
    ifelse(mixed | (con & hw), 313L, ifelse(con, 312L, ifelse(hw, 311L, NA_integer_)))
  }
  leaf_name <- kw_leaf(name)
  leaf_gp   <- kw_leaf(gp)
  leaf_clc <- ifelse(!is.na(leaf_name), leaf_name,
              ifelse(grepl("^deciduous", sbcls), 311L,
              ifelse(!is.na(leaf_gp), leaf_gp,
              ifelse(grepl("^evergreen", sbcls) & phys == "developed", 312L,
                     311L))))
  tree_kw_hit <- !is.na(leaf_name) | !is.na(leaf_gp)

  is_tree  <- lf == "tree"
  is_shrub <- lf == "shrub"
  is_herb  <- lf %in% c("herb", "agriculture") & !grepl("^agricult", phys)

  # ---- 0. nodata -----------------------------------------------------------
  nod <- evt$VALUE == -9999 | phys == "fill-nodata" | lf == "fill-nodata"
  nod[is.na(nod)] <- FALSE
  rule[nod] <- "nodata"; rationale[nod] <- "Fill-NoData -> NA"; done[nod] <- TRUE

  # ---- 1. non-vegetated / land use -----------------------------------------
  assign_rule(phys == "open water" | lf == "water", 512L, "water",
              "Open water -> 512 Water bodies (also the soildata_downscale water code)")
  assign_rule(phys == "snow-ice" | lf == "snow-ice", 335L, "snow_ice",
              "Snow-Ice / glacier -> 335 Glaciers and perpetual snow")
  assign_rule(grepl("^quarries|strip mine|gravel pit", phys), 131L, "quarries",
              "Quarries/strip mines/gravel pits/well & wind pads -> 131 Mineral extraction sites")
  assign_rule(phys == "developed-high intensity", 111L, "developed_high",
              "Developed-High Intensity -> 111 Continuous urban fabric (as NLCD 24 in NLCD_2_CORINE)")
  assign_rule(phys == "developed-medium intensity", 112L, "developed_medium",
              "Developed-Medium Intensity -> 112 Discontinuous urban fabric (as NLCD 23)")
  assign_rule(phys == "developed-low intensity", 112L, "developed_low",
              "Developed-Low Intensity -> 112 Discontinuous urban fabric (as NLCD 22)")
  assign_rule(phys == "developed-open space", 141L, "developed_open",
              "Developed-Open Space (lawns, parks, golf) -> 141 Green urban areas (as NLCD 21)")
  assign_rule(phys == "developed-roads", 122L, "developed_roads",
              "Developed-Roads -> 122 Road and rail networks")

  # ---- 2. disturbance classes ----------------------------------------------
  assign_rule(has("recently burned"), 334L, "recently_burned",
              "Recently Burned -> 334 Burnt areas")
  assign_rule(has("recently logged"), 324L, "recently_logged",
              "Recently Logged -> 324 Transitional woodland-shrub (CLC 324 explicitly includes clear-cuts and regenerating forest)")
  assign_rule(has("recently disturbed") & is_herb, 321L, "recently_disturbed_herb",
              "Recently disturbed, herb/grass cover -> 321 Natural grasslands (by lifeform)", TRUE)
  assign_rule(has("recently disturbed"), 324L, "recently_disturbed_woody",
              "Recently disturbed, shrub/tree cover -> 324 Transitional woodland-shrub (regenerating)", TRUE)

  # ---- 3. agriculture ------------------------------------------------------
  is_ag <- grepl("^agricult", phys)
  assign_rule(is_ag & has("aquaculture"), 512L, "ag_aquaculture",
              "Aquaculture ponds -> 512 Water bodies (open water surface; CLC has no aquaculture class)", TRUE)
  assign_rule(is_ag & has("vineyard"), 221L, "ag_vineyard", "Vineyard -> 221 Vineyards")
  assign_rule(is_ag & has("orchard|bush fruit|berr"), 222L, "ag_orchard",
              "Orchard / bush fruit and berries -> 222 Fruit trees and berry plantations")
  assign_rule(is_ag & has("pasture|hay"), 231L, "ag_pasture",
              "Pasture and hay -> 231 Pastures (as NLCD 81)")
  assign_rule(is_ag & has("irrigated"), 211L, "ag_crops_irrigated",
              "Cultivated crops and irrigated agriculture -> 211 Non-irrigated arable land (as NLCD 82; LANDFIRE does not separate irrigated from dryland so 212 cannot be assigned reliably)", TRUE)
  assign_rule(is_ag, 211L, "ag_crops",
              "Row/close-grown crops, wheat, sugar cane, fallow/idle cropland -> 211 Non-irrigated arable land (as NLCD 82)")

  # ---- 4. vegetated developed classes (urban/developed forest, shrub, herb)
  is_dev <- phys == "developed"
  assign_rule(is_dev & is_tree, leaf_clc, "developed_tree",
              "Urban/developed tree cover -> forest class by leaf type (EVT_SBCLS deciduous/evergreen/mixed -> 311/312/313) to keep canopy leaf traits; alternative is 141 Green urban areas", TRUE)
  assign_rule(is_dev, 141L, "developed_vegetated",
              "Urban/developed herbaceous or shrub cover -> 141 Green urban areas", TRUE)

  # ---- 5. sparse / barren --------------------------------------------------
  is_sparse <- phys == "sparsely vegetated" | lf %in% c("sparse", "barren")
  assign_rule(is_sparse & has("tidal flat|intertidal"), 423L, "sparse_tidal_flat",
              "Tidal / intertidal flat -> 423 Intertidal flats")
  assign_rule(is_sparse & has("cliff|talus|bedrock|scree|\\brock|lava|outcrop|granitic dome|flatrock|canyon|cinder|pavement|volcanic"),
              332L, "sparse_rock",
              "Cliff, talus, bedrock, scree, lava, rock outcrop -> 332 Bare rocks")
  assign_rule(is_sparse & has("beach|dune|strand|sand|shore|outwash|shell hash"), 331L, "sparse_sand",
              "Beach, dune, coastal strand, unconsolidated shore, outwash -> 331 Beaches, dunes, sands")
  assign_rule(is_sparse & has("playa|salt flat"), 333L, "sparse_playa",
              "Playa / salt flat -> 333 Sparsely vegetated areas (natural; CLC 422 Salines is for exploited salt pans)", TRUE)
  assign_rule(is_sparse, 333L, "sparse_other",
              "Other sparse vegetation (badlands, washes, sparse floodplain, bluffs) -> 333 Sparsely vegetated areas (as NLCD 31)")

  # ---- 6. trees: forest by leaf type ---------------------------------------
  assign_rule(is_tree & phys == "conifer", 312L, "tree_conifer",
              "EVT_PHYS Conifer -> 312 Coniferous forest")
  assign_rule(is_tree & phys == "hardwood", 311L, "tree_hardwood",
              "EVT_PHYS Hardwood -> 311 Broad-leaved forest (includes evergreen broadleaf, e.g. live oak)")
  assign_rule(is_tree & phys == "conifer-hardwood", 313L, "tree_conifer_hardwood",
              "EVT_PHYS Conifer-Hardwood -> 313 Mixed forest")
  assign_rule(is_tree & tree_kw_hit, leaf_clc, "tree_other_keyword",
              "Riparian/swamp/exotic tree class -> forest by leaf type from EVT_NAME keywords (else deciduous EVT_SBCLS -> 311, else EVT_GP_N keywords): conifer genera -> 312, broadleaf genera -> 311, both/conifer-hardwood -> 313. NLCD_2_CORINE instead sends woody wetlands to 411")
  assign_rule(is_tree, leaf_clc, "tree_other_default",
              "Riparian/swamp/exotic tree class without leaf-type keywords -> 311 Broad-leaved (N. American riparian, floodplain and swamp forests are mostly broadleaf, e.g. cottonwood, with conifers possibly co-occurring); NLCD_2_CORINE instead sends woody wetlands to 411", TRUE)

  # ---- 7. wetlands (non-tree) ----------------------------------------------
  non_tree <- !is_tree
  assign_rule(non_tree & has("\\bbog|\\bfen\\b|\\bfens\\b|peat|muskeg|pocosin"), 412L, "wetland_peat",
              "Bog / fen / peatland / pocosin (herb or shrub) -> 412 Peat bogs")
  assign_rule(non_tree & has("tidal|brackish|salt marsh|saltmarsh|estuar|mangrove", name) &
                !has("\\bfresh", name),
              421L, "wetland_salt_marsh",
              "Tidal salt/brackish marsh, estuary marsh -> 421 Salt marshes (fresh/oligohaline tidal marsh is left to the inland-marsh rules)")
  assign_rule(phys == "riparian" & lf == "herb", 411L, "riparian_herb",
              "Riparian/wetland herbaceous -> 411 Inland marshes (as NLCD 95)")
  assign_rule(phys == "riparian" & is_shrub, 324L, "riparian_shrub",
              "Riparian/wetland shrubland (willow, alder, shrub swamp) -> 324 Transitional woodland-shrub: tall deciduous shrubs; 324 leaf traits (x=1, gsmax=0.35, leafd=0.04) fit better than 411 reed-marsh traits (x=1.4, gsmax=0.55, leafd=0.09). NLCD_2_CORINE sends woody wetlands to 411", TRUE)
  assign_rule(lf == "herb" & has("marsh|wet meadow|wetland|swamp|\\bseeps?\\b|slough", name), 411L, "wetland_herb",
              "Marsh / wet meadow herbaceous (non-riparian physiognomy) -> 411 Inland marshes (shrub swamps follow the riparian-shrub logic -> 324)", TRUE)

  # ---- 8. shrublands -------------------------------------------------------
  assign_rule(is_shrub & has("sagebrush|artemisia|\\bsage\\b|steppe|scabland") & !has("aspen", name), 322L, "shrub_sagebrush",
              "Sagebrush shrubland/steppe -> 322 Moors and heathland: low (<1-1.5 m), evergreen, small-leaved, closed-to-open shrub cover is structurally closest to heath; 322 traits (x=0.4, gsmax=0.28, leafd=0.02) suit small-leaved Artemisia better than 324 (leafd=0.04) or 323 (Mediterranean sclerophylls). NLCD_2_CORINE maps all shrub to 324", TRUE)
  assign_rule(is_shrub & has("dwarf|tundra|heath|fell-field|\\balpine|krummholz", name), 322L, "shrub_dwarf_heath",
              "Dwarf-shrub, alpine and tundra shrubland, heath -> 322 Moors and heathland")
  assign_rule(is_shrub & has("chaparral|scrub oak|coastal scrub|mahogany|manzanita|juniper-oak|sclerophyll|maritime scrub|coastal sage"),
              323L, "shrub_sclerophyllous",
              "Chaparral, scrub oak, coastal scrub, mountain mahogany -> 323 Sclerophyllous vegetation (evergreen hard-leaved shrubs)", TRUE)
  assign_rule(is_shrub & has("desert|salt|greasewood|blackbrush|creosote|saltbush|shadscale|succulent|sand shrubland|bursage"),
              333L, "shrub_desert",
              "Desert / salt-desert scrub (creosote, blackbrush, greasewood, saltbush) -> 333 Sparsely vegetated areas: typically <20-30% cover; 333 traits (gsmax=0.22, leafd=0.015) suit small-leaved xeric shrubs. Alternatives 322/324", TRUE)
  assign_rule(is_shrub, 324L, "shrub_other",
              "Other shrubland (deciduous, alder/willow, avalanche, aspen/oak/pinyon-juniper shrub, mesic and ruderal shrub) -> 324 Transitional woodland-shrub (as NLCD 52)")

  # ---- 9. herbaceous -------------------------------------------------------
  assign_rule(lf == "herb" & has("tundra"), 321L, "herb_tundra",
              "Herbaceous tundra -> 321 Natural grasslands (could be 322/333 in CLC practice)", TRUE)
  assign_rule(lf == "herb" & has("dune|beach|strand"), 321L, "herb_dune",
              "Dune/beach grassland (vegetated per LANDFIRE) -> 321 Natural grasslands; CLC would often map as 331", TRUE)
  assign_rule(lf == "herb", 321L, "herb_grassland",
              "Grassland, prairie, meadow, forbland, introduced/ruderal annual and perennial grass -> 321 Natural grasslands (as NLCD 71)")

  # ---- 10. lifeform fallback (only fires for unexpected user tables) --------
  assign_rule(rep(TRUE, n) & is_tree, leaf_clc, "fallback_tree",
              "Unrecognised tree class -> forest by leaf type", TRUE)
  assign_rule(is_shrub, 324L, "fallback_shrub", "Unrecognised shrub class -> 324", TRUE)
  assign_rule(lf %in% c("herb", "agriculture"), 321L, "fallback_herb", "Unrecognised herb class -> 321", TRUE)
  assign_rule(rep(TRUE, n), 333L, "fallback_other",
              "Unrecognised class with no usable lifeform -> 333 Sparsely vegetated areas", TRUE)

  cls <- .corine_classes()
  i <- match(code, cls$clc_code)
  data.frame(
    VALUE     = evt$VALUE,
    EVT_NAME  = evt$EVT_NAME,
    EVT_PHYS  = evt$EVT_PHYS,
    EVT_LF    = evt$EVT_LF,
    EVT_GP_N  = evt$EVT_GP_N,
    EVT_SBCLS = evt$EVT_SBCLS,
    clc_code  = code,
    grid_code = cls$grid_code[i],
    clc_label = cls$clc_label[i],
    rule      = rule,
    rationale = rationale,
    ambiguous = ambiguous,
    stringsAsFactors = FALSE
  )
}

#' Convert LANDFIRE Existing Vegetation Type (EVT) to CORINE Land Cover
#'
#' Reclassifies LANDFIRE Existing Vegetation Type (EVT) values to CORINE Land
#' Cover (CLC) level-3 classes, so LANDFIRE can be used in place of
#' \code{\link{NLCD_2_CORINE}} as the land cover input to the microclimate
#' pipeline (\code{microclimdata::x_calc()} / \code{create_veggrid()} with
#' \code{lctype = "CORINE"}, and \code{\link{download_soil}} with
#' \code{water = 512}).
#'
#' By default the built-in \code{\link{landfire_evt_corine}} crosswalk (built
#' from the LANDFIRE 2025 EVT attribute table, all 1068 national classes) is
#' used. Supply \code{evt_table} (a data.frame or CSV path of a LANDFIRE EVT
#' attribute table, e.g. LF2020/LF2022 \code{*_EVT.csv}) to rebuild the
#' crosswalk from that table with the same rules, e.g. for EVT versions whose
#' codes are not in the LF2025 table.
#'
#' @section Mapping caveats:
#' CORINE is a European land cover scheme and has no exact equivalents for
#' many North American vegetation types, so the crosswalk is approximate.
#' Downstream, microclimdata only uses the class to look up leaf angle
#' (\code{x}), maximum stomatal conductance (\code{gsmax}) and leaf dimension
#' (\code{leafd}); where several CLC classes were plausible, the one whose
#' parameters best match the vegetation was chosen. Notable judgement calls
#' (flagged \code{ambiguous = TRUE} in \code{\link{landfire_evt_corine}}):
#' \itemize{
#'   \item Sagebrush shrubland/steppe -> 322 Moors and heathland; warm/salt
#'     desert scrub -> 333 Sparsely vegetated; chaparral, scrub oak, mountain
#'     mahogany -> 323 Sclerophyllous; other shrubland -> 324.
#'   \item Riparian/swamp forests are mapped to forest by leaf type
#'     (311/312/313) rather than 411 (NLCD_2_CORINE sends NLCD woody wetlands
#'     to 411); riparian shrubland -> 324; riparian herbaceous -> 411.
#'   \item Cultivated crops (including "Irrigated Agriculture") -> 211, as
#'     LANDFIRE does not separate irrigated from dryland cropping.
#'   \item Urban/developed tree classes -> forest by leaf type; urban
#'     herbaceous/shrub classes -> 141.
#' }
#' See the \code{rule}, \code{rationale} and \code{ambiguous} columns of
#' \code{\link{landfire_evt_corine}} for every class.
#'
#' @param x A \code{terra::SpatRaster} of LANDFIRE EVT values (single layer;
#'   categories, if attached, are ignored and the raw EVT VALUEs are used), or
#'   a numeric vector of EVT VALUEs.
#' @param evt_table Optional LANDFIRE EVT attribute table (data.frame or path to
#'   the CSV). Must contain \code{VALUE}, \code{EVT_NAME}, \code{EVT_LF} and
#'   \code{EVT_PHYS}; \code{EVT_GP_N} and \code{EVT_SBCLS} are used if present.
#'   If NULL (default) the built-in LF2025 crosswalk is used.
#' @param code Output code format: \code{"clc"} (default) for 3-digit CLC
#'   level-3 codes (111-523, as used by \code{microclimdata}'s
#'   \code{corinetable} and by \code{NLCD_2_CORINE()}), or \code{"grid"} for
#'   the 1-44 CLC raster GRID_CODE index.
#' @param new_crs Optional CRS to reproject a raster output to (nearest
#'   neighbour). Ignored for vector input.
#' @param crop_template Optional SpatRaster; raster output is reprojected
#'   (nearest neighbour) and cropped to it. Ignored for vector input.
#'
#' @return For a SpatRaster input, a categorical SpatRaster of CLC codes with
#'   CLC labels attached as categories (EVT nodata and unmatched values are NA).
#'   For a vector input, an integer vector of CLC codes (NA for nodata / -9999
#'   / unmatched values). Values not found in the crosswalk raise a warning.
#'
#' @seealso \code{\link{landfire_evt_corine}}, \code{\link{NLCD_2_CORINE}},
#'   \code{\link{LandfireVegHght_AsNumeric}}
#'
#' @examples
#' # vector of EVT values: mixed conifer forest, big sagebrush steppe, open water,
#' # developed-medium, cultivated crops, nodata
#' LandfireEVT_2_CORINE(c(7052, 7125, 7292, 7297, 7755, -9999))
#' LandfireEVT_2_CORINE(c(7052, 7125, 7292), code = "grid")
#'
#' # raster
#' r <- terra::rast(nrows = 2, ncols = 3,
#'                  vals = c(7052, 7125, 7292, 7297, 7755, NA))
#' lc <- LandfireEVT_2_CORINE(r)
#' terra::levels(lc)[[1]]
#'
#' \dontrun{
#' evt <- terra::rast("LF2023_EVT.tif")
#' lc  <- LandfireEVT_2_CORINE(evt, evt_table = "LF2023_EVT.csv",
#'                             crop_template = dem)
#' x   <- microclimdata::x_calc(lc, lctype = "CORINE")
#' }
#' @export
LandfireEVT_2_CORINE <- function(x, evt_table = NULL, code = c("clc", "grid"),
                                 new_crs = NULL, crop_template = NULL) {
  code <- match.arg(code)

  lookup <- if (is.null(evt_table)) {
    ThermalScapeR::landfire_evt_corine
  } else {
    .evt_to_clc_rules(evt_table)
  }
  lookup <- lookup[!is.na(lookup$clc_code), , drop = FALSE]
  out_col <- if (code == "clc") "clc_code" else "grid_code"
  cls <- .corine_classes()

  warn_unmatched <- function(vals) {
    vals <- vals[!is.na(vals) & vals != -9999]
    um <- sort(unique(vals[!vals %in% lookup$VALUE]))
    if (length(um)) {
      warning(sprintf(
        "%d EVT value(s) not in the crosswalk were set to NA: %s%s. Supply the matching LANDFIRE EVT attribute table via evt_table.",
        length(um), paste(utils::head(um, 20), collapse = ", "),
        if (length(um) > 20) ", ..." else ""), call. = FALSE)
    }
  }

  if (inherits(x, "SpatRaster")) {
    if (terra::nlyr(x) != 1L) stop("x must be a single-layer SpatRaster")
    # drop any attached categories so classify works on the raw EVT VALUEs
    if (terra::is.factor(x)) {
      x <- terra::deepcopy(x)
      terra::set.cats(x, layer = 1, value = NULL)
    }
    warn_unmatched(terra::unique(x, na.rm = TRUE)[[1]])
    rcl <- cbind(lookup$VALUE, lookup[[out_col]])
    out <- terra::classify(x, rcl = rcl, others = NA)
    names(out) <- "corine"
    lv <- data.frame(value = cls[[out_col]], label = cls$clc_label)
    terra::set.cats(out, layer = 1, value = lv)

    if (!is.null(crop_template)) {
      out <- terra::crop(terra::project(out, terra::crs(crop_template), method = "near"),
                         crop_template)
    }
    if (!is.null(new_crs)) {
      out <- terra::project(out, new_crs, method = "near")
    }
    return(out)
  }

  if (is.numeric(x) || is.factor(x)) {
    x <- if (is.factor(x)) as.numeric(as.character(x)) else as.numeric(x)
    warn_unmatched(x)
    return(as.integer(lookup[[out_col]][match(x, lookup$VALUE)]))
  }

  stop("x must be a terra SpatRaster or a numeric vector of EVT values")
}
