# Rebuilds data/landfire_evt_corine.rda: the LANDFIRE EVT -> CORINE Land Cover
# crosswalk used by LandfireEVT_2_CORINE().
#
# Source: data-raw/LF2025_EVT.csv, the LANDFIRE 2025 (LF 2.5) Existing
# Vegetation Type attribute table (all national classes, CONUS + AK + HI +
# PR/VI + Pacific). The mapping rules live in R/Landfire_Corine.R
# (.evt_to_clc_rules) so the same rules can be re-applied at runtime to any
# user-supplied EVT table (LandfireEVT_2_CORINE(evt_table = ...)).
#
# After editing the rules or the CSV: source this file from the package root,
# then devtools::document() / devtools::load_all().
pkgload::load_all(".")

evt <- utils::read.csv(file.path("data-raw", "LF2025_EVT.csv"), stringsAsFactors = FALSE)
landfire_evt_corine <- .evt_to_clc_rules(evt)

# every EVT class except Fill-NoData must map to a valid CLC level-3 code
nod <- landfire_evt_corine$VALUE == -9999
stopifnot(
  nrow(landfire_evt_corine) == nrow(evt),
  !anyDuplicated(landfire_evt_corine$VALUE),
  all(is.na(landfire_evt_corine$clc_code[nod])),
  !anyNA(landfire_evt_corine$clc_code[!nod]),
  all(landfire_evt_corine$clc_code[!nod] %in% .corine_classes()$clc_code),
  !any(grepl("^fallback", landfire_evt_corine$rule))
)

dir.create("data", showWarnings = FALSE)
save(landfire_evt_corine, file = file.path("data", "landfire_evt_corine.rda"),
     compress = "bzip2")
message(sprintf("saved %d EVT classes -> %d CLC classes (%d flagged ambiguous)",
                sum(!nod), length(unique(landfire_evt_corine$clc_code[!nod])),
                sum(landfire_evt_corine$ambiguous)))
