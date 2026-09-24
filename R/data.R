#' HLS Fmask Bit Description Table
#'
#' A reference data frame describing the bit positions and values used in the
#' Fmask quality band of the NASA Harmonized Landsat Sentinel-2 (HLS) V2.0
#' product. Used internally by download_hls() for cloud and quality masking.
#'
#' @format A data frame with 16 rows and 4 columns:
#' \describe{
#'   \item{Mask_name}{Name of the mask category}
#'   \item{Bit_Position}{Bit position in the Fmask band}
#'   \item{BitValue}{Bit value corresponding to the description}
#'   \item{description}{Human readable description of the bit value}
#' }
#' @source \url{https://lpdaac.usgs.gov/documents/1698/HLS_User_Guide_V2.pdf}
"fmask_bits"

#' MODIS LAI/FPAR QC Bit Description Table
#'
#' A reference data frame describing the bit positions and values used in the
#' FparLai_QC quality band of the MODIS MCD15A3H LAI/FPAR product. Used
#' internally by download_modis_lai() for quality masking.
#'
#' @format A data frame with 15 rows and 4 columns:
#' \describe{
#'   \item{Mask_name}{Name of the mask category}
#'   \item{Bit_Position}{Bit position in the FparLai_QC band}
#'   \item{BitValue}{Bit value corresponding to the description}
#'   \item{description}{Human readable description of the bit value}
#' }
#' @source \url{https://lpdaac.usgs.gov/documents/624/MOD15_User_Guide_V6.pdf}
"FparLAI_QC"

#' AORC Variable Metadata
#'
#' Lookup table describing meteorological variables available in the
#' Analysis of Record for Calibration (AORC) forcing dataset, including
#' variable labels used in AORC files and their associated units.
#'
#' This dataset can be used to identify required climate variables when
#' preparing AORC data for use in microclimate modeling workflows.
#'
#' @format A data frame with 9 rows and 3 variables:
#' \describe{
#'   \item{ClimateVar}{Human-readable climate variable name.}
#'   \item{VarLabel}{Variable name used in AORC data products.}
#'   \item{Units}{Measurement units of the variable.}
#' }
#'
#' @details
#' Variables included are:
#' \itemize{
#'   \item Total precipitation
#'   \item Air temperature
#'   \item Specific humidity
#'   \item Downward longwave radiation
#'   \item Downward shortwave radiation
#'   \item Atmospheric pressure
#'   \item East-west wind component (U)
#'   \item South-north wind component (V)
#'   \item Diffuse radiation
#' }
#'
#' @source NOAA Analysis of Record for Calibration (AORC)
"AORC_meterodf"

#' Microclim Meteorological Variable Metadata
#'
#' Lookup table describing meteorological variables and naming conventions
#' used by the microclimate modeling functions in this package.
#'
#' This dataset provides standardized variable names and units expected by
#' Microclim-compatible forcing datasets.
#'
#' @format A data frame with 9 rows and 3 variables:
#' \describe{
#'   \item{ClimateVar}{Human-readable climate variable name.}
#'   \item{VarLabel}{Standardized variable name used by Microclim.}
#'   \item{Units}{Measurement units of the variable.}
#' }
#'
#' @details
#' Variables included are:
#' \itemize{
#'   \item Total precipitation
#'   \item Air temperature
#'   \item Relative humidity
#'   \item Downward longwave radiation
#'   \item Downward shortwave radiation
#'   \item Atmospheric pressure
#'   \item Wind speed
#'   \item Wind direction
#'   \item Diffuse radiation
#' }
#'
#' @seealso
#' \code{\link{AORC_meterodf}}
"Microclim_meterodf"

#' Metabolic Chamber Calibration: Microclimate (metout)
#'
#' Hand-crafted synthetic microclimate driver data simulating a metabolic
#' chamber temperature ramp, for use with \code{\link{run_metabolic_chamber}}.
#' Air, ground, and sky temperature all increase by approximately 1 degC each
#' hour across the ramp (see \code{@details}); wind speed, relative humidity,
#' and solar input are held constant to isolate the animal's thermal
#' physiology, the way a real metabolic chamber experiment would.
#'
#' @format A data frame with 288 rows (12 nominal "days" x 24 hourly rows)
#'   and 13 columns:
#' \describe{
#'   \item{JULDAY}{Nominal julian day of the ramp (not a real calendar day -
#'     each of the 12 values represents a distinct ramp segment).}
#'   \item{TIME}{Minutes since midnight (0-1380 in 60-minute steps).}
#'   \item{TALOC, TALOC.1, TALOC.2}{Air temperature (degC) at three heights;
#'     identical in this synthetic dataset. Column names are R's
#'     \code{read.csv()} auto-deduplication of the source file's three
#'     literal \code{TALOC} headers - preserved as-is, not renamed, so
#'     writing this data frame back out reproduces the source file's byte
#'     layout (verified against \code{endo_alomvars_auto_V5.R}'s own
#'     read-then-write round trip of the same file).}
#'   \item{RHLOC, RH}{Relative humidity (percent), held at 5.}
#'   \item{VLOC, VREF}{Wind speed (m/s), held at 0.1 (negligible).}
#'   \item{ZEN}{Zenith angle (degrees).}
#'   \item{SOLR}{Solar radiation (W/m^2), held at 0 (no solar input).}
#'   \item{ELEV}{Elevation (m).}
#'   \item{TANNUL}{Annual mean temperature (degC).}
#' }
#'
#' @details
#' Not observational data - a manually constructed calibration input. See
#' \code{Metabolic Chamber Instructions.docx} (supplied by the NicheMapR
#' maintainers alongside \code{endo_alomvars_auto_V5.R}) for the full
#' rationale. Used alongside \code{\link{metchamber_soil}}.
#'
#' @seealso \code{\link{metchamber_soil}}, \code{\link{run_metabolic_chamber}}
#' @source NicheMapR maintainers, distributed with \code{endo_alomvars_auto_V5.R}.
"metchamber_metout"

#' Metabolic Chamber Calibration: Soil Temperature (soil)
#'
#' Hand-crafted synthetic soil temperature profile paired with
#' \code{\link{metchamber_metout}} for use with
#' \code{\link{run_metabolic_chamber}}. Row order aligns 1:1 with
#' \code{metchamber_metout} (no shared julian-day/time key column - both
#' files are read and consumed in lockstep by the Endotherm model exe).
#'
#' @format A data frame with 288 rows and 11 columns:
#' \describe{
#'   \item{TIME}{Minutes since midnight (0-1380 in 60-minute steps).}
#'   \item{D0cm, D1.5cm, D5cm, D10cm, D15cm, D20cm, D30cm, D50cm, D100cm, D200cm}{
#'     Soil temperature (degC) at each depth, tracking the same ramp as
#'     \code{metchamber_metout}'s air temperature.}
#' }
#'
#' @seealso \code{\link{metchamber_metout}}, \code{\link{run_metabolic_chamber}}
#' @source NicheMapR maintainers, distributed with \code{endo_alomvars_auto_V5.R}.
"metchamber_soil"

#' Remotely Sourced Data in the Microclimate Modeling Pipeline
#'
#' This is a documentation-only page (no associated data object) summarizing
#' every externally sourced dataset used across the preprocessing pipeline in
#' `R/Microclimf_DataPrep.R`, for use as a supplementary methods reference
#' when citing data sources in a manuscript.
#'
#' @section Data sources:
#'
#' | Data Type | Title / Product | Variable(s) Used | Unit | Reference |
#' | --- | --- | --- | --- | --- |
#' | Elevation | Copernicus GLO-30 DEM | Elevation | meters | European Space Agency (2024). *Copernicus Global Digital Elevation Model*. Distributed by OpenTopography. \doi{10.5069/G9028PQB} |
#' | Surface reflectance (primary) | Harmonized Landsat Sentinel-2 (HLS) v2.0 - HLSS30 & HLSL30 | Red, green, blue, NIR surface reflectance | unitless (reflectance, 0-1) | Neigh, C., Ju, J., Roger, J.-C., Skakun, S., Vermote, E., Claverie, M., Dungan, J., Yin, Z., Freitag, B., & Justice, C. (2021). *HLS Sentinel-2 Multi-spectral Instrument Surface Reflectance Daily Global 30 m v2.0 (HLSS30)* and *HLS Landsat Operational Land Imager Surface Reflectance Daily Global 30 m v2.0 (HLSL30)*. NASA EOSDIS Land Processes DAAC. \doi{10.5067/HLS/HLSS30.002}; \doi{10.5067/HLS/HLSL30.002} |
#' | Surface reflectance (alternative) | Copernicus Sentinel-2 MSI Level-2A Surface Reflectance (Harmonized) | Red, green, blue, NIR surface reflectance | unitless (reflectance, 0-1) | European Space Agency (2015). *Sentinel-2 User Handbook* (Issue 1.2, Rev. 2). Copernicus Programme. \url{https://sentinels.copernicus.eu/documents/247904/685211/Sentinel-2_User_Handbook} |
#' | Cloud/shadow mask | Cloud Score+ (S2_HARMONIZED) | Cloud/shadow quality score (`cs`, `cs_cdf`) | unitless (0-1 probability) | Pasquarella, V. J., Brown, C. F., Czerwinski, W., & Rucklidge, W. J. (2023). Comprehensive quality assessment of optical satellite imagery using weakly supervised video learning. *CVPR Workshops (EarthVision)*, 2125-2135. \doi{10.1109/CVPRW59228.2023.00206} |
#' | Leaf Area Index | MODIS MCD15A3H v061 | LAI | m^2^ m^-2^ | Myneni, R., Knyazikhin, Y., & Park, T. (2021). *MODIS/Terra+Aqua Leaf Area Index/FPAR 4-Day L4 Global 500 m SIN Grid V061*. NASA EOSDIS Land Processes DAAC. \doi{10.5067/MODIS/MCD15A3H.061} |
#' | Albedo | MODIS MCD43A3 v061 | Black-sky/white-sky albedo | unitless (fraction, 0-1) | Schaaf, C., & Wang, Z. (2021). *MODIS/Terra+Aqua BRDF/Albedo Daily L3 Global 500 m SIN Grid V061*. NASA EOSDIS Land Processes DAAC. \doi{10.5067/MODIS/MCD43A3.061} |
#' | Soil physical properties | SoilGrids 2.0 | Bulk density, clay, sand, silt fraction | cg cm^-3^ (bulk density); g kg^-1^ (clay/sand/silt) | Poggio, L., de Sousa, L. M., Batjes, N. H., Heuvelink, G. B. M., Kempen, B., Ribeiro, E., & Rossiter, D. (2021). SoilGrids 2.0: producing soil information for the globe with quantified spatial uncertainty. *SOIL*, 7, 217-240. \doi{10.5194/soil-7-217-2021} |
#' | Hourly climate forcing | NOAA Analysis of Record for Calibration (AORC) v1.1 | Precipitation, downward longwave/shortwave radiation, surface pressure, specific humidity, 2 m air temperature, 10 m U/V wind components | mm hr^-1^; W m^-2^; Pa; kg kg^-1^; degC (converted from K); m s^-1^ | Fall, G., Kitzmiller, D., Pavlovic, S., Zhang, Z., Patrick, N., St. Laurent, M., Trypaluk, C., Wu, W., & Miller, D. (2023). The Office of Water Prediction's Analysis of Record for Calibration, Version 1.1: Dataset description and precipitation evaluation. *JAWRA Journal of the American Water Resources Association*, 59(6), 1246-1272. \doi{10.1111/1752-1688.13143}. Data distributed via AWS Open Data: \url{https://registry.opendata.aws/noaa-nws-aorc/} |
#' | Land cover (primary) | National Land Cover Database (NLCD) | Land cover class | categorical | Dewitz, J., and U.S. Geological Survey (2021). *National Land Cover Database (NLCD) 2019 Products* (ver. 2.0, June 2021). U.S. Geological Survey data release. \doi{10.5066/P9KZCM54} |
#' | Land cover (alternative) | ESA WorldCover 10 m 2020 v100 | Land cover class | categorical | Zanaga, D., Van De Kerchove, R., De Keersmaecker, W., Souverijns, N., Brockmann, C., Quast, R., Wevers, J., Grosu, A., Paccini, A., Vergnaud, S., Cartus, O., Santoro, M., Fritz, S., Georgieva, I., Lesiv, M., Carter, S., Herold, M., Li, L., Tsendbazar, N.-E., Ramoino, F., & Arino, O. (2021). *ESA WorldCover 10 m 2020 v100*. Zenodo. \doi{10.5281/zenodo.5571936} |
#' | Vegetation height (primary) | LANDFIRE Existing Vegetation Height (EVH) | Vegetation height class | categorical -> meters | LANDFIRE (2020). *Existing Vegetation Height, LANDFIRE 2.2.0 (LF2020)*. U.S. Department of the Interior, Geological Survey; U.S. Department of Agriculture, Forest Service. \url{https://www.landfire.gov} |
#' | Vegetation height (alternative) | ETH Global Canopy Height 2020 (10 m) | Canopy top height | meters | Lang, N., Jetz, W., Schindler, K., & Wegner, J. D. (2023). A high-resolution canopy height model of the Earth. *Nature Ecology & Evolution*, 7(11), 1778-1789. \doi{10.1038/s41559-023-02206-6} |
#'
#' @section Notes:
#' HLS and Sentinel-2 are alternates in the pipeline (`download_hls()` vs
#' `download_s2()`), not both required for a given run - cite whichever
#' produced the final imagery. Cloud Score+ is a mask rather than a modeled
#' variable, but determines which pixels survive into the reflectance
#' composites and is worth citing on that basis. AORC access dates matter for
#' citation purposes since NOAA periodically updates the Zarr store version.
#' The Fall et al. (2023) journal article is the primary citable reference
#' for AORC v1.1; the AWS registry entry is a secondary data-access citation.
#' The NLCD and ESA WorldCover rows are alternates, not both required for a
#' given run: NLCD is produced by this package's own `NLCD_2_CORINE()`
#' preprocessing step, while ESA WorldCover is obtained via
#' `microclimdata::lcover_download(type = "ESA")` (Earth Engine collection
#' `ESA/WorldCover/v100`) - cite whichever supplied the land cover layer
#' actually used. Similarly, LANDFIRE EVH and the ETH Global Canopy Height
#' product are alternates: LANDFIRE is produced by this package's own
#' `LandfireVegHght_AsNumeric()` preprocessing step, while ETH Global Canopy
#' Height is obtained via `microclimdata::vegheight_download()` (Earth Engine
#' asset `users/nlang/ETH_GlobalCanopyHeight_2020_10m_v1`).
#' All references were verified in 2026-07 against their authoritative
#' source pages (GEE Data Catalog, NASA LP DAAC/Earthdata, USGS, ESA/Copernicus,
#' NOAA, Zenodo, and LANDFIRE); see `ThermalScapeR_DataSources.bib` for the
#' corresponding Zotero-importable BibTeX records.
#'
#' @seealso
#' \code{\link{AORC_meterodf}}, \code{\link{Microclim_meterodf}}
#' @name data_sources
NULL

#' Endotherm species preset table
#'
#' Long-format table of NicheMapR Endotherm model input parameters. One row per
#' parameter, one column per species/season preset. Read by
#' \code{\link{get_endotherm_defaults}}; extend it by editing
#' \code{data-raw/endotherm_species_presets.csv} and re-running
#' \code{data-raw/endotherm_species_presets.R}, or supply your own table via
#' \code{\link{endotherm_preset_template}}.
#'
#' @format A data.frame with columns \code{param} (dotted path, e.g.
#'   \code{animal.body_mass}), \code{type} (\code{"numeric"} or
#'   \code{"character"}), \code{by_julday} (\code{"TRUE"} if the scalar cell is
#'   repeated to one value per julian day), and one column per species. The first
#'   data row is \code{__inherits_from__}: the parent species a column inherits
#'   blank cells from (empty for a root). All cells are character.
#'
#' @source \code{"Female Bighorn - Winter"} is the reference parameterization
#'   extracted from a real NicheMapR Endotherm run (SheepMigrationLoss project,
#'   \code{endo.dat}/\code{alomvars.dat}); Bryan Spencer.
"endotherm_species_presets"
