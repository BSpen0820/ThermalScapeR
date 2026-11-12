# ThermalScapeR

An R package for downloading, preprocessing, and packaging spatial and
remote-sensing data for microclimate and endotherm modeling, built around the
`microclimdata`/[`microclimfPara`](https://github.com/BSpen0820/microclimfPara)
ecosystem and the NicheMapR Endotherm model. Also wraps HPC (SLURM) cluster
workflows for both pipelines into reusable functions.

> **Note:** this package uses
> [`microclimfPara`](https://github.com/BSpen0820/microclimfPara), Bryan
> Spencer's own fork of
> [`microclimf`](https://github.com/ilyamaclean/microclimf), not the base
> `microclimf` package. See that repo for what's changed.

Developed by Bryan Spencer (PhD candidate, University of Idaho) 
as part of a landscape-scale microclimate and thermal-energetics
research workflow.

## What it does

Two pipelines, meant to be run in sequence:

1. **Microclimate data pipeline** — pulls DEM, HLS/Sentinel-2 imagery, MODIS
   LAI and albedo, and soil data from Google Earth Engine; downloads hourly
   AORC climate data from NOAA; packages everything into
   [`microclimf`](https://github.com/ilyamaclean/microclimf)-ready climate,
   vegetation, and soil parameter grids; tiles large domains and runs
   `microclimf`/NicheMapR point models (above-ground + below-ground) locally
   or as a SLURM array job.
2. **Endotherm modeling pipeline** — builds the fixed-format input files the
   NicheMapR Endotherm model executable reads, calibrates an animal
   parameterization against a known resting metabolic rate in a simulated
   metabolic chamber, and runs the calibrated model across every valid cell
   of a tiled study area (again, locally or via SLURM), reconstructing
   full-domain metabolic-rate and water-loss rasters from the result.

See `vignettes/` for a full walkthrough of both pipelines, step by step.

## Installation

Two dependencies are hosted on GitHub only and can't go in `Imports` per CRAN
policy, so install them explicitly first:

```r
# install.packages("remotes")
remotes::install_github("rspatial/luna")
remotes::install_github("ilyamaclean/microclimdata")
remotes::install_github("BSpen0820/microclimfPara")

remotes::install_github("BSpen0820/ThermalScapeR")
```

Google Earth Engine access (via `reticulate`) must be initialized before
calling any GEE-backed function:

```r
reticulate::use_python("path/to/your/conda/env")  # if needed
reticulate::import("ee")$Initialize(project = "your-gee-project")
googledrive::drive_auth()  # separate auth needed for poll_drive()
```

`download_aorc()` additionally requires the Python packages `xarray`, `s3fs`,
`zarr`, and `netCDF4` in the active conda environment; it will attempt
`reticulate::py_install()` for anything missing.

### Lake water temperature (optional)

`run_lake_watertemp()` runs 1D lake models through
[LakeEnsemblR](https://github.com/aemon-j/LakeEnsemblR), which is in `Suggests`
and, with its model packages, must be installed from GitHub (in this order):

```r
remotes::install_github("GLEON/rLakeAnalyzer")
remotes::install_github("aemon-j/GLM3r", ref = "v3.1.1")
remotes::install_github("USGS-R/glmtools", ref = "ggplot_overhaul")
remotes::install_github("aemon-j/FLakeR", ref = "inflow")
remotes::install_github("aemon-j/GOTMr")
remotes::install_github("aemon-j/gotmtools")
remotes::install_github("aemon-j/SimstratR")
remotes::install_github("aemon-j/MyLakeR")
remotes::install_github("aemon-j/LakeEnsemblR")
```

The model packages ship prebuilt executables; the Windows builds are tested,
while the Linux builds are old and may need rebuilding on HPC systems.

### NicheMapR Endotherm model executable

`run_metabolic_chamber()`, `run_endo_big_nichemap()`, and
`run_endotherm_model()` all shell out to a compiled NicheMapR Endotherm model
executable (`Endo2022a.exe`). That executable is third-party, separately
licensed software and is **not distributed with this package** — obtain your
own copy and point the relevant `exe_path` argument at it. On non-Windows
systems it runs under Wine; see `init_wine_prefix()`.

## Vignettes

| # | Vignette | Covers |
|---|----------|--------|
| 01 | `gee-setup` | Authenticating Google Earth Engine and Google Drive |
| 02 | `data-pipeline` | Raw GEE/AORC/soil sources → packaged climate, vegetation, and soil grids |
| 03 | `running-microclimf` | Tiling, running `microclimf`/NicheMapR locally or via SLURM, stitching results |
| 04 | `endotherm-inputs` | Writing `endo.dat`/`alomvars.dat`/`JULDAYS.dat` for an animal parameterization |
| 05 | `metabolic-chamber` | Calibrating a parameterization against a known resting metabolic rate |
| 06 | `endotherm-landscape-scale` | Running the calibrated model across a tiled study area, locally or via SLURM |

## HPC / SLURM support

`run_micro_big_nichemap()` and `run_endo_big_nichemap()` both accept hidden
`clust_array_arg`/`clust_array_size` arguments (passed via `...`) so a single
function call can be driven either interactively or as one task in a SLURM
array job — see vignettes 03 and 06 for worked examples of both. These
functions never submit jobs themselves; SBATCH scripts are written by hand.

## License

MIT — see `LICENSE`.
