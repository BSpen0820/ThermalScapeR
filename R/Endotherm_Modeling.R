# Internal default parameter list for write_juldays_dat() ---------------------

.default_habitat_settings <- function(julnum) {
  list(
    startday  = 1,
    endday    = 365,
    absorp    = rep(0.8, julnum),
    shade_min = rep(15.6, julnum),
    shade_max = rep(16.6, julnum),
    surfwet   = rep(5, julnum),
    multihab  = "N"
  )
}

#' Write a NicheMapR Endotherm model JULDAYS.DAT input file
#'
#' Builds the fixed-format \code{JULDAYS.DAT} file required alongside
#' \code{endo.dat}/\code{alomvars.dat} (see \code{\link{write_endotherm_inputs}})
#' by the NicheMapR Endotherm model executable. Shares the same
#' \code{model_settings} list (specifically \code{julnum}/\code{juldays}) so the
#' day list in \code{JULDAYS.DAT} always matches the one written into
#' \code{endo.dat} — a mismatch between the two causes the exe's fixed-format
#' reader to misparse the file.
#'
#' @param output_dir Directory to write \code{JULDAYS.DAT} into. Written using
#'   this exact filename, because the exe hard-codes it in its working
#'   directory.
#' @param model_settings Named list with \code{julnum} and \code{juldays}
#'   (identical in meaning to the same-named argument of
#'   \code{\link{write_endotherm_inputs}} — pass the same list to both
#'   functions). \code{juldays} must have length \code{julnum}.
#' @param habitat_settings Named list of habitat/substrate settings:
#'   \code{startday, endday} (single integers, the overall simulation day-of-year
#'   bounds), \code{absorp} (substrate absorptivity, length \code{julnum}),
#'   \code{shade_min, shade_max} (minimum/maximum percent shade, each length
#'   \code{julnum}), \code{surfwet} (percent surface wet, length \code{julnum}),
#'   and \code{multihab} (\code{"Y"}/\code{"N"}, multiple habitats flag).
#'
#' @return Invisibly, a log \code{data.frame} with columns \code{file_path,
#'   step, status, timestamp}.
#'
#' @export
write_juldays_dat <- function(output_dir, model_settings = list(), habitat_settings = list()) {

  if (!dir.exists(output_dir))
    stop(sprintf("'output_dir' does not exist:\n  %s", output_dir))

  ms <- utils::modifyList(.endo_baseline()$model_settings, model_settings)
  julnum <- ms$julnum
  .chk_vec_len(ms$juldays, julnum, "model_settings$juldays")

  hs <- utils::modifyList(.default_habitat_settings(julnum), habitat_settings)
  hs$absorp <- round(hs$absorp, 3)
  for (.v in list(list(hs$absorp, "habitat_settings$absorp"),
                  list(hs$shade_min, "habitat_settings$shade_min"),
                  list(hs$shade_max, "habitat_settings$shade_max"),
                  list(hs$surfwet, "habitat_settings$surfwet")))
    .chk_vec_len(.v[[1]], julnum, .v[[2]])

  row1  <- c("Julian Days Start Day End Day ", "\n")
  row2  <- c(" ---------- ------ -------- ", "\n")
  row3  <- c(paste(" ", julnum, hs$startday, hs$endday, ""), "\n")
  row4  <- c(" ", "\n")
  row5  <- c(" Julian Days ", "\n")
  row6  <- c(" ---------- ", "\n")
  row7  <- c(paste0(" ", paste(ms$juldays, collapse = " "), " "), "\n")
  row8  <- c(" ", "\n")
  row9  <- c(" Substrate Absorptivity ", "\n")
  row10 <- c(" ---------- ", "\n")
  row11 <- c(paste0(" ", paste(hs$absorp, collapse = " "), " "), "\n")
  row12 <- c(" ", "\n")
  row13 <- c(" Percent Shade ", "\n")
  row14 <- c(" ---------- ", "\n")
  row15 <- c(paste0(" ", paste(hs$shade_min, collapse = " "), " "), "\n")
  row16 <- c(paste0(" ", paste(hs$shade_max, collapse = " "), " "), "\n")
  row17 <- c(" ", "\n")
  row18 <- c(" Percent Surface Wet ", "\n")
  row19 <- c(" ---------- ", "\n")
  row20 <- c(paste0(" ", paste(hs$surfwet, collapse = " "), " "), "\n")
  row21 <- c(" ", "\n")
  row22 <- c(" Multiple habitats (Y/N) ", "\n")
  row23 <- c(" ---------- ", "\n")
  row24 <- c(paste0(" ", sQuote(hs$multihab, q = FALSE)), "\n")

  juldays_path <- file.path(output_dir, "JULDAYS.DAT")
  cat(row1, row2, row3, row4, row5, row6, row7, row8, row9, row10,
      row11, row12, row13, row14, row15, row16, row17, row18, row19, row20,
      row21, row22, row23, row24,
      file = juldays_path, sep = "")

  log_df <- data.frame(
    file_path = juldays_path,
    step      = "write_juldays_dat",
    status    = "success",
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    stringsAsFactors = FALSE
  )
  invisible(log_df)
}

#' Split a total day count into consecutive equal-size chunks
#'
#' Used to break a multi-day simulation into consecutive blocks (each run as
#' one Endotherm model invocation, with \code{model_settings$julnum} set to
#' \code{chunk_size}) so that, e.g., animal mass can be updated between chunks
#' from one chunk's output before running the next.
#'
#' @param total_days Integer. Total number of days to cover. Must be evenly
#'   divisible by \code{chunk_size}.
#' @param chunk_size Integer. Number of days per chunk (i.e. the
#'   \code{julnum}/\code{juldays} length to use for each Endotherm model
#'   invocation).
#'
#' @return A list of integer vectors, each of length \code{chunk_size},
#'   giving the consecutive day-of-year numbers for that chunk. Together they
#'   cover \code{1:total_days} in order.
#'
#' @export
chunk_days <- function(total_days, chunk_size) {
  if (total_days <= 0 || chunk_size <= 0) {
    stop("total_days and chunk_size must both be positive")
  }
  if (total_days %% chunk_size != 0) {
    stop(sprintf(
      "total_days (%d) must be a multiple of chunk_size (%d)",
      total_days, chunk_size
    ))
  }
  n_chunks <- total_days %/% chunk_size
  lapply(seq_len(n_chunks), function(i) {
    start <- (i - 1L) * chunk_size + 1L
    start:(start + chunk_size - 1L)
  })
}

#' Read valid cell indices from a 1/0 mask raster
#'
#' Identifies which cells of a gridded domain have data to process (value
#' \code{1}) versus not (value \code{0}), for domains where not every cell in
#' the bounding extent was actually run (e.g. a sparse/tiled climate dataset).
#'
#' @param mask_path Path to a raster file readable by \code{terra::rast()}
#'   (e.g. \code{.tif}, \code{.nc}) with cell values of \code{0} or \code{1}.
#'
#' @return Integer vector of 1-indexed cell numbers (in \code{terra}'s native
#'   row-major cell order) where the mask value is \code{1}.
#'
#' @export
read_valid_cell_indices <- function(mask_path) {
  r <- terra::rast(mask_path)
  vals <- terra::values(r, mat = FALSE)
  which(vals == 1)
}

#' Distribute cells round-robin across a SLURM array
#'
#' Mirrors the \code{clust_array_arg}/\code{clust_array_size} SLURM convention
#' used by \code{\link{run_micro_big_nichemap}}: the full set of cells is
#' enumerated and distributed round-robin across \code{clust_array_size} array
#' tasks, and only the current task's subset is returned.
#'
#' @param valid_cell_indices Integer vector of cell indices to distribute
#'   (e.g. from \code{\link{read_valid_cell_indices}}).
#' @param clust_array_arg Integer. Value of \code{$SLURM_ARRAY_TASK_ID} for
#'   this node (1-based). \code{NULL} (default) returns every cell — use this
#'   when not running under a SLURM array.
#' @param clust_array_size Integer. Total number of array tasks. Required
#'   when \code{clust_array_arg} is set.
#'
#' @return An integer vector, the subset of \code{valid_cell_indices} assigned
#'   to this array task.
#'
#' @export
cells_for_array_task <- function(valid_cell_indices, clust_array_arg = NULL, clust_array_size = NULL) {
  if (!is.null(clust_array_arg) &&
      (!is.numeric(clust_array_arg) || length(clust_array_arg) != 1))
    stop("clust_array_arg must be a single numeric value or NULL")
  if (!is.null(clust_array_size) &&
      (!is.numeric(clust_array_size) || length(clust_array_size) != 1))
    stop("clust_array_size must be a single numeric value or NULL")
  if (!is.null(clust_array_arg) && is.null(clust_array_size))
    stop("clust_array_size must be provided when clust_array_arg is set")
  if (!is.null(clust_array_arg) &&
      (clust_array_arg < 1 || clust_array_arg > clust_array_size))
    stop("clust_array_arg must be between 1 and clust_array_size")

  node <- if (is.null(clust_array_size)) rep(1L, length(valid_cell_indices)) else
    rep(seq_len(clust_array_size), length.out = length(valid_cell_indices))

  if (is.null(clust_array_arg)) valid_cell_indices else valid_cell_indices[node == clust_array_arg]
}

#' Run the NicheMapR Endotherm model executable
#'
#' Invokes the compiled Endotherm model exe, which must be pointed at by
#' \code{exe_path} and reads its fixed-format inputs
#' (\code{alomvars.dat}, \code{endo.dat}, \code{JULDAYS.DAT},
#' \code{metout.csv}, \code{shadmet.csv}, \code{soil.csv}, \code{shadsoil.csv})
#' from its current working directory - the exe itself need not live in
#' \code{workspace_dir}. On Windows the exe is run natively. Elsewhere it is
#' run under Wine, either with a private, unshared prefix
#' (\code{wineprefix = NULL}, the default) or against an already-initialized
#' shared prefix (\code{wineprefix} set - see \code{\link{init_wine_prefix}}).
#'
#' @param workspace_dir Directory containing the exe's input files
#'   (\code{alomvars.dat}, \code{endo.dat}, \code{JULDAYS.DAT},
#'   \code{metout.csv}, \code{shadmet.csv}, \code{soil.csv},
#'   \code{shadsoil.csv}). The exe is invoked with this as its working
#'   directory, so its outputs (\code{ErrorMsgs.dat}, \code{HOURPLOT.csv},
#'   etc.) also land here.
#' @param exe_path Character. Full path to the Endotherm model executable -
#'   it does not need to be inside \code{workspace_dir}.
#' @param sysname Character, one of \code{Sys.info()[["sysname"]]}'s possible
#'   values. Determines native vs Wine invocation. Default detects the
#'   current OS.
#' @param wineprefix Character or \code{NULL} (default). \code{NULL} means a
#'   private, unshared Wine prefix is created (via \code{\link{init_wine_prefix}})
#'   just for this call - safe for a single unshared process. A path means a
#'   \emph{shared} prefix (the HPC/parallel case): it must already be
#'   initialized via \code{\link{init_wine_prefix}} - this function
#'   \code{stop()}s if it is not, rather than silently booting it itself
#'   (concurrent first-time boots of the same prefix are what corrupts it;
#'   concurrent use of an already-booted prefix is safe). Ignored on Windows.
#' @param headless Logical. If \code{TRUE}, the Wine invocation (and, for the
#'   \code{wineprefix = NULL} case, its own \code{\link{init_wine_prefix}}
#'   call) is wrapped in \code{xvfb-run}, for compute nodes with no display
#'   server. Default \code{FALSE}. On non-Windows with \code{headless =
#'   FALSE}, this function \code{stop()}s if \code{Sys.getenv("DISPLAY")} is
#'   empty, rather than attempting the call and failing obscurely later.
#'   Ignored on Windows.
#' @param timeout Numeric. Maximum time in seconds to let the exe run before
#'   it is killed. Default \code{0} means no timeout. Passed straight through
#'   to \code{\link[base]{system2}}/\code{\link[base]{system}}, which set a
#'   non-zero exit status when the process is killed for exceeding it - a
#'   guard against a hung exe never producing \code{ErrorMsgs.dat}.
#'
#' @return A list with elements \code{success} (logical) and \code{message}
#'   (character, the contents of \code{ErrorMsgs.dat} plus the process exit
#'   status, or a description of why the run could not be evaluated).
#'
#' @details
#' A Wine prefix that is booted once (via \code{\link{init_wine_prefix}}) and
#' then only read by concurrent \code{wine} invocations is safe to share
#' across many worker processes - what is unsafe is concurrent \emph{first-time
#' creation} of the same prefix, which is why the shared-prefix branch here
#' requires initialization to have already happened.
#'
#' @seealso \code{\link{init_wine_prefix}}, \code{\link{run_metabolic_chamber}},
#'   \code{\link{run_endo_big_nichemap}}
#' @export
run_endotherm_model <- function(workspace_dir, exe_path,
                                 sysname = Sys.info()[["sysname"]],
                                 wineprefix = NULL, headless = FALSE,
                                 timeout = 0) {
  if (!file.exists(exe_path)) {
    return(list(success = FALSE, message = sprintf("exe not found at %s", exe_path)))
  }
  exe_path <- normalizePath(exe_path, mustWork = TRUE)
  # Must be absolute: setwd() below changes the process cwd, so a relative
  # workspace_dir would re-resolve against the new cwd in every later
  # file.path(workspace_dir, ...) (e.g. the ErrorMsgs.dat check).
  workspace_dir <- normalizePath(workspace_dir, mustWork = TRUE)

  unlink(file.path(workspace_dir, c("ErrorMsgs.dat", "HOURPLOT.csv")))

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(workspace_dir)

  if (identical(sysname, "Windows")) {
    exe_output <- system2(exe_path, input = c("alomvars.dat", "endo.dat"), stdout = TRUE, stderr = TRUE,
                           timeout = timeout)
    status <- attr(exe_output, "status")
    if (is.null(status)) status <- 0L
  } else {
    wine <- Sys.which("wine")
    xvfb <- Sys.which("xvfb-run")
    if (!nzchar(wine)) stop("'wine' is not available on PATH")
    if (headless && !nzchar(xvfb)) stop("'xvfb-run' is not available on PATH")
    if (!headless && !nzchar(Sys.getenv("DISPLAY"))) {
      stop("run_endotherm_model: non-Windows, headless = FALSE, but DISPLAY is unset. ",
           "Pass headless = TRUE on a compute node with no display server.")
    }

    if (is.null(wineprefix)) {
      wineprefix <- tempfile(pattern = "wineprefix_", tmpdir = tempdir())
      init_wine_prefix(wineprefix, headless = headless)
      on.exit(unlink(wineprefix, recursive = TRUE, force = TRUE), add = TRUE)
    } else if (!file.exists(file.path(wineprefix, ".update-timestamp"))) {
      stop(sprintf(
        "wineprefix '%s' does not look initialized (no .update-timestamp). ",
        wineprefix),
        "Call init_wine_prefix(wineprefix) once, before any parallel workers start.")
    }

    stderr_log <- file.path(workspace_dir, "wine_stderr.log")
    command <- if (headless) {
      paste(
        "printf 'alomvars.dat\\nendo.dat\\n' |",
        shQuote(xvfb), "-a -e /dev/null",
        "env", paste0("WINEPREFIX=", shQuote(wineprefix)), "WINEDEBUG=-all",
        shQuote(wine), shQuote(exe_path),
        "2>", shQuote(stderr_log)
      )
    } else {
      paste(
        "printf 'alomvars.dat\\nendo.dat\\n' |",
        "env", paste0("WINEPREFIX=", shQuote(wineprefix)), "WINEDEBUG=-all",
        shQuote(wine), shQuote(exe_path),
        "2>", shQuote(stderr_log)
      )
    }
    status <- suppressWarnings(system(command, timeout = timeout))
  }

  error_msgs_path <- file.path(workspace_dir, "ErrorMsgs.dat")
  if (!file.exists(error_msgs_path)) {
    return(list(success = FALSE, message = sprintf("exit status %s; ErrorMsgs.dat was not produced", status)))
  }
  error_msgs <- readLines(error_msgs_path, warn = FALSE)
  list(
    success = any(grepl("Calculations completed\\.", error_msgs)),
    message = paste(c(sprintf("exit status %s", status), error_msgs), collapse = " | ")
  )
}

#' Initialize a shared Wine prefix for the Endotherm model
#'
#' Runs \code{wineboot -u} (optionally under \code{xvfb-run}) to
#' materialize/update a Wine prefix, then blocks (\code{wineserver -w}) until
#' the server has fully quiesced. Intended to be called \strong{once} per
#' node/job, before any parallel workers start - \code{wineboot -u} can
#' return exit status 0 while the registry is still being flushed in the
#' background, so a worker that starts immediately afterward could race that
#' flush without the \code{wineserver -w} wait.
#'
#' @param wineprefix Character. Path to the Wine prefix to initialize.
#'   Created if it does not already exist.
#' @param headless Logical. If \code{TRUE} (default), the initialization is
#'   wrapped in \code{xvfb-run}, for compute nodes with no display server.
#'
#' @return Invisibly, \code{TRUE} on success. \code{stop()}s otherwise.
#'
#' @seealso \code{\link{run_endotherm_model}}, \code{\link{run_endo_big_nichemap}}
#' @export
init_wine_prefix <- function(wineprefix, headless = TRUE) {
  wine <- Sys.which("wine")
  xvfb <- Sys.which("xvfb-run")
  if (!nzchar(wine)) stop("'wine' is not available on PATH")
  if (headless && !nzchar(xvfb)) stop("'xvfb-run' is not available on PATH")

  dir.create(wineprefix, recursive = TRUE, showWarnings = FALSE)

  boot_status <- if (headless) {
    suppressWarnings(system2(
      xvfb,
      c("-a", "-e", "/dev/null", "env", paste0("WINEPREFIX=", wineprefix), "WINEDEBUG=-all", "wineboot", "-u"),
      stdout = FALSE, stderr = FALSE
    ))
  } else {
    suppressWarnings(system2(
      "env",
      c(paste0("WINEPREFIX=", wineprefix), "WINEDEBUG=-all", "wineboot", "-u"),
      stdout = FALSE, stderr = FALSE
    ))
  }
  if (boot_status != 0L) stop("wineboot failed while initializing the shared Wine prefix")

  wait_status <- suppressWarnings(system2(
    "env",
    c(paste0("WINEPREFIX=", wineprefix), "wineserver", "-w"),
    stdout = FALSE, stderr = FALSE
  ))
  if (wait_status != 0L) stop("wineserver -w failed while waiting for the prefix to quiesce")

  invisible(TRUE)
}


# Internal helpers for run_metabolic_chamber() --------------------------------

.mc_scenario_ids <- c("standing_variable", "curled_variable",
                       "curled_constant", "standing_constant")

.mc_ramp_juldays <- c(15, 46, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)

.default_mc_overrides <- function() {
  list(
    model_settings = list(outout = "Y", microin = "CSV", outfile = "CSV", strht = "N"),
    physiology     = list(sweat = "N", pilo = "N"),
    diet           = list(act = rep(1.0, 12), repro = rep(0.0, 12)),
    thermoreg      = list(burrow = "N", nest = "N", climb = "N", shdseek = "N",
                           dive = "N", wind = "N", niteshd = "N", dive2 = "N",
                           shdact = "Y", shdpost = "S", treeslp = "N", hudl = "N",
                           tcconcur = "N", tcconcur2 = "N"),
    flying_digging = list(flight = "N", foss = "N", dig = "N", arb = "N")
  )
}

.mc_scenario_overrides <- function(scenario_id, endo_inputs) {
  active <- if (grepl("^standing", scenario_id)) "Y" else "N"

  posture <- if (grepl("^curled", scenario_id)) {
    list(post3 = "Y", post4 = "N", slpstrt = 3, shdstrt = 3, endpost = 3)
  } else {
    list()
  }

  temp <- if (grepl("constant$", scenario_id)) {
    tcreg <- endo_inputs$physiology$tcreg
    list(tcmax = tcreg + 0.1, tcmin = tcreg - 0.1)
  } else {
    list()
  }

  list(
    diet       = list(diurn = rep(active, 12), noct = rep(active, 12), crep = rep(active, 12)),
    physiology = temp,
    allometry  = posture
  )
}

.mc_target_rmr <- function(endo_inputs) {
  an <- endo_inputs$animal
  if (identical(an$usrmet, "Y")) {
    return(an$met)
  }
  if (!identical(an$class, "MAMMAL")) {
    warning(sprintf(
      "target RMR formula is only implemented for class 'MAMMAL' (got '%s'); target_rmr$trgt will be NA",
      an$class
    ))
    return(NA_real_)
  }
  if (identical(an$marsup, "Y")) {
    (2187 * an$mass ^ 0.737) * (4.185 / 3600)
  } else {
    (70 * an$mass ^ 0.75) * (4.185 / (24 * 3.6))
  }
}

.mc_parse_dimensions <- function(output_path) {
  out <- readLines(output_path, warn = FALSE)
  dimensions <- out[131:136]
  dim_table <- utils::read.table(textConnection(dimensions), header = FALSE)
  masses <- out[129]
  mass_table <- utils::read.table(textConnection(masses), header = FALSE)
  mass_table <- t(mass_table[11:16])
  dim_table <- dim_table[, c(1, 6:8)]
  dim_table$mass <- mass_table[, 1]
  colnames(dim_table) <- c("Body Part", "Vertical or Side-to-Side Diameter (m)",
                            "Horizontal or Front-to-Back Diameter (m)", "Length (m)", "Mass (kg)")
  dim_table[4, 1] <- "Front Legs"
  dim_table[5, 1] <- "Rear Legs"
  dim_table[6, 1] <- "6th Appendage (Tail/Proboscis)"
  dim_table
}

#' Run NicheMapR Endotherm metabolic chamber calibration scenarios
#'
#' Exposes an animal model built from \code{endo_inputs} to a synthetic,
#' hand-crafted temperature ramp (\code{\link{metchamber_metout}}/
#' \code{\link{metchamber_soil}}) across up to 4 canonical scenarios -
#' \{standing, curled\} x \{variable, constant core temperature\} - the way a
#' real metabolic chamber experiment would, and returns the resulting
#' metabolic-rate-vs-temperature data. Each scenario is built independently
#' in its own temporary directory via \code{\link{write_endotherm_inputs}}/
#' \code{\link{write_juldays_dat}}/\code{\link{run_endotherm_model}} - unlike
#' the source script this is ported from, it never reads or overwrites any
#' real \code{endo.dat}/\code{alomvars.dat} already on disk.
#'
#' @param endo_inputs Named list in the same 9-group shape
#'   \code{\link{write_endotherm_inputs}}/\code{\link{get_endotherm_defaults}}
#'   use (\code{model_settings, animal, fur, physiology, diet, thermoreg,
#'   flying_digging, nest_shelter, allometry}) - the animal's real parameters.
#'   \code{model_settings$julnum}/\code{juldays} are always overridden to
#'   \code{12}/the bundled ramp's julian days, regardless of what's supplied
#'   here.
#' @param exe_path Full path to the Endotherm model executable (any
#'   version/filename).
#' @param scenarios Character vector, subset/reorder of
#'   \code{c("standing_variable", "curled_variable", "curled_constant",
#'   "standing_constant")}. Default runs all 4.
#' @param mc_overrides Optional named list, same 9-group shape, merged over
#'   the fixed metabolic-chamber override table (disabled sweating/panting/
#'   diving/burrowing/flying, etc. - see \code{@details}) before each
#'   scenario's own overrides are applied. Changing these changes what
#'   "metabolic chamber simulation" means scientifically for this run.
#' @param save_dir Optional directory. If given, each scenario's
#'   \code{endo.dat}/\code{alomvars.dat} (as actually run) is copied there as
#'   \code{{scenario}_endo.dat}/\code{{scenario}_alomvars.dat}.
#' @param sysname Passed through to \code{\link{run_endotherm_model}}
#'   (Windows vs. Wine invocation). Default detects the current OS.
#' @param headless Logical, passed through to \code{\link{run_endotherm_model}}.
#'   If \code{TRUE}, each scenario's Wine invocation (and its own
#'   \code{\link{init_wine_prefix}} call, for \code{wineprefix = NULL}) is
#'   wrapped in \code{xvfb-run}, for compute nodes with no display server.
#'   Default \code{FALSE}. On non-Windows with \code{headless = FALSE}, the run
#'   \code{stop()}s if \code{Sys.getenv("DISPLAY")} is empty. Ignored on
#'   Windows.
#' @param wineprefix Character or \code{NULL} (default), passed through to
#'   \code{\link{run_endotherm_model}}. \code{NULL} means each scenario gets a
#'   private, unshared Wine prefix. A path means a \emph{shared} prefix that
#'   must already be initialized via \code{\link{init_wine_prefix}}. Ignored on
#'   Windows.
#'
#' @return An object of class \code{"metchamber_result"}: a list with
#'   \code{hourplot} (named list of data frames, one per scenario that
#'   actually succeeded - the relevant hours of \code{HOURPLOT.csv}),
#'   \code{dimensions} (data frame, body-part dimensions parsed from one
#'   successful scenario's \code{OUTPUT} file - scenario-invariant),
#'   \code{target_rmr} (list with \code{trgt} and \code{err}, for plotting
#'   reference lines), and \code{log} (data frame: \code{scenario, success,
#'   message, timestamp}, one row per requested scenario).
#'
#' @details
#' Every scenario always applies this fixed override table on top of
#' \code{endo_inputs} (before \code{mc_overrides} or scenario-specific
#' fields): \code{model_settings} \code{outout/microin/outfile = "Y"/"CSV"/"CSV"},
#' \code{strht = "N"}; \code{physiology} \code{sweat/pilo = "N"};
#' \code{diet} \code{act = rep(1.0, 12)}, \code{repro = rep(0.0, 12)};
#' \code{thermoreg} \code{burrow/nest/climb/shdseek/dive/wind/niteshd/dive2 =
#' "N"}, \code{shdact = "Y"}, \code{shdpost = "S"}, \code{treeslp/hudl/
#' tcconcur/tcconcur2 = "N"}; \code{flying_digging}
#' \code{flight/foss/dig/arb = "N"}. If a scenario's exe run fails, a
#' \code{warning()} is issued and that scenario is omitted from
#' \code{$hourplot} (recorded as a failure in \code{$log}); the function only
#' \code{stop()}s if every requested scenario fails.
#'
#' @seealso \code{\link{write_endotherm_inputs}}, \code{\link{get_endotherm_defaults}},
#'   \code{\link{run_endotherm_model}}, \code{\link{plot.metchamber_result}}
#' @export
run_metabolic_chamber <- function(endo_inputs, exe_path,
                                   scenarios = .mc_scenario_ids,
                                   mc_overrides = list(),
                                   save_dir = NULL,
                                   sysname = Sys.info()[["sysname"]],
                                   headless = FALSE,
                                   wineprefix = NULL) {

  if (!all(scenarios %in% .mc_scenario_ids))
    stop(sprintf("'scenarios' must be from: %s", paste(.mc_scenario_ids, collapse = ", ")))
  if (length(scenarios) == 0)
    stop(sprintf("'scenarios' must name at least one of: %s", paste(.mc_scenario_ids, collapse = ", ")))
  if (!file.exists(exe_path))
    stop(sprintf("'exe_path' does not exist:\n  %s", exe_path))
  if (!is.null(save_dir) && !dir.exists(save_dir))
    stop(sprintf("'save_dir' does not exist:\n  %s", save_dir))

  required_groups <- c("model_settings", "animal", "fur", "physiology", "diet",
                        "thermoreg", "flying_digging", "nest_shelter", "allometry")
  missing_groups <- setdiff(required_groups, names(endo_inputs))
  if (length(missing_groups) > 0)
    stop(sprintf("'endo_inputs' is missing required group(s): %s", paste(missing_groups, collapse = ", ")))

  if (!is.null(endo_inputs$model_settings$julnum) && endo_inputs$model_settings$julnum != 12) {
    message(sprintf(
      "run_metabolic_chamber() always uses julnum = 12 (tied to the bundled temperature-ramp data) - overriding endo_inputs$model_settings$julnum (%d). If endo_inputs' other per-julnum vectors (e.g. animal$mass2) were sized to a different julnum, rebuild endo_inputs with julnum = 12 (e.g. get_endotherm_defaults(julnum = 12)) to avoid a length-mismatch error below.",
      endo_inputs$model_settings$julnum
    ))
  }

  fixed_overrides <- utils::modifyList(.default_mc_overrides(), mc_overrides)
  target_rmr_trgt <- .mc_target_rmr(endo_inputs)  # compute once, before the loop - warns early if class isn't MAMMAL

  hourplot <- list()
  log_rows <- list()
  dimensions <- NULL
  scenario_dirs <- character(0)
  on.exit(unlink(scenario_dirs, recursive = TRUE), add = TRUE)

  for (scenario_id in scenarios) {
    scenario_dir <- tempfile(paste0("mc_", scenario_id, "_"))
    dir.create(scenario_dir)
    scenario_dirs <- c(scenario_dirs, scenario_dir)

    scen_overrides <- .mc_scenario_overrides(scenario_id, endo_inputs)

    merged <- endo_inputs
    for (grp in names(fixed_overrides))
      merged[[grp]] <- utils::modifyList(merged[[grp]], fixed_overrides[[grp]])
    for (grp in names(scen_overrides))
      merged[[grp]] <- utils::modifyList(merged[[grp]], scen_overrides[[grp]])
    merged$model_settings <- utils::modifyList(
      merged$model_settings, list(julnum = 12, juldays = .mc_ramp_juldays)
    )

    write_endotherm_inputs(
      output_dir     = scenario_dir,
      model_settings = merged$model_settings,
      animal         = merged$animal,
      fur            = merged$fur,
      physiology     = merged$physiology,
      diet           = merged$diet,
      thermoreg      = merged$thermoreg,
      flying_digging = merged$flying_digging,
      nest_shelter   = merged$nest_shelter,
      allometry      = merged$allometry
    )

    write_juldays_dat(
      scenario_dir,
      model_settings   = list(julnum = 12, juldays = .mc_ramp_juldays),
      habitat_settings = list(startday = 1, endday = 365,
                               absorp = rep(0.8, 12), shade_min = rep(0, 12),
                               shade_max = rep(100, 12), surfwet = rep(5, 12),
                               multihab = "N")
    )

    utils::write.csv(metchamber_metout, file.path(scenario_dir, "metout.csv"), row.names = FALSE)
    utils::write.csv(metchamber_metout, file.path(scenario_dir, "shadmet.csv"), row.names = FALSE)
    utils::write.csv(metchamber_soil, file.path(scenario_dir, "soil.csv"), row.names = FALSE)
    utils::write.csv(metchamber_soil, file.path(scenario_dir, "shadsoil.csv"), row.names = FALSE)

    if (!is.null(save_dir)) {
      file.copy(file.path(scenario_dir, "endo.dat"),
                file.path(save_dir, sprintf("%s_endo.dat", scenario_id)), overwrite = TRUE)
      file.copy(file.path(scenario_dir, "alomvars.dat"),
                file.path(save_dir, sprintf("%s_alomvars.dat", scenario_id)), overwrite = TRUE)
    }

    run_result <- run_endotherm_model(scenario_dir, exe_path = exe_path, sysname = sysname,
                                      headless = headless, wineprefix = wineprefix)

    log_rows[[scenario_id]] <- data.frame(
      scenario  = scenario_id,
      success   = run_result$success,
      message   = run_result$message,
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      stringsAsFactors = FALSE
    )

    if (!run_result$success) {
      warning(sprintf("run_metabolic_chamber: scenario '%s' failed: %s", scenario_id, run_result$message))
      next
    }

    hourplot_path <- file.path(scenario_dir, "HOURPLOT.csv")
    if (!file.exists(hourplot_path)) {
      warning(sprintf(
        "run_metabolic_chamber: scenario '%s' succeeded but HOURPLOT.csv was not produced", scenario_id
      ))
      next
    }
    hp <- utils::read.csv(hourplot_path, skip = 1)
    hourplot[[scenario_id]] <- hp[c(2:23, 27:48, 52:73, 77:96), ]

    if (is.null(dimensions)) {
      dim_result <- tryCatch(
        .mc_parse_dimensions(file.path(scenario_dir, "OUTPUT")),
        error = function(e) {
          warning(sprintf(
            "run_metabolic_chamber: could not parse body-part dimensions from scenario '%s': %s",
            scenario_id, conditionMessage(e)
          ))
          NULL
        }
      )
      if (!is.null(dim_result)) dimensions <- dim_result
    }
  }

  if (length(hourplot) == 0) {
    failures <- vapply(log_rows, function(r) sprintf("%s: %s", r$scenario, r$message), character(1))
    stop(sprintf(
      "run_metabolic_chamber: every requested scenario failed:\n  %s",
      paste(failures, collapse = "\n  ")
    ))
  }

  log_df <- do.call(rbind, log_rows)
  rownames(log_df) <- NULL

  structure(
    list(
      hourplot   = hourplot,
      dimensions = dimensions,
      target_rmr = list(trgt = target_rmr_trgt, err = endo_inputs$model_settings$err),
      log        = log_df
    ),
    class = "metchamber_result"
  )
}

.mc_comparison_plot <- function(standing_hp, curled_hp, target_rmr, title) {
  standing_hp$Posture <- "Standing"
  curled_hp$Posture <- "Curled"
  combined <- rbind(standing_hp, curled_hp)

  ymax <- max(combined[["MET.W."]], na.rm = TRUE) * 1.05

  ggplot2::ggplot(
    combined,
    ggplot2::aes(x = .data[["TAIR"]], y = .data[["MET.W."]], linetype = .data[["Posture"]])
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_hline(yintercept = target_rmr$trgt * (1 + target_rmr$err),
                         color = "red", linetype = "dashed") +
    ggplot2::geom_hline(yintercept = target_rmr$trgt * (1 - target_rmr$err),
                         color = "blue", linetype = "dashed") +
    ggplot2::coord_cartesian(ylim = c(0, ymax)) +
    ggplot2::labs(title = title, x = "Temperature (C)", y = "Metabolic Rate (W)") +
    ggplot2::theme_minimal()
}

#' Plot metabolic chamber calibration results
#'
#' Builds up to two comparison charts (predicted metabolic rate vs. air
#' temperature, standing vs. curled posture) from a
#' \code{\link{run_metabolic_chamber}} result: one for the variable-core-temp
#' scenarios, one for the constant-core-temp scenarios. Each includes
#' reference lines at the animal's target resting metabolic rate +/- its
#' error margin. A chart is only built if both scenarios in its pair are
#' present in \code{x$hourplot}.
#'
#' @param x A \code{"metchamber_result"} object, as returned by
#'   \code{\link{run_metabolic_chamber}}.
#' @param ... Unused, present for S3 generic compatibility.
#'
#' @return Invisibly, a named list of the ggplot2 objects actually built
#'   (\code{variable_temp}, \code{constant_temp} - only the ones with a
#'   complete scenario pair), so they can be further modified or saved with
#'   \code{ggplot2::ggsave()}. Returns \code{invisible(NULL)} (with a
#'   \code{message()}) if no complete pair is available.
#'
#' @method plot metchamber_result
#' @export
plot.metchamber_result <- function(x, ...) {
  plots <- list()

  if (all(c("standing_variable", "curled_variable") %in% names(x$hourplot))) {
    plots$variable_temp <- .mc_comparison_plot(
      x$hourplot$standing_variable, x$hourplot$curled_variable,
      x$target_rmr, "Metabolic Chamber: Variable Core Temperature"
    )
  }
  if (all(c("standing_constant", "curled_constant") %in% names(x$hourplot))) {
    plots$constant_temp <- .mc_comparison_plot(
      x$hourplot$standing_constant, x$hourplot$curled_constant,
      x$target_rmr, "Metabolic Chamber: Constant Core Temperature"
    )
  }

  if (length(plots) == 0) {
    message("plot.metchamber_result: no complete standing/curled scenario pair available to plot")
    return(invisible(NULL))
  }

  for (p in plots) print(p)
  invisible(plots)
}

# --------------------------------------------------------------------------- #
#  Internal helpers for run_endo_big_nichemap
# --------------------------------------------------------------------------- #

# Fields sharing fur$tmdptorfur - supplying any one forces all four to vector mode.
.endo_torfur_fields <- c("torlend", "torlenv", "tordepd", "tordepv")

# Always-vector diet fields with no toggle - name matches endo_inputs$diet[[name]] directly.
.endo_diet_timevar_fields <- c("digef", "act", "repro", "prtn", "fat", "carb",
                               "dry", "diurn", "noct", "crep", "hibrn", "hibfrac",
                               "land", "land2")

.endo_validate_timevar_lengths <- function(time_varying, n_days) {
  for (nm in names(time_varying)) {
    v <- time_varying[[nm]]
    if (!is.null(v) && length(v) != n_days) {
      stop(sprintf("time_varying$%s must have length %d (the sim window's day count), got %d",
                   nm, n_days, length(v)))
    }
  }
  invisible(TRUE)
}

.endo_apply_timevar <- function(endo_inputs, time_varying, chunk_idx) {
  if (!is.null(time_varying$mass2)) {
    endo_inputs$animal$timdepmass <- 1
    endo_inputs$animal$mass2 <- time_varying$mass2[chunk_idx]
  }
  if (!is.null(time_varying$fatpct2)) {
    endo_inputs$animal$timdepfat <- 1
    endo_inputs$animal$fatpct2 <- time_varying$fatpct2[chunk_idx]
  }
  if (!is.null(time_varying$tcreg2)) {
    endo_inputs$physiology$tmdptc <- 1
    endo_inputs$physiology$tcreg2 <- time_varying$tcreg2[chunk_idx]
  }

  is_torfur_supplied <- !vapply(time_varying[.endo_torfur_fields], is.null, logical(1))
  if (any(is_torfur_supplied)) {
    endo_inputs$fur$tmdptorfur <- 1
    n_days <- length(time_varying[[.endo_torfur_fields[which(is_torfur_supplied)[1]]]])
    for (fld in .endo_torfur_fields) {
      if (!is.null(time_varying[[fld]])) {
        endo_inputs$fur[[fld]] <- time_varying[[fld]][chunk_idx]
      } else {
        static_val <- endo_inputs$fur[[fld]][1]
        endo_inputs$fur[[fld]] <- rep(static_val, n_days)[chunk_idx]
      }
    }
  }

  for (fld in .endo_diet_timevar_fields) {
    if (!is.null(time_varying[[fld]])) {
      endo_inputs$diet[[fld]] <- time_varying[[fld]][chunk_idx]
    }
  }

  endo_inputs
}

# Static (non-time-varying) julnum-length fields carry whatever julnum the
# caller originally built endo_inputs with (e.g. get_endotherm_defaults()'s
# default of 12); a chunk's actual julnum (its day count) is only known once
# run_endo_big_nichemap() is inside its per-chunk loop. This re-derives each
# such field's constant value at the chunk's julnum, from its first element.
# Called before .endo_apply_timevar() at every real call site in this
# codebase, idx (and hence chunk_idx) always has length julnum, so
# .endo_apply_timevar()'s slices are already the right length and this
# ordering makes no observable difference there - it's used here only
# because "prepare base field sizes, then apply selective per-chunk
# overrides" is the clearer read.
.endo_resize_static_field <- function(v, julnum) {
  if (!is.null(v) && length(v) != julnum) v <- rep(v[1], julnum)
  v
}

.endo_resize_static_fields <- function(endo_inputs, julnum) {
  endo_inputs$animal$mass2      <- .endo_resize_static_field(endo_inputs$animal$mass2, julnum)
  endo_inputs$animal$fatpct2    <- .endo_resize_static_field(endo_inputs$animal$fatpct2, julnum)
  endo_inputs$physiology$tcreg2 <- .endo_resize_static_field(endo_inputs$physiology$tcreg2, julnum)
  for (fld in .endo_torfur_fields)
    endo_inputs$fur[[fld]] <- .endo_resize_static_field(endo_inputs$fur[[fld]], julnum)
  for (fld in .endo_diet_timevar_fields)
    endo_inputs$diet[[fld]] <- .endo_resize_static_field(endo_inputs$diet[[fld]], julnum)
  endo_inputs
}

.endo_gref_path <- function(refl_dir, study_area, year_month) {
  ym_parts <- strsplit(year_month, "_")[[1]]
  fname <- if (!is.null(study_area)) {
    sprintf("GF_Refl_%s_%s_%s.tif", study_area, ym_parts[1], ym_parts[2])
  } else {
    sprintf("GF_Refl_%s_%s.tif", ym_parts[1], ym_parts[2])
  }
  file.path(refl_dir, "Gref", fname)
}

.endo_absorp_lookup <- function(refl_dir, study_area, sim_dates, cell_xy) {
  year_months <- unique(format(sim_dates, "%Y_%m"))
  values <- matrix(NA_real_, nrow = nrow(cell_xy), ncol = length(year_months),
                   dimnames = list(NULL, year_months))

  for (ym in year_months) {
    gref_path <- .endo_gref_path(refl_dir, study_area, ym)
    if (!file.exists(gref_path)) stop(sprintf("Gref file not found for %s:\n  %s", ym, gref_path))
    gref_r <- terra::rast(gref_path)
    gref_vals <- terra::extract(gref_r, cell_xy)[, 1]
    values[, ym] <- 1 - gref_vals
  }

  list(values = values, year_month = year_months)
}

.normalize_dates_with_sim_window <- function(dates) {
  if (is.data.frame(dates)) {
    if (!all(c("Start_Dates", "End_Dates") %in% names(dates)))
      stop("dates data.frame must contain columns 'Start_Dates' and 'End_Dates'")
    d <- dates
  } else if (inherits(dates, "Date") && length(dates) == 2) {
    d <- data.frame(Start_Dates = as.Date(dates[1]), End_Dates = as.Date(dates[2]),
                    stringsAsFactors = FALSE)
  } else {
    stop("dates must be a data.frame with Start_Dates/End_Dates columns, or a length-2 Date vector")
  }
  if (is.null(d$Sim_Start)) d$Sim_Start <- d$Start_Dates
  if (is.null(d$Sim_End))   d$Sim_End   <- d$End_Dates
  d
}

.endo_chunk_bounds <- function(n_days, chunk_size) {
  n_chunks <- ceiling(n_days / chunk_size)
  bounds <- floor(seq(0, n_days, length.out = n_chunks + 1))
  lapply(seq_len(n_chunks), function(k) (bounds[k] + 1):bounds[k + 1])
}

.endo_period_label <- function(start_date, end_date) {
  sprintf("%s_to_%s", format(start_date, "%Y%m%d"), format(end_date, "%Y%m%d"))
}

.endo_variable_column <- function(variable) {
  variable <- match.arg(variable, c("metabolic_rate", "water_loss"))
  list(
    metabolic_rate = list(column = "MET.W.",   units = "W",     long_name = "Predicted metabolic rate"),
    water_loss     = list(column = "EVP.G.S.", units = "g s-1", long_name = "Predicted evaporative water loss")
  )[[variable]]
}

.endo_read_hourplot_chunk <- function(path, chunk_start, chunk_end, variable_col) {
  first_line <- readLines(path, n = 1)
  # New layout's header starts with a (possibly quoted) "HR" as the first
  # field; the old layout's line 1 is always a free-text metadata line
  # ("Animal species = ...") that never matches this.
  skip <- if (grepl("^\\s*\"?HR\"?\\s*,", first_line)) 0L else 1L

  hp <- utils::read.csv(path, skip = skip)
  hp <- hp[hp$HR != 24, , drop = FALSE]
  n <- nrow(hp)

  expected_n <- 24L * (as.integer(chunk_end - chunk_start) + 1L)
  if (n != expected_n) {
    warning(sprintf(
      "Skipping %s: row count %d does not match the %d real hours expected for %s..%s",
      path, n, expected_n, chunk_start, chunk_end
    ))
    return(data.frame(timestamp = as.POSIXct(character(0), tz = "UTC"), value = numeric(0)))
  }

  day_offset  <- (seq_len(n) - 1L) %/% 24L
  hour_offset <- (seq_len(n) - 1L) %% 24L
  timestamp <- as.POSIXct(chunk_start, tz = "UTC") + day_offset * 86400 + hour_offset * 3600

  data.frame(timestamp = timestamp, value = hp[[variable_col]])
}

.endo_scoped_search_dir <- function(root_dir, period_label) {
  # Both patterns are checked together, not short-circuited independently -
  # if root_dir happens to contain two scenario trees for the same period
  # (one matching each pattern), that's exactly the collision
  # .endo_check_duplicate_chunks() exists to catch. Narrowing to just the
  # first match found would silently drop the second tree instead of
  # falling through to the full-root_dir scan that surfaces it.
  direct <- file.path(root_dir, period_label)
  nested <- Sys.glob(file.path(root_dir, "*", period_label))
  candidates <- unique(c(direct[dir.exists(direct)], nested[dir.exists(nested)]))

  if (length(candidates) == 1) return(candidates)
  root_dir
}

.endo_discover_hourplot_files <- function(root_dir) {
  paths <- list.files(root_dir, pattern = "^HOURPLOT_chunk.*\\.csv$",
                      recursive = TRUE, full.names = TRUE)
  paths <- gsub("\\\\", "/", paths)

  pat <- "Tile_([0-9]+)/Cell_0*([0-9]+)/HOURPLOT_chunk[0-9]+_([0-9]{8})_([0-9]{8})\\.csv$"
  m <- regmatches(paths, regexec(pat, paths))
  keep <- vapply(m, function(x) length(x) == 5, logical(1))

  if (!any(keep)) {
    return(data.frame(tile_id = integer(0), cell_id = integer(0),
                      chunk_start = as.Date(character(0)), chunk_end = as.Date(character(0)),
                      path = character(0), stringsAsFactors = FALSE))
  }

  m <- m[keep]
  data.frame(
    tile_id     = as.integer(vapply(m, `[[`, character(1), 2)),
    cell_id     = as.integer(vapply(m, `[[`, character(1), 3)),
    chunk_start = as.Date(vapply(m, `[[`, character(1), 4), format = "%Y%m%d"),
    chunk_end   = as.Date(vapply(m, `[[`, character(1), 5), format = "%Y%m%d"),
    path        = paths[keep],
    stringsAsFactors = FALSE
  )
}

.endo_discover_manifests <- function(root_dir, period_label) {
  paths <- list.files(root_dir, pattern = "manifest.*\\.csv$",
                      recursive = TRUE, full.names = TRUE)
  paths <- gsub("\\\\", "/", paths)
  paths <- paths[grepl(period_label, paths, fixed = TRUE)]
  paths <- paths[grepl("Tile_[0-9]+", basename(paths))]  # avoid NA + coercion warning below

  if (length(paths) == 0) {
    return(data.frame(tile_id = integer(0), path = character(0), stringsAsFactors = FALSE))
  }

  tile_id <- as.integer(sub(".*Tile_([0-9]+).*", "\\1", basename(paths)))
  data.frame(tile_id = tile_id, path = paths, stringsAsFactors = FALSE)
}

.endo_check_duplicate_chunks <- function(hourplot_files) {
  if (nrow(hourplot_files) == 0) return(invisible(TRUE))
  key <- paste(hourplot_files$tile_id, hourplot_files$cell_id, hourplot_files$chunk_start, sep = "|")
  dup_keys <- unique(key[duplicated(key)])
  if (length(dup_keys) > 0) {
    dup_paths <- hourplot_files$path[key %in% dup_keys]
    stop(sprintf(
      "Found duplicate (tile_id, cell_id, chunk_start) chunk files under root_dir - root_dir must point at a single scenario's output. Colliding paths:\n  %s",
      paste(dup_paths, collapse = "\n  ")
    ))
  }
  invisible(TRUE)
}

.endo_assemble_cell_series <- function(chunk_files_df, sim_start, sim_end, variable_col) {
  expected_hours <- seq(as.POSIXct(sim_start, tz = "UTC"),
                        as.POSIXct(sim_end, tz = "UTC") + 23 * 3600,
                        by = "hour")
  values <- rep(NA_real_, length(expected_hours))

  if (nrow(chunk_files_df) > 0) {
    chunk_files_df <- chunk_files_df[order(chunk_files_df$chunk_start), ]
    for (i in seq_len(nrow(chunk_files_df))) {
      chunk_data <- tryCatch(
        .endo_read_hourplot_chunk(chunk_files_df$path[i], chunk_files_df$chunk_start[i],
                                  chunk_files_df$chunk_end[i], variable_col),
        error = function(e) NULL
      )
      if (is.null(chunk_data) || nrow(chunk_data) == 0) next
      idx <- match(chunk_data$timestamp, expected_hours)
      ok <- !is.na(idx)
      values[idx[ok]] <- chunk_data$value[ok]
    }
  }

  list(timestamps = expected_hours, values = values, has_data = any(!is.na(values)))
}

.endo_create_raster_nc <- function(out_path, tile_map_r, time_axis, variable, variable_meta, compression, chunk_shape) {
  ncol_ <- terra::ncol(tile_map_r)
  nrow_ <- terra::nrow(tile_map_r)

  x_vals <- terra::xFromCol(tile_map_r, seq_len(ncol_))
  y_vals <- terra::yFromRow(tile_map_r, seq_len(nrow_))
  is_lonlat <- terra::is.lonlat(tile_map_r)

  t_origin <- format(time_axis[1], "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  t_vals <- as.numeric(difftime(time_axis, time_axis[1], units = "hours"))

  if (is_lonlat) {
    dim_x <- ncdf4::ncdim_def("lon", "degrees_east", x_vals, longname = "longitude", create_dimvar = TRUE)
    dim_y <- ncdf4::ncdim_def("lat", "degrees_north", y_vals, longname = "latitude", create_dimvar = TRUE)
  } else {
    dim_x <- ncdf4::ncdim_def("x", "m", x_vals, longname = "x coordinate", create_dimvar = TRUE)
    dim_y <- ncdf4::ncdim_def("y", "m", y_vals, longname = "y coordinate", create_dimvar = TRUE)
  }
  # unlim = FALSE: the time axis length is fully known at creation time, so
  # no record dimension is needed.
  dim_time <- ncdf4::ncdim_def("time", sprintf("hours since %s UTC", t_origin), t_vals,
                               unlim = FALSE, longname = "time", calendar = "standard")

  var_crs  <- ncdf4::ncvar_def("crs", "", list(), prec = "integer", longname = "CRS definition")
  # chunk_shape is a balanced (x, y, t) shape from .endo_balanced_chunk_shape() -
  # see that function's docs for why this replaced the old (1, 1, ntime)
  # per-cell chunking (fast writes, catastrophically slow cross-pixel reads).
  # The write loop below always writes in spatial batches that land on whole
  # multiples of chunk_shape$x/$y and span the FULL time axis per batch (see
  # .endo_batch_dims()/.endo_spatial_batches()), so every touched chunk is
  # still written completely in one pass, never partially - the balanced
  # shape does not reintroduce the old read-modify-write penalty.
  var_data <- ncdf4::ncvar_def(variable, variable_meta$units, list(dim_x, dim_y, dim_time),
                               missval = -9999, longname = variable_meta$long_name,
                               compression = compression, prec = "double",
                               chunksizes = c(chunk_shape$x, chunk_shape$y, chunk_shape$t))

  nc <- ncdf4::nc_create(out_path, list(var_crs, var_data))
  ncdf4::ncvar_put(nc, var_crs, 0L)

  crs_wkt <- terra::crs(tile_map_r, proj = FALSE)
  ncdf4::ncatt_put(nc, "crs", "crs_wkt", crs_wkt)
  ncdf4::ncatt_put(nc, "crs", "grid_mapping_name",
                   if (is_lonlat) "latitude_longitude" else "projected_coordinate_system")

  if (is_lonlat) {
    ncdf4::ncatt_put(nc, "lon", "standard_name", "longitude"); ncdf4::ncatt_put(nc, "lon", "axis", "X")
    ncdf4::ncatt_put(nc, "lat", "standard_name", "latitude");  ncdf4::ncatt_put(nc, "lat", "axis", "Y")
  } else {
    ncdf4::ncatt_put(nc, "x", "standard_name", "projection_x_coordinate"); ncdf4::ncatt_put(nc, "x", "axis", "X")
    ncdf4::ncatt_put(nc, "y", "standard_name", "projection_y_coordinate"); ncdf4::ncatt_put(nc, "y", "axis", "Y")
  }

  ncdf4::ncatt_put(nc, variable, "grid_mapping", "crs")
  ncdf4::ncatt_put(nc, variable, "coordinates", if (is_lonlat) "lon lat" else "x y")
  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.8")
  ncdf4::ncatt_put(nc, 0, "history", sprintf(
    "Created %s by R %s / ThermalScapeR::reconstruct_endo_raster()",
    format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), paste(R.version$major, R.version$minor, sep = ".")
  ))

  nc
}

#' Compute a balanced (x, y, t) HDF5 chunk shape for the reconstructed raster.
#'
#' The old chunking, \code{c(1, 1, ntime)}, made every per-cell write hit
#' exactly one chunk (fast writes) but made any cross-pixel read - a spatial
#' slice at one timestep, or a sum/mean across the time dimension for the
#' whole grid - touch every single chunk in the file once per timestep
#' (catastrophically slow reads).
#'
#' This instead follows the balanced-chunking approach described by Unidata
#' (blogs.unidata.ucar.edu, "Chunking Data: Choosing Shapes"): for a
#' (t, y, x) cube, solving \code{chunk_x = nx/N}, \code{chunk_y = ny/N},
#' \code{chunk_t = nt/N^2} for N against a target chunk byte size makes a
#' full per-pixel time series and a full per-timestep spatial slice touch
#' roughly the same number of chunks (N^2 either way) - both stay fast,
#' neither is favored. \code{target_chunk_mb} is a tunable knob, not a fixed
#' constant - the right value depends on actual production grid/period
#' sizes and should be checked with a real timing comparison (see
#' test-reconstruct-endo-raster-chunking.R) rather than trusted blindly.
#'
#' @keywords internal
.endo_balanced_chunk_shape <- function(nx, ny, nt, target_chunk_mb = 4) {
  target_bytes <- target_chunk_mb * 1024^2
  total_bytes  <- as.double(nx) * as.double(ny) * as.double(nt) * 8  # prec = "double"

  # total_bytes / N^4 == target_bytes
  n_est <- max(1, (total_bytes / target_bytes)^(1 / 4))

  list(
    x = as.integer(max(1L, min(nx, round(nx / n_est)))),
    y = as.integer(max(1L, min(ny, round(ny / n_est)))),
    t = as.integer(max(1L, min(nt, round(nt / n_est^2))))
  )
}

#' Compute spatial batch dimensions for the write loop.
#'
#' A batch always spans the FULL time axis in one \code{ncvar_put()} call, so
#' its in-memory size is \code{batch_x * batch_y * nt * 8} bytes - independent
#' of the period's total spatial extent, which is what keeps memory bounded
#' for large study areas. Batch width/height are rounded to whole multiples
#' of \code{chunk_shape$x}/\code{$y} so every write lands on complete chunks -
#' never split across two batches - which is what avoids HDF5
#' read-modify-write.
#'
#' @keywords internal
.endo_batch_dims <- function(nx, ny, nt, chunk_shape, target_batch_mb = 1024) {
  target_bytes <- target_batch_mb * 1024^2
  max_cells <- max(1, floor(target_bytes / (as.double(nt) * 8)))

  chunks_per_side <- max(1, floor(sqrt(max_cells / (chunk_shape$x * chunk_shape$y))))

  list(
    x = as.integer(min(nx, chunk_shape$x * chunks_per_side)),
    y = as.integer(min(ny, chunk_shape$y * chunks_per_side))
  )
}

#' Enumerate the spatial batches (in raster row/col terms) covering a grid.
#' @keywords internal
.endo_spatial_batches <- function(nx, ny, batch_dims) {
  x0s <- seq(1L, nx, by = batch_dims$x)
  y0s <- seq(1L, ny, by = batch_dims$y)

  batches <- vector("list", length(x0s) * length(y0s))
  k <- 1L
  for (y0 in y0s) {
    y1 <- min(ny, y0 + batch_dims$y - 1L)
    for (x0 in x0s) {
      x1 <- min(nx, x0 + batch_dims$x - 1L)
      batches[[k]] <- list(x0 = x0, x1 = x1, y0 = y0, y1 = y1)
      k <- k + 1L
    }
  }
  batches
}

.endo_tile_ids_in_mask <- function(tile_map, valid_cells_mask) {
  tm <- terra::rast(tile_map)
  vm <- terra::rast(valid_cells_mask)
  vm[vm != 1] <- NA
  tm_masked <- terra::mask(tm, vm)
  sort(unique(terra::values(tm_masked, mat = FALSE, na.rm = TRUE)))
}

.endo_microclim_path <- function(microclim_dir, study_area, period_label, tile_id, hgt_lbl, file_fmt) {
  base <- if (!is.null(study_area)) file.path(microclim_dir, study_area) else microclim_dir
  dir  <- file.path(base, "Microclim_Models", period_label, hgt_lbl)
  prefix <- if (!is.null(study_area)) sprintf("%s_", study_area) else ""
  file.path(dir, sprintf("Tile_%03d_%s%s_MicroclimModel_%s.%s",
                         tile_id, prefix, hgt_lbl, period_label, file_fmt))
}

.endo_snow_path <- function(microclim_dir, study_area, period_label, tile_id, file_fmt) {
  base <- if (!is.null(study_area)) file.path(microclim_dir, study_area) else microclim_dir
  dir  <- file.path(base, "Snow_Models", period_label)
  prefix <- if (!is.null(study_area)) sprintf("%s_", study_area) else "SnowModel_"
  file.path(dir, sprintf("Tile_%03d_%sSnowModel_%s.%s", tile_id, prefix, period_label, file_fmt))
}

#' Run the NicheMapR Endotherm model across a large tiled domain
#'
#' Formalizes the landscape-scale Endotherm model workflow: for every valid
#' cell of every tile in \code{tile_map} (restricted to
#' \code{valid_cells_mask}), for the date range(s) in \code{dates}, builds
#' the exe's per-chunk CSV/DAT inputs from the packaged microclimate data at
#' \code{microclim_dir} and runs \code{\link{run_endotherm_model}}, writing
#' a trimmed \code{HOURPLOT.csv} per cell per chunk. Structured like
#' \code{\link{run_micro_big_nichemap}}: SLURM array distribution via hidden
#' \code{...} arguments, one call per scenario (climatology's single period,
#' or year-specific's multi-row \code{dates}).
#'
#' @param tile_map Fine-resolution tile-ID \code{SpatRaster} or file path
#'   (e.g. \code{\link{create_tiles}}'s \code{output_path}).
#' @param valid_cells_mask 1/0 (or NA) \code{SpatRaster} or file path,
#'   caller-combined (e.g. winter range intersected with a water mask) -
#'   consumed via \code{\link{read_valid_cell_indices}}.
#' @param dates Either a \code{data.frame} with columns \code{Start_Dates}/
#'   \code{End_Dates} (resolving \code{period_label} and the
#'   \code{microclim_dir} file paths, one row per period) - optionally with
#'   \code{Sim_Start}/\code{Sim_End} columns giving the actual Endotherm
#'   simulation window within that period (defaults to the full period when
#'   absent) - or a length-2 \code{Date} vector (single period, sim window =
#'   the period).
#' @param microclim_dir The \code{output_dir} value passed to
#'   \code{\link{run_micro_big_nichemap}} for this scenario's microclimate
#'   data - \emph{not} a separate corrected/uncorrected pair of roots; any
#'   statistical correction applied upstream is expected to be written back
#'   into this same location/substructure.
#' @param dem Elevation \code{SpatRaster} or file path.
#' @param refl_dir Root reflectance directory as produced by
#'   \code{\link{compute_reflectance}} (\code{refl_dir/Gref/GF_Refl_*.tif}).
#' @param exe_path Full path to the Endotherm model executable.
#' @param output_dir Root output folder.
#' @param wineprefix Character. Required on non-Windows - a single,
#'   already-initialized (via \code{\link{init_wine_prefix}}) shared Wine
#'   prefix path, reused unchanged across every tile/cell/chunk in this call.
#'   Must \strong{not} be left to default to \code{NULL} or a
#'   \code{tempdir()}-derived path - every parallel worker must share the
#'   same prefix. Ignored on Windows.
#' @param endo_inputs Named list in \code{\link{get_endotherm_defaults}}'s
#'   9-group shape - the animal model. Default \code{get_endotherm_defaults()}.
#'   Its julnum-length fields not overridden via \code{time_varying}
#'   (\code{animal$mass2}/\code{fatpct2}, \code{physiology$tcreg2}, the four
#'   torso-fur fields, and every \code{diet} vector field) are re-sized to
#'   each chunk's own julnum (from their first element) before
#'   \code{\link{write_endotherm_inputs}} is called, so \code{endo_inputs}
#'   need not be pre-built at any particular julnum.
#' @param time_varying Named list from \code{\link{endo_timevar_template}},
#'   with whichever fields should vary over the sim window overwritten with a
#'   vector of length equal to the sim window's day count. Default (all
#'   \code{NULL}) applies no overrides.
#' @param chunk_size Integer, 1-52. The exe's per-invocation day limit is 52;
#'   longer sim windows are split into this many days per chunk (the last
#'   chunk may be shorter). Default \code{52}.
#' @param snow Logical. If \code{TRUE}, the corrected Snow tile is read from
#'   \code{microclim_dir} (same convention as \code{\link{run_micro_big_nichemap}}'s
#'   own \code{snow} argument) and any day with SWE > 0 gets
#'   \code{surfwet = 100}; every other day gets \code{surfwet_dry}. When
#'   \code{FALSE} (default), every day gets \code{surfwet_dry}.
#' @param surfwet_dry Numeric. Percent surface wet on non-snow days. Default
#'   \code{5}, matching \code{\link{write_juldays_dat}}'s own default.
#' @param study_area Character or \code{NULL}. Prefixes resolved
#'   \code{microclim_dir} file names and output paths. Default \code{NULL}.
#' @param clamp,clamp_bounds Passed through to \code{\link{micro_to_csv}}.
#'   Default \code{clamp = TRUE}, \code{clamp_bounds =
#'   micro_to_csv_clamp_defaults()}.
#' @param file_fmt Character, currently only \code{"nc"} is accepted. Unlike
#'   \code{\link{micro_to_csv}} (which dispatches generically between NetCDF
#'   and HDF5 via an internal open/read abstraction), this function's own
#'   tannul precompute and its \code{snow = TRUE} SWE read call
#'   \code{ncdf4::} directly, so they only understand NetCDF tiles. HDF5
#'   support for those two reads is a known, deferred gap - not implemented
#'   by this function - rather than an oversight; \code{microclim_dir} must
#'   therefore have been written with \code{\link{run_micro_big_nichemap}}'s
#'   \code{file_fmt = "nc"}.
#' @param headless Logical, passed to \code{\link{init_wine_prefix}}/
#'   \code{\link{run_endotherm_model}}. Default \code{FALSE}.
#' @param timeout Numeric, passed to \code{\link{run_endotherm_model}} for
#'   every chunk. Maximum time in seconds to let a single exe invocation run
#'   before it is killed. Default \code{0} means no timeout.
#' @param parallel Logical. If \code{TRUE}, cells within a tile are processed
#'   via \code{future_lapply()} with \code{ncores} workers. Default
#'   \code{FALSE}.
#' @param ncores Integer. Workers to use when \code{parallel = TRUE}; also
#'   sets \code{terraOptions(threads = ncores)}. Default \code{2}.
#' @param max_attempts Integer >= 1. Total attempts per chunk (its first run
#'   plus up to \code{max_attempts - 1} retries) before it's given up on.
#'   Every attempt - including retries - builds a brand-new temp workspace
#'   from scratch, so a retry never inherits a previous attempt's partial or
#'   empty directory. Retries for a tile run after every cell in that tile
#'   has been attempted once (still before that tile's rasters/lookups are
#'   freed), and always serially even when \code{parallel = TRUE}, since
#'   contention between concurrent workers is itself a plausible cause of
#'   intermittent failures. Only the \emph{final} failed attempt of a chunk
#'   that never succeeds is copied to \code{Debug_CSVs}. Default \code{2}
#'   (one retry).
#' @param ... Hidden SLURM array arguments \code{clust_array_arg}/
#'   \code{clust_array_size}, same convention as
#'   \code{\link{run_micro_big_nichemap}}.
#'
#' @return Invisibly, the concatenated per-tile log \code{data.frame}
#'   (\code{cell_id, chunk_index, chunk_start, chunk_end, status, message,
#'   output_path, n_attempts}) across every \code{(tile, period)} task this
#'   call processed. \code{n_attempts} is how many tries that chunk's final
#'   status reflects.
#'
#' @seealso \code{\link{run_micro_big_nichemap}}, \code{\link{micro_to_csv}},
#'   \code{\link{get_endotherm_defaults}}, \code{\link{endo_timevar_template}},
#'   \code{\link{run_endotherm_model}}, \code{\link{init_wine_prefix}}
#' @export
run_endo_big_nichemap <- function(tile_map, valid_cells_mask, dates, microclim_dir,
                                  dem, refl_dir, exe_path, output_dir, wineprefix,
                                  endo_inputs   = get_endotherm_defaults(),
                                  time_varying  = endo_timevar_template(),
                                  chunk_size    = 52,
                                  snow          = FALSE,
                                  surfwet_dry   = 5,
                                  study_area    = NULL,
                                  clamp         = TRUE,
                                  clamp_bounds  = micro_to_csv_clamp_defaults(),
                                  file_fmt      = "nc",
                                  headless      = FALSE,
                                  timeout       = 0,
                                  parallel      = FALSE,
                                  ncores        = 2,
                                  max_attempts  = 2,
                                  ...) {
  file_fmt <- match.arg(file_fmt, "nc")

  dots    <- list(...)
  allowed <- c("clust_array_arg", "clust_array_size")
  unknown <- setdiff(names(dots), allowed)
  if (length(unknown) > 0) stop("Unknown argument(s): ", paste(unknown, collapse = ", "))
  clust_array_arg  <- dots$clust_array_arg
  clust_array_size <- dots$clust_array_size
  if (!is.null(clust_array_arg) && is.null(clust_array_size))
    stop("clust_array_size must be provided when clust_array_arg is set")
  if (!is.null(clust_array_arg) &&
      (clust_array_arg < 1 || clust_array_arg > clust_array_size))
    stop("clust_array_arg must be between 1 and clust_array_size")

  if (!is.numeric(chunk_size) || length(chunk_size) != 1 || chunk_size < 1 || chunk_size > 52)
    stop("chunk_size must be a single numeric value between 1 and 52")

  if (!is.numeric(max_attempts) || length(max_attempts) != 1 || max_attempts < 1 ||
      max_attempts != as.integer(max_attempts))
    stop("max_attempts must be a single positive integer")
  max_attempts <- as.integer(max_attempts)

  date_ranges <- .normalize_dates_with_sim_window(dates)

  if (Sys.info()[["sysname"]] != "Windows") {
    init_wine_prefix(wineprefix, headless = headless)
  }

  if (parallel) terra::terraOptions(threads = ncores)

  tile_ids <- .endo_tile_ids_in_mask(tile_map, valid_cells_mask)
  all_combos <- expand.grid(tile_id = tile_ids, date_idx = seq_len(nrow(date_ranges)))

  all_combos$node <- if (is.null(clust_array_size)) 1L else
    rep(seq_len(clust_array_size), length.out = nrow(all_combos))
  task_combos <- if (is.null(clust_array_arg)) all_combos else
    all_combos[all_combos$node == clust_array_arg, ]

  cat(sprintf("Tasks this node: %d of %d total (tile x period combinations)\n",
              nrow(task_combos), nrow(all_combos)))

  node_exe_dir <- tempfile("endo_node_exe_")
  dir.create(node_exe_dir)
  on.exit(unlink(node_exe_dir, recursive = TRUE), add = TRUE)
  node_exe_path <- file.path(node_exe_dir, basename(exe_path))
  if (!file.copy(exe_path, node_exe_path)) {
    stop(sprintf("Failed to copy exe from %s to node-local path %s", exe_path, node_exe_path))
  }

  all_logs <- list()

  for (k in seq_len(nrow(task_combos))) {
    tile_id <- task_combos$tile_id[k]
    d_idx   <- task_combos$date_idx[k]

    period_label <- .endo_period_label(date_ranges$Start_Dates[d_idx], date_ranges$End_Dates[d_idx])
    sim_dates <- seq(date_ranges$Sim_Start[d_idx], date_ranges$Sim_End[d_idx], by = "day")
    n_days <- length(sim_dates)
    .endo_validate_timevar_lengths(time_varying, n_days)

    cat(sprintf("\n=== Task %d/%d | Tile %d | Period: %s ===\n",
                k, nrow(task_combos), tile_id, period_label))

    base_dir <- if (!is.null(study_area)) file.path(output_dir, study_area) else output_dir
    period_dir <- file.path(base_dir, period_label)
    tile_dir   <- file.path(period_dir, sprintf("Tile_%03d", tile_id))
    dir.create(tile_dir, recursive = TRUE, showWarnings = FALSE)

    abv_path <- .endo_microclim_path(microclim_dir, study_area, period_label, tile_id, "AbvGrd", file_fmt)
    blw_path <- .endo_microclim_path(microclim_dir, study_area, period_label, tile_id, "BlwGrd", file_fmt)
    # run_micro_big_nichemap() defaults to file_fmt = "h5", but this function
    # only reads "nc" - point that out explicitly if an .h5 sibling is present.
    .h5_hint <- function(p) {
      if (file.exists(sub("\\.nc$", ".h5", p)))
        " (found an .h5 file at that location instead - run_endo_big_nichemap() only supports file_fmt = \"nc\"; re-run the microclimate pipeline with file_fmt = \"nc\")"
      else ""
    }
    if (!file.exists(abv_path))
      stop(sprintf("AbvGrd microclimate tile not found:\n  %s%s", abv_path, .h5_hint(abv_path)))
    if (!file.exists(blw_path))
      stop(sprintf("BlwGrd microclimate tile not found:\n  %s%s", blw_path, .h5_hint(blw_path)))

    snow_path <- NULL
    if (snow) {
      snow_path <- .endo_snow_path(microclim_dir, study_area, period_label, tile_id, file_fmt)
      if (!file.exists(snow_path)) stop(sprintf("Snow tile not found (snow = TRUE):\n  %s", snow_path))
    }

    abv_r <- terra::rast(abv_path, subds = "Tz")[[1]]
    dem_r <- terra::rast(dem)
    tile_map_r <- terra::rast(tile_map)
    valid_mask_r <- terra::rast(valid_cells_mask)

    tile_extent_mask <- terra::crop(tile_map_r, abv_r) == tile_id
    valid_crop <- terra::crop(valid_mask_r, abv_r)
    valid_crop[valid_crop != 1] <- NA
    valid <- abv_r
    terra::values(valid) <- NA
    valid[tile_extent_mask & !is.na(valid_crop)] <- 1

    valid_cells <- which(!is.na(terra::values(valid, mat = FALSE)))
    xy <- terra::xyFromCell(valid, valid_cells)
    rc <- terra::rowColFromCell(valid, valid_cells)
    elevation <- terra::extract(dem_r, xy)[, 1]

    manifest <- data.frame(cell_id = valid_cells, x_idx = xy[, 1], y_idx = xy[, 2],
                           r_idx = rc[, 1], c_idx = rc[, 2], elevation = elevation,
                           stringsAsFactors = FALSE)
    utils::write.csv(manifest, file.path(period_dir, sprintf("Tile_%03d_manifest.csv", tile_id)),
                     row.names = FALSE)

    nc_abv <- ncdf4::nc_open(abv_path)
    # tannul (mean annual temperature) is the mean over whatever time range is
    # actually in this file, so refuse a file that only covers part of the
    # period (e.g. a winter-only corrected tile) rather than silently
    # returning a season-biased "annual" mean.
    # The corrected tile comes from an external pipeline, so don't assume
    # write_tile()'s exact "hours since <...T...> UTC" spelling: accept the
    # CF-style space separator too, and fail with a readable message rather
    # than an NA comparison if neither parses.
    abv_time_units <- if (is.null(nc_abv$dim$time)) NA_character_ else nc_abv$dim$time$units
    abv_origin_str <- sub("\\s+UTC$", "", trimws(sub("hours since\\s+", "", abv_time_units)))
    abv_origin <- as.POSIXct(abv_origin_str, tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
    if (is.na(abv_origin))
      abv_origin <- as.POSIXct(abv_origin_str, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
    if (is.na(abv_origin)) {
      ncdf4::nc_close(nc_abv)
      stop(sprintf(
        paste0("AbvGrd tile %s has no usable hourly time axis (time units: '%s'). ",
               "Expected a 'time' dimension with units like ",
               "'hours since YYYY-MM-DDTHH:MM:SS UTC'."),
        abv_path, abv_time_units
      ))
    }
    abv_time_range <- range(abv_origin + nc_abv$dim$time$vals * 3600)
    expected_days <- as.numeric(difftime(date_ranges$End_Dates[d_idx],
                                         date_ranges$Start_Dates[d_idx], units = "days")) + 1
    actual_days <- as.numeric(difftime(abv_time_range[2], abv_time_range[1], units = "days"))
    if (actual_days < 0.9 * expected_days) {
      ncdf4::nc_close(nc_abv)
      stop(sprintf(
        paste0("AbvGrd tile %s covers only %.0f days, but period %s..%s expects %.0f days. ",
               "tannul (mean annual temperature) requires the file to span the full period, ",
               "not a winter-only subset - check the upstream statistical-correction ",
               "pipeline's output."),
        abv_path, actual_days, date_ranges$Start_Dates[d_idx],
        date_ranges$End_Dates[d_idx], expected_days
      ))
    }
    tz_full <- aperm(ncdf4::ncvar_get(nc_abv, "Tz"), c(2, 1, 3))
    ncdf4::nc_close(nc_abv)
    tannul_mat <- apply(tz_full, c(1, 2), mean, na.rm = TRUE)
    rm(tz_full); gc()

    absorp_lookup <- .endo_absorp_lookup(refl_dir, study_area, sim_dates, xy)

    chunk_bounds <- .endo_chunk_bounds(n_days, chunk_size)

    # Attempt a single chunk once, in a freshly created temp workspace -
    # every attempt (including retries) gets its own workspace, never a
    # reused one, so a retry can't inherit another attempt's partial/empty
    # directory. Returns list(row, ws, ok): on success the workspace is
    # cleaned up here and ws is NA; on failure ws holds the workspace path so
    # the caller can decide whether to discard it (more attempts remain) or
    # dump it to Debug_CSVs (this was the last allowed attempt).
    attempt_chunk_fn <- function(cell, kk, attempt) {
      idx <- chunk_bounds[[kk]]
      chunk_dates <- sim_dates[idx]
      tannul <- tannul_mat[cell$r_idx, cell$c_idx]
      cell_dir <- file.path(tile_dir, sprintf("Cell_%06d", cell$cell_id))
      ws <- tempfile("endo_cell_")
      dir.create(ws)

      tryCatch({
        csvs <- micro_to_csv(
          abvgrd_input = abv_path, blwgrd_input = blw_path,
          cell = c(cell$c_idx, cell$r_idx), cell_input_type = "index",
          dates = chunk_dates, elev = cell$elevation, tannul = tannul,
          clamp = clamp, clamp_bounds = clamp_bounds
        )
        utils::write.csv(csvs$metout, file.path(ws, "metout.csv"), row.names = FALSE)
        utils::write.csv(csvs$shadmet, file.path(ws, "shadmet.csv"), row.names = FALSE)
        utils::write.csv(csvs$soil, file.path(ws, "soil.csv"), row.names = FALSE)
        utils::write.csv(csvs$shadsoil, file.path(ws, "shadsoil.csv"), row.names = FALSE)

        surfwet <- rep(surfwet_dry, length(chunk_dates))
        if (snow) {
          nc_snow <- ncdf4::nc_open(snow_path)
          # tryCatch(finally = ) - not on.exit() - so the handle is closed on
          # both the success and the error path of THIS chunk. on.exit() here
          # would register on the enclosing attempt_chunk_fn frame and only
          # fire once the whole call returns.
          snow_read <- tryCatch({
            snow_origin_str <- sub("\\s+UTC$", "", trimws(sub("hours since\\s+", "", nc_snow$dim$time$units)))
            snow_origin <- as.POSIXct(snow_origin_str, tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
            tme_snow <- snow_origin + nc_snow$dim$time$vals * 3600
            s_idx <- which(as.Date(tme_snow) >= min(chunk_dates) & as.Date(tme_snow) <= max(chunk_dates))
            if (length(s_idx) == 0) {
              stop(sprintf("Snow tile %s has no timesteps covering %s..%s", snow_path,
                           min(chunk_dates), max(chunk_dates)))
            }
            list(
              swe = ncdf4::ncvar_get(nc_snow, "totalSWE",
                                     start = c(cell$c_idx, cell$r_idx, s_idx[1]),
                                     count = c(1, 1, length(s_idx))),
              day_of_swe = as.Date(tme_snow[s_idx])
            )
          }, finally = ncdf4::nc_close(nc_snow))
          swe <- snow_read$swe
          day_of_swe <- snow_read$day_of_swe
          surfwet <- vapply(chunk_dates, function(d) {
            hrs <- swe[day_of_swe == d]
            if (length(hrs) > 0 && any(hrs > 0, na.rm = TRUE)) 100 else surfwet_dry
          }, numeric(1))
        }

        absorp <- vapply(chunk_dates, function(d) {
          absorp_lookup$values[which(valid_cells == cell$cell_id), format(d, "%Y_%m")]
        }, numeric(1))

        julnum  <- length(chunk_dates)
        juldays <- seq_len(julnum)
        write_juldays_dat(
          output_dir = ws, model_settings = list(julnum = julnum, juldays = juldays),
          habitat_settings = list(startday = 1, endday = julnum, absorp = absorp, surfwet = surfwet)
        )

        chunk_endo_inputs <- endo_inputs
        chunk_endo_inputs$model_settings$julnum <- julnum
        chunk_endo_inputs$model_settings$juldays <- juldays
        chunk_endo_inputs <- .endo_resize_static_fields(chunk_endo_inputs, julnum)
        chunk_endo_inputs <- .endo_apply_timevar(chunk_endo_inputs, time_varying, idx)
        do.call(write_endotherm_inputs, c(list(output_dir = ws), chunk_endo_inputs))

        exe_result <- run_endotherm_model(ws, exe_path = node_exe_path, wineprefix = wineprefix,
                                           headless = headless, timeout = timeout)

        out_csv <- NA_character_
        if (exe_result$success && file.exists(file.path(ws, "HOURPLOT.csv"))) {
          dir.create(cell_dir, recursive = TRUE, showWarnings = FALSE)
          hp <- utils::read.csv(file.path(ws, "HOURPLOT.csv"), skip = 1)
          hp_trim <- hp[, 1:8]
          out_csv <- file.path(cell_dir, sprintf("HOURPLOT_chunk%d_%s_%s.csv", kk,
                                                 format(min(chunk_dates), "%Y%m%d"),
                                                 format(max(chunk_dates), "%Y%m%d")))
          utils::write.csv(hp_trim, out_csv, row.names = FALSE)
        }

        row <- data.frame(cell_id = cell$cell_id, chunk_index = kk,
                          chunk_start = min(chunk_dates), chunk_end = max(chunk_dates),
                          status = if (exe_result$success) "success" else "error",
                          message = exe_result$message, output_path = out_csv,
                          n_attempts = attempt, stringsAsFactors = FALSE)

        if (exe_result$success) {
          unlink(ws, recursive = TRUE)
          list(row = row, ws = NA_character_, ok = TRUE)
        } else {
          list(row = row, ws = ws, ok = FALSE)
        }
      }, error = function(e) {
        row <- data.frame(cell_id = cell$cell_id, chunk_index = kk,
                          chunk_start = min(chunk_dates), chunk_end = max(chunk_dates),
                          status = "error", message = conditionMessage(e),
                          output_path = NA_character_, n_attempts = attempt, stringsAsFactors = FALSE)
        list(row = row, ws = if (dir.exists(ws)) ws else NA_character_, ok = FALSE)
      })
    }

    # Discard a failed attempt's workspace when more attempts remain for it.
    safe_unlink <- function(ws_path) {
      if (!is.na(ws_path) && dir.exists(ws_path)) unlink(ws_path, recursive = TRUE)
    }

    # Preserve a chunk's *final* failed attempt for inspection - never every
    # attempt, so a chunk that eventually succeeds (or that fails several
    # times before exhausting max_attempts) leaves only its last try behind.
    dump_debug <- function(ws_path, cell, kk) {
      if (is.na(ws_path) || !dir.exists(ws_path)) return(invisible(NULL))
      debug_dir <- file.path(output_dir, "Debug_CSVs",
                             sprintf("%s_Tile_%03d_Cell_%06d_chunk%d", period_label, tile_id, cell$cell_id, kk))
      dir.create(debug_dir, recursive = TRUE, showWarnings = FALSE)
      file.copy(list.files(ws_path, full.names = TRUE), debug_dir, overwrite = TRUE)
      unlink(ws_path, recursive = TRUE)
    }

    cell_fn <- function(ci) {
      cell <- manifest[ci, ]
      rows <- vector("list", length(chunk_bounds))
      pending <- list()

      for (kk in seq_along(chunk_bounds)) {
        result <- attempt_chunk_fn(cell, kk, attempt = 1L)
        rows[[kk]] <- result$row
        if (!result$ok) {
          if (max_attempts > 1L) {
            safe_unlink(result$ws)
            pending[[length(pending) + 1]] <- list(cell = cell, kk = kk)
          } else {
            dump_debug(result$ws, cell, kk)
          }
        }
        if (ci %% 50 == 0) cat(sprintf("  Tile %d | %s: %d/%d cells done\n", tile_id, period_label, ci, nrow(manifest)))
      }
      list(rows = do.call(rbind, rows), pending = pending)
    }

    cell_results <- if (parallel) {
      future::plan(future::multisession, workers = ncores)
      on.exit(future::plan(future::sequential), add = TRUE)
      future.apply::future_lapply(seq_len(nrow(manifest)), cell_fn, future.seed = TRUE)
    } else {
      lapply(seq_len(nrow(manifest)), cell_fn)
    }

    tile_rows <- do.call(rbind, lapply(cell_results, `[[`, "rows"))
    pending <- do.call(c, lapply(cell_results, `[[`, "pending"))

    # Retry pass(es): run at the end of THIS tile - before the tile-level
    # rasters/lookups below are freed - rather than deferred to the very end
    # of the whole job, so failed chunks can be retried without re-deriving
    # tannul_mat/absorp_lookup from scratch. Always serial, even when
    # parallel = TRUE: if contention between concurrent workers (e.g. for
    # Wine/Xvfb) is itself part of what's causing intermittent failures,
    # retrying single-threaded improves the odds rather than reproducing it.
    attempt <- 2L
    while (length(pending) > 0 && attempt <= max_attempts) {
      still_pending <- list()
      for (item in pending) {
        result <- attempt_chunk_fn(item$cell, item$kk, attempt = attempt)
        match_row <- tile_rows$cell_id == item$cell$cell_id & tile_rows$chunk_index == item$kk
        tile_rows[match_row, ] <- result$row
        if (!result$ok) {
          if (attempt < max_attempts) {
            safe_unlink(result$ws)
            still_pending[[length(still_pending) + 1]] <- item
          } else {
            dump_debug(result$ws, item$cell, item$kk)
          }
        }
      }
      pending <- still_pending
      attempt <- attempt + 1L
    }

    tile_log <- tile_rows
    utils::write.csv(tile_log, file.path(period_dir, sprintf("Tile_%03d_log.csv", tile_id)), row.names = FALSE)
    all_logs[[k]] <- tile_log

    rm(manifest, tannul_mat, absorp_lookup, cell_results, tile_log, tile_rows, pending); gc()

  }

  invisible(do.call(rbind, all_logs))
}

#' Reconstruct a raster from run_endo_big_nichemap() or the pre-existing
#' hand-run Endotherm workflow's HOURPLOT output
#'
#' Concatenates each cell's \code{HOURPLOT_chunk*.csv} files into one
#' continuous hourly time series and places every cell's series onto a
#' \code{tile_map}-templated NetCDF raster. Robust to both the pre-existing
#' hand-run workflow's directory layout and \code{\link{run_endo_big_nichemap}}'s
#' layout - file discovery never depends on what sits above
#' \code{Tile_NNN/Cell_CCCCCC/} in the directory tree.
#'
#' @param root_dir Directory to recursively search for manifest and
#'   \code{HOURPLOT_chunk*.csv} files. Works with either directory layout.
#'   Must point at a single scenario's output - if two scenario trees
#'   sharing the same tile/cell/chunk-start are both reachable under
#'   \code{root_dir}, this function \code{stop()}s rather than silently
#'   picking one.
#' @param tile_map \code{SpatRaster} or file path. Used purely as a spatial
#'   template (extent, resolution, CRS) - placement uses each cell's
#'   manifest-recorded \code{x_idx}/\code{y_idx}, not tile-ID values.
#' @param dates Either a \code{data.frame} with columns \code{Start_Dates}/
#'   \code{End_Dates} (optionally \code{Sim_Start}/\code{Sim_End}) or a
#'   length-2 \code{Date} vector - same convention as
#'   \code{\link{run_endo_big_nichemap}}. One row produces one output file.
#'   \strong{Reconstructing the pre-existing hand-run workflow's output
#'   requires passing \code{Sim_Start}/\code{Sim_End} explicitly}: its
#'   manifests' \code{Start_Dates}/\code{End_Dates} span the full year, while
#'   the actual chunk files only cover the real simulation window (e.g.
#'   Dec-Apr) - omitting \code{Sim_Start}/\code{Sim_End} defaults them to
#'   \code{Start_Dates}/\code{End_Dates} (per
#'   \code{\link{run_endo_big_nichemap}}'s own convention), producing an
#'   axis that is mostly NA by construction, not by data loss.
#' @param variable Character, \code{"metabolic_rate"} or \code{"water_loss"} -
#'   selects the trimmed HOURPLOT's \code{MET(W)} or \code{EVP(G/S)} column.
#' @param output_dir Root output folder.
#' @param study_area Character or \code{NULL}. Prefixes output filenames.
#'   Default \code{NULL}.
#' @param parallel Logical. If \code{TRUE}, cells are assembled via
#'   \code{future_lapply()} with \code{ncores} workers; the NetCDF write
#'   itself always happens sequentially afterward. Default \code{FALSE}.
#' @param ncores Integer. Workers to use when \code{parallel = TRUE}.
#'   Default \code{2}.
#' @param compression Integer 0-9. Gzip compression level. Default \code{4L}.
#' @param target_chunk_mb Numeric. Target size in MB for each output NetCDF
#'   storage chunk, used by \code{\link{.endo_balanced_chunk_shape}} to derive
#'   a chunk shape that keeps both a single pixel's full time series and a
#'   single timestep's full spatial slice (e.g. a sum across time) fast to
#'   read - replacing the old \code{c(1, 1, ntime)} chunking, which favored
#'   per-cell writes at the cost of any cross-pixel read. Default \code{4}.
#' @param target_batch_mb Numeric. Target in-memory size in MB for each
#'   spatial write batch (a batch spans the full time axis, so its size is
#'   \code{batch_x * batch_y * ntime * 8} bytes); bounds memory for large
#'   study areas regardless of the period's total extent. Default \code{1024}.
#'
#' @return Invisibly, a \code{data.frame} with one row per requested period:
#'   \code{period_label, output_path, cells_attempted} (manifest cells that
#'   were in tile_map's extent and had reconstruction attempted),
#'   \code{cells_placed} (of those, cells with at least one real, non-NA hour
#'   actually written - not merely attempted), \code{cells_with_gaps} (of
#'   those placed, cells with a partial NA gap).
#'
#' @details
#' HOURPLOT rows with \code{HR == 24} are always dropped before
#' timestamping - empirically verified (see the design spec) to be a
#' duplicate of that day's own hour-0 climate inputs, re-run through the
#' model's continued physiological state, not real new data. Every kept
#' row's timestamp is computed from its position in the file, never from
#' the \code{HR}/\code{MO}/\code{DEP} columns. A row-count mismatch against
#' a chunk's own claimed date range is treated as a read failure (that
#' chunk's hours become NA), never silently misaligned data.
#'
#' The two source workflows write structurally different files, not just a
#' different directory layout - the pre-existing workflow's chunk files are
#' raw exe output (metadata line, then header, ~89 columns);
#' \code{\link{run_endo_big_nichemap}}'s own output has already been read,
#' trimmed to 8 columns, and rewritten (header on line 1, no metadata line).
#' This function detects which format a given file is automatically.
#'
#' A cell with no covering chunk data for a period is entirely \code{NA} in
#' the output and is not counted in \code{cells_placed} - never fatal. Only
#' a period with no manifests, or no HOURPLOT chunk files overlapping that
#' period's actual simulation window, is a \code{stop()} - a stray HOURPLOT
#' file from an unrelated period elsewhere under \code{root_dir} does not
#' count as "something was found."
#'
#' The output NetCDF's storage chunking is a balanced (x, y, t) shape (see
#' \code{\link{.endo_balanced_chunk_shape}}), not the pixel-per-chunk layout
#' used previously - a single pixel's full time series and a full-grid
#' spatial slice or time-collapsing reduction (e.g. summing across layers to
#' get one total-cost raster) are both fast to read back. Cells are still
#' assembled in parallel via \code{future_lapply()} when \code{parallel =
#' TRUE}, just scoped to one spatial write batch at a time (see
#' \code{\link{.endo_batch_dims}}) rather than the whole period at once -
#' this also bounds memory for large study areas.
#'
#' @seealso \code{\link{run_endo_big_nichemap}}
#' @export
reconstruct_endo_raster <- function(root_dir, tile_map, dates,
                                    variable    = c("metabolic_rate", "water_loss"),
                                    output_dir,
                                    study_area  = NULL,
                                    parallel    = FALSE,
                                    ncores      = 2,
                                    compression = 4L,
                                    target_chunk_mb = 4,
                                    target_batch_mb = 1024) {
  variable <- match.arg(variable)
  var_meta <- .endo_variable_column(variable)
  variable_col <- var_meta$column

  date_ranges <- .normalize_dates_with_sim_window(dates)
  tile_map_r  <- terra::rast(tile_map)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Set up the parallel plan ONCE for the whole call, not per period - both
  # to avoid re-spawning multisession workers every iteration and because
  # on.exit() inside a loop registers on the function's own frame, not per
  # iteration, and would accumulate/leak across periods.
  if (parallel) {
    future::plan(future::multisession, workers = ncores)
    on.exit(future::plan(future::sequential), add = TRUE)
  }

  log_rows <- list()

  cat(sprintf("Reconstructing %d period(s)\n", nrow(date_ranges)))

  for (d_idx in seq_len(nrow(date_ranges))) {
    period_label <- .endo_period_label(date_ranges$Start_Dates[d_idx], date_ranges$End_Dates[d_idx])
    sim_start <- date_ranges$Sim_Start[d_idx]
    sim_end   <- date_ranges$Sim_End[d_idx]

    cat(sprintf("\n=== Period %d/%d: %s ===\n", d_idx, nrow(date_ranges), period_label))

    # Narrow the recursive scan to this period's own subdirectory when one is
    # unambiguously identifiable (the run_endo_big_nichemap()/hand-run
    # layouts seen in production) - falls back to scanning all of root_dir
    # for layouts with no period-labeled directory (e.g. the old flat
    # hand-run layout, where multiple periods' chunk files share one
    # Tile_NNN/Cell_CCCCCC/ folder). Never changes which files are found,
    # only how much of the tree list.files() has to walk to find them.
    search_dir <- .endo_scoped_search_dir(root_dir, period_label)

    manifests <- .endo_discover_manifests(search_dir, period_label)
    if (nrow(manifests) == 0) {
      stop(sprintf("No manifests found under %s for period %s - check root_dir and dates.",
                   root_dir, period_label))
    }
    cat(sprintf("  Found %d manifest(s)\n", nrow(manifests)))

    # Period-scoped: only chunks overlapping THIS period's simulation window
    # count - "are there any HOURPLOT files anywhere under root_dir" is not
    # sufficient, since files from an unrelated period would otherwise mask
    # a real "nothing found for this period" condition.
    hourplot_files <- .endo_discover_hourplot_files(search_dir)
    hourplot_files <- hourplot_files[
      hourplot_files$chunk_start <= sim_end & hourplot_files$chunk_end >= sim_start,
    ]
    if (nrow(hourplot_files) == 0) {
      stop(sprintf(
        "No HOURPLOT chunk files overlapping %s..%s found under %s for period %s - check root_dir and dates.",
        sim_start, sim_end, root_dir, period_label
      ))
    }
    .endo_check_duplicate_chunks(hourplot_files)
    cat(sprintf("  Found %d chunk file(s) overlapping simulation window\n", nrow(hourplot_files)))

    cell_list <- do.call(rbind, lapply(seq_len(nrow(manifests)), function(i) {
      m <- utils::read.csv(manifests$path[i])
      m$tile_id <- manifests$tile_id[i]
      m
    }))

    # cellFromXY() returns NA for any point outside tile_map_r's extent -
    # reuse that single call both to detect out-of-extent cells and, for the
    # rest, to avoid recomputing it later when placing values.
    cell_list$cell_num <- terra::cellFromXY(tile_map_r, cbind(cell_list$x_idx, cell_list$y_idx))
    out_of_extent <- is.na(cell_list$cell_num)
    if (any(out_of_extent)) {
      warning(sprintf("%d manifest cell(s) fall outside tile_map's extent for period %s - skipped.",
                      sum(out_of_extent), period_label))
      cell_list <- cell_list[!out_of_extent, , drop = FALSE]
    }
    rc_all <- terra::rowColFromCell(tile_map_r, cell_list$cell_num)
    cell_list$.row <- rc_all[, 1]
    cell_list$.col <- rc_all[, 2]

    expected_hours <- seq(as.POSIXct(sim_start, tz = "UTC"),
                          as.POSIXct(sim_end, tz = "UTC") + 23 * 3600, by = "hour")

    output_path <- file.path(output_dir, sprintf(
      "%s%s_%s.nc",
      if (!is.null(study_area)) paste0(study_area, "_") else "",
      variable, period_label
    ))

    nx <- terra::ncol(tile_map_r)
    ny <- terra::nrow(tile_map_r)
    nt <- length(expected_hours)

    # Balanced chunk shape + chunk-aligned spatial batching (see
    # .endo_balanced_chunk_shape()/.endo_batch_dims() docs) replaces the old
    # c(1, 1, ntime) chunking + per-cell write loop. The old layout made
    # writes cheap but made any cross-pixel read (a spatial slice, or a
    # sum/mean across time for the whole grid) touch every chunk in the file
    # once per timestep. Writing in chunk-aligned spatial batches keeps
    # writes just as cheap - every touched chunk is still written whole, in
    # one pass, never partially - while making reads fast in both
    # directions, and bounds memory to one batch's array instead of
    # materializing the whole period at once (the scaling concern this
    # replaced).
    chunk_shape <- .endo_balanced_chunk_shape(nx, ny, nt, target_chunk_mb = target_chunk_mb)
    batch_dims  <- .endo_batch_dims(nx, ny, nt, chunk_shape, target_batch_mb = target_batch_mb)
    batches     <- .endo_spatial_batches(nx, ny, batch_dims)

    cat(sprintf("  Assembling %d cell(s) across %d spatial batch(es) (%s)...\n",
               nrow(cell_list), length(batches),
               if (parallel) sprintf("parallel, %d workers", ncores) else "sequential"))
    cat(sprintf("  Chunk shape %dx%dx%d, batch shape %dx%d\n",
               chunk_shape$x, chunk_shape$y, chunk_shape$t, batch_dims$x, batch_dims$y))

    n_placed <- 0L
    n_gapped <- 0L
    # nc <- NULL first: `exists("nc", inherits = FALSE)` in `finally` below
    # checks the FUNCTION's frame, not this loop iteration's - without
    # resetting it here, a failure early in period 2's tryCatch (before its
    # own .endo_create_raster_nc() call) would see period 1's already-closed
    # connection object still bound to `nc` and attempt to close it again.
    nc <- NULL
    tryCatch({
      nc <- .endo_create_raster_nc(output_path, tile_map_r, expected_hours, variable, var_meta, compression, chunk_shape)

      for (b in batches) {
        batch_cells <- cell_list[
          cell_list$.row >= b$y0 & cell_list$.row <= b$y1 &
          cell_list$.col >= b$x0 & cell_list$.col <= b$x1,
          , drop = FALSE
        ]
        if (nrow(batch_cells) == 0) next

        cell_fn <- function(i) {
          cell <- batch_cells[i, ]
          chunks_i <- hourplot_files[
            hourplot_files$tile_id == cell$tile_id & hourplot_files$cell_id == cell$cell_id &
            hourplot_files$chunk_start <= sim_end & hourplot_files$chunk_end >= sim_start,
          ]
          series <- .endo_assemble_cell_series(chunks_i, sim_start, sim_end, variable_col)
          list(row = cell$.row, col = cell$.col, values = series$values, has_data = series$has_data)
        }

        cell_results <- if (parallel) {
          future.apply::future_lapply(seq_len(nrow(batch_cells)), cell_fn, future.seed = TRUE)
        } else {
          lapply(seq_len(nrow(batch_cells)), cell_fn)
        }

        width  <- b$x1 - b$x0 + 1L
        height <- b$y1 - b$y0 + 1L
        arr <- array(-9999, dim = c(width, height, nt))

        for (res in cell_results) {
          vals <- res$values
          vals[is.na(vals)] <- -9999
          arr[res$col - b$x0 + 1L, res$row - b$y0 + 1L, ] <- vals
          # cells_placed counts cells with at least one REAL hour, not merely
          # cells attempted - a 100%-NA cell must not count as "placed", or a
          # complete reconstruction failure would look identical to a full
          # success in this log.
          if (res$has_data) {
            n_placed <- n_placed + 1L
            if (any(is.na(res$values))) n_gapped <- n_gapped + 1L
          }
        }

        ncdf4::ncvar_put(nc, variable, arr, start = c(b$x0, b$y0, 1), count = c(width, height, nt))
      }
    }, finally = {
      if (!is.null(nc)) tryCatch(ncdf4::nc_close(nc), error = function(e) NULL)
    })

    log_rows[[d_idx]] <- data.frame(
      period_label = period_label, output_path = output_path,
      cells_attempted = nrow(cell_list), cells_placed = n_placed,
      cells_with_gaps = n_gapped,
      stringsAsFactors = FALSE
    )

    cat(sprintf("  Done: %s (%d/%d cells placed, %d with gaps)\n",
               output_path, n_placed, nrow(cell_list), n_gapped))
  }

  invisible(do.call(rbind, log_rows))
}
