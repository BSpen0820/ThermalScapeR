# A stand-in native Endotherm "exe" (/bin/sh script): fails like the real
# Fortran when ENDO.DAT is missing or empty, else reports success and keeps a
# copy of the ENDO.DAT it read as seen_ENDO.DAT.
.endo_fake_native_exe <- function(dir, script = c(
  "#!/bin/sh",
  "if [ ! -s ENDO.DAT ]; then",
  "  echo ' Beginning Calculations...'",
  "  echo 'Fortran runtime error: End of file' 1>&2",
  "  exit 2",
  "fi",
  "cp ENDO.DAT seen_ENDO.DAT",
  "echo ' Calculations completed.' > ErrorMsgs.dat"
)) {
  exe <- file.path(dir, "fake_endo")
  writeLines(script, exe)
  Sys.chmod(exe, "755")
  exe
}
