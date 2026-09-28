crn_stations_fixture <- function() {
  testthat::test_path("fixtures", "crn_stations_sample.tsv")
}

crn_test_lines <- function(lst_from, lst_to, offset_h = -9, vals = list(), wban = "26563") {
  lst <- seq(as.POSIXct(lst_from, tz = "UTC"), as.POSIXct(lst_to, tz = "UTC"), by = "5 min")
  utc <- lst - offset_h * 3600
  cols <- list(
    WBANNO = wban,
    UTC_DATE = format(utc, "%Y%m%d"), UTC_TIME = format(utc, "%H%M"),
    LST_DATE = format(lst, "%Y%m%d"), LST_TIME = format(lst, "%H%M"),
    CRX_VN = "2.623", LONGITUDE = "-150.44", LATITUDE = "60.72",
    AIR_TEMPERATURE = "-5.0", PRECIPITATION = "0.0",
    SOLAR_RADIATION = "12", SR_FLAG = "0",
    SURFACE_TEMPERATURE = "3.0", ST_TYPE = "C", ST_FLAG = "0",
    RELATIVE_HUMIDITY = "84", RH_FLAG = "0",
    SOIL_MOISTURE_5 = "0.123", SOIL_TEMPERATURE_5 = "-0.5",
    WETNESS = "1000", WET_FLAG = "0", WIND_1_5 = "0.50", WIND_FLAG = "0"
  )
  cols[names(vals)] <- vals
  n <- length(lst)
  do.call(paste, c(lapply(cols, function(x) rep_len(x, n)), sep = " "))
}

crn_test_file <- function(path, ...) {
  writeLines(crn_test_lines(...), path)
  invisible(path)
}
