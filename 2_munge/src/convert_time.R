convert_time_nc2posix <- function(tDim) {
  as.POSIXct(
    tDim$vals*60,
    origin = as.POSIXct(strsplit(tDim$units, split = " ")[[1]][3], tz = "UTC"),
    tz = "UTC")
}
