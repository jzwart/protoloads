prep_inputs <- function(
  nwis_site, nwm_model, ref_date,
  site_info_ind, nwis_data_ind,
  nwm_retro_ind, nwm_forecast_ind,
  remake_file
) {

  # read in data
  nwis_data <- readRDS(sc_retrieve(nwis_data_ind, remake_file))
  site_info <- suppressMessages(readr::read_tsv(sc_retrieve(site_info_ind, remake_file)))
  nwm_retro <- readRDS(sc_retrieve(nwm_retro_ind, remake_file))
  nwm_forecast <- readRDS(sc_retrieve(nwm_forecast_ind, remake_file))

  # calculate date boundaries. ref_date is the day after the last fitting day, first day of the forecasts
  ref_Date <- as.Date(ref_date)
  calib_ndays <- as.integer(365.25*5) # 5 years
  start_calib <- ref_Date - as.difftime(calib_ndays, units='days')
  forecast_range <- as.integer(ifelse(nwm_model=='med', 10, 30))
  end_forecast <- ref_Date + as.difftime(forecast_range-1, units='days')

  ## prepare the inputs for an EGRET eList ##

  # prepare flow (all days from start_calib to end_forecast, for both fitting and prediction)
  q_divisor <- 35.3147 # TODO: either ~35 or 1 depending on units of NWM flow, need to convert to m3/s
  #flow_nwm <- nwm_retro[[nwis_site]]
  flow <- data_frame(
    dateTime=seq(as.Date('2010-10-01'), as.Date('2015-12-01'), by=as.difftime(1, units='days')),
    value=rnorm(length(dateTime), 10, 2),
    code='A'
  ) %>%
    dplyr::filter(dateTime >= start_calib, dateTime < end_forecast) %>%
    EGRET::populateDaily(qConvert=q_divisor, verbose=FALSE)
  #Daily <- readNWISDaily("06934500","00060","1979-10-01","2010-09-30")

  # prepare concentrations (only observations before ref_Date)
  conc <- data_frame(
    dateTime=seq(as.Date('2010-10-01'), as.Date('2015-12-01'), by=as.difftime(15, units='days')),
    comment="",
    value=rnorm(length(dateTime), 50, 14)
  ) %>%
    dplyr::filter(dateTime >= start_calib, dateTime < ref_Date) %>%
    EGRET::compressData() %>%
    EGRET::populateSampleColumns()
  #Sample <-readNWISSample("06934500","00631","1970-10-01","2011-09-30")

  # prepare site info
  info <- site_info %>%
    dplyr::filter(site_no == nwis_site) %>%
    mutate(ref_date = ref_Date)
  # INFO <- readNWISInfo(siteNumber,pCode,interactive=FALSE)

  # combine the inputs into an EGRET list
  eList <- EGRET::mergeReport(INFO=info, Daily=flow, Sample=conc)

  return(eList)
}
