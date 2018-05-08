prep_inputs <- function(
  nwis_site, nwm_model, ref_date,
  site_info_ind, nwis_data_ind,
  nwm_retro_ind, nwm_forecast_ind,
  remake_file
) {

  # read in data
  nwis_data <- readRDS(sc_retrieve(nwis_data_ind, remake_file))
  site_info <- readr::read_tsv(sc_retrieve(site_info_ind, remake_file))
  nwm_retro <- readRDS(sc_retrieve(nwm_retro_ind, remake_file))
  nwm_forecast <- readRDS(sc_retrieve(nwm_forecast_ind, remake_file))

  # calculate date boundaries
  calib_ndays <- as.integer(365.25*5) # 5 years
  start_calib <- ref_date - as.difftime(calib_ndays, units='days')
  forecast_range <- as.integer(ifelse(nwm_model=='med', 10, 30))
  end_forecast <- ref_date + as.difftime(forecast_range-1, units='days')

  ## prepare the inputs for an EGRET eList ##

  # prepare flow
  q_multiplier <- 35 # TODO: either ~35 or 1 depending on units of NWM flow, need to convert to m3/s
  #flow_nwm <- nwm_retro[[nwis_site]]
  flow <- data_frame(
    dateTime=seq(as.Date('1979-10-01'), as.Date('1979-12-01'), by=as.difftime(1, units='days')),
    value=rnorm(length(dateTime), 10, 2),
    code='A'
  ) %>%
    EGRET::populateDaily(qConvert=q_multiplier, verbose=FALSE)
  #Daily <- readNWISDaily("06934500","00060","1979-10-01","2010-09-30")

  # prepare concentrations
  conc <- data_frame(
    dateTime=seq(as.Date('1979-10-01'), as.Date('1979-12-01'), by=as.difftime(15, units='days')),
    comment="",
    value=rnorm(length(dateTime), 50, 14)
  ) %>%
    EGRET::compressData() %>%
    EGRET::populateSampleColumns()
  #Sample <-readNWISSample("06934500","00631","1970-10-01","2011-09-30")

  # prepare site info
  info <- site_info %>%
    filter(site_no == nwis_site)
  # INFO <- readNWISInfo(siteNumber,pCode,interactive=FALSE)

  # combine the inputs into an EGRET list
  eList <- EGRET::mergeReport(INFO=info, Daily=flow, Sample=conc)

  return(eList)
}
