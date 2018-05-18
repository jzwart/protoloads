prep_inputs <- function(
  nwis_site, nwm_model, ref_date,
  site_info_ind, nwis_data_ind,
  nwm_retro_ind, nwm_forecast_ind,
  remake_file
) {

  # read in data and filter to nwis_site
  nwis_data <- readRDS(sc_retrieve(nwis_data_ind, remake_file)) %>%
    lapply(function(df) dplyr::filter(df, site_no == nwis_site))
  site_info <- suppressMessages(readr::read_tsv(sc_retrieve(site_info_ind, remake_file))) %>%
    dplyr::filter(site_no == nwis_site)
  nwm_retro <- readRDS(sc_retrieve(nwm_retro_ind, remake_file)) %>%
    dplyr::filter(site_no == nwis_site)
  nwm_forecast <- readRDS(sc_retrieve(nwm_forecast_ind, remake_file)) %>%
    dplyr::filter(site_no == nwis_site)

  # calculate date boundaries. ref_date is the day after the last fitting day, first day of the forecasts
  ref_Date <- as.Date(ref_date)
  calib_ndays <- as.integer(365.25*5) # 5 years
  start_calib <- ref_Date - as.difftime(calib_ndays, units='days')
  start_analysis <- ref_Date - as.difftime(182, units='days')
  forecast_range <- as.integer(ifelse(nwm_model=='med', 10, 30))
  end_forecast <- ref_Date + as.difftime(forecast_range-1, units='days')

  # ultimately we'd like to use the analysis data for some gap between retro and
  # forecast flow predictions, but for now we have (1) no analysis data
  # available yet and (2) the retro ends 12/31/2017, so for later models there
  # are NAs if we only use retro plus a single reference date. SO: for now we'll
  # create a substitude "analysis" dataset made of lag-0 "forecasts" from
  # previous reference days. we'll make this analysis dataset cover the period
  # from 6 months before the reference date up until the reference date (defined
  # in start_analysis above). But we don't have this "analysis" data between
  # 2016-11-08 and 2017-05-08, we we'll need to use the retro data for that
  # period for this "analysis" substitute
  nwm_analysis <- bind_rows(
    nwm_retro %>% filter(date >= as.Date('2016-11-08'), date < min(nwm_forecast$valid_date)),
    nwm_forecast %>% filter(ref_date == valid_date) %>% select(site_no, date = valid_date, flow))

  ## prepare the inputs for an EGRET eList ##

  # prepare flow (all days from start_calib to end_forecast, for both fitting and prediction)
  q_divisor <- 1 # would be 35.3147 if NWM flow were in ft3/s because need to convert to m3/s
  flow_past <- nwm_retro %>%
    dplyr::filter(date >= start_calib, date < start_analysis) %>%
    select(dateTime=date, value=flow)
  flow_analysis <- nwm_analysis %>%
    dplyr::filter(date >= start_analysis, date < ref_Date) %>%
    select(dateTime=date, value=flow)
  flow_future <- nwm_forecast %>%
    dplyr::filter(ref_date == ref_Date) %>%
    select(dateTime=valid_date, value=flow)
  flow <- bind_rows(
    flow_past,
    flow_analysis,
    flow_future
  ) %>%
    mutate(code='') %>%
    arrange(dateTime) %>%
    EGRET::populateDaily(qConvert=q_divisor, verbose = FALSE)
  #Daily <- readNWISDaily("06934500","00060","1979-10-01","2010-09-30")
  if(any(is.na(flow$Q)) || any(is.na(flow$LogQ))) {
    message(sprintf(
      'Found %d NA values in Q and %d NA values in logQ; removing all corresponding rows',
      length(which(is.na(flow$Q))), length(which(is.na(flow$LogQ)))))
    flow <- dplyr::filter(flow, !is.na(Q), !is.na(LogQ))
  }

  # prepare concentrations (only observations before ref_Date)
  conc <- bind_rows(
    nwis_data$nitrate_grab,
    nwis_data$nitrate_sensor
  ) %>%
    group_by(date) %>%
    summarize(
      choice={
        # prioritize sensor data if available, then uncensored, then censored
        # with lowest detection limit, then anything
        best_choices <- if(any(parm_cd == 00931)) {
          which(parm_cd == 00931)
        } else {
          if(any(daily_cd != '<')) {
            which(daily_cd != '<')
          } else {
            which.min(daily_mean)
          }
        }
        best_choices[1]
      },
      comment=daily_cd[choice],
      value=daily_mean[choice]
    ) %>%
    select(dateTime=date, comment, value) %>% # comment must precede value
    dplyr::filter(dateTime >= start_calib, dateTime < ref_Date) %>%
    EGRET::compressData(verbose = FALSE) %>%
    EGRET::populateSampleColumns()
  #Sample <-readNWISSample("06934500","00631","1970-10-01","2011-09-30")

  # prepare site info
  info <- site_info %>%
    dplyr::filter(site_no == nwis_site) %>%
    mutate(ref_date = ref_Date)
  # INFO <- readNWISInfo(siteNumber,pCode,interactive=FALSE)

  # combine the inputs into an EGRET list, first running a check & filter to
  # make sure the merge will go smoothly
  flow_conc <- left_join(conc, flow, by='Date')
  if(any(is.na(flow_conc$Q))) {
    noQ_dates <- filter(flow_conc, is.na(Q))$Date
    message(sprintf(
      'Found %d NA values in Q after merging flow and conc; removing those samples',
      length(noQ_dates)))
    message(sprintf('Bad dates: %s', paste(noQ_dates, collapse=', ')))
    conc <- dplyr::filter(conc, !(Date %in% noQ_dates))
  }
  eList <- EGRET::mergeReport(INFO=info, Daily=flow, Sample=conc, verbose=FALSE)

  return(eList)
}
