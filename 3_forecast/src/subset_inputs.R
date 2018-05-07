subset_inputs <- function(
  nwis_inputs, nwis_site,
  nwm_inputs, nwm_model,
  ref_date, remake_file
) {

  nwis_in <- readRDS(sc_retrieve(nwis_inputs, remake_file))
  nwm_in <- readRDS(sc_retrieve(nwm_inputs, remake_file))

  calib_ndays <- as.integer(365.25*5) # 5 years
  start_calib <- ref_date - as.difftime(calib_ndays, units='days')
  forecast_range <- as.integer(ifelse(nwm_model=='med', 10, 30))
  end_forecast <- ref_date + as.difftime(forecast_range-1, units='days')

  # dummy for now: return a well-formed eList
  eList <- EGRET::Choptank_eList

  return(eList)
}
