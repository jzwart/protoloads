subset_inputs <- function(ind_file, all_inputs, nwm_model, start_calibrate, start_forecast) {
  # dummy for now: return a well-formed eList
  eList <- EGRET::Choptank_eList

  data_file <- as_data_file(ind_file)
  saveRDS(eList, file=data_file)
  sc_indicate(ind_file, data_file=data_file)
}
