aggregate_inputs <- function(ind_file, raw_ind_file, remake_file, sites_yml, dates_yml, params_yml, gd_config) {

  # aggregating NWIS data to a daily scale
  sites = yaml::yaml.load_file(sc_retrieve(sites_yml,remake_file = remake_file))
  dates = yaml::yaml.load_file(dates_yml)

  input_raw = readRDS(sc_retrieve(raw_ind_file, remake_file = remake_file))

  agg_sensor = lapply(sites, function(site){
    agg_out = input_raw$nitrate_sensor %>%
      dplyr::filter(site_no == site) %>% # stats package overrides dplyr filter()
      mutate(date = as.Date(dateTime)) %>%
      group_by(date) %>%
      summarise(X_99133_000000_mean = mean(X_99133_00000)) %>%
      mutate(site_no = site)
  }) %>% bind_rows() %>% as_data_frame()



}
