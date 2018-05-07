aggregate_inputs <- function(ind_file, raw_ind_file, remake_file, sites_yml, gd_config) {

  # aggregating NWIS data to a daily scale
  sites = yaml::yaml.load_file(sc_retrieve(sites_yml,remake_file = remake_file))

  input_raw = readRDS(sc_retrieve(raw_ind_file, remake_file = remake_file))

  agg_sensor = lapply(sites, function(site){
    agg_out = input_raw$nitrate_sensor %>%
      dplyr::filter(site_no == site) %>% # stats package overrides dplyr filter()
      mutate(date = as.Date(dateTime)) %>%
      group_by(date) %>%
      summarise(X_99133_000000_mean = mean(X_99133_00000)) %>%
      mutate(site_no = site, tz_cd = 'UTC')
  }) %>% bind_rows() %>% as_data_frame()

  agg_flow = lapply(sites, function(site){
    agg_out = input_raw$flow %>%
      dplyr::filter(site_no == site) %>% # stats package overrides dplyr filter()
      mutate(date = as.Date(dateTime)) %>%
      group_by(date) %>%
      summarise(X_00060_00000_mean = mean(X_00060_00000)) %>%
      mutate(site_no = site, tz_cd = 'UTC')
  }) %>% bind_rows() %>% as_data_frame()

  agg_grab = lapply(sites, function(site){
    agg_out = input_raw$nitrate_grab %>%
      dplyr::filter(site_no == site, parm_cd != '99133') %>% # stats package overrides dplyr filter()
      mutate(date = as.Date(startDateTime)) %>%
      group_by(date, parm_cd) %>%
      summarise(result_va_mean = mean(result_va)) %>%
      mutate(site_no = site, tz_cd = 'UTC')
  }) %>% bind_rows() %>% as_data_frame()

  data_file <- as_data_file(ind_file)
  saveRDS(list(flow = agg_flow, nitrate_sensor = agg_sensor, nitrate_grab = agg_grab), data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}
