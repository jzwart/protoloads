aggregate_nwis <- function(ind_file, raw_ind_file, remake_file, sites_yml, gd_config) {

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

aggregate_nwm <- function(ind_file, raw_ind_file, remake_file, sites_yml, comids_file, gd_config) {
  # this is just a place holder for now until nwm nc files are in correct format


  # aggregating NWM data to a daily scale
  sites = yaml::yaml.load_file(sc_retrieve(sites_yml,remake_file = remake_file))

  comids_lookup = readr::read_delim(sc_retrieve(comids_file, remake_file = remake_file), delim='\t')

  input_raw = nc_open(sc_retrieve(raw_ind_file, remake_file = remake_file))

  keep <- input_raw$dim$feature_id$vals %in% comids_lookup$COMID # comids

  dimids <- input_raw$var$streamflow$dimids

  site_inds <- which(keep)

  if(length(dimids) == 2) {
    streamflow <- matrix(nrow=input_raw$dim$time$len, ncol=length(site_inds))
  } else if (length(dimids) == 3) {
    if(!all(input_raw$var$streamflow$dimids == c(0,2,1))) stop("dimids now as expected")
    streamflow <- array(dim = c(length(site_inds), input_raw$dim$time$len, input_raw$dim$reference_time$len))
  }

  for(s in 1:length(site_inds)) {
    if(length(dimids) == 2) {
      # Note axis order is assumed here!!!
      streamflow[,s] <- ncvar_get(input_raw, input_raw$var$streamflow,
                                  start = c(site_inds[s], 1),
                                  count = c(1, -1))
      # time <- input_raw$var$streamflow$dim[[2]]$vals # time value; minutes since 1970-01-01 00:00:00 UTC



    } else if(length(dimids) == 3) {
      for(r in 1:input_raw$dim$reference_time$len) {
        streamflow[s,,r] <- ncvar_get(input_raw, input_raw$var$streamflow,
                                      start = c(site_inds[s], 1, r),
                                      count = c(1, -1, 1))
      }
    }
  }
  # stream flow is [site, valid time, reference time]

  ncvar_put(new_nc, new_nc$var$streamflow, streamflow)

  if("reference_time" %in% names(nc$dim)) {
    ncvar_put(new_nc, "reference_time", nc$dim$reference_time$vals)
  }




  agg_nwm = lapply(sites, function(site){
    agg_out = input_raw$nitrate_sensor %>%
      dplyr::filter(site_no == site) %>% # stats package overrides dplyr filter()
      mutate(date = as.Date(dateTime)) %>%
      group_by(date) %>%
      summarise(X_99133_000000_mean = mean(X_99133_00000)) %>%
      mutate(site_no = site, tz_cd = 'UTC')
  }) %>% bind_rows() %>% as_data_frame()


  data_file <- as_data_file(ind_file)
  saveRDS(list(flow = agg_flow, nitrate_sensor = agg_sensor, nitrate_grab = agg_grab), data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}
