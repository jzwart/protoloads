aggregate_nwis <- function(ind_file, raw_ind_file, remake_file, sites_yml, gd_config) {

  # aggregating NWIS data to a daily scale
  sites = yaml::yaml.load_file(sc_retrieve(sites_yml,remake_file = remake_file))

  input_raw = readRDS(sc_retrieve(raw_ind_file, remake_file = remake_file))

  agg_flow <- input_raw$flow %>%
    dplyr::filter(site_no %in% sites) %>% # stats package overrides dplyr filter()
    mutate(date = as.Date(dateTime)) %>%
    group_by(site_no, date, parm_cd) %>%
    summarise(
      daily_mean = mean(result_va),
      daily_cd = ifelse('<' %in% remark_cd, '<', '')) %>%
    ungroup() %>%
    mutate(tz_cd = 'UTC') %>%
    as_data_frame()

  agg_sensor = input_raw$nitrate_sensor %>%
    dplyr::filter(
      site_no %in% sites,
      remark_cd != 'P Eqp') %>%
    mutate(date = as.Date(dateTime)) %>%
    group_by(site_no, date, parm_cd) %>%
    summarise(
      daily_mean = mean(result_va),
      daily_cd = ifelse('<' %in% remark_cd, '<', '')) %>%
    ungroup() %>%
    mutate(tz_cd = 'UTC') %>%
    as_data_frame()

  agg_grab = input_raw$nitrate_grab %>%
    dplyr::filter(site_no %in% sites, parm_cd != '99133') %>% # stats package overrides dplyr filter()
    mutate(date = as.Date(startDateTime)) %>%
    group_by(site_no, date, parm_cd) %>%
    summarise(
      daily_mean = mean(result_va),
      daily_cd = ifelse('<' %in% remark_cd, '<', '')) %>%
    ungroup() %>%
    mutate(tz_cd = 'UTC') %>%
    as_data_frame()

  data_file <- as_data_file(ind_file)
  saveRDS(list(flow = agg_flow, nitrate_sensor = agg_sensor, nitrate_grab = agg_grab), data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}

aggregate_nwm <- function(ind_file, raw_ind_file, remake_file, sites_yml, comids_file, gd_config) {

  # aggregating NWM data to a daily scale
  sites = yaml::yaml.load_file(sc_retrieve(sites_yml,remake_file = remake_file))

  comids_lookup = readr::read_delim(sc_retrieve(comids_file, remake_file = remake_file), delim='\t')

  input_raw = nc_open(sc_retrieve(raw_ind_file, remake_file = remake_file))

  site_inds <- match(comids_lookup$COMID, input_raw$dim$feature_id$vals) # indices into original nc file

  new_feature_id <- input_raw$dim$feature_id$vals[site_inds] # comid list

  dimids <- input_raw$var$streamflow$dimids

  if(length(dimids) == 2) {
    streamflow <- matrix(nrow=input_raw$dim$time$len*length(site_inds), ncol=1)
    time <- convert_time_nc2posix(input_raw$var$streamflow$dim[[2]]) # time value converted to posix
  } else if (length(dimids) == 3) {
    streamflow <- matrix(nrow = length(site_inds) * input_raw$dim$time$len, ncol = input_raw$dim$reference_time$len)
    valid_time <- input_raw$var$streamflow$dim[[2]]$vals
    ref_time <- convert_time_nc2posix(input_raw$var$streamflow$dim[[3]]) # ref time value converted to posix
  }

  for(s in 1:length(site_inds)) {
    if(length(dimids) == 2) {
      streamflow[((s-1)*length(time)+1):((s)*length(time)),1] <- ncvar_get(input_raw, input_raw$var$streamflow,
                start = c(site_inds[s], 1),
                count = c(1, -1),
                raw_datavals = TRUE)

    } else if(length(dimids) == 3) {
      for(r in 1:input_raw$dim$reference_time$len) {
        streamflow[((s-1)*length(valid_time)+1):((s)*length(valid_time)),r] <- ncvar_get(input_raw, input_raw$var$streamflow,
                                                                             start = c(site_inds[s], 1, r),
                                                                             count = c(1, -1, 1),
                                                                             raw_datavals = TRUE)
      }
    }
  }

  #need streamflow, site_no, dateTime, for retro
  # need streamflow, site_no, refTime, validTime, for forecast
  if(length(grep('retro',ind_file))>0){

    agg_nwm <- streamflow %>%
      as.data.frame() %>%
      setNames('flow') %>%
      mutate(
        date = rep(as.Date(time), 3),
        site_no = rep(comids_lookup$site_id[match(input_raw$dim$feature_id$vals[site_inds],comids_lookup$COMID)], each = length(time))) %>%
      group_by(site_no, date) %>%
      summarise(
        flow = mean(flow)) %>%
      ungroup()

  }else{

    agg_nwm <- streamflow %>%
      as.data.frame() %>%
      setNames(ref_time) %>% # columns are ref_time
      select(contains('00:00:00')) %>% # we only want ref dates that start at midnight
      dplyr::do(with(., {
        ref_dates = rep(as.Date(colnames(.)), each = nrow(.))

        out = data.frame(
          ref_date = ref_dates,
          valid_date = rep(valid_time, length(unique(ref_dates)) * length(sites)),
          flow = as.vector(as.matrix(.)),
          site_no = rep(rep(comids_lookup$site_id[match(input_raw$dim$feature_id$vals[site_inds],comids_lookup$COMID)],
                        each = input_raw$dim$time$len), length(unique(ref_dates))))
      })) # need to figure out what valid date vals mean; what are the time steps?
  }

  data_file <- as_data_file(ind_file)
  saveRDS(agg_nwm, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}


