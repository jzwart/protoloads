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

aggregate_nwm <- function(ind_file, raw_ind_file, remake_file, sites_file, gd_config) {

  # aggregating NWM data to a daily scale
  sites = readr::read_delim(sc_retrieve(sites_file,remake_file = remake_file), delim='\t')

  input_raw = nc_open(sc_retrieve(raw_ind_file, remake_file = remake_file))

  site_inds <- which(input_raw$dim$feature_id$vals %in% sites$COMID) # indices into original nc file

  #need to figure out which site_no of the duplicates are correct;
  # looks like the second half of the all the site pulls are correct; but need to check this later if new sites are added
  site_inds <- site_inds[4:6]

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


  if(length(grep('retro',ind_file))>0){

    agg_nwm <- streamflow %>%
      as.data.frame() %>%
      setNames('flow') %>%
      mutate(
        date = rep(as.Date(time), length(input_raw$dim$feature_id$vals[site_inds])),
        site_no = rep(sites$site_no[match(input_raw$dim$feature_id$vals[site_inds],sites$COMID)], each = length(time))) %>%
      group_by(site_no, date) %>%
      summarise(
        flow = mean(flow)) %>%
      ungroup()

  }else{
    valid_time_step = ifelse(length(grep('med',ind_file))>0, 3, 6) #medium range valid_date time step is 3 hours, long range is 6 hours

    agg_nwm <- streamflow %>%
      as.data.frame() %>%
      setNames(ref_time) %>% # columns are ref_time
      select(contains('00:00:00')) %>% # we only want ref dates that start at midnight
      dplyr::do(with(., {
        ref_dates = rep(as.Date(colnames(.), tz = 'UTC'), each = nrow(.))

        out = data.frame(
          ref_date = ref_dates,
          valid_date = rep(valid_time, length(unique(ref_dates)) * length(sites)),
          flow = as.vector(as.matrix(.)),
          site_no = rep(rep(sites$site_no[match(input_raw$dim$feature_id$vals[site_inds],sites$COMID)],
                        each = input_raw$dim$time$len), length(unique(ref_dates))))
      })) %>%
      mutate(valid_date = as.Date(as.POSIXlt(ref_date) + as.difftime(valid_date * valid_time_step, units = 'hours'))) %>%
      group_by(ref_date, valid_date, site_no) %>%
      summarise(
        flow = mean(flow)) %>%
      ungroup()
  }

  data_file <- as_data_file(ind_file)
  saveRDS(agg_nwm, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}


