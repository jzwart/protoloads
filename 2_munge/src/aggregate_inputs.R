# aggregate NWIS data to a daily scale
aggregate_nwis <- function(ind_file, raw_ind_file, remake_file, sites_file, gd_config) {

  # read in data files
  sites <- readr::read_tsv(sc_retrieve(sites_file, remake_file = remake_file))$site_no
  input_raw <- readRDS(sc_retrieve(raw_ind_file, remake_file = remake_file))

  agg_flow <- input_raw$flow %>%
    dplyr::filter(site_no %in% sites) %>% # stats package overrides dplyr filter()
    mutate(date = as.Date(dateTime)) %>%
    group_by(site_no, date, parm_cd) %>%
    summarise(
      daily_mean = mean(result_va) * 0.02831685, # convert to m^3 s^-1
      daily_cd = ifelse('<' %in% remark_cd, '<', '')) %>%
    ungroup() %>%
    mutate(tz_cd = 'UTC') %>%
    as_data_frame()

  # agg_sensor = input_raw$nitrate_sensor %>%
  #   dplyr::filter(
  #     site_no %in% sites,
  #     remark_cd != 'P Eqp') %>%
  #   mutate(date = as.Date(dateTime)) %>%
  #   group_by(site_no, date, parm_cd) %>%
  #   summarise(
  #     daily_mean = mean(result_va),
  #     daily_cd = ifelse('<' %in% remark_cd, '<', '')) %>%
  #   ungroup() %>%
  #   mutate(tz_cd = 'UTC') %>%
  #   as_data_frame()

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
  saveRDS(list(flow = agg_flow, nitrate_grab = agg_grab), data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}

# read NWM data from nc files and aggregate to daily means
aggregate_nwm <- function(ind_file, raw_ind_file, remake_file, sites_file, gd_config) {

  # read data files
  sites <- readr::read_tsv(sc_retrieve(sites_file, remake_file = remake_file))
  input_raw <- nc_open(sc_retrieve(raw_ind_file, remake_file = remake_file))

  # pick out site indices and identifiers
  site_inds <- which(input_raw$dim$feature_id$vals %in% sites$COMID) # indices into original nc file
  new_feature_id <- input_raw$dim$feature_id$vals[site_inds] # comid list
  site_nos = sites$site_no[match(input_raw$dim$feature_id$vals[site_inds],sites$COMID)]

  # decide what sort of file we're working with (retro or forecast)
  dimids <- input_raw$var$streamflow$dimids
  is_analysis <- grepl('ana', ind_file)
  is_retro <- length(dimids) == 2 # or grepl('retro', ind_file)
  is_forecast <- !is_analysis && length(dimids) == 3

  # generate a tidy data_frame of predictions
  if(is_retro) {
    ref_times <- convert_time_nc2posix(input_raw$var$streamflow$dim[[2]])
    raw_nwm <- bind_rows(lapply(1:length(site_inds), function(s) {
      data_frame(
        site_no=site_nos[s],
        time=ref_times,
        flow = ncvar_get(
          input_raw, input_raw$var$streamflow,
          start = c(site_inds[s], 1),
          count = c(1, -1),
          raw_datavals = FALSE) # F means apply the scaleFactor
      )
    }))
  } else if(is_analysis || is_forecast) {
    val_times <- convert_time_nc2posix(input_raw$var$streamflow$dim[[2]])
    ref_times <- convert_time_nc2posix(input_raw$var$streamflow$dim[[3]])
    raw_nwm <- bind_rows(lapply(1:length(site_inds), function(s) {
      bind_rows(lapply(1:length(ref_times), function(r) {
        data_frame(
          site_no=site_nos[s],
          ref_time=ref_times[r],
          valid_time=val_times[,r],
          flow = ncvar_get(
            input_raw, input_raw$var$streamflow,
            start = c(site_inds[s], 1, r),
            count = c(1, -1, 1),
            raw_datavals = FALSE) # F means apply the scaleFactor
        )
      }))
    }))
  }

  # munge and group data according to the data type
  if(is_retro){
    valid_time_step <- 1
    grouped_nwm <- raw_nwm %>%
      # use 1am to 12am to be more similar to med & long forecast daily means:
      mutate(date = as.Date(time - as.difftime(1/60, units = 'hours'))) %>%
      group_by(site_no, date)

  } else if(is_analysis) {
    valid_time_step <- 1
    grouped_nwm <- raw_nwm %>%
      mutate(
        valid_date = as.Date(valid_time)) %>%
      dplyr::filter(valid_time == ref_time + as.difftime(3, units='hours')) %>%
      group_by(site_no, valid_date)

  } else if(is_forecast) {
    # decide what time step should be: medium range valid_date time step is 3
    # hours, long range is 6 hours
    valid_time_step <- ifelse(length(grep('med',ind_file)) > 0, 3, 6)
    grouped_nwm <- raw_nwm %>%
      dplyr::filter(grepl('00:00:00', ref_time)) %>% # we only want ref dates that start at midnight
      mutate(
        ref_date = as.Date(ref_time),
        valid_date = as.Date(valid_time - as.difftime(1/60, units = 'hours'))) %>%
      group_by(site_no, ref_date, valid_date)
  }

  # aggregate according to the groups set above
  agg_nwm <- grouped_nwm %>%
    summarise(
      n = length(flow),
      flow = if(n==24/valid_time_step) mean(flow) else NA) %>%
    ungroup() %>%
    dplyr::filter(!is.na(flow))

  # write and post the output
  data_file <- as_data_file(ind_file)
  saveRDS(agg_nwm, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}


