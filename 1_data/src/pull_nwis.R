pull_nwis <- function(ind_file, sites_subset_yml, dates_yml, params_yml, gd_config) {

  dates <- yaml::yaml.load_file(dates_yml)
  pcodes <- yaml::yaml.load_file(params_yml)
  site_ids <- yaml::yaml.load_file(sites_subset_yml)

  dates$pull$start = '2010-01-01' # pulling less data and not changing dates.yml to not force a rebuild

  # dummyStart=as.Date(dates$forecast$start) - as.difftime(5, units='days')
  # dummyEnd=as.Date(dates$forecast$start) + as.difftime(5, units='days')
  message('  starting download of NWIS flow data at ', Sys.time())
  flow <- readNWISuv(
    siteNumbers=site_ids, parameterCd=pcodes$flow,
    # startDate=dummyStart, endDate=dummyEnd) %>%
    startDate=dates$pull$start, endDate=dates$forecast$end) %>%
    mutate(parm_cd = '00060') %>%
    rename(result_va=X_00060_00000, remark_cd=X_00060_00000_cd)
  message('  starting download of NWIS nitrate sensor data at ', Sys.time())
  nitrate_sensor <- readNWISuv(
    siteNumbers=site_ids, parameterCd=pcodes$nitrate,
    # startDate=dummyStart, endDate=dummyEnd) %>%
    startDate=dates$pull$start, endDate=dates$forecast$end) %>%
    mutate(parm_cd = '99133') %>%
    rename(result_va=X_99133_00000, remark_cd=X_99133_00000_cd)
  message('  starting download of NWIS nitrate grab sample data at ', Sys.time())
  nitrate_grab <- readNWISqw(
    siteNumber=site_ids, parameterCd=pcodes$nitrate,
    # startDate=dummyStart, endDate=dummyEnd)
    startDate=dates$pull$start, endDate=dates$forecast$end)
    message('  finished downloading all NWIS data at ', Sys.time())

  data_file <- as_data_file(ind_file)
  saveRDS(list(flow = flow, nitrate_sensor = nitrate_sensor, nitrate_grab = nitrate_grab), data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}
