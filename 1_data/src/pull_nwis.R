
pull_nwis <- function(ind_file, sites_subset_yml, dates_subset_yml, params_yml, gd_config) {

  dates <- yaml::yaml.load_file(dates_subset_yml)
  pcodes <- yaml::yaml.load_file(params_yml)
  site_ids <- yaml::yaml.load_file(sites_subset_yml)

  flow <- readNWISuv(siteNumbers=site_ids, parameterCd=pcodes$flow, startDate=dates$calibrate$start, endDate=dates$forecast$start)
  nitrate_sensor <- readNWISuv(siteNumbers=site_ids, parameterCd=pcodes$nitrate, startDate=dates$calibrate$start, endDate=dates$forecast$start)
  nitrate_grab <- readNWISqw(siteNumber=site_ids, parameterCd=pcodes$nitrate, startDate=dates$calibrate$start, endDate=dates$forecast$start)

  data_file <- as_data_file(ind_file)
  saveRDS(list(flow = flow, nitrate_sensor = nitrate_sensor, nitrate_grab = nitrate_grab), data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}
