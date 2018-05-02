
pull_nwis <- function(ind_file, sites_yml, dates_yml, params_yml, gd_config) {

  dates <- yaml::yaml.load_file(dates_yml)
  pcodes <- yaml::yaml.load_file(params_yml)
  site_ids <- yaml::yaml.load_file(sites_yml)

  flow <- readNWISuv(siteNumbers=site_ids, parameterCd=pcodes$flow, startDate=dates$calibrate$start, endDate=dates$forecast$start)
  nitrate <- readNWISuv(siteNumbers=site_ids, parameterCd=pcodes$nitrate, startDate=dates$calibrate$start, endDate=dates$forecast$start)

  writeLines(list(flow=flow, nitrate=nitrate), outfile)

  data_file <- as_data_file(ind_file)
  writeLines(list(flow = flow, nitrate = nitrate), data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}

