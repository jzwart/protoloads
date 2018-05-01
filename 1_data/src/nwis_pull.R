choose_sites <- function(outfile) {
  dataRetrival::whatNWISdata()

  sites <- dataRetrieval::readNWISsite(site_ids)

  yaml::write_yaml(sites)
}

pull_nwis <- function(outfile, site_ids) {

  dates <- yaml::yaml.load_file('1_data/cfg/data_pulls.yml')$calibrate


  flow_codes <- '00060'
  nitrate_codes <- c('00630','99124','00631','99133','99137')
  flow <- readNWISuv(siteNumbers=site_ids, parameterCd=flow_codes, startDate=dates$start, endDate=dates$end)
  nitrate <- readNWISuv(siteNumbers=site_ids, parameterCd=nitrate_codes, startDate=dates$start, endDate=dates$end)

  writeLines(list(flow=flow, nitrate=nitrate), outfile)
}
