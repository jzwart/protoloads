
pull_nwis <- function(outfile, site_ids) {

  dates <- yaml::yaml.load_file('1_data/cfg/dates.yml')$calibrate
  pcodes <- yaml::yaml.load_file('1_data/cfg/params.yml')

  if(is.null(dates$end)){
    flow <- readNWISuv(siteNumbers=site_ids, parameterCd=pcodes$flow, startDate=dates$start)
    nitrate <- readNWISuv(siteNumbers=site_ids, parameterCd=pcodes$nitrate, startDate=dates$start)
  }else{
    flow <- readNWISuv(siteNumbers=site_ids, parameterCd=pcodes$flow, startDate=dates$start, endDate=dates$end)
    nitrate <- readNWISuv(siteNumbers=site_ids, parameterCd=pcodes$nitrate, startDate=dates$start, endDate=dates$end)
  }

  writeLines(list(flow=flow, nitrate=nitrate), outfile)
}

