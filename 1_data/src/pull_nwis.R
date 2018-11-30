pull_nwis <- function(ind_file, sites_subset_yml, dates_yml, params_yml, gd_config) {

  dates <- yaml::yaml.load_file(dates_yml)
  pcodes <- yaml::yaml.load_file(params_yml)
  site_ids <- yaml::yaml.load_file(sites_subset_yml)

  dates$pull$start = '2010-01-01' # pulling less data and not changing dates.yml to not force a rebuild

  message('  starting download of NWIS flow data at ', Sys.time())
  flow <- lapply(site_ids, function(site){
    tryCatch({readNWISuv(
      siteNumbers=site, parameterCd=pcodes$flow,
      startDate=dates$pull$start, endDate=dates$forecast$end) %>%
        mutate(parm_cd = '00060') %>%
        rename(result_va=X_00060_00000, remark_cd=X_00060_00000_cd)
      }, error = function(e){
        NULL
      })
  })

  flow_munged <- flow[!sapply(flow, is.null)] %>%
    bind_rows()

  # message('  starting download of NWIS nitrate sensor data at ', Sys.time())
  # nitrate_sensor <- readNWISuv(
  #   siteNumbers=site_ids, parameterCd=pcodes$nitrate,
  #   # startDate=dummyStart, endDate=dummyEnd) %>%
  #   startDate=dates$pull$start, endDate=dates$forecast$end) %>%
  #   mutate(parm_cd = '99133') %>%
  #   rename(result_va=X_99133_00000, remark_cd=X_99133_00000_cd)

  message('  starting download of NWIS nitrate grab sample data at ', Sys.time())
  nitrate_grab <- lapply(site_ids, function(site){
    tryCatch({readNWISqw(
      siteNumber=site, parameterCd=pcodes$nitrate,
      startDate=dates$pull$start, endDate=dates$forecast$end)
    }, error = function(e){
      NULL
    })
  })

  nitrate_grab_munged <- nitrate_grab[!sapply(nitrate_grab, is.null)] %>%
    bind_rows()

  message('  finished downloading all NWIS data at ', Sys.time())

  data_file <- as_data_file(ind_file)
  # saveRDS(list(flow = flow, nitrate_sensor = nitrate_sensor, nitrate_grab = nitrate_grab), data_file)
  saveRDS(list(flow = flow_munged, nitrate_grab = nitrate_grab_munged), data_file)
  # saveRDS(nitrate_grab_munged, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}
