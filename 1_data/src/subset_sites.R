
subset_sites <- function(ind_file, sites_yml, dates_yml, params_yml, gd_config){
  # subsetting the sites for which have longer time series of grab samples in addition to nitrate sensor data

  dates <- yaml::yaml.load_file(dates_yml)
  pcodes <- yaml::yaml.load_file(params_yml)
  sites <- yaml::yaml.load_file(sites_yml)

  nitrate_data <- lapply(sites, function(site){
    tryCatch({whatNWISdata(siteNumber = site,
                 parameterCd = c('00630','00631'), # checking for grab nitrate data
                 startDT = dates$pull$start,
                 endDT= dates$forecast$end)
    }, error = function(e) {
     NULL
    })
  })

  nitrate_data_munged <- lapply(nitrate_data[!sapply(nitrate_data, is.null)], function(nitrate) {
    nitrate %>% mutate(
      alt_acy_va = as.character(alt_acy_va) # some of these were numeric b/c all NA, others chr
    )
  }) %>% bind_rows() %>% as_data_frame()

  no3_sites <- nitrate_data_munged %>%
    dplyr::filter(
      is.na(stat_cd),
      count_nu > 50) %>%  # choosing sites over 300 grab samples
    dplyr::pull(site_no) %>%
    sort()

  data_file <- as_data_file(ind_file)
  readr::write_lines(sprintf("- '%s'", no3_sites), data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}
