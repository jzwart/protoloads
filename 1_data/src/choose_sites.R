choose_sites <- function(ind_file, dates_yml, params_yml, gd_config) {

  # parameters for whatNWISdata, from https://waterservices.usgs.gov/rest/Site-Service.html
  # data_type_cd is associated with a value of "dv". "dv" stands for daily values. So this site collects or collected daily values. Data type codes are defined here.
  # parm_cd is the USGS parameter code defined here External Link. 00010 corresponds to water temperature in degrees Celsius. So now we know this site this site collects daily values for water temperature.
  # stat_cd stands for statistics code defined here External Link. 00003 corresponds to mean, so now we know this site collects daily values for water temperature published as a mean (average) value for the day.
  # begin_date means the date that this information started to be collected, which was October 1, 1988.
  # end_date means end date and is April 8, 2011. If the site is active, this date will usually be the last full day. In short, mean water temperature is still being collected, so there is a continuous record for this kind of data since October 1, 1988 through April 8, 2011.
  # count_nu is count number, or the number of mean daily values that are available for water temperature for this site, which is 7856, presumably one for each day from October 1, 1988 through April 8, 2011.

  dates <- yaml::yaml.load_file(dates_yml)
  pcodes <- yaml::yaml.load_file(params_yml)

  # //waterservices.usgs.gov/nwis/site/?format=rdb&stateCd=al&startDT=2013-03-01&endDT=2018-03-01&parameterCd=00630,99124,00631,99133,99137&siteType=ST&siteStatus=all&hasDataTypeCd=iv
  hucs <- sprintf('%02d', 1:21)
  inventories <- lapply(hucs, function(huc) {
    message(paste('HUC', huc))
    tryCatch({
      dataRetrieval::whatNWISdata(
        huc=huc, siteType='ST',
        hasDataTypeCd='iv', parameterCd=c(pcodes$flow, pcodes$nitrate),
        startDT=dates$calibrate$start, endDT=dates$forecast$end)
    }, error=function(e) {
      NULL
    })
  })
  inv_munged <- lapply(inventories[!sapply(inventories, is.null)], function(inv) {
    inv %>% mutate(
      alt_acy_va = as.character(alt_acy_va) # some of these were numeric b/c all NA, others chr
    )
  }) %>% bind_rows() %>% as_data_frame()

  # 162 sites have instantaneous values for pcode 99133, so let's use that pcode
  # inv_munged %>%
  #   select(site_no, parm_cd, stat_cd, count_nu) %>%
  #   filter(site_no %in% flow_sites, is.na(stat_cd), parm_cd %in% pcodes$nitrate) %>%
  #   group_by(parm_cd) %>%
  #   summarize(n_sites=length(site_no), n_obs=sum(count_nu))

  # identify those sites with
  flow_sites <- inv_munged %>%
    filter(
      is.na(stat_cd),
      parm_cd %in% pcodes$flow,
      as.Date(begin_date) <= as.Date(dates$calibrate$start),
      as.Date(end_date) >= as.Date(dates$forecast$end)) %>%
    pull(site_no)

  # of those 162 sites, we want the sites with long time series that include the period of
  no3_sites <- inv_munged %>%
    filter(
      site_no %in% flow_sites,
      is.na(stat_cd),
      parm_cd=='99133',
      as.Date(begin_date) <= as.Date(dates$calibrate$start),
      as.Date(end_date) >= as.Date(dates$forecast$end)) %>%
    pull(site_no) %>%
    sort()

  data_file <- as_data_file(ind_file)
  readr::write_lines(sprintf("- '%s'", no3_sites), data_file)
  gd_put(remote_ind=ind_file, local_source=data_file, config_file=gd_config)
}
