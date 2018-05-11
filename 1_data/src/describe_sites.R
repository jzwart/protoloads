describe_sites <- function(ind_file, sites_comids_ind, params_yml, remake_file, gd_config) {

  # read the comid-site table. don't bother filtering to the final 3 sites, just prep all 8
  comids <- readr::read_tsv(sc_retrieve(sites_comids_ind, remake_file=remake_file))
  pcodes <- yaml::yaml.load_file(params_yml)$nitrate

  # function to create a single value per column - either take the 1 unique
  # value or paste the unique values together into a single character string
  unique_or_concat <- function(vals) {
    unique_vals <- unique(vals)
    if(length(unique_vals) == 1) {
      return(unique_vals)
    } else {
      return(paste(unique_vals, collapse=';'))
    }
  }

  # go to NWIS for site details, then join with COMIDs
  site_info <- lapply(comids$site_id, function(nwis_site) {
    lapply(pcodes, function(pcode) {
      readNWISInfo(nwis_site, pcode, interactive=FALSE)
    }) %>%
      bind_rows() %>%
      mutate(project_no = as.character(project_no)) %>%
      summarize_all(.funs=funs(unique_or_concat))
  }) %>%
    bind_rows() %>%
    left_join(comids, by=c(site_no='site_id'))

  # write and post the compiled site/parameter information
  data_file <- scipiper::as_data_file(ind_file)
  readr::write_tsv(site_info, data_file)
  gd_put(ind_file, data_file, config_file=gd_config)

}
