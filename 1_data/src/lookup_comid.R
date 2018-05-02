lookup_comid <- function(ind_file, sites_yml, site_lookup, gd_config) {

  lookup <- sf::st_read(sc_retrieve(site_lookup))

  sites <- yaml::yaml.load_file(sc_retrieve(sites_yml))

  comids <- dplyr::filter(lookup, site_id %in% sites)$COMID

  data_file <- as_data_file(ind_file)
  writeLines(sprintf("- '%s'", comids), data_file)
  gd_put(remote_ind=ind_file, local_source=data_file, config_file=gd_config)
}
