fig_error_v_leadtime <- function(fig_ind, preds_ind, agg_nwis_ind, remake_file, config_file) {

  # predictions
  preds_df <- readRDS(sc_retrieve(preds_ind, remake_file)) %>%
    mutate(LeadTime = as.numeric(Date - ref_date, units='days'))

  # "truth"
  agg_nwis <- readRDS(sc_retrieve(agg_nwis_ind, remake_file))
  agg_nwis$flux <- left_join(agg_nwis$nitrate_sensor, agg_nwis$flow, by=c('site_no','date'), suffix=c('_conc','_flow')) %>%
    mutate(daily_mean_flux = daily_mean_conc * daily_mean_flow * 60*60*24/1000) %>% # flow in kg/d
    rename(site=site_no)

  # error
  error_df <- left_join(agg_nwis$flux, preds_df,
                        by = c('site' = 'Site', 'date' = 'Date'),
                        suffix = c('_truth', '_pred')) %>%
    dplyr::filter(!is.na(Flux), !is.na(daily_mean_flux)) %>%
    mutate(flux_error = Flux - daily_mean_flux) %>%
    group_by(site) %>%
    mutate(z_score_flux_error = (flux_error-mean(flux_error))/sd(flux_error)) %>% # z_score for error comparison between sites
    ungroup()

  # create the plot
  g <- ggplot(mutate(preds_df, LeadTime=as.numeric(Date - ref_date, units='days')), aes(x=Date, y=Flux/1000, color=LeadTime)) +
    geom_point() +
    geom_line(data=filter(agg_nwis$flux, date %in% preds_df$Date), aes(x=date, y=daily_mean_flux/1000), color='red') +
    facet_grid(site ~ ., scale='free_y') +
    scale_color_continuous('Lead Time (d)') +
    xlab('Date') +
    ylab(expression('Flux'~(Mg~'N-NO'[3]~d^{-1}))) +
    theme_classic()

  # save and post to Drive
  fig_file <- as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width=6, height=5)
  gd_put(remote_ind=fig_ind, local_source=fig_file, config_file=config_file)
}
