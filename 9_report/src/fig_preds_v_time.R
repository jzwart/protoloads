fig_preds_v_time <- function(fig_ind, preds_ind, agg_nwis_ind, remake_file, config_file) {

  # read in the predictions
  preds_df <- readRDS(sc_retrieve(preds_ind, remake_file))

  # read in "truth"
  agg_nwis <- readRDS(sc_retrieve(agg_nwis_ind, remake_file))
  agg_nwis$flux <- left_join(agg_nwis$nitrate_sensor, agg_nwis$flow, by=c('site_no','date'), suffix=c('_conc','_flow')) %>%
    mutate(daily_mean_flux = daily_mean_conc * daily_mean_flow * 60*60*24/1000) %>% # flow in kg/d
    rename(site=site_no)

  # create the plot
  g <- ggplot(preds_loadest, aes(x=Date, y=Flux, color=ref_date)) +
    geom_point() +
    facet_wrap(~site, scale='free_y', nrow=3)



  ggplot(mutate(preds_df, LeadTime=as.numeric(Date - ref_date, units='days')), aes(x=Date, y=Flux, color=LeadTime)) +
    geom_point() +
    geom_line(data=filter(agg_nwis$flux, date %in% preds_df$Date), aes(x=date, y=daily_mean_flux), color='red') +
    facet_grid(site ~ ., scale='free_y') +
    theme_classic()

  ggplot(mutate(preds_loadest, LeadTime=as.numeric(Date - ref_date, units='days')), aes(x=Date, y=Flux, color=LeadTime)) +
    geom_point() +
    geom_line(data=filter(agg_nwis$flux, date %in% preds_df$Date), aes(x=date, y=daily_mean_flux), color='red') +
    facet_grid(site ~ ., scale='free_y') +
    theme_classic()






  # save and post to Drive
  fig_file <- as_data_file(fig_ind)
  ggsave(fig_file, plot=g)
  gd_put(remote_ind=fig_ind, local_source=fig_file, config_file=config_file)
}
