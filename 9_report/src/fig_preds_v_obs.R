fig_preds_v_obs <- function(fig_ind, loadest_preds_ind, wrtds_preds_ind, agg_nwis_ind, remake_file, config_file){
  # read in the predictions
  loadest_preds_df <- readRDS(sc_retrieve(loadest_preds_ind, remake_file)) %>%
    select('Date','Flux','ref_date','Site','model_range')

  wrtds_preds_df <- readRDS(sc_retrieve(wrtds_preds_ind, remake_file)) %>%
    select('Date', 'Flux', 'ref_date', 'Site', 'model_range')

  # read in "truth"
  agg_nwis <- readRDS(sc_retrieve(agg_nwis_ind, remake_file))
  agg_nwis$flux <- left_join(agg_nwis$nitrate_sensor, agg_nwis$flow, by=c('site_no','date'), suffix=c('_conc','_flow')) %>%
    mutate(daily_mean_flux = daily_mean_conc * daily_mean_flow * 60*60*24/1000) %>% # flow in kg/d
    rename(site=site_no) %>%
    select('site', 'date', 'daily_mean_flux')

  # join predictions and obs together
  preds_obs <- left_join(loadest_preds_df, wrtds_preds_df, by = c('Date', 'ref_date', 'Site', 'model_range'), suffix = c('_loadest', '_wrtds')) %>%
    gather(key = 'flux_model', value = 'pred_flux', starts_with('Flux')) %>%
    mutate(LeadTime = as.numeric(Date - ref_date, unit = 'days')) %>%
    left_join(y = agg_nwis$flux, by = c('Site' = 'site', 'Date' = 'date')) %>%
    dplyr::filter(!is.na(daily_mean_flux), !is.na(pred_flux)) %>%
    rename(obs_flux = daily_mean_flux)


  # create the plot
  g <- ggplot(preds_obs, aes(x = obs_flux/1000, y = pred_flux/1000, color = flux_model)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    facet_wrap(~Site, scales='free', nrow = 3, ncol = 1,
               strip.position = 'right') +
    xlab('Observed Flux') +
    ylab('Predicted Flux') +
    theme(legend.title = element_blank()) +
    theme_classic()
  g


    scale_color_continuous('Lead Time (d)') +
    xlab('Date') +
    ylab(expression('Flux'~(Mg~'N-NO'[3]~d^{-1}))) +
    theme_classic()

  # save and post to Drive
  fig_file <- as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width=6, height=5)
  gd_put(remote_ind=fig_ind, local_source=fig_file, config_file=config_file)
}
