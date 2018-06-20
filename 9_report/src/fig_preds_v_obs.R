fig_preds_v_obs <- function(fig_ind, config_fig_yml, loadest_preds_ind, wrtds_preds_ind, agg_nwis_ind, remake_file, config_file){
  # read in figure scheme config
  fig_config <- yaml::yaml.load_file(config_fig_yml)

  site_labels <- fig_config$site_abbrev %>%
    bind_rows() %>%
    as.character()
  names(site_labels) <- names(fig_config$site_abbrev)

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

  LeadTimes = c(0) # number of leadtimes we want to plot

  # join predictions and obs together
  preds_obs <- left_join(loadest_preds_df, wrtds_preds_df, by = c('Date', 'ref_date', 'Site', 'model_range'), suffix = c('_loadest', '_wrtds')) %>%
    gather(key = 'flux_model', value = 'pred_flux', starts_with('Flux')) %>%
    mutate(LeadTime = as.numeric(Date - ref_date, unit = 'days')) %>%
    left_join(y = agg_nwis$flux, by = c('Site' = 'site', 'Date' = 'date')) %>%
    dplyr::filter(!is.na(daily_mean_flux), !is.na(pred_flux),
                  LeadTime %in% LeadTimes) %>%
    rename(obs_flux = daily_mean_flux) %>%
    dplyr::filter(flux_model == 'Flux_wrtds')   ### just sticking with wrtds for iemss poster; remove line if we want both models plotted on 1:1 ###

  rmse <- preds_obs %>%
    group_by(Site, flux_model) %>%
    summarise(rmse = sqrt(mean((obs_flux/1000 - pred_flux/1000)^2))) %>%
    ungroup()

  # creating dummy data frame for making same axis ranges for facet_wrap
  dummy <- preds_obs %>%
    group_by(Site) %>%
    summarise(x_min = min(min(obs_flux),min(pred_flux)), x_max = max(max(obs_flux),max(pred_flux)),
              y_min = min(min(obs_flux),min(pred_flux)), y_max = max(max(obs_flux),max(pred_flux))) %>%
    ungroup() %>%
    gather('x', 'x_val', starts_with('x')) %>%
    gather('y', 'y_val', starts_with('y')) %>%
    mutate(flux_model = 'Flux_loadest')

  # create the plot
  g <- ggplot(preds_obs, aes(x = obs_flux/1000, y = pred_flux/1000)) +
    geom_point(size = 2) +
    geom_blank(data = dummy, aes(x= x_val/1000, y = y_val/1000)) + # to make 1:1 axes
    theme(legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.text = element_text(size = 15),
          strip.text = element_text(size = 15),
          axis.title = element_text(size = 15),
          legend.text = element_text(size = 12),
          axis.line = element_line(colour = "black"),
          legend.position = c(.05,.90),
          legend.key = element_blank(),
          strip.background = element_blank(),
          plot.margin = unit(c(.5,1,.5,.5),units = 'cm')) +
    # scale_color_manual(values = c('Flux_loadest' = fig_config$load_model$loadest, # just plotting wrtds pred vs. obs now
    #                               'Flux_wrtds' = fig_config$load_model$wrtds),
    #                    labels = c('Loadest', 'WRTDS')) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
    facet_wrap(~Site, scales='free', nrow = 1, ncol = 3, labeller = labeller(Site = site_labels),
               strip.position = 'top') +
    xlab(expression(Observed~nitrate~flux~(Mg~N~day^-1))) +
    ylab(expression(Predicted~nitrate~flux~(Mg~N~day^-1))) +
    scale_x_log10() + scale_y_log10()

  # save and post to Drive
  fig_file <- as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width=12, height=5)
  gd_put(remote_ind=fig_ind, local_source=fig_file, config_file=config_file)
}
