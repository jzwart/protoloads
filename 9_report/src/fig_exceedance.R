fig_exceedance <- function(fig_ind, config_fig_yml, exceed_cfg_yml, preds_ind, agg_nwis_ind, remake_file, config_file) {
  # read in figure scheme config
  fig_config <- yaml::yaml.load_file(config_fig_yml)

  # exceedance thresholds
  exceed_thresh <- yaml::yaml.load_file(exceed_cfg_yml) %>%
    bind_rows() %>%
    gather(key = 'site', value = 'flux_threshold')

  # predictions
  preds_df <- readRDS(sc_retrieve(preds_ind, remake_file)) %>%
    mutate(LeadTime = as.numeric(Date - ref_date, units='days'))

  # "truth"
  agg_nwis <- readRDS(sc_retrieve(agg_nwis_ind, remake_file))
  agg_nwis$flux <- left_join(agg_nwis$nitrate_sensor, agg_nwis$flow, by=c('site_no','date'), suffix=c('_conc','_flow')) %>%
    mutate(daily_mean_flux = daily_mean_conc * daily_mean_flow * 60*60*24/1000) %>% # flow in kg/d
    rename(site=site_no)

  # cumulative frequency
  prob_exceed <- left_join(agg_nwis$flux, preds_df,
                        by = c('site' = 'Site', 'date' = 'Date'),
                        suffix = c('_truth', '_pred')) %>%
    dplyr::filter(!is.na(Flux), !is.na(daily_mean_flux)) %>%
    mutate(flux_error = Flux - daily_mean_flux,
           std_flux_error = (Flux - daily_mean_flux)/daily_mean_flux) %>%
    group_by(site) %>%
    arrange(daily_mean_flux, .by_group = T) %>%
    mutate(freq = seq(1,n())/n()) %>%
    ungroup() %>%
    left_join(y = exceed_thresh, by ='site') %>%
    mutate(obs_exceeded = case_when(daily_mean_flux/1000 < flux_threshold ~ 'no',
                                    TRUE ~ 'yes'),
           pred_exceeded = case_when(Flux/1000 < flux_threshold ~ 'no',
                                     TRUE ~ 'yes')) %>%
    group_by(site, date) %>%
    mutate(prob_exceed = sum(pred_exceeded == 'yes')/n()) %>%
    ungroup()

    # probability of exceeding is counting how many times forecasts exceeded threshold / number of forecasts

  # create the plot
  # g <- ggplot(dplyr::filter(cumulative_freq, site == '07374000'), aes(x = daily_mean_flux/1000, y = freq, color = exceeded)) +
  #   geom_line(size = 1.2) +
  #   theme(legend.title = element_blank(),
  #         panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         panel.background = element_blank(),
  #         axis.line = element_line(colour = "black"),
  #         legend.position = c(.15,.90),
  #         legend.key = element_blank(),
  #         strip.background = element_blank()) +
  #   scale_color_manual(values = c('no' = 'black',
  #                                 'yes' = 'red'),
  #                      labels = c('Below','Exceeded')) +
  #   facet_wrap(~site, scales='free_x', nrow = 1, ncol = 3,
  #              strip.position = 'top') +
  #   xlab(expression(Observed~nitrate~flux~(Mg~N~day^-1))) +
  #   ylab(expression(Cumulative~Frequency)) +
  #   geom_vline(aes(xintercept=flux_threshold), color='red', linetype= 'dashed', dplyr::filter(exceed_thresh, site =='07374000')) # adding threshold
  #
  # g

  g <- ggplot(dplyr::filter(prob_exceed, site == '07374000'), aes(x = daily_mean_flux/1000, y = prob_exceed)) +
    geom_line(size = 1.2) +
    theme(legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = c(.15,.90),
          legend.key = element_blank(),
          strip.background = element_blank()) +
    # scale_color_manual(values = c('no' = 'black',
    #                               'yes' = 'red'),
    #                    labels = c('Below','Exceeded')) +
    facet_wrap(~site, scales='free_x', nrow = 1, ncol = 3,
               strip.position = 'top') +
    xlab(expression(Observed~nitrate~flux~(Mg~N~day^-1))) +
    ylab(expression(Probability~of~threshold~exceedance)) +
    geom_vline(aes(xintercept=flux_threshold), color='red', size = 1.3, linetype= 'dashed', dplyr::filter(exceed_thresh, site =='07374000')) # adding threshold

  # g
  #
  # g <- ggplot(prob_exceed, aes(x = daily_mean_flux/1000, y = prob_exceed)) +
  #   geom_line(size = 1.2) +
  #   theme(legend.title = element_blank(),
  #         panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         panel.background = element_blank(),
  #         axis.line = element_line(colour = "black"),
  #         legend.position = c(.15,.90),
  #         legend.key = element_blank(),
  #         strip.background = element_blank()) +
  #   # scale_color_manual(values = c('no' = 'black',
  #   #                               'yes' = 'red'),
  #   #                    labels = c('Below','Exceeded')) +
  #   facet_wrap(~site, scales='free_x', nrow = 1, ncol = 3,
  #              strip.position = 'top') +
  #   xlab(expression(Observed~nitrate~flux~(Mg~N~day^-1))) +
  #   ylab(expression(Cumulative~Frequency)) +
  #   geom_vline(aes(xintercept=flux_threshold), color='red', linetype= 'dashed', exceed_thresh) # adding threshold
  #
  # g


  # save and post to Drive
  fig_file <- as_data_file(fig_ind)
  png(fig_file, width = 7, height = 7, units = 'in',res = 600); grid::grid.draw(g); dev.off()
  gd_put(remote_ind=fig_ind, local_source=fig_file, config_file=config_file)
}
