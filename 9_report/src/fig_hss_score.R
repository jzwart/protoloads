fig_hss_score <- function(fig_ind, config_fig_yml, exceed_cfg_yml, preds_ind, agg_nwis_ind, remake_file, config_file) {
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

  # Heidke Skill Score http://www.eumetrain.org/data/4/451/english/msg/ver_categ_forec/uos3/uos3_ko1.htm

  # The Heidke Skill score is in the usual skill score format,
  #
  # Skill = (score value – score for the standard forecast) / (perfect score – score for the standard forecast)
  #
  # For the HSS, the "score" is the number correct or the proportion correct. The "standard forecast" is usually the number correct by chance or the proportion correct by chance. Thus using the proportion correct,
  #
  # HSS = {(a+d)/n – [(a+b)(a+c)+(b+d)(c+d)]/n 2 }/{1 – [(a+b)(a+c) +(b+d)(c+d)]/n 2 }
  # which is simplified to:
  # HSS = 2(ad-bc)/[(a+c)(c+d) + (a+b)(b+d)]
  # The HSS measures the fractional improvement of the forecast over the standard forecast. Like most skill scores, it is normalized by the total range of possible improvement over the standard, which means Heidke Skill scores can safely be compared on different datasets. The range of the HSS is -∞ to 1. Negative values indicate that the chance forecast is better, 0 means no skill, and a perfect forecast obtains a HSS of 1.
  #  where a = yes_exceeded_obs + yes_exceeded_pred, b = no_exceeded_obs + yes_exceeded_pred, c = yes_exceeded_obs + no_exceeded_pred, d = no_exceeded_obs + no_exceeded_pred, n = a+b+c+d or count

  hss = prob_exceed %>%
    mutate(hss_code = case_when(obs_exceeded == 'yes' & pred_exceeded == 'yes' ~ 'a',
                                obs_exceeded == 'no' & pred_exceeded == 'yes' ~ 'b',
                                obs_exceeded == 'yes' & pred_exceeded == 'no' ~ 'c',
                                obs_exceeded == 'no' & pred_exceeded == 'no' ~ 'd')) %>%
    group_by(site, LeadTime, model_range) %>%
    summarise(hss = 2*(sum(hss_code=='a')*sum(hss_code=='d')-sum(hss_code=='b')*sum(hss_code=='c'))/
                ((sum(hss_code=='a')+sum(hss_code=='c'))*(sum(hss_code=='c')+sum(hss_code=='d'))+
                   (sum(hss_code=='a')+sum(hss_code=='b'))*(sum(hss_code=='b')+sum(hss_code=='d')))) %>%
    ungroup()

  # create the plot
  g <- ggplot(hss, aes(x = LeadTime, y = hss, fill = model_range)) +
    geom_point(position = 'jitter', size =4, shape = 21, stroke = 1.5) +
    geom_hline(yintercept = 0,
               linetype = 'dashed') +
    theme(legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text = element_text(size = 15),
          strip.text = element_text(size = 15),
          axis.title = element_text(size = 15),
          legend.text = element_text(size = 12),
          legend.position = c(.1,.85),
          legend.key = element_blank(),
          strip.background = element_blank()) +
    scale_x_reverse(breaks = c(0,9,19,29))+
    scale_fill_manual(values = c('long1' = fig_config$forecast_range$long1,
                                  'med' = fig_config$forecast_range$med),
                       labels = c('Long Range', 'Medium Range')) +
    facet_wrap(~site, scales='fixed', nrow = 1, ncol = 3,
               strip.position = 'top') +
    xlab(expression(Lead~Time~(days))) +
    ylab(expression(Forecasting~Skill~(Heidke~Skill~Score)))

  # save and post to Drive
  fig_file <- as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width=14, height=5)
  gd_put(remote_ind=fig_ind, local_source=fig_file, config_file=config_file)
}
