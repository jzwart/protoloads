fig_exceedance <- function(fig_ind, config_fig_yml, exceedance_df, exceed_cfg_yml, preds_ind, agg_nwis_ind, remake_file, config_file) {

  # read in figure scheme config
  fig_config <- yaml::yaml.load_file(config_fig_yml)

  site_labels <- fig_config$site_abbrev %>%
    bind_rows() %>%
    as.character()
  names(site_labels) <- names(fig_config$site_abbrev)

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

  # combined predictions and observations
  pred_obs <- preds_df %>%
    as_data_frame() %>%
    # add in manually-selected thresholds and observations
    left_join(mutate(exceed_thresh, FluxThreshold=1000*flux_threshold), by=c(Site='site')) %>%
    left_join(select(agg_nwis$flux, Site=site, Date=date, ObsFlux=daily_mean_flux), by=c('Date','Site'))

  # examples
  exceedance_site <- filter(exceedance_df, Site == '05465500')
  examples_specs <- list(
    true_positive = exceedance_site %>% filter(TruePositive, StartsBelow),
    false_positive = exceedance_site %>% filter(FalsePositive, PredNumExceeded==5, ObsNumExceeded==0, StartsBelow, EndsBelow),
    true_negative = exceedance_site %>% filter(TrueNegative, PredNumExceeded==0, StartsBelow, EndsBelow),
    false_negative = exceedance_site %>% filter(FalseNegative, StartDate==as.Date('2017-05-14'))
  ) %>% lapply(slice, 1)
  examples_data <- lapply(examples_specs, function(specs) {
    type <- specs %>%
      gather(TypeName, IsType, TruePositive, FalsePositive, TrueNegative, FalseNegative) %>%
      filter(IsType) %>%
      pull(TypeName)
    pred_obs %>%
      filter(
        Site == specs$Site,
        model_range == specs$model_range,
        ref_date==specs$ref_date,
        Date >= specs$StartDate,
        Date <= specs$EndDate) %>%
      mutate(ErrorType = type)
  }) %>% bind_rows() %>%
    mutate(
      ErrorType = ordered(
        gsub('False', 'False ',
             gsub('True', 'True ',
                  gsub('Positive', 'Exceedance',
                       gsub('Negative', 'Non-Exceedance',
                            ErrorType)))),
        levels=c('True Exceedance', 'False Exceedance', 'True Non-Exceedance', 'False Non-Exceedance')),
      Predicted=Flux,
      Observed=ObsFlux) %>%
    gather(CurveType, Flux, Predicted, Observed)

  # identify some friendly date breaks
  date_breaks <- examples_specs %>%
    bind_rows %>%
    transmute(Left=StartDate + as.difftime(2, units='days'), Right = EndDate - as.difftime(2, units='days')) %>%
    gather(Bound, Date, Left, Right) %>%
    pull(Date)
  g <- ggplot(examples_data, aes(x=Date)) +
    geom_hline(aes(yintercept=flux_threshold), linetype='dashed') +
    geom_line(aes(y=Flux/1000, group=CurveType, color=CurveType), size=1) +
    geom_point(aes(y=Flux/1000, group=CurveType, color=CurveType), size=2) +
    theme(legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.text = element_text(size = 15),
          strip.text = element_text(size = 15),
          axis.title = element_text(size = 15),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size = 12),
          legend.position = c(.15,.90),
          legend.key = element_blank(),
          strip.background = element_blank()) +
    theme(legend.position=c(0.1, 0.2), legend.background=element_blank()) +
    facet_grid(. ~ ErrorType, scales='free_x') +
    scale_x_date(breaks=date_breaks, date_labels="%m/%d/%y") +
    scale_color_manual('', values=c(Predicted=fig_config$model_type$forecast, Observed=fig_config$model_type$obs)) +
    ylab(expression(Nitrate~flux~(Mg~'N-NO'[3]^'-'~d^-1)))

  # save and post to Drive
  fig_file <- as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width=12, height=3.6)
  gd_put(remote_ind=fig_ind, local_source=fig_file, config_file=config_file)
}
