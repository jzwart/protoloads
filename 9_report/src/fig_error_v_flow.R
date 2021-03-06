fig_error_v_flow <- function(fig_ind, config_fig_yml, preds_ind, agg_nwis_ind, remake_file, config_file) {
  # read in figure scheme config
  fig_config <- yaml::yaml.load_file(config_fig_yml)

  site_labels <- fig_config$site_abbrev %>%
    bind_rows() %>%
    as.character()
  names(site_labels) <- names(fig_config$site_abbrev)

  # predictions
  preds_df <- readRDS(sc_retrieve(preds_ind, remake_file)) %>%
    mutate(LeadTime = as.numeric(Date - ref_date, units='days'))

  # "truth"
  agg_nwis <- readRDS(sc_retrieve(agg_nwis_ind, remake_file))
  agg_nwis$flux <- left_join(agg_nwis$nitrate_sensor, agg_nwis$flow, by=c('site_no','date'), suffix=c('_conc','_flow')) %>%
    mutate(daily_mean_flux = daily_mean_conc * daily_mean_flow * 60*60*24/1000) %>% # flow in kg/d
    rename(site=site_no)

  LeadTimes <- c(0,9,19,29) # leadtimes that we want to plot

  # error
  error_df <- left_join(agg_nwis$flux, preds_df,
                        by = c('site' = 'Site', 'date' = 'Date'),
                        suffix = c('_truth', '_pred')) %>%
    dplyr::filter(!is.na(Flux), !is.na(daily_mean_flux)) %>%
    mutate(flux_error = Flux - daily_mean_flux,
           std_flux_error = (Flux - daily_mean_flux)/daily_mean_flux) %>%
    group_by(site) %>%
    mutate(flow_class = case_when( # adding low and high flow class for below and above median flow; could adjust to be higher /lower percentiles
      daily_mean_flux < quantile(daily_mean_flux, 0.25) ~ 'low',
      daily_mean_flux > quantile(daily_mean_flux, 0.75) ~ 'high',
      TRUE ~ 'med')) %>%
    ungroup() %>%
    dplyr::filter(LeadTime %in% LeadTimes,
                  !is.na(flow_class)) %>%
    mutate(flow_class = factor(flow_class, levels = c('high', 'med', 'low')))

  sites = unique(error_df$site)

  # prepare a color scale and theme to share
  shared_fill_scale <- scale_fill_manual(
    name='flow_class',
    values = setNames(
      colorRampPalette(c(fig_config$flow_class$high, fig_config$flow_class$low))(3),
      c('high','med','low')),
    labels = c(expression('High Flow '('>75'^'th')),
               expression('Medium Flow '('25'^'th'-'75'^'th')),
               expression('Low Flow '('<25'^'th'))))
  shared_theme <- theme_classic() +
    theme(axis.text = element_text(size = 15),
          strip.text = element_text(size = 15),
          axis.title = element_text(size = 15),
          plot.margin = unit(c(1,3,1,1),'lines'))

  # plot with standardized flux error
  g1 <- ggplotGrob(ggplot(error_df[error_df$site==sites[1],], aes(x=factor(LeadTime), y=std_flux_error, fill = flow_class)) +
                     geom_hline(yintercept = 0,
                                linetype = 'dashed') +
                     geom_boxplot(outlier.shape = NA,
                                  width = 0.7) +
                     shared_fill_scale +
                     ylim(boxplot.stats(error_df$std_flux_error[error_df$site==sites[1]])$stats[c(1,5)]) +
                     scale_x_discrete(limits = rev(levels(factor(error_df$LeadTime[error_df$site==sites[1]])))) +
                     shared_theme +
                     theme(axis.title.x = element_blank(),
                           axis.title.y = element_blank(),
                           legend.position = 'none') +
                     annotation_custom(grob = textGrob(label = site_labels[sites[1]], hjust = 0, rot = 270),
                                       ymin = diff(boxplot.stats(error_df$std_flux_error[error_df$site==sites[1]])$stats[c(1,5)])/2,
                                       ymax = diff(boxplot.stats(error_df$std_flux_error[error_df$site==sites[1]])$stats[c(1,5)])/2,
                                       xmin = 4.75,
                                       xmax = 4.75))

  g2 <- ggplotGrob(ggplot(error_df[error_df$site==sites[2],], aes(x=factor(LeadTime), y=std_flux_error, fill = flow_class)) +
                     geom_hline(yintercept = 0,
                                linetype = 'dashed') +
                     geom_boxplot(outlier.shape = NA,
                                  width = 0.7) +
                     shared_fill_scale +
                     ylim(boxplot.stats(error_df$std_flux_error[error_df$site==sites[2]])$stats[c(1,5)]) +
                     scale_x_discrete(limits = rev(levels(factor(error_df$LeadTime[error_df$site==sites[2]])))) +
                     shared_theme +
                     theme(axis.title.x = element_blank(),
                           legend.position = c(.9,.8),
                           legend.title = element_blank(),
                           legend.text = element_text(size = 12),
                           legend.text.align = 0) +
                     ylab(expression('Relative flux error'~(('predict - obs')~'/'~'obs')))+
                     annotation_custom(grob = textGrob(label = site_labels[sites[2]], hjust = 0, rot = 270),
                                       ymin = diff(boxplot.stats(error_df$std_flux_error[error_df$site==sites[2]])$stats[c(1,5)])*0.9,
                                       ymax = diff(boxplot.stats(error_df$std_flux_error[error_df$site==sites[2]])$stats[c(1,5)])*0.9,
                                       xmin = 4.75,
                                       xmax = 4.75))

  g3 <- ggplotGrob(ggplot(error_df[error_df$site==sites[3],], aes(x=factor(LeadTime), y=std_flux_error, fill = flow_class)) +
                     geom_hline(yintercept = 0,
                                linetype = 'dashed') +
                     geom_boxplot(outlier.shape = NA,
                                  width = 0.7) +
                     shared_fill_scale +
                     ylim(boxplot.stats(error_df$std_flux_error[error_df$site==sites[3]])$stats[c(1,5)]) +
                     scale_x_discrete(limits = rev(levels(factor(error_df$LeadTime[error_df$site==sites[3]])))) +
                     xlab('Lead Time (days)') +
                     shared_theme +
                     theme(legend.position = 'none',
                           axis.title.y = element_blank())+
                     annotation_custom(grob = textGrob(label = site_labels[sites[3]], hjust = 0, rot = 270),
                                       ymin = diff(boxplot.stats(error_df$std_flux_error[error_df$site==sites[3]])$stats[c(1,5)])*0.7,
                                       ymax = diff(boxplot.stats(error_df$std_flux_error[error_df$site==sites[3]])$stats[c(1,5)])*0.7,
                                       xmin = 4.75,
                                       xmax = 4.75))

  g <- rbind(g1, g2, g3, size='first')
  g$widths <- grid::unit.pmax(g1$widths, g2$widths, g3$widths)
  g$layout$clip[g$layout$name=='panel'] <- 'off' # so site labels don't get cut off

  # save and post to Drive
  fig_file <- as_data_file(fig_ind)
  png(fig_file, width = 12, height = 6, units = 'in',res = 300); grid::grid.draw(g); dev.off()
  gd_put(remote_ind=fig_ind, local_source=fig_file, config_file=config_file)
}
