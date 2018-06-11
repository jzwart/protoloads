fig_error_v_leadtime <- function(fig_ind, config_fig_yml, preds_ind, agg_nwis_ind, remake_file, config_file) {
  # read in figure scheme config
  fig_config <- yaml::yaml.load_file(config_fig_yml)

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
    mutate(flux_error = Flux - daily_mean_flux,
           std_flux_error = (Flux - daily_mean_flux)/daily_mean_flux) %>%
    group_by(site) %>%
    mutate(z_score_flux_error = (flux_error-mean(flux_error))/sd(flux_error)) %>% # z_score for error comparison between sites
    ungroup()

  sites = unique(error_df$site)

  # standardized flux error
  g1 <- ggplotGrob(ggplot(error_df[error_df$site==sites[1],], aes(x=factor(LeadTime), y=std_flux_error, fill = model_range)) +
                     geom_hline(yintercept = 0,
                                linetype = 'dashed') +
                     geom_boxplot(outlier.shape = NA,
                                  width = 0.7) +
                     scale_fill_manual(name = 'model_range',
                       values = c('long1' = fig_config$forecast_range$long1,
                                  'med' = fig_config$forecast_range$med),
                       labels = c('Long Range','Medium Range')) +
                     theme_classic()+
                     ylim(boxplot.stats(error_df$std_flux_error[error_df$site==sites[1]])$stats[c(1,5)]) +
                     scale_x_discrete(limits = rev(levels(factor(error_df$LeadTime[error_df$site==sites[1]])))) +
                     theme(axis.title.x = element_blank(),
                           axis.title.y = element_blank(),
                           legend.position = c(.2,.8),
                           legend.title = element_blank(),
                           plot.margin = unit(c(1,3,1,1),'lines'))+
                     annotation_custom(grob = textGrob(label = sites[1], hjust = 0, rot = 270),
                                       ymin = diff(boxplot.stats(abs(error_df$std_flux_error[error_df$site==sites[1]]))$stats[c(1,5)])/2,
                                       ymax = diff(boxplot.stats(abs(error_df$std_flux_error[error_df$site==sites[1]]))$stats[c(1,5)])/2,
                                       xmin = 32,
                                       xmax = 32))

  g2 <- ggplotGrob(ggplot(error_df[error_df$site==sites[2],], aes(x=factor(LeadTime), y=std_flux_error, fill = model_range)) +
                     geom_hline(yintercept = 0,
                                linetype = 'dashed') +
                     geom_boxplot(outlier.shape = NA,
                                  width = 0.7) +
                     scale_fill_manual(name = 'model_range',
                                       values = c('long1' = fig_config$forecast_range$long1,
                                                  'med' = fig_config$forecast_range$med),
                                       labels = c('Long Range','Medium Range')) +
                     theme_classic()+
                     ylim(boxplot.stats(error_df$std_flux_error[error_df$site==sites[2]])$stats[c(1,5)]) +
                     scale_x_discrete(limits = rev(levels(factor(error_df$LeadTime[error_df$site==sites[2]])))) +
                     theme(axis.title.x = element_blank(),
                           legend.position = 'none',
                           plot.margin = unit(c(1,3,1,1),'lines')) +
                     ylab(expression('Relative flux error'~(('predict - obs')~'/'~'obs')))+
                     annotation_custom(grob = textGrob(label = sites[2], hjust = 0, rot = 270),
                                       ymin = diff(boxplot.stats(abs(error_df$std_flux_error[error_df$site==sites[2]]))$stats[c(1,5)])/2,
                                       ymax = diff(boxplot.stats(abs(error_df$std_flux_error[error_df$site==sites[2]]))$stats[c(1,5)])/2,
                                       xmin = 32,
                                       xmax = 32))

  g3 <- ggplotGrob(ggplot(error_df[error_df$site==sites[3],], aes(x=factor(LeadTime), y=std_flux_error, fill = model_range)) +
                     geom_hline(yintercept = 0,
                                linetype = 'dashed') +
                     geom_boxplot(outlier.shape = NA,
                                  width = 0.7) +
                     scale_fill_manual(name = 'model_range',
                                       values = c('long1' = fig_config$forecast_range$long1,
                                                  'med' = fig_config$forecast_range$med),
                                       labels = c('Long Range','Medium Range')) +
                     theme_classic()+
                     ylim(boxplot.stats(error_df$std_flux_error[error_df$site==sites[3]])$stats[c(1,5)]) +
                     scale_x_discrete(limits = rev(levels(factor(error_df$LeadTime[error_df$site==sites[3]])))) +
                     xlab('Lead Time (days)') +
                     theme(legend.position = 'none',
                           axis.title.y = element_blank(),
                           plot.margin = unit(c(1,3,1,1),'lines'))+
                     annotation_custom(grob = textGrob(label = sites[3], hjust = 0, rot = 270),
                                       ymin = diff(boxplot.stats(abs(error_df$std_flux_error[error_df$site==sites[3]]))$stats[c(1,5)])/2,
                                       ymax = diff(boxplot.stats(abs(error_df$std_flux_error[error_df$site==sites[3]]))$stats[c(1,5)])/2,
                                       xmin = 32,
                                       xmax = 32))

  g <- rbind(g1, g2, g3, size='first')
  g$widths <- grid::unit.pmax(g1$widths, g2$widths, g3$widths)
  g$layout$clip[g$layout$name=='panel'] <- 'off' # so site labels don't get cut off

  # windows()
  # grid.draw(g)

  # save and post to Drive
  fig_file <- as_data_file(fig_ind)
  png(fig_file); grid::grid.draw(g); dev.off()
  gd_put(remote_ind=fig_ind, local_source=fig_file, config_file=config_file)
}
