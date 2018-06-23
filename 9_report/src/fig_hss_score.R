fig_hss_score <- function(fig_ind, config_fig_yml, hss_df, config_file) {
  # read in figure scheme config
  fig_config <- yaml::yaml.load_file(config_fig_yml)

  site_labels <- fig_config$site_abbrev %>%
    bind_rows() %>%
    as.character()
  names(site_labels) <- names(fig_config$site_abbrev)

  # create the plot
  g <- ggplot(hss_df, aes(x = WindowLeadTime, y = HSS, fill = model_range)) +
    geom_point(position = 'jitter', size = 4, shape = 21, stroke = 1.2, alpha=0.7) +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    theme(legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text = element_text(size = 15),
          strip.text = element_text(size = 15),
          axis.title = element_text(size = 15),
          legend.text = element_text(size = 12),
          legend.background = element_blank(),
          legend.position = c(.1,.82),
          legend.key = element_blank(),
          strip.background = element_blank()) +
    scale_x_reverse(breaks = c(0,9,19,29))+
    annotate("segment", x=Inf, xend=Inf, y=-Inf, yend=Inf, size = 1.1)+
    scale_fill_manual(values = c('long1' = fig_config$forecast_range_dark$long1,
                                  'med' = fig_config$forecast_range_dark$med),
                       labels = c('Long Range', 'Medium Range')) +
    facet_wrap(~Site, scales='fixed', nrow = 1, ncol = 3, labeller = labeller(Site = site_labels),
               strip.position = 'top') +
    xlab(expression(Lead~Time~(days))) +
    ylab(expression(atop(Exceedance~Forecast~Skill,(Heidke~Skill~Score))))

  # save and post to Drive
  fig_file <- as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width=12, height=4.4)
  gd_put(remote_ind=fig_ind, local_source=fig_file, config_file=config_file)
}
