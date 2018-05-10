fig_preds_v_time <- function(fig_ind, preds_ind, remake_file, config_file) {

  # read in the predictions
  preds_df <- readRDS(sc_retrieve(preds_ind, remake_file))

  # create the plot
  g <- ggplot(preds_loadest, aes(x=Date, y=Flux, color=ref_date)) +
    geom_point() +
    facet_wrap(~site, scale='free_y', nrow=3)

  # save and post to Drive
  fig_file <- as_data_file(fig_ind)
  ggsave(fig_file, plot=g)
  gd_put(remote_ind=fig_ind, local_source=fig_file, config_file=config_file)
}
