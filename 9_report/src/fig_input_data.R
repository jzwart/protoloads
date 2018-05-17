fig_input_data <- function(fig_ind, preds_ind, remake_file, config_file){
  #discharge, concentration, loads

  site = '05465500'
  model = 'long1'
  ref_date = '2017-05-09'

  preds <- readRDS(sc_retrieve(preds_ind, remake_file)) %>%
    dplyr::filter(ref_date == '2017-05-09', # dplyr::filter doesn't recognize objects??
                  model_range == 'long1',
                  Site == '05465500')

  # example of input data for a forecast
  eList <- prep_inputs(nwis_site = site,
              nwm_model = model,
              ref_date = ref_date,
              site_info_ind = '1_data/out/site_info.tsv.ind',
              nwis_data_ind = '2_munge/out/agg_nwis.rds.ind',
              nwm_retro_ind = '2_munge/out/agg_nwm_retro.rds.ind',
              nwm_forecast_ind = '2_munge/out/agg_nwm_long1.rds.ind',
              remake_file = '3_forecast.yml')

  xlim = range(c(eList$Daily$Date, eList$Sample$Date, preds$Date))

  # flow
  g1 <- ggplotGrob(ggplot(data = eList$Daily, aes(x = Date, y = Q)) +
    geom_line() + theme_classic() + xlim(xlim) +
      theme(axis.title.x = element_blank()))

  #concentration
  g2 <- ggplotGrob(ggplot(data = eList$Sample, aes(x = Date, y = ConcAve)) +
    geom_point() + theme_classic()+ xlim(xlim) +
      theme(axis.title.x = element_blank()))

  #loads
  g3 <- ggplotGrob(ggplot(data = preds, aes(x = Date, y = Flux)) +
                     xlim(xlim) +
                     geom_line() + theme_classic())

  g <- rbind(g1, g2, g3, size='first')
  g$widths <- grid::unit.pmax(g1$widths, g2$widths, g3$widths)

  # grid.draw(g)

  fig_file <- as_data_file(fig_ind)
  png(fig_file); grid::grid.draw(g); dev.off()
  gd_put(remote_ind=fig_ind, local_source=fig_file, config_file=config_file)
}
