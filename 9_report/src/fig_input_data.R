fig_input_data <- function(fig_ind, input_example_yml, preds_ind, remake_file, config_file){
  #discharge, concentration, loads

  input_ex <- yaml::yaml.load_file(input_example_yml)

  preds <- readRDS(sc_retrieve(preds_ind, remake_file)) %>%
    dplyr::filter(ref_date == !!(input_ex$ref_date),
                  model_range == !!(input_ex$model),
                  Site == !!(input_ex$site))

  # example of input data for a forecast
  eList <- prep_inputs(nwis_site = input_ex$site,
              nwm_model = input_ex$model,
              ref_date = input_ex$ref_date,
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
