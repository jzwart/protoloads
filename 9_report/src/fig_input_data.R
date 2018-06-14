fig_input_data <- function(fig_ind, config_fig_yml, input_example_yml, preds_ind, remake_file, config_file){
  #discharge, concentration, loads
  # read in figure scheme config
  fig_config <- yaml::yaml.load_file(config_fig_yml)

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
              nwm_ana_ind = '2_munge/out/agg_nwm_ana.rds.ind',
              nwm_retro_ind = '2_munge/out/agg_nwm_retro.rds.ind',
              nwm_forecast_ind = '2_munge/out/agg_nwm_long1.rds.ind',
              remake_file = '3_forecast.yml')

  flow_retro <- eList$Daily %>%
    dplyr::filter(!Date %in% eList$Sample$Date, !Date %in% preds$Date) %>%
    select(Date, Q) %>%
    mutate(Conc = NA, Flux = NA, data = 'nwis')

  flux_nwis <- eList$Sample %>%
    select(Date, Q, ConcAve, Uncen) %>%
    mutate(Flux = Q * ConcAve * 60*60*24/1000/1000, data = 'nwis') %>% # flux in Mg N day-1
    rename(Conc = ConcAve) %>%
    bind_rows(flow_retro, .) %>%
    arrange(Date)

  preds <- preds %>%
    select(Date, Flow, Conc, Flux) %>%
    rename(Q = Flow) %>%
    mutate(data = 'forecast', Flux = Flux / 1000, Uncen = 2) %>% #convert flux to Mg N day-1
    bind_rows(flux_nwis, .)

  xlim = range(c(eList$Daily$Date, eList$Sample$Date, preds$Date))

  # flow
  g1 <- ggplotGrob(ggplot(data = preds, aes(x = Date, y = Q)) +
                     geom_line(size = 1,
                               aes(color = factor(data))) +
                     scale_color_manual(name = 'data',
                                        values = c('nwis' = fig_config$model_type$retro,
                                                   'forecast' = fig_config$model_type$forecast),
                                        labels = c('Forecast','Retrospective')) +
                     theme_classic() +
                     xlim(xlim) +
                     labs(y = expression(Discharge~(m^3~s^-1))) +
                     theme(axis.title.x = element_blank(),
                           legend.position = c(.1,.9),
                           axis.text = element_text(size = 15),
                           axis.text.x = element_blank(),
                           strip.text = element_text(size = 15),
                           axis.title = element_text(size = 12),
                           legend.text = element_text(size = 10),
                           legend.title = element_blank()) +
                     geom_vline(xintercept = as.Date(input_ex$ref_date),
                                linetype = 'dashed'))

  #concentration
  g2 <- ggplotGrob(ggplot(data = preds[!is.na(preds$Uncen),], aes(x = Date, y = Conc)) +
                     geom_point(size = 1,
                                aes(colour = factor(Uncen))) +
                     scale_color_manual(name = 'Uncen',
                                        values = c('1' = fig_config$model_type$retro,
                                                   '0' = 'grey',
                                                   '2' = fig_config$model_type$forecast),
                                        labels = c('Censored', 'Retrospective', 'Forecast')) +
                     theme_classic()+
                     xlim(xlim) +
                     labs(y = expression(Nitrate~concentration~(mg~N~L^-1))) +
                     theme(axis.title.x = element_blank(),
                           axis.text = element_text(size = 15),
                           axis.text.x = element_blank(),
                           strip.text = element_text(size = 15),
                           axis.title = element_text(size = 12),
                           legend.text = element_text(size =10),
                           legend.position = c(.1,.85), legend.title = element_blank()) +
                     geom_vline(xintercept = as.Date(input_ex$ref_date), linetype = 'dashed'))

  #loads
  g3 <- ggplotGrob(ggplot(data = preds[preds$Uncen != 0 & !is.na(preds$Uncen),], aes(x = Date, y = Flux)) +
                     xlim(xlim) +
                     geom_point(size = 1,
                               aes(colour = factor(data))) +
                     scale_color_manual(name = 'data',
                                        values = c('nwis' = fig_config$model_type$retro,
                                                   'forecast' = fig_config$model_type$forecast),
                                        labels = c('Forecast', 'Retrospective')) +
                     theme_classic() +
                     labs(y = expression(Nitrate~flux~(Mg~N~day^-1))) +
                     theme(legend.position = c(.1,.9),
                           axis.text = element_text(size = 15),
                           strip.text = element_text(size = 15),
                           axis.title = element_text(size = 12),
                           legend.text = element_text(size =10),
                           legend.title = element_blank()) +
                     geom_vline(xintercept = as.Date(input_ex$ref_date), linetype = 'dashed'))

  g <- rbind(g1, g2, g3, size='first')
  g$widths <- grid::unit.pmax(g1$widths, g2$widths, g3$widths)

  # grid.draw(g)

  fig_file <- as_data_file(fig_ind)
  png(fig_file, width = 10, height = 7, units = 'in',res = 300); grid::grid.draw(g); dev.off()
  gd_put(remote_ind=fig_ind, local_source=fig_file, config_file=config_file)
}
