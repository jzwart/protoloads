fig_input_data <- function(fig_ind, config_fig_yml, input_example_yml, nwis_data_ind, preds_ind, remake_file, config_file){
  #discharge, concentration, loads
  # read in figure scheme config
  fig_config <- yaml::yaml.load_file(config_fig_yml)

  input_ex <- yaml::yaml.load_file(input_example_yml)

  # read in data and forecasts
  nwis_data <- readRDS(sc_retrieve(nwis_data_ind, remake_file)) %>%
    lapply(function(df) dplyr::filter(df, site_no == input_ex$site))

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

  # separate/compile into separates dfs for flow, concentration, and flux
  flow <- bind_rows(
    nwis_data$flow %>%
      filter(date >= min(eList$Daily$Date), date < min(preds$Date)) %>%
      mutate(Flow = daily_mean) %>%
      select(Date = date, Flow) %>%
      mutate(Source = 'Observed'),
    eList$Daily %>%
      mutate(Source = case_when(
        Date < (min(preds$Date) - as.difftime(182, units='days')) ~ 'Retrospective',
        Date >= min(preds$Date) ~ 'Forecast',
        TRUE ~ 'Analysis'
      )) %>%
      select(Date, Flow = Q, Source)
  ) %>%
    mutate(Source = ordered(Source, levels=c('Observed','Retrospective','Analysis','Forecast')))

  fluxconc <- bind_rows(
    # observations
    eList$Sample %>%
      select(Date, Conc = ConcAve, Uncen) %>%
      left_join(select(filter(flow, Source == 'Observed'), -Source), by='Date') %>%
      mutate(
        Source = ifelse(Uncen == 1, 'Observed', 'Observed (Censored)'),
        Flux = Flow * Conc * 60*60*24/1000/1000) %>% # flux in Mg N day-1
      arrange(Date),

    # forecasts
    preds %>%
      select(Date, Conc, Flux) %>%
      mutate(
        Source = 'Forecast',
        Flux = Flux / 1000, Uncen = 2) #convert flux to Mg N day-1
  ) %>%
    mutate(Source = ordered(Source, levels=c('Observed','Observed (Censored)','Forecast')))


  # define shared plotting parameters
  xlim <- range(c(flow$Date, fluxconc$Date))
  shared_theme <- theme_classic() +
    theme(
      axis.text = element_text(size = 12),
      strip.text = element_text(size = 15),
      axis.title = element_text(size = 12),
      legend.text = element_text(size =10),
      legend.background = element_blank(),
      legend.title = element_blank())

  # plot flow
  g1 <- ggplotGrob(
    ggplot(data = flow, aes(x = Date, y = Flow)) +
      geom_point(data = flow, size = 1, aes(color = Source, shape = Source)) +
      geom_line(data = flow, size = 1, aes(color = Source, linetype = Source)) +
      scale_color_manual(
        name = 'Source',
        values = c('Observed' = fig_config$model_type$obs,
                   'Retrospective' = fig_config$model_type$retro,
                   'Analysis' = fig_config$model_type$analysis,
                   'Forecast' = fig_config$model_type$forecast)) +
      scale_shape_manual(
        name = 'Source',
        values = c('Observed' = 19,
                   'Retrospective' = NA,
                   'Analysis' = NA,
                   'Forecast' = NA)) +
      scale_linetype_manual(
        name = 'Source',
        values = c('Observed' = 'blank',
                   'Retrospective' = 'solid',
                   'Analysis' = 'solid',
                   'Forecast' = 'solid')) +
      xlim(xlim) +
      labs(y = expression(atop('Discharge',(m^3~s^-1)))) +
      shared_theme +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            legend.position = c(.19,.7)) +
      geom_vline(xintercept = as.Date(input_ex$ref_date),
                 linetype = 'dashed'))

  #concentration
  g2 <- ggplotGrob(
    ggplot(data = fluxconc, aes(x = Date, y = Conc)) +
      geom_point(size = 1, aes(color = Source, shape = Source)) +
      geom_line(size = 1, aes(color = Source, linetype = Source)) +
      scale_color_manual(
        name = 'Source',
        values = c('Observed' = fig_config$model_type$obs,
                   'Observed (Censored)' = fig_config$model_type$cens,
                   'Forecast' = fig_config$model_type$forecast)) +
      scale_shape_manual(
        name = 'Source',
        values = c('Observed' = 19,
                   'Observed (Censored)' = 4,
                   'Forecast' = NA)) +
      scale_linetype_manual(
        name = 'Source',
        values = c('Observed' = 'blank',
                   'Observed (Censored)' = 'blank',
                   'Forecast' = 'solid')) +
      xlim(xlim) +
      labs(y = expression(atop('Nitrate concentration',(mg~'N-NO'[3]^'-'~L^-1)))) +
      shared_theme +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            legend.position = c(.18,.8)) +
      geom_vline(xintercept = as.Date(input_ex$ref_date), linetype = 'dashed'))

  #loads
  g3 <- ggplotGrob(
    ggplot(data = fluxconc, aes(x = Date, y = Flux)) +
      xlim(xlim) +
      geom_point(size = 1, aes(colour = Source, shape = Source)) +
      geom_line(size = 1, aes(colour = Source, linetype = Source)) +
      scale_color_manual(
        values = c('Observed' = fig_config$model_type$obs,
                   'Forecast' = fig_config$model_type$forecast)) +
      scale_shape_manual(
        name = 'Source',
        values = c('Observed' = 19,
                   'Forecast' = NA)) +
      scale_linetype_manual(
        name = 'Source',
        values = c('Observed' = 'blank',
                   'Forecast' = 'solid')) +
      labs(y = expression(atop('Nitrate flux',(Mg~'N-NO'[3]^'-'~d^-1)))) +
      shared_theme +
      theme(legend.position = c(.18,.8)) +
      geom_vline(xintercept = as.Date(input_ex$ref_date), linetype = 'dashed'))

  g <- rbind(g1, g2, g3, size='first')
  g$widths <- grid::unit.pmax(g1$widths, g2$widths, g3$widths)

  grid.draw(g)

  fig_file <- as_data_file(fig_ind)
  png(fig_file, width = 10, height = 5, units = 'in',res = 300); grid::grid.draw(g); dev.off()
  gd_put(remote_ind=fig_ind, local_source=fig_file, config_file=config_file)
}
