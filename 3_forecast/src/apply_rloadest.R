apply_rloadest <- function(output_rds, input_ind) {

  # Read the inputs from a file
  eList <- readRDS(sc_retrieve(input_ind))

  # Define a lookup table for translating INFO$param.units to conc.units
  units_lookup <- c('mg/l as N'='mg/L')

  # Fit the 7-parameter model
  fit_data <- eList$Sample %>%
    select(Date, ConcLow, ConcHigh, Q)
  fit <- loadReg(
    survival::Surv(ConcLow, ConcHigh, type="interval2") ~ model(9),
    data=fit_data,
    flow="Q",
    dates="Date",
    conc.units=units_lookup[eList$INFO$param.units],
    flow.units='cms',
    station=eList$INFO$site.no)

  # Generate load forecasts
  est_data <- eList$Daily %>%
    select(Date, Q)
  preds <- predLoad(
    fit, newdata=est_data, by='day'
  ) %>%
    mutate(site = eList$INFO$site.no)

  # Write the forecasts to file
  saveRDS(preds, output_rds)
}
