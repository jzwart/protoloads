apply_wrtds <- function(output_rds, eList, log_rds) {

  sys_time <- system.time({
    # Fit the WRTDS model
    nCores <- parallel::detectCores()-1
    cl <- makePSOCKcluster(nCores)
    registerDoParallel(cl)
    fit_eList <- modelEstimation(eList, run.parallel=TRUE, verbose=FALSE)
    stopCluster(cl)

    # Extract the load forecasts
    preds <- fit_eList$Daily %>%
      select(Date, Flow=Q, Flux=FluxDay, Conc=ConcDay) %>%
      filter(Date >= eList$INFO$ref_date) %>%
      mutate(Site = eList$INFO$site_no)
  })
  message(sprintf("Fitted and forecast from WRTDS model in %0.2f seconds", sys_time[['elapsed']]))

  # Write the full model output as an augmented eList
  aList <- c(eList, list(preds=preds))
  saveRDS(aList, log_rds)

  # Write the forecasts to file
  saveRDS(preds, output_rds)
}
