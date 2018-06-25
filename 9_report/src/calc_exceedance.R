calc_exceedance <- function(exceed_cfg_yml, preds_ind, agg_nwis_ind, remake_file) {
  # exceedance thresholds
  exceed_thresh <- yaml::yaml.load_file(exceed_cfg_yml) %>%
    bind_rows() %>%
    gather(key = 'site', value = 'flux_threshold')

  # predictions
  preds_df <- readRDS(sc_retrieve(preds_ind, remake_file)) %>%
    mutate(LeadTime = as.numeric(Date - ref_date, units='days'))

  # "truth"
  agg_nwis <- readRDS(sc_retrieve(agg_nwis_ind, remake_file))
  agg_nwis$flux <- left_join(agg_nwis$nitrate_sensor, agg_nwis$flow, by=c('site_no','date'), suffix=c('_conc','_flow')) %>%
    mutate(daily_mean_flux = daily_mean_conc * daily_mean_flow * 60*60*24/1000) %>% # flow in kg/d
    rename(site=site_no)

  # apply exceedance criterion
  window_length <- 10
  system.time({
    exceedance_df <- preds_df %>%
      as_data_frame() %>%

      # add in manually-selected thresholds and observations
      left_join(mutate(exceed_thresh, FluxThreshold=1000*flux_threshold), by=c(Site='site')) %>%
      left_join(select(agg_nwis$flux, Site=site, Date=date, ObsFlux=daily_mean_flux), by=c('Date','Site')) %>%

      # cut out any incomplete dates
      filter(!is.na(Flux), !is.na(ObsFlux)) %>%

      # identify >=3 day exceedances within 10-day windows
      group_by(Site, model_range, ref_date) %>%
      do({
        all_preds <- .
        bind_rows(lapply(seq(0, max(LeadTime) - window_length + 1), function(lead_time) {
          all_preds %>%
            slice(which(LeadTime %in% seq(lead_time, lead_time + window_length - 1))) %>% # get a 10-day window
            summarize(
              StartDate = min(Date),
              EndDate = max(Date),
              WindowLeadTime = lead_time,
              CompleteWindow = n() == window_length,
              StartsBelow = Flux[1] < FluxThreshold && ObsFlux[1] < FluxThreshold,
              EndsBelow = Flux[window_length] < FluxThreshold && ObsFlux[window_length] < FluxThreshold,
              PredNumExceeded = length(which(Flux > FluxThreshold)),
              PredExceeded = PredNumExceeded >= 3,
              ObsNumExceeded = length(which(ObsFlux > FluxThreshold)),
              ObsExceeded = ObsNumExceeded >= 3
            )
        }))
      }) %>%
      ungroup() %>%

      # cut out any incomplete date windows
      filter(CompleteWindow) %>%

      # count matches/mismatches between pred & obs exceedances
      mutate(
        TruePositive = PredExceeded & ObsExceeded,
        FalsePositive = PredExceeded & !ObsExceeded,
        TrueNegative = !PredExceeded & !ObsExceeded,
        FalseNegative = !PredExceeded & ObsExceeded
      )

  })

  return(exceedance_df)
}

# Heidke Skill Score http://www.eumetrain.org/data/4/451/english/msg/ver_categ_forec/uos3/uos3_ko1.htm

# The Heidke Skill score is in the usual skill score format,

# Skill = (score value – score for the standard forecast) / (perfect score – score for the standard forecast)

# For the HSS, the "score" is the number correct or the proportion correct. The
# "standard forecast" is usually the number correct by chance or the proportion
# correct by chance. Thus using the proportion correct,

# HSS = {(a+d)/n – [(a+b)(a+c)+(b+d)(c+d)]/n 2 }/{1 – [(a+b)(a+c) +(b+d)(c+d)]/n 2 }
# which is simplified to:
# HSS = 2(ad-bc)/[(a+c)(c+d) + (a+b)(b+d)]
# where a = yes_exceeded_obs + yes_exceeded_pred, b = no_exceeded_obs +
# yes_exceeded_pred, c = yes_exceeded_obs + no_exceeded_pred, d =
# no_exceeded_obs + no_exceeded_pred, n = a+b+c+d or count

# The HSS measures the fractional improvement of the forecast over the standard
# forecast. Like most skill scores, it is normalized by the total range of
# possible improvement over the standard, which means Heidke Skill scores can
# safely be compared on different datasets. The range of the HSS is -∞ to 1.
# Negative values indicate that the chance forecast is better, 0 means no skill,
# and a perfect forecast obtains a HSS of 1.

calc_exceedance_HSS <- function(exceedance_df, remake_file) {

  # add WindowType column
  exceed_df <- exceedance_df %>%
    mutate(WindowType = sprintf('%s_%d', model_range, WindowLeadTime))

  max_window_types <- exceed_df %>%
    group_by(Site, StartDate) %>%
    summarize(NumWindows = length(unique(WindowType))) %>%
    pull(NumWindows) %>%
    max()

  hss_df <- exceed_df %>%
    # get rid of windows for which not all possible WindowTypes are available
    group_by(Site, StartDate) %>%
    filter(length(unique(WindowType)) == max_window_types) %>%
    ungroup() %>%

    # simplify to one contingency table (4 numbers) per site-window combo
    group_by(Site, WindowType, model_range, WindowLeadTime) %>%
    summarize(
      NumWindows = n(),
      NumExceedances = sum(ObsExceeded),
      TruePositives = sum(TruePositive),
      FalsePositives = sum(FalsePositive),
      TrueNegatives = sum(TrueNegative),
      FalseNegatives = sum(FalseNegative)
    ) %>%
    ungroup() %>%
    arrange(Site, WindowLeadTime, desc(model_range)) %>%

    # compute the Heidke Skill Score (HSS) for each WindowType
    mutate(
      a = TruePositives,
      b = FalsePositives,
      c = FalseNegatives,
      d = TrueNegatives,
      HSS = 2 * (a*d - b*c) / ( (a + c)*(c + d) + (a + b)*(b + d) )
    ) %>%
    select(-a, -b, -c, -d)

  return(hss_df)
}
