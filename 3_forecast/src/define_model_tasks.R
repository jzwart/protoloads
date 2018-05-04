# list tasks
# (2 model types per reference date)*(3 sites)*(30 valid dates)*(10sec/model)/(60sec*60min) = 40 minutes
list_tasks <- function(sites_yml='1_data/out/sites_subset.yml.ind') {
  sites <- yaml::yaml.load_file(sc_retrieve(sites_yml, remake_file='2_munge.yml'))
  valid_dates <- as.Date('2017-05-04') %>%
    {seq(
      .,
      .+as.difftime(11,units='days'),
      by=as.difftime(1,units='days'))}
  tasks <-
    tidyr::crossing(
      model_range=c('med', 'long1'), # 'long2', 'long3', 'long4'
      valid_date=valid_dates
    ) %>%
    group_by(model_range, valid_date) %>%
    dplyr::do(with(., {
      forecast_range <- ifelse(model_range=='med', 10, 30)
      first_ref <- valid_date - as.difftime(forecast_range-1, units='days')
      ref_seq <- seq(first_ref, valid_date, by=as.difftime(1, units='days'))
      data_frame(
        model_range,
        valid_date,
        ref_date=ref_seq
        #last_valid_date=ref_date+as.difftime(forecast_range-1, units='days'))
      )
    })) %>%
    ungroup() %>%
    select(-valid_date) %>%
    distinct() %>%
    group_by(model_range, ref_date) %>%
    do(with(., {data_frame(model_range, ref_date, site=sites)})) %>%
    mutate(ref_datestr=format(ref_date, '%Y%m%d')) %>%
    tidyr::unite(
      task_name, site, model_range, ref_datestr,
      sep='_', remove=FALSE) %>%
    select(-ref_datestr)

  return(tasks)
}


# prepare a plan for downloading (from WQP) and posting (to GD) one data file
# per state
plan_forecasts <- function(partitions, folders) {

  psprintf <- function(..., sep='\n      ') {
    args <- list(...)
    strs <- mapply(function(string, variables) {
      spargs <- if(string == '') list(variables) else c(list(string), list(variables))
      do.call(sprintf, spargs)
    }, string=names(args), variables=args)
    paste(strs, collapse=sep)
  }

  # steps: partition, forecast, post, retrieve
  partition <- scipiper::create_task_step(
    step_name = 'partition',
    target_name = function(task_name, step_name, ...) {
      sprintf('partition_%s', task_name)
    },
    command = function(task_name, ...) {
      psprintf(
        "subset_inputs(",
        "all_inputs=I('%s'),"=,
        "nwm_model=I('%s'),",
        "start_calibrate=I('%s'),",
        "start_forecast=I('%s'))", task_name,
        sep="\n      ")
    }
  )

  forecast_rloadest <- scipiper::create_task_step(
    step_name = 'forecast_rloadest',
    target_name = function(task_name, step_name, ...) {
      scipiper::as_ind_file(file.path(folders$tmp, sprintf('%s.feather', task_name)))
    },
    command = function(task_name, ...) {
      paste(
        "apply_rloadest(",
        "ind_file=target_name,",
        sprintf("partition=partition_%s,", task_name),
        "wq_dates=wq_dates)",
        sep="\n      ")
    }
  )

  post <- scipiper::create_task_step(
    step_name = 'post',
    target_name = function(task_name, step_name, ...) {
      scipiper::as_ind_file(file.path(folders$out, sprintf('%s.feather', task_name)))
    },
    command = function(task_name, ...) {
      sprintf(
        paste(
          "gd_put(",
          "remote_ind=target_name,",
          "local_source='%s',",
          "mock_get=I('move'),",
          "on_exists=I('update'))",
          sep="\n      "),
        scipiper::as_ind_file(file.path(folders$tmp, sprintf('%s.feather', task_name))))
    }
  )

  retrieve <- scipiper::create_task_step(
    step_name = 'retrieve',
    target_name = function(task_name, step_name, ...) {
      file.path(folders$out, sprintf('%s.feather', task_name))
    },
    command = function(task_name, target_name, ...) {
      sprintf(
        paste(
          "gd_get(",
          "ind_file='%s')",
          sep="\n      "),
        scipiper::as_ind_file(target_name))
    }
  )

  task_plan <- scipiper::create_task_plan(
    task_names=sort(partitions$PullTask),
    task_steps=list(partition, forecast_rloadest, post, retrieve),
    final_steps='post',
    add_complete=FALSE,
    ind_dir=folders$log)

}

create_forecast_makefile <- function(makefile, task_plan) {
  create_task_makefile(
    makefile=makefile, task_plan=task_plan,
    #include='remake.yml',
    packages=c('dplyr', 'dataRetrieval', 'feather', 'scipiper','doMC','foreach'),
    file_extensions=c('ind','feather'))
}
