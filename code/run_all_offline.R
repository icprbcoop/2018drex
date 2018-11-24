#******************************************************************
# run_all_offline runs the model outside of Shiny, for QAing purposes
#******************************************************************
# First run global.R, which loads all data, paths, functions
source("global.R", local = TRUE) 
#
# date_today0 is set in /input/parameters/parameters.R, 
#    but might want to change it
# date_today0 <- as.Date("1930-01-03")
#
  # Run the main simulation to the hard-coded input, date_today
  #    - ts here is the precursor of the set of reactive values

  ts0 <- list(sen = sen.ts.df0, 
              jrr = jrr.ts.df0, 
              pat = pat.ts.df0,
              occ = occ.ts.df0,
              flows = potomac.ts.df0,
              states = state.ts.df0)
  ts_new <- sim_main_func(date_today0,
                          dr_va0,
                          dr_md_cent0,
                          dr_md_west0,
                          mos_1day0,
                          dr_wma_override0,
                          ts0)
  #
  flows.ts.df <- ts_new$flows
  date_last <- last(flows.ts.df$date_time)
  # print(paste("After initial main run, the last date is ", date_last))
  # Now write some output
  write.csv(ts_new$flows, paste(ts_output, "offline_flows.csv"))
  write.csv(ts_new$sen, paste(ts_output, "offline_sen.csv"))
  write.csv(ts_new$jrr, paste(ts_output, "offline_jrr.csv"))
  write.csv(ts_new$occ, paste(ts_output, "offline_occ.csv"))
  write.csv(ts_new$pat, paste(ts_output, "offline_pat.csv"))
  #
  # # Now rerun, just as in the Shiny model
  # ts_new2 <- sim_main_func(date_today, ts_new)
  # flows.ts.df <- ts_new2$flows
  # date_last <- last(flows.ts.df$date_time)
  # print(paste("After second main run, the last date is ", date_last))
  #   #
  # # Now add chunks of days twice
  # chunkofdays <- 7
  # ts <- sim_add_days_func(chunkofdays, ts_new2)
  # flows.ts.df <- ts$flows
  # date_last <- last(flows.ts.df$date_time)
  # print(paste("After adding 1st chunkofdays, the last date is ", date_last))
  #
  # Now write some output
