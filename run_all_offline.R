#******************************************************************
# run_all_offline runs the model outside of Shiny, for QAing purposes
#******************************************************************
# First run global.R, which loads all data, paths, functions
source("global.R", local = TRUE) 
#
# date_today is set in /input/parameters/parameters.R, 
#    but might want to change it
date_today <- as.Date("1930-02-01")
#
  # Run the main simulation to the hard-coded input, date_today
  #    - ts here is the precursor of the set of reactive values
  ts0 <- list(sen = sen.ts.df0, 
              jrr = jrr.ts.df0, 
              pat = pat.ts.df0,
              occ = occ.ts.df0,
              flows = potomac.ts.df0)
  ts <- sim_main_func(date_today, ts0)
  #
  flows.ts.df <- ts$flows
  date_last <- flows.ts.df$date_time
  print(paste("After initial main run, the last date is ", date_last))
  # Now rerun, just as in the Shiny model
  ts <- sim_main_func(date_today, ts)
  flows.ts.df <- ts$flows
  date_last <- flows.ts.df$date_time
  print(paste("After second main run, the last date is ", date_last))
    #
  # Now add chunks of days twice
  chunkofdays <- 7
  ts <- sim_add_days_func(chunkofdays, ts)
  flows.ts.df <- ts$flows
  date_last <- flows.ts.df$date_time
  print(paste("After adding 1st chunkofdays, the last date is ", date_last))
  ts <- sim_add_days_func(chunkofdays, ts)
  flows.ts.df <- ts$flows
  date_last <- flows.ts.df$date_time
  print(paste("After adding 2nd chunkofdays, the last date is ", date_last))
  #
  # Now write some output
  write.csv(ts$flows, paste(ts_output, "offline_flows.csv"))
  write.csv(ts$sen, paste(ts_output, "offline_sen.csv"))
  write.csv(ts$jrr, paste(ts_output, "offline_jrr.csv"))
  write.csv(ts$occ, paste(ts_output, "offline_occ.csv"))
  write.csv(ts$pat, paste(ts_output, "offline_pat.csv"))
  #
  # The End