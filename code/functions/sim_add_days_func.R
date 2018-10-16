#******************************************************************
# Adds more days to the simulation
#******************************************************************
#
#******************************************************************
# Inputs - probably alot more things used should be inputs
#******************************************************************
# added_days = number of days to be added to the simulation
# ts = the list of dfs: potomac.ts.df, sen.ts.df, jrr.ts.df, ...
#   - where the time series begin on date_start and end yesterday
#******************************************************************
# Output
#******************************************************************
# ts - the time series with added_days days added
#--------------------------------------------------------------------------------
#
sim_add_days_func <- function(added_days, ts){
  df1 <- ts$flows
  date_start <- last(df1$date_time)
  for (sim_i in 1:added_days) { # start the simulation on the 2nd day
    date_sim <- as.Date(date_start + sim_i)
    ts <- simulation_func(date_sim,
                                   mos_0day,
                                   mos_9day,
                                   demands.daily.df,
                                   potomac.daily.df,
                                   sen, jrr, pat, occ,
                                   ts)
  }
  return(ts)
}