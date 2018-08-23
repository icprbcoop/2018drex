#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Adds more days to the simulation
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Inputs
#--------------------------------------------------------------------------------
# added_days = number of days to be added to the simulation
# uptodate.ts = the list of dfs: sen.ts.df, jrr.ts.df, potomac.ts.df
#--------------------------------------------------------------------------------
# Output
#--------------------------------------------------------------------------------
# uptodate.ts
#--------------------------------------------------------------------------------
#
sim_add_days_func <- function(added_days, ts){
  df1 <- ts$flows
  date_start <- last(df1$date_time)
  for (sim_i in 2:added_days + 1) { # start the simulation on the 2nd day
    date_sim <- as.Date(date_start + sim_i - 1)
    uptodate.ts <- simulation_func(date_sim,
                                   mos_0day,
                                   mos_9day,
                                   demands.daily.df,
                                   potomac.daily.df,
                                   sen,
                                   jrr,
                                   ts)
  }
  return(ts)
}