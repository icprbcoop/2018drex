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
# Define the main sim function - simulates from date_start to date_today

sim_main_func <- function(date_today, ts0){
  sim_n <- as.numeric(as.POSIXct(date_today) - as.POSIXct(date_start),
                      units = "days")
  ts <- ts0 # this holds the time series for the first day, date_start
  for (sim_i in 2:sim_n + 1) { # start by adding the 2nd day
    date_sim <- as.Date(date_start + sim_i - 1)
    # simulation_func adds one day, date_sim, to all of the time series
    ts <- simulation_func(date_sim,
                          mos_0day,
                          mos_9day,
                          demands.daily.df,
                          potomac.daily.df,
                          sen,
                          jrr,
                          ts)
  } # end of simulation loop
  return(ts)
} # end of function
