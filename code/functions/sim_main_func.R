#******************************************************************
# Runs the simulation, beginning on date_start, defined in global.R
#******************************************************************
#
#******************************************************************
# Inputs - probably alot more things used should be inputs
#******************************************************************
# date_today = final day of the simulation
# ts0 = the list of dfs: potomac.ts.df, sen.ts.df, jrr.ts.df, ...
#   - where the time series begin on date_start and end yesterday
#******************************************************************
# Output
#******************************************************************
# ts - this may just be a list of the ts dfs, or may be reactive values
#    - the time series begin on date_start and end today
#--------------------------------------------------------------------------------
# Define the main sim function - simulates from date_start to date_today

sim_main_func <- function(date_today,
                          dr_va,
                          dr_md_cent,
                          dr_md_west,
                          mos_1day,
                          dr_wma_override,
                          ts){
  sim_n <- as.numeric(as.POSIXct(date_today) - as.POSIXct(date_start),
                      units = "days")
  # ts <- ts00 # why??? this holds the time series for the first day, date_start
  # print(paste("In sim_main_func, date_start is ", date_start))
  #     print(paste("In sim_main_func, date_today is ", date_today))
    for (sim_i in 1:sim_n) { # start by adding the 2nd day
    #date_sim <- as.Date(date_start + sim_i - 1)
      date_sim <- date_start + lubridate::days(sim_i)
      # print(paste("date_sim is ", date_sim))
    # print(paste("in sim_main_func, sim_i, date_sim are: ", sim_i, date_sim))
    # simulation_func adds one day, date_sim, to all of the time series
    ts <- simulation_func(date_sim,
                          mos_0day,
                          mos_9day,
                          dr_va,
                          dr_md_cent,
                          dr_md_west,
                          mos_1day,
                          dr_wma_override,
                          demands.daily.df,
                          potomac.daily.df,
                          sen, jrr, pat, occ, # these are the reservoir "objects"
                          ts)
  } # end of simulation loop
  return(ts)
} # end of function
