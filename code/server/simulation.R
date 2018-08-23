#------------------------------------------------------------------
#------------------------------------------------------------------
# Simulate CO-OP system operations 
# - this is the "main" code
#------------------------------------------------------------------
#------------------------------------------------------------------
#
#--------------------------------------------------------------------------------
# Define the simulation date range and "todays" date
#--------------------------------------------------------------------------------
# date_start and date_end are for now defined in global.R.
# potomac.data.df is a placeholder for the various data sources
#   - it's loaded with data from date_start to date_end by potomac_flows_init.R.
# 
# We want to simulate up to date_today, and see things graphed
#   up thru date_today + (not yet implemented) some forecasts (fcs) 
#   up thru some period - maybe 15 days out into the future?
date_today <- as.Date("1930-05-01") # later to be reactive
# date_today <- input$DREXtoday - this doesn't work
# sim_n <- as.numeric(as.POSIXct(date_today) - as.POSIXct(date_start),
#                     units = "days")
# 
mos_0day <- 40 # margin of safety for Seneca release
mos_9day <- 0 # margin of safety for N Br release
#
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Main program - run the daily simulation
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#
# # Define the main sim function - simulates from date_start to date_today
# 
# sim_main_func <- function(date_today, ts0){
#   sim_n <- as.numeric(as.POSIXct(date_today) - as.POSIXct(date_start),
#                       units = "days")
#   ts <- ts0 # this holds the time series for the day, date_start
#   for (sim_i in 2:sim_n + 1) { # start by adding the 2nd day
#     date_sim <- as.Date(date_start + sim_i - 1)
#     # simulation_func adds one day, date_sim, to all of the time series
#     ts <- simulation_func(date_sim,
#                           mos_0day,
#                           mos_9day,
#                           demands.daily.df,
#                           potomac.daily.df,
#                           sen,
#                           jrr,
#                           ts)
#     } # end of simulation loop
#   return(ts)
# } # end of function
#
# Now run the main simulation
# ts0 <- list(sen = sen.ts.df0, jrr = jrr.ts.df0, flows = potomac.ts.df0)
# ts <- sim_main_func(date_today, ts0)

# # Add 30 days to simulation
# 
# sim_add_days_func <- function(added_days, uptodate.ts){
#   date_start <- last(uptodate.ts[[1]]$date_time)
#   for (sim_i in 2:added_days + 1) { # start the simulation on the 2nd day
#     date_sim <- as.Date(date_start + sim_i - 1)
#     uptodate.ts <- simulation_func(date_sim,
#                                    mos_0day,
#                                    mos_9day,
#                                    demands.daily.df,
#                                    potomac.daily.df,
#                                    sen,
#                                    jrr,
#                                    uptodate.ts)
#   }
#   return(uptodate.ts)
# }
# added_days <- 30
# uptodate.ts <- sim_add_days_func(added_days, uptodate.ts)
#
# *************************************************
# Grab ts and prepare for graphing:
# sen.ts.df <- ts$sen
# jrr.ts.df <- ts$jrr
# potomac.ts.df <- ts$flows
# 
# #
# potomac.graph.df0 <- left_join(potomac.ts.df, 
#                                potomac.data.df, 
#                                by = "date_time") %>%
#   dplyr::select(date_time, lfalls_nat = lfalls_nat.x, 
#                 por_nat, demand, lfalls_obs,
#                 sen_outflow, jrr_outflow)
# potomac.graph.df <- potomac.graph.df0 %>%
#   gather(key = "location", 
#          value = "flow_mgd", -date_time) 
# dplyr::filter(date_time <= date_today)
# temp.df <- data.frame(date_time = date_today + 9, 
#                       location = "test", 
#                       flow_mgd = 999,
#                       stringsAsFactors = FALSE)
# *************************************************

 
