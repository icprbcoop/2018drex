#******************************************************************
# A function that provides the necessary demand forecasts
# (Right now its just reading the input demand time series)
#   - it's being called by simulation_func, 
#   - which is called by sim_main_func and sim_add_days_func
#   - which is called by server.R
#******************************************************************
# Inputs
#******************************************************************
# date_sim - the current date in the simulation
# demands.df - a df containing the demand time series, with
#   d_total - total wma demands
#   d_fw_e - Fairfax Water eastern service area demand (no LW)
#   d_fw_w - Fairfax Water western service area demand (no LW)
#   d_fw_c - Fairfax Water central service area demand (no LW)
#   d_lw - Loudoun Water total demand (incl. purchase from FW)
#   d_wa - Washington Aqueduct demand
#   d_wssc - WSSC demand
#******************************************************************
# Output
#******************************************************************
# A vector of 15 demands, beginning with today's 
#   and ending 14 days hence.
# These are actual data, serving as placeholder values for forecasts.
forecasts_demands_func <- function(date_sim, demands.daily.df){
  demands.fc.df <- demands.daily.df %>%
    dplyr::filter(date_time >= date_sim,
                  date_time < date_sim + 15) %>%
    dplyr::select(date_time, d_fw_e, d_fw_w, d_fw_c, d_lw,
                  d_wa, d_wssc)
  return(demands.fc.df)
}