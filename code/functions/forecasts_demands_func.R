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
forecasts_demands_func <- function(date_sim000,
                                   dr_wma,
                                   demands.daily.df){
  d_reduction_factor <- 1.0 - dr_wma
  # This is an demand multiplication factor set in parameters.R
  #   - it's usually 1 but can be something else for QAing or other purposes
  d_factor <- d_reduction_factor*d_wma_factor
  #
  demands.fc.df <- demands.daily.df %>%
    dplyr::filter(date_time >= date_sim000,
                  date_time < date_sim000 + 15) %>%
    dplyr::mutate(d_fw_e = d_fw_e*d_factor,
                  d_fw_w = d_fw_w*d_factor,
                  d_fw_c = d_fw_c*d_factor,
                  d_lw = d_lw*d_factor,
                  d_wssc = d_wssc*d_factor,
                  d_wa = d_wa*d_factor,
                  d_total = d_total*d_factor) %>%
    dplyr::select(date_time, d_fw_e, d_fw_w, d_fw_c, d_lw,
                  d_wa, d_wssc)
  return(demands.fc.df)
}