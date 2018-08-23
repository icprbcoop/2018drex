#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# A function that provides the necessary demand forecasts
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Inputs
#--------------------------------------------------------------------------------
# date_sim - the current date in the simulation
# demands.df - a df containing the demand time series
#--------------------------------------------------------------------------------
# Output
#--------------------------------------------------------------------------------
# A vector of 15 demands, beginning with today's 
#   and ending with 14 days hence.
# At the moment, the "forecasts" are just read from 
#    input demands in potomac.data.df
#
forecasts_demands_func <- function(date_sim, demands.df){
  demands.fc.df <- demands.df %>%
    dplyr::filter(date_time >= date_sim,
                  date_time < date_sim + 15) %>%
    dplyr::mutate(demands_fc = demands_total_unrestricted) %>%
    dplyr::select(date_time, demands_fc)
  return(demands.fc.df)
}