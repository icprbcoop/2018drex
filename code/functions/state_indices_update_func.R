#******************************************************************
# A function that updates the dataframe with state drought indices
#    by adding today's values
#   - it's being called by simulation_func, 
#   - which is called by sim_main_func and sim_add_days_func
#   - which is called by server.R
#******************************************************************
# Inputs
#******************************************************************
# date_sim00 - the current date in the simulation
# ts$states = state.ts.df 
#    - a df containing the drought indice time series
#   
#******************************************************************
# Output
#******************************************************************
# A new ts$states with today's values added
# 
state_indices_update_func <- function(date_sim0, state.df){
  newrow.df <- subset(state.drought.df, 
                      date_time == date_sim0) %>%
    select(date_time,
           gw_va_shen, p_va_shen, sw_va_shen, r_va_shen,
           gw_va_nova, p_va_nova, sw_va_nova, r_va_nova,
           #         gw_md_cent, p_md_cent, sw_md_cent, r_md_cent,
           #         gw_md_west, p_md_west, sw_md_west, r_md_west,
           region_md_cent, region_md_west
           )
  state.ts.df <- rbind(state.df, newrow.df)
  # print(paste(date_sim0, newrow.df$region_md_west[1]))
  return(state.ts.df)
  }