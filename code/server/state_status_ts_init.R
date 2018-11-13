#******************************************************************
# Create and initialize dataframe of state drought status
#******************************************************************
#
#--------------------------------------------------------------------------------
# The format of the columns in state.drought.df is gw_va_shen
#   - the states are va and md
#   - the state regions are va: shen, nova
#                           md: cent, west
#   - the types of time series are gw, p, sw, and r
#       but md just displays a single value for each region
#--------------------------------------------------------------------------------
# Just grab one row of the values in state.drought.df
state.ts.df0 <- subset(state.drought.df, 
                       date_time == date_start) %>%
  select(date_time,
         gw_va_shen, p_va_shen, sw_va_shen, r_va_shen,
         gw_va_nova, p_va_nova, sw_va_nova, r_va_nova,
#         gw_md_cent, p_md_cent, sw_md_cent, r_md_cent,
#         gw_md_west, p_md_west, sw_md_west, r_md_west,
         region_md_cent, region_md_west
         )
state.ts.df <- state.ts.df0
#
