#------------------------------------------------------------------
#------------------------------------------------------------------
# Create two dataframes of Potomac River flows inflow & outflow data
#    and Potomac River simulated flow time series
#------------------------------------------------------------------
#------------------------------------------------------------------
#
#--------------------------------------------------------------------------------
# Create dataframe of the data needed to compute Potomac flows 
#--------------------------------------------------------------------------------
#   - date_time - load in all data within date_start & date_end
#   - por_nat, below_por, lfalls_nat - coop's "natural" flows
#   - demands_total_unrestricted
#--------------------------------------------------------------------------------
potomac.data.df <- flows.daily.mgd.df %>%
  dplyr:: select(date_time, por_nat, below_por, lfalls_nat) %>%
  dplyr:: filter(date_time <= date_end,
                 date_time >= date_start)
#
# For the moment need to be careful - didn't add enough demand data
demands.daily.df <- demands.daily.df %>%
  dplyr:: filter(date_time <= date_end,
                 date_time >= date_start)
#
potomac.data.df <- left_join(potomac.data.df, 
                             demands.daily.df,
                             by = "date_time") %>%
  select(date_time,por_nat, below_por, 
         lfalls_nat, demands_total_unrestricted)
#--------------------------------------------------------------------------------
# Create and initialize dataframe of Potomac simulated flow time series
#--------------------------------------------------------------------------------
#   - date_time - initialized as date_start
#   - lfalls_nat - for QAing
#   - demand - actual & maybe fc'd demands, incl. restrictions
#   - lfalls_adj - lfalls "adjusted" - without effect of COOP withdrawals
#   - lfalls_obs
#   - lfalls_obs_fc9 - our 9-day fc for lfalls
#   - sen_outflow
#   - jrr_outflow
#   - jrr_outflow_lagged
#--------------------------------------------------------------------------------
jrr_outflow_lagged_default <- 129
potomac.ts.df0 <- potomac.data.df[1,] %>%
  mutate(lfalls_obs = lfalls_nat - 
           demands_total_unrestricted,
         lfalls_adj = lfalls_nat,
         lfalls_obs_fc9 = NA,
         demand = demands_total_unrestricted,
         sen_outflow = sen.ts.df$outflow[1],
         jrr_outflow = jrr.ts.df$outflow[1],
         jrr_outflow_lagged = jrr_outflow_lagged_default) %>%
  select(date_time, lfalls_nat, por_nat, demand, 
         lfalls_adj, lfalls_obs, lfalls_obs_fc9,
         sen_outflow, jrr_outflow, jrr_outflow_lagged)
potomac.ts.df <- potomac.ts.df0
#
# Make the 9-day flow forecast, using our old empirical eq., also used in PRRISM
#--------------------------------------------------------------------------------

