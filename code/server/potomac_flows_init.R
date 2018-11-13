#******************************************************************
# Create two dataframes of Potomac River flows inflow & outflow data
#    and Potomac River simulated flow time series
#******************************************************************
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
#         lfalls_nat, d_total)
         lfalls_nat)
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
sen_outflow_lagged_default <- 9
sen_other <- sen_other_watershed_flows # from parameters.R
potomac.ts.df0 <- potomac.data.df[1,] %>%
  mutate(lfalls_adj = lfalls_nat,
         qad = date_start,  # a slot for debugging dates
         qav = 9999.9,  # a slot for debugging values
         dQ_va = 0.0, # water use restriction benefits, mgd
         dQ_md = 0.0, # ''
         lfalls_obs_fc9 = 1000,
         lfalls_obs_fc1 = 1000,
         demand = 300, # delete this later
         sen_outflow = 0.0, # represents reservoir outflow
         sen_outflow_lagged = sen_outflow_lagged_default, # one-day lag
         sen_watershed = sen_other, # represents other seneca cr watershed flows
         jrr_outflow = 120,
         savage_outflow = 50,
         jrr_outflow_lagged = jrr_outflow_lagged_default,
         savage_outflow_lagged = 50,
         # creating Potomac withdrawals with reasonable start day values:
         withdr_pot_fw = 100,
         withdr_pot_fw_lagged = 100,
         withdr_pot_wssc = 100,
         need_0day = 0.0,
         need_1day = 0.0,
         withdr_pot_wa = 100,
         lfalls_obs = lfalls_nat - 300) %>%
  select(date_time, qad, qav, dQ_va, dQ_md,
         lfalls_nat, por_nat, below_por, demand, 
         lfalls_adj, lfalls_obs, 
         lfalls_obs_fc9, lfalls_obs_fc1,
         sen_outflow, sen_outflow_lagged, sen_watershed, 
         jrr_outflow, jrr_outflow_lagged,
         savage_outflow, savage_outflow_lagged,
         withdr_pot_fw, withdr_pot_fw_lagged,
         withdr_pot_wssc, need_0day, need_1day, withdr_pot_wa)
potomac.ts.df <- potomac.ts.df0
#
# Make the 9-day flow forecast, using our old empirical eq., also used in PRRISM
#--------------------------------------------------------------------------------

