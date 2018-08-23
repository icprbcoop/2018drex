#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Adds/updated flow forecasts in potomac.ts.df
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Inputs
#--------------------------------------------------------------------------------
# date_sim = current date in the simulation
# demands.fc.df = forecasts of demands from date_sim to date_sim + 14
# sen_outflow_today - today's Seneca Reservoir release (total)
# jrr_outflow_today - today's JR Reservoir release (total)
# flow.ts.df (= potomac.ts.df) - the data frame to be updated
#
# Note that data.df = potomac.data.df is also used, but
#   this is created in global.R and is a placeholder for
#   all sorts of data and forecast sources
#--------------------------------------------------------------------------------
# Output
#--------------------------------------------------------------------------------
# flow.ts.df (= potomac.ts.df)
#   - date_time
#   - lfalls_nat
#   - por_nat
#   - demand
#   - lfalls_adj = lfalls_nat + sen_outflow + jrr_outflow_lagged
#   - lfalls_obs = lfalls_adj - demand
#   - lfalls_obs_fc9
#   - sen_outflow
#   - jrr_outflow
#   - jrr_outflow_lagged
#--------------------------------------------------------------------------------
#
# For QA'ing:
# date_sim <- as.Date("1930-03-15")
# flows.ts.df <- potomac.ts.df
#
forecasts_flows_func <- function(date_sim,
                                 demands.fc.df,                                  
                                 sen_outflow_today,
                                 jrr_outflow_today,
                                 flows.ts.df){
  #------------------------------------------------------------------------------
  # First the 1-day fc of lfalls_obs
  #------------------------------------------------------------------------------
  # Grab today's flow fc - temporarily from the data.df
  #
  # Run for QAing:
  # date_sim <- "1930-01-02"
  # demands.fc.df                                  
  # sen_outflow_today
  # jrr_outflow_today
#  flows.ts.df <- potomac.ts.df
  # End for QAing  
  #
  # First we need to get some data
  #   (potomac.data.df is a placeholder for data & fc sources)
  #   and we also make use of the values passed to the func
  flows.fc.df <- potomac.data.df %>%
    dplyr::filter(date_time == date_sim) %>%
    dplyr::mutate(demand = first(demands.fc.df$demands_fc),
                  sen_outflow = sen_outflow_today,
                  jrr_outflow = jrr_outflow_today,
                  jrr_outflow_lagged = -9999.0,
                  lfalls_nat = lfalls_nat*1.0, # somehow int - need num
                  lfalls_adj = -9999.0,
                  lfalls_obs = -9999.0,
    # and we have enough info to make the 9-day lfalls fc
    #   though we don't have sen release in 9 days, use today's
    #   but FIX sen release - need sen wq release here!!!
                  lfalls_nat_fc9 = 288*exp(0.0009*lfalls_nat),
                  lfalls_nat_fc9 = if_else(lfalls_nat_fc9 <= lfalls_nat,
                                           lfalls_nat_fc9, lfalls_nat*1.0),
                  lfalls_obs_fc9 = lfalls_nat_fc9 + sen_outflow +
                    jrr_outflow - demands.fc.df$demands_fc[10]) %>%
                    
    dplyr::select(date_time, lfalls_nat, por_nat, demand, 
                  lfalls_adj, lfalls_obs, lfalls_obs_fc9,
                  sen_outflow, jrr_outflow, jrr_outflow_lagged)
  # Add the fc row for today to potomac.ts.df
  #   first deleting any preliminary values in the df
  flows.ts.df <- data.frame(flows.ts.df) %>%
    dplyr::filter(date_time < date_sim)
  flows.ts.df <- rbind(flows.ts.df, flows.fc.df)
  #
  # Finally correct value of lfalls_obs
  #   - need the lagged impact of the jrr release:
  #
  flows.ts.df <- flows.ts.df %>% 
    dplyr::mutate(jrr_outflow_lagged = lag(jrr_outflow, 9, default = 129),
                  lfalls_adj = lfalls_nat + sen_outflow + jrr_outflow_lagged,
                  lfalls_obs = lfalls_adj - demand
                    )
  #
  return(flows.ts.df)
}