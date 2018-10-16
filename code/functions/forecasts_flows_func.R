#******************************************************************
# Adds/updates flow forecasts in potomac.ts.df
#******************************************************************
# Inputs
#******************************************************************
# date_sim = current date in the simulation
# demands.fc.df = forecasts of demands from date_sim to date_sim + 14
# sen_outflow_today - today's Seneca Reservoir release (total)
# jrr_outflow_today - today's JR Reservoir release (total)
# flow.ts.df (= potomac.ts.df) - the data frame to be updated
#
# Note that data.df = potomac.data.df is also used, but
#   this is created in global.R and is a placeholder for
#   all sorts of data and forecast sources
#******************************************************************
# Output
#******************************************************************
# flow.ts.df (=> potomac.ts.df)
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
forecasts_flows_func <- function(date_sim00, qavald, qavaln,
                                 demands.fc.df, # created by forecast_demands_func                                  
                                 sen_outflow_today,
                                 jrr_outflow_today,
                                 pat_withdr_today,
                                 occ_withdr_today,
                                 need_0day,
                                 need_1day,
                                 flows.ts){
  #------------------------------------------------------------------------------
  # Get data
  #------------------------------------------------------------------------------
  # Grab last 9 rows of flows.ts.df - 9 days past thru yesterday
  #   - and extract some info needed for the forecasts
  #   - should later check speed of filter vs tail( , 9)
  #   - how to handle beginning of sim?
  #
  # For some reason the next 2 lines don't work!!! but tail does!!!
  # flows.dayold <- data.frame(flows.ts.df) %>%
  #   dplyr::filter(date_time >= date_sim - 9, date_time < date_sim)
  #
  # flows.ts.df <- ts$flows 
  # flows.ts.df <- flows.ts
  # Trim the flow.ts.df to make sure the last row is yesterday
  flows.ts.df <- data.frame(flows.ts) %>%
    dplyr::filter(date_time < date_sim00)
  #
  # Get the last row of the flows.ts.df, "yesterday", 
  #    and read its values:
  yesterday.df <- tail(flows.ts.df,1)
  yesterday_date <- yesterday.df[1,1] # yesterday's date
  #
  flows.past9 <- tail(flows.ts.df, 9) # from 9 days past to yesterday
  jrr_outflow_lagged_today <- flows.past9$jrr_outflow[1] # Jrr release nine days ago
  sen_outflow_yesterday <- flows.past9$sen_outflow[9] # sen release yesterday
  upstr_m1 <- flows.past9$por_nat[9] + flows.past9$below_por[9]
  upstr_m2 <- flows.past9$por_nat[8] + flows.past9$below_por[8]
  withdr_pot_fw_yesterday <- flows.past9$withdr_pot_fw[9]
  #
  # Grab today's flows from potomac.data.df - a placeholder for flow data sources
  #   - but probably should be passing this to the function
  #   - and we also make use of the values passed to the func
#  newrow.df <- potomac.data.df %>%
#    dplyr::filter(date_time == date_sim) %>%  
  newrow.df <- subset(potomac.data.df, 
                      date_time == yesterday_date + 1) %>%
    dplyr::mutate(qad = qavald, # a slot for debugging dates
                  qav = qavaln, # a slot for debugging values
                  withdr_pot_wa = demands.fc.df$d_fw_c[1] +
                                  demands.fc.df$d_wa[1],
#                  withdr_pot_wssc = demands.fc.df$withdr_pot_wssc[1],
                  withdr_pot_wssc = demands.fc.df$d_wssc[1] -
                                     pat_withdr_today,
                  need_0day = need_0day,
                  need_1day = need_1day,
                  withdr_pot_fw = demands.fc.df$d_fw_e[1] +
                                  demands.fc.df$d_fw_w[1] +
                                  demands.fc.df$d_lw[1] - 
                                  occ_withdr_today,
                  withdr_pot_fw_lagged = withdr_pot_fw_yesterday,
# need to rename this demand_pot
                  demand = withdr_pot_wa + withdr_pot_wssc + withdr_pot_fw,
                  sen_outflow = sen_outflow_today, # a func input
                  sen_outflow_lagged = sen_outflow_yesterday,
                  sen_watershed = sen_other_watershed_flows, # from parameters.R
                  jrr_outflow = jrr_outflow_today, # a func input
                  jrr_outflow_lagged = jrr_outflow_lagged_today,
                  lfalls_nat = lfalls_nat*1.0, # somehow int - need num
                  lfalls_adj = lfalls_nat + jrr_outflow_lagged,
                  #----------------------------------------------------------------
                  # The 0-day fc happens here
                  # ie what's where we need it to be today
                  #
                  lfalls_obs = lfalls_nat + sen_watershed +
                    jrr_outflow_lagged + sen_outflow_lagged -
                    withdr_pot_wa - withdr_pot_wssc - 
                    withdr_pot_fw_lagged,
    #------------------------------------------------------------------------------
    # The 9-day lfalls observed fc
    #
                  lfalls_nat_fc9 = 288*exp(0.0009*lfalls_nat),
                  lfalls_nat_fc9 = if_else(lfalls_nat_fc9 <= lfalls_nat,
                                           lfalls_nat_fc9, lfalls_nat*1.0),
                  # lfalls_obs_fc9 = lfalls_nat_fc9 +
                  #     jrr_outflow + sen_watershed -
                  #     withdr_pot_wa - withdr_pot_wssc - withdr_pot_fw_lagged,
                  lfalls_obs_fc9 = lfalls_nat_fc9 +
                    jrr_outflow + sen_watershed - # assume no lsen release
                    withdr_pot_wa -
                    withdr_pot_wssc -
                    withdr_pot_fw_lagged,
    # The 1-day lfalls observed fc
    #
                  lfalls_obs_fc1 = lfalls_obs + 
                                   upstr_m1 - upstr_m2 +
                                   sen_outflow - sen_outflow_lagged
    ) %>% # end of mutate
    #------------------------------------------------------------------------------
    dplyr::select(date_time, qad, qav, lfalls_nat, 
                  por_nat, below_por, demand, 
                  lfalls_adj, lfalls_obs, 
                  lfalls_obs_fc9, lfalls_obs_fc1,
                  sen_outflow, sen_outflow_lagged, sen_watershed, 
                  jrr_outflow, jrr_outflow_lagged,
                  withdr_pot_wa, withdr_pot_wssc, 
                  need_0day, need_1day,
                  withdr_pot_fw, withdr_pot_fw_lagged)
  # Add the fc row for today to potomac.ts.df
  #   first deleting any preliminary values in the df
  # flows.ts.df <- data.frame(flows.ts.df) %>%
  #   dplyr::filter(date_time < date_sim)
  flows.ts <- rbind(flows.ts.df, newrow.df)
  #
  # Finally correct value of lfalls_obs
  #   - need the lagged impact of the jrr release:
  #
  # flows.ts.df <- flows.ts.df %>%
  #   dplyr::mutate(jrr_outflow_lagged = lag(jrr_outflow, 9, default = 129),
  #                 lfalls_adj = lfalls_nat + sen_outflow + jrr_outflow_lagged,
  #                 lfalls_obs = lfalls_adj - demand
  #                   )
  #
  # ts$flows <- flows.ts.df
  return(flows.ts)
}