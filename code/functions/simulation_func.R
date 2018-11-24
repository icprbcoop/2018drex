#******************************************************************
# Simulates today's ops and adds today to all time series (ts) dfs
#******************************************************************
# Inputs
#******************************************************************
# date_sim - "today" in date format
# mos_0day - margin of safety for Little Seneca release
# mos_9day - margin of safety for Jennings Randolph release
# demands.daily.df - placeholder - has demand time series
# potomac.daily.df - placeholder - has flow data
# sen - a reservior object representing Little Seneca Reservoir
# jrr - a reservoir object representing Jennings Randolph Reservoir
# pat - a reservoir object representing the Patuxent reservoirs
# occ - a reservoir object representing Occoquan Reservoir
# ts = list(sen.ts.df, ..., potomac.ts.df) - all ops time series
#******************************************************************
# Outputs
#******************************************************************
# ts is originally defined as a list of dataframes,
#    and then as a set of reactive values initialized to the original list
# ts = list(sen.ts.df, jrr.ts.df, pat.ts.df,
#           occ.ts.df, potomac.ts.df) - up through today
#--------------------------------------------------------------------------------
simulation_func <- function(date_sim0,
                            mos_0day,
                            mos_9day,
                            dr_va,
                            dr_md_cent,
                            dr_md_west,
                            mos_1day,
                            dr_wma_override,
                            demands.daily.df,
                            potomac.daily.df,
                            sen,
                            jrr,
                            pat,
                            occ,
                            ts){
  #
  #-----------------------------------------------------------------------------
  # 0. The demand forecasts (fcs)
  #-----------------------------------------------------------------------------
  #  - right now just use values from input demand time series
  #  - eventually, would use CO-OP demand models
  #
  # Find out demand restriction status
  # First compute upstr storage as fraction of capacity
  month_today <- month(date_sim0)
  jrr.ds <- ts$jrr
  sen.ds <- ts$sen
  jrr_stor_yesterday <- last(jrr.ds$storage_ws)
  sen_stor_yesterday <- last(sen.ds$storage)
  upstr_stor_frac <- (jrr_stor_yesterday + 
                        sen_stor_yesterday)/
    upstr_stor_cap
  dr_override <- 0.0
  # print(paste(date_sim0," upstr stor is ", upstr_stor_frac))
  summer <- if_else(month_today < 10 & month_today > 5, 1, 0)
  d_reduction_wma0 <- case_when(upstr_stor_frac < 0.6 &
                                 summer == 1 ~ 0.05,
                               upstr_stor_frac < 0.6 & 
                                 summer == 0 ~ 0.03,
                               TRUE ~ 0.0)
  d_reduction_wma <- if_else(dr_wma_override == 0.0,
                             d_reduction_wma0,
                             dr_wma_override/100)

  #
  # Now apply demand reduction if restrictions in place
  # print(paste(date_sim0," d_reduction's are ",
  #             d_reduction_wma0, d_reduction_wma))
  #
  # Next get the demand forecasts
  demands.fc.df <- forecasts_demands_func(date_sim0,
                                          d_reduction_wma,
                                          demands.daily.df)
  # This time series df has a length = 15, from today to 14 days hence
  # It is a placeholder for all the supplier total demand fcs
  # The demands.daily.df columns are: date_time, d_fw_e, d_fw_w, d_fw_c,
  #                  d_lw, d_wa, d_wssc, d_total
  #-----------------------------------------------------------------------------
  # Grab demand fcs for today (and tomorrow, and 9 days hence)
  # print(paste("in simulation_func, date_sim is ", date_sim0))
  d_today <- first(demands.fc.df)
  #
  #-----------------------------------------------------------------------------
  # Compute flow benefits (= demand reductions) due to
  #   water use restrictions in upstream  VA & MD counties
  dQ <- restriction_flow_benefits_func(dr_va, # % demand reduction in va
                                       dr_md_cent,
                                       dr_md_west)
  dQ_va <- dQ[[1]]
  dQ_md <- dQ[[2]]
  # dQ_va <- 9
  # dQ_md <- 11
  #
  #-----------------------------------------------------------------------------
  # Estimate Potomac & reservoir withdrawals assuming no ws releases/loadshifts
  #
  # First find wssc's pat withdr according to Patuxent rule curves (RCs)
  #   - based on today's demand fc
  pat.df <- ts$pat
  d_today_wssc <- d_today$d_wssc
  patstuff <- last(pat.df)
  pat_stor <- last(pat.df$storage)
  pat_withdr_req0 <- rule_curve_func(month_today, pat_stor, pat)
  pat_pot_withdr0 <- d_today_wssc - pat_withdr_req0 # for QAing
  #
  #-----------------------------------------------------------------------------
  # Next find FW's occ withdr according to Occoquan RCs
  #   - based on today's demand fc
  # (Need to add a lot of checks!)
  d_today_fw_e <- d_today$d_fw_e # eastern sa is served by Griffith/Occoquan
  occ.df <- ts$occ
  occ_stor <- last(occ.df$storage)
  occ_withdr_rc <- rule_curve_func(month_today, occ_stor, occ)
  # don't withdraw more than the eastern sa demand
  #  (no load-shifting yet)
  occ_withdr_req0 <- case_when(
    d_today_fw_e > occ_withdr_rc ~ occ_withdr_rc,
    d_today_fw_e <= occ_withdr_rc ~ d_today_fw_e)
  #
  # Finally compute Jennings water quality release 
  jrr.df <- ts$jrr
  jrr_stor <- last(jrr.df$storage)
  jrr_rel_rc <- nbr_rule_curve_func(month_today, jrr_stor, jrr)
  #
  #-----------------------------------------------------------------------------
  # 1. Compute today's upstr releases assuming no water supply (ws) needs
  #-----------------------------------------------------------------------------
  # last 2 function inputs are withdr_req & ws_rel_req
  #  - don't need flow fcs yet - assuming normal res wq releases
  #  - this will provide today's natural sen release to
  #     calculate lfalls_obs without ws releases.
  #
  ts$sen <- reservoir_ops_today_func(date_sim0,
                                        sen, 
                                        ts$sen,
                                        0,
                                        0) 
  ts$jrr <- jrr_reservoir_ops_today_func2(date_sim0,
                                        jrr,
                                        ts$jrr,
                                        0,
                                        0,
                                        jrr_rel_rc)
  ts$pat <- reservoir_ops_today_func(date_sim0,
                                        pat, # res
                                        ts$pat, # res.ts.df
                                        pat_withdr_req0, # withdr_req
                                        0)  # ws_rel_req
  ts$occ <- reservoir_ops_today_func(date_sim0,
                                        occ,
                                        ts$occ,
                                        occ_withdr_req0,
                                        0)
  #
  sen.ts.df1 <- ts$sen
  jrr.ts.df1 <- ts$jrr
  occ.ts.df1 <- ts$occ
  pat.ts.df1 <- ts$pat
  #-----------------------------------------------------------------------------
  # 2. Do prelim update of flows in potomac.ts.df 
  #    - this adds fc's of today's flows
  #    - later intend to also add future flows - ie 15-day fcs
  #    - in this step, assuming no ws releases
  #-----------------------------------------------------------------------------
  # qad and qav are 2 debugging slots
  qad1 <- as.Date(last(pat.ts.df1$date_time))
  qav1 <- 222.2
  demands.fc.df <- forecasts_demands_func(date_sim0, 
                                          d_reduction_wma,
                                          demands.daily.df)
  ts$flows <- forecasts_flows_func(date_sim0,
                                   qad1,
                                   qav1,
                                   dQ_va,
                                   dQ_md,
                                   demands.fc.df,
                                   last(sen.ts.df1$outflow),
                                   last(jrr.ts.df1$outflow),
                                   last(pat.ts.df1$withdr),
                                   last(occ.ts.df1$withdr),
                                   0,
                                   0,
                                   ts$flows)
  # Grab some results for use as input in next step
  potomac.ts.df2 <- ts$flows
  write.csv(potomac.ts.df2, paste(ts_output, "potomac.ts.df2.csv"))
  lfalls_obs_fc0_no_ws <- last(potomac.ts.df2$lfalls_obs)
  lfalls_obs_fc1_no_ws <- last(potomac.ts.df2$lfalls_obs_fc1)
  lfalls_obs_fc9_no_ws <- last(potomac.ts.df2$lfalls_obs_fc9)
  #-----------------------------------------------------------------------------
  # 3. Compute today's ws needs
  #-----------------------------------------------------------------------------
  #
  # Compute ws need today - for Patuxent loadshift
  #
  ws_need_0day <- estimate_need_func(lfalls_obs_fc0_no_ws,
                                     mos_0day)
  # Compute ws need today - for Seneca release
  #
  ws_need_1day <- estimate_need_func(lfalls_obs_fc1_no_ws,
                                     mos_1day)
  #
  # Compute ws need in 9 days - for N Br release
  #  - add a bit extra for quick and dirty balancing
  # jrr_sen_balance set in parameters.R
  ws_need_9day <- estimate_need_func(lfalls_obs_fc9_no_ws,
                                     mos_9day + jrr_sen_balance)
  #
  # What about the "Occoquan load-shift"? to save L Seneca storage
  #   Load-shifting, ie additional Occ withdrawal, is only allowed
  #     if Occoquan storage is above an "emergency level"
  #     occ_stor_emerg is the emergency level, set in parameters.R
  #     occ_fixed_ls is a fixed load-shift amount, set in parameters.R
  occ_ls <- 0
  sen_rel_req <- ws_need_1day
  occ_stor <- last(occ.ts.df1$storage)
  if (ws_need_1day > 0 & occ_stor > occ_stor_emerg){
    occ_ls <- occ_fixed_ls # set this as 15 in parameters.R
    sen_rel_req <- ws_need_1day - occ_fixed_ls
    if (sen_rel_req < 0) {sen_rel_req = 0}
  }
  #
  #-----------------------------------------------------------------------------
  # 4. Compute today's reservoir releases, taking into account ws needs
  #-----------------------------------------------------------------------------
  #   There are no withdrawals from Sen or JRR
  #
  ts$sen <- reservoir_ops_today_func(date_sim0,
                                        sen, 
                                        ts$sen,
                                        0,
                                        sen_rel_req)
  ts$jrr <- jrr_reservoir_ops_today_func2(date_sim0,
                                        jrr, 
                                        ts$jrr,
                                        0,
                                        ws_need_9day,
                                        jrr_rel_rc) # wq_rel_req
  ts$pat <- reservoir_ops_today_func(date_sim0,
                                        pat, # res = pat
                                        ts$pat, 
                                        pat_withdr_req0 + ws_need_0day, 
                                        0)  # ws_rel_req
  ts$occ <- reservoir_ops_today_func(date_sim0,
                                        occ, 
                                        ts$occ,
                                        occ_withdr_req0 + occ_ls,
                                        0)
  sen.ts.df4 <- ts$sen
  jrr.ts.df4 <- ts$jrr
  occ.ts.df4 <- ts$occ
  pat.ts.df4 <- ts$pat
  #
  sen_out <- last(sen.ts.df4$outflow)
  jrr_out <- last(jrr.ts.df4$outflow)
  pat_withdr <- last(pat.ts.df4$withdr)
  occ_withdr <- last(occ.ts.df4$withdr)
  #
  #-----------------------------------------------------------------------------
  # 5. Do final update of flows in potomac.ts.df
  #   -  taking into account possible changes in res releases
  #-----------------------------------------------------------------------------

  
  # On the one hand, there are repetitive aspects 
  #   to use of the same function twice (here and step 2).
  # On the other hand, it seems more consistent 
  #   to use just one function to update flows.
  #
  # First update the 15-day demand estimate?
  # demands.fc.df <- demands.fc.df %>%
  #   mutate(withdr_pot_wssc = d_wssc - last(pat.ts.df$withdr))
  #
  qad2 <- as.Date(last(occ.ts.df4$date_time))
  qad3 <- as.Date(last(potomac.ts.df2$date_time))
  qad5 <- date_sim0
  qav2 <- lfalls_obs_fc1_no_ws
  qav3 <- jrr_rel_rc
  ts$flows <- forecasts_flows_func(date_sim0,
                                   qad1,
                                   qav3,
                                   dQ_va,
                                   dQ_md,
                                   demands.fc.df,
                                   sen_out,
                                   jrr_out,
                                   pat_withdr,
                                   occ_withdr,
                                   ws_need_0day,
                                   ws_need_1day,
                                   ts$flows)
  potomac.ts.df5 <- ts$flows
  write.csv(potomac.ts.df5, paste(ts_output, "potomac.ts.df5.csv"))
  #-----------------------------------------------------------------------------
  # 6. Add today's values of va and md drought status indicators
  #
  # The indicator values are:
  #   0 = NORMAL
  #   1 = WATCH
  #   2 = WARNING
  #   3 = EMERGENCY
  #
  # The indicator names are
  #    gw_va_shen, p_va_shen, sw_va_shen, r_va_shen,
  #    gw_va_nova, p_va_nova, sw_va_nova, r_va_nova,
  #    region_md_cent, region_md_west
  #-----------------------------------------------------------------------------
  ts$states <- state_indices_update_func(date_sim0, ts$states)
  #
  return(ts)  
}