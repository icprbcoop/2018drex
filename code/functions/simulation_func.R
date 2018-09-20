#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Simulates today's ops and adds today to all time series (ts) dfs
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Inputs
#--------------------------------------------------------------------------------
# date_sim - "today" in date format
# mos_0day - margin of safety for Little Seneca release
# mos_9day - margin of safety for Jennings Randolph release
# demands.daily.df - placeholder - has demand time series
# potomac.daily.df - placeholder - has flow data
# sen - a reservior object representing Little Seneca
# jrr - a reservoir object representing Jennings Randolph
# ts = list(sen.ts.df, ..., potomac.ts.df) - all ops time series
#--------------------------------------------------------------------------------
# Outputs
#--------------------------------------------------------------------------------
# ts = list(sen.ts.df, ..., potomac.ts.df) - with today added
#--------------------------------------------------------------------------------
simulation_func <- function(date_sim,
                            mos_0day,
                            mos_9day,
                            demands.daily.df,
                            potomac.daily.df,
                            sen,
                            jrr,
                            pat,
                            occ,
                            ts){
  #
  #-----------------------------------------------------------------------------
  # 0. The demand forecasts
  #-----------------------------------------------------------------------------
  #  - right now just use values from input demand time series
  #  - eventually, would use CO-OP demand models
  #  - also will depend on restriction status, ie res levels
  #
  demands.fc.df <- forecasts_demands_func(date_sim, demands.daily.df)
  # This time series df has a length = 15, from today to 14 days in the future
  # The columns are: date_time, d_fw_e, d_fw_w, d_fw_c, d_lw
  #                  d_wa, d_wssc, d_total
  #-----------------------------------------------------------------------------
  # Grab today's demands
  d_today <- first(demands.fc.df)
  month_sim <- month(date_sim)  
  #
  # First find wssc's pot & pat withdr according to pat RCs
  d_today_pat <- d_today$d_pat
  pat.df <- ts$pat
  pat_stor <- last(pat.df$storage)
  pat_withdr_req0 <- rule_curve_func(month_sim, pat_stor, pat)
  wssc_pot_withdr0 <- d_today_pat - pat_withdr_req0
  #
  #-----------------------------------------------------------------------------
  # Next find FW's pot & occ withdr according to occ RCs
  d_today_fw_e <- d_today$d_fw_e
  d_today_fw_w <- d_today$d_fw_w
  d_today_fw_c <- d_today$d_fw_c
  occ.df <- ts$occ
  occ_stor <- last(occ.df$storage)
  occ_rc_withdr <- rule_curve_func(month_sim, occ_stor, occ)
  occ_withdr_req0 <- case_when(
    d_today_fw_e > occ_rc_withdr ~ occ_rc_withdr,
    d_today_fw_e <= occ_rc_withdr ~ d_today_fw_e)
  fw_pot_withdr0 <- d_today_fw_e + d_today_fw_w - occ_withdr_req0
  #
  #-----------------------------------------------------------------------------
  # Next compute WA's pot withdr = WA's demand + FW Central SA demand
  wa_pot_withdr0 <- d_today$d_wa + d_today_fw_c
  #-----------------------------------------------------------------------------
  # Next compute WA's pot withdr = WA's demand + FW Central SA demand
  tot_pot_withdr0 <- wssc_pot_withdr0 + fw_pot_withdr0 + wa_pot_withdr0
  #-----------------------------------------------------------------------------
  # 1. Compute today's res releases assuming no water supply (ws) need
  #-----------------------------------------------------------------------------
  # last 2 function inputs are withdr_req & ws_rel_req
  #  - don't need flow fcs yet - assuming normal res wq releases
  #  - this will provide today's natural sen release to
  #     calculate lfalls_obs without ws releases.
  #
  sen.ts.df <- reservoir_ops_today_func(date_sim = date_sim,
                                        res = sen, 
                                        res.ts.df = ts$sen,
                                        withdr_req = 0,
                                        ws_rel_req = 0) 
  jrr.ts.df <- reservoir_ops_today_func(date_sim = date_sim,
                                        res = jrr,
                                        res.ts.df = ts$jrr,
                                        withdr_req = 0,
                                        ws_rel_req = 0)
  pat.ts.df <- reservoir_ops_today_func(date_sim,
                                        pat, # res
                                        ts$pat, # res.ts.df
                                        pat_withdr_req0, # withdr_req
                                        0)  # ws_rel_req
  occ.ts.df <- reservoir_ops_today_func(date_sim = date_sim,
                                        res = occ,
                                        res.ts.df = ts$occ,
                                        withdr_req = occ_withdr_req0,
                                        ws_rel_req = 0)
  #
  #-----------------------------------------------------------------------------
  # 2. Do prelim update of flows in potomac.ts.df 
  #    - this adds fc's of today's flows
  #    - later intend to also add future flows - ie 15-day fcs
  #    - in this step, assuming no ws releases
  #-----------------------------------------------------------------------------
  #
  potomac.ts.df <- forecasts_flows_func(date_sim = date_sim,
                                        demands.fc.df = demands.fc.df,
                                        sen_outflow_today = last(sen.ts.df$outflow),
                                        jrr_outflow_today = last(jrr.ts.df$outflow),
                                        flows.ts.df = ts$flows)
  # Grab some results for use as input in next step
  lfalls_obs_today_no_ws <- last(potomac.ts.df$lfalls_obs)
  lfalls_obs_fc9_no_ws <- last(potomac.ts.df$lfalls_obs_fc9)
  #-----------------------------------------------------------------------------
  # 3. Compute today's ws needs
  #-----------------------------------------------------------------------------
  #
  # Compute ws need today - for Seneca release
  #
  ws_need_0day <- estimate_need_func(lfalls_flow = lfalls_obs_today_no_ws,
                                     mos = mos_0day)
  #
  # Compute ws need in 9 days - for N Br release 
  ws_need_9day <- estimate_need_func(lfalls_flow = lfalls_obs_fc9_no_ws,
                                     mos = mos_9day)
  #
  #-----------------------------------------------------------------------------
  # 4. Compute today's reservoir releases, taking into account ws needs
  #-----------------------------------------------------------------------------
  #   There are no withdrawals from Sen or JRR
  #
  sen.ts.df <- reservoir_ops_today_func(date_sim = date_sim,
                                        res = sen, 
                                        res.ts.df = sen.ts.df,
                                        withdr_req = 0,
                                        ws_rel_req = ws_need_0day)
  jrr.ts.df <- reservoir_ops_today_func(date_sim,
                                        jrr, 
                                        res.ts.df = jrr.ts.df,
                                        withdr_req = 0,
                                        ws_rel_req = ws_need_9day)
  pat.ts.df <- reservoir_ops_today_func(date_sim,
                                        pat, # res = pat
                                        ts$pat, # pat.ts.df
# why doesn't this work???              pat.ts.df,
                                        pat_withdr_req0, # withdr_req
                                        0)  # ws_rel_req
  occ.ts.df <- reservoir_ops_today_func(date_sim = date_sim,
                                        res = occ, 
                                        res.ts.df = ts$occ,
                                        withdr_req = occ_withdr_req0,
                                        ws_rel_req = 0)
  #
  #-----------------------------------------------------------------------------
  # 5. Do final update of flows in potomac.ts.df
  #   -  taking into account possible changes in res releases
  #-----------------------------------------------------------------------------
  # On the one hand, there are repetitive aspects 
  #   to use of the same function twice (here and step 2).
  # On the other hand, it seems more consistent 
  #   to use just one function to update flows.
  potomac.ts.df <- forecasts_flows_func(date_sim = date_sim,
                                        demands.fc.df = demands.fc.df,
                                        sen_outflow_today = last(sen.ts.df$outflow),
                                        jrr_outflow_today = last(jrr.ts.df$outflow),
                                        flows.ts.df = potomac.ts.df)
  # Send the results back to the set of reactive values, ts:
  ts$sen <- sen.ts.df
  ts$jrr <- jrr.ts.df
  ts$pat <- pat.ts.df
  ts$occ <- occ.ts.df
  ts$flows <- potomac.ts.df
  return(ts)  
}