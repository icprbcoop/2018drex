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
# sen - a reservior object representing Little Seneca
# jrr - a reservoir object representing Jennings Randolph
# ts = list(sen.ts.df, ..., potomac.ts.df) - all ops time series
#******************************************************************
# Outputs
#******************************************************************
# ts = list(sen.ts.df, ..., potomac.ts.df) - with today added
#--------------------------------------------------------------------------------
simulation_func <- function(date_sim0,
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
  # 0. The demand forecasts (fcs)
  #-----------------------------------------------------------------------------
  #  - right now just use values from input demand time series
  #  - eventually, would use CO-OP demand models
  #  - also will depend on restriction status, ie res levels
  #
  demands.fc.df <- forecasts_demands_func(date_sim0, demands.daily.df)
  # This time series df has a length = 15, from today to 14 days hence
  # It is a placeholder for all the supplier total demand fcs
  # The demands.daily.df columns are: date_time, d_fw_e, d_fw_w, d_fw_c,
  #                  d_lw, d_wa, d_wssc, d_total
  # Will add Potomac withdrawals: withdr_pot_fw, withdr_pot_wssc,
  #   withdr_pot_wa, withdr_pot_lw - these will simply be from today's 
  #                      - RC withdrawals; can improve later
  #-----------------------------------------------------------------------------
  # Grab demand fcs for today, tomorrow, and 9 days hence
  d_today <- first(demands.fc.df)
  d_1day <- demands.fc.df[2,]
  d_9day <- demands.fc.df[10,]
  month_today <- month(date_sim0)  
  month_1day <- month(date_sim0 + 1)
  month_9day <- month(date_sim0 + 9)
  #
  #-----------------------------------------------------------------------------
  # Estimate Potomac & reservoir withdrawals assuming no ws releases/loadshifts
  #
  # First find wssc's pot & pat withdr according to pat RCs
  #   - based on today's demand fc
  # (Need to add checks!)
  # d_today_pat <- d_today$d_wssc
  pat.df <- ts$pat
  pat_stor <- last(pat.df$storage)
  pat_withdr_req0 <- rule_curve_func(month_today, pat_stor, pat)
  # wssc_pot_withdr0 <- d_today_pat - pat_withdr_req0
  #
  #-----------------------------------------------------------------------------
  # Next find FW's pot & occ withdr according to occ RCs
  #   - based on tomorrow's demand fc
  # (Need to add a lot of checks!)
  d_1day_fw_e <- d_1day$d_fw_e # eastern sa is served by Griffith/Occoquan
  # d_1day_fw_w <- d_1day$d_fw_w # western sa is served by Corbalis/Potomac
  # d_1day_lw <- d_1day$d_lw # FW serves LW from Corbalis
  # d_today_fw_c <- d_today$d_fw_c # central sa (Falls Church) is served by Aqueduct
  occ.df <- ts$occ
  occ_stor <- last(occ.df$storage)
  occ_withdr_rc <- rule_curve_func(month_today, occ_stor, occ)
  occ_withdr_req0 <- case_when(
    d_1day_fw_e > occ_withdr_rc ~ occ_withdr_rc,
    d_1day_fw_e <= occ_withdr_rc ~ d_1day_fw_e)
  # fw_pot_withdr0 <- d_1day_fw_e + d_1day_fw_w + d_1day_lw - occ_withdr_req0
  #
  #-----------------------------------------------------------------------------
  # Finally compute WA's pot withdr = WA's demand + FW Central SA demand
  #   - based on today's demand fc
  # wa_pot_withdr0 <- d_today$d_wa + d_today_fw_c
  #
  # Take the 15-day demand df and estimate Potomac withdrawals each day
  # For now, use today's RC withdrawal for all of the 15 days of estimates
  demands.fc.df <- demands.fc.df %>%
    mutate(withdr_pot_wa = d_wa + d_fw_c,
           withdr_pot_wssc = d_wssc - pat_withdr_req0,
           withdr_pot_fw = d_fw_e + d_fw_w + d_lw - occ_withdr_req0)
  #-----------------------------------------------------------------------------
  # 1. Compute today's upstr releases assuming no water supply (ws) needs
  #-----------------------------------------------------------------------------
  # last 2 function inputs are withdr_req & ws_rel_req
  #  - don't need flow fcs yet - assuming normal res wq releases
  #  - this will provide today's natural sen release to
  #     calculate lfalls_obs without ws releases.
  #
  sen.ts.df <- reservoir_ops_today_func(date_sim0,
                                        sen, 
                                        ts$sen,
                                        0,
                                        0) 
  jrr.ts.df <- reservoir_ops_today_func(date_sim0,
                                        jrr,
                                        ts$jrr,
                                        0,
                                        0)
  pat.ts.df <- reservoir_ops_today_func(date_sim0,
                                        pat, # res
                                        ts$pat, # res.ts.df
                                        pat_withdr_req0, # withdr_req
                                        0)  # ws_rel_req
  occ.ts.df <- reservoir_ops_today_func(date_sim0,
                                        occ,
                                        ts$occ,
                                        occ_withdr_req0,
                                        0)
  #
  #-----------------------------------------------------------------------------
  # 2. Do prelim update of flows in potomac.ts.df 
  #    - this adds fc's of today's flows
  #    - later intend to also add future flows - ie 15-day fcs
  #    - in this step, assuming no ws releases
  #-----------------------------------------------------------------------------
  #
  qad1 <- as.Date(last(pat.ts.df$date_time))
  qav1 <- 111.1
  potomac.ts.df <- forecasts_flows_func(date_sim0,
                                        qad1,
                                        qav1,
                                        demands.fc.df,
                                        last(sen.ts.df$outflow),
                                        last(jrr.ts.df$outflow),
                                        last(pat.ts.df$withdr),
                                        0,
                                        ts$flows)
  # Grab some results for use as input in next step
  lfalls_obs_fc0_no_ws <- last(potomac.ts.df$lfalls_obs)
  lfalls_obs_fc1_no_ws <- last(potomac.ts.df$lfalls_obs_fc1)
  lfalls_obs_fc9_no_ws <- last(potomac.ts.df$lfalls_obs_fc9)
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
  ws_need_9day <- estimate_need_func(lfalls_obs_fc9_no_ws,
                                     mos_9day)
  #
  #-----------------------------------------------------------------------------
  # 4. Compute today's reservoir releases, taking into account ws needs
  #-----------------------------------------------------------------------------
  #   There are no withdrawals from Sen or JRR
  #
  sen.ts.df <- reservoir_ops_today_func(date_sim0,
                                        sen, 
                                        sen.ts.df,
                                        0,
                                        ws_need_1day)
  jrr.ts.df <- reservoir_ops_today_func(date_sim0,
                                        jrr, 
                                        jrr.ts.df,
                                        0,
                                        ws_need_9day)
  pat.ts.df <- reservoir_ops_today_func(date_sim0,
                                        pat, # res = pat
                                        ts$pat, # pat.ts.df
# why doesn't this work???              pat.ts.df,
                                        pat_withdr_req0 + ws_need_0day, 
                                        0)  # ws_rel_req
  occ.ts.df <- reservoir_ops_today_func(date_sim0,
                                        occ, 
                                        ts$occ,
                                        occ_withdr_req0,
                                        0)
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
  qad2 <- as.Date(last(occ.ts.df$date_time))
  qad3 <- as.Date(last(potomac.ts.df$date_time))
  qav2 <- 10.9
  potomac.ts.df <- forecasts_flows_func(date_sim0,
                                        qad2,
                                        qav2,
                                        demands.fc.df,
                                        last(sen.ts.df$outflow),
                                        last(jrr.ts.df$outflow),
                                        last(pat.ts.df$withdr),
                                        ws_need_0day,
                                        potomac.ts.df)
  # Send the results back to the set of reactive values, ts:
  ts$sen <- sen.ts.df
  ts$jrr <- jrr.ts.df
  ts$pat <- pat.ts.df
  ts$occ <- occ.ts.df
  ts$flows <- potomac.ts.df
  return(ts)  
}