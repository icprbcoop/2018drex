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
# ts = list(sen.ts.df, jrr.ts.df, potomac.ts.df) - all ops time series
#--------------------------------------------------------------------------------
# Outputs
#--------------------------------------------------------------------------------
# ts = list(sen.ts.df, jrr.ts.df, potomac.ts.df) - with today added
#--------------------------------------------------------------------------------
simulation_func <- function(date_sim,
                            mos_0day,
                            mos_9day,
                            demands.daily.df,
                            potomac.daily.df,
                            sen,
                            jrr,
                            ts){
  #
  #-----------------------------------------------------------------------------
  # 0. First grab the demand forecasts
  #-----------------------------------------------------------------------------
  #  - right now just use values from input demand time series
  #  - eventually, would use CO-OP demand models
  #  - also will depend on restriction status, ie res levels
  #
  demands.fc.df <- forecasts_demands_func(date_sim, demands.daily.df)
  # This df has a length of 15, with cols date_time, demands_fc
  #
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
                                        #                                        res.ts.df = sen.ts.df,
                                        res.ts.df = ts$sen,
                                        withdr_req = 0,
                                        ws_rel_req = 0) 
  jrr.ts.df <- reservoir_ops_today_func(date_sim = date_sim,
                                        res = jrr,
                                        #                                        res.ts.df = jrr.ts.df,
                                        res.ts.df = ts$jrr,
                                        withdr_req = 0,
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
  jrr.ts.df <- reservoir_ops_today_func(date_sim = date_sim,
                                        res = jrr, 
                                        res.ts.df = jrr.ts.df,
                                        withdr_req = 0,
                                        ws_rel_req = ws_need_9day)
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
  # Package up the time series in a list to return
  # temp <- list(sen = sen.ts.df, jrr = jrr.ts.df, flows = potomac.ts.df)
  ts$sen <- sen.ts.df
  ts$jrr <- jrr.ts.df
  ts$flows <- potomac.ts.df
  return(ts)  
}