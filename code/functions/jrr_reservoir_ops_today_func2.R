#******************************************************************
# Adds/updates the reservoir time series (ts) dfs with today's values
#******************************************************************
# Inputs
#******************************************************************
# date_sim = current date in the simulation
# res = reservoir name (sen, jrr, pat, or occ) 
#  (?to simulate sav: add 20% to jrr cap and combine sav & jrr inflows?)
# res.ts.df = reservoir time series dataframe
# withdr_req = withdrawal request (= 0 for sen & jrr)
# ws_rel_req = release request for water supply purposes
#     (release over dam to stream below)
# prioritization of flowby vs water supply is really a policy decision!
# prioritization: in logic below, 1st priority is flowby = res@flowby
#                 2nd priority is withdr_req
#                 3rd priority is ws_rel_req
#   all of WMA reservoirs except Savage have ws withdr OR rel, NOT both
#   in case of Savage, I THINK Westernport withdr would have priority
#       over a release, but I THINK min flowby would have 1st priority?
#                 
#******************************************************************
# Intermediate variables
#******************************************************************
# the variables, cap & flowby, are stored in the reservoir objects
#   (the names of the reservoir objects are sen, jrr, occ, pat)
# cap = capacity = reservoir's maximum storage capacity
# flowby = required flowby over the dam
#******************************************************************
# Outputs
#******************************************************************
# jrr.ts.df (with one new row - today)
#--------------------------------------------------------------------------------
# jrr.ts.df is special, and has the following columns:
#    date_time
#    storage - beginning of period (BOP) - right now is beginning of day
#    inflow - from inflow input time series
#    withdr - actual withdrawal (might be less than requested)
#    outflow - downstream discharge, ie, release over the dam
#        (includes total release, for whatever purposes, incl. spill)
#    w_req - water supply withdrawal request (from intake)
#    ws_rel_req - water supply release request
#    available - available for release over dam
#
jrr_reservoir_ops_today_func2 <- function(date_sim, res, res.ts.df, 
                                     withdr_req, 
                                     ws_rel_req,
                                     wq_rel_req) {
  #
  # Implement water balance eq. for s = beginning of period (BOP) storage:
  #   i = period i (and right now, it means day i)
  #   s(i+1) = s(i) + inflow(i) - withdr(i) - outflow(i)
  #   taking into account constraints:
  #       0 <= s <= cap
  #       w(i) = withdr_req(i), or = s + inflow if not enough water
  #       spill(i) = excess water, over capacity
  # (Note the loop will write 1 extra row at end with date_time = <NA>)
  # Access some values storaged in the reservoir object
  #
  # print(paste("date_sim  in jrr_reservoir_ops is ", date_sim))
  #
    cap <- res@capacity # see reservoir_make.R for basic res data
    # also for jrr need to know the "conservation pool" storage 
    #   because all water above this is neither ws nor wq storage
    #   (jrr_cap_cp is currently given in parameters.R) 
    jrr_cap_cp <- 28223.0
    cap_ws <- jrr_cap_cp*jrr_ws_frac
    cap_wq <- jrr_cap_cp*jrr_wq_frac
    #
    flowby <- res@flowby
    withdr_max <- res@withdr_max
    withdr_min <- res@withdr_min
    rc.df <- res@rc
  
    #
    if(withdr_req > withdr_max) {withdr_req = withdr_max}
  # Trim the res.ts.df to make sure the last row is yesterday
    res.ts.df <- data.frame(res.ts.df) %>%
      dplyr::filter(date_time < date_sim)
  # Get the last row of the res.ts.df, "yesterday", 
  #    and read its values:
    yesterday.df <- tail(res.ts.df,1)
    #
    yesterday_date <- yesterday.df$date_time[1] # yesterday's date
    month_sim_in <- month(yesterday_date + 1) 
    rc <- subset(rc.df, month_sim == month_sim_in)   
    #
    stor <- yesterday.df[1,2] # yesterday's BOP storage
    stor_ws <- yesterday.df[1,3]
    inflow <- yesterday.df[1,5] # yesterday's inflow
    inflow_ws <- inflow*jrr_ws_frac # not quite right but good enough for now
    w <- yesterday.df[1,6] # yesterday's  withdrawal from intake in reservoir
    outflow <- yesterday.df[1,7] # yesterday's release
    outflow_ws <- yesterday.df[1,8] # yesterday's ws release
    outflow_wq <- yesterday.df[1,9] # yesterday's wq release
    #
# calculate today's BOP storage from yesterday's stor, inflow & outflow
    stor <- stor + inflow - w - outflow # today's bop storage
    stor_ws <- stor_ws + inflow_ws - outflow_ws
    if(stor_ws >= cap_ws) {stor_ws <- cap_ws}    
    stor_wq <- stor - stor_ws # this is really wq + perhaps flood storage
    if(stor_wq >= cap_wq) {stor_wq <- cap_wq}
#
# Calculate the desired release over (or thru) the dam 
# Important to remember ws_rel_req was need ABOVE the wq_rel_req
    # where wq_rel_req was the assumed wq rel if NO ws rel
    # but wq_rel will drop to min if there IS a ws rel
    rel_req <- ws_rel_req + wq_rel_req
# Calculate values for a new row, "today", of the res.ops.df:
    newrow.df0 <- subset(res@inflows, 
                        date_time == yesterday_date + 1)
    date_today <- newrow.df0$date_time
    inflow <- newrow.df0$inflows
    inflow_ws <- inflow*jrr_ws_frac
    inflow_wq <- inflow - inflow_ws
    w_req <- withdr_req # this is 0 for Jennings
    available <- stor + inflow - w_req
    #
    # if there's a ws release request, drop the wq release down to the min
    ws_rel <- ws_rel_req
    wq_rel <- wq_rel_req
    # print(paste("ws_rel0", ws_rel))
      if(ws_rel_req > 0){
      wq_rel <- rc$withdr1*1.0       
      ws_rel <- rel_req - wq_rel
      }
    # if there's excess water over the top RC, release it, and if
    #   excess < rel_req it's safe to release rel_req
    # (forget for now about the Corps' max release of 9000 cfs)
    if(available >= rc$stor3) {
      outflow0 <- if_else((available - rc$stor3) >= rel_req,
                          available - rc$stor3, rel_req)
      outflow0 <- if_else(outflow0 > 1000.0, 1000.0, outflow0)} # impose a max
    # outflow0 <- case_when(
    #   available - rc$stor3 >= rel_req & available - rc$stor3 <- 1000.0
    #   ~ available - rc$stor3,
    #   available - rc$stor3 >= rel_req & available - rc$stor3 > 1000.0,
    #   ~ 1000.0,
    #   TRUE ~ rel_req)
    # }
    # if between top and normal RCs, do similarly
    temp <- available - rc$stor2
    if(available < rc$stor3 & available >= rc$stor2) {
      outflow0 <- if_else(temp >= rel_req, 
                          temp, rel_req, missing = NULL)}
    # if below the normal or the lower RC, release rc amount
    #   unless there's a ws request
    if(available < rc$stor2) {
      if(ws_rel_req == 0){
        outflow0 <- if_else(available < rc$stor1, rc$withdr1, rc$withdr2)
        ws_rel <- 0.0
      }
      if(ws_rel_req > 0){
        # make sure there's enough in ws storage for the ws release
        ws_rel <- if_else(ws_rel < stor_ws, ws_rel, stor_ws)
        # if resulting rel is less than Corps would release, skip ws rel
        if(ws_rel + wq_rel < wq_rel_req) {
          wq_rel <- wq_rel_req
          ws_rel <- 0.0
        }
        # print(paste("ws_rel3", ws_rel))
      }
      # print(paste("ws_rel4", ws_rel))
      outflow0 <- ws_rel + wq_rel
    }
    
    newrow.df1 <- newrow.df0 %>%
      mutate(stor = stor,
             inflow = inflow,
             inflow_ws = inflow_ws,
             inflow_wq = inflow - inflow_ws,
             w_req = w_req, # this is 0 for jrr so don't worry
             # available for release over dam - high storage conditions
             available = available,
             rel_req = rel_req,
             ws_rel_req = ws_rel_req,
             wq_rel_req = wq_rel_req,
             outflow_ws = ws_rel,
             outflow_wq = wq_rel
             )
    #-------------------------------------------------------------------
    # What is outflow today?
    newrow.df3 <- newrow.df1 %>%
      mutate(outflow = outflow0,
             withdr_req = w_req, # jrr withdr and withdr_req both 0
             withdr = withdr_req,
             storage = stor,
             storage_ws = stor_ws,
             storage_wq = stor_wq
      )
    newrow.df4 <- newrow.df3 %>%
      select(date_time, storage, storage_ws, storage_wq,
             inflow, withdr, outflow, outflow_ws, outflow_wq,
             withdr_req, rel_req, ws_rel_req, wq_rel_req, available)
    #
    # add the new row, today, to res.ops.df:
    res.ts.df <- rbind(res.ts.df, newrow.df4)
    # temp <- tail(res.ts.df,2)
    # print("temp")
    # print(temp)
  #  ts$res <- rbind(res.ts.df, newrow.df) ?
  return(res.ts.df)
}