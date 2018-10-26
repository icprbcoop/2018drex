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
jrr_reservoir_ops_today_func <- function(date_sim, res, res.ts.df, 
                                     withdr_req, 
                                     ws_rel_req,
                                     wq_rel_req){
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
    if(withdr_req > withdr_max) {withdr_req = withdr_max}
  # Trim the res.ts.df to make sure the last row is yesterday
    res.ts.df <- data.frame(res.ts.df) %>%
      dplyr::filter(date_time < date_sim)
  # Get the last row of the res.ts.df, "yesterday", 
  #    and read its values:
    yesterday.df <- tail(res.ts.df,1)
    #
    yesterday_date <- yesterday.df$date_time[1] # yesterday's date
    month_today <- month(yesterday_date + 1) 
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
    # print("df0 is: ")
    # print(month_today)
# calculate today's BOP storage
    stor <- stor + inflow - w - outflow # today's bop storage
    stor_ws <- stor_ws + inflow_ws - outflow_ws
    if(stor >= jrr_cap_cp) {stor_ws <- cap_ws}    
    stor_wq <- stor - stor_ws
    if(stor >= jrr_cap_cp) {stor_wq <- cap_wq}
# calculate the requested release over (or thru) the dam 
    rel_req <- ws_rel_req + wq_rel_req
# calculate values for a new row, "today", of the res.ops.df:
    newrow.df0 <- subset(res@inflows, 
                        date_time == yesterday_date + 1)
    # print("df0 is: ")
    # print(newrow.df0)
    newrow.df1 <- newrow.df0 %>%
      mutate(stor = stor,
             inflow = inflows,
             inflow_ws = inflow*jrr_ws_frac,
             inflow_wq = inflow - inflow_ws,
             w_req = withdr_req, # this is 0 for jrr so don't worry
             # available for release over dam - high storage conditions
             available = stor + inflow - w_req,
             rel_req = rel_req,
             ws_rel_req = ws_rel_req,
             wq_rel_req = wq_rel_req)
    # print("df1 is: ")
    # print(newrow.df1)
    newrow.df2 <- newrow.df1 %>%
      mutate(outflow0 = case_when(
               # if excess water (over capacity) is greater 
               #    than that needed for release, release it all
               available - cap >= rel_req ~ available - cap, # spill
               # if there's enough water to meet the release request, do it
               available - cap < rel_req & available > rel_req 
               ~ rel_req,
               # if there's not enough water to meet the release request, 
               #    then release what's there
               available - cap < rel_req & available <= rel_req 
               ~ available))
    # print("df2 is: ")
    # print(newrow.df2)
    newrow.df3 <- newrow.df2 %>%
      mutate(outflow = case_when(
        # make sure outflow isn't negative
               outflow0 < 0 ~ 0,
               outflow0 >= 0 ~ outflow0),
             outflow_ws = case_when(
               # if more than enough water, ws request met
               outflow >= rel_req ~ ws_rel_req,
               # if not enough water but meet the min wq release
               outflow < rel_req & outflow >= wq_rel_req ~ outflow - wq_rel_req,
               # if not enough water even for the wq release, then no ws deduction
               outflow < rel_req & outflow < wq_rel_req ~ 0),
             outflow_wq = outflow - outflow_ws,
             withdr_req = w_req, # jrr withdr and withdr_req both 0
             withdr = withdr_req,
             storage = stor,
             storage_ws = stor_ws,
             storage_wq = stor_wq
      )
    # print("df3 is: ")
    # print(newrow.df3)
    newrow.df4 <- newrow.df3 %>%
      select(date_time, storage, storage_ws, storage_wq,
             inflow, withdr, outflow, outflow_ws, outflow_wq,
             withdr_req, rel_req, ws_rel_req, wq_rel_req, available)
    # print("last res.ts.df and df4 are: ")
    # print(last(res.ts.df))
    # print(newrow.df4)
    # print(length(newrow.df4))
    #
    # add the new row, today, to res.ops.df:
    res.ts.df <- rbind(res.ts.df, newrow.df4)
    # temp <- tail(res.ts.df,2)
    # print("temp")
    # print(temp)
  #  ts$res <- rbind(res.ts.df, newrow.df) ?
  return(res.ts.df)
}