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
# res.ts.df (with one new row - today)
#--------------------------------------------------------------------------------
# res.ts.df has the following columns:
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
reservoir_ops_today_func <- function(date_sim, res, res.ts.df, 
                                     withdr_req, ws_rel_req){
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
    cap <- res@capacity # see reservoir_make.R for basic res data
    flowby <- res@flowby
    withdr_max <- res@withdr_max
    withdr_min <- res@withdr_min
    if(withdr_req > withdr_max) {withdr_req = withdr_max}
  # Trim the res.ts.df to make sure the last row is yesterday
    res.ts.df <- data.frame(res.ts.df) %>%
      dplyr::filter(date_time < date_sim)
  # Get the last row of the res.ts.df, "yesterday", 
  #    and read its values:
    yesterday.df <- tail(res.ts.df,1)
    yesterday_date <- yesterday.df[1,1] # yesterday's date
    stor <- yesterday.df[1,2] # yesterday's BOP storage
    inflow <- yesterday.df[1,3] # yesterday's inflow
    w <- yesterday.df[1,4] # yesterday's  withdrawal from intake in reservoir
    outflow <- yesterday.df[1,5] # yesterday's release over dam
# calculate today's BOP storage
    stor <- stor + inflow - w - outflow # today's bop storage
# calculate the minimum release over the dam
    rel_min <- if_else(flowby > ws_rel_req, flowby, ws_rel_req)
# calculate a new row, "today", of the res.ops.df:
    newrow.df <- subset(res@inflows, 
                        date_time == yesterday_date + 1) %>%
      mutate(stor = stor,
             inflow = inflows,
             w_req = withdr_req,
             # available for release over dam - high storage conditions
             available = stor + inflow - w_req,
             rel_req = ws_rel_req,
             outflow = case_when(
               # if excess water (over capacity) is greater 
               #    than that needed for release, release it all
               available - cap >= rel_min ~ available - cap, # spill
               # if there's enough water to meet the release request, do it
               available - cap < rel_min & available > rel_min 
               ~ rel_min,
               # if there's not enough water to meet the release request, 
               #    then release what's there
               available - cap < rel_min & available <= rel_min 
               ~ available,
               available - cap < rel_min & available < 0
               ~ 0),
             # So far it's been assumed withdrawal = withdr_req
             withdr_req = w_req, # output withdrawal request in the ts
             withdr = case_when(
               outflow >= flowby ~ w_req, 
               # but if outflow < flowby, reduce the withdrawal
               #   if you could have made the flowby by reducing the
               #   withdrawal, then reduce the withdrawal:
               outflow < flowby & available + w_req >= flowby
               ~ available + w_req - flowby,
               outflow < flowby & available + w_req < flowby
               ~ 0),
             # # if withdr turns out to be < withdr_min, set to 0?
             # #   since withdr_min assumes a hydraulic constraint
             # if(withdr < withdr_min) {withdr = 0},
             # Finally, readjust outflow if necessary
             outflow = case_when(
               outflow >= flowby ~ outflow,
               outflow < flowby ~ available + w_req - withdr),
             storage = stor
      ) %>%
      select(date_time, storage, inflow, withdr, outflow, 
             withdr_req, rel_req, available)
    #
    # add the new row, today, to res.ops.df:
    res.ts.df <- rbind(res.ts.df, newrow.df)
  #  ts$res <- rbind(res.ts.df, newrow.df) ?
  return(res.ts.df)
}