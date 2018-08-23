#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Adds/updates the reservoir time series (ts) dfs with today's values
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Inputs
#--------------------------------------------------------------------------------
# date_sim = current date in the simulation
# res = reservoir name (sen or jrr)
# res.ts.df = reservoir ts (sen.ts.df or jrr.ts.df)
# withdr_req = water supply withdrawal request (= 0 for sen & jrr)
# ws_rel_req = release request for water supply purposes
#     (release to stream below)
#--------------------------------------------------------------------------------
# Outputs
#--------------------------------------------------------------------------------
# res.ts.df (= sen.ts.df or jrr.ts.df, with one new row - today)
#--------------------------------------------------------------------------------
# res.ts.df has the following columns:
#    date_time
#    storage
#    inflow
#    outflow
#    withdr_req - water supply withdrawal request (from intake)
#    rel_req - water supply release request (downstream discharge)
#    available
#
reservoir_ops_today_func <- function(date_sim, res, res.ts.df, 
                                     withdr_req, ws_rel_req){
  #
  # Implement the water balance eq. for s = beginning of period (BOP) storage:
  #   s(i+1) = s(i) + inflow(i) - w(i)
  #   taking into account constraints:
  #       0 <= s <= cap
  #       w(i) = withdr_req(i), or = s + inflow if not enough water
  #       spill(i) = excess water, over capacity
  # (Note the loop will write 1 extra row at end with date_time = <NA>)
    cap <- res@capacity # see reservoir_make.R for basic res data
    flowby <- res@flowby
    w_req <- withdr_req
    rel_req <- ws_rel_req
    # Trim the res.ts.df to make sure the last row is yesterday
        res.ts.df <- data.frame(res.ts.df) %>%
      dplyr::filter(date_time < date_sim)
    # Get the last row of the res.ops.df, "yesterday", 
    # and read its values:
    yesterday.df <- tail(res.ts.df,1)
    yesterday_date <- yesterday.df[1,1] # yesterday's date
    stor <- yesterday.df[1,2] # yesterday's storage
    inflow <- yesterday.df[1,3] # yesterday's inflow
    outflow <- yesterday.df[1,4] # yesterday's release over dam
    w <- yesterday.df[1,5] # yesterday's  withdrawal from intake in reservoir
#    rel_req <- yesterday.df[1,6] but this is yesterday's
# calculate today's BOP storage
    stor <- stor + inflow - outflow - w # today's bop storage
    rel_min <- if_else(flowby > rel_req, flowby, rel_req)
# calculate a new row, "today", of the res.ops.df:
    newrow.df <- subset(res@inflows, 
                        date_time == yesterday_date + 1) %>%
      mutate(stor = stor,
             inflow = inflows,
             available = stor + inflow - w_req,
             rel_req = rel_req,
             # rel_min = case_when(flowby > ws_rel_req ~ flowby,
             #                     flowby <= ws_rel_req ~ ws_rel_req),
             outflow = case_when(
               cap - available <= -rel_min ~ available - cap, # spill
               cap - available > -rel_min & available > rel_min ~ rel_min,
               cap - available > -rel_min & available <= rel_min ~ available),
             withdr_req = case_when(stor + inflow >= w_req ~ w_req,
                           stor + inflow < w_req ~ stor + inflow),
             storage = stor
      ) %>%
      select(date_time, storage, inflow, outflow, 
             withdr_req, rel_req, available)
    # add the new row, today, to res.ops.df:
    
    res.ts.df <- rbind(res.ts.df, newrow.df)
  return(res.ts.df)
}