#------------------------------------------------------------------
#------------------------------------------------------------------
# This script:
#   A) loads in basic reservoir data, ie capacity, inflow, etc.,
#   B) creates reservoir "objects" (R S4 class type objects) to hold this data,
#   C) initializes reservoir dataframes with daily time series
#       (calling reservoir_ops_init_func)
#------------------------------------------------------------------
#------------------------------------------------------------------
#
#------------------------------------------------------------------
# A. Create a dataframe of reservoir inflows
#------------------------------------------------------------------
# (This is not really necessary - maybe get rid of)
#
inflows.df <- flows.daily.mgd.df %>%
  dplyr::select(date_time, jrr_in, lsen_in, pat_in, occ_in) %>%
  dplyr::filter(date_time <= date_end,
                date_time >= date_start)
#
#------------------------------------------------------------------
# Load the basic reservoir data and inflow time series
#------------------------------------------------------------------
# Note - read for each res from /input/parameters/*.R
# res_cap <- 4000.0
# res_stor0 <- 3000.0
# res_flowby <- 10.0
# res_withdr_req0 <- 5.0
# res_withdr_max <- 2.0
# res_withdr_min <- 1.0
# res_ws_rel_req0 <- 3.0
#
sen.inflows.df <- inflows.df %>%
  select(date_time, inflows = lsen_in)
#
jrr.inflows.df <- inflows.df %>%
  select(date_time, inflows = jrr_in)
#
pat.inflows.df <- inflows.df %>%
  select(date_time, inflows = pat_in)
#
occ.inflows.df <- inflows.df %>%
  select(date_time, inflows = occ_in)
#
#------------------------------------------------------------------
# B. Create "reservoir" objects - in the reservoir class:
#------------------------------------------------------------------
sen <- new("Reservoir", name = "Little Seneca Reservoir", 
           capacity = sen_cap,
           withdr_max = sen_withdr_max,
           withdr_min = sen_withdr_max,
           rc = sen.rc.df,
           stor0 = sen_stor0,
           flowby = sen_flowby,
           inflows = sen.inflows.df)
#
jrr <- new("Reservoir", name = "Jennings Randolph Reservoir", 
           capacity = jrr_cap,
           withdr_max = jrr_withdr_max,
           withdr_min = jrr_withdr_max,
           rc = jrr.rc.df,
           stor0 = jrr_stor0,
           flowby = jrr_flowby,
           inflows = jrr.inflows.df)
#
pat <- new("Reservoir", name = "Patuxent reservoirs", 
           capacity = pat_cap,
           withdr_max = pat_withdr_max,
           withdr_min = pat_withdr_max,
           rc = pat.rc.df,
           stor0 = pat_stor0,
           flowby = pat_flowby,
           inflows = pat.inflows.df)
#
occ <- new("Reservoir", name = "Occoquan Reservoir", 
           capacity = occ_cap,
           withdr_max = occ_withdr_max,
           withdr_min = occ_withdr_max,
           rc = occ.rc.df,
           stor0 = occ_stor0,
           flowby = occ_flowby,
           inflows = occ.inflows.df)
#
#------------------------------------------------------------------
# C. Initialize dataframes that hold the reservoir time series (ts)
#------------------------------------------------------------------
sen.ts.df0 <- reservoir_ops_init_func(sen, sen_withdr_req0, sen_ws_rel_req0)
jrr.ts.df00 <- reservoir_ops_init_func(jrr, jrr_withdr_req0, jrr_ws_rel_req0)
jrr.ts.df0 <- jrr.ts.df00 %>%
  mutate(storage_ws = 13000,
         storage_wq = 8000, # in DREX we start in January
         ws_rel_req = 0,
         wq_rel_req = 120,
         outflow = 120, # adding this because of mysterious problem
         outflow_ws = 0,
         outflow_wq = 120) %>%
  select(date_time, storage, storage_ws, storage_wq,
         inflow, withdr, outflow, outflow_ws, outflow_wq,
         withdr_req, rel_req, ws_rel_req, wq_rel_req, available)
pat.ts.df0 <- reservoir_ops_init_func(pat, pat_withdr_req0, pat_ws_rel_req0)
occ.ts.df0 <- reservoir_ops_init_func(occ, occ_withdr_req0, occ_ws_rel_req0)
#
sen.ts.df <- sen.ts.df0
jrr.ts.df <- jrr.ts.df0
pat.ts.df <- pat.ts.df0
occ.ts.df <- occ.ts.df0