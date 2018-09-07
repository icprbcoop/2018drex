#------------------------------------------------------------------
#------------------------------------------------------------------
# This script:
#   a) loads in basic reservoir data, ie capacity, inflow, etc.,
#   b) creates reservoir "objects" (R S4 class type objects) to hold this data,
#   c) initializes reservoir dataframes with daily time series
#       (calling reservoir_ops_init_func)
#------------------------------------------------------------------
#------------------------------------------------------------------
#
#------------------------------------------------------------------
# Create a dataframe of reservoir inflows
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
# Later want to read this from /input/parameters/*.R
sen_cap <- 4000
sen_stor0 <- 3000
sen_flowby <- 10
sen_withdr_req0 <- 5
sen_ws_rel_req0 <- 3
sen.inflows.df <- inflows.df %>%
  select(date_time, inflows = lsen_in)
#
jrr_cap <- 16000
jrr_stor0 <- 15000
jrr_flowby <- 120
jrr_withdr_req0 <- 120
jrr_ws_rel_req0 <- 300
jrr.inflows.df <- inflows.df %>%
  select(date_time, inflows = jrr_in)
#
pat_cap <- 10000
pat_stor0 <- 10000
pat_flowby <- 120
pat_withdr_req0 <- 50
pat_ws_rel_req0 <- 0
pat.inflows.df <- inflows.df %>%
  select(date_time, inflows = pat_in)
#
occ_cap <- 8000
occ_stor0 <- 8000
occ_flowby <- 0
occ_withdr_req0 <- 70
occ_ws_rel_req0 <- 0
occ.inflows.df <- inflows.df %>%
  select(date_time, inflows = occ_in)
#
#------------------------------------------------------------------
# Create "reservoir" objects - in the reservoir class:
#------------------------------------------------------------------
sen <- new("Reservoir", name = "Little Seneca Reservoir", 
           capacity = sen_cap,
           stor0 = sen_stor0,
           flowby = sen_flowby,
           inflows = sen.inflows.df)
#
jrr <- new("Reservoir", name = "Jennings Randolph Reservoir", 
           capacity = jrr_cap,
           stor0 = jrr_stor0,
           flowby = jrr_flowby,
           inflows = jrr.inflows.df)
#
pat <- new("Reservoir", name = "Patuxent reservoirs", 
           capacity = pat_cap,
           stor0 = pat_stor0,
           flowby = pat_flowby,
           inflows = pat.inflows.df)
#
occ <- new("Reservoir", name = "Occoquan Reservoir", 
           capacity = occ_cap,
           stor0 = occ_stor0,
           flowby = occ_flowby,
           inflows = occ.inflows.df)
#
#------------------------------------------------------------------
# Initialize dataframes that hold the reservoir time series (ts)
#------------------------------------------------------------------
sen.ts.df0 <- reservoir_ops_init_func(sen, sen_withdr_req0, sen_ws_rel_req0)
jrr.ts.df0 <- reservoir_ops_init_func(jrr, jrr_withdr_req0, jrr_ws_rel_req0)
pat.ts.df0 <- reservoir_ops_init_func(pat, pat_withdr_req0, pat_ws_rel_req0)
occ.ts.df0 <- reservoir_ops_init_func(occ, occ_withdr_req0, occ_ws_rel_req0)
#
sen.ts.df <- sen.ts.df0
jrr.ts.df <- jrr.ts.df0
pat.ts.df <- pat.ts.df0
occ.ts.df <- occ.ts.df0