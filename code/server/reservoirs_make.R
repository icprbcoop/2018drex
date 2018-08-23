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
  dplyr::select(date_time, jrr_in, lsen_in) %>%
  dplyr::filter(date_time <= date_end,
                date_time >= date_start)
#
#------------------------------------------------------------------
# Load the basic reservoir data and inflow time series
#------------------------------------------------------------------
# Later want to read this from /input/parameters/*.R
sen_cap <- 4000
sen_stor0 <- 3000
sen_flowby <- 3
sen_withdr_req0 <- 10
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
#------------------------------------------------------------------
# Create "reservoir" objects - in the reservoir class:
#------------------------------------------------------------------
sen <- new("Reservoir", name = "Little Seneca Reservoir", 
           capacity = sen_cap,
           stor0 = sen_stor0,
           flowby = sen_flowby,
#           withdr_req = sen_withdr_req,
           inflows = sen.inflows.df)
jrr <- new("Reservoir", name = "Jennings Randolph Reservoir", 
           capacity = jrr_cap,
           stor0 = jrr_stor0,
           flowby = jrr_flowby,
#           withdr_req = jrr_withdr_req,
           inflows = jrr.inflows.df)
#
#------------------------------------------------------------------
# Initialize dataframes that hold the reservoir time series (ts)
#------------------------------------------------------------------
sen.ts.df0 <- reservoir_ops_init_func(sen, sen_withdr_req0, sen_ws_rel_req0)
jrr.ts.df0 <- reservoir_ops_init_func(jrr, jrr_withdr_req0, jrr_ws_rel_req0)
sen.ts.df <- sen.ts.df0
jrr.ts.df <- jrr.ts.df0
