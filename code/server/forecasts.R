#------------------------------------------------------------------
#------------------------------------------------------------------
# Compute, or load, Potomac River flow forecasts
#------------------------------------------------------------------
#------------------------------------------------------------------
#
# Switch to "wide" format to make forecasts.
potomac.fc.df <- potomac.data.df %>%
  tidyr:: spread(key = "location", value = "flow_mgd", sep = NULL)
#
#--------------------------------------------------------------------------------
# Make the 9-day flow forecast, using our old empirical eq., also used in PRRISM
#--------------------------------------------------------------------------------
potomac.fc.df <- potomac.fc.df %>%
   dplyr:: mutate(lfalls_fc_9days = 288.79*exp(0.0009*lfalls_nat))
#
#--------------------------------------------------------------------------------
# Make the 1-day flow forecast, using the POR-delta method, also used in PRRISM
#--------------------------------------------------------------------------------
tot <- 2 # approximating time of travel from POR to LFalls as 2
potomac.fc.df <- potomac.fc.df %>%
  dplyr::mutate(lfalls_fc_1day = lfalls_nat +
  lag(por_nat, n = tot - 1) - lag(por_nat, n = tot)
  )

# Switch back to the long format
  potomac.fc.df <- potomac.fc.df %>%
   gather(key = "location", value = "flow_mgd", -date_time)
#
