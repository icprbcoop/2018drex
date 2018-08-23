#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# This script imports time series inputs (ts).
# The path to the time series is defined by /config/paths.R
#   and is currently input/ts/daily_test - just 2 years of data.
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#
# Import river flow and reservoir inflow ts:
flows.daily.mgd.df <- data.table::fread(paste(ts_path, "flows_daily_mgd.csv", sep = ""),
                                      data.table = FALSE) %>%
  dplyr::mutate(date_time = as.Date(date)) %>%
  dplyr::select(sim_day, date_time,
                jrr_in = jrr,
                lsen_in = lsen,
                por_nat, below_por, lfalls_nat)
#
# Import a time series of total system demands.
# (Later we could create a stochastic demand model like in PRRISM.)
demands.daily.df <- data.table::fread(paste(ts_path, "demands_daily.csv", sep = ""),
                                       data.table = FALSE) %>%
  dplyr::mutate(date_time = as.Date(date),
                demands_total_unrestricted = sys_demands)

                                        