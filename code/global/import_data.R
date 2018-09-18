#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# This script imports time series inputs (ts).
# The path to the time series is defined by /config/paths.R
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
                pat_in = pat,
                occ_in = occ,
                por_nat, below_por, lfalls_nat)
#
# Import a time series of total system demands.
# (Later we could create a stochastic demand model like in PRRISM.)
demands.daily.df <- data.table::fread(paste(ts_path, "demands_daily.csv", sep = ""),
                                       data.table = FALSE) %>%
  dplyr::mutate(date_time = as.Date(date)) %>%
  dplyr::select(date_time, d_fw_e, d_fw_w, d_fw_c, d_lw,
                d_wa, d_wssc, d_total)
# Import the rule curve tables

                                        