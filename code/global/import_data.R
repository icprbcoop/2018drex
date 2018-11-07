#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# This script imports time series inputs (ts).
# The path to the time series is defined by /config/paths.R
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Import river flow and reservoir inflow ts:
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Make adjustments to Occoquan inflows, and river flows
#  - Occoquan inflows are increased by UOSA discharge,
#     and decreased by Lake Manassas operations
#  - River flow at POR and LFalls are decreased by upstr CU
#     and increased by WWTP discharges, ie dflow = wwtp - cu
#     (see parameters.R for values)
flows.daily.mgd.df0 <- data.table::fread(paste(ts_path, "flows_daily_mgd.csv", sep = ""),
                                      data.table = FALSE) %>%
  dplyr::mutate(date_time = as.Date(date),
                month_sim = month(date_time),
                occ_in0 = occ) %>%
  left_join(uosa.out.df, by = "month_sim", copy = FALSE)
flows.daily.mgd.df <- flows.daily.mgd.df0 %>%
  mutate(occ_in = occ_in0 + uosa_discharge -
           lake_manassas_reduction) %>%
  left_join(dflow.upstr.df, by = "month_sim", copy = FALSE) %>%
  mutate(lfalls_nat = lfalls_nat + dflow,
         por_nat = por_nat + dflow) %>%

  #
  #------------------------------------------------------------------------------
  # Temporarily increase jrr inflow by 20% to approximately simulate Savage
  #------------------------------------------------------------------------------
  # dplyr::mutate(jrr = jrr*1.2) %>%
  #----------------------------------------------------------------------------
  dplyr::select(sim_day, date_time, month_sim,
                jrr_in = jrr,
                lsen_in = lsen,
                pat_in = pat,
                occ_in0,
                occ_in,
                uosa_discharge,
                dflow,
                por_nat, below_por, lfalls_nat
)
#
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Import a time series of total system demands.
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#
# (Later we could create a stochastic demand model like in PRRISM.)
demands.daily.df <- data.table::fread(paste(ts_path, "demands_daily.csv", sep = ""),
                                       data.table = FALSE) %>%
  dplyr::mutate(date_time = as.Date(date)) %>%
  dplyr::select(date_time, d_fw_e, d_fw_w, d_fw_c, d_lw,
                d_wa, d_wssc, d_total)
# Import the rule curve tables

                                        