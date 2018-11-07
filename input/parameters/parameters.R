#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# INPUT PARAMETERS
# THIS FILE IS SPECIFIC TO DREX 2018 - WITH 2039 SCENARIO VALUES
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Scenario
#--------------------------------------------------------------------------------
# date_start <- as.Date("1930-01-01")
# date_end <- as.Date("1931-09-30")
# date_today0 <- as.Date("1930-05-01")
date_start <- as.Date("2038-01-01")
date_end <- as.Date("2039-09-30")
date_today0 <- as.Date("2038-08-01")
#
#--------------------------------------------------------------------------------
# Universal conversion factors
#--------------------------------------------------------------------------------
mgd_to_cfs <- 1.547
#
#--------------------------------------------------------------------------------
# Miscellaneous values - some are placeholders to be refined later
#--------------------------------------------------------------------------------
#
month_sim <- c(1:12)
#
sen_other_watershed_flows <- 30 # wwtp and other land area watershed flows, MGD
#
# Upstream total consumptive use, MGD, 2039 scenario year
cu_2039 <- c(25.4, 25.8, 25.4, 28.3, 36.9, 48.0,
             50.7, 51.1, 43.1, 33.0, 27.8, 26.1)
cu.df <- data.frame(month_sim, cu_2039) %>%
  mutate(cu = cu_2039) %>%
  select(month_sim, cu)
#
# Upstream WWTP discharges, ie Seneca, Damascus, & Broad Run
wwtp_2039 <- c(32.6, 33.5, 34.3, 32.4, 33.5, 31.9,
               30.3, 30.4, 30.6, 32.5, 3.7, 32.3)
# Net upstream change in flow
dflow.upstr.df <- data.frame(month_sim, cu_2039, wwtp_2039) %>%
  mutate(dflow = wwtp_2039 - cu_2039) %>%
  select(month_sim, dflow)
  
#
#--------------------------------------------------------------------------------
# Reservoir data and rule curves
#--------------------------------------------------------------------------------
# Maybe eventually rule curves should be read from csv files
#
# Little Seneca and Jennings Randolph have no water supply intake/withdrawals, 
#   so these are dummy values
#--------------------------------------------------------------------------------
# Little Seneca Reservoir
#
sen_cap <- 3787.0
sen_stor0 <- 3987.0
sen_flowby <- 1.12
sen_withdr_req0 <- 5.0
sen_withdr_max <- 2.0
sen_withdr_min <- 1.0
sen_ws_rel_req0 <- 3.0
#
sen.rc.df <- data.frame(month_sim) %>%
  dplyr::mutate(stor1 = 500,
                stor2 = 1000,
                stor3 = 1500,
                withdr1 = 0,
                withdr2 = 0,
                withdr3 = 0)
#
#--------------------------------------------------------------------------------
# Jennings Randolph Reservoir
#
jrr_cap <- 28823.0
jrr_cap_cp <- 28223.0
jrr_stor0 <- 19795.0 # starting Jan 1 for DREX
jrr_flowby <- 77.6
jrr_withdr_req0 <- 0.0
jrr_withdr_max <- 5818
jrr_withdr_min <- 0.0
jrr_ws_rel_req0 <- 300.0
#
jrr_ws_frac <- 0.4456
jrr_wq_frac <- 0.5544
#
# jrr.rc.df <- sen.rc.df
#
# Note these storages assume the scenario year is 2039!
stor1 <- c(16379, 16379, 19795, 26466, 26466, 26466,
           25047, 23667, 22327, 21035, 18604, 16379)
stor2 <- c(19795, 19795, 22327, 28223, 28223, 28223,
           28223, 28223, 28223, 25047, 22327, 19795)
stor3 <- c(26466, 26466, 26466, 28823, 28823, 28823,
           28823, 28823, 28823, 27926, 27045, 26466)
withdr2 <- c(300, 300, 300, 400, 300, 300,
             250, 200, 150, 200, 300, 300)
jrr.rc.df <- data.frame(month_sim, stor1, stor2, stor3, withdr2) %>%
  dplyr::mutate(withdr1 = 77.6, withdr3 = 970)
#--------------------------------------------------------------------------------
# Occoquan Reservoir
#
occ_cap <- 7479.0
occ_stor0 <- 7470.0
occ_flowby <- 0.0
occ_withdr_req0 <- 70.0
occ_withdr_max <- 120.0
occ_withdr_min <- 45.0
occ_ws_rel_req0 <- 0.0
#
occ_fixed_ls <- 15 # load-shift amount, mgd
occ_stor_emerg <- 1000 # emergency storage, below which there are no load-shifts
#
# uosa discharge in 2039, to be added to occ inflows in import_data.R
uosa_discharge <- c(36.3, 40.9, 42.5, 38.0, 39.5, 37.8,
                36.2, 36.4, 35.2, 36.7, 37.0, 39.1)
uosa.out.df <- data.frame(month_sim, uosa_discharge)
#
# Reduction of Occoquan inflows because of Lake Manassas
#  - just assuming a constant reduction (MGD)
#    (averaging PRRISM results)
lake_manassas_reduction <- 14
#
# uosa incremental discharge above 2000 value in 2039:
uosa_incr <- c(14.4, 16.2, 16.8, 15.0, 15.7, 15.0,
                      14.3, 14.4, 13.9, 14.5, 14.7, 15.5)
# For Occoquan I'm skipping the middle RC
stor1 <- c(1000, 1000, 1000, 1000, 1000, 1000,
           1000, 1000, 1000, 1000, 1000, 1000)
stor2 <- c(3000, 3000, 3000, 4500, 6000, 4000,
           3750, 3500, 3250, 3000, 3000, 3000)
stor3 <- c(3000, 3000, 3000, 4500, 6000, 7500,
           6750, 6000, 5250, 4500, 3750, 3000)

occ.rc.df <- data.frame(month_sim, stor1, stor2, stor3, uosa_incr) %>%
  dplyr::mutate(withdr1 = 45, withdr2 = 50 + uosa_incr, withdr3 = 70 + uosa_incr)
#--------------------------------------------------------------------------------
# Patuxent reservoirs
#
pat_cap <- 9725.0
pat_stor0 <- 9725.0
pat_flowby <- 10.3 # still bugs in res_ops_today - NA's when this was 120
pat_withdr_req0 <- 40.0
pat_withdr_max <- 65.0
pat_withdr_min <- 1.0
pat_ws_rel_req0 <- 0.0
#
stor1 <- c(1000.0, 1000.0, 1000.0, 1000.0, 1000.0, 1000.0,
           1000.0, 1000.0, 1000.0, 1000.0, 1000.0, 1000.0)
stor2 <- c(3406.0, 3915.0, 5299.0, 6808.0, 8195.0, 5716.0,
           5618.0, 5145.0, 4569.0, 3997.0, 3441.0, 3072.0)
stor3 <- c(4406.0, 4915.0, 6299.0, 7808.0, 9195.0, 6716.0,
           6818.0, 6145.0, 5569.0, 4997.0, 4441.0, 4072.0)
pat.rc.df <- data.frame(month_sim, stor1, stor2, stor3) %>%
  dplyr::mutate(withdr1 = 20, withdr2 = 34, withdr3 = 40)