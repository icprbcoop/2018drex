#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# INPUT PARAMETERS
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Scenario
#--------------------------------------------------------------------------------
date_start <- as.Date("1930-01-01")
date_end <- as.Date("1931-09-30")
date_today0 <- as.Date("1930-05-01")
# date_start <- as.Date("2038-01-01")
# date_end <- as.Date("2039-09-30")
# date_today0 <- as.Date("2039-05-01")
#
#--------------------------------------------------------------------------------
# Universal conversion factors
#--------------------------------------------------------------------------------
mgd_to_cfs <- 1.547
#
#--------------------------------------------------------------------------------
# Miscellaneous values - some are placeholders to be refined later
#--------------------------------------------------------------------------------
sen_other_watershed_flows <- 30 # wwtp and other land area watershed flows, MGD
#
#--------------------------------------------------------------------------------
# Reservoir data and rule curves
#--------------------------------------------------------------------------------
# Maybe eventually rule curves should be read from csv files
#
month_sim <- c(1:12)
# Little Seneca and Jennings Randolph have no water supply intake/withdrawals, 
#   so these are dummy values
#--------------------------------------------------------------------------------
# Little Seneca Reservoir
#
sen_cap <- 4000.0
sen_stor0 <- 3000.0
sen_flowby <- 10.0
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
jrr_stor0 <- 28223.0
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
occ_cap <- 8000.0
occ_stor0 <- 8000.0
occ_flowby <- 0.0
occ_withdr_req0 <- 70.0
occ_withdr_max <- 120.0
occ_withdr_min <- 45.0
occ_ws_rel_req0 <- 0.0
#
occ_fixed_ls <- 15 # load-shift amount, mgd
occ_stor_emerg <- 1000 # emergency storage, below which there are no load-shifts
# In PRRISM the UOSA wwtp discharge, above the 2000 level, is added to RC withdrawals
#    Below I'm neglecting monthly factors and just using annual averages, MGD
uosa2000 <- 25.1
uosa2020 <- 35.8
uosa2025 <- 37.8
uosa_added <- uosa2025 - uosa2000
# For Occoquan I'm skipping the middle RC
stor1 <- c(1000, 1000, 1000, 1000, 1000, 1000,
           1000, 1000, 1000, 1000, 1000, 1000)
stor2 <- c(3000, 3000, 3000, 4500, 6000, 4000,
           3750, 3500, 3250, 3000, 3000, 3000)
stor3 <- c(3000, 3000, 3000, 4500, 6000, 7500,
           6750, 6000, 5250, 4500, 3750, 3000)
occ.rc.df <- data.frame(month_sim, stor1, stor2, stor3) %>%
  dplyr::mutate(withdr1 = 48, withdr2 = 50 + uosa_added, withdr3 = 70 + uosa_added)
#--------------------------------------------------------------------------------
# Patuxent reservoirs
#
pat_cap <- 10000.0
pat_stor0 <- 10000.0
pat_flowby <- 10.0 # still bugs in res_ops_today - NA's when this was 120
pat_withdr_req0 <- 40.0
pat_withdr_max <- 65.0
pat_withdr_min <- 1.0
pat_ws_rel_req0 <- 0.0
#
stor1 <- c(1000, 1000, 1000, 1000, 1000, 1000,
           1000, 1000, 1000, 1000, 1000, 1000)
stor2 <- c(3406, 3915, 5299, 6808, 8195, 5716,
           5618, 5145, 4569, 3997, 3441, 3072)
stor3 <- c(4406, 4915, 6299, 7808, 9195, 6716,
           6818, 6145, 5569, 4997, 4441, 4072)
pat.rc.df <- data.frame(month_sim, stor1, stor2, stor3) %>%
  dplyr::mutate(withdr1 = 20, withdr2 = 34, withdr3 = 40)