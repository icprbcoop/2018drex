#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# INPUT PARAMETERS
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Scenario
#--------------------------------------------------------------------------------
date_start <- as.Date("1930-01-01")
date_end <- as.Date("1930-09-30")
date_today <- as.Date("1930-05-01")
# date_start <- as.Date("2039-01-01")
# date_end <- as.Date("2039-09-30")
# date_today <- as.Date("2039-05-01")
#
#--------------------------------------------------------------------------------
# Universal conversion factors
#--------------------------------------------------------------------------------
mgd_to_cfs <- 1.547
#
#--------------------------------------------------------------------------------
# Reservoir rule curves
#--------------------------------------------------------------------------------
# Maybe eventually these should be read from csv files
#
month_sim <- c(1:12)
# Little Seneca and Jennings Randolph have no water supply intake/withdrawals, 
#   so these are dummy values
#--------------------------------------------------------------------------------
# Little Seneca Reservoir
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
jrr.rc.df <- sen.rc.df
#
#--------------------------------------------------------------------------------
# Occoquan Reservoir
#
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
stor1 <- c(1000, 1000, 1000, 1000, 1000, 1000,
           1000, 1000, 1000, 1000, 1000, 1000)
stor2 <- c(3406, 3915, 5299, 6808, 8195, 5716,
           5618, 5145, 4569, 3997, 3441, 3072)
stor3 <- c(4406, 4915, 6299, 7808, 9195, 6716,
           6818, 6145, 5569, 4997, 4441, 4072)
pat.rc.df <- data.frame(month_sim, stor1, stor2, stor3) %>%
  dplyr::mutate(withdr1 = 20, withdr2 = 34, withdr3 = 40)