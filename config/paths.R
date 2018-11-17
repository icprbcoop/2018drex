# Paths
#
# Time series scenario
# Right now just using 2-year test dataset
#   beginning Oct-01 and ending two years later on Sep-30
# This is our test data - PRRISM estimates of flows in drought of record
 # ts_path <- "input/ts/1929_1929prrism/" # 1929-10-01 thru 1931-09-30
#
# Here are 2018drex data - from PRRISM & from CBP climate change runs
#   with daily variation from 1998-10-01 to 2000-09-30
# ts_path <- "input/ts/2038_1998prrism/" 
ts_path <- "input/ts/2038_1998_p532_i_a1b/"
# ts_path <- "input/ts/2038_1998_p532_i_b1/" 
#
ts_output <- "output/" # path of output directory

map_path <- "data/Drought_Regions" #MD shapefiles