#------------------------------------------------------------------------------
# Info found online:
  # "global.R is a script that is executed before
  # the application launch. For this reason, it can include the same
  # pieces of code to execute the reactive independent processes,
  # but it also has an additional capability: the objects 
  # generated in global.R can be used both in server.R and UI.R"
#------------------------------------------------------------------------------
# The script, import_data.R, loads in all available time series data. 
#  Right now the directory with time series data (set by paths.R)
#  has 2 years of data, from 1929-10-01 to 1931-09-30.
# date_tsdata_start <- as.Date("1929-10-01")
# date_tsdata_end <- as.Date("1931-09-30")

source("code/global/load_packages.R", local = TRUE)
source("config/paths.R", local = TRUE)
source("input/parameters/parameters.R", local = TRUE)
source("code/global/import_data.R", local = TRUE)
#
#-----------------------------------------------------------------
# Block temporarily pasted into global.R - for ease of debugging
#-----------------------------------------------------------------
#
source("code/classes/reservoir_class.R", local = TRUE)
source("code/functions/reservoir_ops_init_func.R", local = TRUE)
source("code/functions/reservoir_ops_today_func.R", local = TRUE)
source("code/functions/jrr_reservoir_ops_today_func.R", local = TRUE)
source("code/functions/jrr_reservoir_ops_today_func2.R", local = TRUE)
source("code/functions/forecasts_demands_func.R", local = TRUE)
source("code/functions/forecasts_flows_func.R", local = TRUE)
source("code/functions/state_indices_update_func.R", local = TRUE)
source("code/functions/estimate_need_func.R", local = TRUE)
source("code/functions/restriction_flow_benefits_func.R", local = TRUE)
source("code/functions/sim_main_func.R", local = TRUE)
source("code/functions/simulation_func.R", local = TRUE)
source("code/functions/sim_add_days_func.R", local = TRUE)
source("code/functions/rule_curve_func.R", local = TRUE)
source("code/functions/nbr_rule_curve_func.R", local = TRUE)
source("code/functions/display_graph_res_func.R", local = TRUE)
#--------------------------------------------------------------------------------
#functions added by Luke
source("code/functions/date_func.R", local = TRUE)
source("code/functions/warning_color_func.R", local = TRUE)
source("code/functions/warning_color_map_func.R", local = TRUE)#this is a lazy Friday fix that should be changed later
#--------------------------------------------------------------------------------
# Make the reservoir objects and reservoir time series df's
#--------------------------------------------------------------------------------
source("code/server/reservoirs_make.R", local = TRUE) 
# What this does is create the reservoir "objects", jrr, sen, occ, pat
#    and the reservor time series, res.ts.df, e.g.:
# sen.ts.df - initialized with first day of ops time series
# ...
#--------------------------------------------------------------------------------
# Make the Potomac input data and flow time series dataframes
#--------------------------------------------------------------------------------
source("code/server/potomac_flows_init.R", local = TRUE)
# What this does is create:
# potomac.data.df - filled with all nat flow, trib flow data
# potomac.ts.df - initialized with first day of flows
#    - contains lfalls_obs, sen_outflow, jrr_outflow
#--------------------------------------------------------------------------------
# Make and initialize state drought status time series dataframes
#--------------------------------------------------------------------------------
source("code/server/state_status_ts_init.R", local = TRUE)
# What this does is create:
# state.ts.df - filled with status indices:
#    - 0 = Normal
#    - 1 = Watch
#    - 0 = Warning
#    - 0 = Emergency
#--------------------------------------------------------------------------------
# A few needed inputs which will probably be moved at some point
#--------------------------------------------------------------------------------
# 
lfalls_flowby <- 100 # change to read from parameter file!!!
mos_0day <- 40 # margin of safety for Patuxent load shift
# mos_1day <- 120 # margin of safety for Seneca release
mos_9day <- 0 # margin of safety for N Br release
#
# Don't want to calculate this every time step
#  - but probably should create script to do this sort of thing
upstr_stor_cap <- jrr_cap_cp*jrr_ws_frac + sen@capacity
#-----------------------------------------------------------------
#
#------------------------------------------------------------------------------
plot.height <- "340px"
plot.width <- "95%"
#------------------------------------------------------------------------------


#------------------------------------------------------------------
#colors for squares
green <- "background-color:#5CC33D"
yellow <- "background-color:yellow"
orange <- "background-color:orange"
red <- "background-color:red"
navy <- "background-color:navy"
black <- "background-color: black"

#colors for MD drought map
map_green <- "#5CC33D"
map_yellow <- "yellow"
map_orange <- "orange"
map_red <- "red"
map_black <- "black"

#read map shapefiles in
clipcentral = readOGR(dsn=map_path, layer = "clipcentral")
western_dslv = readOGR(dsn=map_path, layer = "western_dslv")
#transform map shapefiles
clipcentral_t <- spTransform(clipcentral, CRS("+init=epsg:4326"))
western_region_t <- spTransform(western_dslv, CRS("+init=epsg:4326"))

#-------------------------------------------------------------------
#paths to data for warning squares pulled from template verion
#these variables need to be changed to value outputs of 
#actual sim when it's up and running

# my_data_p <-fread(paste(ts_path, "va_shenandoah_p.csv", sep = ""))
# my_data_q = fread(paste(ts_path,"va_shenandoah_q.csv", sep = ""))
# my_data_s = fread(paste(ts_path,"va_shenandoah_stor.csv", sep = ""))
# my_data_g = fread(paste(ts_path,"va_shenandoah_gw.csv", sep = ""))



