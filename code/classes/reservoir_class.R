#
# This defines an S4 Reservoir class, to allow creation of
#   Reservoir objects, which hold basic reservoir data
#   and the inflow time series.
#
# Later should add checks for the input info
#
setClass("Reservoir",
         slots = c(name = "character",
                   capacity = "numeric", # MG
                   withdr_max = "numeric", # MGD
                   withdr_min = "numeric", # MGD
                   rc = "data.frame", # the rule curves
                   # rc specifies storage thresholds, MG, 
                   #   and withdrawals, MGD
                   stor0 = "numeric", # MG
                   flowby = "numeric", # min envir. flowby over dam, MGD
                   inflows = "data.frame") # input file has MGD
)
