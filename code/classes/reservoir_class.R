#
# This defines an S4 Reservoir class, to allow creation of
#   Reservoir objects, which hold basic reservoir data
#   and the inflow time series.
#
setClass("Reservoir",
         slots = c(name = "character",
                   capacity = "numeric", # MG
                   prod_max = "numeric", # MGD
                   rc = "data.frame", # the rule curves
                   # rc specifies storage thresholds, MG, 
                   #   and withdrawals, MGD
                   stor0 = "numeric", # MG
                   flowby = "numeric", # min envir. flowby over dam, MGD
#                   withdr_req = "numeric", # withdr request from intake in reservoir
#                   rel_req = "numeric", # requested release over dam
                   inflows = "data.frame") # input file has MGD
)
