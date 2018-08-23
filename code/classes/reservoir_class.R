#
# This defines an S4 Reservoir class, to allow creation of
#   Reservoir objects, which hold basic reservoir data
#   and the inflow time series.
#
setClass("Reservoir",
         slots = c(name = "character",
                   capacity = "numeric",
                   stor0 = "numeric",
                   flowby = "numeric", # minimum environmental flowby over dam
#                   withdr_req = "numeric", # withdr request from intake in reservoir
#                   rel_req = "numeric", # requested release over dam
                   inflows = "data.frame") #,
         # prototype creates default values:
         # prototype = list(name = "unknown",
         #                  capacity = c(0.0),
         #                  stor0 =  c(0.0),
         #                  withdr_req = c(0.0)
         #)
)
