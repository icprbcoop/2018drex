#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Estimate Potomac River flow increases due to upstream
#   water use restrictions
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Inputs
#--------------------------------------------------------------------------------
# dr_va - demand reduction due to VA upstream use restrictions
# dr_md - demand reduction due to MD upstream use restrictions
#
#--------------------------------------------------------------------------------
# Output
#--------------------------------------------------------------------------------
# dQ - a list containing dQ_va and dQ_md
#    - expected increases in Potomac River flow, MGD
#--------------------------------------------------------------------------------
#
restriction_flow_benefits_func <- function(dr_va, dr_md){
  d_va <- 100.0
  d_md <- 100.0
  dQ_va <- d_va*dr_va/100.0
  dQ_md <- d_md*dr_md/100.0
  dQ <- list(dQ_va, dQ_md)
  return(dQ)
}