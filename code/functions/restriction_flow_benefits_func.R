#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Estimate Potomac River flow increases due to upstream
#   water use restrictions
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Inputs
#--------------------------------------------------------------------------------
# dr_va - demand reduction due to VA upstream (Shenandoah) use restrictions
# dr_md_cent - demand reduction due to MD Central upstr use restrictions
# dr_md_west - demand reduction due to MD Western upstr use restrictions
#
#--------------------------------------------------------------------------------
# Output
#--------------------------------------------------------------------------------
# dQ - a list containing dQ_va and dQ_md
#    - expected increases in Potomac River flow, MGD
#--------------------------------------------------------------------------------
#
restriction_flow_benefits_func <- function(dr_va, 
                                           dr_md_cent, 
                                           dr_md_west){
  d_va <- 50.0 # ave annual Shenandoah demand subject to restrictions
  d_md_cent <- 28.0 # ave annual MD-Central demand subject to restrictions
  d_md_west <- 47.0 # ave annual MD-Western demand subject to restrictions
  dQ_va <- d_va*dr_va/100.0
  dQ_md <- d_md_cent*dr_md_cent/100.0 + d_md_west*dr_md_west/100.0
  dQ <- list(dQ_va, dQ_md)
  return(dQ)
}