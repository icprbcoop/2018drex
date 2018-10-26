#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Determine water supply withdrawal request, according to "rule curves"
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Inputs
#--------------------------------------------------------------------------------
# date_sim - will be used to calculate month_sim
# stor - reservoir beginning of period storage, MG
# rc.df - data frame containing the rule curves 
#    - get this from the res object
# withdr_max - max withdrawal from res, MGD
# withdr_min - min withdrawal from res, MGD
#    - get these from the res object
#
#--------------------------------------------------------------------------------
# Output
#--------------------------------------------------------------------------------
# withdr_req - the water supply withdrawal request from the res, MGD
#--------------------------------------------------------------------------------
#
nbr_rule_curve_func <- function(month_sim_in, stor, res){
  res.rc.df <- res@rc
  rel_max <- res@withdr_max
  rc <- subset(res.rc.df, month_sim == month_sim_in)
  if(stor > rc$stor3) {rel_req <- rel_max}
  if(stor <= rc$stor3) {rel_req <- rc$withdr3}
  if(stor <= rc$stor2) {rel_req <- rc$withdr2}
  if(stor <= rc$stor1) {rel_req <- rc$withdr1}
  #
  return(rel_req)
}