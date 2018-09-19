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
# prod_max - max production rate for plant associated with res, MGD
#    - get this from the res object
#
#--------------------------------------------------------------------------------
# Output
#--------------------------------------------------------------------------------
# withdr_req - the water supply withdrawal request from the res, MGD
#--------------------------------------------------------------------------------
#
rule_curve_func <- function(month_sim_in, stor, res){
  res.rc.df <- res@rc
  prod_max <- res@prod_max
  rc <- subset(res.rc.df, month_sim == month_sim_in)
  if(stor > rc$stor3) {withdr_req <- prod_max}
  if(stor <= rc$stor3) {withdr_req <- rc$withdr3}
  if(stor <= rc$stor2) {withdr_req <- rc$withdr2}
  if(stor <= rc$stor1) {withdr_req <- rc$withdr1}
  # if(stor > 9000) {withdr_req <- prod_max}
  # if(stor <= 9000) {withdr_req <- 40}
  # if(stor <= 4500) {withdr_req <- rc$withdr2}
  # if(stor <= 1000) {withdr_req <- rc$withdr1}
  #
  return(withdr_req)
}