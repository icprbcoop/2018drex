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
rule_curve_func <- function(month_sim_in, stor, res){
  res.rc.df <- res@rc
  withdr_max <- res@withdr_max
  rc <- subset(res.rc.df, month_sim == month_sim_in)
  # print(paste("stor is ", stor))
  # print(rc)
  # print(" ")
  if(stor > rc$stor3) {withdr_req <- withdr_max}
  if(stor <= rc$stor3) {withdr_req <- rc$withdr3}
  if(stor <= rc$stor2) {withdr_req <- rc$withdr2}
  if(stor <= rc$stor1) {withdr_req <- rc$withdr1}
  #
  return(withdr_req)
}