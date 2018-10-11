date_func <- function(col_date, col_val, date_today ){
  our_value <- 0
  for(i in col_date){
    if(as.character(i) == date_today){our_value <- (col_val[which(col_date == i)])}
  }
  return(our_value)}