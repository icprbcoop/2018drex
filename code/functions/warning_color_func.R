warning_color_func <- function(number){
  case_when(
    number == 0 ~ green,
    number == 1 ~ yellow,
    number == 2 ~ orange,
    number == 3 ~ red,
    TRUE ~ black
    
  )}