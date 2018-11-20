warning_color_map_func <- function(number){
  case_when(
    number == 0 ~ map_green,
    number == 1 ~ map_yellow,
    number == 2 ~ map_orange,
    number == 3 ~ map_red,
    TRUE ~ map_black

    
  )}