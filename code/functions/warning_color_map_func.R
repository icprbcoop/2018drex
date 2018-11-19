warning_color_map_func <- function(number){
  case_when(
    number == 0 ~ map_green,
    number == 1 ~ map_yellow,
    number == 2 ~ map_orange,
<<<<<<< HEAD
    number == 3 ~ map_red
=======
    number == 3 ~ map_red,
    TRUE ~ map_black
>>>>>>> cf58871baa404880688b4ad9d3bacfe2448a8415
    
  )}