groundwell_warn_func <- eventReactive(index, percent(){
  case_when(
    #percent() <= .0 ~ "background-color:purple", #"#000000",
    percent() > .25  ~ green,#"background-color:red", #"#cc3300",
    percent() > .10 && percent() <= .25 ~ yellow,#"background-color:orange",  #"#ff9966",
    percent() > .05 && percent() <= .10 ~ orange,#"background-color:yellow",  #"#ffcc00",
    percent() <= .05 ~ red,#"background-color:green", #"#99cc33",
    #percent() > .80 && percent() < 1 ~  navy, #"background-color:navy" #"#339900"
    TRUE ~ black
  )
})