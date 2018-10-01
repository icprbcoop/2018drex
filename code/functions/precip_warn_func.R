precip_warn_func <- eventReactive(index, percent(){
  case_when(
    percent() <= .0 ~ "background-color:purple", #"#000000",
    percent() > .0 && percent() <= .20 ~ green,#"background-color:red", #"#cc3300",
    percent() > .20 && percent() <= .40 ~ yellow,#"background-color:orange",  #"#ff9966",
    percent() > .40 && percent() <= .60 ~ orange,#"background-color:yellow",  #"#ffcc00",
    percent() > .60 && percent() <= .80 ~ red,#"background-color:green", #"#99cc33",
    percent() > .80 && percent() < 1 ~  navy, #"background-color:navy" #"#339900"
    TRUE ~ black
  )
})

#input$data_index
#p_data_percent()