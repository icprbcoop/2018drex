#******************************************************************
# server.R defines reactive values & uses observeEvent to do simulation,
# then produces output
#******************************************************************
shinyServer(function(input, output, session) {
  #----------------------------------------------------------------
  #----------------------------------------------------------------
  # First, do the simulation
  #----------------------------------------------------------------
  #----------------------------------------------------------------
  # Run the main simulation to the hard-coded input, date_today
  #    - ts here is the precursor of the set of reactive values
  ts0 <- list(sen = sen.ts.df0, 
              jrr = jrr.ts.df0, 
              pat = pat.ts.df0,
              occ = occ.ts.df0,
              flows = potomac.ts.df0)
  ts <- sim_main_func(date_today, ts0)
  #
  # Now make ts reactive, initializing to results from above
  ts <- reactiveValues(flows = ts$flows, 
                       sen = ts$sen, 
                       jrr = ts$jrr,
                       pat = ts$pat,
                       occ = ts$occ)
  #
  # Allow the user to re-run the simulation 
  #   - say, if end date (aka DREXtoday) changes
  observeEvent(input$run_main, {
    test_date$test_date_value <- input$DREXtoday
    ts <- sim_main_func(input$DREXtoday, ts)
  })
  #
  # Allow the user to add chunks of days to the simulation
  observeEvent(input$run_add, {
    test_date$test_date_value <- as.Date(input$DREXtoday) +input$chunkofdays
    ts <- sim_add_days_func(input$chunkofdays, ts)
  })
  
  #cretes a reactiveValue to hold the current date
  test_date <- reactiveValues(test_date_value = "1930-05-01")
  #
  #
  # Allow the user to write simulation output time series to files
  observeEvent(input$write_ts, {
    write.csv(ts$flows, paste(ts_output, "output_flows.csv"))
    write.csv(ts$sen, paste(ts_output, "output_sen.csv"))
    write.csv(ts$jrr, paste(ts_output, "output_jrr.csv"))
    write.csv(ts$occ, paste(ts_output, "output_occ.csv"))
    write.csv(ts$pat, paste(ts_output, "output_pat.csv"))
  })
  #------------------------------------------------------------------
  #------------------------------------------------------------------
  # Second, creates graphs and values for boxes for display
  #------------------------------------------------------------------
  #------------------------------------------------------------------
  # All of these below probably should be done in sub-scripts:
  #
  #------------------------------------------------------------------
  # Create graph of Potomac River flows
  #------------------------------------------------------------------
  output$potomacFlows <- renderPlot({
  # Grab ts and prepare for graphing:
  potomac.ts.df <- ts$flows
  #
  potomac.graph.df0 <- left_join(potomac.ts.df, 
                                 potomac.data.df, 
                                 by = "date_time") %>%
    dplyr::select(Date = date_time, LFalls = lfalls_obs, 
 #                 por_nat = por_nat.x, 
                  Withdrawals = demand)
  potomac.graph.df <- potomac.graph.df0 %>%
    gather(key = "Flow", 
           value = "MGD", -Date) 
  
    potomac.graph.df <- potomac.graph.df %>%
      filter(Date >= input$plot_range[1],
             Date <= input$plot_range[2])
    ggplot(data = potomac.graph.df, aes(x = Date, y = MGD, group = Flow)) +
      geom_line(aes(color = Flow, size = Flow)) +
      scale_color_manual(values = c("deepskyblue1", "red")) +
      scale_size_manual(values = c(2, 1)) +
      theme(axis.title.x = element_blank())
    }) # end output$potomacFlows
  # #------------------------------------------------------------------
  # # Create today's date
  # #------------------------------------------------------------------
  #   output$sim_today <- renderValueBox({
  #   potomac.ts.df <- ts$flows
  #   sim_today0 <- last(potomac.ts.df$date_time)
  #   sim_today <- paste("Today's date is ", sim_today0)
  #   valueBox(
  #     value = tags$p(sim_today, style = "font-size: 60%;"),
  #     subtitle = NULL,
  #     color = "black"
  #   )
  # }) 
  #------------------------------------------------------------------
  # Create value for yesterday's Potomac River flow at Point of Rocks
  #------------------------------------------------------------------
  output$por_flow <- renderValueBox({
    por_threshold <- 2000 # (cfs) CO-OP's trigger for daily monitoring/reporting
    potomac.ts.df <- ts$flows
    por_flow <- paste("Flow at Point of Rocks = ",
                      round(last(potomac.ts.df$por_nat)*mgd_to_cfs),
                            " cfs")
    valueBox(
      value = tags$p(por_flow, style = "font-size: 50%;"),
      subtitle = NULL,
#      color = if (por_flow >= por_threshold) "green" else "yellow"
      color = "blue"
    )
  })
  #------------------------------------------------------------------
  # Create value for yesterday's total WMA demand
  #------------------------------------------------------------------
  output$demand <- renderValueBox({
    potomac.ts.df <- ts$flows
    demand <- paste("Total WMA Demand = ", 
                    round(last(potomac.ts.df$demand)), 
                    " MGD")
    valueBox(
      value = tags$p(demand, style = "font-size: 50%;"),
      subtitle = NULL,
      color = "blue"
    )
  })  
  #------------------------------------------------------------------
  # # Output today's "adjusted" flow at Little Falls
  # output$lfalls_adj <- renderValueBox({
  #   potomac.ts.df <- ts$flows
  #   lfalls_adj <- paste("Flow at Little Falls (adjusted) = ",
  #                     round(last(potomac.ts.df$lfalls_adj)),
  #                     " MGD")
  #   valueBox(
  #     value = tags$p(lfalls_adj, style = "font-size: 50%;"),
  #     subtitle = NULL,
  #     color = "blue"
  #   )
  # })
  #------------------------------------------------------------------
  # Create value for yesterday's Potomac River flow at Little Falls
  #------------------------------------------------------------------
    output$lfalls_obs <- renderValueBox({
    potomac.ts.df <- ts$flows
    lfalls_obs <- paste("Flow at Little Falls (observed) = ",
                        round(last(potomac.ts.df$lfalls_obs)),
                        " MGD")
    valueBox(
      value = tags$p(lfalls_obs, style = "font-size: 50%;"),
      subtitle = NULL,
      color = "blue"
    )
  })
  #------------------------------------------------------------------
  # Create info on CO-OP operational status
  #------------------------------------------------------------------
    # output$coop_ops <- renderInfoBox({
    # potomac.ts.df <- ts$flows
    # por_flow <- last(potomac.ts.df$por_nat*mgd_to_cfs)
    # if(por_flow > 2000) {
    #   text_stage <- "NORMAL"
    #   color_stage <- "green"}
    # if(por_flow <= 2000) {
    #   text_stage <- "Daily monitoring & reporting"
    #   color_stage <- "yellow"}
    # infoBox(
    #   title = "CO-OP operations status",
    #   value = paste(text_stage),
    #   subtitle = NULL,
    #   icon = shiny::icon("arrow"),
    #   color = color_stage
    # )
    # }) # end output$coop_ops
  
  output$coop_ops <- renderUI({
    potomac.ts.df <- ts$flows
    por_flow <- last(potomac.ts.df$por_nat*mgd_to_cfs)
    if(por_flow > 2000) {
      text_stage <- "NORMAL"
      text_stage2 <- ""
      color_stage <- green}
    if(por_flow <= 2000) {
      text_stage <- "WATCH" 
      text_stage2 <- "Daily monitoring & reporting"
      color_stage <- yellow}
    div(class="longbox",
        div(class="squarei", style = color_stage,
            div(class="my_content",
                div(class="table",
                    div(class="table-cell",
                        p(class="p2",text_stage)
                          )))),
        div(class="ibox", style = "background-color:white",
            div(class="my_content",
                div(class="table",
                    div(class="table-cell",
                        p(class = "p1",paste0("CO-OP operations status "))#,text_stage2))
                    ))))
    )
    
  }
  )

  output$lfaa_alert <- renderUI({
    potomac.ts.df <- ts$flows
    W <- last(potomac.ts.df$demand*1.0)
    Qadj <- round(last(potomac.ts.df$lfalls_adj*1.0))
    if(Qadj > W/0.5) {
      text_stage <- "NORMAL"
      color_stage <- green}
    if(Qadj <= W/0.5 & Qadj > (W + 100)/0.8){
      text_stage <- "ALERT"
      color_stage <- yellow}
    if(Qadj <= (W + 100)/0.8 & Qadj > W + 110){
      text_stage <- "RESTRICTION"
      color_stage <- orange}
    if(Qadj <= (W + 110)){
      text_stage <- "EMERGENCY"
      color_stage <- red}
  
    div(class="longbox",
        div(class="squarei", style = color_stage,
            div(class="my_content",
                div(class="table",
                    div(class="table-cell",
                        p(class="p2",text_stage)
                        )))),
        div(class="ibox", style = "background-color:white",
            div(class="my_content",
                div(class="table",
                    div(class="table-cell",
                        p(class = "p1",paste0("LFAA stage"))#"Little Falls adj. flow, MGD "))#,text_stage2))
                    ))))
    )
    
  }
  )
  #------------------------------------------------------------------
  # Create info on MWCOG Drought Plan stage
  #------------------------------------------------------------------
  # 
  output$mwcog_stage <- renderUI({
    potomac.ts.df <- ts$flows
    por_flow <- last(potomac.ts.df$por_nat)
    if(por_flow > 2000) {
      text_stage <- "NORMAL" 
      text_stage2 <- "- Wise Water Use"
      color_stage <- green}
    if(por_flow <= 2000) {
      # based on NOAA drought status - D1
      # then "notifications" upon 1st release, & when jrr+sen at 75%
      text_stage <- "WATCH" 
      text_stage2 <- "- Voluntary Water Conservation"
      color_stage <- yellow}
    div(class="longbox",
        div(class="squarei", style = color_stage,
            div(class="my_content",
                div(class="table",
                    div(class="table-cell",
                        p(class="p2",text_stage)
                        )))),
        div(class="ibox", style = "background-color:white",
            div(class="my_content",
                div(class="table",
                    div(class="table-cell",
                        p(class = "p1",paste0("MWCOG drought stage "))#,text_stage2))
                    ))))
    )
    
  }
  )
  #------------------------------------------------------------------
  # Create graph of storage and releases for each reservoir
  #------------------------------------------------------------------
  output$jrrStorageReleases <- renderPlot({
    jrr.graph <- ts$jrr %>%
      filter(date_time >= input$plot_range[1],
             date_time <= input$plot_range[2])
    ggplot(data = jrr.graph, aes(x = date_time)) +
      geom_line(aes(y = storage_ws, color = "WS Storage")) +
      geom_line(aes(y = storage_wq, color = "WQ Storage")) +
      geom_line(aes(y = outflow_ws, color = "WS Outflow")) +
      geom_line(aes(y = outflow_wq, color = "WQ Outflow")) +
      scale_color_manual(values = c("grey", "blue", "yellow", "black"))
  }) # end jrr renderPlot
  
  #
  #------------------------------------------------------------------
    output$senStorageReleases <- renderPlot({
    sen.graph <- ts$sen %>%
      filter(date_time >= input$plot_range[1],
             date_time <= input$plot_range[2])
    ggplot(data = sen.graph, aes(x = date_time)) +
      geom_line(aes(y = storage, color = "Storage")) +
      geom_line(aes(y = outflow, color = "Outflow")) +
      scale_color_manual(values = c("grey", "black"))
  }) # end sen renderPlot
  #
  #------------------------------------------------------------------
  output$patStorageReleases <- renderPlot({
    pat.graph <- ts$pat %>%
      filter(date_time >= input$plot_range[1],
             date_time <= input$plot_range[2])
    ggplot(data = pat.graph, aes(x = date_time)) +
      geom_line(aes(y = storage, color = "Storage")) +
      geom_line(aes(y = outflow, color = "Outflow")) +
      scale_color_manual(values = c("grey", "black"))
  }) # end pat renderPlot
  #
  #------------------------------------------------------------------
  output$occStorageReleases <- renderPlot({
    occ.graph <- ts$occ %>%
      filter(date_time >= input$plot_range[1],
             date_time <= input$plot_range[2])
    ggplot(data = occ.graph, aes(x = date_time)) +
      geom_line(aes(y = storage, color = "Storage")) +
      geom_line(aes(y = outflow, color = "Outflow")) +
      scale_color_manual(values = c("grey", "black"))
  }) # end occ renderPlot
  #
  #------------------------------------------------------------------
  #------------------------------------------------------------------
  # Temporary output for QAing purposes
  #------------------------------------------------------------------
  output$QA_out <- renderValueBox({
    potomac.df <- ts$flows
    sen.df <- ts$sen
    jrr.df <- ts$jrr
    pat.df <- ts$pat
    occ.df <- ts$occ
    QA_out <- paste("Min flow at LFalls = ",
                      round(min(potomac.df$lfalls_obs, na.rm = TRUE)),
                      " mgd",
                      "________ Min sen, jrr, pat, occ stor = ",
                      round(min(sen.df$storage, na.rm = TRUE)), " mg, ",
                    round(min(jrr.df$storage, na.rm = TRUE)), " mg,  ",
                    round(min(pat.df$storage, na.rm = TRUE)), " mg,  ",
                    round(min(occ.df$storage, na.rm = TRUE)),
                    " mg")
    valueBox(
      value = tags$p(QA_out, style = "font-size: 60%;"),
      subtitle = NULL,
      color = "blue"
    )
  })
  #------------------------------------------------------------------
  #------------------------------------------------------------------
  # Temporary output for QAing
  #------------------------------------------------------------------

  #------------------------------------------------------------------
  #date to login bar
  output$date_text  <- renderText({
    paste("Today's date is", as.character(test_date$test_date_value))
  })
  
  #------------------------------------------------------------------
  #Shenandoah warning status squares
  output$boxes  <- renderUI({
    
    div(class="topbox_main",
      img( src="https://md.water.usgs.gov/drought/MDE-Drought2017-02-28.png", height="160px", width="360px"),
      
    div(class="sidebox",
        div(class="squareside1", style = "background-color:white",
            div(class="my_content",
                div(class="table",
                    div(class="table-cell",
                        p(class="p3","Flow benefit, MGD")
                    )))),
        div(class="squareside2", style="background-color:silver",
            div(class="my_content",
                div(class="table",
                    div(class="table-cell", style="text-align:right;",
                        p(style="font-size:15px;","12")
                    ))))
    ) #end of sidebox
    ) #end of topbox_main

  })
  
  #------------------------------------------------------------------
  #NoVa warning status squares
  output$boxes2  <- renderUI({
    div(class="topbox_main",
      div(class="topbox1",
          div(class="square", style=precip_value(),#"background-color:yellow"
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p4","P")
                      )))), 
          div(class="square", style=g_value(),#"background-color:red",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p4","GW")
                      )))),
          div(class="square", style=s_value(),#"background-color:orange",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p4","SW")
                      )))),
          div(class="square", style=q_value(),#"background-color:green",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p4","R")
                      )))),
          div(class="ibox", style = "background-color:white",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class = "p3",paste0("Shenandoah "))#,text_stage2))
                      ))))
      ), #end of topbox1
      div(class="topbox2", 
          div(class="square", style="background-color:yellow",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p4","P")
                      )))),
          div(class="square", style="background-color:orange",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p4","GW")
                      )))),
          div(class="square", style="background-color:red",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p4","SW")
                      )))),
          div(class="square", style="background-color:green",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p4","R")
                      )))),
          div(class="ibox", style = "background-color:white",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class = "p3",paste0("NoVa "))#,text_stage2))
                      ))))
          
      ), #end of topbox2
      div(class="sidebox",
          div(class="squareside1", style = "background-color:white",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p3","Flow benefit, MGD")
                      )))),
          div(class="squareside2", style="background-color:silver",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(style="font-size:15px;","12")
                      ))))
      ) #end of sidebox
    )#end of topbox_main
  })
  
  #------------------------------------------------------------------
  #code for outputting colors based on values in data(data and log are toy)
  #some of this needs to be placed in functions section
  
  p_data_percent <- eventReactive(test_date$test_date_value, {
    date_func(my_data_p$date, my_data_p$p_percent_normal, test_date$test_date_value)

  })
  
  
  precip_value <- eventReactive(test_date$test_date_value,{#a_index,{
    case_when(
      p_data_percent() <= 0 ~ green,#"background-color:purple", #"#000000",
      p_data_percent() > .0 && p_data_percent() <= .20 ~ red,#"background-color:red", #"#cc3300",
      p_data_percent() > .20 && p_data_percent() <= .40 ~ orange,#"background-color:orange",  #"#ff9966",
      p_data_percent() > .40 && p_data_percent() <= .60 ~ yellow,#"background-color:yellow",  #"#ffcc00",
      p_data_percent() > .60 && p_data_percent() <= .80 ~ green,#"background-color:green", #"#99cc33",
      p_data_percent() > .80 && p_data_percent() < 1 ~  navy, #"background-color:navy" #"#339900"
      TRUE ~ black
      
    )
    
  })
  
  q_data_percent <- eventReactive(test_date$test_date_value, {
    date_func(my_data_q$date, my_data_q$flow_cfs, test_date$test_date_value)
  })

  q_value <- eventReactive(test_date$test_date_value,{
    case_when(
      q_data_percent() <= 0 ~ red,#"background-color:purple", #"#000000",
      q_data_percent() > 0 && q_data_percent() <= 100 ~ red,#"background-color:red", #"#cc3300",
      q_data_percent() > 100 && q_data_percent() <= 200 ~ orange,#"background-color:orange",  #"#ff9966",
      q_data_percent() > 200 && q_data_percent() <= 300 ~ yellow,#"background-color:yellow",  #"#ffcc00",
      q_data_percent() > 300 && q_data_percent() <= 400 ~ green,#"background-color:green", #"#99cc33",
      #q_data_percent() > 400 && q_data_percent() < 500 ~  navy, #"background-color:navy" #"#339900"
      q_data_percent() > 400 ~ navy,
      TRUE ~ black
    )
  })
  
  s_data_percent <- eventReactive(test_date$test_date_value, {
    date_func(my_data_s$date, my_data_s$storage_days, test_date$test_date_value)
  })
  
  s_value <- eventReactive(test_date$test_date_value,{
    case_when(
      s_data_percent() <= 0 ~ yellow,#"background-color:purple", #"#000000",
      s_data_percent() > 0 && s_data_percent() <= 60 ~ red,#"background-color:red", #"#cc3300",
      s_data_percent() > 60 && s_data_percent() <= 90 ~ orange,#"background-color:orange",  #"#ff9966",
      s_data_percent() > 90 && s_data_percent() <= 120 ~ yellow,#"background-color:yellow",  #"#ffcc00",
      s_data_percent() > 120 && s_data_percent() <= 500 ~ green,#"background-color:green", #"#99cc33",
      s_data_percent() > 500 && s_data_percent() <= 1130 ~  navy, #"background-color:navy" #"#339900"
      TRUE ~ black
    )
  })
  
  g_data_percent <- eventReactive(test_date$test_date_value, {
    date_func(my_data_g$date, my_data_g$flow_cfs, test_date$test_date_value)
  })
  
  g_value <- eventReactive(test_date$test_date_value,{
    case_when(
      g_data_percent() <= 0 ~ orange,#"background-color:purple", #"#000000",
      g_data_percent() > 0 && g_data_percent() <= 55 ~ red,#"background-color:red", #"#cc3300",
      g_data_percent() > 55 && g_data_percent() <= 110 ~ orange,#"background-color:orange",  #"#ff9966",
      g_data_percent() > 110 && g_data_percent() <= 165 ~ yellow,#"background-color:yellow",  #"#ffcc00",
      g_data_percent() > 165 && g_data_percent() <= 220 ~ green,#"background-color:green", #"#99cc33",
      g_data_percent() > 220 && g_data_percent() < 275 ~  navy, #"background-color:navy" #"#339900"
      TRUE ~ black
    )
  })
  
  #------------------------------------------------------------------
  }) # end shinyServer

