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
              flows = potomac.ts.df0,
              states = state.ts.df0)
  ts <- sim_main_func(date_today0,
                      dr_va0, # VA upstream demand reduction
                      dr_md0, # MD upstream demand reduction
                      mos_1day0, # COOP 1-day margin of safety
                      dr_wma_override0, # WMA demand reduction override
                      ts0) 
  #
  # Now make ts reactive, initializing to results from above
  ts <- reactiveValues(flows = ts$flows, 
                       sen = ts$sen, 
                       jrr = ts$jrr,
                       pat = ts$pat,
                       occ = ts$occ,
                       states = ts$states)
  #
  # Allow the user to re-run the simulation 
  #   - say, if end date (aka DREXtoday) changes
  observeEvent(input$run_main, {
    ts <- sim_main_func(input$DREXtoday,
                        input$dr_va,
                        input$dr_md,
                        input$mos_1day,
                        input$dr_wma_override,
                        ts)
  })
  #
  # Allow the user to add chunks of days to the simulation
  observeEvent(input$run_add, {
    ts <- sim_add_days_func(input$chunkofdays,
                            input$dr_va,
                            input$dr_md,
                            input$mos_1day,
                            input$dr_wma_override,
                            ts)
  })
  
  #
  # Allow the user to write simulation output time series to files
  observeEvent(input$write_ts, {
    write.csv(ts$flows, paste(ts_output, "output_flows.csv"))
    write.csv(ts$sen, paste(ts_output, "output_sen.csv"))
    write.csv(ts$jrr, paste(ts_output, "output_jrr.csv"))
    write.csv(ts$occ, paste(ts_output, "output_occ.csv"))
    write.csv(ts$pat, paste(ts_output, "output_pat.csv"))
    write.csv(ts$states, paste(ts_output, "output_states.csv"))
  })
  #------------------------------------------------------------------
  #------------------------------------------------------------------
  # Second, creates graphs
  #------------------------------------------------------------------
  #------------------------------------------------------------------
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
  #------------------------------------------------------------------
  # Create graph of storage and releases for each reservoir
  #------------------------------------------------------------------
  output$jrrStorageReleases <- renderPlot({
    jrr.graph <- ts$jrr %>%
      select(Date = date_time,
             "Water supply" = storage_ws, 
             "Water quality" = storage_wq
             # ,
             # "WS release" = outflow_ws, 
             # "WQ release" = outflow_wq
             ) %>%
      gather(key = "Legend", 
             value = "MGD", -Date) %>%
      filter(Date >= input$plot_range[1],
             Date <= input$plot_range[2])
    ggplot(data = jrr.graph,
           aes(x = Date, y = MGD, group = Legend)) +
      geom_line(aes(color = Legend, size = Legend)) +
      scale_color_manual(values = c("lightgreen", "lightblue",
                                    "green", "blue")) +
      scale_size_manual(values = c(1, 1, 1.5, 1.5)) +
      theme(axis.title.x = element_blank()) +
      theme(legend.position = "bottom", 
            legend.title = element_blank())
  }) # end jrr renderPlot testing
  #
  #------------------------------------------------------------------
  # output$senStorageReleases <- renderPlot({
  #   sen.graph <- ts$sen
  #   graph_title <- "Seneca"
  #   display_graph_res_func(graph_title, sen.graph)
  # })
  output$senStorageReleases <- renderPlot({
    sen.graph <- ts$sen
    graph_title <- "Little Seneca"
    res.graph <- sen.graph %>%
      select(date_time, storage, outflow) %>%
      gather(key = "Legend",
             value = "MG", -date_time) %>%
      filter(date_time >= input$plot_range[1],
             date_time <= input$plot_range[2])
    ggplot(data = res.graph,
                       aes(x = date_time, y = MG, group = Legend)) +
      geom_line(aes(color = Legend, size = Legend)) +
      scale_color_manual(values = c("lightblue",
                                    "blue")) +
      scale_size_manual(values = c(0.5, 1)) +
      ggtitle(graph_title) +
      theme(plot.title = element_text(size = 16,
                                      face = "bold")) +
      theme(axis.title.x = element_blank()) +
      theme(legend.position = "none")
   }) # end sen renderPlot
  #
  #------------------------------------------------------------------
  output$patStorageReleases <- renderPlot({
    pat.graph <- ts$pat
    graph_title <- "Patuxent"
    res.graph <- pat.graph %>%
      select(date_time, storage, outflow) %>%
      gather(key = "Legend",
             value = "MG", -date_time) %>%
      filter(date_time >= input$plot_range[1],
             date_time <= input$plot_range[2])
    ggplot(data = res.graph,
           aes(x = date_time, y = MG, group = Legend)) +
      geom_line(aes(color = Legend, size = Legend)) +
      scale_color_manual(values = c("lightblue",
                                    "blue")) +
      scale_size_manual(values = c(0.5, 1)) +
      ggtitle(graph_title) +
      theme(plot.title = element_text(size = 16,
                                      face = "bold")) +
      theme(axis.title.x = element_blank()) +
      theme(legend.position = "none")
  }) # end pat renderPlot
  #
  #     output$patStorageReleases <- renderPlot({
  #   graph_title <- "Patuxent"
  #   pat.graph <- ts$pat %>%
  #     filter(date_time >= input$plot_range[1],
  #            date_time <= input$plot_range[2])
  #   ggplot(data = pat.graph, aes(x = date_time)) +
  #     ggtitle(graph_title) +
  #     geom_line(aes(y = storage, color = "Storage")) +
  #     geom_line(aes(y = outflow, color = "Outflow")) +
  #     scale_color_manual(values = c("grey", "black"))
  # }) # end pat renderPlot
  #
  #------------------------------------------------------------------
  output$occStorageReleases <- renderPlot({
    occ.graph <- ts$occ
    graph_title <- "Occoquan"
    res.graph <- occ.graph %>%
      select(date_time, storage, outflow) %>%
      gather(key = "Legend",
             value = "MG", -date_time) %>%
      filter(date_time >= input$plot_range[1],
             date_time <= input$plot_range[2])
    ggplot(data = res.graph,
           aes(x = date_time, y = MG, group = Legend)) +
      geom_line(aes(color = Legend, size = Legend)) +
      scale_color_manual(values = c("lightblue",
                                    "blue")) +
      scale_size_manual(values = c(0.5, 1)) +
      ggtitle(graph_title) +
      theme(plot.title = element_text(size = 16,
                                      face = "bold")) +
      theme(axis.title.x = element_blank()) +
      theme(legend.position = "none")
  }) # end occ renderPlot
  #
  #   output$occStorageReleases <- renderPlot({
  #   occ.graph <- ts$occ %>%
  #     filter(date_time >= input$plot_range[1],
  #            date_time <= input$plot_range[2])
  #   ggplot(data = occ.graph, aes(x = date_time)) +
  #     geom_line(aes(y = storage, color = "Storage")) +
  #     geom_line(aes(y = outflow, color = "Outflow")) +
  #     scale_color_manual(values = c("grey", "black"))
  # }) # end occ renderPlot
  #
  #------------------------------------------------------------------
  #------------------------------------------------------------------
  #------------------------------------------------------------------
  # Finally, create boxes with values and triggers
  #------------------------------------------------------------------
  #------------------------------------------------------------------
  #
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
  #
  output$coop_ops <- renderUI({
    flows.last <- last(ts$flows)
    por_flow <- flows.last$por_nat[1]*mgd_to_cfs
    q_adj <- flows.last$lfalls_adj[1]
    withdr_pot <- flows.last$demand[1]
    #
    if(por_flow > 2000) {
      text_stage <- "NORMAL"
      text_stage2 <- ""
      color_stage <- green}
    if(por_flow <= 2000) {
      text_stage <- "DAILY OPS" 
      text_stage2 <- "Daily monitoring & reporting"
      color_stage <- yellow}
    if(q_adj <= 100 + 2*withdr_pot) {
      text_stage <- "HOURLY OPS" 
      text_stage2 <- "Hourly monitoring & reporting"
      color_stage <- orange}
    div(class="longbox",
        div(class="ibox", style = "background-color:silver",
            div(class="my_content",
                div(class="table",
                    div(class="table-cell2",
                        p(class = "p1",paste0("CO-OP operations status "))#,text_stage2))
                    )))),
        div(class="squarei", style = color_stage,
            div(class="my_content",
                div(class="table",
                    div(class="table-cell2",
                        p(class="p2",text_stage)
                          ))))
        
    ) # end div(class="longbox" 
  }) # end renderUI
  #------------------------------------------------------------------
  # Create info on LFAA status
  #------------------------------------------------------------------
  #
  output$lfaa_alert <- renderUI({
    flows.last <- last(ts$flows)
    q_adj <- flows.last$lfalls_adj[1]
    W <- flows.last$demand[1]
    #
    sen.last <- last(ts$sen)
    jrr.last <- last(ts$jrr)
    sen_stor <- sen.last$stor[1]
    jrr_ws_stor <- jrr.last$storage_ws[1]
    jrr_ws_cap_cp <- jrr_cap*jrr_ws_frac
    shared_ws_frac <- (sen_stor + jrr_ws_stor)/(sen_cap + jrr_ws_cap_cp)
    #
    if(q_adj > W/0.5) {
      text_stage <- "NORMAL"
      color_stage <- green}
    if(q_adj <= W/0.5 & q_adj > (W + 100)/0.8){
      text_stage <- "ALERT"
      color_stage <- yellow}
    if(q_adj <= (W + 100)/0.8) {
      text_stage <- "RESTRICTION"
      color_stage <- orange}
    if(shared_ws_frac <= 0.05){
      text_stage <- "EMERGENCY"
      color_stage <- red}
  
    div(class="longbox",
        div(class="ibox", style = "background-color:silver",
            div(class="my_content",
                div(class="table",
                    div(class="table-cell2",
                        p(class = "p1",paste0("LFAA stage"))#"Little Falls adj. flow, MGD "))#,text_stage2))
                    )))),
        div(class="squarei", style = color_stage,
            div(class="my_content",
                div(class="table",
                    div(class="table-cell2",
                        p(class="p2",text_stage)
                        ))))
        
    ) # end div(class="longbox"
  }) # end renderUI
  #
  #------------------------------------------------------------------
  # Create info on MWCOG Drought Plan stage
  #------------------------------------------------------------------
  # 
  output$mwcog_stage <- renderUI({
    flows.last <- last(ts$flows)
    por_flow <- flows.last$por_nat[1]*mgd_to_cfs
    sen.last <- last(ts$sen)
    jrr.last <- last(ts$jrr)
    sen_stor <- sen.last$stor[1]
    jrr_ws_stor <- jrr.last$storage_ws[1]
    jrr_ws_cap_cp <- jrr_cap*jrr_ws_frac
    shared_ws_frac <- (sen_stor + jrr_ws_stor)/(sen_cap + jrr_ws_cap_cp)
    #
    # would 1500 cfs work as a surrogate for NOAA D1?
    noaa_d1_surrogate <- 1500
    if(por_flow > noaa_d1_surrogate) {
      text_stage <- "NORMAL" 
      text_stage2 <- "- Wise Water Use"
      color_stage <- green}
    if(por_flow <= noaa_d1_surrogate) { # surrogate
      # based on NOAA drought status - D1
      # then "notifications" upon 1st release, & when jrr+sen at 75%
      text_stage <- "WATCH" 
      text_stage2 <- "- Voluntary Water Conservation"
      color_stage <- yellow}
    if(shared_ws_frac <= 0.60){
      text_stage <- "WARNING"
      text_stage2 <- "- Voluntary Water Conservation"
      color_stage <- orange}
    if(shared_ws_frac <= 0.05){
      text_stage <- "EMERGENCY"
      text_stage2 <- "- Voluntary Water Conservation"
      color_stage <- red}
    
    div(class="longbox",
        div(class="ibox", style = "background-color:silver",
            div(class="my_content",
                div(class="table",
                    div(class="table-cell2",
                        p(class = "p1",paste0("MWCOG drought stage "))#,text_stage2))
                    )))),
        div(class="squarei", style = color_stage,
            div(class="my_content",
                div(class="table",
                    div(class="table-cell2",
                        p(class="p2",text_stage)
                        ))))
 

    ) # end div(class="longbox",
  }) # end renderUI
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
                    round(min(jrr.df$storage_ws, na.rm = TRUE)), " mg,  ",
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
  #this outputs the last date to the login bar at the top right of the screen.
  output$date_text  <- renderText({
    potomac.ts.df <- ts$flows
    test_date <- last(potomac.ts.df$date_time)
#    paste("Today's date is", as.character(test_date$test_date_value))
    paste("Today's date is ", as.character(test_date),"  ")
  })
  #
  #------------------------------------------------------------------
  # Create state drought status boxes
  #------------------------------------------------------------------
  #
  # # Luke - here's how you can access the values 
  # #        from within a render function:
  # state.indices <- last(ts$states)
  # # The indicator values are:
  # #   0 = NORMAL
  # #   1 = WATCH
  # #   2 = WARNING
  # #   3 = EMERGENCY
  # #
  # # The indicator names are
  # #    gw_va_shen, p_va_shen, sw_va_shen, r_va_shen,
  # #    gw_va_nova, p_va_nova, sw_va_nova, r_va_nova,
  # #    region_md_cent, region_md_west
  # # -----------------------------------
  # # The VA and MD flow changes:
  # flows.today <- last(ts$flows)
  # dQ_va <- flows.today$dQ_va[1]
  # dQ_md <- flows.today$dQ_md[1]
  #------------------------------------------------------------------
  #Shenandoah warning status squares
  output$boxes  <- renderUI({
    #MD flow benefit
    flows.today <- last(ts$flows)
    dQ_md <- flows.today$dQ_md[1]
    
    div(class="topbox_main", p(class= "title","MARYLAND DROUGHT STATUS"),
    #the image link below is a placeholder for an interactive leaflet map forthcoming
    
      #img( src="https://md.water.usgs.gov/drought/MDE-Drought2017-02-28.png", height="160px", width="360px"),

    
    #this is html in a format taht shiny will accept.  This along with main.css structures the 
    #properties of the Maryland Drought Status section
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
                    div(class="table-cell3", #style="text-align:right;",
                        p(style="font-size:15px;", dQ_md)
                    ))))
    ) #end of sidebox
    ) #end of topbox_main

  })
  
  output$mymap <- renderLeaflet({
    #for MD drough map
    state.indices <- last(ts$states)
    i_region_md_cent <- state.indices$region_md_cent[1]
    color_i_region_md_cent <- warning_color_map_func(i_region_md_cent)
    i_region_md_west <- state.indices$region_md_west[1]
    color_i_region_md_west <- warning_color_map_func(i_region_md_west)
    
    leaflet() %>%
      addPolygons(data = clipcentral_t, color="black", fillColor = color_i_region_md_cent, opacity = 1, weight = 1,
                  fillOpacity = 1) %>%
      addPolygons(data = western_region_t, color="black", fillColor = color_i_region_md_west, opacity = 1, weight= 1,
                  fillOpacity = 1)# %>%
  })
  
  #------------------------------------------------------------------
  #NoVa warning status squares
  output$boxes2  <- renderUI({
    #this code finds the last value in ts and outputs a number value between 0-3 that 
    #when run through function warning_color_func outputs a color variable that is linked 
    #to a css style color output value in global.R
    #for NoVa
    state.indices <- last(ts$states)
    i_p_va_nova <- state.indices$p_va_nova[1]
    color_p_va_nova <- warning_color_func(i_p_va_nova)
    i_gw_va_nova <- state.indices$gw_va_nova[1]
    color_gw_va_nova <- warning_color_func(i_gw_va_nova)
    i_sw_va_nova <- state.indices$sw_va_nova[1]
    color_sw_va_nova <- warning_color_func(i_sw_va_nova)
    i_r_va_nova <- state.indices$r_va_nova[1]
    color_r_va_nova <- warning_color_func(i_r_va_nova)
    
    #for Shenandoah
    i_p_va_shen <- state.indices$p_va_shen[1]
    color_p_va_shen <- warning_color_func(i_p_va_shen)
    i_gw_va_shen <- state.indices$gw_va_shen[1]
    color_gw_va_shen <- warning_color_func(i_gw_va_shen)
    i_sw_va_shen <- state.indices$sw_va_shen[1]
    color_sw_va_shen <- warning_color_func(i_sw_va_shen)
    i_r_va_shen<- state.indices$r_va_shen[1]
    color_r_va_shen <- warning_color_func(i_r_va_shen)
    
    #Virginia flow benefit
    flows.today <- last(ts$flows)
    dQ_va <- flows.today$dQ_va[1]
    
    #this is html in a format taht shiny will accept.  This along with main.css structures the 
    #properties of the Virginia Drought Status section
    div(class="topbox_main", p(class= "title", "VIRGINIA DROUGHT STATUS"),
      div(class="topbox1", 
          div(class="square", style=color_p_va_nova,#"background-color:yellow"
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p4","P")
                      )))), 
          div(class="square", style=color_gw_va_nova,#"background-color:red",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p4","GW")
                      )))),
          div(class="square", style=color_sw_va_nova,#"background-color:orange",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p4","SW")
                      )))),
          div(class="square", style=color_r_va_nova,#"background-color:green",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p4","R")
                      )))),
          div(class="ibox", style = "background-color:white",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class = "p5",paste0("Shenandoah "))#,text_stage2))
                      ))))
      ), #end of topbox1
      div(class="topbox2", 
          div(class="square", style=color_p_va_shen,
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p4","P")
                      )))),
          div(class="square", style=color_gw_va_shen,
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p4","GW")
                      )))),
          div(class="square", style=color_sw_va_shen,
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p4","SW")
                      )))),
          div(class="square", style=color_r_va_shen,
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class="p4","R")
                      )))),
          div(class="ibox", style = "background-color:white",
              div(class="my_content",
                  div(class="table",
                      div(class="table-cell",
                          p(class = "p5",paste0("NoVa "))#,text_stage2))
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
                      div(class="table-cell3", #style= "text-align:right",
                          p(style="font-size:15px;",dQ_va)
                      ))))
      ) #end of sidebox
    )#end of topbox_main
  })
  
  #------------------------------------------------------------------
  #code for outputting colors based on values in data(data and log are toy)
  #some of this needs to be placed in functions section
  
  # p_data_percent <- eventReactive(test_date$test_date_value, {
  #   date_func(my_data_p$date, my_data_p$p_percent_normal, test_date$test_date_value)
  # 
  # })
  # 
  # 
  # precip_value <- eventReactive(test_date$test_date_value,{#a_index,{
  #   case_when(
  #     p_data_percent() <= 0 ~ green,#"background-color:purple", #"#000000",
  #     p_data_percent() > .0 && p_data_percent() <= .20 ~ red,#"background-color:red", #"#cc3300",
  #     p_data_percent() > .20 && p_data_percent() <= .40 ~ orange,#"background-color:orange",  #"#ff9966",
  #     p_data_percent() > .40 && p_data_percent() <= .60 ~ yellow,#"background-color:yellow",  #"#ffcc00",
  #     p_data_percent() > .60 && p_data_percent() <= .80 ~ green,#"background-color:green", #"#99cc33",
  #     p_data_percent() > .80 && p_data_percent() < 1 ~  navy, #"background-color:navy" #"#339900"
  #     TRUE ~ black
  #     
  #   )
  #   
  # })
  # 
  # q_data_percent <- eventReactive(test_date$test_date_value, {
  #   date_func(my_data_q$date, my_data_q$flow_cfs, test_date$test_date_value)
  # })
  # 
  # q_value <- eventReactive(test_date$test_date_value,{
  #   case_when(
  #     q_data_percent() <= 0 ~ red,#"background-color:purple", #"#000000",
  #     q_data_percent() > 0 && q_data_percent() <= 100 ~ red,#"background-color:red", #"#cc3300",
  #     q_data_percent() > 100 && q_data_percent() <= 200 ~ orange,#"background-color:orange",  #"#ff9966",
  #     q_data_percent() > 200 && q_data_percent() <= 300 ~ yellow,#"background-color:yellow",  #"#ffcc00",
  #     q_data_percent() > 300 && q_data_percent() <= 400 ~ green,#"background-color:green", #"#99cc33",
  #     #q_data_percent() > 400 && q_data_percent() < 500 ~  navy, #"background-color:navy" #"#339900"
  #     q_data_percent() > 400 ~ navy,
  #     TRUE ~ black
  #   )
  # })
  # 
  # s_data_percent <- eventReactive(test_date$test_date_value, {
  #   date_func(my_data_s$date, my_data_s$storage_days, test_date$test_date_value)
  # })
  # 
  # s_value <- eventReactive(test_date$test_date_value,{
  #   case_when(
  #     s_data_percent() <= 0 ~ yellow,#"background-color:purple", #"#000000",
  #     s_data_percent() > 0 && s_data_percent() <= 60 ~ red,#"background-color:red", #"#cc3300",
  #     s_data_percent() > 60 && s_data_percent() <= 90 ~ orange,#"background-color:orange",  #"#ff9966",
  #     s_data_percent() > 90 && s_data_percent() <= 120 ~ yellow,#"background-color:yellow",  #"#ffcc00",
  #     s_data_percent() > 120 && s_data_percent() <= 500 ~ green,#"background-color:green", #"#99cc33",
  #     s_data_percent() > 500 && s_data_percent() <= 1130 ~  navy, #"background-color:navy" #"#339900"
  #     TRUE ~ black
  #   )
  # })
  # 
  # g_data_percent <- eventReactive(test_date$test_date_value, {
  #   date_func(my_data_g$date, my_data_g$flow_cfs, test_date$test_date_value)
  # })
  # 
  # g_value <- eventReactive(test_date$test_date_value,{
  #   case_when(
  #     g_data_percent() <= 0 ~ orange,#"background-color:purple", #"#000000",
  #     g_data_percent() > 0 && g_data_percent() <= 55 ~ red,#"background-color:red", #"#cc3300",
  #     g_data_percent() > 55 && g_data_percent() <= 110 ~ orange,#"background-color:orange",  #"#ff9966",
  #     g_data_percent() > 110 && g_data_percent() <= 165 ~ yellow,#"background-color:yellow",  #"#ffcc00",
  #     g_data_percent() > 165 && g_data_percent() <= 220 ~ green,#"background-color:green", #"#99cc33",
  #     g_data_percent() > 220 && g_data_percent() < 275 ~  navy, #"background-color:navy" #"#339900"
  #     TRUE ~ black
  #   )
  # })
  
  #------------------------------------------------------------------
  }) # end shinyServer

