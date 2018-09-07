#
#------------------------------------------------------------------
# 
#------------------------------------------------------------------
shinyServer(function(input, output, session) {
  #
  # Run the main simulation to the hard-coded input, date_today
  ts0 <- list(sen = sen.ts.df0, 
              jrr = jrr.ts.df0, 
              pat = pat.ts.df0,
              occ = occ.ts.df0,
              flows = potomac.ts.df0)
  ts <- sim_main_func(date_today, ts0)
  # ts <- sim_main_func(input$DREXtoday, ts0)
  #
  # Now make ts reactive, initializing to results from above
  ts <- reactiveValues(flows = ts$flows, 
                       sen = ts$sen, 
                       jrr = ts$jrr,
                       pat = ts$pat,
                       occ = ts$occ)
  #
  # Now allow a change of the simulation end date - to input$DREXtoday
  observeEvent(input$run_main, {
    ts <- sim_main_func(input$DREXtoday, ts)
  })
  #
  # Now allow the user to add chunks of days to the simulation
  observeEvent(input$run_add, {
    ts <- sim_add_days_func(input$chunkofdays, ts)
  })
  #
  # Allow the user to write simulation output time series to file
  observeEvent(input$write_ts, {
    write.csv(ts$sen, paste(ts_output, "output.csv"))
  })
  #------------------------------------------------------------------
  # Create the graphs etc to be displayed by the Shiny app
  #------------------------------------------------------------------
  output$potomacFlows <- renderPlot({
  # Grab ts and prepare for graphing:
  potomac.ts.df <- ts$flows
  #
  potomac.graph.df0 <- left_join(potomac.ts.df, 
                                 potomac.data.df, 
                                 by = "date_time") %>%
    dplyr::select(date_time, lfalls_nat = lfalls_nat.x, 
                  por_nat = por_nat.x, 
                  demand, lfalls_obs,
                  sen_outflow, jrr_outflow)
  potomac.graph.df <- potomac.graph.df0 %>%
    gather(key = "location", 
           value = "flow_mgd", -date_time) 
  
    potomac.graph.df <- potomac.graph.df %>%
      filter(date_time >= input$plot_range[1],
             date_time <= input$plot_range[2])
    ggplot(data = potomac.graph.df, aes(x = date_time, y = flow_mgd, group = location)) +
      geom_line(aes(color = location))
    }) 
  #------------------------------------------------------------------
  # Display today's date
  output$sim_today <- renderInfoBox({
    potomac.ts.df <- ts$flows 
    sim_today <- last(potomac.ts.df$date_time)
    infoBox(
    title = "Today's date",
    value = sim_today,
    icon = shiny::icon("calendar"),
    color = "blue"
    )
  })
  #------------------------------------------------------------------
  # Output today's flow at Point of Rocks, and compare with 2000 cfs trigger
  output$por_flow <- renderValueBox({
    por_threshold <- 2000 # (cfs) CO-OP's trigger for daily monitoring/reporting
    potomac.ts.df <- ts$flows
    por_flow <- paste("POR flow = ",
                      round(last(potomac.ts.df$por_nat)*mgd_to_cfs),
                            " cfs")
    valueBox(
      value = tags$p(por_flow, style = "font-size: 80%;"),
      subtitle = "Flow at Point of Rocks",
      color = if (por_flow >= por_threshold) "green" else "yellow"
    )
  })
  #------------------------------------------------------------------
  # Output today's demand
  output$demand <- renderValueBox({
    potomac.ts.df <- ts$flows
    demand <- paste("Demand = ", 
                    round(last(potomac.ts.df$demand)), 
                    " MGD")
    valueBox(
      value = tags$p(demand, style = "font-size: 80%;"),
      subtitle = NULL,
      color = "yellow"
    )
  })  
  #------------------------------------------------------------------
  # Let total WMA Potomac River withdrawals = W
  # and adjusted flow at Little Falls = Little Falls obs + W = Qadj
  # The LFAA Alert stage is triggered when 
  #     W >= 0.5*Qadj
  # or equivalently, W/0.8 < Qadj <= W/0.5
  # Restrictions stage: (W + 110)??? < Qadj <= (W + 100)/0.8
  # Emergency stage???: Qadj < W + 110???
  # The problem is the definition of Emergency stage is probabalistic
  #   - will do some modeling to get a better surrogate definition
    output$lfaa_alert <- renderInfoBox({
    potomac.ts.df <- ts$flows
    W <- last(potomac.ts.df$demand*1.0)
    Qadj <- round(last(potomac.ts.df$lfalls_adj*1.0))
    if(Qadj > W/0.5) {
      text_stage <- "NORMAL"
      color_stage <- "green"}
    if(Qadj <= W/0.5 & Qadj > (W + 100)/0.8){
      text_stage <- "ALERT"
      color_stage <- "yellow"}
    if(Qadj <= (W + 100)/0.8 & Qadj > W + 110){
      text_stage <- "RESTRICTION"
      color_stage <- "orange"}
    if(Qadj <= (W + 110)){
      text_stage <- "EMERGENCY"
      color_stage <- "red"}
#
    infoBox(
      title = "LFAA Stage",
      value = paste(Qadj, text_stage),
      subtitle = "Little Falls adj. flow, MGD",
      icon = shiny::icon("arrow"),
      color = color_stage
    )
  })
  #
  #------------------------------------------------------------------
    output$jrrStorageReleases <- renderPlot({
    jrr.graph <- ts$jrr %>%
      filter(date_time >= input$plot_range[1],
             date_time <= input$plot_range[2])
    ggplot(data = jrr.graph, aes(x = date_time)) +
      geom_line(aes(y = storage, color = "Storage")) +
      geom_line(aes(y = outflow, color = "Outflow")) +
      scale_color_manual(values = c("grey", "black"))
    #    scale_linetype_manual(values = c("solid", "dotted"))
  }) # end renderPlot
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
    #    scale_linetype_manual(values = c("solid", "dotted"))
  }) # end renderPlot
  #------------------------------------------------------------------
  mde_map = "http://mde.maryland.gov/programs/Water/droughtinformation/Currentconditions/PublishingImages/DroughtGraphsStarting2017Apr30/Drought2018-04-30.png"
  output$MDEStatus <- renderText({c('<img src="', mde_map, '">')
  })
  
  vadeq_map = "http://deq1.bse.vt.edu/drought/state/images/maps/imageMapFile152838207923720.png"
  output$VADEQStatus <- renderText({c('<img src="', vadeq_map, '">')
  })
  #------------------------------------------------------------------
  })

