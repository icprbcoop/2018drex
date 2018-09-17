#------------------------------------------------------------------
#------------------------------------------------------------------
# First, server defines reactive values and uses 3 observeEvent's
#------------------------------------------------------------------
#------------------------------------------------------------------
shinyServer(function(input, output, session) {
  #
  # There's probably an easier way to do this!
  # Run the main simulation to the hard-coded input, date_today
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
  # Allow a change of the simulation end date and a re-run
  observeEvent(input$run_main, {
    ts <- sim_main_func(input$DREXtoday, ts)
  })
  #
  # Allow the user to add chunks of days to the simulation
  observeEvent(input$run_add, {
    ts <- sim_add_days_func(input$chunkofdays, ts)
  })
  #
  # Allow the user to write simulation output time series to file
  observeEvent(input$write_ts, {
    write.csv(ts$pat, paste(ts_output, "output.csv"))
  })
  #------------------------------------------------------------------
  #------------------------------------------------------------------
  # Second, server creates graphs and values for boxes to be displayed
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
    }) # end output$potomacFlows
  #------------------------------------------------------------------
  # Display today's date
  # output$sim_today <- renderInfoBox({
  #   potomac.ts.df <- ts$flows 
  #   sim_today <- last(potomac.ts.df$date_time)
  #   infoBox(
  #   title = "Today's date",
  #   value = sim_today,
  #   icon = shiny::icon("calendar"),
  #   color = "blue"
  #   )
  # })
  #------------------------------------------------------------------
  # Create today's date
  #------------------------------------------------------------------
    output$sim_today <- renderValueBox({
    potomac.ts.df <- ts$flows
    sim_today0 <- last(potomac.ts.df$date_time)
    sim_today <- paste("Today's date is ", sim_today0)
    valueBox(
      value = tags$p(sim_today, style = "font-size: 60%;"),
      subtitle = NULL,
      color = "black"
    )
  }) 
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
    output$coop_ops <- renderInfoBox({
    potomac.ts.df <- ts$flows
    por_flow <- last(potomac.ts.df$por_nat*mgd_to_cfs)
    if(por_flow > 2000) {
      text_stage <- "NORMAL"
      color_stage <- "green"}
    if(por_flow <= 2000) {
      text_stage <- "Daily monitoring & reporting"
      color_stage <- "yellow"}
    infoBox(
      title = "CO-OP operations status",
      value = paste(text_stage),
      subtitle = NULL,
      icon = shiny::icon("arrow"),
      color = color_stage
    )
    }) # end output$coop_ops
  #------------------------------------------------------------------
  # Output the LFAA stage
  # Let total WMA Potomac River withdrawals = W
  # and adjusted flow at Little Falls = Little Falls obs + W = Qadj
  # The LFAA Alert stage is triggered when 
  #     W >= 0.5*Qadj
  # or equivalently, W/0.8 < Qadj <= W/0.5
  # Restrictions stage: (W + 110)??? < Qadj <= (W + 100)/0.8
  # Emergency stage???: Qadj < W + 110???
  # The problem is the definition of Emergency stage is probabalistic
  #   - will do some modeling to get a better surrogate definition
  #------------------------------------------------------------------
  # Create info on LFAA stage
  #------------------------------------------------------------------
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
  }) # end output$lfaa_alert
  #
  #------------------------------------------------------------------
  # Create info on MWCOG Drought Plan stage
  #------------------------------------------------------------------
  output$mwcog_stage <- renderInfoBox({
    potomac.ts.df <- ts$flows
    por_flow <- last(potomac.ts.df$por_nat)
    if(por_flow > 2000) {
      text_stage <- "NORMAL - Wise Water Use"
      color_stage <- "green"}
    if(por_flow <= 2000) {
      # based on NOAA drought status - D1
      # then "notifications" upon 1st release, & when jrr+sen at 75%
      text_stage <- "WATCH - Voluntary Water Conservation"
      color_stage <- "yellow"}
    infoBox(
      title = "MWCOG drought stage",
      value = paste(text_stage),
      subtitle = NULL,
      icon = shiny::icon("arrow"),
      color = color_stage
    )
  }) # end output$mwcog_stage
  #------------------------------------------------------------------
  # Create graph of storage and releases for each reservoir
  #------------------------------------------------------------------
  output$jrrStorageReleases <- renderPlot({
    jrr.graph <- ts$jrr %>%
      filter(date_time >= input$plot_range[1],
             date_time <= input$plot_range[2])
    ggplot(data = jrr.graph, aes(x = date_time)) +
      geom_line(aes(y = storage, color = "Storage")) +
      geom_line(aes(y = outflow, color = "Outflow")) +
      scale_color_manual(values = c("grey", "black"))
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
  # misc placeholders for state drought status info
  #------------------------------------------------------------------
    mde_map = "http://mde.maryland.gov/programs/Water/droughtinformation/Currentconditions/PublishingImages/DroughtGraphsStarting2017Apr30/Drought2018-04-30.png"
  output$MDEStatus <- renderText({c('<img src="', mde_map, '">')
  })
  
  vadeq_map = "http://deq1.bse.vt.edu/drought/state/images/maps/imageMapFile152838207923720.png"
  output$VADEQStatus <- renderText({c('<img src="', vadeq_map, '">')
  })
  #------------------------------------------------------------------
  }) # end shinyServer

