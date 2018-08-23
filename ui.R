# 
# This is the user-interface of a Shiny web application for the 2018 DREX.
# Run the application by clicking 'Run App' above.
#

dashboardPage(skin = "blue",
  dashboardHeader(title = "WMA 2018 DREX"),
  dashboardSidebar(
    width = 250,

dateRangeInput("plot_range",
               "Specify plot range",
               start = "1929-10-01",
               end = "1930-12-31",
               format = "yyyy-mm-dd",
               width = NULL), #"250px"),

dateInput("DREXtoday",
          "Change today's date",
          value = "1930-06-01",
          min = "1929-10-02",
          max = "1931-12-31",
          format = "yyyy-mm-dd"),

actionButton("run_main",
             "Run simulation",
             icon = NULL,
             width = "150px"),

numericInput("chunkofdays",
             "Chunk of days",
             value = 7,
             min = 1,
             max = 30,
             width = "180px"),

actionButton("run_add",
             "Add days to simulation",
             icon = NULL,
             width = "150px")
      ),
  dashboardBody(

    fluidRow(
      column(
        width = 10,
        box(
          title = "Potomac River flow",
          width = NULL,
          plotOutput("potomacFlows")
          )
        ),
      column(
        width = 2,
      
        infoBoxOutput("sim_today", width = NULL),
        valueBoxOutput("por_flow", width = NULL),
        tags$head( 
          tags$style(HTML(".fa { font-size: 12px; }"))),  
        infoBoxOutput("lfaa_alert", width = NULL),
        valueBoxOutput("demand", width = NULL)
      )
    ), # end fluidRow with Potomac flows
    fluidRow(
      tabBox(
#        title = "Storage in system reservoirs",
        width = 10,
        # the id lets us use input$tabset1 on the server to find current tab
        tabPanel("Jennings Randolph",
          plotOutput("jrrStorageReleases")
          ),
        tabPanel("Little Seneca",
          plotOutput("senStorageReleases")
          )
        ),
        box(
          title = "Storage triggers",
          width = 2,
          "voluntary, mandatory")
), # end fluidRow with reservoir storage
    fluidRow(
      box(
       title = "MD drought status map",
      width = 3,
      tags$img(src = "http://mde.maryland.gov/programs/Water/droughtinformation/Currentconditions/PublishingImages/DroughtGraphsStarting2017Apr30/Drought2018-01-31.png",
                          width = "300px",
                          height = "300px")),
      box(
        title = "VA drought status map",
        width = 3,
        tags$img(src = "http://deq1.bse.vt.edu/drought/state/images/maps/imageMapFile152838207923720.png",
                 width = "300px",
                 height = "300px")),
      box(
        title = "State restrictions",
        width = 4),
      box(
        title = "MWCOG restrictions",
        width = 2)
      ) # end fluidRow with state and COG drought status/restrictions
      ) # end dashboardBody
) # end dashboardPage

