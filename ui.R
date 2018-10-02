# 
# This is the user-interface of a Shiny web app for the 2018 DREX.
# Run the application by clicking 'Run App' above.
#

dashboardPage(skin = "blue",
  dashboardHeader(title = "WMA 2018 DREX",
                  .list = NULL),
  dashboardSidebar(
    width = 250,

dateRangeInput("plot_range",
               "Specify plot range",
#               start = "1929-10-01",
#               end = "1930-12-31",
               start = date_start,
               end = date_end,
               format = "yyyy-mm-dd",
               width = NULL), #"250px"),

dateInput("DREXtoday",
          "Change last day of simulation",
          value = date_today, # date_today set in global.R
#          min = "1929-10-02",
#          max = "1931-12-31",
          min = date_start,
          max = date_end,
          format = "yyyy-mm-dd"),

actionButton("run_main",
             "Re-run simulation",
             icon = NULL,
             width = "150px"),
br(),br(),
numericInput("chunkofdays",
             "Chunk of days",
             value = 7,
             min = 1,
             max = 30,
             width = "180px"),

actionButton("run_add",
             "Add days to simulation",
             icon = NULL,
             width = "150px"),
br(), br(), br(), br(), br(),
br(), br(), br(), br(), br(),
br(), br(), br(), br(), br(),
actionButton("write_ts",
             "Write output time series",
             icon = NULL,
             width = "180px")
      ),
  dashboardBody(

    fluidRow(
      column(
        width = 8,
        box(
          title = "Potomac River flow",
          width = NULL,
          plotOutput("potomacFlows", height = "240px")
          )
        ),
      column(
        width = 4,
#        infoBoxOutput("sim_today", width = NULL),
#        valueBoxOutput("sim_today", width = NULL),
        valueBoxOutput("por_flow", width = NULL),
        valueBoxOutput("demand", width = NULL),
#        valueBoxOutput("lfalls_adj", width = NULL),
        valueBoxOutput("lfalls_obs", width = NULL),
        infoBoxOutput("coop_ops", width = NULL)
      )
    ), # end fluidRow with Potomac flows
    #
    fluidRow(
      column(
        width = 4,
        box(
          title = "Jennings Randolph",
          width = NULL,
          plotOutput("jrrStorageReleases", height = "150px")
        ),
        box(
          title = "Occoquan",
          width = NULL,
          plotOutput("occStorageReleases", height = "150px")
        )
      ),
      column(
        width = 4,
        box(
          title = "Little Seneca",
          width = NULL,
          plotOutput("senStorageReleases", height = "150px")
        ),
        box(
          title = "Patuxent",
          width = NULL,
          plotOutput("patStorageReleases", height = "150px")
        )
      ), 
      column(
        width = 4,
#        tags$head( 
#          tags$style(HTML(".fa { font-size: 12px; }"))), 
        infoBoxOutput("lfaa_alert", width = NULL),
        infoBoxOutput("mwcog_stage", width = NULL),
        box(
          title = "PLACE-HOLDER FOR MARYLAND DROUGHT STATUS",
          width = NULL,
          tags$p("Western region: Drought Watch; Central region: Drought Warning")),
        box(
          title = "PLACE-HOLDER FOR VIRGINIA DROUGHT STATUS",
          width = NULL,
          "NoVa: Drought Watch; Shenandoah: Drought Emergency")
        )
    ), # end fluidRow with reservoir storage
#
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

