#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# This is the user-interface of a Shiny web app for the 2018 DREX.
# Run the application by clicking 'Run App' above.
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
dashboardPage(skin = "blue",
  dashboardHeader(title = "WMA 2018 DREX",
                  .list = NULL, 
                  tags$li(textOutput("date_text"),
                          class = "dropdown")
                  ),
  #--------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------
  # The sidebar - has the user inputs and controls
  #--------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------
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
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# The body - has the graphs and other output info
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "CSS/main.css")),
    #--------------------------------------------------------------------------------
    # Potomac River flows graph
    #--------------------------------------------------------------------------------
        fluidRow(
      column(
        width = 8,
        box(
          title = "Potomac River flow",
          width = NULL,
          plotOutput("potomacFlows", height = "220px")
          )
        ),
      #--------------------------------------------------------------------------------
      # Output to the left of the Potomac River flows graph
      #--------------------------------------------------------------------------------
        column(
        width = 4,
#        infoBoxOutput("sim_today", width = NULL),
#        valueBoxOutput("sim_today", width = NULL),
        valueBoxOutput("por_flow", width = NULL),
# may not have space for displaying demand:
#        valueBoxOutput("demand", width = NULL),
#        valueBoxOutput("lfalls_adj", width = NULL),
        valueBoxOutput("lfalls_obs", width = NULL),
        infoBoxOutput("coop_ops", width = NULL)
      )
    ), # end fluidRow with Potomac flows
    #
    #--------------------------------------------------------------------------------
    # The four reservoir storage/release graphs
    #--------------------------------------------------------------------------------
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
      #--------------------------------------------------------------------------------
      # Output to the left of the reservoir storage graphs
      #--------------------------------------------------------------------------------
        column(
        width = 4,
        infoBoxOutput("lfaa_alert", width = NULL),
        infoBoxOutput("mwcog_stage", width = NULL),
        box(
          title = "MARYLAND DROUGHT STATUS",
          width = 6,
          height = 220,
          htmlOutput(outputId = "boxes")
          ),
          #tags$p("Western region: Drought Watch; Central region: Drought Warning")),
        box(
          title = "VIRGINIA DROUGHT STATUS",
          width = 6,
          height = 220,
          htmlOutput(outputId = "boxes2")
          )
          #"NoVa: Drought Watch; Shenandoah: Drought Emergency")
        )
    ), # end fluidRow with reservoir storage
#
#--------------------------------------------------------------------------------
# Temporary row to display some output for QAing
#--------------------------------------------------------------------------------
# 
    fluidRow(
      valueBoxOutput("QA_out", width = NULL) 
      ) # end fluidRow for QAing purposes
      ) # end dashboardBody
) # end dashboardPage

