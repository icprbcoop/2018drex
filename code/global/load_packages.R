# List of packages and code to load packages was copied from Zach's code.
# He copied the scripts from:
# https://www.r-bloggers.com/install-and-load-missing-specifiedneeded-packages-on-the-fly/
#
need <- c("shiny",
          "shinythemes",
          "shinydashboard",
          "ggplot2",
          "dplyr",
          "rlang",
          "data.table",
          "stringi",
          #"plotly",
          "Cairo",
          "RcppRoll",
          "tidyr",
          "lubridate",
          "pryr",
          "zoo",
          "sp",
          "leaflet",
          "rgdal") 
# find out which packages are installed
ins <- installed.packages()[, 1] 
# check if the needed packages are installed
(Get <-
    need[which(is.na(match(need, ins)))]) 
# install the needed packages if they are not-installed
if (length(Get) > 0) {
  install.packages(Get)
} 
# load the needed packages
eval(parse(text = paste("library(", need, ")")))
rm(Get, ins, need)
#------------------------------------------------------------------------------
options(shiny.usecairo = TRUE)