#script for sourcing all necessary scripts. 

#---------------------------librarys--------------------------------------------
library(shiny)
library(shinydashboard)
library(corrplot)
library(plotly)
library(ggplot2)
library(rgdal)
library(broom)
library(maptools)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(htmltools)

#----------------------------scripts--------------------------------------------
source('Scripts/load_data.R')
source('Scripts/util_correlations.R')
source('Scripts/ui.R')
source('Scripts/server.R')
source('Scripts/map_functions.R')
source('Scripts/plot_functions.R')
source('Scripts/util_munc.R')

# Run the application 
shinyApp(ui, server)
