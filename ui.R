
suppressMessages(library(shiny,warn.conflicts =F))
suppressMessages(library(plotly,warn.conflicts =F))
suppressMessages(library(shinydashboardPlus,warn.conflicts =F))
suppressMessages(library(shinydashboard,warn.conflicts =F))
suppressMessages(library(shinyjs,warn.conflicts =F))
suppressMessages(library(tmap,warn.conflicts =F))
suppressMessages(library(xts,warn.conflicts =F))
suppressMessages(library(dygraphs,warn.conflicts =F))
suppressMessages(library(leaflet,warn.conflicts =F))


source("objects in UI.R")
source("new_model_UI_elements_Compute.R")
shinyUI(
  
  #titlePanel(tags$h4("EWARS-Dashboard")),
 uiOutput('log_list') 
  

      )

        



