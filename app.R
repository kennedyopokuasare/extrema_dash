library(shiny)
library(shinydashboard)
library(DT)
library(RMySQL)
library(jsonlite)
library(rmarkdown)
library(dplyr)
library(anytime)
library(ggplot2)
library(cowplot)

setwd(".")
source("ui/participants_ui.R",local=TRUE)
source("ui/dashboard_ui.R",local=TRUE)
source("db/dbaccess.R",local=TRUE)
ui <- dashboardPage(
  dashboardHeader(title = "Extrema"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    ),
    menuItem("Data", tabName = "studyData", icon = icon("user")),
    menuItem("Report", tabName = "report", icon = icon("chart-bar"))
  )),
  dashboardBody(
      tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              dashboard_ui()),
      
      # Second tab content
      tabItem(tabName = "studyData",
              participants_ui())
      # Third tab content
      
    
      
    )
  
  )
)

server <- function(input, output, session) {
  ruuviSync(output)
  showParticantList(output)
  showSurveyData(output)
  showRuuviTagData(output)
  showBluetoothData(output)
  showlocationData(output)
}

shinyApp(ui, server)