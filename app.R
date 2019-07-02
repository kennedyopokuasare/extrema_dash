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
library(plotly)
library(leaflet)

setwd(".")
source("ui/participants_ui.R",local=TRUE)
source("ui/dashboard_ui.R",local=TRUE)
source("ui/report_ui.R",local=TRUE)
source("db/dbaccess.R",local=TRUE)
ui <- dashboardPage(
  dashboardHeader(title = "PEHMO"),
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
              participants_ui()),
      # Third tab content
      
      tabItem(tabName = "report",
              report_ui())
      
    )
  
  )
)

server <- function(input, output, session) {
  DailySurveyData(input,output)
  ruuviSync(output)
  showParticantList(output)
  showSurveyData(output)
  showRuuviTagData(output)
  showBluetoothData(output)
  showlocationData(output)
  report_server(input, output,session)
}

shinyApp(ui, server)