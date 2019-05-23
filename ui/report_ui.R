library(dplyr)
report_ui<-function(){
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "participantId", label = "Participant ID", choices = NULL)
      # ,
      # #textInput("participantId", "Participant ID"),
      # actionButton(
      #   "view_button",
      #   label = "View Report",
      #   class = "btn-primary",
      #   style = "color:#FFF",
      #   icon = icon("chart-bar")
      # )
      ,width = 2
      
    ),
    mainPanel(
        wellPanel(
          leafletOutput("map"),
          plotOutput("symptoms",height = "600px")
          )
        
      )
    
  )
}

report_server<-function(input, output,session){
  participants=as.data.frame(options()$participantsDataSet)
  surveyDataset=as.data.frame(options()$surveyDataSet)

  updateSelectInput(session, "participantId",
                    choices = unique(participants$participantId),selected = NULL)
  
  observeEvent(input$participantId,{
    
    output$map<-renderLeaflet({
    req(input$participantId)
    
    location=as.data.frame(options()$locationDataSet)%>%
                  dplyr::filter(participantId==trimws(input$participantId))%>%
                 #dplyr::select(latitude,longitude)%>%
                dplyr::mutate(latitude=as.numeric(latitude),longitude=as.numeric(longitude))
   
    avLat=mean(as.numeric(location$latitude))
    avlog=mean(as.numeric(location$longitude))
    #leaflet()%>%setView(lng=avlog,lat=avLat,zoom = 4)%>%addTiles()
    m <- leaflet()%>%setView(lng=avlog,lat=avLat,zoom = 12)
    m %>% addTiles()%>% addCircleMarkers( 
            data = location[,] ,
            lng = ~longitude,
            lat =~latitude,
            label = ~as.character(paste("Accuracy:",accuracy,
                                        " , Indoors:",isIndoors,
                                        " , Date:",entryDate,
                                        sep = " ") )
            )
   
  })
    # output$symptoms<-renderCachedPlot({
    #   req(input$participantId)
    #   
    #   surveyData=surveyDataset%>%
    #     dplyr::filter(participantId==trimws(input$participantId))%>%
    #     dplyr::select(shortnessOfBreath,cough,phlegm)
    #   m->melt
    # },cacheKeyExpr = {})
  })
  
}
