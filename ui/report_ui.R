library(dplyr)
report_ui<-function(){
  sidebarLayout(
    sidebarPanel(
      textInput("participantId", "Participant ID"),
      actionButton(
        "view_button",
        label = "View Report",
        class = "btn-primary",
        style = "color:#FFF",
        icon = icon("chart-bar")
      ),width = 2
      
    ),
    mainPanel(
        wellPanel(
          leafletOutput("map")
          )
        
      )
    
  )
}

report_server<-function(input, output,session){
 
  observeEvent(input$view_button,{
    
    output$map<-renderLeaflet({
    req(input$participantId)
    
    location=as.data.frame(options()$locationDataSet)%>%
                  dplyr::filter(participantId==trimws(input$participantId))%>%
                 #dplyr::select(latitude,longitude)%>%
                dplyr::mutate(latitude=as.numeric(latitude),longitude=as.numeric(longitude))
   print(paste(names(location)))
    avLat=mean(as.numeric(location$latitude))
    avlog=mean(as.numeric(location$longitude))
    #leaflet()%>%setView(lng=avlog,lat=avLat,zoom = 4)%>%addTiles()
    m <- leaflet()%>%setView(lng=avlog,lat=avLat,zoom = 12)
    m %>% addTiles()%>% addCircleMarkers( 
            data = location[,] ,
            lng = ~longitude,
            lat =~latitude,
            label = ~as.character(paste("Accuracy:",accuracy,
                                        " ,Indoors:",isIndoors,
                                        " ,Date:",entryDate,
                                        sep = " ") )
            )
   
  })
  })
  
}
