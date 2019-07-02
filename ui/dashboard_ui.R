dashboard_ui<-function(){
  # fluidRow(
  #   uiOutput("surveyDataCheck", inline=TRUE),
  #   plotOutput("ruuviSync",height = "600px")
  #   )
      
  navbarPage(
    "",id="ComplainceTabs",
    tabPanel(
      "Daily Survey Data",
      wellPanel(
        column(3,dateInput("complainceDate","Select Date")),
        br(),
        DT::dataTableOutput("daily_survey_data_table")
      )
    ),
    tabPanel(
      "Ruuvu Tag Sync",
      wellPanel(
        plotOutput("ruuviSync",height = "600px")
      )
    )
  )
  
}

DailySurveyData<-function(input, output){
  output$daily_survey_data_table<-DT::renderDataTable(
    {
      req(input$complainceDate)
      
      referenceDate=input$complainceDate
     
      query='select 
        	distinct p.data->>"$.participantEmail" as participantEmail,
            participantId,entryDate, 
            d->>"$.regulateToday" as regulateToday,
          	d->>"$.symptomShortness" as shortnessOfBreath,
            d->>"$.symptomCough" as cough,
            d->>"$.symptomPhlegm" as phlegm,
          	d->>"$.symptomWheezing" as wheezing,
          	d->>"$.frequencyNocturnal" as freqNocturnalWakeups,
            d->>"$.frequencyOpenMeds" as openingMeds,
            d->>"$.estimationAsthmaBalance" as estimationAsthmaBalance,
            d->>"$.preventNormal" as preventNormalActivity,
		  	    d->>"$.otherObs" as otherObs
            from	
            (SELECT data->>"$.surveyData" as d, 
                       data->>"$.participantId" as participantId,
                       data->>"$.entryDate" as raw,
                       deviceId,
        			   from_unixtime(data->>"$.entryDate"/1000, "%Y-%m-%d %H:%i") as entryDate
        		from murad.surveyData) as x
                left join murad.participantData p on x.deviceId=p.deviceId
			where from_unixtime(raw/1000, "%Y-%m-%d")="#referenceDate#"
			order by participantId,entryDate desc;'
      
      query=gsub("#referenceDate#",referenceDate,query)
      data=loaddbData(query)
    
      DT::datatable(
         data,
         rownames = FALSE,
         class = 'cell-border stripe',
         options = list(
           dom = 'Bfrtip' ,
           buttons = list('csv'),
           pageLength = nrow(data)
         ),
         extensions = c("Responsive", "Buttons"),
         selection = 'single'
       )
    
      
    }
  )
}
ruuviSync<-function(output){
  
  timestamp_10_hours <- floor(as.numeric(Sys.time())*1000)-36000000
  data=loaddbData(paste('SELECT deviceId,Max(timestamp) as timestamp
                          FROM murad.IndoorsTag where timestamp<',
                        timestamp_10_hours," group by deviceId  order by timestamp desc",sep = '') )
 
    currentTime=floor(as.numeric(Sys.time())*1000)
    data$originalTimestamp<-data$timestamp
    data$timestamp<-trunc((currentTime-data$timestamp)/(1*60*60*1000)) #difftime(floor(as.numeric(Sys.time(),origin = "1970-01-01")*1000),data$timestamp,units = "hours")
    
    colnames(data)[2] <- "last_sync"
  colourCount1 = length(unique(data$deviceId))


  output$ruuviSync<-renderPlot({
   p<- ggplot(data, aes(
                      x = deviceId, 
                      y = last_sync, 
                      fill=factor(deviceId)
                      )) + 
            geom_bar(stat="identity") + 
            labs(title = "Time in hours since ruuvitags synced in the last 10 hours", x="RuuviTag" ,y = "Last Sync (hours)") + 
            theme(
                  plot.title = element_text(hjust = 0.5, size = 20, face="bold"), 
                  axis.title.x=element_blank(), axis.title=element_text(size=15),
                  axis.text=element_text(size=12),
                  axis.text.x = element_text(angle=70, vjust=0.5)
                  ) + 
           guides(fill=FALSE) + 
      background_grid(major = "xy", minor = "none")
   p
  }
  # ,cacheKeyExpr={
  #   sum(data$originalTimestamp)# if sum of timestamp doesnt change render same cached plot
  # }
  
  )
  }
  
   