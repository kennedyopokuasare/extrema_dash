dashboard_ui<-function(){
  # fluidRow(
  #   uiOutput("surveyDataCheck", inline=TRUE),
  #   plotOutput("ruuviSync",height = "600px")
  #   )
      
  navbarPage(
    "",id="ComplainceTabs",
    tabPanel(
      "Ruuvu Tag Sync",
      wellPanel(
        plotOutput("ruuviSync",height = "600px")
      )
    ),
    tabPanel(
      "Compliance",
      wellPanel(
        DT::dataTableOutput("compliance_overview_table")
      )
    ),
    tabPanel(
      "Daily Survey Data",
      wellPanel(
        column(3,dateInput("complainceDate","Select Date")),
        br(),
        DT::dataTableOutput("daily_survey_data_table")
      )
    ),
    tabPanel(
      "Daily Location Data",
      wellPanel(
        fluidRow(
          column(3,dateInput("locationDate","Select Date")),
          column(3,textInput("location_compliance_participantId","Enter Participant"))
        ),
        br(),
        DT::dataTableOutput("daily_locaiton_data_table")
      )
    )
     
    
  )
  
}

participantLastEntries<-function(input,output){
  locationQuery='SELECT 
                    lower(p.data->>"$.participantEmail") as participantEmail,
                  	p.data->>"$.participantId" as participantId,
                	  max(from_unixtime(l.data->>"$.entryDate"/1000, "%Y-%m-%d %H:%i")) as location
                 FROM murad.participantData p left join murad.locationData l on p.deviceId=l.deviceId
                 group by participantId,participantEmail
                 order by participantId,location desc;'
  locationData=data=loaddbData(locationQuery)
  surveyQuery='SELECT 
                	lower(p.data->>"$.participantEmail") as participantEmail,
                	p.data->>"$.participantId" as participantId,
                	max(from_unixtime(s.data->>"$.entryDate"/1000, "%Y-%m-%d %H:%i")) as survey
               FROM murad.participantData p left join murad.surveyData s on p.deviceId=s.deviceId
               group by participantId,participantEmail
               order by participantId,survey desc;'
  surveydata=loaddbData(surveyQuery)
  data=merge(surveydata,locationData,by="participantId")[,c("participantEmail.x","participantId","survey","location")]
  names(data)[names(data)=="participantEmail.x"]<-"participantEmail"
  data=data%>%arrange(desc(survey),desc(location))
  output$compliance_overview_table<-DT::renderDataTable(
    {
      DT::datatable(
        data,
        class = 'cell-border stripe',
        caption = "The table shows the date of the last entries received from each participant",
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

DailySurveyData<-function(input, output){
  output$daily_survey_data_table<-DT::renderDataTable(
    {
      req(input$complainceDate)
      
      referenceDate=input$complainceDate
      
     
      query='select 
            	distinct participantId,entryDate, 
              CASE 
          	    	WHEN d->>"$.regulateToday" IS NULL THEN d->>"$.frequencyOpenMeds" 
                  ELSE d->>"$.regulateToday"
          	  END as regulateToday,
            	d->>"$.symptomShortness" as shortnessOfBreath,
              d->>"$.symptomCough" as cough,
              d->>"$.symptomPhlegm" as phlegm,
            	d->>"$.symptomWheezing" as wheezing,
            	d->>"$.frequencyNocturnal" as freqNocturnalWakeups,
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
         class = 'cell-border stripe',
         caption = "Table show the survey entries for a particular day. Select date of interest above to show survey entries",
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
dailyLocationData<-function(input,output){
  output$daily_locaiton_data_table<-DT::renderDataTable(
    {
      req(input$locationDate)
      
      referenceDate=input$locationDate
      refParticipantId=trimws(input$location_compliance_participantId)
      #print(paste(referenceDate))
      
      query='SELECT 
      distinct
      data->>"$.participantId" as participantId,
      deviceId,
       from_unixtime(data->>"$.entryDate"/1000, "%Y-%m-%d %H:%i") as entryDate,
       data->>"$.latitude" as latitude,
       data->>"$.longitude" as longitude,
       data->>"$.accuracy" as accuracy,
       data->>"$.isIndoors" as isIndoors,
       data->>"$.satellites" as satellites,
       data->>"$.source" as source,
       data->>"$.speed" as speed
 FROM murad.locationData 
 where from_unixtime( json_unquote(json_extract(data,"$.entryDate"))/1000, "%Y-%m-%d")="#referenceDate#"
 order by participantId,entryDate desc;'
      
      if(refParticipantId!='')
      {
        query='SELECT 
      distinct
      data->>"$.participantId" as participantId,
      deviceId,
       from_unixtime(data->>"$.entryDate"/1000, "%Y-%m-%d %H:%i") as entryDate,
       data->>"$.latitude" as latitude,
       data->>"$.longitude" as longitude,
       data->>"$.accuracy" as accuracy,
       data->>"$.isIndoors" as isIndoors,
       data->>"$.satellites" as satellites,
       data->>"$.source" as source,
       data->>"$.speed" as speed
 FROM murad.locationData 
 where from_unixtime( json_unquote(json_extract(data,"$.entryDate"))/1000, "%Y-%m-%d")="#referenceDate#"
      and json_unquote(json_extract(data,"$.participantId"))="#refPartipantId#"
 order by participantId,entryDate desc;'
        
        query=gsub("#refPartipantId#",refParticipantId,query)
      }
        
      query=gsub("#referenceDate#",referenceDate,query)
      data=loaddbData(query)
      
      print(paste(query))
      DT::datatable(
        data,
        class = 'cell-border stripe',
        caption = "Table shows the GPS location entries for a particular day and per participant. 
                    Select date of interest above to show location entries",
        options = list(
          dom = 'Bfrtip' ,
          buttons = list('csv'),
          pageLength = nrow(data)
        ),
        extensions = c("Responsive", "Buttons"),
        selection = 'single'
      )
      
      
    })
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
  
   