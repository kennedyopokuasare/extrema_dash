dashboard_ui<-function(){
  fluidRow(
  plotOutput("ruuviSync",height = "600px")
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
  print(paste(sum(data$originalTimestamp)))
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
  
   