# Create a new Row in the UI for selectInputs


participants_ui <- function() {
  navbarPage(
    "",
    tabPanel(
      "Participant List",
      wellPanel(
        DT::dataTableOutput("participants_table")
      )
    ),
    tabPanel(
      "Survey Data",
    # wellPanel(
    #     fluidRow(width=5,
    #       textInput("participantId","ParticipantId",width = 10) ,
    #       dateInput("entryDate","Entry Date"),
    #       actionButton("filterBtn","Filter",class = "btn-primary",
    #                    style = "color:#FFF",
    #                    icon = icon("search"))
    #     )
    #   )
    # ,br(),
      wellPanel(
        DT::dataTableOutput("survey_table")
      )
    ),
    tabPanel("Ruuvi Tag", wellPanel(
        DT::dataTableOutput("ruuviTag_table")
    
    )),
    
    tabPanel(
      "Bluetooth Data",
      wellPanel(
        DT::dataTableOutput("bluetooth_table")
      )
    ),
    
    tabPanel(
      "Location Data",
      wellPanel(
        DT::dataTableOutput("location_table")
      )
    )
  )
}


loadParticipants<-function(){
  query="SELECT  
	 data->>'$.participantName' as Name,
	 data->>'$.participantEmail' as Email,
	 data->>'$.participantId' as participantId,
	 data->>'$.ruuviTag' as ruuviTag,date_format(from_unixtime(data->>'$.onboardDate'/1000),'%W %M %e %Y %H:%i:%S')  as onboardDate,
	 data->>'$.uid' as uid,
	 deviceId
 FROM murad.participantData order by onboardDate desc;"
  return(loaddbData(query))
}
loadSurveyData<-function(){
  query=' select 
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
    d->>"$.isColdToday" as havingColdToday,
    d->>"$.isVisitDoctor" as visitedDoctorToday,
    d->>"$.rescueCount" as rescueCount,d->>"$.otherMeds" as otherMeds,d->>"$.otherObs" as otherObs
 FROM
	(
		SELECT data->>"$.surveyData" as d, 
               data->>"$.participantId" as participantId,
               data->>"$.entryDate" as raw,
			   date_format(from_unixtime(data->>"$.entryDate"/1000),"%W %M %e %Y %H:%i:%S")  as entryDate
		from murad.surveyData order by raw desc) as X;'
  return(loaddbData(query))
}
showSurveyData<-function(output){
  surveyData=  loadSurveyData()
  
  output$survey_table <-  DT::renderDataTable({
    DT::datatable(
      surveyData,
      options = list(
        dom = 'Brtip' ,
        buttons = list('csv'),
        lengthMenu = c(5, 30, 50),
        pageLength = 15
      ),
      extensions = c("Responsive", "Buttons"),
      selection = 'single'
    )
  })
}
showParticantList<-function(output){
  participants=  loadParticipants()
  options(
    participantsDataSet =participants 
  )
  output$participants_table <-  DT::renderDataTable({
    DT::datatable(
      participants,
      options = list(
        dom = 'Bfrtip' ,
        buttons = list('csv'),
        lengthMenu = c(5, 30, 50),
        pageLength = 20
      ),
      extensions = c("Responsive", "Buttons"),
      selection = 'single'
    )
  })
}
showRuuviTagData<-function(output){
  query='SELECT 
      timestamp, deviceId as ruuviTag,
      data->>"$.temperature" as temperature,
      data->>"$.humidity" as humidity,
      data->>"$.pressure" as pressure,
      data->>"$.acceleration" as acceleration,
      data->>"$.acceleration_x" as acceleration_X,
      data->>"$.acceleration_y" as acceleration_Y,
      data->>"$.acceleration_z" as acceleration_Z,
      data->>"$.battery" as battery
  FROM murad.IndoorsTag order by timestamp desc;'
data=loaddbData(query)
options(
  ruuviDataSet =data 
)
output$ruuviTag_table <-  DT::renderDataTable({
  DT::datatable(
    data,
    options = list(
      dom = 'Bfrtip' ,
      buttons = list('csv'),
      lengthMenu = c(5, 30, 50),
      pageLength = 20
    ),
    extensions = c("Responsive", "Buttons"),
    selection = 'single'
  )
})
}
showBluetoothData<-function(output){
  query='SELECT 
    data->>"$.participantId" as participantId,
    deviceId,
     data->>"$.btRSSI" as btRSSI,
     data->>"$.macAddress" as macAddress,
     date_format(from_unixtime(data->>"$.entryDate"/1000),"%W %M %e %Y %H:%i:%S") as entryDate,
     data->>"$.uid" as uid
 FROM murad.bluetoothData order by timestamp desc;'
  
  data=loaddbData(query)
  options(
    bluetoothDataSet =data 
  )
  output$bluetooth_table <-  DT::renderDataTable({
    DT::datatable(
      data,
      options = list(
        dom = 'Bfrtip' ,
        buttons = list('csv'),
        lengthMenu = c(5, 30, 50),
        pageLength = 20
      ),
      extensions = c("Responsive", "Buttons"),
      selection = 'single'
    )
  })
}
showlocationData<-function(output){
  query='SELECT 
      data->>"$.participantId" as participantId,
      deviceId,
       date_format(from_unixtime(data->>"$.entryDate"/1000),"%W %M %e %Y %H:%i:%S") as entryDate,
       data->>"$.latitude" as latitude,
       data->>"$.longitude" as longitude,
       data->>"$.accuracy" as accuracy,
       data->>"$.isIndoors" as isIndoors,
       data->>"$.satellites" as satellites,
       data->>"$.source" as source,
       data->>"$.speed" as speed,
       data->>"$.uid" as uid
 FROM murad.locationData order by timestamp desc;'
  
  data=loaddbData(query)
  options(
    locationDataSet =data 
  )
  output$location_table <-  DT::renderDataTable({
    DT::datatable(
      data,
      options = list(
        dom = 'Bfrtip' ,
        buttons = list('csv'),
        lengthMenu = c(5, 30, 50),
        pageLength = 20
      ),
      extensions = c("Responsive", "Buttons"),
      selection = 'single'
    )
  })
}
expandDataTable <- function(db_data) {
  if (is.null(db_data))
    return(db_data)
  results <- data.frame(
    participantId = character(),
    entryDate = character(), 
    regulatedAsthmaToday = character(),
    shortnessOfBreath = character(),
    cough = character(),
    phlegms = character(),
    wheezing = character(),
    freqNocturnal = character(),
    freqOpenMeds= character(),
    estimatedAsthmaBalance = character(),
    preventedFromActingNormal = character(),
    isColdToday = character(),
    visitedDoctorToday = character(),
    rescueCount = character(),
    otherMeds = character(),
    deviceId=character()
  )
  
  for (row in 1:nrow(db_data)) {
    json_field <- db_data[row, "surveyData"]
    id<-db_data[row, "participantId"]
    eDate<-db_data[row, "entryDate"]
    device<-db_data[row, "deviceId"]
    json_data = fromJSON(paste(json_field))
    results <- rbind(
      results,
      data.frame(
        participantId=id,
        entryDate=eDate,
        regulatedAsthmaToday = json_data$regulateToday,
        shortnessOfBreath = json_data$symptomShortness,
        cough = json_data$symptomCough,
        phlegms = json_data$symptomPhlegm,
        wheezing = json_data$symptomWheezing,
        freqNocturnal = json_data$frequencyNocturnal,
        freqOpenMeds = json_data$frequencyOpenMeds,
        estimatedAsthmaBalance = json_data$estimationAsthmaBalance,
        preventedFromActingNormal = json_data$preventNormal,
        isColdToday = json_data$isColdToday,
        visitedDoctorToday = json_data$isVisitDoctor,
        rescueCount = json_data$rescueCount,
        otherMeds = json_data$otherMeds,
        deviceId = device
      )
    )
  }
  
  return(results)
}