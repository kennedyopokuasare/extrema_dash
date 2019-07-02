library(RMySQL)
library(jsonlite)
options(
  mysql = list(
    "host" ="co2.awareframework.com", #,54.171.58.227
    "host_" = "localhost",
    "port" = 3306,
    "user" = "murad",
    "user_" = "goals",
    "password" = "9frqvofesD",
    "password_" = "asare.pass",
    "dbname_" = "goals_study",
    "dbname" = "murad"
  
  ),
  globalData=list(
    location=""
  )
)
getdbConnection <- function(dbname=options()$mysql$dbname,host=options()$mysql$host,port=options()$mysql$port,user=options()$mysql$user, password = options()$mysql$password) {
  db <-
    dbConnect(
      MySQL(),
      dbname = dbname,
      host = host,
      port = port,
      user = user,
      password = password
   
    )
  return(db)
}

loaddbData<-function(query,dbconn=NULL){
  db=dbconn
  if(is.null(dbconn))db=getdbConnection()
  dbSendQuery(db,"SET NAMES utf8")
  results = dbGetQuery(db, query)
  dbDisconnect(db)
  return(results)
}

savejsonData<-function(data,dbTable=options()$mysql$dbTable_tests,dbconn=NULL){
  db=dbconn
  if(is.null(dbconn))db=getdbConnection()
  data=as.list(data)
  dbData <-
    c(
      "timestamp"=floor(as.numeric(Sys.time())*1000),
      "deviceId"= data[["device_id"]] ,
      "data"=toJSON(
        data,
        auto_unbox = TRUE,
        na = "null",
        null = "null"
      )
    )
  
  query <- sprintf(
    "INSERT INTO %s(%s) VALUES ('%s');",
    dbTable,
    paste(names(dbData), collapse = ", "),
    paste(dbData, collapse = "', '")
  )
  
  dbGetQuery(db, query)
  dbDisconnect(db)
}