source("simple.R")
source("forest.R")
source("predict3.R")

predict<-function(){
  # Append and clean the data
  # Append data
  if(exists("newtest")){
    newtest$Day=NA
    newtest$Wk=NA
    newtest$Mon=NA
    newtest$Yr=NA
    newtest$Days=NA
    newtest$dayHoliday=NA
    newtest$logsales=NA
    newtest$tDays=NA
    newtest$days30=NA
    train<<-rbind(train,newtest) #Altering a globally defined variable
  }
  
  # Define derived quantities
  train$Date <<- as.Date(train$Date, '%Y-%m-%d')
  test$Date <<- as.Date(test$Date, '%Y-%m-%d')
  
  library(lubridate)
  train$Yr <<- year(train$Date)
  test$Yr <<- year(test$Date)
  
  train$Mon <<- month(train$Date)
  test$Mon <<- month(test$Date)
  
  train$Day <<- day(train$Date)
  test$Day <<- day(test$Date)
  
  train$Days <<- (train$Mon-1)*30 + train$Day
  test$Days <<- (test$Mon-1)*30 + test$Day
  
  train$dayHoliday[train$IsHoliday] <<- train$Days[train$IsHoliday]
  train$dayHoliday[!train$IsHoliday] <<- 0
  test$dayHoliday[test$IsHoliday] <<- test$Days[test$IsHoliday]
  test$dayHoliday[!test$IsHoliday] <<- 0
  
  train$logsales <<- log(4990+train$Weekly_Sales)

  train$tDays <<- 360*(train$Yr-2010) + (train$Mon-1)*30 + train$Day
  train$days30 <<- (train$Mon-1)*30 + train$Day
  test$tDays <<- 360*(test$Yr-2010) + (test$Mon-1)*30 + test$Day
  test$days30 <<- (test$Mon-1)*30 + test$Day
  
  train.wk = train$Date
  train.wk = train.wk - train.wk[1]  # date is now 0, 7, 14, ...
  train.wk = train.wk/7 + 5  # make 2010-2-5 as '5', and date becomes continuous integers, i.e., 5, 6, 7, ...
  train.wk = as.numeric(train.wk) %% 52  ## 52 weeks in a year
  train$Wk <<- train.wk
  
  test.wk = test$Date
  test.wk = test.wk - test.wk[1]
  test.wk = test.wk/7 + 44 # make 2012-11-02 as '44'.
  test.wk = as.numeric(test.wk) %% 52
  test$Wk <<- test.wk
  
  # Do prediction
  simple_pred()
  random_forest()
  predict3()
  
  # Write output file
  write.csv(test,"test.csv",row.names=FALSE)
}