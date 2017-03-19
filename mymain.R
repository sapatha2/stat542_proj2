source("simple.R")
# source("pred2.R")
# source("pred3.R")

predict<-function(){
  # Append and clean the data
  # Append data
  if(exists("newtest")){
    newtest$Wk=NA
    newtest$Mon=NA
    newtest$Yr=NA
    train<<-rbind(train,newtest) #Altering a globally defined variable
  }
  
  # Define the years, months and weeks
  train$Date <<- as.Date(train$Date, '%Y-%m-%d')
  test$Date <<- as.Date(test$Date, '%Y-%m-%d')
  
  library(lubridate)
  train$Yr <<- year(train$Date)
  test$Yr <<- year(test$Date)
  
  train$Mon <<- month(train$Date)
  test$Mon <<- month(test$Date)
  
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
  # predict2()
  # predict3()
  
  # Write output file
  write.csv(test,"test.csv",row.names=FALSE)
}