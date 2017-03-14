# Main script 
train$Date=as.Date(train$Date)
test$Date=as.Date(test$Date)

source("simple.R")
# source("pred2.R")
# source("pred3.R")

predict<-function(){
  if(exists("newtest")){
    newtest$Date=as.Date(newtest$Date)
    train<<-rbind(train,newtest) #Altering a globally defined variable
  }
  simple_pred()
  # predict2()
  # predict3()
}