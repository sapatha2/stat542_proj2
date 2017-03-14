# Evaluation script for this project
rm(list=ls())
train=read.csv("train.csv")
test=read.csv("test.csv")

source("mymain.R") 

for(t in 1:1){
  
  # predict the weekly sales for month t, 
  # e.g., month 1 --> 2011-03, and month 20 --> 2012-10. 
  predict();
  
  # newtest: sales data for this month; taking the
  # same format as "train". 
  tmp.filename = paste('xxx', t, '.csv', sep='');
  newtest=read.csv(tmp.filename)
  
  # Evaluate your prediction accuracy for this month
  if(t+2<=12){
    month=t+2
    year=2011
  }else{
    month=(t+2)-12
    year=2012
  }
  
  # Loop over all test cases with the correct month and year (lazy coding)
  tmp=test[month==as.numeric(format(test$Date,"%m")) & year==as.numeric(format(test$Date,"%Y")),]
  E1=0
  d1=0
  for(i in nrow(tmp)){
    if(newtest$IsHoliday[i]){
      E1=E1+abs(tmp$Weekly_Pred1[i]-newtest$Weekly_Sales[i])*5
      d1=d1+5
    }else{
      E1=E1+abs(tmp$Weekly_Pred1[i]-newtest$Weekly_Sales[i])
      d1=d1+1
    }
  }
  E1=E1/d1
  #MSE2=mean((tmp$Weekly_Pred2-newtest$Weekly_Sales)^2)
  #MSE2=mean((tmp$Weekly_Pred2-newtest$Weekly_Sales)^2)
  print(E1) #Old: 15378.16, New (all sep):, Last (avg over stores): 
}


