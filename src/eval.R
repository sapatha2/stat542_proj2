# Evaluation script for this project
rm(list=ls())
train=read.csv("train.csv")
test=read.csv("test.csv")

source("mymain.R") 

month=3
year=2011

for(t in 1:1){
  # predict the weekly sales for month t, 
  predict();
  
  # newtest: sales data for this month; taking the
  # same format as "train". 
  tmp.filename = paste('xxx', t, '.csv', sep='');
  newtest=read.csv(tmp.filename)
  
  # Evaluate prediction accuracy p/month
  i=test[test$Mon==month & test$Yr == year, "IsHoliday"]
  Y.pred1=test[test$Mon==month & test$Yr == year, "Weekly_Pred1"]
  Y.pred2=test[test$Mon==month & test$Yr == year, "Weekly_Pred2"]
  Y.pred3=test[test$Mon==month & test$Yr == year, "Weekly_Pred3"]
  Y=newtest$Weekly_Sales
  
  E1=sum(abs(Y.pred1-Y)[i])*5+sum(abs(Y.pred1-Y)[!i])
  E2=sum(abs(Y.pred2-Y)[i])*5+sum(abs(Y.pred2-Y)[!i])
  E3=sum(abs(Y.pred3-Y)[i])*5+sum(abs(Y.pred3-Y)[!i])
  d=sum(i)*5+sum(!i)
  print(E1/d)
  print(E2/d)
  print(E3/d)
}





