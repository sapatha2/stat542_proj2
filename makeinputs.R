# Convert train.csv into mytrain.csv, mytest.csv, and myxxx1.csv -> myxxx20.csv as 
# shown in project.html
# NOT required for actual submission, but makes it so we can do testing

# Read in full training data (change the location to where your original train.csv)
# file downloaded from Kaggle is
setwd("/Users/shiveshpathak/Documents/School Work/STAT 542/Rcode/stat542_proj2")
train=read.csv("Kaggle Data/orig-train.csv")
train$Date=as.Date(train$Date)

# mytrain.csv should only go from 2010-02 to 2011-02
mytrain=train[train$Date<as.Date("2011-03-01"),]
mytest=train[train$Date>=as.Date("2011-03-01"),]
myxxx=train[train$Date>=as.Date("2011-03-01"),]

# mytest.csv is everything else, drop the Weekly_Sales and add Weekly_pred(1-3)
mytest=subset(mytest,select=-Weekly_Sales)
mytest$Weekly_Pred1=0
mytest$Weekly_Pred2=0
mytest$Weekly_Pred3=0

# Write these two files
write.csv(mytrain,'train.csv',row.names=FALSE)
write.csv(mytest,'test.csv',row.names=FALSE)

# Make myxxx(1-20).csv files
year=2011
month=3
for(t in 1:20){
  tmp.filename = paste('xxx', t, '.csv', sep='')
  tmp=myxxx[as.numeric(format(myxxx$Date,"%Y"))==year,]
  tmp=tmp[as.numeric(format(tmp$Date,"%m"))==month,]
  write.csv(tmp,tmp.filename,row.names=FALSE)
  if(month<12){
    month=month+1
  }else{
    month=1
    year=year+1
  }
}