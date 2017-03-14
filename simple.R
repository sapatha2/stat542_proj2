# Simple prediction

# Just say that test[store,dept,y/m/*]={average over the last three months at that store/dept}
simple_pred <- function(){
  # Find the month and year we want to predict for
  if(t+2<=12){
    month=t+2
    year=2011
  }else{
    month=(t+2)-12
    year=2012
  }
  
  # Loop over all test cases with the correct month and year (lazy coding)
  # for(store in 1:45){
  #   for(dept in test[test$Store==store,]$Dept){
  #     tmp=test[test$Store==store & test$Dept==dept & month==as.numeric(format(test$Date,"%m")) & year==as.numeric(format(test$Date,"%Y")),]
  #     print(nrow(tmp))
  #     if(month>3){
  #       tmp1=train[train$Store==store & train$Dept==dept & month-1==as.numeric(format(train$Date,"%m")) & year==as.numeric(format(train$Date,"%Y")),]$Weekly_Sales
  #       tmp2=train[train$Store==store & train$Dept==dept & month-2==as.numeric(format(train$Date,"%m")) & year==as.numeric(format(train$Date,"%Y")),]$Weekly_Sales
  #       tmp3=train[train$Store==store & train$Dept==dept & month-3==as.numeric(format(train$Date,"%m")) & year==as.numeric(format(train$Date,"%Y")),]$Weekly_Sales
  #     }else if(month==3){
  #       tmp1=train[train$Store==store & train$Dept==dept & month-1==as.numeric(format(train$Date,"%m")) & year==as.numeric(format(train$Date,"%Y")),"Weekly_Sales"]
  #       tmp2=train[train$Store==store & train$Dept==dept & month-2==as.numeric(format(train$Date,"%m")) & year==as.numeric(format(train$Date,"%Y")),"Weekly_Sales"]
  #       tmp3=train[train$Store==store & train$Dept==dept & 12==as.numeric(format(train$Date,"%m")) & year-1==as.numeric(format(train$Date,"%Y")),"Weekly_Sales"]
  #     }else if(month==2){
  #       tmp1=train[train$Store==store & train$Dept==dept & month-1==as.numeric(format(train$Date,"%m")) & year==as.numeric(format(train$Date,"%Y")),]$Weekly_Sales
  #       tmp2=train[train$Store==store & train$Dept==dept & 12==as.numeric(format(train$Date,"%m")) & year-1==as.numeric(format(train$Date,"%Y")),]$Weekly_Sales
  #       tmp3=train[train$Store==store & train$Dept==dept & 11==as.numeric(format(train$Date,"%m")) & year-1==as.numeric(format(train$Date,"%Y")),]$Weekly_Sales
  #     }else{
  #       tmp1=train[train$Store==store & train$Dept==dept & 12==as.numeric(format(train$Date,"%m")) & year-1==as.numeric(format(train$Date,"%Y")),]$Weekly_Sales
  #       tmp2=train[train$Store==store & train$Dept==dept & 11==as.numeric(format(train$Date,"%m")) & year-1==as.numeric(format(train$Date,"%Y")),]$Weekly_Sales
  #       tmp3=train[train$Store==store & train$Dept==dept & 10==as.numeric(format(train$Date,"%m")) & year-1==as.numeric(format(train$Date,"%Y")),]$Weekly_Sales
  #     }
  #     print(store)
  #     print(dept)
  #     print("MIH1")
  #     tmp$Weekly_Pred1=(mean(tmp1)+mean(tmp2)+mean(tmp3))/3.0
  #     test[test$Store==store & test$Dept==dept & month==as.numeric(format(test$Date,"%m")) & year==as.numeric(format(test$Date,"%Y")),]<<-tmp
  #     print("MIH2")
  #   }
  # }
  tmp=test[month==as.numeric(format(test$Date,"%m")) & year==as.numeric(format(test$Date,"%Y")),]
  if(month>3){
    tmp1=train[month-1==as.numeric(format(train$Date,"%m")) & year==as.numeric(format(train$Date,"%Y")),]
    tmp2=train[month-2==as.numeric(format(train$Date,"%m")) & year==as.numeric(format(train$Date,"%Y")),]
    tmp3=train[month-3==as.numeric(format(train$Date,"%m")) & year==as.numeric(format(train$Date,"%Y")),]
  }else if(month==3){
    tmp1=train[month-1==as.numeric(format(train$Date,"%m")) & year==as.numeric(format(train$Date,"%Y")),]
    tmp2=train[month-2==as.numeric(format(train$Date,"%m")) & year==as.numeric(format(train$Date,"%Y")),]
    tmp3=train[12==as.numeric(format(train$Date,"%m")) & year-1==as.numeric(format(train$Date,"%Y")),]
  }else if(month==2){
    tmp1=train[month-1==as.numeric(format(train$Date,"%m")) & year==as.numeric(format(train$Date,"%Y")),]
    tmp2=train[12==as.numeric(format(train$Date,"%m")) & year-1==as.numeric(format(train$Date,"%Y")),]
    tmp3=train[11==as.numeric(format(train$Date,"%m")) & year-1==as.numeric(format(train$Date,"%Y")),]
  }else{
    tmp1=train[12==as.numeric(format(train$Date,"%m")) & year-1==as.numeric(format(train$Date,"%Y")),]
    tmp2=train[11==as.numeric(format(train$Date,"%m")) & year-1==as.numeric(format(train$Date,"%Y")),]
    tmp3=train[10==as.numeric(format(train$Date,"%m")) & year-1==as.numeric(format(train$Date,"%Y")),]
  }
  # Currently averaging over all departments and stores, leads to error of 15378.16
  # Make it so that doesn't happen!
  tmp$Weekly_Pred1=(mean(tmp1$Weekly_Sales)+mean(tmp2$Weekly_Sales)+mean(tmp3$Weekly_Sales))/3.0
  test[month==as.numeric(format(test$Date,"%m")) & year==as.numeric(format(test$Date,"%Y")),]<<-tmp
}