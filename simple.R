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
  
  # Average over the previous three months of data
  tmp=test[month==as.numeric(format(test$Date,"%m")) & year==as.numeric(format(test$Date,"%Y")),]
  tmp1=0
  tmp2=0
  tmp3=0
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
  
  # Previously I averaged over all departments, lead to a huge error! ~20000
  # Now I don't average over the departments/stores, reduces the error to ~5000
  for(dept in 1:99){
    if(sum(tmp$Dept==dept)!=0){
      one=mean(tmp1[tmp1$Dept==dept,"Weekly_Sales"])
      two=mean(tmp2[tmp2$Dept==dept,"Weekly_Sales"])
      three=mean(tmp3[tmp3$Dept==dept,"Weekly_Sales"])
      d=0
      if(!is.na(one)){
        tmp[tmp$Dept==dept,"Weekly_Pred1"]=tmp[tmp$Dept==dept,"Weekly_Pred1"]+one
        d=d+1
      }
      if(!is.na(two)){
        tmp[tmp$Dept==dept,"Weekly_Pred1"]=tmp[tmp$Dept==dept,"Weekly_Pred1"]+two
        d=d+1
      }
      if(!is.na(three)){
        tmp[tmp$Dept==dept,"Weekly_Pred1"]=tmp[tmp$Dept==dept,"Weekly_Pred1"]+three
        d=d+1
      }
      tmp[tmp$Dept==dept,"Weekly_Pred1"]=tmp[tmp$Dept==dept,"Weekly_Pred1"]/3.0
    }
  }
  test[month==as.numeric(format(test$Date,"%m")) & year==as.numeric(format(test$Date,"%Y")),]<<-tmp
}