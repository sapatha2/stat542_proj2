####RANDOM FOREST####

#import libraries
library(randomForest)

random_forest <- function(){
  test$Weekly_Pred2 <<- 0
  store = sort(unique(test$Store))
  n.store = length(store)
  dept = sort(unique(test$Dept))
  n.dept = length(dept)
  
  for (s in 1:n.store){
    for (d in 1:n.dept){
      test.id = which(test$Store == store[s] & test$Dept == dept[d])
      cat("Forest, Store: ", store[s], "\t Dept:", dept[d], "\n")
      dataF1=train[train$Dept==dept[d],]
      tmpL = nrow(dataF1[dataF1$Store==store[s],])
      tmpF = dataF1[dataF1$IsHoliday==1,]
      dataF1 = rbind(dataF1,do.call("rbind", replicate(4, tmpF, simplify = FALSE)))
      dataF2 = dataF1[dataF1$Store==store[s],]  
      testF1 = test[test$Dept==dept[d],]
      testF1 = testF1[testF1$Store==store[s],]
      testRows = nrow(testF1)
      tmpModel=NULL
      if(tmpL>0 & testRows>0){
        if(tmpL<10){
          tmpModel =  randomForest(logsales~ Yr + Mon + Day + Days + dayHoliday + tDays + days30, 
                               ntree=4800, replace=TRUE, mtry=4, data=dataF1)
        }else{
          tmpModel =  randomForest(logsales ~ Yr + Mon + Day + Days + dayHoliday + tDays + days30, 
                               ntree=4800, replace=TRUE, mtry=3, data=dataF2)
        }
        tmpP=exp(stats::predict(tmpModel,newdata=testF1))-4990
        for(i in 1:length(test.id)){
          test$Weekly_Pred2[test.id[i]]<<-tmpP[i]
        }
      }else if(tmpL==0 & testRows>0){
        for(i in 1:length(test.id)){
          test$Weekly_Pred2[test.id[i]]<<-0
        }
      }
    }
  }
}
