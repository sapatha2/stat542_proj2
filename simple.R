# Simple prediction

simple_pred <- function(){
  ## create a new column of predicted sales in the test set
  test$Weekly_Pred1 <<- 0
  store = sort(unique(test$Store))
  n.store = length(store)
  dept = sort(unique(test$Dept))
  n.dept = length(dept)

  for (s in 1:n.store){
    for (d in 1:n.dept){

      cat("Simple, Store: ", store[s], "\t Dept ", dept[d], "\n")

      # find the data for (store, dept) = (s, d)
      test.id = which(test$Store == store[s] & test$Dept == dept[d])
      test.temp = test[test.id, ]
      train.id = which(train$Store == store[s] & train$Dept == dept[d])
      train.temp = train[train.id, ]
      
      for (i in 1:length(test.id)){
        id = which(train.temp$Wk == test.temp[i,]$Wk & train.temp$Yr == test.temp[i,]$Yr - 1)
        threeWeeksId = c(id - 1, id, id + 1)  ## three weeks in the last year
        tempSales = train.temp[threeWeeksId, 'Weekly_Sales']
        if (length(tempSales) == 0){
          test$Weekly_Pred1[test.id[i]] <<- 0
        }else{
          test$Weekly_Pred1[test.id[i]] <<- median(tempSales)
        }
      }
    }
  }

  # still 5 NA's, for simplicity use 0.
  test$Weekly_Pred1[which(is.na(test$Weekly_Pred1))] <<- 0
}