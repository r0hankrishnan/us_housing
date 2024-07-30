
#Take df, split based on parameters, return train and test set to global env
train_test_split <- function(df, propTrain, propTest, trainName, testName){
  #Set seed for reproducibility
  set.seed(100)
  
  stopifnot(is.double(propTrain)==T, is.double(propTest)==T, 
            propTrain<1 & propTrain>0, propTest<1 & propTest>0)
  
  trainName <<- data.frame()
  testName <<- data.frame()
  
  #Generate T/F vector of length nrow(df) to sample df
  sample <- sample(c(TRUE,FALSE), nrow(df),  
                   replace=TRUE, prob=c(propTrain,propTest))
  
  #Sample df with .70 T values for train & assign to global env
  train  <<- df[sample, ] 
  #Take rest as test & assign to global env
  test  <<- df[!sample, ]
}