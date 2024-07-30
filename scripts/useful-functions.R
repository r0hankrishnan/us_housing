
#Take df, split based on parameters, return train and test set to global env
train_test_split <- function(df, propTrain, propTest, 
                             trainName = "train", testName = "test"){
  #Set seed for reproducibility
  set.seed(100)
  
  #Check types and make sure proportion values are within 0 and 1
  stopifnot(is.double(propTrain)==T, is.double(propTest)==T, 
            propTrain<1 & propTrain>0, propTest<1 & propTest>0,
            propTrain + propTest == 1, is.character(trainName), 
            is.character(testName))
  
  #Generate T/F vector of length nrow(df) to sample df
  sample <- sample(c(TRUE,FALSE), nrow(df),  
                   replace=TRUE, prob=c(propTrain,propTest))
  
  #Sample df with .70 T values for train & assign to global env
  assign(trainName, df[sample, ], envir = .GlobalEnv)
  
  #Take rest as test & assign to global env
  assign(testName, df[!sample, ], envir = .GlobalEnv)
}