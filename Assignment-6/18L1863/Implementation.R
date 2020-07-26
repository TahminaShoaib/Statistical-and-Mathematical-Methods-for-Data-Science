# Input data 
X_train = as.matrix(read.table('train2_5.txt'))
Y_train = as.matrix(read.table('train2_5Labels.txt'))
X_test = as.matrix(read.table('test2_5.txt'))
Y_test = as.matrix(read.table('test2_5Labels.txt'))


# Training Gradient Descent
trainGradientDescent <- function(X, Y, learningRate, momentum){
  set.seed(257)
  add_one_matrix <- cbind(new_col=1, X)
  weights <- rnorm(n = ncol(add_one_matrix), m=1)
  Change_Weights <- matrix(0, ncol(add_one_matrix),1)
  for(epoch in 1:100){
    for(i in seq_len(nrow(add_one_matrix))){
      r <- add_one_matrix[i,]
      pred <- r%*%weights
      #momentum <- 0
      #learningRate <- 0.001
      y <- Y[i,]
      diff <- y - pred
      a <- momentum*Change_Weights
      b <- learningRate*diff
      c <- b%*%r
      c <- t(c)
      Change_Weights <- (a) + (c)
      weights <- weights + Change_Weights
    }
  }
  return(weights)
}

# Test Gradient Descent
testGradientDescent <- function (testX,regressionCoefficients){
  add_one_matrix <- cbind(new_col=1, testX)
  prediction <- add_one_matrix%*%weights
}

# Finding the confusion matrix
Matrix <- function(prediction,actualLabels){
  TP=FP=TN=FN=0
  for(i in seq_len(nrow(actualLabels)))
    if(prediction[i]==2 && actualLabels[i]==2){
      TP <- TP + 1
    }else if(prediction[i]==5 && actualLabels[i]==5){
      TN <- TN + 1
    }else if(prediction[i]==2 && actualLabels[i]==5){
      FP <- FP + 1
    }else if(prediction[i]==5 && actualLabels[i]==2){
      FN <- FN + 1
    }
  CM <- list("TP" = TP, "FP" = FP, "TN" = TN, "FN" = FN)
  return(CM)
}

# Implementation on Train Set
weights <- trainGradientDescent(X_train, Y_train, 0.001,0.9)
predictions <- testGradientDescent(X_train, weights)
predicted_labels <- 0
for(i in seq_len(nrow(predictions))){
  if(abs(predictions[i]-2)<abs(predictions[i]-5)){
    predicted_labels[i] <- 2
  }else{
    predicted_labels[i] <- 5
  }
}
CM_train <- Matrix(predicted_labels, Y_train)

# BAC Accuracy
TP <- as.numeric(CM_train[1])
TN <- as.numeric(CM_train[3])
positveClass <- sum(Y_train==2)
negativeClass <- sum(Y_train==5)
BACAccuracy <- ((TP/positveClass)+(TN/negativeClass))/2

# Implementation on Test Set
predictions <- testGradientDescent(X_test, weights)
predicted_labels <- 0
for(i in seq_len(nrow(predictions))){
  if(abs(predictions[i]-2)<abs(predictions[i]-5)){
    predicted_labels[i] <- 2
  }else{
    predicted_labels[i] <- 5
  }
}
CM_test <- Matrix(predicted_labels, Y_test)

# BAC Accuracy
TP <- as.numeric(CM_test[1])
TN <- as.numeric(CM_test[3])
positveClass <- sum(Y_test==2)
negativeClass <- sum(Y_test==5)
BACAccuracy <- ((TP/positveClass)+(TN/negativeClass))/2