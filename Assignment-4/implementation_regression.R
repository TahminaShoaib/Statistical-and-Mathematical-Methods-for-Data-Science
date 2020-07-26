# Input data for regression
X_train = as.matrix(read.table('train2_5.txt'))
Y_train = as.matrix(read.table('train2_5Labels.txt'))
X_test = as.matrix(read.table('test2_5.txt'))
Y_test = as.matrix(read.table('test2_5Labels.txt'))

# Find the regression coefficients
regressionCoefficients <- function(X, Y, lambda){
  add_one_matrix <- cbind(new_col=1, X)
  transpose <- t(add_one_matrix)
  a <- nrow(transpose)
  iden <- diag(a)
  
  new_matrix <- transpose%*%add_one_matrix
  mult <- lambda*iden
  sum <- new_matrix + mult
  inverse_matrix <- solve(sum)
  mult_new <- transpose%*%Y
  final <- inverse_matrix%*%mult_new
  #returnValue(final)
}

# Finding the prediction values
predictions <- function(testX,regressionCoefficients){
  add_one_matrix <- cbind(new_col=1, testX)
  predicted_labels <- add_one_matrix%*%regressionCoefficients
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

# Implementation on Training set
coefficient_values <- regressionCoefficients(X_train, Y_train, 0.2)
predicted_values <- predictions(X_train, coefficient_values)
predicted_labels <- 0
for(i in seq_len(nrow(predicted_values))){
  if(abs(predicted_values[i]-2)<abs(predicted_values[i]-5)){
    predicted_labels[i] <- 2
  }else{
    predicted_labels[i] <- 5
  }
}
confusion_matrix_train <- Matrix(predicted_labels, Y_train)

# Implementation on Test set
coefficient_values <- regressionCoefficients(X_test, Y_test, 0.2)
predicted_values <- predictions(X_test, coefficient_values)
predicted_labels <- 0
for(i in seq_len(nrow(predicted_values))){
  if(abs(predicted_values[i]-2)<abs(predicted_values[i]-5)){
    predicted_labels[i] <- 2
  }else{
    predicted_labels[i] <- 5
  }
}
confusion_matrix_test <- Matrix(predicted_labels, Y_test)