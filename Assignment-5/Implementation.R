# Inputting the dataset for evaluation
X = as.matrix(read.table('ocr.txt'))
Y = as.matrix(read.table('ocr_lables.txt'))

# Viewing an image
r = X[1,]
im = matrix(r,nrow=16,byrow=TRUE)
image(im[,ncol(im):1])

# Task-1: MULTI-DIMENSIONAL SCALING (MDS) For DATA VISUALIZATION
Mean_col = colMeans(X)
X_C = X - Mean_col
X_C_t = t(X_C)
B = X_C%*%X_C_t

e = eigen(B)
V = e$vectors[,c(1,2)]
S = sqrt(diag(e$values[1:2]))

Z = V%*%S

# All the rows with the class label = 2
label_two = Y==2
# All the rows with the class label = 3
label_three = Y==3
# All the rows with the class label = 4
label_four = Y==4

# Data matrix for class = 2
twoDat = Z[label_two,]
# Data matrix for class = 3
threeDat = Z[label_three,]
# Data matrix for class = 4
fourDat = Z[label_four,]

# Scatter Plot
plot(twoDat[,1],twoDat[,2],col='blue', main = 'MULTI-DIMENSIONAL SCALING (MDS)', 
     xlab="value of eigen vector(x)" , ylab="value of eigen vector(y)", 
     ylim=c(-2.5,3.5), xlim=c(-1,5)) 
points(threeDat[,1],threeDat[,2],col='green')
points(fourDat[,1],fourDat[,2],col='red')
legend(x=4,y=3,c("Label-2", "Label-3", "Label-4"),
       cex=.8,col=c("blue", "green", "red"),pch=c(1,2))

# TASK 2: DISPLAY EIGEN DIGITS

# All the rows with the class label = 2
label_two = Y==2
# All the rows with the class label = 3
label_three = Y==3
# All the rows with the class label = 4
label_four = Y==4

# Digit 2 - Class
# Data matrix for class = 2
X_2 = X[label_two,]
# Covariance Matrixof X_2
cov_X_2 = cov(X_2)
# Calculate eigen values and vectors
e_2 = eigen(cov_X_2)
eValues_2 = e_2$values
eVectors_2 = e_2$vectors

# First 4 Eigen Vectors - Digit = 2
# First Eigen Vector for digit = 2
V_2 = e_2$vectors[,1]
# Reshaped 4x4 Matrix
R_2 = matrix(V_2, 16, 16)
# Get the images of the digit
im_2 = matrix(R_2,nrow=16,byrow=TRUE)
image(im_2[,ncol(im_2):1])
# Second Eigen Vector for digit = 2
V_2 = e_2$vectors[,2]
# Reshaped 4x4 Matrix
R_2 = matrix(V_2, 16, 16)
# Get the images of the digit
im_2 = matrix(R_2,nrow=16,byrow=TRUE)
image(im_2[,ncol(im_2):1])
# Third Eigen Vector for the digit = 2
V_2 = e_2$vectors[,3]
# Reshaped 4x4 Matrix
R_2 = matrix(V_2, 16, 16)
# Get the images of the digit
im_2 = matrix(R_2,nrow=16,byrow=TRUE)
image(im_2[,ncol(im_2):1])
# Fourth Eigen Vector for the digit = 2
V_2 = e_2$vectors[,4]
# Reshaped 4x4 Matrix
R_2 = matrix(V_2, 16, 16)
# Get the images of the digit
im_2 = matrix(R_2,nrow=16,byrow=TRUE)
image(im_2[,ncol(im_2):1])

# Last 4 Eigen Vectors - Digit = 2
# Last Eigen Vector for digit = 2
V_2 = eVectors_2[,ncol(eVectors_2)]
# Reshaped 4x4 Matrix
R_2 = matrix(V_2, 16, 16)
# Get the images of the digit
im_2 = matrix(R_2,nrow=16,byrow=TRUE)
image(im_2[,ncol(im_2):1])
# Second Last Eigen Vector for digit = 2
V_2 = eVectors_2[,(ncol(eVectors_2)-1):(ncol(eVectors_2)-1)]
# Reshaped 4x4 Matrix
R_2 = matrix(V_2, 16, 16)
# Get the images of the digit
im_2 = matrix(R_2,nrow=16,byrow=TRUE)
image(im_2[,ncol(im_2):1])
# Third Last Eigen Vector for digit = 2
V_2 = eVectors_2[,(ncol(eVectors_2)-2):(ncol(eVectors_2)-2)]
# Reshaped 4x4 Matrix
R_2 = matrix(V_2, 16, 16)
# Get the images of the digit
im_2 = matrix(R_2,nrow=16,byrow=TRUE)
image(im_2[,ncol(im_2):1])
# Fourth Last Eigen Vector for digit = 2
V_2 = eVectors_2[,(ncol(eVectors_2)-3):(ncol(eVectors_2)-3)]
# Reshaped 4x4 Matrix
R_2 = matrix(V_2, 16, 16)
# Get the images of the digit
im_2 = matrix(R_2,nrow=16,byrow=TRUE)
image(im_2[,ncol(im_2):1])

# Digit 3 - Class
# Data matrix for class = 3
X_3 = X[label_three,]
# Covariance Matrixof X_3
cov_X_3 = cov(X_3)
# Calculate eigen values and vectors
e_3 = eigen(cov_X_3)
eValues_3 = e_3$values
eVectors_3 = e_3$vectors

# First 4 Eigen Vectors - Digit = 3
# First Eigen Vector for digit = 3
V_3 = e_3$vectors[,1]
# Reshaped 4x4 Matrix
R_3 = matrix(V_3, 16, 16)
# Get the images of the digit
im_3 = matrix(R_3,nrow=16,byrow=TRUE)
image(im_3[,ncol(im_3):1])
# Second Eigen Vector for digit = 3
V_3 = e_3$vectors[,2]
# Reshaped 4x4 Matrix
R_3 = matrix(V_3, 16, 16)
# Get the images of the digit
im_3 = matrix(R_3,nrow=16,byrow=TRUE)
image(im_3[,ncol(im_3):1])
# Third Eigen Vector for the digit = 3
V_3 = e_3$vectors[,3]
# Reshaped 4x4 Matrix
R_3 = matrix(V_3, 16, 16)
# Get the images of the digit
im_3 = matrix(R_3,nrow=16,byrow=TRUE)
image(im_3[,ncol(im_3):1])
# Fourth Eigen Vector for the digit = 3
V_3 = e_3$vectors[,4]
# Reshaped 4x4 Matrix
R_3 = matrix(V_3, 16, 16)
# Get the images of the digit
im_3 = matrix(R_3,nrow=16,byrow=TRUE)
image(im_3[,ncol(im_3):1])

# Last 4 Eigen Vectors - Digit = 3
# Last Eigen Vector for digit = 3
V_3 = eVectors_3[,ncol(eVectors_3)]
# Reshaped 4x4 Matrix
R_3 = matrix(V_3, 16, 16)
# Get the images of the digit
im_3 = matrix(R_3,nrow=16,byrow=TRUE)
image(im_3[,ncol(im_3):1])
# Second Last Eigen Vector for digit = 3
V_3 = eVectors_3[,(ncol(eVectors_3)-1):(ncol(eVectors_3)-1)]
# Reshaped 4x4 Matrix
R_3 = matrix(V_3, 16, 16)
# Get the images of the digit
im_3 = matrix(R_3,nrow=16,byrow=TRUE)
image(im_3[,ncol(im_3):1])
# Third Last Eigen Vector for digit = 3
V_3 = eVectors_3[,(ncol(eVectors_3)-2):(ncol(eVectors_3)-2)]
# Reshaped 4x4 Matrix
R_3 = matrix(V_3, 16, 16)
# Get the images of the digit
im_3 = matrix(R_3,nrow=16,byrow=TRUE)
image(im_3[,ncol(im_3):1])
# Fourth Last Eigen Vector for digit = 3
V_3 = eVectors_3[,(ncol(eVectors_3)-3):(ncol(eVectors_3)-3)]
# Reshaped 4x4 Matrix
R_3 = matrix(V_3, 16, 16)
# Get the images of the digit
im_3 = matrix(R_3,nrow=16,byrow=TRUE)
image(im_3[,ncol(im_3):1])