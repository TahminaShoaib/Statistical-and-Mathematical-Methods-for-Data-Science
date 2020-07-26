# Read the training data 
datAll=read.table("assign3GaussTrain.txt")
# Convert to Matrix Data Structure
datAll=data.matrix(datAll)
# Store first column
f1 = datAll[,1]
# Store first row
r1 = datAll[1,]

# Total number of columns
ncol(datAll)
# Store all columns except the first
x = datAll[,-1]

# Store last column of datAll in labels
labels = datAll[,ncol(datAll)]
# Store all features except label in trainX
trainX = datAll[,-ncol(datAll)]

# All the rows with the class label = 1
oneClass = labels==1
# All the rows with the class label = 0
zeroClass = labels==0

# Data matrix for class = 1
oneDat = trainX[oneClass,]
# Data matrix for class = 0
zeroDat = trainX[zeroClass,]

# Mean of class = 1
oneMean = colMeans(oneDat)
# Mean of class = 0
zeroMean = colMeans(zeroDat)

# Covariance of Class = 1
oneCov = cov(oneDat)
# Covariance of class = 0
zeroCov = cov(zeroDat)
# Total Covariance
totalCov = cov(trainX)

# Identity Covariance Matrix
IdenCov = diag(2)
# Inverse of Identity Covariance Matrix
invIdenCov = solve(IdenCov)

# Inverse of the Matrix for class = 1
invOneCov = solve(oneCov)
# Inverse of the Matrix for class = 0
invZeroCov = solve(zeroCov)

# Total count of observations in training set
n <- nrow(trainX)
# Count of observations for class = 0
count0 <- nrow(zeroDat)
# prior with class = 0
prior0 <- count0/n
# Count of observations for class = 1
count1 <- nrow(oneDat)
# prior with class = 1
prior1 <- count1/n

# Test dataset
testdat=read.table("assign3GaussTest.txt")
# Convert to Matrix Data Structure
testdat=data.matrix(testdat)

# gaussian function
gauss <- function(x, meanVe, covM){
  likelihood <- 0
  for(i in seq_len(nrow(x))){
    row <- x[i,]
    xu <- row - meanVe
    detcov <- sqrt(det(covM))
    c <- (ncol(x))/2
    a <- (2*pi)^c
    xut <- t(xu)
    denom <- a*detcov
    l <- -0.5
    invcov <- solve(covM)
    scalar <- (xut%*%invcov%*%xu)*l
    numer <- exp(scalar)
    likelihood[i] <- numer/denom
  }
  return(likelihood)
}

##################################Training Model#########################
# Training Data with Diagonal Identity Covariance
TDI1<- gauss(trainX, oneMean, IdenCov)
TDI0<- gauss(trainX, zeroMean, IdenCov)
TDIMap1 <- 0
for(i in seq_len(nrow(trainX))){
  prob <- (TDI1[i]*prior1)
  sum <- (TDI1[i]*prior1 + TDI0[i]*prior0)
  TDIMap1[i] <- prob/sum
}
TDIMap0 <- 0
for(i in seq_len(nrow(trainX))){
  prob <- (TDI0[i]*prior0)
  sum <- (TDI1[i]*prior1 + TDI0[i]*prior0)
  TDIMap0[i] <- prob/sum
}
combine <- cbind(TDIMap0, TDIMap1)
Tlabels <- ifelse(combine[,2] > combine[,1], 1, 0)
mistake = Tlabels!=labels
# Plot for the training data
plot(oneDat[,1],oneDat[,2],col='blue', main = 'Training Data') 
points(zeroDat[,1],zeroDat[,2],col='green')
points(trainX[mistake,1],trainX[mistake,2],col='red')  

# Training Data with Common Covariance
Tcommon1<- gauss(trainX, oneMean, totalCov)
Tcommon0<- gauss(trainX, zeroMean, totalCov)
TCMap1 <- 0
for(i in seq_len(nrow(trainX))){
  prob <- (Tcommon1[i]*prior1)
  sum <- (Tcommon1[i]*prior1 + Tcommon0[i]*prior0)
  TCMap1[i] <- prob/sum
}
TCMap0 <- 0
for(i in seq_len(nrow(trainX))){
  prob <- (Tcommon0[i]*prior0)
  sum <- (Tcommon1[i]*prior1 + Tcommon0[i]*prior0)
  TCMap0[i] <- prob/sum
}
combine <- cbind(TCMap0, TCMap1)
Tlabels <- ifelse(combine[,2] > combine[,1], 1, 0)
mistake = Tlabels!=labels
# Plot for the training data
plot(oneDat[,1],oneDat[,2],col='blue', main = 'Training Data') 
points(zeroDat[,1],zeroDat[,2],col='green')
points(trainX[mistake,1],trainX[mistake,2],col='red')

# Training Data with Respective Covariances
Tresp1<- gauss(trainX, oneMean, oneCov)
Tresp0<- gauss(trainX, zeroMean, zeroCov)
TRMap1 <- 0
for(i in seq_len(nrow(trainX))){
  prob <- (Tresp1[i]*prior1)
  sum <- (Tresp1[i]*prior1 + Tresp0[i]*prior0)
  TRMap1[i] <- prob/sum
}
TRMap0 <- 0
for(i in seq_len(nrow(trainX))){
  prob <- (Tresp0[i]*prior0)
  sum <- (Tresp1[i]*prior1 + Tresp0[i]*prior0)
  TRMap0[i] <- prob/sum
}
combine <- cbind(TRMap0, TRMap1)
Tlabels <- ifelse(combine[,2] > combine[,1], 1, 0)
mistake = Tlabels!=labels
# Plot for the training data
plot(oneDat[,1],oneDat[,2],col='blue', main = 'Training Data') 
points(zeroDat[,1],zeroDat[,2],col='green')
points(trainX[mistake,1],trainX[mistake,2],col='red')

############################Test Model###############################
# With Diagonal Identity Covariance
DI1<- gauss(testdat, oneMean, IdenCov)
DI0<- gauss(testdat, zeroMean, IdenCov)
DIMap1 <- 0
for(i in seq_len(nrow(testdat))){
  prob <- (DI1[i]*prior1)
  sum <- (DI1[i]*prior1 + DI0[i]*prior0)
  DIMap1[i] <- prob/sum
}
DIMap0 <- 0
for(i in seq_len(nrow(testdat))){
  prob <- (DI0[i]*prior0)
  sum <- (DI1[i]*prior1 + DI0[i]*prior0)
  DIMap0[i] <- prob/sum
}
combine <- cbind(DIMap0, DIMap1)
Idenlabels <- ifelse(combine[,2] > combine[,1], 1, 0)
Idendata <- cbind(testdat, Idenlabels)
testlabels = Idendata[,ncol(Idendata)]
testX = Idendata[,-ncol(Idendata)]
oneTClass = testlabels==1	
zeroTClass = testlabels==0
oneTDat = testX[oneTClass,]
zeroTDat = testX[zeroTClass,]	
plot(oneTDat[,1],oneTDat[,2],col='red', main = 'Test Data') 
points(zeroTDat[,1],zeroTDat[,2],col='blue')

# With Common Covariance
common1<- gauss(testdat, oneMean, totalCov)
common0<- gauss(testdat, zeroMean, totalCov)
CMap1 <- 0
for(i in seq_len(nrow(testdat))){
  prob <- (common1[i]*prior1)
  sum <- (common1[i]*prior1 + common0[i]*prior0)
  CMap1[i] <- prob/sum
}
CMap0 <- 0
for(i in seq_len(nrow(testdat))){
  prob <- (common0[i]*prior0)
  sum <- (common1[i]*prior1 + common0[i]*prior0)
  CMap0[i] <- prob/sum
}
Ccombine <- cbind(CMap0, CMap1)
Clabels <- ifelse(Ccombine[,2] > Ccombine[,1], 1, 0)
Cdata <- cbind(testdat, Clabels)
testClabels = Cdata[,ncol(Cdata)]
testCX = Cdata[,-ncol(Cdata)]
oneTCClass = testClabels==1	
zeroTCClass = testClabels==0
oneTCDat = testCX[oneTCClass,]
zeroTCDat = testCX[zeroTCClass,]	
plot(oneTCDat[,1],oneTCDat[,2],col='red', main = 'Test Data') 
points(zeroTCDat[,1],zeroTCDat[,2],col='blue')

# With Respective Covariances
resp1<- gauss(testdat, oneMean, oneCov)
resp0<- gauss(testdat, zeroMean, zeroCov)
RMap1 <- 0
for(i in seq_len(nrow(testdat))){
  prob <- (resp1[i]*prior1)
  sum <- (resp1[i]*prior1 + resp0[i]*prior0)
  RMap1[i] <- prob/sum
}
RMap0 <- 0
for(i in seq_len(nrow(testdat))){
  prob <- (resp0[i]*prior0)
  sum <- (resp1[i]*prior1 + resp0[i]*prior0)
  RMap0[i] <- prob/sum
}
Rcombine <- cbind(RMap0, RMap1)
Rlabels <- ifelse(Rcombine[,2] > Rcombine[,1], 1, 0)
Rdata <- cbind(testdat, Rlabels)
testRlabels = Rdata[,ncol(Rdata)]
testRX = Rdata[,-ncol(Rdata)]
oneTRClass = testRlabels==1	
zeroTRClass = testRlabels==0
oneTRDat = testRX[oneTRClass,]
zeroTRDat = testRX[zeroTRClass,]	
plot(oneTRDat[,1],oneTRDat[,2],col='red', main = 'Test Data') 
points(zeroTRDat[,1],zeroTRDat[,2],col='blue')
 


