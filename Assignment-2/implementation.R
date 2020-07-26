# Read the training data 
datAll = read.table("trainData.txt")
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

# Mean vector of columns of class = 1
probOf1Class1 = colMeans(oneDat)
probof0Class1 = 1 - probOf1Class1
# Mean vector of columns of class = 0
probOf0Class0 = colMeans(zeroDat)
probof1Class0 = 1- probOf0Class0

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

testAll <- read.table("testData.txt") 
# First Row
Row1 <- testAll[1,]
a <- t(Row1)
ml01 <- vector()
ml11 <- vector()
size <- length(Row1)

for (i in 1:size){
  if(a[i]==0){
    ml01[i] <- (1-probOf0Class0[i])
    ml11[i] <- (1-probOf1Class1[i])
  }
  else if(a[i]==1){
    ml01[i] <- (probOf0Class0[i])
    ml11[i] <- (probOf1Class1[i])
  }
}
paste("MLEClass0-Row1:", prod(ml01))
paste("MLEClass1-Row1:", prod(ml11))
if(prod(ml01)>prod(ml01)){
  paste("Class: 0")
}else{
  paste("Class:1")
}

# Second Row
Row2 <- testAll[2,]
a <- t(Row2)
ml02 <- vector()
ml12 <- vector()
size <- length(Row1)

for (i in 1:size){
  if(a[i]==0){
    ml02[i] <- (1-probOf0Class0[i])
    ml12[i] <- (1-probOf1Class1[i])
  }
  else if(a[i]==1){
    ml02[i] <- (probOf0Class0[i])
    ml12[i] <- (probOf1Class1[i])
  }
}
paste("MLEClass0-Row2:", prod(ml02))
paste("MLEClass1-Row2:", prod(ml12))
if(prod(ml02)>prod(ml12)){
  paste("Class: 0")
}else{
  paste("Class:1")
}

# Thrid Row
Row3 <- testAll[3,]
a <- t(Row3)
ml03 <- vector()
ml13 <- vector()
size <- length(Row1)

for (i in 1:size){
  if(a[i]==0){
    ml03[i] <- (1-probOf0Class0[i])
    ml13[i] <- (1-probOf1Class1[i])
  }
  else if(a[i]==1){
    ml03[i] <- (probOf0Class0[i])
    ml13[i] <- (probOf1Class1[i])
  }
}
paste("MLEClass0-Row3:", prod(ml03))
paste("MLEClass1-Row3:", prod(ml13))
if(prod(ml03)>prod(ml13)){
  paste("Class: 0")
}else{
  paste("Class:1")
}

# Fourth Row
Row4 <- testAll[4,]
a <- t(Row4)
ml04 <- vector()
ml14 <- vector()
size <- length(Row1)

for (i in 1:size){
  if(a[i]==0){
    ml04[i] <- (1-probOf0Class0[i])
    ml14[i] <- (1-probOf1Class1[i])
  }
  else if(a[i]==1){
    ml04[i] <- (probOf0Class0[i])
    ml14[i] <- (probOf1Class1[i])
  }
}
paste("MLEClass0-Row4:", prod(ml04))
paste("MLEClass1-Row4:", prod(ml14))
if(prod(ml04)>prod(ml14)){
  paste("Class: 0")
}else{
  paste("Class:1")
}

# Fifth Row
Row5 <- testAll[5,]
a <- t(Row5)
ml05 <- vector()
ml15 <- vector()
size <- length(Row1)

for (i in 1:size){
  if(a[i]==0){
    ml05[i] <- (1-probOf0Class0[i])
    ml15[i] <- (1-probOf1Class1[i])
  }
  else if(a[i]==1){
    ml05[i] <- (probOf0Class0[i])
    ml15[i] <- (probOf1Class1[i])
  }
}
paste("MLEClass0-Row5:", prod(ml05))
paste("MLEClass1-Row5:", prod(ml15))
if(prod(ml05)>prod(ml15)){
  paste("Class: 0")
}else{
  paste("Class:1")
}

# Sixth Row
Row6 <- testAll[6,]
a <- t(Row6)
ml06 <- vector()
ml16 <- vector()
size <- length(Row6)

for (i in 1:size){
  if(a[i]==0){
    ml06[i] <- (1-probOf0Class0[i])
    ml16[i] <- (1-probOf1Class1[i])
  }
  else if(a[i]==1){
    ml06[i] <- (probOf0Class0[i])
    ml16[i] <- (probOf1Class1[i])
  }
}
paste("MLEClass0-Row6:", prod(ml06))
paste("MLEClass1-Row6:", prod(ml16))
if(prod(ml06)>prod(ml16)){
  paste("Class: 0")
}else{
  paste("Class:1")
}

# Seventh Row
Row7 <- testAll[7,]
a <- t(Row7)
ml07 <- vector()
ml17 <- vector()
size <- length(Row1)

for (i in 1:size){
  if(a[i]==0){
    ml07[i] <- (1-probOf0Class0[i])
    ml17[i] <- (1-probOf1Class1[i])
  }
  else if(a[i]==1){
    ml07[i] <- (probOf0Class0[i])
    ml17[i] <- (probOf1Class1[i])
  }
}
paste("MLEClass0-Row7:", prod(ml07))
paste("MLEClass1-Row7:", prod(ml17))
if(prod(ml07)>prod(ml17)){
  paste("Class: 0")
}else{
  paste("Class:1")
}

# Eight Row
Row8 <- testAll[8,]
a <- t(Row8)
ml08 <- vector()
ml18 <- vector()
size <- length(Row1)

for (i in 1:size){
  if(a[i]==0){
    ml08[i] <- (1-probOf0Class0[i])
    ml18[i] <- (1-probOf1Class1[i])
  }
  else if(a[i]==1){
    ml08[i] <- (probOf0Class0[i])
    ml18[i] <- (probOf1Class1[i])
  }
}
paste("MLEClass0-Row8:", prod(ml08))
paste("MLEClass1-Row8:", prod(ml18))
if(prod(ml08)>prod(ml18)){
  paste("Class: 0")
}else{
  paste("Class:1")
}

# Ninth Row
Row9 <- testAll[9,]
a <- t(Row9)
ml09 <- vector()
ml19 <- vector()
size <- length(Row1)

for (i in 1:size){
  if(a[i]==0){
    ml09[i] <- (1-probOf0Class0[i])
    ml19[i] <- (1-probOf1Class1[i])
  }
  else if(a[i]==1){
    ml09[i] <- (probOf0Class0[i])
    ml19[i] <- (probOf1Class1[i])
  }
}
paste("MLEClass0-Row9:", prod(ml09))
paste("MLEClass1-Row9:", prod(ml19))
if(prod(ml09)>prod(ml19)){
  paste("Class: 0")
}else{
  paste("Class:1")
}

# Tenth Row
Row10 <- testAll[10,]
a <- t(Row10)
ml010 <- vector()
ml110 <- vector()
size <- length(Row1)

for (i in 1:size){
  if(a[i]==0){
    ml010[i] <- (1-probOf0Class0[i])
    ml110[i] <- (1-probOf1Class1[i])
  }
  else if(a[i]==1){
    ml010[i] <- (probOf0Class0[i])
    ml110[i] <- (probOf1Class1[i])
  }
}
paste("MLEClass0-Row10:", prod(ml010))
paste("MLEClass1-Row10:", prod(ml110))
if(prod(ml010)>prod(ml110)){
  paste("Class: 0")
}else{
  paste("Class:1")
}

############ MAP ########################
# Row 1 MAP
a <- (prod(ml01)*prior0)
b <- (prod(ml11)*prior1)
map01 <- (a)/(a+b)
map11 <- (b)/(a+b)
paste("MAPClass0-Row1:", map01)
paste("MAPClass1-Row1:", map11)
if(map01>map11){
  paste("Class: 0")
}else{
  paste("Class:1")
}

# Row 2 MAP
a <- (prod(ml02)*prior0)
b <- (prod(ml12)*prior1)
map02 <- (a)/(a+b)
map12 <- (b)/(a+b)
paste("MAPClass0-Row2:", map02)
paste("MAPClass1-Row2:", map12)
if(map02>map12){
  paste("Class: 0")
}else{
  paste("Class:1")
}

# Row 3 MAP
a <- (prod(ml03)*prior0)
b <- (prod(ml13)*prior1)
map03 <- (a)/(a+b)
map13 <- (b)/(a+b)
paste("MAPClass0-Row3:", map03)
paste("MAPClass1-Row3:", map13)
if(map03>map13){
  paste("Class: 0")
}else{
  paste("Class:1")
}

# Row 4 MAP
a <- (prod(ml04)*prior0)
b <- (prod(ml14)*prior1)
map04 <- (a)/(a+b)
map14 <- (b)/(a+b)
paste("MAPClass0-Row4:", map04)
paste("MAPClass1-Row4:", map14)
if(map04>map14){
  paste("Class: 0")
}else{
  paste("Class:1")
}

# Row 5 MAP
a <- (prod(ml05)*prior0)
b <- (prod(ml15)*prior1)
map05 <- (a)/(a+b)
map15 <- (b)/(a+b)
paste("MAPClass0-Row5:", map05)
paste("MAPClass1-Row5:", map15)
if(map05>map15){
  paste("Class: 0")
}else{
  paste("Class:1")
}

# Row 6 MAP
a <- (prod(ml06)*prior0)
b <- (prod(ml16)*prior1)
map06 <- (a)/(a+b)
map16 <- (b)/(a+b)
paste("MAPClass0-Row6:", map06)
paste("MAPClass1-Row6:", map16)
if(map06>map16){
  paste("Class: 0")
}else{
  paste("Class:1")
}

# Row 7 MAP
a <- (prod(ml07)*prior0)
b <- (prod(ml17)*prior1)
map07 <- (a)/(a+b)
map17 <- (b)/(a+b)
paste("MAPClass0-Row7:", map07)
paste("MAPClass1-Row7:", map17)
if(map07>map17){
  paste("Class: 0")
}else{
  paste("Class:1")
}

# Row 8 MAP
a <- (prod(ml08)*prior0)
b <- (prod(ml18)*prior1)
map08 <- (a)/(a+b)
map18 <- (b)/(a+b)
paste("MAPClass0-Row8:", map08)
paste("MAPClass1-Row8:", map18)
if(map08>map18){
  paste("Class: 0")
}else{
  paste("Class:1")
}

# Row 9 MAP
a <- (prod(ml09)*prior0)
b <- (prod(ml19)*prior1)
map09 <- (a)/(a+b)
map19 <- (b)/(a+b)
paste("MAPClass0-Row9:", map09)
paste("MAPClass1-Row9:", map19)
if(map09>map19){
  paste("Class: 0")
}else{
  paste("Class:1")
}

# Row 10 MAP
a <- (prod(ml010)*prior0)
b <- (prod(ml110)*prior1)
map010 <- (a)/(a+b)
map110 <- (b)/(a+b)
paste("MAPClass0-Row10:", map010)
paste("MAPClass1-Row10:", map110)
if(map010>map110){
  paste("Class: 0")
}else{
  paste("Class:1")
}