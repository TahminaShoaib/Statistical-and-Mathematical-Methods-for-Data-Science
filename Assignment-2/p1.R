testAll <- read.table("testData.txt")
df <- t(testAll)
for (i in 1:nrow(testAll)) { 
  li_col <- df[,i] 
  col <- unlist(li_col)
  print(col)
}

str(col)