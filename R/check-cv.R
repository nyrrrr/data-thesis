# check if attributes are useless

stringFileTimestamp <- 16122802 #17011020
windowSize <- 43

fKeyTrain <- read.csv(
  paste(getwd(),"/datasets/", stringFileTimestamp, "-feature-dataset-TEST-", windowSize ,".csv", sep=""),
  header = TRUE,
  stringsAsFactors=FALSE
)

names(which(apply(fKeyTrain[,!(colnames(fKeyTrain) == "IsKey"|colnames(fKeyTrain) == "Keypress")], 2, sd)/abs(apply(fKeyTrain[,!(colnames(fKeyTrain) == "IsKey"|colnames(fKeyTrain) == "Keypress")], 2, mean)) < 0.1))