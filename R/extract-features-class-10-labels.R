# this script just removes some labeled data that no longer is supposed to be labeled as "iskey" == TRUE
fKeyTrain <- read.csv(
  "C:\\git\\data-thesis\\R\\datasets\\17011417-dataset-training-61.csv",
  header = TRUE,
  stringsAsFactors=FALSE
)

ftest <- fKeyTrain[-which(fKeyTrain$Keypress == "NONE"),]

