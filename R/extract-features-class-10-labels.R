# this script just removes some labeled data that no longer is supposed to be labeled as "iskey" == TRUE
fKeyTrain <- read.csv(
  "C:\\git\\data-thesis\\R\\datasets\\17011020-dataset-training-tap-detection-25.csv",
  header = TRUE,
  stringsAsFactors=FALSE
)

ftest <- fKeyTrain[-which(fKeyTrain == "NONE"),]

write.csv(
  ftest,
  "C:\\git\\data-thesis\\R\\datasets\\17011020-dataset-training-tap-detection-cut-25.csv",
  row.names = FALSE
)