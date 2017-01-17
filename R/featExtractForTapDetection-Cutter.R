# this script just removes some labeled data that no longer is supposed to be labeled as "iskey" == TRUE
fKeyTrain <- read.csv(
  "C:\\git\\data-thesis\\R\\datasets\\17011020-dataset-training-tap-detection-25.csv",
  header = TRUE,
  stringsAsFactors=FALSE
)

ftest <- fKeyTrain

for (i in seq_along(ftest$Timestamp)) {
  
  if(ftest$IsKeyProb[i] > 0) {
    if(i == 1) {
      if(!(ftest$IsKeyProb[i] > ftest$IsKeyProb[i+1])) {
        ftest$IsKey[i] <- FALSE
        ftest$Keypress[i] <- "NONE"
      }
      else next()
    }
    else if(i == length(ftest$Timestamp)) {
      if(!(ftest$IsKeyProb[i] >= ftest$IsKeyProb[i-1])) {
        ftest$IsKey[i] <- FALSE
        ftest$Keypress[i] <- "NONE"
      } else next()
    }
    else if(!(ftest$IsKeyProb[i] > ftest$IsKeyProb[i+1] & ftest$IsKeyProb[i] >= ftest$IsKeyProb[i-1])) {
      # ftest <- ftest[-which(ftest$Timestamp[i] == ftest$Timestamp),]
      ftest$IsKey[i] <- FALSE
      ftest$Keypress[i] <- "NONE"
    }
  }
}

write.csv(
  ftest,
  "C:\\git\\data-thesis\\R\\datasets\\17011020-dataset-training-tap-detection-cut-25.csv",
  row.names = FALSE
)