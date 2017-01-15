# this script is used for debugging and for plots
sensortrain <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\17011020-sensor-dataset-training-preprocessed.csv",
    header = TRUE
  )

keytrain <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\17011020-key-dataset-training-raw.csv",
    header = TRUE,
    stringsAsFactors=FALSE
  )

sensortrain$belongsToKey <- 0;

for (i in seq_along(keytrain$DownTime)) {
  for (j in sensortrain$Timestamp[sensortrain$Timestamp >= (keytrain$DownTime[i] * 1000000) &
                            sensortrain$Timestamp <= (keytrain$EventTime[i] * 1000000)]) {
    sensortrain$belongsToKey[sensortrain$Timestamp == j] <- 1
  }
}

write.csv(sensortrain,
          "C:\\git\\data-thesis\\R\\datasets\\17011020-sensor-dataset-training-preprocessed.csv",
          row.names = FALSE)