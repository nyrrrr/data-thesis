# this script is used for debugging and for plots
sensortrain <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\17011020-sensor-dataset-training-preprocessed.csv",
    header = TRUE
  )

ftrain <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\17011020-dataset-training.csv",
    header = TRUE
  )

sensortrain$belongsToKey <- 0;

for (i in seq_along(ftrain$DownTime)) {
  for (j in sensortrain$Timestamp[sensortrain$Timestamp >= (ftrain$DownTime[i] * 1000000) &
                            sensortrain$Timestamp <= (ftrain$EventTime[i] * 1000000)]) {
    sensortrain$belongsToKey[sensortrain$Timestamp == j] <- 1
  }
}

write.csv(sensortrain,
          "C:\\git\\data-thesis\\R\\datasets\\17011020-sensor-dataset-training-preprocessed.csv",
          row.names = FALSE)