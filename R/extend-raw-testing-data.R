rawdata <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\transferred-17011205-victim-data-preprocessed.csv",
    header = TRUE
  )

keytrain <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\17011205-key-dataset-test-raw.csv",
    header = TRUE
  )
keytrain$DownTime <- keytrain$DownTime / 1000000
keytrain$EventTime <- keytrain$EventTime / 1000000

rawdata$belongsToKey <- FALSE;

for (i in seq_along(keytrain$DownTime)) {
  for (j in rawdata$Timestamp[rawdata$Timestamp >= (keytrain$DownTime[i] * 1000000) &
                            rawdata$Timestamp <= (keytrain$EventTime[i] * 1000000)]) {
    rawdata$belongsToKey[rawdata$Timestamp == j] <- TRUE
  }
}

write.csv(rawdata,
          "C:\\git\\data-thesis\\R\\datasets\\transferred-17011205-victim-data-preprocessed.csv",
          row.names = FALSE)