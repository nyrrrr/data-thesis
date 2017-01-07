sdata <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\16122802-sensor-dataset-training-raw.csv",
    header = TRUE
  )

fsd <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\16122802-dataset-training.csv",
    header = TRUE
  )

for (i in seq_along(sdata$Timestamp)) {
  sdata$Timestamp[i] <- sdata$Timestamp[i]
  sdata$magnA[i] <-
    sqrt(sdata$x[i] ^ 2 + sdata$y[i] ^ 2 + sdata$z[i] ^
           2)
  sdata$magnG[i] <- sqrt(sdata$a[i]^2 + sdata$b[i]^2 + sdata$c[i]^2)
  sdata$magnO[i] <- sqrt(sdata$alpha[i]^2 + sdata$beta[i]^2 + sdata$gamma[i]^2)
  
}
sdata$belongsToKey <- FALSE;

for (i in seq_along(fsd$DownTime)) {
  for (j in sdata$Timestamp[sdata$Timestamp >= (fsd$DownTime[i] * 1000000) &
                            sdata$Timestamp <= (fsd$EventTime[i] * 1000000)]) {
    sdata$belongsToKey[sdata$Timestamp == j] <- TRUE
  }
}

write.csv(sdata,
          "C:\\git\\data-thesis\\R\\datasets\\magn.csv",
          row.names = FALSE)