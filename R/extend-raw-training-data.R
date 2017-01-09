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
  
  sdata$SqSumA[i] <- sdata$x[i] ^ 2 + sdata$y[i] ^ 2 + sdata$z[i] ^ 2
  sdata$SqSumG[i] <- sdata$a[i]^2 + sdata$b[i]^2 + sdata$c[i]^2
  sdata$SqSumO[i] <- sdata$alpha[i]^2 + sdata$beta[i]^2 + sdata$gamma[i]^2
  
  sdata$MagnA[i] <- sqrt(sdata$SqSumA[i])
  sdata$MagnG[i] <- sqrt(sdata$SqSumG[i])
  sdata$MagnO[i] <- sqrt(sdata$SqSumO[i])
  
  if(i == 1) {
    sdata$XdeltaA[i] <- sdata$x[1]
    sdata$YdeltaA[i] <- sdata$y[1]
    sdata$ZdeltaA[i] <- sdata$z[1]
    sdata$XdeltaG[i] <- sdata$a[1]
    sdata$YdeltaG[i] <- sdata$b[1]
    sdata$ZdeltaG[i] <- sdata$c[1]
  } 
  else {
    sdata$XdeltaA[i] <- sdata$x[i] - sdata$x[i-1]
    sdata$YdeltaA[i] <- sdata$y[i] - sdata$y[i-1]
    sdata$ZdeltaA[i] <- sdata$z[i] - sdata$z[i-1]
    sdata$XdeltaG[i] <- sdata$a[i] - sdata$a[i-1]
    sdata$YdeltaG[i] <- sdata$b[i] - sdata$b[i-1]
    sdata$ZdeltaG[i] <- sdata$c[i] - sdata$c[i-1]
  }
}
sdata$belongsToKey <- FALSE;

for (i in seq_along(fsd$DownTime)) {
  for (j in sdata$Timestamp[sdata$Timestamp >= (fsd$DownTime[i] * 1000000) &
                            sdata$Timestamp <= (fsd$EventTime[i] * 1000000)]) {
    sdata$belongsToKey[sdata$Timestamp == j] <- TRUE
  }
}

sdata$xA <- sdata$x
sdata$yA <- sdata$y
sdata$zA <- sdata$z
sdata$xG <- sdata$a
sdata$yG <- sdata$b
sdata$zG <- sdata$c

sdata$x <- NULL
sdata$y <- NULL
sdata$z <- NULL
sdata$a <- NULL
sdata$b <- NULL
sdata$c <- NULL

write.csv(sdata,
          "C:\\git\\data-thesis\\R\\datasets\\16122802-sensor-dataset-training-preprocess.csv",
          row.names = FALSE)