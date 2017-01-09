sdata <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\transferred-16122802-victim-data.csv",
    header = TRUE
  )

fsd <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\16122802-dataset-fake-training.csv",
    header = TRUE
  )

for (i in seq_along(sdata$Timestamp)) {
  
  sdata$magnA[i] <- sqrt(sdata$x[i] ^ 2 + sdata$y[i] ^ 2 + sdata$z[i] ^ 2)
  sdata$magnG[i] <- sqrt(sdata$a[i]^2 + sdata$b[i]^2 + sdata$c[i]^2)
  sdata$magnO[i] <- sqrt(sdata$alpha[i]^2 + sdata$beta[i]^2 + sdata$gamma[i]^2)
  
  sdata$magnA[i] <- sqrt(sdata$SqSumA[i])
  sdata$magnG[i] <- sqrt(sdata$SqSumG[i])
  sdata$magnO[i] <- sqrt(sdata$SqSumO[i])
  
  if(i == 1) {
    sdata$XdeltaA[i] <- sdata$x[1]
    sdata$YdeltaA[i] <- sdata$x[1]
    sdata$ZdeltaA[i] <- sdata$x[1]
    sdata$XdeltaG[i] <- sdata$x[1]
    sdata$YdeltaG[i] <- sdata$x[1]
    sdata$ZdeltaG[i] <- sdata$x[1]
  }
  
  sdata$XdeltaA[i] <- sdata$x[i] - sdata$x[i-1]
  sdata$XdeltaA[i] <- sdata$x[i] - sdata$x[i-1]
  sdata$XdeltaA[i] <- sdata$x[i] - sdata$x[i-1]
  sdata$XdeltaA[i] <- sdata$x[i] - sdata$x[i-1]
  sdata$XdeltaA[i] <- sdata$x[i] - sdata$x[i-1]
  sdata$XdeltaA[i] <- sdata$x[i] - sdata$x[i-1]
  
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
          "C:\\git\\data-thesis\\R\\datasets\\transferred-16122802-victim-data-preprocessed.csv",
          row.names = FALSE)