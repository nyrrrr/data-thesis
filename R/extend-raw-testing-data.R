sdata <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\transferred-17011020-victim-data.csv",
    header = TRUE
  )

keytrain <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\17011020-key-dataset-test-raw.csv",
    header = TRUE
  )
keytrain$DownTime <- keytrain$DownTime / 1000000
keytrain$EventTime <- keytrain$EventTime / 1000000

sdata$SqSumA <- sdata$x ^ 2 + sdata$y ^ 2 + sdata$z ^ 2
sdata$SqSumG <- sdata$a^2 + sdata$b^2 + sdata$c^2
sdata$SqSumO <- sdata$alpha^2 + sdata$beta^2 + sdata$gamma^2

sdata$MagnA <- sqrt(sdata$SqSumA)
sdata$MagnG <- sqrt(sdata$SqSumG)
sdata$MagnO <- sqrt(sdata$SqSumO)

sdata$XdeltaA[2:length(sdata$x)] <- sdata$x[-1] - sdata$x[-length(sdata$x)]
sdata$YdeltaA[2:length(sdata$x)] <- sdata$y[-1] - sdata$y[-length(sdata$y)]
sdata$ZdeltaA[2:length(sdata$x)] <- sdata$z[-1] - sdata$z[-length(sdata$z)]
sdata$XdeltaG[2:length(sdata$x)] <- sdata$a[-1] - sdata$a[-length(sdata$x)]
sdata$YdeltaG[2:length(sdata$x)] <- sdata$b[-1] - sdata$b[-length(sdata$y)]
sdata$ZdeltaG[2:length(sdata$x)] <- sdata$c[-1] - sdata$c[-length(sdata$z)]

sdata$XdeltaA[1] <- sdata$x[1]
sdata$YdeltaA[1] <- sdata$y[1]
sdata$ZdeltaA[1] <- sdata$z[1]
sdata$XdeltaG[1] <- sdata$a[1]
sdata$YdeltaG[1] <- sdata$b[1]
sdata$ZdeltaG[1] <- sdata$c[1]

sdata$belongsToKey <- FALSE;

for (i in seq_along(keytrain$DownTime)) {
  for (j in sdata$Timestamp[sdata$Timestamp >= (keytrain$DownTime[i] * 1000000) &
                            sdata$Timestamp <= (keytrain$EventTime[i] * 1000000)]) {
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
          "C:\\git\\data-thesis\\R\\datasets\\transferred-17011020-victim-data-preprocessed.csv",
          row.names = FALSE)