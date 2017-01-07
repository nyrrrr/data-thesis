sensordata <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\16122802-sensor-dataset-training-raw.csv",
    header = TRUE
  )

keydata <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\16122802-key-dataset-training-raw.csv",
    header = TRUE
  )


for (i in seq_along(sensordata$Timestamp)) {
  sensordata$SqSumA[i] <-
    sensordata$x[i] ^ 2 + sensordata$y[i] ^ 2 + sensordata$z[i] ^ 2
  sensordata$SqSumG[i] <-
    sensordata$a[i] ^ 2 + sensordata$b[i] ^ 2 + sensordata$c[i] ^ 2
  sensordata$SqSumO[i] <-
    sensordata$alpha[i] ^ 2 + sensordata$beta[i] ^ 2 + sensordata$gamma[i] ^
    2
  
  sensordata$magnA[i] <- sqrt(sensordata$SqSumA[i])
  sensordata$magnG[i] <- sqrt(sensordata$SqSumG[i])
  sensordata$magnO[i] <- sqrt(sensordata$SqSumO[i])
}


# feature data set;
fsd <- keydata


for (i in seq_along(keydata$DownTime)) {
  tmpRawXWindow <-
    sensordata$x[sensordata$Timestamp >= keydata$DownTime[i] &
                   sensordata$Timestamp <= keydata$EventTime[i]]
  
  tmpRawYWindow <-
    sensordata$y[sensordata$Timestamp >= keydata$DownTime[i] &
                   sensordata$Timestamp <= keydata$EventTime[i]]
  
  tmpRawZWindow <-
    sensordata$z[sensordata$Timestamp >= keydata$DownTime[i] &
                   sensordata$Timestamp <= keydata$EventTime[i]]
  
  tmpRawAWindow <-
    sensordata$a[sensordata$Timestamp >= keydata$DownTime[i] &
                   sensordata$Timestamp <= keydata$EventTime[i]]
  
  tmpRawBWindow <-
    sensordata$b[sensordata$Timestamp >= keydata$DownTime[i] &
                   sensordata$Timestamp <= keydata$EventTime[i]]
  
  tmpRawCWindow <-
    sensordata$c[sensordata$Timestamp >= keydata$DownTime[i] &
                   sensordata$Timestamp <= keydata$EventTime[i]]
  
  tmpRawAlphaWindow <-
    sensordata$alpha[sensordata$Timestamp >= keydata$DownTime[i] &
                       sensordata$Timestamp <= keydata$EventTime[i]]
  
  tmpRawBetaWindow <-
    sensordata$beta[sensordata$Timestamp >= keydata$DownTime[i] &
                      sensordata$Timestamp <= keydata$EventTime[i]]
  
  tmpRawGammaWindow <-
    sensordata$gamma[sensordata$Timestamp >= keydata$DownTime[i] &
                       sensordata$Timestamp <= keydata$EventTime[i]]
  
  tmpRawMagnAWindow <-
    sensordata$magnA[sensordata$Timestamp >= keydata$DownTime[i] &
                       sensordata$Timestamp <= keydata$EventTime[i]]
  
  tmpRawMagnGWindow <-
    sensordata$magnG[sensordata$Timestamp >= keydata$DownTime[i] &
                       sensordata$Timestamp <= keydata$EventTime[i]]
  
  tmpRawMagnOWindow <-
    sensordata$magnO[sensordata$Timestamp >= keydata$DownTime[i] &
                       sensordata$Timestamp <= keydata$EventTime[i]]
  
  tmpRawSqSumAWindow <-
    sensordata$SqSumA[sensordata$Timestamp >= keydata$DownTime[i] &
                        sensordata$Timestamp <= keydata$EventTime[i]]
  
  tmpRawSqSumGWindow <-
    sensordata$SqSumG[sensordata$Timestamp >= keydata$DownTime[i] &
                        sensordata$Timestamp <= keydata$EventTime[i]]
  
  tmpRawSqSumOWindow <-
    sensordata$SqSumO[sensordata$Timestamp >= keydata$DownTime[i] &
                        sensordata$Timestamp <= keydata$EventTime[i]]
  
  
  fsd$WindowSize[i] <- length(tmpRawXWindow)
  
  
  wSize <- fsd$WindowSize[i]
  
  
  # Min (x) Accelerometer
  fsd$XminA[i] <- min(tmpRawXWindow)
  
  # Min (y) Accelerometer
  fsd$YminA[i] <- min(tmpRawYWindow)
  
  # Min (z) Accelerometer
  fsd$ZminA[i] <- min(tmpRawZWindow)
  
  # Min (x) Gyroscope
  fsd$XminG[i] <- min(tmpRawAWindow)
  
  # Min (y) Gyroscope
  fsd$YminG[i] <- min(tmpRawBWindow)
  
  # Min (z) Gyroscope
  fsd$ZminG[i] <- min(tmpRawCWindow)
  
  # Min (x) Orientation
  fsd$XminO[i] <- min(tmpRawAlphaWindow)
  
  # Min (y) Orientation
  fsd$YminO[i] <- min(tmpRawBetaWindow)
  
  # Min (z) Orientation
  fsd$ZminO[i] <- min(tmpRawGammaWindow)
  
  # Max (x) Accelerometer
  fsd$XmaxA[i] <- max(tmpRawXWindow)
  
  # Max (y) Accelerometer
  fsd$YmaxA[i] <- max(tmpRawYWindow)
  
  # Max (z) Accelerometer
  fsd$ZmaxA[i] <- max(tmpRawZWindow)
  
  # Max (x) Gyroscope
  fsd$XmaxG[i] <- max(tmpRawAWindow)
  
  # Max (y) Gyroscope
  fsd$YmaxG[i] <- max(tmpRawBWindow)
  
  # Max (z) Gyroscope
  fsd$ZmaxG[i] <- max(tmpRawCWindow)
  
  # Max (x) Orientation
  fsd$XmaxO[i] <- max(tmpRawAlphaWindow)
  
  # Max (y) Orientation
  fsd$YmaxO[i] <- max(tmpRawBetaWindow)
  
  # Max (z) Orientation
  fsd$ZmaxO[i] <- max(tmpRawGammaWindow)
  
  # Mean (x) Accelerometer
  fsd$XmeanA[i] <- mean(tmpRawXWindow)
  
  # Mean (y) Accelerometer
  fsd$YmeanA[i] <- mean(tmpRawYWindow)
  
  # Mean (z) Accelerometer
  fsd$ZmeanA[i] <- mean(tmpRawZWindow)
  
  # Mean (x) Gyroscope
  fsd$XmeanG[i] <- mean(tmpRawAWindow)
  
  # Mean (y) Gyroscope
  fsd$YmeanG[i] <- mean(tmpRawBWindow)
  
  # Mean (z) Gyroscope
  fsd$ZmeanG[i] <- mean(tmpRawCWindow)
  
  # Mean (x) Orientation
  fsd$XmeanO[i] <- mean(tmpRawAlphaWindow)
  
  # Mean (y) Orientation
  fsd$YmeanO[i] <- mean(tmpRawBetaWindow)
  
  # Mean (z) Orientation
  fsd$ZmeanO[i] <- mean(tmpRawGammaWindow)
  
  # Median (x) Accelerometer
  fsd$XmedianA[i] <- median(tmpRawXWindow)
  
  # Median (y) Accelerometer
  fsd$YmedianA[i] <- median(tmpRawYWindow)
  
  # Median (z) Accelerometer
  fsd$ZmedianA[i] <- median(tmpRawZWindow)
  
  # Median (x) Gyroscope
  fsd$XmedianG[i] <- median(tmpRawAWindow)
  
  # Median (y) Gyroscope
  fsd$YmedianG[i] <- median(tmpRawBWindow)
  
  # Median (z) Gyroscope
  fsd$ZmedianG[i] <- median(tmpRawCWindow)
  
  # Median (x) Orientation
  fsd$XmedianO[i] <- median(tmpRawAlphaWindow)
  
  # Median (y) Orientation
  fsd$YmedianO[i] <- median(tmpRawBetaWindow)
  
  # Median (z) Orientation
  fsd$ZmedianO[i] <- median(tmpRawGammaWindow)
  
  # Standard Deviation (x) Accelerometer
  fsd$XsdA[i] <- sd(tmpRawXWindow)
  
  # Standard Deviation (y) Accelerometer
  fsd$YsdA[i] <- sd(tmpRawYWindow)
  
  # Standard Deviation (z) Accelerometer
  fsd$ZsdA[i] <- sd(tmpRawZWindow)
  
  # Standard Deviation (x) Gyroscope
  fsd$XsdG[i] <- sd(tmpRawAWindow)
  
  # Standard Deviation (y) Gyroscope
  fsd$YsdG[i] <- sd(tmpRawBWindow)
  
  # Standard Deviation (z) Gyroscope
  fsd$ZsdG[i] <- sd(tmpRawCWindow)
  
  # Standard Deviation (x) Orientation
  fsd$XsdO[i] <- sd(tmpRawAlphaWindow)
  
  # Standard Deviation (y) Orientation
  fsd$YsdO[i] <- sd(tmpRawBetaWindow)
  
  # Standard Deviation (z) Orientation
  fsd$ZsdO[i] <- sd(tmpRawGammaWindow)
  
  # Variance (x) Accelerometer
  fsd$XvarA[i] <- var(tmpRawXWindow)
  
  # Variance (y) Accelerometer
  fsd$YvarA[i] <- var(tmpRawYWindow)
  
  # Variance (z) Accelerometer
  fsd$ZvarA[i] <- var(tmpRawZWindow)
  
  # Variance (x) Gyroscope
  fsd$XvarG[i] <- var(tmpRawAWindow)
  
  # Variance (y) Gyroscope
  fsd$YvarG[i] <- var(tmpRawBWindow)
  
  # Variance (z) Gyroscope
  fsd$ZvarG[i] <- var(tmpRawCWindow)
  
  # Variance (x) Orientation
  fsd$XvarO[i] <- var(tmpRawAlphaWindow)
  
  # Variance (y) Orientation
  fsd$YvarO[i] <- var(tmpRawBetaWindow)
  
  # Variance (z) Orientation
  fsd$ZvarO[i] <- var(tmpRawGammaWindow)
  
  # Skewness (x) Accelerometer
  fsd$XskewA[i] <-
    3 * (fsd$XmeanA[i] - fsd$XmedianA[i]) / fsd$XsdA[i]
  # Skewness (y) Accelerometer
  fsd$YskewA[i] <-
    3 * (fsd$YmeanA[i] - fsd$YmedianA[i]) / fsd$YsdA[i]
  # Skewness (z) Accelerometer
  fsd$ZskewA[i] <-
    3 * (fsd$ZmeanA[i] - fsd$ZmedianA[i]) / fsd$ZsdA[i]
  # Skewness (x) Gyroscope
  fsd$XskewG[i] <-
    3 * (fsd$XmeanG[i] - fsd$XmedianG[i]) / fsd$XsdG[i]
  # Skewness (y) Gyroscope
  fsd$YskewG[i] <-
    3 * (fsd$YmeanG[i] - fsd$YmedianG[i]) / fsd$YsdG[i]
  # Skewness (z) Gyroscope
  fsd$ZskewG[i] <-
    3 * (fsd$ZmeanG[i] - fsd$ZmedianG[i]) / fsd$ZsdG[i]
  # Skewness (x) Orientation
  fsd$XskewO[i] <-
    3 * (fsd$XmeanO[i] - fsd$XmedianO[i]) / fsd$XsdO[i]
  # Skewness (y) Orientation
  fsd$YskewO[i] <-
    3 * (fsd$YmeanO[i] - fsd$YmedianO[i]) / fsd$YsdO[i]
  # Skewness (z) Orientation
  fsd$ZskewO[i] <-
    3 * (fsd$ZmeanO[i] - fsd$ZmedianO[i]) / fsd$ZsdO[i]
  # RMS (x) Accelerometer
  fsd$XrmsA[i] <- sqrt(sum((tmpRawXWindow) ^ 2) / wSize)
  # RMS (y) Accelerometer
  fsd$YrmsA[i] <- sqrt(sum((tmpRawYWindow) ^ 2) / wSize)
  # RMS (z) Accelerometer
  fsd$ZrmsA[i] <- sqrt(sum((tmpRawZWindow) ^ 2) / wSize)
  # RMS (x) Gyroscope
  fsd$XrmsG[i] <- sqrt(sum((tmpRawAWindow) ^ 2) / wSize)
  # RMS (y) Gyroscope
  fsd$YrmsG[i] <- sqrt(sum((tmpRawBWindow) ^ 2) / wSize)
  # RMS (z) Gyroscope
  fsd$ZrmsG[i] <- sqrt(sum((tmpRawCWindow) ^ 2) / wSize)
  # RMS (x) Orientation
  fsd$XrmsO[i] <- sqrt(sum((tmpRawAlphaWindow) ^ 2) / wSize)
  
  # RMS (y) Orientation
  fsd$YrmsO[i] <- sqrt(sum((tmpRawBetaWindow) ^ 2) / wSize)
  
  # RMS (z) Orientation
  fsd$ZrmsO[i] <- sqrt(sum((tmpRawGammaWindow) ^ 2) / wSize)
  
  
  # Root Mean Square of the magnitude of the Accelerometer vector
  fsd$MagnRmsA[i] <- sqrt((sum((
    tmpRawMagnAWindow
  ) ^ 2)) / wSize)
  # Root Mean Square of the magnitude of the Gyroscope vector
  fsd$MagnRmsG[i] <- sqrt((sum((
    tmpRawMagnGWindow
  ) ^ 2)) / wSize)
  # Root Mean Square of the magnitude of the Orientation vector
  fsd$MagnRmsO[i] <- sqrt((sum((
    tmpRawMagnOWindow
  ) ^ 2)) / wSize)
  
  # Root Mean Square of the square sum of the Accelerometer vector
  fsd$SqSumRmsA[i] <- sqrt((sum((
    tmpRawSqSumAWindow
  ) ^ 2)) / wSize)
  # Root Mean Square of the square sum of the Gyroscope vector
  fsd$SqSumRmsG[i] <- sqrt((sum((
    tmpRawSqSumGWindow
  ) ^ 2)) / wSize)
  # Root Mean Square of the square sum of the Orientation vector
  fsd$SqSumRmsO[i] <- sqrt((sum((
    tmpRawSqSumOWindow
  ) ^ 2)) / wSize)
  
  #peak of SqSum A
  fsd$SqSumPeakA[i] <- max(tmpRawSqSumAWindow)
  #peak of SqSum G
  fsd$SqSumPeakG[i] <- max(tmpRawSqSumGWindow)
  #peak of SqSum O
  fsd$SqSumPeakO[i] <- max(tmpRawSqSumOWindow)
  
  #min of SqSum A
  fsd$SqSumThroughA[i] <- min(tmpRawSqSumAWindow)
  #min of SqSum G
  fsd$SqSumThroughG[i] <- min(tmpRawSqSumGWindow)
  #min of SqSum O
  fsd$SqSumThroughO[i] <- min(tmpRawSqSumOWindow)
  
  # delta sqsum
  fsd$DeltaSqSumA[i] <- fsd$SqSumPeakA[i] - fsd$SqSumThroughA[i]
  fsd$DeltaSqSumG[i] <- fsd$SqSumPeakG[i] - fsd$SqSumThroughG[i]
  fsd$DeltaSqSumO[i] <- fsd$SqSumPeakO[i] - fsd$SqSumThroughO[i]
  
  
  tmpTimestampWindowMaxA <-
    sensordata$Timestamp[sensordata$Timestamp >= keydata$DownTime[i] &
                           sensordata$Timestamp <= keydata$EventTime[i] &
                           fsd$SqSumPeakA[i] == sensordata$SqSumA]
  
  tmpTimestampWindowMaxG <-
    sensordata$Timestamp[sensordata$Timestamp >= keydata$DownTime[i] &
                           sensordata$Timestamp <= keydata$EventTime[i] &
                           fsd$SqSumPeakG[i] == sensordata$SqSumG]
  
  tmpTimestampWindowMaxO <-
    sensordata$Timestamp[sensordata$Timestamp >= keydata$DownTime[i] &
                           sensordata$Timestamp <= keydata$EventTime[i] &
                           fsd$SqSumPeakO[i] == sensordata$SqSumO]
  
  tmpTimestampWindowMinA <-
    sensordata$Timestamp[sensordata$Timestamp >= keydata$DownTime[i] &
                           sensordata$Timestamp <= keydata$EventTime[i] &
                           fsd$SqSumThroughA[i] == sensordata$SqSumA]
  
  tmpTimestampWindowMinG <-
    sensordata$Timestamp[sensordata$Timestamp >= keydata$DownTime[i] &
                           sensordata$Timestamp <= keydata$EventTime[i] &
                           fsd$SqSumThroughG[i] == sensordata$SqSumG]
  
  tmpTimestampWindowMinO <-
    sensordata$Timestamp[sensordata$Timestamp >= keydata$DownTime[i] &
                           sensordata$Timestamp <= keydata$EventTime[i] &
                           fsd$SqSumThroughO[i] == sensordata$SqSumO]
  
  
  # time gap between sqsum peak and through
  fsd$SqSumTimeGapA[i] <-
    tmpTimestampWindowMaxA - tmpTimestampWindowMinA
  fsd$SqSumTimeGapG[i] <-
    tmpTimestampWindowMaxG - tmpTimestampWindowMinG
  fsd$SqSumTimeGapO[i] <-
    tmpTimestampWindowMaxO - tmpTimestampWindowMinO
  
  #sd of magnitude
  fsd$MsdA[i] <- sd(tmpRawMagnAWindow)
  fsd$MsdG[i] <- sd(tmpRawMagnGWindow)
  fsd$MsdO[i] <- sd(tmpRawMagnOWindow)
  
  #sd of sqsum
  fsd$SsdA[i] <- sd(tmpRawSqSumAWindow)
  fsd$SsdG[i] <- sd(tmpRawSqSumGWindow)
  fsd$SsdO[i] <- sd(tmpRawSqSumOWindow)
  
  # time of window
  fsd$TotalTime[i] <-
    (keydata$EventTime[i] - keydata$DownTime[i]) / 1000000
  
  
  fsd$DownTime[i] <- fsd$DownTime[i] / 1000000
  
  fsd$EventTime[i] <- fsd$EventTime[i] / 1000000
  
}
#
# for (i in seq_along(keydata$DownTime)) {
#   tmpRawXWindow <-
#     sensordata$x[sensordata$Timestamp >= keydata$DownTime[i] &
#                    sensordata$Timestamp <= keydata$EventTime[i]]
#
#   tmpRawYWindow <-
#     sensordata$y[sensordata$Timestamp >= keydata$DownTime[i] &
#                    sensordata$Timestamp <= keydata$EventTime[i]]
#
#   tmpRawZWindow <-
#     sensordata$z[sensordata$Timestamp >= keydata$DownTime[i] &
#                    sensordata$Timestamp <= keydata$EventTime[i]]
#
#   tmpRawAWindow <-
#     sensordata$a[sensordata$Timestamp >= keydata$DownTime[i] &
#                    sensordata$Timestamp <= keydata$EventTime[i]]
#
#   tmpRawBWindow <-
#     sensordata$b[sensordata$Timestamp >= keydata$DownTime[i] &
#                    sensordata$Timestamp <= keydata$EventTime[i]]
#
#   tmpRawCWindow <-
#     sensordata$c[sensordata$Timestamp >= keydata$DownTime[i] &
#                    sensordata$Timestamp <= keydata$EventTime[i]]
#
#   tmpRawAlphaWindow <-
#     sensordata$alpha[sensordata$Timestamp >= keydata$DownTime[i] &
#                        sensordata$Timestamp <= keydata$EventTime[i]]
#
#   tmpRawBetaWindow <-
#     sensordata$beta[sensordata$Timestamp >= keydata$DownTime[i] &
#                       sensordata$Timestamp <= keydata$EventTime[i]]
#
#   tmpRawGammaWindow <-
#     sensordata$gamma[sensordata$Timestamp >= keydata$DownTime[i] &
#                        sensordata$Timestamp <= keydata$EventTime[i]]
#
#   for (j in max(fsd$WindowSize)) {
#     #     fsd[,80+j] <- sensordata
#   }
#   fsd[, 80 + j] <-
#
# }



fsd$Keypress <- gsub("KEYCODE_", "", fsd$Keypress)

write.csv(
  fsd,
  "C:\\git\\data-thesis\\R\\datasets\\16122802-dataset-training.csv",
  row.names = FALSE
)