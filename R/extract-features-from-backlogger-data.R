rawdata <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\transferred-16121319-victim-data-raw.csv",
    header = TRUE
  )
# feature data set;
fsd <- NULL
# window size
wSize <- 5
# helper vars
wIndex <- 0
wJumper <- as.integer(wSize / 2)

for (i in 1:nrow(rawdata)) {
  if (i > 0 & i %% wJumper == 0 & i <= nrow(rawdata) - wJumper) {
    fsd$Timestamp[wIndex] <- # timestamp
      as.character(rawdata$Timestamp[i - wJumper - 1])
    
    tmpRawXWindow <- rawdata$x[rawdata$id >= i - wJumper - 1 & rawdata$id < i + wJumper];
    tmpRawYWindow <- rawdata$y[rawdata$id >= i - wJumper - 1 & rawdata$id < i + wJumper];
    tmpRawZWindow <- rawdata$z[rawdata$id >= i - wJumper - 1 & rawdata$id < i + wJumper];
    tmpRawAlphaWindow <- rawdata$alpha[rawdata$id >= i - wJumper - 1 & rawdata$id < i + wJumper];
    tmpRawBetaWindow <- rawdata$beta[rawdata$id >= i - wJumper - 1 & rawdata$id < i + wJumper];
    tmpRawGammaWindow <- rawdata$gamma[rawdata$id >= i - wJumper - 1 & rawdata$id < i + wJumper];
    
    # Min (x) Accelerometer
    fsd$XminA[wIndex] <- min(tmpRawXWindow);
    # Min (y) Accelerometer
    fsd$YminA[wIndex] <- min(tmpRawYWindow);
    # Min (z) Accelerometer
    fsd$ZminA[wIndex] <- min(tmpRawZWindow);
    # Min (x) Gyroscope
    fsd$XminG[wIndex] <- min(tmpRawAlphaWindow);
    # Min (y) Gyroscope
    fsd$YminG[wIndex] <- min(tmpRawBetaWindow);
    # Min (z) Gyroscope
    fsd$ZminG[wIndex] <- min(tmpRawGammaWindow);
    # Max (x) Accelerometer
    fsd$XmaxA[wIndex] <- max(tmpRawXWindow);
    # Max (y) Accelerometer
    fsd$YmaxA[wIndex] <- max(tmpRawYWindow);
    # Max (z) Accelerometer
    fsd$ZmaxA[wIndex] <- max(tmpRawZWindow);
    # Max (x) Gyroscope
    fsd$XmaxG[wIndex] <- max(tmpRawAlphaWindow);
    # Max (y) Gyroscope
    fsd$YmaxG[wIndex] <- max(tmpRawBetaWindow);
    # Max (z) Gyroscope
    fsd$ZmaxG[wIndex] <- max(tmpRawGammaWindow);
    # Mean (x) Accelerometer
    fsd$XmeanA[wIndex] <- mean(tmpRawXWindow);
    # Mean (y) Accelerometer
    fsd$YmeanA[wIndex] <- mean(tmpRawYWindow);
    # Mean (z) Accelerometer
    fsd$ZmeanA[wIndex] <- mean(tmpRawZWindow);
    # Mean (x) Gyroscope
    fsd$XmeanG[wIndex] <- mean(tmpRawAlphaWindow);
    # Mean (y) Gyroscope
    fsd$YmeanG[wIndex] <- mean(tmpRawBetaWindow);
    # Mean (z) Gyroscope
    fsd$ZmeanG[wIndex] <- mean(tmpRawGammaWindow);
    # Median (x) Accelerometer
    fsd$XmedianA[wIndex] <- median(tmpRawXWindow);
    # Median (y) Accelerometer
    fsd$YmedianA[wIndex] <- median(tmpRawYWindow);
    # Median (z) Accelerometer
    fsd$ZmedianA[wIndex] <- median(tmpRawZWindow);
    # Median (x) Gyroscope
    fsd$XmedianG[wIndex] <- median(tmpRawAlphaWindow);
    # Median (y) Gyroscope
    fsd$YmedianG[wIndex] <- median(tmpRawBetaWindow);
    # Median (z) Gyroscope
    fsd$ZmedianG[wIndex] <- median(tmpRawGammaWindow);
    # Standard Deviation (x) Accelerometer
    fsd$XsdA[wIndex] <- sd(tmpRawXWindow);
    # Standard Deviation (y) Accelerometer
    fsd$YsdA[wIndex] <- sd(tmpRawYWindow);
    # Standard Deviation (z) Accelerometer
    fsd$ZsdA[wIndex] <- sd(tmpRawZWindow);
    # Standard Deviation (x) Gyroscope
    fsd$XsdG[wIndex] <- sd(tmpRawAlphaWindow);
    # Standard Deviation (y) Gyroscope
    fsd$YsdG[wIndex] <- sd(tmpRawBetaWindow);
    # Standard Deviation (z) Gyroscope
    fsd$ZsdG[wIndex] <- sd(tmpRawGammaWindow);
    # Variance (x) Accelerometer
    fsd$XvarA[wIndex] <- var(tmpRawXWindow);
    # Variance (y) Accelerometer
    fsd$YvarA[wIndex] <- var(tmpRawYWindow);
    # Variance (z) Accelerometer
    fsd$ZvarA[wIndex] <- var(tmpRawZWindow);
    # Variance (x) Gyroscope
    fsd$XvarG[wIndex] <- var(tmpRawAlphaWindow);
    # Variance (y) Gyroscope
    fsd$YvarG[wIndex] <- var(tmpRawBetaWindow);
    # Variance (z) Gyroscope
    fsd$ZvarG[wIndex] <- var(tmpRawGammaWindow);
    # Skewness (x) Accelerometer
    fsd$XskewA[wIndex] <- 3 * (fsd$XmeanA[wIndex] - fsd$XmedianA[wIndex])/fsd$XsdA[wIndex]
    # Skewness (y) Accelerometer
    fsd$YskewA[wIndex] <- 3 * (fsd$YmeanA[wIndex] - fsd$YmedianA[wIndex])/fsd$YsdA[wIndex]
    # Skewness (z) Accelerometer
    fsd$ZskewA[wIndex] <- 3 * (fsd$ZmeanA[wIndex] - fsd$ZmedianA[wIndex])/fsd$ZsdA[wIndex]
    # Skewness (x) Gyroscope
    fsd$XskewG[wIndex] <- 3 * (fsd$XmeanG[wIndex] - fsd$XmedianG[wIndex])/fsd$XsdG[wIndex]
    # Skewness (y) Gyroscope
    fsd$YskewG[wIndex] <- 3 * (fsd$YmeanG[wIndex] - fsd$YmedianG[wIndex])/fsd$YsdG[wIndex]
    # Skewness (z) Gyroscope
    fsd$ZskewG[wIndex] <- 3 * (fsd$ZmeanG[wIndex] - fsd$ZmedianG[wIndex])/fsd$ZsdG[wIndex]
    # RMS(x) Accelerometer
    fsd$XrmsA[wIndex] <- sqrt(sum((tmpRawXWindow) ^ 2) / wSize)
    # RMS(y) Accelerometer
    fsd$YrmsA[wIndex] <- sqrt(sum((tmpRawYWindow) ^ 2) / wSize)
    # RMS(z) Accelerometer
    fsd$ZrmsA[wIndex] <- sqrt(sum((tmpRawZWindow) ^ 2) / wSize)
    # RMS(x) Gyroscope
    fsd$XrmsG[wIndex] <- sqrt(sum((tmpRawAlphaWindow) ^ 2) / wSize)
    # RMS(y) Gyroscope
    fsd$YrmsG[wIndex] <- sqrt(sum((tmpRawBetaWindow) ^ 2) / wSize)
    # RMS(z) Gyroscope
    fsd$ZrmsG[wIndex] <- sqrt(sum((tmpRawGammaWindow) ^ 2) / wSize)
    # Root Mean Square of the magnitude of the Accelerometer vector
    fsd$MagnRmsA[wIndex] <- sqrt((sum((tmpRawXWindow) ^ 2) + sum((tmpRawYWindow) ^ 2) + sum((tmpRawZWindow) ^ 2)) / wSize)
    # Root Mean Square of the magnitude of the Gyroscope vector
    fsd$MagnRmsG[wIndex] <- sqrt((sum((tmpRawAlphaWindow) ^ 2) + sum((tmpRawBetaWindow) ^ 2) + sum((tmpRawGammaWindow) ^ 2)) / wSize) 
    # time of window
    fsd$TotalTime[wIndex] <- rawdata$Timestamp[i + wJumper - 1] - rawdata$Timestamp[i - wJumper - 1]
    # number of samples in window
    fsd$WindowSize[wIndex] <- wSize
    
    wIndex <- wIndex + 1
    # window loop var
  }
  i <- i + wJumper
}
write.csv(fsd,
          "C:\\git\\data-thesis\\R\\datasets\\new.csv",
          row.names = FALSE)