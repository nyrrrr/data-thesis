sensortrain <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\17011020-sensor-dataset-training-raw.csv",
    header = TRUE,
    stringsAsFactors=FALSE
  )

keytrain <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\17011020-key-dataset-training-raw.csv",
    header = TRUE,
    stringsAsFactors=FALSE
  )

options(digits=16)
# --- preprocessing

# rename vars
sensortrain$xA <- sensortrain$x
sensortrain$yA <- sensortrain$y
sensortrain$zA <- sensortrain$z
sensortrain$xG <- sensortrain$a
sensortrain$yG <- sensortrain$b
sensortrain$zG <- sensortrain$c

sensortrain$x <- NULL
sensortrain$y <- NULL
sensortrain$z <- NULL
sensortrain$a <- NULL
sensortrain$b <- NULL
sensortrain$c <- NULL

# slope dy/dx
sensortrain$XslopeA[1] <- sensortrain$xA[1]
sensortrain$XslopeA[2:length(sensortrain$Timestamp)] <- (((sensortrain$Timestamp[-1] - sensortrain$Timestamp[-length(sensortrain$Timestamp)]))/1000000000)/(sensortrain$xA[-1] - sensortrain$xA[-length(sensortrain$xA)])
sensortrain$YslopeA[1] <- sensortrain$yA[1]
sensortrain$YslopeA[2:length(sensortrain$Timestamp)] <- (((sensortrain$Timestamp[-1] - sensortrain$Timestamp[-length(sensortrain$Timestamp)]))/1000000000)/(sensortrain$yA[-1] - sensortrain$yA[-length(sensortrain$yA)])
sensortrain$ZslopeA[1] <- sensortrain$zA[1]
sensortrain$ZslopeA[2:length(sensortrain$Timestamp)] <- (((sensortrain$Timestamp[-1] - sensortrain$Timestamp[-length(sensortrain$Timestamp)]))/1000000000)/(sensortrain$zA[-1] - sensortrain$zA[-length(sensortrain$zA)])
sensortrain$XslopeG[1] <- sensortrain$xG[1]
sensortrain$XslopeG[2:length(sensortrain$Timestamp)] <- (((sensortrain$Timestamp[-1] - sensortrain$Timestamp[-length(sensortrain$Timestamp)]))/1000000000)/(sensortrain$xG[-1] - sensortrain$xG[-length(sensortrain$xG)])
sensortrain$YslopeG[1] <- sensortrain$yG[1]
sensortrain$YslopeG[2:length(sensortrain$Timestamp)] <- (((sensortrain$Timestamp[-1] - sensortrain$Timestamp[-length(sensortrain$Timestamp)]))/1000000000)/(sensortrain$yG[-1] - sensortrain$yG[-length(sensortrain$yG)])
sensortrain$ZslopeG[1] <- sensortrain$zG[1]
sensortrain$ZslopeG[2:length(sensortrain$Timestamp)] <- (((sensortrain$Timestamp[-1] - sensortrain$Timestamp[-length(sensortrain$Timestamp)]))/1000000000)/(sensortrain$zG[-1] - sensortrain$zG[-length(sensortrain$zG)])
sensortrain$XslopeO[1] <- sensortrain$alpha[1]
sensortrain$XslopeO[2:length(sensortrain$Timestamp)] <- (((sensortrain$Timestamp[-1] - sensortrain$Timestamp[-length(sensortrain$Timestamp)]))/1000000000)/(sensortrain$alpha[-1] - sensortrain$alpha[-length(sensortrain$alpha)])
sensortrain$YslopeO[1] <- sensortrain$beta[1]
sensortrain$YslopeO[2:length(sensortrain$Timestamp)] <- (((sensortrain$Timestamp[-1] - sensortrain$Timestamp[-length(sensortrain$Timestamp)]))/1000000000)/(sensortrain$beta[-1] - sensortrain$beta[-length(sensortrain$beta)])
sensortrain$ZslopeO[1] <- sensortrain$gamma[1]
sensortrain$ZslopeO[2:length(sensortrain$Timestamp)] <- (((sensortrain$Timestamp[-1] - sensortrain$Timestamp[-length(sensortrain$Timestamp)]))/1000000000)/(sensortrain$gamma[-1] - sensortrain$gamma[-length(sensortrain$gamma)])

sensortrain$XslopeA[sensortrain$XslopeA == Inf] <- 0
sensortrain$YslopeA[sensortrain$YslopeA == Inf] <- 0
sensortrain$ZslopeA[sensortrain$ZslopeA == Inf] <- 0
sensortrain$XslopeG[sensortrain$XslopeG == Inf] <- 0
sensortrain$YslopeG[sensortrain$YslopeG == Inf] <- 0
sensortrain$ZslopeG[sensortrain$ZslopeG == Inf] <- 0
sensortrain$XslopeO[sensortrain$XslopeO == Inf] <- 0
sensortrain$YslopeO[sensortrain$YslopeO == Inf] <- 0
sensortrain$ZslopeO[sensortrain$ZslopeO == Inf] <- 0

# Square Sum of 3D vectors
sensortrain$SqSumA <- sensortrain$xA ^ 2 + sensortrain$yA ^ 2 + sensortrain$zA ^ 2
sensortrain$SqSumG <- sensortrain$xG^2 + sensortrain$yG^2 + sensortrain$zG^2
sensortrain$SqSumO <- sensortrain$alpha^2 + sensortrain$beta^2 + sensortrain$gamma^2

# Magnitude of 3D Vectors
sensortrain$MagnA <- sqrt(sensortrain$SqSumA)
sensortrain$MagnG <- sqrt(sensortrain$SqSumG)
sensortrain$MagnO <- sqrt(sensortrain$SqSumO)

# norm vector values
sensortrain$XnormMagnA <- sensortrain$xA/sensortrain$MagnA
sensortrain$YnormMagnA <- sensortrain$yA/sensortrain$MagnA
sensortrain$ZnormMagnA <- sensortrain$zA/sensortrain$MagnA
sensortrain$XnormMagnG <- sensortrain$xG/sensortrain$MagnG
sensortrain$YnormMagnG <- sensortrain$yG/sensortrain$MagnG
sensortrain$ZnormMagnG <- sensortrain$zG/sensortrain$MagnG
sensortrain$XnormMagnO <- sensortrain$xG/sensortrain$MagnO
sensortrain$YnormMagnO <- sensortrain$yG/sensortrain$MagnO
sensortrain$ZnormMagnO <- sensortrain$zG/sensortrain$MagnO

# norm vector values
sensortrain$XnormMeanA <- sensortrain$xA - mean(sensortrain$xA)
sensortrain$YnormMeanA <- sensortrain$yA - mean(sensortrain$yA)
sensortrain$ZnormMeanA <-sensortrain$zA - mean(sensortrain$zA)
sensortrain$XnormMeanG <- sensortrain$xG - mean(sensortrain$xG)
sensortrain$YnormMeanG <- sensortrain$yG - mean(sensortrain$yG)
sensortrain$ZnormMeanG <- sensortrain$zG - mean(sensortrain$zG)
sensortrain$XnormMeanO <- sensortrain$alpha - mean(sensortrain$alpha)
sensortrain$YnormMeanO <- sensortrain$beta - mean(sensortrain$beta)
sensortrain$ZnormMeanO <- sensortrain$gamma - mean(sensortrain$gamma)

# distance between Value and previous value
sensortrain$XdeltaA[1] <- sensortrain$xA[1]
sensortrain$YdeltaA[1] <- sensortrain$yA[1]
sensortrain$ZdeltaA[1] <- sensortrain$zA[1]
sensortrain$XdeltaG[1] <- sensortrain$xG[1]
sensortrain$YdeltaG[1] <- sensortrain$yG[1]
sensortrain$ZdeltaG[1] <- sensortrain$zG[1]
sensortrain$XdeltaO[1] <- sensortrain$alpha[1]
sensortrain$YdeltaO[1] <- sensortrain$beta[1]
sensortrain$ZdeltaO[1] <- sensortrain$gamma[1]
sensortrain$XdeltaA[2:length(sensortrain$xA)] <- sensortrain$xA[-1] - sensortrain$xA[-length(sensortrain$xA)]
sensortrain$YdeltaA[2:length(sensortrain$xA)] <- sensortrain$yA[-1] - sensortrain$yA[-length(sensortrain$yA)]
sensortrain$ZdeltaA[2:length(sensortrain$xA)] <- sensortrain$zA[-1] - sensortrain$zA[-length(sensortrain$zA)]
sensortrain$XdeltaG[2:length(sensortrain$xA)] <- sensortrain$xG[-1] - sensortrain$xG[-length(sensortrain$xG)]
sensortrain$YdeltaG[2:length(sensortrain$xA)] <- sensortrain$yG[-1] - sensortrain$yG[-length(sensortrain$yG)]
sensortrain$ZdeltaG[2:length(sensortrain$xA)] <- sensortrain$zG[-1] - sensortrain$zG[-length(sensortrain$zG)]
sensortrain$XdeltaO[2:length(sensortrain$xA)] <- sensortrain$alpha[-1] - sensortrain$alpha[-length(sensortrain$alpha)]
sensortrain$YdeltaO[2:length(sensortrain$xA)] <- sensortrain$beta[-1] - sensortrain$beta[-length(sensortrain$beta)]
sensortrain$ZdeltaO[2:length(sensortrain$xA)] <- sensortrain$gamma[-1] - sensortrain$gamma[-length(sensortrain$gamma)]

sensortrain$MdeltaA[1] <- sensortrain$MagnA[1]
sensortrain$MdeltaG[1] <- sensortrain$MagnG[1]
sensortrain$MdeltaO[1] <- sensortrain$MagnO[1]
sensortrain$MdeltaA[2:length(sensortrain$MagnA)] <- sensortrain$MagnA[-1] - sensortrain$MagnA[-length(sensortrain$MagnA)]
sensortrain$MdeltaG[2:length(sensortrain$MagnG)] <- sensortrain$MagnG[-1] - sensortrain$MagnG[-length(sensortrain$MagnG)]
sensortrain$MdeltaO[2:length(sensortrain$MagnO)] <- sensortrain$MagnO[-1] - sensortrain$MagnO[-length(sensortrain$MagnO)]

# low pass filter: y[i] := y[i-1] + α * (x[i] - x[i-1])
filterfactor <- 0.9
for(i in seq_along(sensortrain$xA)) {
  if(i == 1) {
    sensortrain$XlpA[1] <- sensortrain$xA[1]
    sensortrain$YlpA[1] <- sensortrain$yA[1]
    sensortrain$ZlpA[1] <- sensortrain$zA[1]
    sensortrain$XlpG[1] <- sensortrain$xG[1]
    sensortrain$YlpG[1] <- sensortrain$yG[1]
    sensortrain$ZlpG[1] <- sensortrain$zG[1]
    sensortrain$XlpO[1] <- sensortrain$alpha[1]
    sensortrain$YlpO[1] <- sensortrain$beta[1]
    sensortrain$ZlpO[1] <- sensortrain$gamma[1]
  }
  else {
    sensortrain$XlpA[i] <- sensortrain$XlpA[i-1] + filterfactor * sensortrain$XdeltaA[i]
    sensortrain$YlpA[i] <- sensortrain$YlpA[i-1] + filterfactor * sensortrain$YdeltaA[i]
    sensortrain$ZlpA[i] <- sensortrain$ZlpA[i-1] + filterfactor * sensortrain$ZdeltaA[i]
    sensortrain$XlpG[i] <- sensortrain$XlpG[i-1] + filterfactor * sensortrain$XdeltaG[i]
    sensortrain$YlpG[i] <- sensortrain$YlpG[i-1] + filterfactor * sensortrain$YdeltaG[i]
    sensortrain$ZlpG[i] <- sensortrain$ZlpG[i-1] + filterfactor * sensortrain$ZdeltaG[i]
    sensortrain$XlpO[i] <- sensortrain$XlpO[i-1] + filterfactor * sensortrain$XdeltaO[i]
    sensortrain$YlpO[i] <- sensortrain$YlpO[i-1] + filterfactor * sensortrain$YdeltaO[i]
    sensortrain$ZlpO[i] <- sensortrain$ZlpO[i-1] + filterfactor * sensortrain$ZdeltaO[i]
  }
}
# magnitude of low pass filtered values
sensortrain$MagnLpA <- sqrt(sensortrain$XlpA^2 + sensortrain$YlpA^2 + sensortrain$ZlpA^2)
sensortrain$MagnLpG <- sqrt(sensortrain$XlpG^2 + sensortrain$YlpG^2 + sensortrain$ZlpG^2)
sensortrain$MagnLpO <- sqrt(sensortrain$XlpO^2 + sensortrain$YlpO^2 + sensortrain$ZlpO^2)

write.csv(sensortrain,
          "C:\\git\\data-thesis\\R\\datasets\\17011020-sensor-dataset-training-preprocessed.csv",
          row.names = FALSE)

# --- feature extraction


# feature data set;
# ftrain <- keytrain
ftrain <- keytrain[rep(1:nrow(keytrain),each=2),] 

varDownTime <- ftrain$DownTime
varEventTime <- ftrain$EventTime

for (i in 1:length(seq_along(ftrain$DownTime))) {
  
  # create windows for aggregation over keys
  if(i %% 2 == 0) {
    tmpRawXWindow <- sensortrain$xA[sensortrain$Timestamp >= ftrain$DownTime[i] &
                      sensortrain$Timestamp <= ftrain$EventTime[i]]
    tmpRawYWindow <- sensortrain$xA[sensortrain$Timestamp >= ftrain$DownTime[i] &
                      sensortrain$Timestamp <= ftrain$EventTime[i]]
    tmpRawZWindow <- sensortrain$xA[sensortrain$Timestamp >= ftrain$DownTime[i] &
                      sensortrain$Timestamp <= ftrain$EventTime[i]]
    tmpRawAWindow <- sensortrain$xA[sensortrain$Timestamp >= ftrain$DownTime[i] &
                      sensortrain$Timestamp <= ftrain$EventTime[i]]
    tmpRawBWindow <- sensortrain$xA[sensortrain$Timestamp >= ftrain$DownTime[i] &
                      sensortrain$Timestamp <= ftrain$EventTime[i]]
    tmpRawCWindow <- sensortrain$xA[sensortrain$Timestamp >= ftrain$DownTime[i] &
                      sensortrain$Timestamp <= ftrain$EventTime[i]]
    tmpRawAlphaWindow <- sensortrain$xA[sensortrain$Timestamp >= ftrain$DownTime[i] &
                      sensortrain$Timestamp <= ftrain$EventTime[i]]
    tmpRawBetaWindow <- sensortrain$xA[sensortrain$Timestamp >= ftrain$DownTime[i] &
                      sensortrain$Timestamp <= ftrain$EventTime[i]]
    tmpRawGammaWindow <- sensortrain$xA[sensortrain$Timestamp >= ftrain$DownTime[i] &
                      sensortrain$Timestamp <= ftrain$EventTime[i]]
    tmpRawMagnAWindow <- sensortrain$xA[sensortrain$Timestamp >= ftrain$DownTime[i] &
                      sensortrain$Timestamp <= ftrain$EventTime[i]]
    tmpRawMagnGWindow <- sensortrain$xA[sensortrain$Timestamp >= ftrain$DownTime[i] &
                      sensortrain$Timestamp <= ftrain$EventTime[i]]
    tmpRawMagnOWindow <- sensortrain$xA[sensortrain$Timestamp >= ftrain$DownTime[i] &
                      sensortrain$Timestamp <= ftrain$EventTime[i]]
    tmpRawSqSumAWindow <- sensortrain$xA[sensortrain$Timestamp >= ftrain$DownTime[i] &
                      sensortrain$Timestamp <= ftrain$EventTime[i]]
    tmpRawSqSumGWindow <- sensortrain$xA[sensortrain$Timestamp >= ftrain$DownTime[i] &
                      sensortrain$Timestamp <= ftrain$EventTime[i]]
    tmpRawSqSumOWindow <- sensortrain$xA[sensortrain$Timestamp >= ftrain$DownTime[i] &
                      sensortrain$Timestamp <= ftrain$EventTime[i]]
  } else {
  # create windows for aggregation over the sensor data between two key presses
    tmpRawXWindow <- sensortrain$xA[sensortrain$Timestamp < ftrain$DownTime[i] & sensortrain$Timestamp > c(0, ftrain$DownTime)[i]]
    tmpRawYWindow <- sensortrain$yA[sensortrain$Timestamp < ftrain$DownTime[i] & sensortrain$Timestamp > c(0, ftrain$DownTime)[i]]
    tmpRawZWindow <- sensortrain$zA[sensortrain$Timestamp < ftrain$DownTime[i] & sensortrain$Timestamp > c(0, ftrain$DownTime)[i]]
    tmpRawAWindow <- sensortrain$xG[sensortrain$Timestamp < ftrain$DownTime[i] & sensortrain$Timestamp > c(0, ftrain$DownTime)[i]]
    tmpRawBWindow <- sensortrain$yG[sensortrain$Timestamp < ftrain$DownTime[i] & sensortrain$Timestamp > c(0, ftrain$DownTime)[i]]
    tmpRawCWindow <- sensortrain$zG[sensortrain$Timestamp < ftrain$DownTime[i] & sensortrain$Timestamp > c(0, ftrain$DownTime)[i]]
    tmpRawAlphaWindow <- sensortrain$alpha[sensortrain$Timestamp < ftrain$DownTime[i] & sensortrain$Timestamp > c(0, ftrain$DownTime)[i]]
    tmpRawBetaWindow <- sensortrain$beta[sensortrain$Timestamp < ftrain$DownTime[i] & sensortrain$Timestamp > c(0, ftrain$DownTime)[i]]
    tmpRawGammaWindow <- sensortrain$gamma[sensortrain$Timestamp < ftrain$DownTime[i] & sensortrain$Timestamp > c(0, ftrain$DownTime)[i]]
    tmpRawMagnAWindow <- sensortrain$MagnA[sensortrain$Timestamp < ftrain$DownTime[i] & sensortrain$Timestamp > c(0, ftrain$DownTime)[i]]
    tmpRawMagnGWindow <- sensortrain$MagnG[sensortrain$Timestamp < ftrain$DownTime[i] & sensortrain$Timestamp > c(0, ftrain$DownTime)[i]]
    tmpRawMagnOWindow <- sensortrain$MagnO[sensortrain$Timestamp < ftrain$DownTime[i] & sensortrain$Timestamp > c(0, ftrain$DownTime)[i]]
    tmpRawSqSumAWindow <- sensortrain$SqSumA[sensortrain$Timestamp < ftrain$DownTime[i] & sensortrain$Timestamp > c(0, ftrain$DownTime)[i]]
    tmpRawSqSumGWindow <- sensortrain$SqSumG[sensortrain$Timestamp < ftrain$DownTime[i] & sensortrain$Timestamp > c(0, ftrain$DownTime)[i]]
    tmpRawSqSumOWindow <- sensortrain$SqSumO[sensortrain$Timestamp < ftrain$DownTime[i] & sensortrain$Timestamp > c(0, ftrain$DownTime)[i]]
    
    #fix label
    ftrain$Keypress[i] <- "NONE"
    
    # fix timestamps
    if(i == 1) varDownTime[1] <- sensortrain$Timestamp[1]
    else varDownTime[i] <- ftrain$EventTime[i-1] + 1
    
    varEventTime[i] <- ftrain$DownTime[i] - 1
  }
  
  ftrain$WindowSize[i] <- length(tmpRawXWindow)
  wSize <- ftrain$WindowSize[i]
  
  # Min Accelerometer
  ftrain$XminA[i] <- min(tmpRawXWindow)
  ftrain$YminA[i] <- min(tmpRawYWindow)
  ftrain$ZminA[i] <- min(tmpRawZWindow)
  ftrain$XminG[i] <- min(tmpRawAWindow)
  ftrain$YminG[i] <- min(tmpRawBWindow)
  ftrain$ZminG[i] <- min(tmpRawCWindow)
  ftrain$XminO[i] <- min(tmpRawAlphaWindow)
  ftrain$YminO[i] <- min(tmpRawBetaWindow)
  ftrain$ZminO[i] <- min(tmpRawGammaWindow)

  #min magnitude
  ftrain$MminA[i] <- min(tmpRawMagnAWindow)
  ftrain$MminG[i] <- min(tmpRawMagnGWindow)
  ftrain$MminO[i] <- min(tmpRawMagnOWindow)

  # Max
  ftrain$XmaxA[i] <- max(tmpRawXWindow)
  ftrain$YmaxA[i] <- max(tmpRawYWindow)
  ftrain$ZmaxA[i] <- max(tmpRawZWindow)
  ftrain$XmaxG[i] <- max(tmpRawAWindow)
  ftrain$YmaxG[i] <- max(tmpRawBWindow)
  ftrain$ZmaxG[i] <- max(tmpRawCWindow)
  ftrain$XmaxO[i] <- max(tmpRawAlphaWindow)
  ftrain$YmaxO[i] <- max(tmpRawBetaWindow)
  ftrain$ZmaxO[i] <- max(tmpRawGammaWindow)

  #max magnitude
  ftrain$MmaxA[i] <- max(tmpRawMagnAWindow)
  ftrain$MmaxG[i] <- max(tmpRawMagnGWindow)
  ftrain$MmaxO[i] <- max(tmpRawMagnOWindow)

  # Mean
  ftrain$XmeanA[i] <- mean(tmpRawXWindow)
  ftrain$YmeanA[i] <- mean(tmpRawYWindow)
  ftrain$ZmeanA[i] <- mean(tmpRawZWindow)
  ftrain$XmeanG[i] <- mean(tmpRawAWindow)
  ftrain$YmeanG[i] <- mean(tmpRawBWindow)
  ftrain$ZmeanG[i] <- mean(tmpRawCWindow)
  ftrain$XmeanO[i] <- mean(tmpRawAlphaWindow)
  ftrain$YmeanO[i] <- mean(tmpRawBetaWindow)
  ftrain$ZmeanO[i] <- mean(tmpRawGammaWindow)

  # mean magn
  ftrain$MmeanA[i] <- mean(tmpRawMagnAWindow)
  ftrain$MmeanG[i] <- mean(tmpRawMagnGWindow)
  ftrain$MmeanO[i] <- mean(tmpRawMagnOWindow)

  # Median
  ftrain$XmedianA[i] <- median(tmpRawXWindow)
  ftrain$YmedianA[i] <- median(tmpRawYWindow)
  ftrain$ZmedianA[i] <- median(tmpRawZWindow)
  ftrain$XmedianG[i] <- median(tmpRawAWindow)
  ftrain$YmedianG[i] <- median(tmpRawBWindow)
  ftrain$ZmedianG[i] <- median(tmpRawCWindow)
  ftrain$XmedianO[i] <- median(tmpRawAlphaWindow)
  ftrain$YmedianO[i] <- median(tmpRawBetaWindow)
  ftrain$ZmedianO[i] <- median(tmpRawGammaWindow)

  # median magn
  ftrain$MmedianA[i] <- median(tmpRawMagnAWindow)
  ftrain$MmedianG[i] <- median(tmpRawMagnGWindow)
  ftrain$MmedianO[i] <- median(tmpRawMagnOWindow)

  # Standard Deviation
  ftrain$XsdA[i] <- sd(tmpRawXWindow)
  ftrain$YsdA[i] <- sd(tmpRawYWindow)
  ftrain$ZsdA[i] <- sd(tmpRawZWindow)
  ftrain$XsdG[i] <- sd(tmpRawAWindow)
  ftrain$YsdG[i] <- sd(tmpRawBWindow)
  ftrain$ZsdG[i] <- sd(tmpRawCWindow)
  ftrain$XsdO[i] <- sd(tmpRawAlphaWindow)
  ftrain$YsdO[i] <- sd(tmpRawBetaWindow)
  ftrain$ZsdO[i] <- sd(tmpRawGammaWindow)

  #sd magn
  ftrain$MsdA[i] <- sd(tmpRawMagnAWindow)
  ftrain$MsdG[i] <- sd(tmpRawMagnGWindow)
  ftrain$MsdO[i] <- sd(tmpRawMagnOWindow)

  # Variance
  ftrain$XvarA[i] <- var(tmpRawXWindow)
  ftrain$YvarA[i] <- var(tmpRawYWindow)
  ftrain$ZvarA[i] <- var(tmpRawZWindow)
  ftrain$XvarG[i] <- var(tmpRawAWindow)
  ftrain$YvarG[i] <- var(tmpRawBWindow)
  ftrain$ZvarG[i] <- var(tmpRawCWindow)
  ftrain$XvarO[i] <- var(tmpRawAlphaWindow)
  ftrain$YvarO[i] <- var(tmpRawBetaWindow)
  ftrain$ZvarO[i] <- var(tmpRawGammaWindow)

  #var magn
  ftrain$MvarA[i] <- var(tmpRawMagnAWindow)
  ftrain$MvarG[i] <- var(tmpRawMagnGWindow)
  ftrain$MvarO[i] <- var(tmpRawMagnOWindow)


  # RMS  Accelerometer
  ftrain$XrmsA[i] <- sqrt(sum((tmpRawXWindow) ^ 2) / wSize)
  ftrain$YrmsA[i] <- sqrt(sum((tmpRawYWindow) ^ 2) / wSize)
  ftrain$ZrmsA[i] <- sqrt(sum((tmpRawZWindow) ^ 2) / wSize)
  ftrain$XrmsG[i] <- sqrt(sum((tmpRawAWindow) ^ 2) / wSize)
  ftrain$YrmsG[i] <- sqrt(sum((tmpRawBWindow) ^ 2) / wSize)
  ftrain$ZrmsG[i] <- sqrt(sum((tmpRawCWindow) ^ 2) / wSize)
  ftrain$XrmsO[i] <- sqrt(sum((tmpRawAlphaWindow) ^ 2) / wSize)
  ftrain$YrmsO[i] <- sqrt(sum((tmpRawBetaWindow) ^ 2) / wSize)
  ftrain$ZrmsO[i] <- sqrt(sum((tmpRawGammaWindow) ^ 2) / wSize)

  # Root Mean Square of the magnitude
  ftrain$MagnRmsA[i] <- sqrt((sum((tmpRawMagnAWindow) ^ 2)) / wSize)
  ftrain$MagnRmsG[i] <- sqrt((sum((tmpRawMagnGWindow) ^ 2)) / wSize)
  ftrain$MagnRmsO[i] <- sqrt((sum((tmpRawMagnOWindow) ^ 2)) / wSize)

  # Root Mean Square of the square sum of
  ftrain$SqSumRmsA[i] <- sqrt((sum((tmpRawSqSumAWindow) ^ 2)) / wSize)
  ftrain$SqSumRmsG[i] <- sqrt((sum((tmpRawSqSumGWindow) ^ 2)) / wSize)
  ftrain$SqSumRmsO[i] <- sqrt((sum((tmpRawSqSumOWindow) ^ 2)) / wSize)
}

# Skewness 
ftrain$XskewA <- 3 * (ftrain$XmeanA - ftrain$XmedianA) / ftrain$XsdA
ftrain$YskewA <- 3 * (ftrain$YmeanA - ftrain$YmedianA) / ftrain$YsdA
ftrain$ZskewA <- 3 * (ftrain$ZmeanA - ftrain$ZmedianA) / ftrain$ZsdA
ftrain$XskewG <- 3 * (ftrain$XmeanG - ftrain$XmedianG) / ftrain$XsdG
ftrain$YskewG <- 3 * (ftrain$YmeanG - ftrain$YmedianG) / ftrain$YsdG
ftrain$ZskewG <- 3 * (ftrain$ZmeanG - ftrain$ZmedianG) / ftrain$ZsdG
ftrain$XskewO <- 3 * (ftrain$XmeanO - ftrain$XmedianO) / ftrain$XsdO
ftrain$YskewO <- 3 * (ftrain$YmeanO - ftrain$YmedianO) / ftrain$YsdO
ftrain$ZskewO <- 3 * (ftrain$ZmeanO - ftrain$ZmedianO) / ftrain$ZsdO
# skewness magn
ftrain$MskewA <- 3 * (ftrain$MmeanA - ftrain$MmedianA) / ftrain$MsdA
ftrain$MskewG <- 3 * (ftrain$MmeanG - ftrain$MmedianG) / ftrain$MsdG
ftrain$MskewO <- 3 * (ftrain$MmeanO - ftrain$MmedianO) / ftrain$MsdO

# time of window
ftrain$DownTime <- floor(ftrain$DownTime / 1000000)
ftrain$EventTime <- ftrain$EventTime / 1000000
ftrain$TotalTime <- (ftrain$EventTime - ftrain$DownTime)

ftrain$Keypress <- gsub("KEYCODE_", "KEY_", ftrain$Keypress)

#fix timestamps pt. 2
ftrain$DownTime <- varDownTime
ftrain$EventTime <- varEventTime
# fix keys
ftrain$IsKey <- ftrain$Keypress != "NONE"

write.csv(
  ftrain,
  "C:\\git\\data-thesis\\R\\datasets\\17011020-dataset-training.csv",
  row.names = FALSE
)