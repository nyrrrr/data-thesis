rawdata <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\transferred-17011417-victim-data.csv",
    header = TRUE
  )


options(digits=20)
# --- preprocessing

# rename vars
rawdata$xA <- rawdata$x
rawdata$yA <- rawdata$y
rawdata$zA <- rawdata$z
rawdata$xG <- rawdata$a
rawdata$yG <- rawdata$b
rawdata$zG <- rawdata$c

rawdata$x <- NULL
rawdata$y <- NULL
rawdata$z <- NULL
rawdata$a <- NULL
rawdata$b <- NULL
rawdata$c <- NULL

# slope dy/dx
rawdata$XslopeA[1] <- rawdata$xA[1]
rawdata$XslopeA[2:length(rawdata$Timestamp)] <- (((rawdata$Timestamp[-1] - rawdata$Timestamp[-length(rawdata$Timestamp)]))/1000000000)/(rawdata$xA[-1] - rawdata$xA[-length(rawdata$xA)])
rawdata$YslopeA[1] <- rawdata$yA[1]
rawdata$YslopeA[2:length(rawdata$Timestamp)] <- (((rawdata$Timestamp[-1] - rawdata$Timestamp[-length(rawdata$Timestamp)]))/1000000000)/(rawdata$yA[-1] - rawdata$yA[-length(rawdata$yA)])
rawdata$ZslopeA[1] <- rawdata$zA[1]
rawdata$ZslopeA[2:length(rawdata$Timestamp)] <- (((rawdata$Timestamp[-1] - rawdata$Timestamp[-length(rawdata$Timestamp)]))/1000000000)/(rawdata$zA[-1] - rawdata$zA[-length(rawdata$zA)])
rawdata$XslopeG[1] <- rawdata$xG[1]
rawdata$XslopeG[2:length(rawdata$Timestamp)] <- (((rawdata$Timestamp[-1] - rawdata$Timestamp[-length(rawdata$Timestamp)]))/1000000000)/(rawdata$xG[-1] - rawdata$xG[-length(rawdata$xG)])
rawdata$YslopeG[1] <- rawdata$yG[1]
rawdata$YslopeG[2:length(rawdata$Timestamp)] <- (((rawdata$Timestamp[-1] - rawdata$Timestamp[-length(rawdata$Timestamp)]))/1000000000)/(rawdata$yG[-1] - rawdata$yG[-length(rawdata$yG)])
rawdata$ZslopeG[1] <- rawdata$zG[1]
rawdata$ZslopeG[2:length(rawdata$Timestamp)] <- (((rawdata$Timestamp[-1] - rawdata$Timestamp[-length(rawdata$Timestamp)]))/1000000000)/(rawdata$zG[-1] - rawdata$zG[-length(rawdata$zG)])
rawdata$XslopeO[1] <- rawdata$alpha[1]
rawdata$XslopeO[2:length(rawdata$Timestamp)] <- (((rawdata$Timestamp[-1] - rawdata$Timestamp[-length(rawdata$Timestamp)]))/1000000000)/(rawdata$alpha[-1] - rawdata$alpha[-length(rawdata$alpha)])
rawdata$YslopeO[1] <- rawdata$beta[1]
rawdata$YslopeO[2:length(rawdata$Timestamp)] <- (((rawdata$Timestamp[-1] - rawdata$Timestamp[-length(rawdata$Timestamp)]))/1000000000)/(rawdata$beta[-1] - rawdata$beta[-length(rawdata$beta)])
rawdata$ZslopeO[1] <- rawdata$gamma[1]
rawdata$ZslopeO[2:length(rawdata$Timestamp)] <- (((rawdata$Timestamp[-1] - rawdata$Timestamp[-length(rawdata$Timestamp)]))/1000000000)/(rawdata$gamma[-1] - rawdata$gamma[-length(rawdata$gamma)])

rawdata$XslopeA[rawdata$XslopeA == Inf] <- 0
rawdata$YslopeA[rawdata$YslopeA == Inf] <- 0
rawdata$ZslopeA[rawdata$ZslopeA == Inf] <- 0
rawdata$XslopeG[rawdata$XslopeG == Inf] <- 0
rawdata$YslopeG[rawdata$YslopeG == Inf] <- 0
rawdata$ZslopeG[rawdata$ZslopeG == Inf] <- 0
rawdata$XslopeO[rawdata$XslopeO == Inf] <- 0
rawdata$YslopeO[rawdata$YslopeO == Inf] <- 0
rawdata$ZslopeO[rawdata$ZslopeO == Inf] <- 0

# Square Sum of 3D vectors
rawdata$SqSumA <- rawdata$xA ^ 2 + rawdata$yA ^ 2 + rawdata$zA ^ 2
rawdata$SqSumG <- rawdata$xG^2 + rawdata$yG^2 + rawdata$zG^2
rawdata$SqSumO <- rawdata$alpha^2 + rawdata$beta^2 + rawdata$gamma^2

# Magnitude of 3D Vectors
rawdata$MagnA <- sqrt(rawdata$SqSumA)
rawdata$MagnG <- sqrt(rawdata$SqSumG)
rawdata$MagnO <- sqrt(rawdata$SqSumO)

# norm vector values
rawdata$XnormMagnA <- rawdata$xA/rawdata$MagnA
rawdata$YnormMagnA <- rawdata$yA/rawdata$MagnA
rawdata$ZnormMagnA <- rawdata$zA/rawdata$MagnA
rawdata$XnormMagnG <- rawdata$xG/rawdata$MagnG
rawdata$YnormMagnG <- rawdata$yG/rawdata$MagnG
rawdata$ZnormMagnG <- rawdata$zG/rawdata$MagnG
rawdata$XnormMagnO <- rawdata$xG/rawdata$MagnO
rawdata$YnormMagnO <- rawdata$yG/rawdata$MagnO
rawdata$ZnormMagnO <- rawdata$zG/rawdata$MagnO

# norm vector values
rawdata$XnormMeanA <- rawdata$xA - mean(rawdata$xA)
rawdata$YnormMeanA <- rawdata$yA - mean(rawdata$yA)
rawdata$ZnormMeanA <- rawdata$zA - mean(rawdata$zA)
rawdata$XnormMeanG <- rawdata$xG - mean(rawdata$xG)
rawdata$YnormMeanG <- rawdata$yG - mean(rawdata$yG)
rawdata$ZnormMeanG <- rawdata$zG - mean(rawdata$zG)
rawdata$XnormMeanO <- rawdata$alpha - mean(rawdata$alpha)
rawdata$YnormMeanO <- rawdata$beta - mean(rawdata$beta)
rawdata$ZnormMeanO <- rawdata$gamma - mean(rawdata$gamma)

# distance between Value and previous value
rawdata$XdeltaA[1] <- rawdata$xA[1]
rawdata$YdeltaA[1] <- rawdata$yA[1]
rawdata$ZdeltaA[1] <- rawdata$zA[1]
rawdata$XdeltaG[1] <- rawdata$xG[1]
rawdata$YdeltaG[1] <- rawdata$yG[1]
rawdata$ZdeltaG[1] <- rawdata$zG[1]
rawdata$XdeltaO[1] <- rawdata$alpha[1]
rawdata$YdeltaO[1] <- rawdata$beta[1]
rawdata$ZdeltaO[1] <- rawdata$gamma[1]
rawdata$XdeltaA[2:length(rawdata$xA)] <- rawdata$xA[-1] - rawdata$xA[-length(rawdata$xA)]
rawdata$YdeltaA[2:length(rawdata$xA)] <- rawdata$yA[-1] - rawdata$yA[-length(rawdata$yA)]
rawdata$ZdeltaA[2:length(rawdata$xA)] <- rawdata$zA[-1] - rawdata$zA[-length(rawdata$zA)]
rawdata$XdeltaG[2:length(rawdata$xA)] <- rawdata$xG[-1] - rawdata$xG[-length(rawdata$xG)]
rawdata$YdeltaG[2:length(rawdata$xA)] <- rawdata$yG[-1] - rawdata$yG[-length(rawdata$yG)]
rawdata$ZdeltaG[2:length(rawdata$xA)] <- rawdata$zG[-1] - rawdata$zG[-length(rawdata$zG)]
rawdata$XdeltaO[2:length(rawdata$xA)] <- rawdata$alpha[-1] - rawdata$alpha[-length(rawdata$alpha)]
rawdata$YdeltaO[2:length(rawdata$xA)] <- rawdata$beta[-1] - rawdata$beta[-length(rawdata$beta)]
rawdata$ZdeltaO[2:length(rawdata$xA)] <- rawdata$gamma[-1] - rawdata$gamma[-length(rawdata$gamma)]

rawdata$MdeltaA[1] <- rawdata$MagnA[1]
rawdata$MdeltaG[1] <- rawdata$MagnG[1]
rawdata$MdeltaO[1] <- rawdata$MagnO[1]
rawdata$MdeltaA[2:length(rawdata$MagnA)] <- rawdata$MagnA[-1] - rawdata$MagnA[-length(rawdata$MagnA)]
rawdata$MdeltaG[2:length(rawdata$MagnG)] <- rawdata$MagnG[-1] - rawdata$MagnG[-length(rawdata$MagnG)]
rawdata$MdeltaO[2:length(rawdata$MagnO)] <- rawdata$MagnO[-1] - rawdata$MagnO[-length(rawdata$MagnO)]

# low pass filter: y[i] := y[i-1] + α * (x[i] - x[i-1])
filterfactor <- 0.9
for(i in seq_along(rawdata$xA)) {
  if(i == 1) {
    rawdata$XlpA[1] <- rawdata$xA[1]
    rawdata$YlpA[1] <- rawdata$yA[1]
    rawdata$ZlpA[1] <- rawdata$zA[1]
    rawdata$XlpG[1] <- rawdata$xG[1]
    rawdata$YlpG[1] <- rawdata$yG[1]
    rawdata$ZlpG[1] <- rawdata$zG[1]
    rawdata$XlpO[1] <- rawdata$alpha[1]
    rawdata$YlpO[1] <- rawdata$beta[1]
    rawdata$ZlpO[1] <- rawdata$gamma[1]
  }
  else {
    rawdata$XlpA[i] <- rawdata$XlpA[i-1] + filterfactor * rawdata$XdeltaA[i]
    rawdata$YlpA[i] <- rawdata$YlpA[i-1] + filterfactor * rawdata$YdeltaA[i]
    rawdata$ZlpA[i] <- rawdata$ZlpA[i-1] + filterfactor * rawdata$ZdeltaA[i]
    rawdata$XlpG[i] <- rawdata$XlpG[i-1] + filterfactor * rawdata$XdeltaG[i]
    rawdata$YlpG[i] <- rawdata$YlpG[i-1] + filterfactor * rawdata$YdeltaG[i]
    rawdata$ZlpG[i] <- rawdata$ZlpG[i-1] + filterfactor * rawdata$ZdeltaG[i]
    rawdata$XlpO[i] <- rawdata$XlpO[i-1] + filterfactor * rawdata$XdeltaO[i]
    rawdata$YlpO[i] <- rawdata$YlpO[i-1] + filterfactor * rawdata$YdeltaO[i]
    rawdata$ZlpO[i] <- rawdata$ZlpO[i-1] + filterfactor * rawdata$ZdeltaO[i]
  }
  rawdata$id[i] <- i
}
# magnitude of low pass filtered values
rawdata$MagnLpA <- sqrt(rawdata$XlpA^2 + rawdata$YlpA^2 + rawdata$ZlpA^2)
rawdata$MagnLpG <- sqrt(rawdata$XlpG^2 + rawdata$YlpG^2 + rawdata$ZlpG^2)
rawdata$MagnLpO <- sqrt(rawdata$XlpO^2 + rawdata$YlpO^2 + rawdata$ZlpO^2)

# ------ DEBUG ONLY

keytrain <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\17011417-key-dataset-test-raw.csv",
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

# ------ END DEBUG


write.csv(rawdata,
          "C:\\git\\data-thesis\\R\\datasets\\transferred-17011417-victim-data-preprocessed.csv",
          row.names = FALSE)


# window size
wSize <- read.csv(
  "C:\\git\\data-thesis\\R\\datasets\\17011417-dataset-training.csv",
  header = TRUE
)
# wSize <- ceiling(median(wSize$WindowSize))
# wSize <- ceiling(mean(wSize$WindowSize))
wSize <- ceiling(median(wSize$WindowSize[wSize$IsKey == TRUE]))
if(wSize %% 2 == 0) wSize <- wSize - 1
# feature data set;
fsd <- NULL

# helper vars
wIndex <- 0
wJumper <- as.integer(wSize / 2)

for (i in 1:nrow(rawdata)) {
  if (i > 0 & i %% wJumper == 0 & i <= nrow(rawdata) - wJumper) {
    fsd$Timestamp[wIndex] <- # timestamp
      as.character(rawdata$Timestamp[i - wJumper - 1])
    
    tmpWindowCopy <- rawdata[rawdata$id >= i - wJumper - 1 &
                                 rawdata$id < i + wJumper,]
    
    tmpRawXWindow <- tmpWindowCopy$xA
    tmpRawYWindow <- tmpWindowCopy$yA
    tmpRawZWindow <- tmpWindowCopy$zA
    tmpRawAWindow <- tmpWindowCopy$xG
    tmpRawBWindow <- tmpWindowCopy$yG
    tmpRawCWindow <- tmpWindowCopy$zG
    tmpRawAlphaWindow <- tmpWindowCopy$alpha
    tmpRawBetaWindow <- tmpWindowCopy$beta
    tmpRawGammaWindow <- tmpWindowCopy$gamma
    tmpRawMagnAWindow <- tmpWindowCopy$MagnA
    tmpRawMagnGWindow <- tmpWindowCopy$MagnG
    tmpRawMagnOWindow <- tmpWindowCopy$MagnO
    tmpRawSqSumAWindow <- tmpWindowCopy$SqSumA
    tmpRawSqSumGWindow <- tmpWindowCopy$SqSumG
    tmpRawSqSumOWindow <- tmpWindowCopy$SqSumO
    
    
    if(i == 1) {
      fsd$PreXminA[wIndex] <- 0
      fsd$PreYminA[wIndex] <- 0
      fsd$PreZminA[wIndex] <- 0
      fsd$PreXminG[wIndex] <- 0
      fsd$PreYminG[wIndex] <- 0
      fsd$PreZminG[wIndex] <- 0
      fsd$PreXminO[wIndex] <- 0
      fsd$PreYminO[wIndex] <- 0
      fsd$PreZminO[wIndex] <- 0
      
      fsd$PreXmaxA[wIndex] <- 0
      fsd$PreYmaxA[wIndex] <- 0
      fsd$PreZmaxA[wIndex] <- 0
      fsd$PreXmaxG[wIndex] <- 0
      fsd$PreYmaxG[wIndex] <- 0
      fsd$PreZmaxG[wIndex] <- 0
      fsd$PreXmaxO[wIndex] <- 0
      fsd$PreYmaxO[wIndex] <- 0
      fsd$PreZmaxO[wIndex] <- 0
      
      fsd$PreXdeltaMinA[wIndex] <- 0
      fsd$PreYdeltaMinA[wIndex] <- 0
      fsd$PreZdeltaMinA[wIndex] <- 0
      fsd$PreXdeltaMinG[wIndex] <- 0
      fsd$PreZdeltaMinG[wIndex] <- 0
      fsd$PreYdeltaMinG[wIndex] <- 0
      fsd$PreXdeltaMinO[wIndex] <- 0
      fsd$PreYdeltaMinO[wIndex] <- 0
      fsd$PreZdeltaMinO[wIndex] <- 0
      
      fsd$PreXdeltaMaxA[wIndex] <- 0
      fsd$PreYdeltaMaxA[wIndex] <- 0
      fsd$PreZdeltaMaxA[wIndex] <- 0
      fsd$PreXdeltaMaxG[wIndex] <- 0
      fsd$PreYdeltaMaxG[wIndex] <- 0
      fsd$PreZdeltaMaxG[wIndex] <- 0
      fsd$PreXdeltaMaxO[wIndex] <- 0
      fsd$PreYdeltaMaxO[wIndex] <- 0
      fsd$PreZdeltaMaxO[wIndex] <- 0
    }
    else {
      tmpPreWindow <- rawdata[rawdata$id < (tmpWindowCopy$id[1]) & rawdata$id >= (tmpWindowCopy$id[1]-9),]
      
      # some stats of the 9 entries before the actual window
      fsd$PreXminA[wIndex] <- min(tmpPreWindow$xA)
      fsd$PreYminA[wIndex] <- min(tmpPreWindow$yA)
      fsd$PreZminA[wIndex] <- min(tmpPreWindow$zA)
      fsd$PreXminG[wIndex] <- min(tmpPreWindow$xG)
      fsd$PreYminG[wIndex] <- min(tmpPreWindow$yG)
      fsd$PreZminG[wIndex] <- min(tmpPreWindow$zG)
      fsd$PreXminO[wIndex] <- min(tmpPreWindow$alpha)
      fsd$PreYminO[wIndex] <- min(tmpPreWindow$beta)
      fsd$PreZminO[wIndex] <- min(tmpPreWindow$gamma)
      
      fsd$PreXmaxA[wIndex] <- max(tmpPreWindow$xA)
      fsd$PreYmaxA[wIndex] <- max(tmpPreWindow$yA)
      fsd$PreZmaxA[wIndex] <- max(tmpPreWindow$zA)
      fsd$PreXmaxG[wIndex] <- max(tmpPreWindow$xG)
      fsd$PreYmaxG[wIndex] <- max(tmpPreWindow$yG)
      fsd$PreZmaxG[wIndex] <- max(tmpPreWindow$zG)
      fsd$PreXmaxO[wIndex] <- max(tmpPreWindow$alpha)
      fsd$PreYmaxO[wIndex] <- max(tmpPreWindow$beta)
      fsd$PreZmaxO[wIndex] <- max(tmpPreWindow$gamma)
      
      fsd$PreXdeltaMinA[wIndex] <- min(tmpPreWindow$XdeltaA)
      fsd$PreYdeltaMinA[wIndex] <- min(tmpPreWindow$YdeltaA)
      fsd$PreZdeltaMinA[wIndex] <- min(tmpPreWindow$ZdeltaA)
      fsd$PreXdeltaMinG[wIndex] <- min(tmpPreWindow$XdeltaG)
      fsd$PreYdeltaMinG[wIndex] <- min(tmpPreWindow$XdeltaG)
      fsd$PreZdeltaMinG[wIndex] <- min(tmpPreWindow$ZdeltaG)
      fsd$PreXdeltaMinO[wIndex] <- min(tmpPreWindow$XdeltaO)
      fsd$PreYdeltaMinO[wIndex] <- min(tmpPreWindow$YdeltaO)
      fsd$PreZdeltaMinO[wIndex] <- min(tmpPreWindow$ZdeltaO)
      
      fsd$PreXdeltaMaxA[wIndex] <- max(tmpPreWindow$XdeltaA)
      fsd$PreYdeltaMaxA[wIndex] <- max(tmpPreWindow$YdeltaA)
      fsd$PreZdeltaMaxA[wIndex] <- max(tmpPreWindow$ZdeltaA)
      fsd$PreXdeltaMaxG[wIndex] <- max(tmpPreWindow$XdeltaG)
      fsd$PreYdeltaMaxG[wIndex] <- max(tmpPreWindow$XdeltaG)
      fsd$PreZdeltaMaxG[wIndex] <- max(tmpPreWindow$ZdeltaG)
      fsd$PreXdeltaMaxO[wIndex] <- max(tmpPreWindow$XdeltaO)
      fsd$PreYdeltaMaxO[wIndex] <- max(tmpPreWindow$YdeltaO)
      fsd$PreZdeltaMaxO[wIndex] <- max(tmpPreWindow$ZdeltaO)
      
      
      #sd of SquareSUm of pre window
      fsd$PreSqSumSdA[wIndex] <- sd(tmpPreWindow$xA^2 + tmpPreWindow$yA^2 + tmpPreWindow$zA^2)
      fsd$PreSqSumSdG[wIndex] <- sd(tmpPreWindow$xG^2 + tmpPreWindow$yG^2 + tmpPreWindow$zG^2)
      fsd$PreSqSumSdO[wIndex] <- sd(tmpPreWindow$alpha^2 + tmpPreWindow$beta^2 + tmpPreWindow$gamma^2)
    }
    
    # number of samples in window
    fsd$WindowSize[wIndex] <- wSize
    
    
    # Min Accelerometer
    fsd$XminA[wIndex] <- min(tmpRawXWindow)
    fsd$YminA[wIndex] <- min(tmpRawYWindow)
    fsd$ZminA[wIndex] <- min(tmpRawZWindow)
    fsd$XminG[wIndex] <- min(tmpRawAWindow)
    fsd$YminG[wIndex] <- min(tmpRawBWindow)
    fsd$ZminG[wIndex] <- min(tmpRawCWindow)
    fsd$XminO[wIndex] <- min(tmpRawAlphaWindow)
    fsd$YminO[wIndex] <- min(tmpRawBetaWindow)
    fsd$ZminO[wIndex] <- min(tmpRawGammaWindow)
    
    #min magnitude
    fsd$MminA[wIndex] <- min(tmpRawMagnAWindow)
    fsd$MminG[wIndex] <- min(tmpRawMagnGWindow)
    fsd$MminO[wIndex] <- min(tmpRawMagnOWindow)
    
    # Max
    fsd$XmaxA[wIndex] <- max(tmpRawXWindow)
    fsd$YmaxA[wIndex] <- max(tmpRawYWindow)
    fsd$ZmaxA[wIndex] <- max(tmpRawZWindow)
    fsd$XmaxG[wIndex] <- max(tmpRawAWindow)
    fsd$YmaxG[wIndex] <- max(tmpRawBWindow)
    fsd$ZmaxG[wIndex] <- max(tmpRawCWindow)
    fsd$XmaxO[wIndex] <- max(tmpRawAlphaWindow)
    fsd$YmaxO[wIndex] <- max(tmpRawBetaWindow)
    fsd$ZmaxO[wIndex] <- max(tmpRawGammaWindow)
    
    #max magnitude
    fsd$MmaxA[wIndex] <- max(tmpRawMagnAWindow)
    fsd$MmaxG[wIndex] <- max(tmpRawMagnGWindow)
    fsd$MmaxO[wIndex] <- max(tmpRawMagnOWindow)
    
    # Mean
    fsd$XmeanA[wIndex] <- mean(tmpRawXWindow)
    fsd$YmeanA[wIndex] <- mean(tmpRawYWindow)
    fsd$ZmeanA[wIndex] <- mean(tmpRawZWindow)
    fsd$XmeanG[wIndex] <- mean(tmpRawAWindow)
    fsd$YmeanG[wIndex] <- mean(tmpRawBWindow)
    fsd$ZmeanG[wIndex] <- mean(tmpRawCWindow)
    fsd$XmeanO[wIndex] <- mean(tmpRawAlphaWindow)
    fsd$YmeanO[wIndex] <- mean(tmpRawBetaWindow)
    fsd$ZmeanO[wIndex] <- mean(tmpRawGammaWindow)
    
    # mean magn
    fsd$MmeanA[wIndex] <- mean(tmpRawMagnAWindow)
    fsd$MmeanG[wIndex] <- mean(tmpRawMagnGWindow)
    fsd$MmeanO[wIndex] <- mean(tmpRawMagnOWindow)
    
    # Median
    fsd$XmedianA[wIndex] <- median(tmpRawXWindow)
    fsd$YmedianA[wIndex] <- median(tmpRawYWindow)
    fsd$ZmedianA[wIndex] <- median(tmpRawZWindow)
    fsd$XmedianG[wIndex] <- median(tmpRawAWindow)
    fsd$YmedianG[wIndex] <- median(tmpRawBWindow)
    fsd$ZmedianG[wIndex] <- median(tmpRawCWindow)
    fsd$XmedianO[wIndex] <- median(tmpRawAlphaWindow)
    fsd$YmedianO[wIndex] <- median(tmpRawBetaWindow)
    fsd$ZmedianO[wIndex] <- median(tmpRawGammaWindow)
    
    # median magn
    fsd$MmedianA[wIndex] <- median(tmpRawMagnAWindow)
    fsd$MmedianG[wIndex] <- median(tmpRawMagnGWindow)
    fsd$MmedianO[wIndex] <- median(tmpRawMagnOWindow)
    
    # Standard Deviation
    fsd$XsdA[wIndex] <- sd(tmpRawXWindow)
    fsd$YsdA[wIndex] <- sd(tmpRawYWindow)
    fsd$ZsdA[wIndex] <- sd(tmpRawZWindow)
    fsd$XsdG[wIndex] <- sd(tmpRawAWindow)
    fsd$YsdG[wIndex] <- sd(tmpRawBWindow)
    fsd$ZsdG[wIndex] <- sd(tmpRawCWindow)
    fsd$XsdO[wIndex] <- sd(tmpRawAlphaWindow)
    fsd$YsdO[wIndex] <- sd(tmpRawBetaWindow)
    fsd$ZsdO[wIndex] <- sd(tmpRawGammaWindow)
    
    #sd magn
    fsd$MsdA[wIndex] <- sd(tmpRawMagnAWindow)
    fsd$MsdG[wIndex] <- sd(tmpRawMagnGWindow)
    fsd$MsdO[wIndex] <- sd(tmpRawMagnOWindow)
    
    # Variance
    fsd$XvarA[wIndex] <- var(tmpRawXWindow)
    fsd$YvarA[wIndex] <- var(tmpRawYWindow)
    fsd$ZvarA[wIndex] <- var(tmpRawZWindow)
    fsd$XvarG[wIndex] <- var(tmpRawAWindow)
    fsd$YvarG[wIndex] <- var(tmpRawBWindow)
    fsd$ZvarG[wIndex] <- var(tmpRawCWindow)
    fsd$XvarO[wIndex] <- var(tmpRawAlphaWindow)
    fsd$YvarO[wIndex] <- var(tmpRawBetaWindow)
    fsd$ZvarO[wIndex] <- var(tmpRawGammaWindow)
    
    #var magn
    fsd$MvarA[wIndex] <- var(tmpRawMagnAWindow)
    fsd$MvarG[wIndex] <- var(tmpRawMagnGWindow)
    fsd$MvarO[wIndex] <- var(tmpRawMagnOWindow)
    
    
    # RMS  Accelerometer
    fsd$XrmsA[wIndex] <- sqrt(sum((tmpRawXWindow) ^ 2) / wSize)
    fsd$YrmsA[wIndex] <- sqrt(sum((tmpRawYWindow) ^ 2) / wSize)
    fsd$ZrmsA[wIndex] <- sqrt(sum((tmpRawZWindow) ^ 2) / wSize)
    fsd$XrmsG[wIndex] <- sqrt(sum((tmpRawAWindow) ^ 2) / wSize)
    fsd$YrmsG[wIndex] <- sqrt(sum((tmpRawBWindow) ^ 2) / wSize)
    fsd$ZrmsG[wIndex] <- sqrt(sum((tmpRawCWindow) ^ 2) / wSize)
    fsd$XrmsO[wIndex] <- sqrt(sum((tmpRawAlphaWindow) ^ 2) / wSize)
    fsd$YrmsO[wIndex] <- sqrt(sum((tmpRawBetaWindow) ^ 2) / wSize)
    fsd$ZrmsO[wIndex] <- sqrt(sum((tmpRawGammaWindow) ^ 2) / wSize)
    
    # Root Mean Square of the magnitude
    fsd$MagnRmsA[wIndex] <- sqrt((sum((tmpRawMagnAWindow) ^ 2)) / wSize)
    fsd$MagnRmsG[wIndex] <- sqrt((sum((tmpRawMagnGWindow) ^ 2)) / wSize)
    fsd$MagnRmsO[wIndex] <- sqrt((sum((tmpRawMagnOWindow) ^ 2)) / wSize)
    
    # Root Mean Square of the square sum of
    fsd$SqSumRmsA[wIndex] <- sqrt((sum((tmpRawSqSumAWindow) ^ 2)) / wSize)
    fsd$SqSumRmsG[wIndex] <- sqrt((sum((tmpRawSqSumGWindow) ^ 2)) / wSize)
    fsd$SqSumRmsO[wIndex] <- sqrt((sum((tmpRawSqSumOWindow) ^ 2)) / wSize)
    
    #sd of SquareSUm
    fsd$SqSumSdA[wIndex] <- sd(tmpRawSqSumAWindow)
    fsd$SqSumSdG[wIndex] <- sd(tmpRawSqSumGWindow)
    fsd$SqSumSdO[wIndex] <- sd(tmpRawSqSumOWindow)
    
    fsd$IsKeyProb[wIndex] <- length(tmpWindowCopy$belongsToKey[tmpWindowCopy$belongsToKey == TRUE]) / wSize
    
    # time of window
    fsd$TotalTime[wIndex] <- rawdata$Timestamp[i + wJumper - 1] - rawdata$Timestamp[i - wJumper - 1]
    
    wIndex <- wIndex + 1
    # window loop var
  }
  i <- i + wJumper
}

# Skewness 
fsd$XskewA <- 3 * (fsd$XmeanA - fsd$XmedianA) / fsd$XsdA
fsd$YskewA <- 3 * (fsd$YmeanA - fsd$YmedianA) / fsd$YsdA
fsd$ZskewA <- 3 * (fsd$ZmeanA - fsd$ZmedianA) / fsd$ZsdA
fsd$XskewG <- 3 * (fsd$XmeanG - fsd$XmedianG) / fsd$XsdG
fsd$YskewG <- 3 * (fsd$YmeanG - fsd$YmedianG) / fsd$YsdG
fsd$ZskewG <- 3 * (fsd$ZmeanG - fsd$ZmedianG) / fsd$ZsdG
fsd$XskewO <- 3 * (fsd$XmeanO - fsd$XmedianO) / fsd$XsdO
fsd$YskewO <- 3 * (fsd$YmeanO - fsd$YmedianO) / fsd$YsdO
fsd$ZskewO <- 3 * (fsd$ZmeanO - fsd$ZmedianO) / fsd$ZsdO
# skewness magn
fsd$MskewA <- 3 * (fsd$MmeanA - fsd$MmedianA) / fsd$MsdA
fsd$MskewG <- 3 * (fsd$MmeanG - fsd$MmedianG) / fsd$MsdG
fsd$MskewO <- 3 * (fsd$MmeanO - fsd$MmedianO) / fsd$MsdO

write.csv(
  fsd,
  "C:\\git\\data-thesis\\R\\datasets\\17011417-dataset-test-wsize-median-key-only.csv",
  row.names = FALSE
)