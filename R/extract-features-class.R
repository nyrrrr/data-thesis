set.seed(123)
options(digits=20)

feature.extraction <- function(boolTraining, stringFileTimestamp, boolLabeled) {
  start.time <- proc.time()
  
  if(boolTraining) print(paste("Generate features from ", stringFileTimestamp, "-sensor-dataset-training-raw.csv", sep=""))
  else print(paste("Generate features from ","transferred-", stringFileTimestamp, "-victim-data",", labeled=", boolLabeled, sep=""))

  
  if(boolTraining) {
    sensortrain <-
      read.csv(
        paste(getwd(),"/datasets/", stringFileTimestamp, "-sensor-dataset-training-raw.csv", sep=""), 
        header = TRUE,
        stringsAsFactors=FALSE
      )
    
    keytrain <-
      read.csv(
        paste(getwd(),"/datasets/", stringFileTimestamp, "-key-dataset-training-raw.csv", sep=""),
        header = TRUE,
        stringsAsFactors=FALSE
      )
  } else {
    sensortrain <-
      read.csv(
        paste(getwd(),"/datasets/", "transferred-", stringFileTimestamp, "-victim-data.csv", sep=""), # 17011417, 17011205, 17011020, 16122802
        header = TRUE,
        stringsAsFactors=FALSE
      )
    
    keytrain <-
      read.csv(
        paste(getwd(),"/datasets/", stringFileTimestamp, "-key-dataset-test-raw.csv", sep=""),
        header = TRUE,
        stringsAsFactors=FALSE
      )
  }
  sensortrain$belongsToKey <- FALSE;
  for (i in seq_along(keytrain$DownTime)) {
    keytrain$id <- seq_along(keytrain$DownTime)
    keytrain$WindowSize[i] <- length(sensortrain$Timestamp[sensortrain$Timestamp >= keytrain$DownTime[i] & sensortrain$Timestamp <= keytrain$EventTime[i]])
    for (j in sensortrain$Timestamp[sensortrain$Timestamp >= (keytrain$DownTime[i]) &
                                    sensortrain$Timestamp <= (keytrain$EventTime[i])]) {
      sensortrain$belongsToKey[sensortrain$Timestamp == j] <- TRUE
    }
  }
  
  # write.csv(
  #   keytrain,
  #   "C:\\git\\data-thesis\\R\\datasets\\17011020-key-dataset-training-raw.csv",
  #   row.names = FALSE
  # )
  
  # --- preprocessing
  
  sensortrain$id <- seq_along(sensortrain$Timestamp)
  
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
  sensortrain$alpha <- NULL
  sensortrain$beta <- NULL
  sensortrain$gamma <- NULL
  
  # Square Sum of 3D vectors
  sensortrain$SqSumA <- sensortrain$xA ^ 2 + sensortrain$yA ^ 2 + sensortrain$zA ^ 2
  sensortrain$SqSumG <- sensortrain$xG^2 + sensortrain$yG^2 + sensortrain$zG^2
  
  # Magnitude of 3D Vectors
  sensortrain$MagnA <- sqrt(sensortrain$SqSumA)
  sensortrain$MagnG <- sqrt(sensortrain$SqSumG)
  
  # norm vector values
  sensortrain$XnormMeanA <- sensortrain$xA - mean(sensortrain$xA)
  sensortrain$YnormMeanA <- sensortrain$yA - mean(sensortrain$yA)
  sensortrain$ZnormMeanA <- sensortrain$zA - mean(sensortrain$zA)
  sensortrain$XnormMeanG <- sensortrain$xG - mean(sensortrain$xG)
  sensortrain$YnormMeanG <- sensortrain$yG - mean(sensortrain$yG)
  sensortrain$ZnormMeanG <- sensortrain$zG - mean(sensortrain$zG)
  
  sensortrain$MnormMeanA <- sensortrain$MagnA - mean(sensortrain$MagnA)
  sensortrain$MnormMeanG <- sensortrain$MagnG - mean(sensortrain$MagnG)
  
  
  # write.csv(sensortrain,
  #           "C:\\git\\data-thesis\\R\\datasets\\17011417-sensor-dataset-training-preprocessed.csv",
  #           row.names = FALSE)
  
  
  # wSize <- ceiling(max(keytrain$WindowSize)) * 2 + 1
  # wSize <- 35
  wSize <- 61
  if(wSize %% 2 == 0) wSize <- wSize + 1
  
  fKeyTrain <- NULL
  
  
  # helper vars
  wIndex <- 0
  wJumper <- as.integer(wSize / 2)
  
  for (i in 1:nrow(sensortrain)) {
    if (i > 0 & i %% wJumper == 0 & i <= nrow(sensortrain) - wJumper) {
      fKeyTrain$Timestamp[wIndex] <- # timestamp
        as.character(sensortrain$Timestamp[i - wJumper - 1])
      
      tmpWindowCopy <- sensortrain[sensortrain$id >= i - wJumper - 1 &
                                 sensortrain$id < i + wJumper,]
      
      fKeyTrain$WindowSize[wIndex] <- wSize
      
      tmpRawXWindow <- tmpWindowCopy$xA
      tmpRawYWindow <- tmpWindowCopy$yA
      tmpRawZWindow <- tmpWindowCopy$zA
      tmpRawAWindow <- tmpWindowCopy$xG
      tmpRawBWindow <- tmpWindowCopy$yG
      tmpRawCWindow <- tmpWindowCopy$zG
      tmpRawMagnAWindow <- tmpWindowCopy$MagnA
      tmpRawMagnGWindow <- tmpWindowCopy$MagnG
  
      # linear interpolation
      linInterpN <- 201
      tmpRawXWindowLinInterp <- approx(tmpWindowCopy$Timestamp, tmpRawXWindow, n=linInterpN)$y
      tmpRawYWindowLinInterp <- approx(tmpWindowCopy$Timestamp, tmpRawYWindow, n=linInterpN)$y
      tmpRawZWindowLinInterp <- approx(tmpWindowCopy$Timestamp, tmpRawZWindow, n=linInterpN)$y
      tmpRawAWindowLinInterp <- approx(tmpWindowCopy$Timestamp, tmpRawAWindow, n=linInterpN)$y
      tmpRawBWindowLinInterp <- approx(tmpWindowCopy$Timestamp, tmpRawBWindow, n=linInterpN)$y
      tmpRawCWindowLinInterp <- approx(tmpWindowCopy$Timestamp, tmpRawAWindow, n=linInterpN)$y
      tmpRawMagnAWindowLinInterp <- approx(tmpWindowCopy$Timestamp, tmpRawMagnAWindow, n=linInterpN)$y
      tmpRawMagnGWindowLinInterp <- approx(tmpWindowCopy$Timestamp, tmpRawMagnGWindow, n=linInterpN)$y
  
      # polynom (3) interpol
      polInterpN <- 3
  
      tmpRawPolyMod <- lm(tmpRawXWindow ~ poly(tmpWindowCopy$Timestamp, degree=polInterpN))
      tmpRawXWindowPolInterp <- predict(tmpRawPolyMod)
      fKeyTrain$Xd0A[wIndex] <- tmpRawPolyMod$coefficients[1]
      fKeyTrain$Xd1A[wIndex] <- tmpRawPolyMod$coefficients[2]
      fKeyTrain$Xd2A[wIndex] <- tmpRawPolyMod$coefficients[3]
      fKeyTrain$Xd3A[wIndex] <- tmpRawPolyMod$coefficients[4]
  
      tmpRawPolyMod <- lm(tmpRawYWindow ~ poly(tmpWindowCopy$Timestamp, degree=polInterpN))
      tmpRawYWindowPolInterp <- predict(tmpRawPolyMod)
      fKeyTrain$Yd0A[wIndex] <- tmpRawPolyMod$coefficients[1]
      fKeyTrain$Yd1A[wIndex] <- tmpRawPolyMod$coefficients[2]
      fKeyTrain$Yd2A[wIndex] <- tmpRawPolyMod$coefficients[3]
      fKeyTrain$Yd3A[wIndex] <- tmpRawPolyMod$coefficients[4]
  
      tmpRawPolyMod <- lm(tmpRawZWindow ~ poly(tmpWindowCopy$Timestamp, degree=polInterpN))
      tmpRawZWindowPolInterp <- predict(tmpRawPolyMod)
      fKeyTrain$Zd0A[wIndex] <- tmpRawPolyMod$coefficients[1]
      fKeyTrain$Zd1A[wIndex] <- tmpRawPolyMod$coefficients[2]
      fKeyTrain$Zd2A[wIndex] <- tmpRawPolyMod$coefficients[3]
      fKeyTrain$Zd3A[wIndex] <- tmpRawPolyMod$coefficients[4]
  
      tmpRawPolyMod <- lm(tmpRawAWindow ~ poly(tmpWindowCopy$Timestamp, degree=polInterpN))
      tmpRawAWindowPolInterp <- predict(tmpRawPolyMod)
      fKeyTrain$Xd0G[wIndex] <- tmpRawPolyMod$coefficients[1]
      fKeyTrain$Xd1G[wIndex] <- tmpRawPolyMod$coefficients[2]
      fKeyTrain$Xd2G[wIndex] <- tmpRawPolyMod$coefficients[3]
      fKeyTrain$Xd3G[wIndex] <- tmpRawPolyMod$coefficients[4]
  
      tmpRawPolyMod <- lm(tmpRawBWindow ~ poly(tmpWindowCopy$Timestamp, degree=polInterpN))
      tmpRawBWindowPolInterp <- predict(tmpRawPolyMod)
      fKeyTrain$Yd0G[wIndex] <- tmpRawPolyMod$coefficients[1]
      fKeyTrain$Yd1G[wIndex] <- tmpRawPolyMod$coefficients[2]
      fKeyTrain$Yd2G[wIndex] <- tmpRawPolyMod$coefficients[3]
      fKeyTrain$Yd3G[wIndex] <- tmpRawPolyMod$coefficients[4]
  
      tmpRawPolyMod <- lm(tmpRawCWindow ~ poly(tmpWindowCopy$Timestamp, degree=polInterpN))
      tmpRawCWindowPolInterp <- predict(tmpRawPolyMod)
      fKeyTrain$Zd0G[wIndex] <- tmpRawPolyMod$coefficients[1]
      fKeyTrain$Zd1G[wIndex] <- tmpRawPolyMod$coefficients[2]
      fKeyTrain$Zd2G[wIndex] <- tmpRawPolyMod$coefficients[3]
      fKeyTrain$Zd3G[wIndex] <- tmpRawPolyMod$coefficients[4]
      
      tmpRawPolyMod <- lm(tmpRawMagnAWindow ~ poly(tmpWindowCopy$Timestamp, degree=polInterpN))
      tmpRawMagnAWindowPolInterp <- predict(tmpRawPolyMod)
      fKeyTrain$Md0A[wIndex] <- tmpRawPolyMod$coefficients[1]
      fKeyTrain$Md1A[wIndex] <- tmpRawPolyMod$coefficients[2]
      fKeyTrain$Md2A[wIndex] <- tmpRawPolyMod$coefficients[3]
      fKeyTrain$Md3A[wIndex] <- tmpRawPolyMod$coefficients[4]
      
      tmpRawPolyMod <- lm(tmpRawMagnGWindow ~ poly(tmpWindowCopy$Timestamp, degree=polInterpN))
      tmpRawMagnGWindowPolInterp <- predict(tmpRawPolyMod)
      fKeyTrain$Md0G[wIndex] <- tmpRawPolyMod$coefficients[1]
      fKeyTrain$Md1G[wIndex] <- tmpRawPolyMod$coefficients[2]
      fKeyTrain$Md2G[wIndex] <- tmpRawPolyMod$coefficients[3]
      fKeyTrain$Md3G[wIndex] <- tmpRawPolyMod$coefficients[4]
      
      # cubic spline interpol
      cubicInterpN <- 201
      spar <- 0.35
      tmpRawXWindowCubInterp <- predict(smooth.spline(tmpWindowCopy$Timestamp, tmpRawXWindow, spar=spar), seq(tmpWindowCopy$Timestamp[1], tmpWindowCopy$Timestamp[length(tmpWindowCopy$Timestamp)], length.out=cubicInterpN))$y
      tmpRawYWindowCubInterp <- predict(smooth.spline(tmpWindowCopy$Timestamp, tmpRawYWindow, spar=spar), seq(tmpWindowCopy$Timestamp[1], tmpWindowCopy$Timestamp[length(tmpWindowCopy$Timestamp)], length.out=cubicInterpN))$y
      tmpRawZWindowCubInterp <- predict(smooth.spline(tmpWindowCopy$Timestamp, tmpRawZWindow, spar=spar), seq(tmpWindowCopy$Timestamp[1], tmpWindowCopy$Timestamp[length(tmpWindowCopy$Timestamp)], length.out=cubicInterpN))$y
      tmpRawAWindowCubInterp <- predict(smooth.spline(tmpWindowCopy$Timestamp, tmpRawAWindow, spar=spar), seq(tmpWindowCopy$Timestamp[1], tmpWindowCopy$Timestamp[length(tmpWindowCopy$Timestamp)], length.out=cubicInterpN))$y
      tmpRawBWindowCubInterp <- predict(smooth.spline(tmpWindowCopy$Timestamp, tmpRawBWindow, spar=spar), seq(tmpWindowCopy$Timestamp[1], tmpWindowCopy$Timestamp[length(tmpWindowCopy$Timestamp)], length.out=cubicInterpN))$y
      tmpRawCWindowCubInterp <- predict(smooth.spline(tmpWindowCopy$Timestamp, tmpRawCWindow, spar=spar), seq(tmpWindowCopy$Timestamp[1], tmpWindowCopy$Timestamp[length(tmpWindowCopy$Timestamp)], length.out=cubicInterpN))$y
      tmpRawMagnAWindowCubInterp <- predict(smooth.spline(tmpWindowCopy$Timestamp, tmpRawMagnAWindow, spar=spar), seq(tmpWindowCopy$Timestamp[1], tmpWindowCopy$Timestamp[length(tmpWindowCopy$Timestamp)], length.out=cubicInterpN))$y
      tmpRawMagnGWindowCubInterp <- predict(smooth.spline(tmpWindowCopy$Timestamp, tmpRawMagnGWindow, spar=spar), seq(tmpWindowCopy$Timestamp[1], tmpWindowCopy$Timestamp[length(tmpWindowCopy$Timestamp)], length.out=cubicInterpN))$y
  
      # mean norm
      tmpRawXWindowMeanNorm <- tmpWindowCopy$XnormMeanA
      tmpRawYWindowMeanNorm <- tmpWindowCopy$YnormMeanA
      tmpRawZWindowMeanNorm <- tmpWindowCopy$ZnormMeanA
      tmpRawAWindowMeanNorm <- tmpWindowCopy$XnormMeanG
      tmpRawBWindowMeanNorm <- tmpWindowCopy$YnormMeanG
      tmpRawCWindowMeanNorm <- tmpWindowCopy$ZnormMeanG
      tmpRawMagnAWindowMeanNorm <- tmpWindowCopy$MnormMeanA
      tmpRawMagnGWindowMeanNorm <- tmpWindowCopy$MnormMeanG
  
      # Min Accelerometer
      fKeyTrain$XminA[wIndex] <- min(tmpRawXWindow)
      fKeyTrain$YminA[wIndex] <- min(tmpRawYWindow)
      fKeyTrain$ZminA[wIndex] <- min(tmpRawZWindow)
      fKeyTrain$XminG[wIndex] <- min(tmpRawAWindow)
      fKeyTrain$YminG[wIndex] <- min(tmpRawBWindow)
      fKeyTrain$ZminG[wIndex] <- min(tmpRawCWindow)
  
      fKeyTrain$XminLinInterpA[wIndex] <- min(tmpRawXWindowLinInterp)
      fKeyTrain$YminLinInterpA[wIndex] <- min(tmpRawYWindowLinInterp)
      fKeyTrain$ZminLinInterpA[wIndex] <- min(tmpRawZWindowLinInterp)
      fKeyTrain$XminLinInterpG[wIndex] <- min(tmpRawAWindowLinInterp)
      fKeyTrain$YminLinInterpG[wIndex] <- min(tmpRawBWindowLinInterp)
      fKeyTrain$ZminLinInterpG[wIndex] <- min(tmpRawCWindowLinInterp)
  
      fKeyTrain$XminPolInterpA[wIndex] <- min(tmpRawXWindowPolInterp)
      fKeyTrain$YminPolInterpA[wIndex] <- min(tmpRawYWindowPolInterp)
      fKeyTrain$ZminPolInterpA[wIndex] <- min(tmpRawZWindowPolInterp)
      fKeyTrain$XminPolInterpG[wIndex] <- min(tmpRawAWindowPolInterp)
      fKeyTrain$YminPolInterpG[wIndex] <- min(tmpRawBWindowPolInterp)
      fKeyTrain$ZminPolInterpG[wIndex] <- min(tmpRawCWindowPolInterp)
  
      fKeyTrain$XminCubInterpA[wIndex] <- min(tmpRawXWindowCubInterp)
      fKeyTrain$YminCubInterpA[wIndex] <- min(tmpRawYWindowCubInterp)
      fKeyTrain$ZminCubInterpA[wIndex] <- min(tmpRawZWindowCubInterp)
      fKeyTrain$XminCubInterpG[wIndex] <- min(tmpRawAWindowCubInterp)
      fKeyTrain$YminCubInterpG[wIndex] <- min(tmpRawBWindowCubInterp)
      fKeyTrain$ZminCubInterpG[wIndex] <- min(tmpRawCWindowCubInterp)
  
      fKeyTrain$XminMeanNormA[wIndex] <- min(tmpRawXWindowMeanNorm)
      fKeyTrain$YminMeanNormA[wIndex] <- min(tmpRawYWindowMeanNorm)
      fKeyTrain$ZminMeanNormA[wIndex] <- min(tmpRawZWindowMeanNorm)
      fKeyTrain$XminMeanNormG[wIndex] <- min(tmpRawAWindowMeanNorm)
      fKeyTrain$YminMeanNormG[wIndex] <- min(tmpRawBWindowMeanNorm)
      fKeyTrain$ZminMeanNormG[wIndex] <- min(tmpRawCWindowMeanNorm)
  
      #min magnitude
      fKeyTrain$MminA[wIndex] <- min(tmpRawMagnAWindow)
      fKeyTrain$MminG[wIndex] <- min(tmpRawMagnGWindow)
  
      fKeyTrain$MminLinInterpA[wIndex] <- min(tmpRawMagnAWindowLinInterp)
      fKeyTrain$MminLinInterpG[wIndex] <- min(tmpRawMagnGWindowLinInterp)
  
      fKeyTrain$MminPolInterpA[wIndex] <- min(tmpRawMagnAWindowPolInterp)
      fKeyTrain$MminPolInterpG[wIndex] <- min(tmpRawMagnGWindowPolInterp)
  
      fKeyTrain$MminCubInterpA[wIndex] <- min(tmpRawMagnAWindowCubInterp)
      fKeyTrain$MminCubInterpG[wIndex] <- min(tmpRawMagnGWindowCubInterp)
  
      fKeyTrain$MminMeanNormA[wIndex] <- min(tmpRawMagnAWindowMeanNorm)
      fKeyTrain$MminMeanNormG[wIndex] <- min(tmpRawMagnGWindowMeanNorm)
  
      # Max
      fKeyTrain$XmaxA[wIndex] <- max(tmpRawXWindow)
      fKeyTrain$YmaxA[wIndex] <- max(tmpRawYWindow)
      fKeyTrain$ZmaxA[wIndex] <- max(tmpRawZWindow)
      fKeyTrain$XmaxG[wIndex] <- max(tmpRawAWindow)
      fKeyTrain$YmaxG[wIndex] <- max(tmpRawBWindow)
      fKeyTrain$ZmaxG[wIndex] <- max(tmpRawCWindow)
  
      fKeyTrain$XmaxLinInterpA[wIndex] <- max(tmpRawXWindowLinInterp)
      fKeyTrain$YmaxLinInterpA[wIndex] <- max(tmpRawYWindowLinInterp)
      fKeyTrain$ZmaxLinInterpA[wIndex] <- max(tmpRawZWindowLinInterp)
      fKeyTrain$XmaxLinInterpG[wIndex] <- max(tmpRawAWindowLinInterp)
      fKeyTrain$YmaxLinInterpG[wIndex] <- max(tmpRawBWindowLinInterp)
      fKeyTrain$ZmaxLinInterpG[wIndex] <- max(tmpRawCWindowLinInterp)
  
      fKeyTrain$XmaxPolInterpA[wIndex] <- max(tmpRawXWindowPolInterp)
      fKeyTrain$YmaxPolInterpA[wIndex] <- max(tmpRawYWindowPolInterp)
      fKeyTrain$ZmaxPolInterpA[wIndex] <- max(tmpRawZWindowPolInterp)
      fKeyTrain$XmaxPolInterpG[wIndex] <- max(tmpRawAWindowPolInterp)
      fKeyTrain$YmaxPolInterpG[wIndex] <- max(tmpRawBWindowPolInterp)
      fKeyTrain$ZmaxPolInterpG[wIndex] <- max(tmpRawCWindowPolInterp)
  
      fKeyTrain$XmaxCubInterpA[wIndex] <- max(tmpRawXWindowCubInterp)
      fKeyTrain$YmaxCubInterpA[wIndex] <- max(tmpRawYWindowCubInterp)
      fKeyTrain$ZmaxCubInterpA[wIndex] <- max(tmpRawZWindowCubInterp)
      fKeyTrain$XmaxCubInterpG[wIndex] <- max(tmpRawAWindowCubInterp)
      fKeyTrain$YmaxCubInterpG[wIndex] <- max(tmpRawBWindowCubInterp)
      fKeyTrain$ZmaxCubInterpG[wIndex] <- max(tmpRawCWindowCubInterp)
  
      fKeyTrain$XmaxMeanNormA[wIndex] <- max(tmpRawXWindowMeanNorm)
      fKeyTrain$YmaxMeanNormA[wIndex] <- max(tmpRawYWindowMeanNorm)
      fKeyTrain$ZmaxMeanNormA[wIndex] <- max(tmpRawZWindowMeanNorm)
      fKeyTrain$XmaxMeanNormG[wIndex] <- max(tmpRawAWindowMeanNorm)
      fKeyTrain$YmaxMeanNormG[wIndex] <- max(tmpRawBWindowMeanNorm)
      fKeyTrain$ZmaxMeanNormG[wIndex] <- max(tmpRawCWindowMeanNorm)
  
      #max magnitude
      fKeyTrain$MmaxA[wIndex] <- max(tmpRawMagnAWindow)
      fKeyTrain$MmaxG[wIndex] <- max(tmpRawMagnGWindow)
  
      fKeyTrain$MmaxLinInterpA[wIndex] <- max(tmpRawMagnAWindowLinInterp)
      fKeyTrain$MmaxLinInterpG[wIndex] <- max(tmpRawMagnGWindowLinInterp)
  
      fKeyTrain$MmaxPolInterpA[wIndex] <- max(tmpRawMagnAWindowPolInterp)
      fKeyTrain$MmaxPolInterpG[wIndex] <- max(tmpRawMagnGWindowPolInterp)
  
      fKeyTrain$MmaxCubInterpA[wIndex] <- max(tmpRawMagnAWindowCubInterp)
      fKeyTrain$MmaxCubInterpG[wIndex] <- max(tmpRawMagnGWindowCubInterp)
  
      fKeyTrain$MmaxMeanNormA[wIndex] <- max(tmpRawMagnAWindowMeanNorm)
      fKeyTrain$MmaxMeanNormG[wIndex] <- max(tmpRawMagnGWindowMeanNorm)
  
      # Mean
      fKeyTrain$XmeanA[wIndex] <- mean(tmpRawXWindow)
      fKeyTrain$YmeanA[wIndex] <- mean(tmpRawYWindow)
      fKeyTrain$ZmeanA[wIndex] <- mean(tmpRawZWindow)
      fKeyTrain$XmeanG[wIndex] <- mean(tmpRawAWindow)
      fKeyTrain$YmeanG[wIndex] <- mean(tmpRawBWindow)
      fKeyTrain$ZmeanG[wIndex] <- mean(tmpRawCWindow)
  
      fKeyTrain$XmeanLinInterpA[wIndex] <- mean(tmpRawXWindowLinInterp)
      fKeyTrain$YmeanLinInterpA[wIndex] <- mean(tmpRawYWindowLinInterp)
      fKeyTrain$ZmeanLinInterpA[wIndex] <- mean(tmpRawZWindowLinInterp)
      fKeyTrain$XmeanLinInterpG[wIndex] <- mean(tmpRawAWindowLinInterp)
      fKeyTrain$YmeanLinInterpG[wIndex] <- mean(tmpRawBWindowLinInterp)
      fKeyTrain$ZmeanLinInterpG[wIndex] <- mean(tmpRawCWindowLinInterp)
  
      fKeyTrain$XmeanPolInterpA[wIndex] <- mean(tmpRawXWindowPolInterp)
      fKeyTrain$YmeanPolInterpA[wIndex] <- mean(tmpRawYWindowPolInterp)
      fKeyTrain$ZmeanPolInterpA[wIndex] <- mean(tmpRawZWindowPolInterp)
      fKeyTrain$XmeanPolInterpG[wIndex] <- mean(tmpRawAWindowPolInterp)
      fKeyTrain$YmeanPolInterpG[wIndex] <- mean(tmpRawBWindowPolInterp)
      fKeyTrain$ZmeanPolInterpG[wIndex] <- mean(tmpRawCWindowPolInterp)
  
      fKeyTrain$XmeanCubInterpA[wIndex] <- mean(tmpRawXWindowCubInterp)
      fKeyTrain$YmeanCubInterpA[wIndex] <- mean(tmpRawYWindowCubInterp)
      fKeyTrain$ZmeanCubInterpA[wIndex] <- mean(tmpRawZWindowCubInterp)
      fKeyTrain$XmeanCubInterpG[wIndex] <- mean(tmpRawAWindowCubInterp)
      fKeyTrain$YmeanCubInterpG[wIndex] <- mean(tmpRawBWindowCubInterp)
      fKeyTrain$ZmeanCubInterpG[wIndex] <- mean(tmpRawCWindowCubInterp)
  
      fKeyTrain$XmeanMeanNormA[wIndex] <- mean(tmpRawXWindowMeanNorm)
      fKeyTrain$YmeanMeanNormA[wIndex] <- mean(tmpRawYWindowMeanNorm)
      fKeyTrain$ZmeanMeanNormA[wIndex] <- mean(tmpRawZWindowMeanNorm)
      fKeyTrain$XmeanMeanNormG[wIndex] <- mean(tmpRawAWindowMeanNorm)
      fKeyTrain$YmeanMeanNormG[wIndex] <- mean(tmpRawBWindowMeanNorm)
      fKeyTrain$ZmeanMeanNormG[wIndex] <- mean(tmpRawCWindowMeanNorm)
  
      # mean magn
      fKeyTrain$MmeanA[wIndex] <- mean(tmpRawMagnAWindow)
      fKeyTrain$MmeanG[wIndex] <- mean(tmpRawMagnGWindow)
  
      fKeyTrain$MmeanLinInterpA[wIndex] <- mean(tmpRawMagnAWindowLinInterp)
      fKeyTrain$MmeanLinInterpG[wIndex] <- mean(tmpRawMagnGWindowLinInterp)
  
      fKeyTrain$MmeanPolInterpA[wIndex] <- mean(tmpRawMagnAWindowPolInterp)
      fKeyTrain$MmeanPolInterpG[wIndex] <- mean(tmpRawMagnGWindowPolInterp)
  
      fKeyTrain$MmeanCubInterpA[wIndex] <- mean(tmpRawMagnAWindowCubInterp)
      fKeyTrain$MmeanCubInterpG[wIndex] <- mean(tmpRawMagnGWindowCubInterp)
  
      fKeyTrain$MmeanMeanNormA[wIndex] <- mean(tmpRawMagnAWindowMeanNorm)
      fKeyTrain$MmeanMeanNormG[wIndex] <- mean(tmpRawMagnGWindowMeanNorm)
  
      # Median
      fKeyTrain$XmedianA[wIndex] <- median(tmpRawXWindow)
      fKeyTrain$YmedianA[wIndex] <- median(tmpRawYWindow)
      fKeyTrain$ZmedianA[wIndex] <- median(tmpRawZWindow)
      fKeyTrain$XmedianG[wIndex] <- median(tmpRawAWindow)
      fKeyTrain$YmedianG[wIndex] <- median(tmpRawBWindow)
      fKeyTrain$ZmedianG[wIndex] <- median(tmpRawCWindow)
  
      fKeyTrain$XmedianLinInterpA[wIndex] <- median(tmpRawXWindowLinInterp)
      fKeyTrain$YmedianLinInterpA[wIndex] <- median(tmpRawYWindowLinInterp)
      fKeyTrain$ZmedianLinInterpA[wIndex] <- median(tmpRawZWindowLinInterp)
      fKeyTrain$XmedianLinInterpG[wIndex] <- median(tmpRawAWindowLinInterp)
      fKeyTrain$YmedianLinInterpG[wIndex] <- median(tmpRawBWindowLinInterp)
      fKeyTrain$ZmedianLinInterpG[wIndex] <- median(tmpRawCWindowLinInterp)
  
      fKeyTrain$XmedianPolInterpA[wIndex] <- median(tmpRawXWindowPolInterp)
      fKeyTrain$YmedianPolInterpA[wIndex] <- median(tmpRawYWindowPolInterp)
      fKeyTrain$ZmedianPolInterpA[wIndex] <- median(tmpRawZWindowPolInterp)
      fKeyTrain$XmedianPolInterpG[wIndex] <- median(tmpRawAWindowPolInterp)
      fKeyTrain$YmedianPolInterpG[wIndex] <- median(tmpRawBWindowPolInterp)
      fKeyTrain$ZmedianPolInterpG[wIndex] <- median(tmpRawCWindowPolInterp)
  
      fKeyTrain$XmedianCubInterpA[wIndex] <- median(tmpRawXWindowCubInterp)
      fKeyTrain$YmedianCubInterpA[wIndex] <- median(tmpRawYWindowCubInterp)
      fKeyTrain$ZmedianCubInterpA[wIndex] <- median(tmpRawZWindowCubInterp)
      fKeyTrain$XmedianCubInterpG[wIndex] <- median(tmpRawAWindowCubInterp)
      fKeyTrain$YmedianCubInterpG[wIndex] <- median(tmpRawBWindowCubInterp)
      fKeyTrain$ZmedianCubInterpG[wIndex] <- median(tmpRawCWindowCubInterp)
  
      fKeyTrain$XmedianMeanNormA[wIndex] <- median(tmpRawXWindowMeanNorm)
      fKeyTrain$YmedianMeanNormA[wIndex] <- median(tmpRawYWindowMeanNorm)
      fKeyTrain$ZmedianMeanNormA[wIndex] <- median(tmpRawZWindowMeanNorm)
      fKeyTrain$XmedianMeanNormG[wIndex] <- median(tmpRawAWindowMeanNorm)
      fKeyTrain$YmedianMeanNormG[wIndex] <- median(tmpRawBWindowMeanNorm)
      fKeyTrain$ZmedianMeanNormG[wIndex] <- median(tmpRawCWindowMeanNorm)
  
      # median magn
      fKeyTrain$MmedianA[wIndex] <- median(tmpRawMagnAWindow)
      fKeyTrain$MmedianG[wIndex] <- median(tmpRawMagnGWindow)
  
      fKeyTrain$MmedianLinInterpA[wIndex] <- median(tmpRawMagnAWindowLinInterp)
      fKeyTrain$MmedianLinInterpG[wIndex] <- median(tmpRawMagnGWindowLinInterp)
  
      fKeyTrain$MmedianPolInterpA[wIndex] <- median(tmpRawMagnAWindowPolInterp)
      fKeyTrain$MmedianPolInterpG[wIndex] <- median(tmpRawMagnGWindowPolInterp)
  
      fKeyTrain$MmedianCubInterpA[wIndex] <- median(tmpRawMagnAWindowCubInterp)
      fKeyTrain$MmedianCubInterpG[wIndex] <- median(tmpRawMagnGWindowCubInterp)
  
      fKeyTrain$MmedianMeanNormA[wIndex] <- median(tmpRawMagnAWindowMeanNorm)
      fKeyTrain$MmedianMeanNormG[wIndex] <- median(tmpRawMagnGWindowMeanNorm)
  
  
      # Standard Deviation
      fKeyTrain$XsdA[wIndex] <- sd(tmpRawXWindow)
      fKeyTrain$YsdA[wIndex] <- sd(tmpRawYWindow)
      fKeyTrain$ZsdA[wIndex] <- sd(tmpRawZWindow)
      fKeyTrain$XsdG[wIndex] <- sd(tmpRawAWindow)
      fKeyTrain$YsdG[wIndex] <- sd(tmpRawBWindow)
      fKeyTrain$ZsdG[wIndex] <- sd(tmpRawCWindow)
  
      fKeyTrain$XsdLinInterpA[wIndex] <- sd(tmpRawXWindowLinInterp)
      fKeyTrain$YsdLinInterpA[wIndex] <- sd(tmpRawYWindowLinInterp)
      fKeyTrain$ZsdLinInterpA[wIndex] <- sd(tmpRawZWindowLinInterp)
      fKeyTrain$XsdLinInterpG[wIndex] <- sd(tmpRawAWindowLinInterp)
      fKeyTrain$YsdLinInterpG[wIndex] <- sd(tmpRawBWindowLinInterp)
      fKeyTrain$ZsdLinInterpG[wIndex] <- sd(tmpRawCWindowLinInterp)
  
      fKeyTrain$XsdPolInterpA[wIndex] <- sd(tmpRawXWindowPolInterp)
      fKeyTrain$YsdPolInterpA[wIndex] <- sd(tmpRawYWindowPolInterp)
      fKeyTrain$ZsdPolInterpA[wIndex] <- sd(tmpRawZWindowPolInterp)
      fKeyTrain$XsdPolInterpG[wIndex] <- sd(tmpRawAWindowPolInterp)
      fKeyTrain$YsdPolInterpG[wIndex] <- sd(tmpRawBWindowPolInterp)
      fKeyTrain$ZsdPolInterpG[wIndex] <- sd(tmpRawCWindowPolInterp)
  
      fKeyTrain$XsdCubInterpA[wIndex] <- sd(tmpRawXWindowCubInterp)
      fKeyTrain$YsdCubInterpA[wIndex] <- sd(tmpRawYWindowCubInterp)
      fKeyTrain$ZsdCubInterpA[wIndex] <- sd(tmpRawZWindowCubInterp)
      fKeyTrain$XsdCubInterpG[wIndex] <- sd(tmpRawAWindowCubInterp)
      fKeyTrain$YsdCubInterpG[wIndex] <- sd(tmpRawBWindowCubInterp)
      fKeyTrain$ZsdCubInterpG[wIndex] <- sd(tmpRawCWindowCubInterp)
  
      fKeyTrain$XsdMeanNormA[wIndex] <- sd(tmpRawXWindowMeanNorm)
      fKeyTrain$YsdMeanNormA[wIndex] <- sd(tmpRawYWindowMeanNorm)
      fKeyTrain$ZsdMeanNormA[wIndex] <- sd(tmpRawZWindowMeanNorm)
      fKeyTrain$XsdMeanNormG[wIndex] <- sd(tmpRawAWindowMeanNorm)
      fKeyTrain$YsdMeanNormG[wIndex] <- sd(tmpRawBWindowMeanNorm)
      fKeyTrain$ZsdMeanNormG[wIndex] <- sd(tmpRawCWindowMeanNorm)
  
      #sd magn
      fKeyTrain$MsdA[wIndex] <- sd(tmpRawMagnAWindow)
      fKeyTrain$MsdG[wIndex] <- sd(tmpRawMagnGWindow)
  
      fKeyTrain$MsdLinInterpA[wIndex] <- sd(tmpRawMagnAWindowLinInterp)
      fKeyTrain$MsdLinInterpG[wIndex] <- sd(tmpRawMagnGWindowLinInterp)
  
      fKeyTrain$MsdPolInterpA[wIndex] <- sd(tmpRawMagnAWindowPolInterp)
      fKeyTrain$MsdPolInterpG[wIndex] <- sd(tmpRawMagnGWindowPolInterp)
  
      fKeyTrain$MsdCubInterpA[wIndex] <- sd(tmpRawMagnAWindowCubInterp)
      fKeyTrain$MsdCubInterpG[wIndex] <- sd(tmpRawMagnGWindowCubInterp)
  
      fKeyTrain$MsdMeanNormA[wIndex] <- sd(tmpRawMagnAWindowMeanNorm)
      fKeyTrain$MsdMeanNormG[wIndex] <- sd(tmpRawMagnGWindowMeanNorm)
  
      # Variance
      fKeyTrain$XvarA[wIndex] <- var(tmpRawXWindow)
      fKeyTrain$YvarA[wIndex] <- var(tmpRawYWindow)
      fKeyTrain$ZvarA[wIndex] <- var(tmpRawZWindow)
      fKeyTrain$XvarG[wIndex] <- var(tmpRawAWindow)
      fKeyTrain$YvarG[wIndex] <- var(tmpRawBWindow)
      fKeyTrain$ZvarG[wIndex] <- var(tmpRawCWindow)
  
      fKeyTrain$XvarLinInterpA[wIndex] <- var(tmpRawXWindowLinInterp)
      fKeyTrain$YvarLinInterpA[wIndex] <- var(tmpRawYWindowLinInterp)
      fKeyTrain$ZvarLinInterpA[wIndex] <- var(tmpRawZWindowLinInterp)
      fKeyTrain$XvarLinInterpG[wIndex] <- var(tmpRawAWindowLinInterp)
      fKeyTrain$YvarLinInterpG[wIndex] <- var(tmpRawBWindowLinInterp)
      fKeyTrain$ZvarLinInterpG[wIndex] <- var(tmpRawCWindowLinInterp)
  
      fKeyTrain$XvarPolInterpA[wIndex] <- var(tmpRawXWindowPolInterp)
      fKeyTrain$YvarPolInterpA[wIndex] <- var(tmpRawYWindowPolInterp)
      fKeyTrain$ZvarPolInterpA[wIndex] <- var(tmpRawZWindowPolInterp)
      fKeyTrain$XvarPolInterpG[wIndex] <- var(tmpRawAWindowPolInterp)
      fKeyTrain$YvarPolInterpG[wIndex] <- var(tmpRawBWindowPolInterp)
      fKeyTrain$ZvarPolInterpG[wIndex] <- var(tmpRawCWindowPolInterp)
  
      fKeyTrain$XvarCubInterpA[wIndex] <- var(tmpRawXWindowCubInterp)
      fKeyTrain$YvarCubInterpA[wIndex] <- var(tmpRawYWindowCubInterp)
      fKeyTrain$ZvarCubInterpA[wIndex] <- var(tmpRawZWindowCubInterp)
      fKeyTrain$XvarCubInterpG[wIndex] <- var(tmpRawAWindowCubInterp)
      fKeyTrain$YvarCubInterpG[wIndex] <- var(tmpRawBWindowCubInterp)
      fKeyTrain$ZvarCubInterpG[wIndex] <- var(tmpRawCWindowCubInterp)
  
      fKeyTrain$XvarMeanNormA[wIndex] <- var(tmpRawXWindowMeanNorm)
      fKeyTrain$YvarMeanNormA[wIndex] <- var(tmpRawYWindowMeanNorm)
      fKeyTrain$ZvarMeanNormA[wIndex] <- var(tmpRawZWindowMeanNorm)
      fKeyTrain$XvarMeanNormG[wIndex] <- var(tmpRawAWindowMeanNorm)
      fKeyTrain$YvarMeanNormG[wIndex] <- var(tmpRawBWindowMeanNorm)
      fKeyTrain$ZvarMeanNormG[wIndex] <- var(tmpRawCWindowMeanNorm)
  
      #var magn
      fKeyTrain$MvarA[wIndex] <- var(tmpRawMagnAWindow)
      fKeyTrain$MvarG[wIndex] <- var(tmpRawMagnGWindow)
  
      fKeyTrain$MvarLinInterpA[wIndex] <- var(tmpRawMagnAWindowLinInterp)
      fKeyTrain$MvarLinInterpG[wIndex] <- var(tmpRawMagnGWindowLinInterp)
  
      fKeyTrain$MvarPolInterpA[wIndex] <- var(tmpRawMagnAWindowPolInterp)
      fKeyTrain$MvarPolInterpG[wIndex] <- var(tmpRawMagnGWindowPolInterp)
  
      fKeyTrain$MvarCubInterpA[wIndex] <- var(tmpRawMagnAWindowCubInterp)
      fKeyTrain$MvarCubInterpG[wIndex] <- var(tmpRawMagnGWindowCubInterp)
  
      fKeyTrain$MvarMeanNormA[wIndex] <- var(tmpRawMagnAWindowMeanNorm)
      fKeyTrain$MvarMeanNormG[wIndex] <- var(tmpRawMagnGWindowMeanNorm)
  
  
      # RMS  Accelerometer
      fKeyTrain$XrmsA[wIndex] <- sqrt(sum((tmpRawXWindow) ^ 2) / wSize)
      fKeyTrain$YrmsA[wIndex] <- sqrt(sum((tmpRawYWindow) ^ 2) / wSize)
      fKeyTrain$ZrmsA[wIndex] <- sqrt(sum((tmpRawZWindow) ^ 2) / wSize)
      fKeyTrain$XrmsG[wIndex] <- sqrt(sum((tmpRawAWindow) ^ 2) / wSize)
      fKeyTrain$YrmsG[wIndex] <- sqrt(sum((tmpRawBWindow) ^ 2) / wSize)
      fKeyTrain$ZrmsG[wIndex] <- sqrt(sum((tmpRawCWindow) ^ 2) / wSize)
  
      fKeyTrain$XrmsLinInterpA[wIndex] <- sqrt(sum((tmpRawXWindowLinInterp) ^ 2) / wSize)
      fKeyTrain$YrmsLinInterpA[wIndex] <- sqrt(sum((tmpRawYWindowLinInterp) ^ 2) / wSize)
      fKeyTrain$ZrmsLinInterpA[wIndex] <- sqrt(sum((tmpRawZWindowLinInterp) ^ 2) / wSize)
      fKeyTrain$XrmsLinInterpG[wIndex] <- sqrt(sum((tmpRawAWindowLinInterp) ^ 2) / wSize)
      fKeyTrain$YrmsLinInterpG[wIndex] <- sqrt(sum((tmpRawBWindowLinInterp) ^ 2) / wSize)
      fKeyTrain$ZrmsLinInterpG[wIndex] <- sqrt(sum((tmpRawCWindowLinInterp) ^ 2) / wSize)
      
      fKeyTrain$XrmsPolInterpA[wIndex] <- sqrt(sum((tmpRawXWindowPolInterp) ^ 2) / wSize)
      fKeyTrain$YrmsPolInterpA[wIndex] <- sqrt(sum((tmpRawYWindowPolInterp) ^ 2) / wSize)
      fKeyTrain$ZrmsPolInterpA[wIndex] <- sqrt(sum((tmpRawZWindowPolInterp) ^ 2) / wSize)
      fKeyTrain$XrmsPolInterpG[wIndex] <- sqrt(sum((tmpRawAWindowPolInterp) ^ 2) / wSize)
      fKeyTrain$YrmsPolInterpG[wIndex] <- sqrt(sum((tmpRawBWindowPolInterp) ^ 2) / wSize)
      fKeyTrain$ZrmsPolInterpG[wIndex] <- sqrt(sum((tmpRawCWindowPolInterp) ^ 2) / wSize)
  
      fKeyTrain$XrmsCubInterpA[wIndex] <- sqrt(sum((tmpRawXWindowCubInterp) ^ 2) / wSize)
      fKeyTrain$YrmsCubInterpA[wIndex] <- sqrt(sum((tmpRawYWindowCubInterp) ^ 2) / wSize)
      fKeyTrain$ZrmsCubInterpA[wIndex] <- sqrt(sum((tmpRawZWindowCubInterp) ^ 2) / wSize)
      fKeyTrain$XrmsCubInterpG[wIndex] <- sqrt(sum((tmpRawAWindowCubInterp) ^ 2) / wSize)
      fKeyTrain$YrmsCubInterpG[wIndex] <- sqrt(sum((tmpRawBWindowCubInterp) ^ 2) / wSize)
      fKeyTrain$ZrmsCubInterpG[wIndex] <- sqrt(sum((tmpRawCWindowCubInterp) ^ 2) / wSize)
  
      fKeyTrain$XrmsMeanNormA[wIndex] <- sqrt(sum(((tmpRawXWindowMeanNorm) ^ 2)) / wSize)
      fKeyTrain$YrmsMeanNormA[wIndex] <- sqrt(sum(((tmpRawYWindowMeanNorm) ^ 2)) / wSize)
      fKeyTrain$ZrmsMeanNormA[wIndex] <- sqrt(sum(((tmpRawZWindowMeanNorm) ^ 2)) / wSize)
      fKeyTrain$XrmsMeanNormG[wIndex] <- sqrt(sum(((tmpRawAWindowMeanNorm) ^ 2)) / wSize)
      fKeyTrain$YrmsMeanNormG[wIndex] <- sqrt(sum(((tmpRawBWindowMeanNorm) ^ 2)) / wSize)
      fKeyTrain$ZrmsMeanNormG[wIndex] <- sqrt(sum(((tmpRawCWindowMeanNorm) ^ 2)) / wSize)
  
      # Root Mean Square of the magnitude
      fKeyTrain$MagnRmsA[wIndex] <- sqrt((sum((tmpRawMagnAWindow) ^ 2)) / wSize)
      fKeyTrain$MagnRmsG[wIndex] <- sqrt((sum((tmpRawMagnGWindow) ^ 2)) / wSize)
  
      fKeyTrain$MagnRmsLinInterpA[wIndex] <- sqrt((sum((tmpRawMagnAWindowLinInterp) ^ 2)) / wSize)
      fKeyTrain$MagnRmsLinInterpG[wIndex] <- sqrt((sum((tmpRawMagnGWindowLinInterp) ^ 2)) / wSize)
  
      fKeyTrain$MagnRmsPolInterpA[wIndex] <- sqrt((sum((tmpRawMagnAWindowPolInterp) ^ 2)) / wSize)
      fKeyTrain$MagnRmsPolInterpG[wIndex] <- sqrt((sum((tmpRawMagnGWindowPolInterp) ^ 2)) / wSize)
  
      fKeyTrain$MagnRmsCubInterpA[wIndex] <- sqrt((sum((tmpRawMagnAWindowCubInterp) ^ 2)) / wSize)
      fKeyTrain$MagnRmsCubInterpG[wIndex] <- sqrt((sum((tmpRawMagnGWindowCubInterp) ^ 2)) / wSize)
  
      fKeyTrain$MrmsMeanNormA[wIndex] <- sqrt(sum(((tmpRawMagnAWindowMeanNorm) ^ 2)) / wSize)
      fKeyTrain$MrmsMeanNormG[wIndex] <- sqrt(sum(((tmpRawMagnGWindowMeanNorm) ^ 2)) / wSize)
  
      if(boolLabeled) {
        keyCount <- length(tmpWindowCopy$belongsToKey[tmpWindowCopy$belongsToKey == TRUE])
        if(keyCount > 0) {
        tmpKey <- keytrain[keytrain$DownTime <= tmpWindowCopy$Timestamp[tmpWindowCopy$belongsToKey == TRUE][1] & keytrain$EventTime >= tmpWindowCopy$Timestamp[tmpWindowCopy$belongsToKey == TRUE][1],]
    
        fKeyTrain$IsKeyProb[wIndex] <- keyCount/tmpKey$WindowSize
          if(keyCount/tmpKey$WindowSize >= 0.85) {
            fKeyTrain$IsKey[wIndex] <- TRUE
            fKeyTrain$Keypress[wIndex] <- tmpKey$Keypress
          }
          else {
            fKeyTrain$IsKey[wIndex] <- FALSE
            fKeyTrain$Keypress[wIndex] <- "NONE"
          }
        } else {
          fKeyTrain$IsKeyProb[wIndex] <- 0
          fKeyTrain$IsKey[wIndex] <- FALSE
          fKeyTrain$Keypress[wIndex] <- "NONE"
        }
      }
      
      # time of window
      fKeyTrain$TotalTime[wIndex] <- sensortrain$Timestamp[i + wJumper - 1] - sensortrain$Timestamp[i - wJumper - 1]
      
      wIndex <- wIndex + 1
      # window loop var
    }
    i <- i + wJumper
  }
  
  # Skewness
  fKeyTrain$XskewA <- 3 * (fKeyTrain$XmeanA - fKeyTrain$XmedianA) / fKeyTrain$XsdA
  fKeyTrain$YskewA <- 3 * (fKeyTrain$YmeanA - fKeyTrain$YmedianA) / fKeyTrain$YsdA
  fKeyTrain$ZskewA <- 3 * (fKeyTrain$ZmeanA - fKeyTrain$ZmedianA) / fKeyTrain$ZsdA
  fKeyTrain$XskewG <- 3 * (fKeyTrain$XmeanG - fKeyTrain$XmedianG) / fKeyTrain$XsdG
  fKeyTrain$YskewG <- 3 * (fKeyTrain$YmeanG - fKeyTrain$YmedianG) / fKeyTrain$YsdG
  fKeyTrain$ZskewG <- 3 * (fKeyTrain$ZmeanG - fKeyTrain$ZmedianG) / fKeyTrain$ZsdG
  
  fKeyTrain$XskewLinInterpA <- 3 * (fKeyTrain$XmeanLinInterpA - fKeyTrain$XmedianLinInterpA) / fKeyTrain$XsdLinInterpA
  fKeyTrain$YskewLinInterpA <- 3 * (fKeyTrain$YmeanLinInterpA - fKeyTrain$YmedianLinInterpA) / fKeyTrain$YsdLinInterpA
  fKeyTrain$ZskewLinInterpA <- 3 * (fKeyTrain$ZmeanLinInterpA - fKeyTrain$ZmedianLinInterpA) / fKeyTrain$ZsdLinInterpA
  fKeyTrain$XskewLinInterpG <- 3 * (fKeyTrain$XmeanLinInterpG - fKeyTrain$XmedianLinInterpG) / fKeyTrain$XsdLinInterpG
  fKeyTrain$YskewLinInterpG <- 3 * (fKeyTrain$YmeanLinInterpG - fKeyTrain$YmedianLinInterpG) / fKeyTrain$YsdLinInterpG
  fKeyTrain$ZskewLinInterpG <- 3 * (fKeyTrain$ZmeanLinInterpG - fKeyTrain$ZmedianLinInterpG) / fKeyTrain$ZsdLinInterpG
  
  fKeyTrain$XskewPolInterpA <- 3 * (fKeyTrain$XmeanPolInterpA - fKeyTrain$XmedianPolInterpA) / fKeyTrain$XsdPolInterpA
  fKeyTrain$YskewPolInterpA <- 3 * (fKeyTrain$YmeanPolInterpA - fKeyTrain$YmedianPolInterpA) / fKeyTrain$YsdPolInterpA
  fKeyTrain$ZskewPolInterpA <- 3 * (fKeyTrain$ZmeanPolInterpA - fKeyTrain$ZmedianPolInterpA) / fKeyTrain$ZsdPolInterpA
  fKeyTrain$XskewPolInterpG <- 3 * (fKeyTrain$XmeanPolInterpG - fKeyTrain$XmedianPolInterpG) / fKeyTrain$XsdPolInterpG
  fKeyTrain$YskewPolInterpG <- 3 * (fKeyTrain$YmeanPolInterpG - fKeyTrain$YmedianPolInterpG) / fKeyTrain$YsdPolInterpG
  fKeyTrain$ZskewPolInterpG <- 3 * (fKeyTrain$ZmeanPolInterpG - fKeyTrain$ZmedianPolInterpG) / fKeyTrain$ZsdPolInterpG
  
  fKeyTrain$XskewCubInterpA <- 3 * (fKeyTrain$XmeanCubInterpA - fKeyTrain$XmedianCubInterpA) / fKeyTrain$XsdCubInterpA
  fKeyTrain$YskewCubInterpA <- 3 * (fKeyTrain$YmeanCubInterpA - fKeyTrain$YmedianCubInterpA) / fKeyTrain$YsdCubInterpA
  fKeyTrain$ZskewCubInterpA <- 3 * (fKeyTrain$ZmeanCubInterpA - fKeyTrain$ZmedianCubInterpA) / fKeyTrain$ZsdCubInterpA
  fKeyTrain$XskewCubInterpG <- 3 * (fKeyTrain$XmeanCubInterpG - fKeyTrain$XmedianCubInterpG) / fKeyTrain$XsdCubInterpG
  fKeyTrain$YskewCubInterpG <- 3 * (fKeyTrain$YmeanCubInterpG - fKeyTrain$YmedianCubInterpG) / fKeyTrain$YsdCubInterpG
  fKeyTrain$ZskewCubInterpG <- 3 * (fKeyTrain$ZmeanCubInterpG - fKeyTrain$ZmedianCubInterpG) / fKeyTrain$ZsdCubInterpG
  
  fKeyTrain$XskewMeanNormA <- 3 * (fKeyTrain$XmeanMeanNormA - fKeyTrain$XmedianMeanNormA) / fKeyTrain$XsdMeanNormA
  fKeyTrain$YskewMeanNormA <- 3 * (fKeyTrain$YmeanMeanNormA - fKeyTrain$YmedianMeanNormA) / fKeyTrain$YsdMeanNormA
  fKeyTrain$ZskewMeanNormA <- 3 * (fKeyTrain$ZmeanMeanNormA - fKeyTrain$ZmedianMeanNormA) / fKeyTrain$ZsdMeanNormA
  fKeyTrain$XskewMeanNormG <- 3 * (fKeyTrain$XmeanMeanNormG - fKeyTrain$XmedianMeanNormG) / fKeyTrain$XsdMeanNormG
  fKeyTrain$YskewMeanNormG <- 3 * (fKeyTrain$YmeanMeanNormG - fKeyTrain$YmedianMeanNormG) / fKeyTrain$YsdMeanNormG
  fKeyTrain$ZskewMeanNormG <- 3 * (fKeyTrain$ZmeanMeanNormG - fKeyTrain$ZmedianMeanNormG) / fKeyTrain$ZsdMeanNormG
  
  # skewness magn
  fKeyTrain$MskewA <- 3 * (fKeyTrain$MmeanA - fKeyTrain$MmedianA) / fKeyTrain$MsdA
  fKeyTrain$MskewG <- 3 * (fKeyTrain$MmeanG - fKeyTrain$MmedianG) / fKeyTrain$MsdG
  
  fKeyTrain$MskewLinInterpA <- 3 * (fKeyTrain$MmeanLinInterpA - fKeyTrain$MmedianLinInterpA) / fKeyTrain$MsdLinInterpA
  fKeyTrain$MskewLinInterpG <- 3 * (fKeyTrain$MmeanLinInterpG - fKeyTrain$MmedianLinInterpG) / fKeyTrain$MsdLinInterpG
  
  fKeyTrain$MskewPolInterpA <- 3 * (fKeyTrain$MmeanPolInterpA - fKeyTrain$MmedianPolInterpA) / fKeyTrain$MsdPolInterpA
  fKeyTrain$MskewPolInterpG <- 3 * (fKeyTrain$MmeanPolInterpG - fKeyTrain$MmedianPolInterpG) / fKeyTrain$MsdPolInterpG
  
  fKeyTrain$MskewCubInterpA <- 3 * (fKeyTrain$MmeanCubInterpA - fKeyTrain$MmedianCubInterpA) / fKeyTrain$MsdCubInterpA
  fKeyTrain$MskewCubInterpG <- 3 * (fKeyTrain$MmeanCubInterpG - fKeyTrain$MmedianCubInterpG) / fKeyTrain$MsdCubInterpG
  
  fKeyTrain$MskewMeanNormA <- 3 * (fKeyTrain$MmeanMeanNormA - fKeyTrain$MmedianMeanNormA) / fKeyTrain$MsdMeanNormA
  fKeyTrain$MskewMeanNormG <- 3 * (fKeyTrain$MmeanMeanNormG - fKeyTrain$MmedianMeanNormG) / fKeyTrain$MsdMeanNormG
  
  # write files
  if(boolTraining) {
    write.csv(
      fKeyTrain,
      paste(getwd(),"/datasets/", stringFileTimestamp, "-dataset-training-", wSize, ".csv", sep=""),
      row.names = FALSE
    )
  } else if(boolLabeled) {
    write.csv(
      fKeyTrain,
      paste(getwd(),"/datasets/", stringFileTimestamp, "-dataset-test-", wSize, ".csv", sep=""),
      row.names = FALSE
    )
  } else {
    write.csv(
      fKeyTrain,
      paste(getwd(),"/datasets/", stringFileTimestamp, "-dataset-test-unlabeled-", wSize, ".csv", sep=""),
      row.names = FALSE
    )
  }
  
  # write extra feature set with no "NONE" values
  ftest <- fKeyTrain[-which(fKeyTrain == "NONE"),]
  
  write.csv(
    ftest,
    paste(getwd(),"/datasets/", stringFileTimestamp, "-dataset-training-keys-only", wSize, ".csv", sep=""),
    row.names = FALSE
  )
  
  end.time <- proc.time()
  print(paste("DONE in ", end.time[3] - start.time[3],"s",sep=""))
  return(fKeyTrain)
}
