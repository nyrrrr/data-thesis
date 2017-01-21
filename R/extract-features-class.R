# feature processing function
feature.extraction <- function(boolTraining, stringFileTimestamp, boolLabeled) {
  start.time <- proc.time()
  
  if(boolTraining) print(paste("Generate features from ", stringFileTimestamp, "-sensor-dataset-training-raw.csv", sep=""))
  else print(paste("Generate features from ","transferred-", stringFileTimestamp, "-victim-data",", labeled=", boolLabeled, sep=""))

  
  if(boolTraining) {
    sensortrain <-
      read.csv(
        paste(getwd(),"/datasets/", stringFileTimestamp, "-sensor-dataset-training-RAW.csv", sep=""), 
        header = TRUE,
        stringsAsFactors=FALSE
      )
    
    keytrain <-
      read.csv(
        paste(getwd(),"/datasets/", stringFileTimestamp, "-key-dataset-training-RAW.csv", sep=""),
        header = TRUE,
        stringsAsFactors=FALSE
      )
  } else {
    sensortrain <-
      read.csv(
        paste(getwd(),"/datasets/", "transferred-", stringFileTimestamp, "-victim-data-RAW.csv", sep=""), # 17011417, 17011205, 17011020, 16122802
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
  print(paste("Processing ", length(sensortrain$Timestamp), " entries", sep=""))
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
  sensortrain$alpha <- sensortrain$alpha - mean(sensortrain$alpha)
  sensortrain$beta <- sensortrain$beta - mean(sensortrain$beta)
  sensortrain$gamma <- sensortrain$gamma - mean(sensortrain$gamma)
  
  # Square Sum of 3D vectors
  sensortrain$SqSumA <- sensortrain$xA ^ 2 + sensortrain$yA ^ 2 + sensortrain$zA ^ 2
  sensortrain$SqSumG <- sensortrain$xG^2 + sensortrain$yG^2 + sensortrain$zG^2
  
  # Magnitude of 3D Vectors
  sensortrain$MagnA <- sqrt(sensortrain$SqSumA)
  sensortrain$MagnG <- sqrt(sensortrain$SqSumG)
  
  
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
      tmpRawLinearInterpolMod <- lm(tmpRawXWindow ~ poly(tmpWindowCopy$Timestamp, degree=1))
      tmpRawXWindowLinInterp <- tmpRawXWindow - predict(tmpRawLinearInterpolMod)
      fKeyTrain$Xd0LinInterpA[wIndex] <- tmpRawLinearInterpolMod$coefficients[1]
      fKeyTrain$Xd1LinInterpA[wIndex] <- tmpRawLinearInterpolMod$coefficients[2]
      
      tmpRawLinearInterpolMod <- lm(tmpRawYWindow ~ poly(tmpWindowCopy$Timestamp, degree=1))
      tmpRawYWindowLinInterp <- tmpRawYWindow - predict(tmpRawLinearInterpolMod)
      fKeyTrain$Yd0LinInterpA[wIndex] <- tmpRawLinearInterpolMod$coefficients[1]
      fKeyTrain$Yd1LinInterpA[wIndex] <- tmpRawLinearInterpolMod$coefficients[2]
      
      tmpRawLinearInterpolMod <- lm(tmpRawZWindow ~ poly(tmpWindowCopy$Timestamp, degree=1))
      tmpRawZWindowLinInterp <- tmpRawZWindow - predict(tmpRawLinearInterpolMod)
      fKeyTrain$Zd0LinInterpA[wIndex] <- tmpRawLinearInterpolMod$coefficients[1]
      fKeyTrain$Zd1LinInterpA[wIndex] <- tmpRawLinearInterpolMod$coefficients[2]
      
      tmpRawLinearInterpolMod <- lm(tmpRawAWindow ~ poly(tmpWindowCopy$Timestamp, degree=1))
      tmpRawAWindowLinInterp <- tmpRawAWindow - predict(tmpRawLinearInterpolMod)
      fKeyTrain$Xd0LinInterpG[wIndex] <- tmpRawLinearInterpolMod$coefficients[1]
      fKeyTrain$Xd1LinInterpG[wIndex] <- tmpRawLinearInterpolMod$coefficients[2]
      
      tmpRawLinearInterpolMod <- lm(tmpRawBWindow ~ poly(tmpWindowCopy$Timestamp, degree=1))
      tmpRawBWindowLinInterp <- tmpRawBWindow - predict(tmpRawLinearInterpolMod)
      fKeyTrain$Yd0LinInterpG[wIndex] <- tmpRawLinearInterpolMod$coefficients[1]
      fKeyTrain$Yd1LinInterpG[wIndex] <- tmpRawLinearInterpolMod$coefficients[2]
      
      tmpRawLinearInterpolMod <- lm(tmpRawCWindow ~ poly(tmpWindowCopy$Timestamp, degree=1))
      tmpRawCWindowLinInterp <- tmpRawCWindow - predict(tmpRawLinearInterpolMod)
      fKeyTrain$Zd0LinInterpG[wIndex] <- tmpRawLinearInterpolMod$coefficients[1]
      fKeyTrain$Zd1LinInterpG[wIndex] <- tmpRawLinearInterpolMod$coefficients[2]
      
      tmpRawLinearInterpolMod <- lm(tmpRawMagnAWindow ~ poly(tmpWindowCopy$Timestamp, degree=1))
      tmpRawMagnAWindowLinInterp <- tmpRawMagnAWindow - predict(tmpRawLinearInterpolMod)
      fKeyTrain$Md0LinInterpA[wIndex] <- tmpRawLinearInterpolMod$coefficients[1]
      fKeyTrain$Md1LinInterpA[wIndex] <- tmpRawLinearInterpolMod$coefficients[2]
      
      tmpRawLinearInterpolMod <- lm(tmpRawMagnGWindow ~ poly(tmpWindowCopy$Timestamp, degree=1))
      tmpRawMagnGWindowLinInterp <- tmpRawMagnGWindow - predict(tmpRawLinearInterpolMod)
      fKeyTrain$Md0LinInterpG[wIndex] <- tmpRawLinearInterpolMod$coefficients[1]
      fKeyTrain$Md1LinInterpG[wIndex] <- tmpRawLinearInterpolMod$coefficients[2]
  
      # polynom (3) interpol
      tmpRawPolyMod <- lm(tmpRawXWindow ~ poly(tmpWindowCopy$Timestamp, degree=3))
      tmpRawXWindowPoly3DInterp <- predict(tmpRawPolyMod)
      fKeyTrain$Xd0Poly3DInterpA[wIndex] <- tmpRawPolyMod$coefficients[1]
      fKeyTrain$Xd1Poly3DInterpA[wIndex] <- tmpRawPolyMod$coefficients[2]
      fKeyTrain$Xd2Poly3DInterpA[wIndex] <- tmpRawPolyMod$coefficients[3]
      fKeyTrain$Xd3Poly3DInterpA[wIndex] <- tmpRawPolyMod$coefficients[4]
  
      tmpRawPolyMod <- lm(tmpRawYWindow ~ poly(tmpWindowCopy$Timestamp, degree=3))
      tmpRawYWindowPoly3DInterp <- predict(tmpRawPolyMod)
      fKeyTrain$Yd0Poly3DInterpA[wIndex] <- tmpRawPolyMod$coefficients[1]
      fKeyTrain$Yd1Poly3DInterpA[wIndex] <- tmpRawPolyMod$coefficients[2]
      fKeyTrain$Yd2Poly3DInterpA[wIndex] <- tmpRawPolyMod$coefficients[3]
      fKeyTrain$Yd3Poly3DInterpA[wIndex] <- tmpRawPolyMod$coefficients[4]
  
      tmpRawPolyMod <- lm(tmpRawZWindow ~ poly(tmpWindowCopy$Timestamp, degree=3))
      tmpRawZWindowPoly3DInterp <- predict(tmpRawPolyMod)
      fKeyTrain$Zd0Poly3DInterpA[wIndex] <- tmpRawPolyMod$coefficients[1]
      fKeyTrain$Zd1Poly3DInterpA[wIndex] <- tmpRawPolyMod$coefficients[2]
      fKeyTrain$Zd2Poly3DInterpA[wIndex] <- tmpRawPolyMod$coefficients[3]
      fKeyTrain$Zd3Poly3DInterpA[wIndex] <- tmpRawPolyMod$coefficients[4]
  
      tmpRawPolyMod <- lm(tmpRawAWindow ~ poly(tmpWindowCopy$Timestamp, degree=3))
      tmpRawAWindowPoly3DInterp <- predict(tmpRawPolyMod)
      fKeyTrain$Xd0Poly3DInterpG[wIndex] <- tmpRawPolyMod$coefficients[1]
      fKeyTrain$Xd1Poly3DInterpG[wIndex] <- tmpRawPolyMod$coefficients[2]
      fKeyTrain$Xd2Poly3DInterpG[wIndex] <- tmpRawPolyMod$coefficients[3]
      fKeyTrain$Xd3Poly3DInterpG[wIndex] <- tmpRawPolyMod$coefficients[4]
  
      tmpRawPolyMod <- lm(tmpRawBWindow ~ poly(tmpWindowCopy$Timestamp, degree=3))
      tmpRawBWindowPoly3DInterp <- predict(tmpRawPolyMod)
      fKeyTrain$Yd0Poly3DInterpG[wIndex] <- tmpRawPolyMod$coefficients[1]
      fKeyTrain$Yd1Poly3DInterpG[wIndex] <- tmpRawPolyMod$coefficients[2]
      fKeyTrain$Yd2Poly3DInterpG[wIndex] <- tmpRawPolyMod$coefficients[3]
      fKeyTrain$Yd3Poly3DInterpG[wIndex] <- tmpRawPolyMod$coefficients[4]
  
      tmpRawPolyMod <- lm(tmpRawCWindow ~ poly(tmpWindowCopy$Timestamp, degree=3))
      tmpRawCWindowPoly3DInterp <- predict(tmpRawPolyMod)
      fKeyTrain$Zd0Poly3DInterpG[wIndex] <- tmpRawPolyMod$coefficients[1]
      fKeyTrain$Zd1Poly3DInterpG[wIndex] <- tmpRawPolyMod$coefficients[2]
      fKeyTrain$Zd2Poly3DInterpG[wIndex] <- tmpRawPolyMod$coefficients[3]
      fKeyTrain$Zd3Poly3DInterpG[wIndex] <- tmpRawPolyMod$coefficients[4]
      
      tmpRawPolyMod <- lm(tmpRawMagnAWindow ~ poly(tmpWindowCopy$Timestamp, degree=3))
      tmpRawMagnAWindowPoly3DInterp <- predict(tmpRawPolyMod)
      fKeyTrain$Md0Poly3DInterpA[wIndex] <- tmpRawPolyMod$coefficients[1]
      fKeyTrain$Md1Poly3DInterpA[wIndex] <- tmpRawPolyMod$coefficients[2]
      fKeyTrain$Md2Poly3DInterpA[wIndex] <- tmpRawPolyMod$coefficients[3]
      fKeyTrain$Md3Poly3DInterpA[wIndex] <- tmpRawPolyMod$coefficients[4]
      
      tmpRawPolyMod <- lm(tmpRawMagnGWindow ~ poly(tmpWindowCopy$Timestamp, degree=3))
      tmpRawMagnGWindowPoly3DInterp <- predict(tmpRawPolyMod)
      fKeyTrain$Md0Poly3DInterpG[wIndex] <- tmpRawPolyMod$coefficients[1]
      fKeyTrain$Md1Poly3DInterpG[wIndex] <- tmpRawPolyMod$coefficients[2]
      fKeyTrain$Md2Poly3DInterpG[wIndex] <- tmpRawPolyMod$coefficients[3]
      fKeyTrain$Md3Poly3DInterpG[wIndex] <- tmpRawPolyMod$coefficients[4]
      
      # cubic spline interpol
      tmpRawXWindowCubInterp <- (spline(tmpWindowCopy$Timestamp, tmpRawXWindow))$y
      tmpRawYWindowCubInterp <- (spline(tmpWindowCopy$Timestamp, tmpRawYWindow))$y
      tmpRawZWindowCubInterp <- (spline(tmpWindowCopy$Timestamp, tmpRawZWindow))$y
      tmpRawAWindowCubInterp <- (spline(tmpWindowCopy$Timestamp, tmpRawAWindow))$y
      tmpRawBWindowCubInterp <- (spline(tmpWindowCopy$Timestamp, tmpRawBWindow))$y
      tmpRawCWindowCubInterp <- (spline(tmpWindowCopy$Timestamp, tmpRawCWindow))$y
      tmpRawMagnAWindowCubInterp <- (spline(tmpWindowCopy$Timestamp, tmpRawMagnAWindow))$y
      tmpRawMagnGWindowCubInterp <- (spline(tmpWindowCopy$Timestamp, tmpRawMagnGWindow))$y
  
      # mean norm
      tmpRawXWindowMeanNorm <- tmpRawXWindow - mean(tmpRawXWindow)
      tmpRawYWindowMeanNorm <- tmpRawYWindow - mean(tmpRawYWindow)
      tmpRawZWindowMeanNorm <- tmpRawZWindow - mean(tmpRawZWindow)
      tmpRawAWindowMeanNorm <- tmpRawAWindow - mean(tmpRawAWindow)
      tmpRawBWindowMeanNorm <- tmpRawBWindow - mean(tmpRawBWindow)
      tmpRawCWindowMeanNorm <- tmpRawCWindow - mean(tmpRawCWindow)
      tmpRawMagnAWindowMeanNorm <- tmpRawMagnAWindow - mean(tmpRawMagnAWindow)
      tmpRawMagnGWindowMeanNorm <- tmpRawMagnGWindow - mean(tmpRawMagnGWindow)
  
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
  
      fKeyTrain$XminPoly3DInterpA[wIndex] <- min(tmpRawXWindowPoly3DInterp)
      fKeyTrain$YminPoly3DInterpA[wIndex] <- min(tmpRawYWindowPoly3DInterp)
      fKeyTrain$ZminPoly3DInterpA[wIndex] <- min(tmpRawZWindowPoly3DInterp)
      fKeyTrain$XminPoly3DInterpG[wIndex] <- min(tmpRawAWindowPoly3DInterp)
      fKeyTrain$YminPoly3DInterpG[wIndex] <- min(tmpRawBWindowPoly3DInterp)
      fKeyTrain$ZminPoly3DInterpG[wIndex] <- min(tmpRawCWindowPoly3DInterp)
  
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
  
      fKeyTrain$MminPoly3DInterpA[wIndex] <- min(tmpRawMagnAWindowPoly3DInterp)
      fKeyTrain$MminPoly3DInterpG[wIndex] <- min(tmpRawMagnGWindowPoly3DInterp)
  
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
  
      fKeyTrain$XmaxPoly3DInterpA[wIndex] <- max(tmpRawXWindowPoly3DInterp)
      fKeyTrain$YmaxPoly3DInterpA[wIndex] <- max(tmpRawYWindowPoly3DInterp)
      fKeyTrain$ZmaxPoly3DInterpA[wIndex] <- max(tmpRawZWindowPoly3DInterp)
      fKeyTrain$XmaxPoly3DInterpG[wIndex] <- max(tmpRawAWindowPoly3DInterp)
      fKeyTrain$YmaxPoly3DInterpG[wIndex] <- max(tmpRawBWindowPoly3DInterp)
      fKeyTrain$ZmaxPoly3DInterpG[wIndex] <- max(tmpRawCWindowPoly3DInterp)
  
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
  
      fKeyTrain$MmaxPoly3DInterpA[wIndex] <- max(tmpRawMagnAWindowPoly3DInterp)
      fKeyTrain$MmaxPoly3DInterpG[wIndex] <- max(tmpRawMagnGWindowPoly3DInterp)
  
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
  
      fKeyTrain$XmeanPoly3DInterpA[wIndex] <- mean(tmpRawXWindowPoly3DInterp)
      fKeyTrain$YmeanPoly3DInterpA[wIndex] <- mean(tmpRawYWindowPoly3DInterp)
      fKeyTrain$ZmeanPoly3DInterpA[wIndex] <- mean(tmpRawZWindowPoly3DInterp)
      fKeyTrain$XmeanPoly3DInterpG[wIndex] <- mean(tmpRawAWindowPoly3DInterp)
      fKeyTrain$YmeanPoly3DInterpG[wIndex] <- mean(tmpRawBWindowPoly3DInterp)
      fKeyTrain$ZmeanPoly3DInterpG[wIndex] <- mean(tmpRawCWindowPoly3DInterp)
  
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
  
      fKeyTrain$MmeanPoly3DInterpA[wIndex] <- mean(tmpRawMagnAWindowPoly3DInterp)
      fKeyTrain$MmeanPoly3DInterpG[wIndex] <- mean(tmpRawMagnGWindowPoly3DInterp)
  
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
  
      fKeyTrain$XmedianPoly3DInterpA[wIndex] <- median(tmpRawXWindowPoly3DInterp)
      fKeyTrain$YmedianPoly3DInterpA[wIndex] <- median(tmpRawYWindowPoly3DInterp)
      fKeyTrain$ZmedianPoly3DInterpA[wIndex] <- median(tmpRawZWindowPoly3DInterp)
      fKeyTrain$XmedianPoly3DInterpG[wIndex] <- median(tmpRawAWindowPoly3DInterp)
      fKeyTrain$YmedianPoly3DInterpG[wIndex] <- median(tmpRawBWindowPoly3DInterp)
      fKeyTrain$ZmedianPoly3DInterpG[wIndex] <- median(tmpRawCWindowPoly3DInterp)
  
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
  
      fKeyTrain$MmedianPoly3DInterpA[wIndex] <- median(tmpRawMagnAWindowPoly3DInterp)
      fKeyTrain$MmedianPoly3DInterpG[wIndex] <- median(tmpRawMagnGWindowPoly3DInterp)
  
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
  
      fKeyTrain$XsdPoly3DInterpA[wIndex] <- sd(tmpRawXWindowPoly3DInterp)
      fKeyTrain$YsdPoly3DInterpA[wIndex] <- sd(tmpRawYWindowPoly3DInterp)
      fKeyTrain$ZsdPoly3DInterpA[wIndex] <- sd(tmpRawZWindowPoly3DInterp)
      fKeyTrain$XsdPoly3DInterpG[wIndex] <- sd(tmpRawAWindowPoly3DInterp)
      fKeyTrain$YsdPoly3DInterpG[wIndex] <- sd(tmpRawBWindowPoly3DInterp)
      fKeyTrain$ZsdPoly3DInterpG[wIndex] <- sd(tmpRawCWindowPoly3DInterp)
  
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
  
      # sd magn
      fKeyTrain$MsdA[wIndex] <- sd(tmpRawMagnAWindow)
      fKeyTrain$MsdG[wIndex] <- sd(tmpRawMagnGWindow)
  
      fKeyTrain$MsdLinInterpA[wIndex] <- sd(tmpRawMagnAWindowLinInterp)
      fKeyTrain$MsdLinInterpG[wIndex] <- sd(tmpRawMagnGWindowLinInterp)
  
      fKeyTrain$MsdPoly3DInterpA[wIndex] <- sd(tmpRawMagnAWindowPoly3DInterp)
      fKeyTrain$MsdPoly3DInterpG[wIndex] <- sd(tmpRawMagnGWindowPoly3DInterp)
  
      fKeyTrain$MsdCubInterpA[wIndex] <- sd(tmpRawMagnAWindowCubInterp)
      fKeyTrain$MsdCubInterpG[wIndex] <- sd(tmpRawMagnGWindowCubInterp)
  
      fKeyTrain$MsdMeanNormA[wIndex] <- sd(tmpRawMagnAWindowMeanNorm)
      fKeyTrain$MsdMeanNormG[wIndex] <- sd(tmpRawMagnGWindowMeanNorm)
      
      # kurtosis
      fKeyTrain$XkurtA <- (sum((tmpRawXWindow - mean(tmpRawXWindow))^4)/length(tmpRawXWindow))/fKeyTrain$XsdA^4 - 3
      fKeyTrain$YkurtA <- (sum((tmpRawYWindow - mean(tmpRawYWindow))^4)/length(tmpRawYWindow))/fKeyTrain$YsdA^4 - 3
      fKeyTrain$ZkurtA <- (sum((tmpRawZWindow - mean(tmpRawZWindow))^4)/length(tmpRawZWindow))/fKeyTrain$ZsdA^4 - 3
      fKeyTrain$XkurtG <- (sum((tmpRawAWindow - mean(tmpRawAWindow))^4)/length(tmpRawAWindow))/fKeyTrain$XsdG^4 - 3
      fKeyTrain$YkurtG <- (sum((tmpRawBWindow - mean(tmpRawBWindow))^4)/length(tmpRawBWindow))/fKeyTrain$YsdG^4 - 3
      fKeyTrain$ZkurtG <- (sum((tmpRawCWindow - mean(tmpRawCWindow))^4)/length(tmpRawCWindow))/fKeyTrain$ZsdG^4 - 3
      
      fKeyTrain$XkurtLinInterpA <- (sum((tmpRawXWindowLinInterp - mean(tmpRawXWindowLinInterp))^4)/length(tmpRawXWindowLinInterp))/fKeyTrain$XsdLinInterpA^4 - 3
      fKeyTrain$YkurtLinInterpA <- (sum((tmpRawYWindowLinInterp - mean(tmpRawYWindowLinInterp))^4)/length(tmpRawYWindowLinInterp))/fKeyTrain$YsdLinInterpA^4 - 3
      fKeyTrain$ZkurtLinInterpA <- (sum((tmpRawZWindowLinInterp - mean(tmpRawZWindowLinInterp))^4)/length(tmpRawZWindowLinInterp))/fKeyTrain$ZsdLinInterpA^4 - 3
      fKeyTrain$XkurtLinInterpG <- (sum((tmpRawAWindowLinInterp - mean(tmpRawAWindowLinInterp))^4)/length(tmpRawAWindowLinInterp))/fKeyTrain$XsdLinInterpG^4 - 3
      fKeyTrain$YkurtLinInterpG <- (sum((tmpRawBWindowLinInterp - mean(tmpRawBWindowLinInterp))^4)/length(tmpRawBWindowLinInterp))/fKeyTrain$YsdLinInterpG^4 - 3
      fKeyTrain$ZkurtLinInterpG <- (sum((tmpRawCWindowLinInterp - mean(tmpRawCWindowLinInterp))^4)/length(tmpRawCWindowLinInterp))/fKeyTrain$ZsdLinInterpG^4 - 3
      
      fKeyTrain$XkurtPoly3DInterpA <- (sum((tmpRawXWindowPoly3DInterp - mean(tmpRawXWindowPoly3DInterp))^4)/length(tmpRawXWindowPoly3DInterp))/fKeyTrain$XsdPoly3DInterpA^4 - 3
      fKeyTrain$YkurtPoly3DInterpA <- (sum((tmpRawYWindowPoly3DInterp - mean(tmpRawYWindowPoly3DInterp))^4)/length(tmpRawYWindowPoly3DInterp))/fKeyTrain$YsdPoly3DInterpA^4 - 3
      fKeyTrain$ZkurtPoly3DInterpA <- (sum((tmpRawZWindowPoly3DInterp - mean(tmpRawZWindowPoly3DInterp))^4)/length(tmpRawZWindowPoly3DInterp))/fKeyTrain$ZsdPoly3DInterpA^4 - 3
      fKeyTrain$XkurtPoly3DInterpG <- (sum((tmpRawAWindowPoly3DInterp - mean(tmpRawAWindowPoly3DInterp))^4)/length(tmpRawAWindowPoly3DInterp))/fKeyTrain$XsdPoly3DInterpG^4 - 3
      fKeyTrain$YkurtPoly3DInterpG <- (sum((tmpRawBWindowPoly3DInterp - mean(tmpRawBWindowPoly3DInterp))^4)/length(tmpRawBWindowPoly3DInterp))/fKeyTrain$YsdPoly3DInterpG^4 - 3
      fKeyTrain$ZkurtPoly3DInterpG <- (sum((tmpRawCWindowPoly3DInterp - mean(tmpRawCWindowPoly3DInterp))^4)/length(tmpRawCWindowPoly3DInterp))/fKeyTrain$ZsdPoly3DInterpG^4 - 3
      
      fKeyTrain$XkurtCubInterpA <- (sum((tmpRawXWindowCubInterp - mean(tmpRawXWindowCubInterp))^4)/length(tmpRawXWindowCubInterp))/fKeyTrain$XsdCubInterpA^4 - 3
      fKeyTrain$YkurtCubInterpA <- (sum((tmpRawYWindowCubInterp - mean(tmpRawYWindowCubInterp))^4)/length(tmpRawYWindowCubInterp))/fKeyTrain$YsdCubInterpA^4 - 3
      fKeyTrain$ZkurtCubInterpA <- (sum((tmpRawZWindowCubInterp - mean(tmpRawZWindowCubInterp))^4)/length(tmpRawZWindowCubInterp))/fKeyTrain$ZsdCubInterpA^4 - 3
      fKeyTrain$XkurtCubInterpG <- (sum((tmpRawAWindowCubInterp - mean(tmpRawAWindowCubInterp))^4)/length(tmpRawAWindowCubInterp))/fKeyTrain$XsdCubInterpG^4 - 3
      fKeyTrain$YkurtCubInterpG <- (sum((tmpRawBWindowCubInterp - mean(tmpRawBWindowCubInterp))^4)/length(tmpRawBWindowCubInterp))/fKeyTrain$YsdCubInterpG^4 - 3
      fKeyTrain$ZkurtCubInterpG <- (sum((tmpRawCWindowCubInterp - mean(tmpRawCWindowCubInterp))^4)/length(tmpRawCWindowCubInterp))/fKeyTrain$ZsdCubInterpG^4 - 3
      
      fKeyTrain$XkurtMeanNormA <- (sum((tmpRawXWindowMeanNorm - mean(tmpRawXWindowMeanNorm))^4)/length(tmpRawXWindowMeanNorm))/fKeyTrain$XsdMeanNormA^4 - 3
      fKeyTrain$YkurtMeanNormA <- (sum((tmpRawYWindowMeanNorm - mean(tmpRawYWindowMeanNorm))^4)/length(tmpRawYWindowMeanNorm))/fKeyTrain$YsdMeanNormA^4 - 3
      fKeyTrain$ZkurtMeanNormA <- (sum((tmpRawZWindowMeanNorm - mean(tmpRawZWindowMeanNorm))^4)/length(tmpRawZWindowMeanNorm))/fKeyTrain$ZsdMeanNormA^4 - 3
      fKeyTrain$XkurtMeanNormG <- (sum((tmpRawAWindowMeanNorm - mean(tmpRawAWindowMeanNorm))^4)/length(tmpRawAWindowMeanNorm))/fKeyTrain$XsdMeanNormG^4 - 3
      fKeyTrain$YkurtMeanNormG <- (sum((tmpRawBWindowMeanNorm - mean(tmpRawBWindowMeanNorm))^4)/length(tmpRawBWindowMeanNorm))/fKeyTrain$YsdMeanNormG^4 - 3
      fKeyTrain$ZkurtMeanNormG <- (sum((tmpRawCWindowMeanNorm - mean(tmpRawCWindowMeanNorm))^4)/length(tmpRawCWindowMeanNorm))/fKeyTrain$ZsdMeanNormG^4 - 3
  
      # kurtosis magn
      fKeyTrain$MkurtA <- (sum((tmpRawMagnAWindow - mean(tmpRawMagnAWindow))^4)/length(tmpRawMagnAWindow))/fKeyTrain$MsdA^4 - 3
      fKeyTrain$MkurtG <- (sum((tmpRawMagnGWindow - mean(tmpRawMagnGWindow))^4)/length(tmpRawMagnGWindow))/fKeyTrain$MsdG^4 - 3
      
      fKeyTrain$MkurtA <- (sum((tmpRawMagnAWindowLinInterp - mean(tmpRawMagnAWindowLinInterp))^4)/length(tmpRawMagnAWindowLinInterp))/fKeyTrain$MsdLinInterpA^4 - 3
      fKeyTrain$MkurtG <- (sum((tmpRawMagnGWindowLinInterp - mean(tmpRawMagnGWindowLinInterp))^4)/length(tmpRawMagnGWindowLinInterp))/fKeyTrain$MsdLinInterpG^4 - 3
      
      fKeyTrain$MkurtA <- (sum((tmpRawMagnAWindowPoly3DInterp - mean(tmpRawMagnAWindowPoly3DInterp))^4)/length(tmpRawMagnAWindowPoly3DInterp))/fKeyTrain$MsdPoly3DInterpA^4 - 3
      fKeyTrain$MkurtG <- (sum((tmpRawMagnGWindowPoly3DInterp - mean(tmpRawMagnGWindowPoly3DInterp))^4)/length(tmpRawMagnGWindowPoly3DInterp))/fKeyTrain$MsdPoly3DInterpG^4 - 3
      
      fKeyTrain$MkurtA <- (sum((tmpRawMagnAWindowCubInterp - mean(tmpRawMagnAWindowCubInterp))^4)/length(tmpRawMagnAWindowCubInterp))/fKeyTrain$MsdCubInterpA^4 - 3
      fKeyTrain$MkurtG <- (sum((tmpRawMagnGWindowCubInterp - mean(tmpRawMagnGWindowCubInterp))^4)/length(tmpRawMagnGWindowCubInterp))/fKeyTrain$MsdCubInterpG^4 - 3
      
      fKeyTrain$MkurtA <- (sum((tmpRawMagnAWindowMeanNorm - mean(tmpRawMagnAWindowMeanNorm))^4)/length(tmpRawMagnAWindowMeanNorm))/fKeyTrain$MsdMeanNormA^4 - 3
      fKeyTrain$MkurtG <- (sum((tmpRawMagnGWindowMeanNorm - mean(tmpRawMagnGWindowMeanNorm))^4)/length(tmpRawMagnGWindowMeanNorm))/fKeyTrain$MsdMeanNormG^4 - 3
      
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
      
      fKeyTrain$XrmsPoly3DInterpA[wIndex] <- sqrt(sum((tmpRawXWindowPoly3DInterp) ^ 2) / wSize)
      fKeyTrain$YrmsPoly3DInterpA[wIndex] <- sqrt(sum((tmpRawYWindowPoly3DInterp) ^ 2) / wSize)
      fKeyTrain$ZrmsPoly3DInterpA[wIndex] <- sqrt(sum((tmpRawZWindowPoly3DInterp) ^ 2) / wSize)
      fKeyTrain$XrmsPoly3DInterpG[wIndex] <- sqrt(sum((tmpRawAWindowPoly3DInterp) ^ 2) / wSize)
      fKeyTrain$YrmsPoly3DInterpG[wIndex] <- sqrt(sum((tmpRawBWindowPoly3DInterp) ^ 2) / wSize)
      fKeyTrain$ZrmsPoly3DInterpG[wIndex] <- sqrt(sum((tmpRawCWindowPoly3DInterp) ^ 2) / wSize)
  
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
  
      fKeyTrain$MagnRmsPoly3DInterpA[wIndex] <- sqrt((sum((tmpRawMagnAWindowPoly3DInterp) ^ 2)) / wSize)
      fKeyTrain$MagnRmsPoly3DInterpG[wIndex] <- sqrt((sum((tmpRawMagnGWindowPoly3DInterp) ^ 2)) / wSize)
  
      fKeyTrain$MagnRmsCubInterpA[wIndex] <- sqrt((sum((tmpRawMagnAWindowCubInterp) ^ 2)) / wSize)
      fKeyTrain$MagnRmsCubInterpG[wIndex] <- sqrt((sum((tmpRawMagnGWindowCubInterp) ^ 2)) / wSize)
  
      fKeyTrain$MrmsMeanNormA[wIndex] <- sqrt(sum(((tmpRawMagnAWindowMeanNorm) ^ 2)) / wSize)
      fKeyTrain$MrmsMeanNormG[wIndex] <- sqrt(sum(((tmpRawMagnGWindowMeanNorm) ^ 2)) / wSize)
  
      if(boolLabeled) {
        keyCount <- length(tmpWindowCopy$belongsToKey[tmpWindowCopy$belongsToKey == TRUE])
        if(keyCount > 0) {
        tmpKey <- keytrain[keytrain$DownTime <= tmpWindowCopy$Timestamp[tmpWindowCopy$belongsToKey == TRUE][1] & keytrain$EventTime >= tmpWindowCopy$Timestamp[tmpWindowCopy$belongsToKey == TRUE][1],]
    
        fKeyTrain$IsKeyProb[wIndex] <- keyCount/tmpKey$WindowSize
        fKeyTrain$IsKey[wIndex] <- TRUE
        fKeyTrain$Keypress[wIndex] <- tmpKey$Keypress
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
  
  fKeyTrain$XskewPoly3DInterpA <- 3 * (fKeyTrain$XmeanPoly3DInterpA - fKeyTrain$XmedianPoly3DInterpA) / fKeyTrain$XsdPoly3DInterpA
  fKeyTrain$YskewPoly3DInterpA <- 3 * (fKeyTrain$YmeanPoly3DInterpA - fKeyTrain$YmedianPoly3DInterpA) / fKeyTrain$YsdPoly3DInterpA
  fKeyTrain$ZskewPoly3DInterpA <- 3 * (fKeyTrain$ZmeanPoly3DInterpA - fKeyTrain$ZmedianPoly3DInterpA) / fKeyTrain$ZsdPoly3DInterpA
  fKeyTrain$XskewPoly3DInterpG <- 3 * (fKeyTrain$XmeanPoly3DInterpG - fKeyTrain$XmedianPoly3DInterpG) / fKeyTrain$XsdPoly3DInterpG
  fKeyTrain$YskewPoly3DInterpG <- 3 * (fKeyTrain$YmeanPoly3DInterpG - fKeyTrain$YmedianPoly3DInterpG) / fKeyTrain$YsdPoly3DInterpG
  fKeyTrain$ZskewPoly3DInterpG <- 3 * (fKeyTrain$ZmeanPoly3DInterpG - fKeyTrain$ZmedianPoly3DInterpG) / fKeyTrain$ZsdPoly3DInterpG
  
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
  
  fKeyTrain$MskewPoly3DInterpA <- 3 * (fKeyTrain$MmeanPoly3DInterpA - fKeyTrain$MmedianPoly3DInterpA) / fKeyTrain$MsdPoly3DInterpA
  fKeyTrain$MskewPoly3DInterpG <- 3 * (fKeyTrain$MmeanPoly3DInterpG - fKeyTrain$MmedianPoly3DInterpG) / fKeyTrain$MsdPoly3DInterpG
  
  fKeyTrain$MskewCubInterpA <- 3 * (fKeyTrain$MmeanCubInterpA - fKeyTrain$MmedianCubInterpA) / fKeyTrain$MsdCubInterpA
  fKeyTrain$MskewCubInterpG <- 3 * (fKeyTrain$MmeanCubInterpG - fKeyTrain$MmedianCubInterpG) / fKeyTrain$MsdCubInterpG
  
  fKeyTrain$MskewMeanNormA <- 3 * (fKeyTrain$MmeanMeanNormA - fKeyTrain$MmedianMeanNormA) / fKeyTrain$MsdMeanNormA
  fKeyTrain$MskewMeanNormG <- 3 * (fKeyTrain$MmeanMeanNormG - fKeyTrain$MmedianMeanNormG) / fKeyTrain$MsdMeanNormG
  
  # fKeyTrain <- as.data.frame(fKeyTrain)
  
  # write files
  if(boolTraining) {
    write.csv(
      fKeyTrain,
      paste(getwd(),"/datasets/", stringFileTimestamp, "-feature-dataset-TRAINING-", wSize, ".csv", sep=""),
      row.names = FALSE
    )
    # # write extra feature set with no "NONE" values
    # ftest <- fKeyTrain[-which(fKeyTrain$Keypress == "NONE"),]
    # write.csv(
    #   ftest,
    #   paste(getwd(),"/datasets/", stringFileTimestamp, "-dataset-training-", wSize, ".csv", sep=""),
    #   row.names = FALSE
    # )
  } else if(boolLabeled) {
    write.csv(
      fKeyTrain,
      paste(getwd(),"/datasets/", stringFileTimestamp, "-feature-dataset-TEST-", wSize, ".csv", sep=""),
      row.names = FALSE
    )
  } else {
    write.csv(
      fKeyTrain,
      paste(getwd(),"/datasets/", stringFileTimestamp, "-feature-dataset-TEST-unlabeled-", wSize, ".csv", sep=""),
      row.names = FALSE
    )
  }
  
  end.time <- proc.time()
  print(paste("DONE in ", format(round(end.time[3] - start.time[3], 1), nsmall = 1),"s",sep=""))
  return(fKeyTrain)
}
