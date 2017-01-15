set.seed(123)
options(digits=20)

sensortrain <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\17011020-sensor-dataset-training-raw.csv", # 17011417, 17011205, 17011020, 16122802
    header = TRUE,
    stringsAsFactors=FALSE
  )

keytrain <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\17011020-key-dataset-training-raw.csv",
    header = TRUE,
    stringsAsFactors=FALSE
  )

for (i in seq_along(keytrain$DownTime)) {
  keytrain$id <- seq_along(keytrain$DownTime)
  keytrain$WindowSize[i] <- length(sensortrain$Timestamp[sensortrain$Timestamp >= keytrain$DownTime[i] & sensortrain$Timestamp <= keytrain$EventTime[i]])
}

write.csv(
  keytrain,
  "C:\\git\\data-thesis\\R\\datasets\\17011020-key-dataset-training-raw.csv",
  row.names = FALSE
)

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

# Square Sum of 3D vectors
sensortrain$SqSumA <- sensortrain$xA ^ 2 + sensortrain$yA ^ 2 + sensortrain$zA ^ 2
sensortrain$SqSumG <- sensortrain$xG^2 + sensortrain$yG^2 + sensortrain$zG^2
sensortrain$SqSumO <- sensortrain$alpha^2 + sensortrain$beta^2 + sensortrain$gamma^2

# Magnitude of 3D Vectors
sensortrain$MagnA <- sqrt(sensortrain$SqSumA)
sensortrain$MagnG <- sqrt(sensortrain$SqSumG)
sensortrain$MagnO <- sqrt(sensortrain$SqSumO)

# norm vector values
sensortrain$XnormMeanA <- sensortrain$xA - mean(sensortrain$xA)
sensortrain$YnormMeanA <- sensortrain$yA - mean(sensortrain$yA)
sensortrain$ZnormMeanA <- sensortrain$zA - mean(sensortrain$zA)
sensortrain$XnormMeanG <- sensortrain$xG - mean(sensortrain$xG)
sensortrain$YnormMeanG <- sensortrain$yG - mean(sensortrain$yG)
sensortrain$ZnormMeanG <- sensortrain$zG - mean(sensortrain$zG)
sensortrain$XnormMeanO <- sensortrain$alpha - mean(sensortrain$alpha)
sensortrain$YnormMeanO <- sensortrain$beta - mean(sensortrain$beta)
sensortrain$ZnormMeanO <- sensortrain$gamma - mean(sensortrain$gamma)

sensortrain$MnormMeanA <- sensortrain$MagnA - mean(sensortrain$MagnA)
sensortrain$MnormMeanG <- sensortrain$MagnG - mean(sensortrain$MagnG)
sensortrain$MnormMeanO <- sensortrain$MagnO - mean(sensortrain$MagnO)

write.csv(sensortrain,
          "C:\\git\\data-thesis\\R\\datasets\\17011417-sensor-dataset-training-preprocessed.csv",
          row.names = FALSE)

sensortrain$belongsToKey <- FALSE;

for (i in seq_along(keytrain$DownTime)) {
  for (j in sensortrain$Timestamp[sensortrain$Timestamp >= (keytrain$DownTime[i]) &
                              sensortrain$Timestamp <= (keytrain$EventTime[i])]) {
    sensortrain$belongsToKey[sensortrain$Timestamp == j] <- TRUE
  }
}


# wSize <- ceiling(max(keytrain$WindowSize)) * 2 + 1
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
    tmpRawAlphaWindow <- tmpWindowCopy$alpha
    tmpRawBetaWindow <- tmpWindowCopy$beta
    tmpRawGammaWindow <- tmpWindowCopy$gamma
    tmpRawMagnAWindow <- tmpWindowCopy$MagnA
    tmpRawMagnGWindow <- tmpWindowCopy$MagnG
    tmpRawMagnOWindow <- tmpWindowCopy$MagnO
    
    # linear interpolation
    linInterpN <- 201
    tmpRawXWindowLinInterp <- approx(tmpWindowCopy$Timestamp, tmpRawXWindow, n=linInterpN)$y
    tmpRawYWindowLinInterp <- approx(tmpWindowCopy$Timestamp, tmpRawYWindow, n=linInterpN)$y
    tmpRawZWindowLinInterp <- approx(tmpWindowCopy$Timestamp, tmpRawZWindow, n=linInterpN)$y
    tmpRawAWindowLinInterp <- approx(tmpWindowCopy$Timestamp, tmpRawAWindow, n=linInterpN)$y
    tmpRawBWindowLinInterp <- approx(tmpWindowCopy$Timestamp, tmpRawBWindow, n=linInterpN)$y
    tmpRawCWindowLinInterp <- approx(tmpWindowCopy$Timestamp, tmpRawAWindow, n=linInterpN)$y
    tmpRawAlphaWindowLinInterp <- approx(tmpWindowCopy$Timestamp, tmpRawAlphaWindow, n=linInterpN)$y
    tmpRawBetaWindowLinInterp <- approx(tmpWindowCopy$Timestamp, tmpRawBetaWindow, n=linInterpN)$y
    tmpRawGammaWindowLinInterp <- approx(tmpWindowCopy$Timestamp, tmpRawGammaWindow, n=linInterpN)$y
    tmpRawMagnAWindowLinInterp <- approx(tmpWindowCopy$Timestamp, tmpRawMagnAWindow, n=linInterpN)$y
    tmpRawMagnGWindowLinInterp <- approx(tmpWindowCopy$Timestamp, tmpRawMagnGWindow, n=linInterpN)$y
    tmpRawMagnOWindowLinInterp <- approx(tmpWindowCopy$Timestamp, tmpRawMagnOWindow, n=linInterpN)$y
    
    # polynom (3) interpol
    polInterpN <- 3
    
    tmpRawPolyMod <- lm(tmpRawXWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
    tmpRawXWindowPolInterp <- predict(tmpRawPolyMod)
    fKeyTrain$Xd0A[wIndex] <- tmpRawPolyMod$coefficients[1]
    fKeyTrain$Xd1A[wIndex] <- tmpRawPolyMod$coefficients[2]
    fKeyTrain$Xd2A[wIndex] <- tmpRawPolyMod$coefficients[3]
    fKeyTrain$Xd3A[wIndex] <- tmpRawPolyMod$coefficients[4]
    
    tmpRawPolyMod <- lm(tmpRawYWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
    tmpRawYWindowPolInterp <- predict(tmpRawPolyMod)
    fKeyTrain$Yd0A[wIndex] <- tmpRawPolyMod$coefficients[1]
    fKeyTrain$Yd1A[wIndex] <- tmpRawPolyMod$coefficients[2]
    fKeyTrain$Yd2A[wIndex] <- tmpRawPolyMod$coefficients[3]
    fKeyTrain$Yd3A[wIndex] <- tmpRawPolyMod$coefficients[4]
    
    tmpRawPolyMod <- lm(tmpRawZWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
    tmpRawZWindowPolInterp <- predict(tmpRawPolyMod)
    fKeyTrain$Zd0A[wIndex] <- tmpRawPolyMod$coefficients[1]
    fKeyTrain$Zd1A[wIndex] <- tmpRawPolyMod$coefficients[2]
    fKeyTrain$Zd2A[wIndex] <- tmpRawPolyMod$coefficients[3]
    fKeyTrain$Zd3A[wIndex] <- tmpRawPolyMod$coefficients[4]
    
    tmpRawPolyMod <- lm(tmpRawYWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
    tmpRawAWindowPolInterp <- predict(tmpRawPolyMod)
    fKeyTrain$Xd0G[wIndex] <- tmpRawPolyMod$coefficients[1]
    fKeyTrain$Xd1G[wIndex] <- tmpRawPolyMod$coefficients[2]
    fKeyTrain$Xd2G[wIndex] <- tmpRawPolyMod$coefficients[3]
    fKeyTrain$Xd3G[wIndex] <- tmpRawPolyMod$coefficients[4]
    
    tmpRawPolyMod <- lm(tmpRawBWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
    tmpRawBWindowPolInterp <- predict(tmpRawPolyMod)
    fKeyTrain$Yd0G[wIndex] <- tmpRawPolyMod$coefficients[1]
    fKeyTrain$Yd1G[wIndex] <- tmpRawPolyMod$coefficients[2]
    fKeyTrain$Yd2G[wIndex] <- tmpRawPolyMod$coefficients[3]
    fKeyTrain$Yd3G[wIndex] <- tmpRawPolyMod$coefficients[4]
    
    tmpRawPolyMod <- lm(tmpRawCWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
    tmpRawCWindowPolInterp <- predict(tmpRawPolyMod)
    fKeyTrain$Zd0G[wIndex] <- tmpRawPolyMod$coefficients[1]
    fKeyTrain$Zd1G[wIndex] <- tmpRawPolyMod$coefficients[2]
    fKeyTrain$Zd2G[wIndex] <- tmpRawPolyMod$coefficients[3]
    fKeyTrain$Zd3G[wIndex] <- tmpRawPolyMod$coefficients[4]
    
    tmpRawPolyMod <- lm(tmpRawAlphaWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
    tmpRawAlphaWindowPolInterp <- predict(tmpRawPolyMod)
    fKeyTrain$Xd0O[wIndex] <- tmpRawPolyMod$coefficients[1]
    fKeyTrain$Xd1O[wIndex] <- tmpRawPolyMod$coefficients[2]
    fKeyTrain$Xd2O[wIndex] <- tmpRawPolyMod$coefficients[3]
    fKeyTrain$Xd3O[wIndex] <- tmpRawPolyMod$coefficients[4]
    
    tmpRawPolyMod <- lm(tmpRawBetaWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
    tmpRawBetaWindowPolInterp <- predict(tmpRawPolyMod)
    fKeyTrain$Yd0O[wIndex] <- tmpRawPolyMod$coefficients[1]
    fKeyTrain$Yd1O[wIndex] <- tmpRawPolyMod$coefficients[2]
    fKeyTrain$Yd2O[wIndex] <- tmpRawPolyMod$coefficients[3]
    fKeyTrain$Yd3O[wIndex] <- tmpRawPolyMod$coefficients[4]
    
    tmpRawPolyMod <- lm(tmpRawGammaWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
    tmpRawGammaWindowPolInterp <- predict(tmpRawPolyMod)
    fKeyTrain$Zd0O[wIndex] <- tmpRawPolyMod$coefficients[1]
    fKeyTrain$Zd1O[wIndex] <- tmpRawPolyMod$coefficients[2]
    fKeyTrain$Zd2O[wIndex] <- tmpRawPolyMod$coefficients[3]
    fKeyTrain$Zd3O[wIndex] <- tmpRawPolyMod$coefficients[4]
    
    tmpRawPolyMod <- lm(tmpRawMagnAWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
    tmpRawMagnAWindowPolInterp <- predict(tmpRawPolyMod)
    fKeyTrain$Md0A[wIndex] <- tmpRawPolyMod$coefficients[1]
    fKeyTrain$Md1A[wIndex] <- tmpRawPolyMod$coefficients[2]
    fKeyTrain$Md2A[wIndex] <- tmpRawPolyMod$coefficients[3]
    fKeyTrain$Md3A[wIndex] <- tmpRawPolyMod$coefficients[4]
    
    tmpRawPolyMod <- lm(tmpRawMagnGWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
    tmpRawMagnGWindowPolInterp <- predict(tmpRawPolyMod)
    fKeyTrain$Md0G[wIndex] <- tmpRawPolyMod$coefficients[1]
    fKeyTrain$Md1G[wIndex] <- tmpRawPolyMod$coefficients[2]
    fKeyTrain$Md2G[wIndex] <- tmpRawPolyMod$coefficients[3]
    fKeyTrain$Md3G[wIndex] <- tmpRawPolyMod$coefficients[4]
    
    tmpRawPolyMod <- lm(tmpRawMagnOWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
    tmpRawMagnOWindowPolInterp <- predict(tmpRawPolyMod)
    fKeyTrain$Md0O[wIndex] <- tmpRawPolyMod$coefficients[1]
    fKeyTrain$Md1O[wIndex] <- tmpRawPolyMod$coefficients[2]
    fKeyTrain$Md2O[wIndex] <- tmpRawPolyMod$coefficients[3]
    fKeyTrain$Md3O[wIndex] <- tmpRawPolyMod$coefficients[4]
    
    # cubic spline interpol
    cubicInterpN <- 201
    spar <- 0.35
    tmpRawXWindowCubInterp <- predict(smooth.spline(tmpWindowCopy$Timestamp, tmpRawXWindow, spar=spar), seq(tmpWindowCopy$Timestamp[1], tmpWindowCopy$Timestamp[length(tmpWindowCopy$Timestamp)], length.out=cubicInterpN))$y
    tmpRawYWindowCubInterp <- predict(smooth.spline(tmpWindowCopy$Timestamp, tmpRawYWindow, spar=spar), seq(tmpWindowCopy$Timestamp[1], tmpWindowCopy$Timestamp[length(tmpWindowCopy$Timestamp)], length.out=cubicInterpN))$y
    tmpRawZWindowCubInterp <- predict(smooth.spline(tmpWindowCopy$Timestamp, tmpRawZWindow, spar=spar), seq(tmpWindowCopy$Timestamp[1], tmpWindowCopy$Timestamp[length(tmpWindowCopy$Timestamp)], length.out=cubicInterpN))$y
    tmpRawAWindowCubInterp <- predict(smooth.spline(tmpWindowCopy$Timestamp, tmpRawAWindow, spar=spar), seq(tmpWindowCopy$Timestamp[1], tmpWindowCopy$Timestamp[length(tmpWindowCopy$Timestamp)], length.out=cubicInterpN))$y
    tmpRawBWindowCubInterp <- predict(smooth.spline(tmpWindowCopy$Timestamp, tmpRawBWindow, spar=spar), seq(tmpWindowCopy$Timestamp[1], tmpWindowCopy$Timestamp[length(tmpWindowCopy$Timestamp)], length.out=cubicInterpN))$y
    tmpRawCWindowCubInterp <- predict(smooth.spline(tmpWindowCopy$Timestamp, tmpRawCWindow, spar=spar), seq(tmpWindowCopy$Timestamp[1], tmpWindowCopy$Timestamp[length(tmpWindowCopy$Timestamp)], length.out=cubicInterpN))$y
    tmpRawAlphaWindowCubInterp <- predict(smooth.spline(tmpWindowCopy$Timestamp, tmpRawAlphaWindow, spar=spar), seq(tmpWindowCopy$Timestamp[1], tmpWindowCopy$Timestamp[length(tmpWindowCopy$Timestamp)], length.out=cubicInterpN))$y
    tmpRawBetaWindowCubInterp <- predict(smooth.spline(tmpWindowCopy$Timestamp, tmpRawBetaWindow, spar=spar), seq(tmpWindowCopy$Timestamp[1], tmpWindowCopy$Timestamp[length(tmpWindowCopy$Timestamp)], length.out=cubicInterpN))$y
    tmpRawGammaWindowCubInterp <- predict(smooth.spline(tmpWindowCopy$Timestamp, tmpRawGammaWindow, spar=spar), seq(tmpWindowCopy$Timestamp[1], tmpWindowCopy$Timestamp[length(tmpWindowCopy$Timestamp)], length.out=cubicInterpN))$y
    tmpRawMagnAWindowCubInterp <- predict(smooth.spline(tmpWindowCopy$Timestamp, tmpRawMagnAWindow, spar=spar), seq(tmpWindowCopy$Timestamp[1], tmpWindowCopy$Timestamp[length(tmpWindowCopy$Timestamp)], length.out=cubicInterpN))$y
    tmpRawMagnGWindowCubInterp <- predict(smooth.spline(tmpWindowCopy$Timestamp, tmpRawMagnGWindow, spar=spar), seq(tmpWindowCopy$Timestamp[1], tmpWindowCopy$Timestamp[length(tmpWindowCopy$Timestamp)], length.out=cubicInterpN))$y
    tmpRawMagnOWindowCubInterp <- predict(smooth.spline(tmpWindowCopy$Timestamp, tmpRawMagnOWindow, spar=spar), seq(tmpWindowCopy$Timestamp[1], tmpWindowCopy$Timestamp[length(tmpWindowCopy$Timestamp)], length.out=cubicInterpN))$y
    
    # mean norm
    tmpRawXWindowMeanNorm <- tmpWindowCopy$XnormMeanA
    tmpRawYWindowMeanNorm <- tmpWindowCopy$YnormMeanA
    tmpRawZWindowMeanNorm <- tmpWindowCopy$ZnormMeanA
    tmpRawAWindowMeanNorm <- tmpWindowCopy$XnormMeanG
    tmpRawBWindowMeanNorm <- tmpWindowCopy$YnormMeanG
    tmpRawCWindowMeanNorm <- tmpWindowCopy$ZnormMeanG
    tmpRawAlphaWindowMeanNorm <- tmpWindowCopy$XnormMeanO
    tmpRawBetaWindowMeanNorm <- tmpWindowCopy$YnormMeanO
    tmpRawGammaWindowMeanNorm <- tmpWindowCopy$ZnormMeanO
    tmpRawMagnAWindowMeanNorm <- tmpWindowCopy$MnormMeanA
    tmpRawMagnGWindowMeanNorm <- tmpWindowCopy$MnormMeanG
    tmpRawMagnOWindowMeanNorm <- tmpWindowCopy$MnormMeanO
    
    # Min Accelerometer
    fKeyTrain$XminA[wIndex] <- min(tmpRawXWindow)
    fKeyTrain$YminA[wIndex] <- min(tmpRawYWindow)
    fKeyTrain$ZminA[wIndex] <- min(tmpRawZWindow)
    fKeyTrain$XminG[wIndex] <- min(tmpRawAWindow)
    fKeyTrain$YminG[wIndex] <- min(tmpRawBWindow)
    fKeyTrain$ZminG[wIndex] <- min(tmpRawCWindow)
    fKeyTrain$XminO[wIndex] <- min(tmpRawAlphaWindow)
    fKeyTrain$YminO[wIndex] <- min(tmpRawBetaWindow)
    fKeyTrain$ZminO[wIndex] <- min(tmpRawGammaWindow)
    
    fKeyTrain$XminLinInterpA[wIndex] <- min(tmpRawXWindowLinInterp)
    fKeyTrain$YminLinInterpA[wIndex] <- min(tmpRawYWindowLinInterp)
    fKeyTrain$ZminLinInterpA[wIndex] <- min(tmpRawZWindowLinInterp)
    fKeyTrain$XminLinInterpG[wIndex] <- min(tmpRawAWindowLinInterp)
    fKeyTrain$YminLinInterpG[wIndex] <- min(tmpRawBWindowLinInterp)
    fKeyTrain$ZminLinInterpG[wIndex] <- min(tmpRawCWindowLinInterp)
    fKeyTrain$XminLinInterpO[wIndex] <- min(tmpRawAlphaWindowLinInterp)
    fKeyTrain$YminLinInterpO[wIndex] <- min(tmpRawBetaWindowLinInterp)
    fKeyTrain$ZminLinInterpO[wIndex] <- min(tmpRawGammaWindowLinInterp)
    
    fKeyTrain$XminPolInterpA[wIndex] <- min(tmpRawXWindowPolInterp)
    fKeyTrain$YminPolInterpA[wIndex] <- min(tmpRawYWindowPolInterp)
    fKeyTrain$ZminPolInterpA[wIndex] <- min(tmpRawZWindowPolInterp)
    fKeyTrain$XminPolInterpG[wIndex] <- min(tmpRawAWindowPolInterp)
    fKeyTrain$YminPolInterpG[wIndex] <- min(tmpRawBWindowPolInterp)
    fKeyTrain$ZminPolInterpG[wIndex] <- min(tmpRawCWindowPolInterp)
    fKeyTrain$XminPolInterpO[wIndex] <- min(tmpRawAlphaWindowPolInterp)
    fKeyTrain$YminPolInterpO[wIndex] <- min(tmpRawBetaWindowPolInterp)
    fKeyTrain$ZminPolInterpO[wIndex] <- min(tmpRawGammaWindowPolInterp)
    
    fKeyTrain$XminCubInterpA[wIndex] <- min(tmpRawXWindowCubInterp)
    fKeyTrain$YminCubInterpA[wIndex] <- min(tmpRawYWindowCubInterp)
    fKeyTrain$ZminCubInterpA[wIndex] <- min(tmpRawZWindowCubInterp)
    fKeyTrain$XminCubInterpG[wIndex] <- min(tmpRawAWindowCubInterp)
    fKeyTrain$YminCubInterpG[wIndex] <- min(tmpRawBWindowCubInterp)
    fKeyTrain$ZminCubInterpG[wIndex] <- min(tmpRawCWindowCubInterp)
    fKeyTrain$XminCubInterpO[wIndex] <- min(tmpRawAlphaWindowCubInterp)
    fKeyTrain$YminCubInterpO[wIndex] <- min(tmpRawBetaWindowCubInterp)
    fKeyTrain$ZminCubInterpO[wIndex] <- min(tmpRawGammaWindowCubInterp)
    
    fKeyTrain$XminMeanNormA[wIndex] <- min(tmpRawXWindowMeanNorm)
    fKeyTrain$YminMeanNormA[wIndex] <- min(tmpRawYWindowMeanNorm)
    fKeyTrain$ZminMeanNormA[wIndex] <- min(tmpRawZWindowMeanNorm)
    fKeyTrain$XminMeanNormG[wIndex] <- min(tmpRawAWindowMeanNorm)
    fKeyTrain$YminMeanNormG[wIndex] <- min(tmpRawBWindowMeanNorm)
    fKeyTrain$ZminMeanNormG[wIndex] <- min(tmpRawCWindowMeanNorm)
    fKeyTrain$XminMeanNormO[wIndex] <- min(tmpRawAlphaWindowMeanNorm)
    fKeyTrain$YminMeanNormO[wIndex] <- min(tmpRawBetaWindowMeanNorm)
    fKeyTrain$ZminMeanNormO[wIndex] <- min(tmpRawGammaWindowMeanNorm)
    
    #min magnitude
    fKeyTrain$MminA[wIndex] <- min(tmpRawMagnAWindow)
    fKeyTrain$MminG[wIndex] <- min(tmpRawMagnGWindow)
    fKeyTrain$MminO[wIndex] <- min(tmpRawMagnOWindow)
    
    fKeyTrain$MminLinInterpA[wIndex] <- min(tmpRawMagnAWindowLinInterp)
    fKeyTrain$MminLinInterpG[wIndex] <- min(tmpRawMagnGWindowLinInterp)
    fKeyTrain$MminLinInterpO[wIndex] <- min(tmpRawMagnOWindowLinInterp)
    
    fKeyTrain$MminPolInterpA[wIndex] <- min(tmpRawMagnAWindowPolInterp)
    fKeyTrain$MminPolInterpG[wIndex] <- min(tmpRawMagnGWindowPolInterp)
    fKeyTrain$MminPolInterpO[wIndex] <- min(tmpRawMagnOWindowPolInterp)
    
    fKeyTrain$MminCubInterpA[wIndex] <- min(tmpRawMagnAWindowCubInterp)
    fKeyTrain$MminCubInterpG[wIndex] <- min(tmpRawMagnGWindowCubInterp)
    fKeyTrain$MminCubInterpO[wIndex] <- min(tmpRawMagnOWindowCubInterp)
    
    fKeyTrain$MminMeanNormA[wIndex] <- min(tmpRawMagnAWindowMeanNorm)
    fKeyTrain$MminMeanNormG[wIndex] <- min(tmpRawMagnGWindowMeanNorm)
    fKeyTrain$MminMeanNormO[wIndex] <- min(tmpRawMagnOWindowMeanNorm)
    
    # Max
    fKeyTrain$XmaxA[wIndex] <- max(tmpRawXWindow)
    fKeyTrain$YmaxA[wIndex] <- max(tmpRawYWindow)
    fKeyTrain$ZmaxA[wIndex] <- max(tmpRawZWindow)
    fKeyTrain$XmaxG[wIndex] <- max(tmpRawAWindow)
    fKeyTrain$YmaxG[wIndex] <- max(tmpRawBWindow)
    fKeyTrain$ZmaxG[wIndex] <- max(tmpRawCWindow)
    fKeyTrain$XmaxO[wIndex] <- max(tmpRawAlphaWindow)
    fKeyTrain$YmaxO[wIndex] <- max(tmpRawBetaWindow)
    fKeyTrain$ZmaxO[wIndex] <- max(tmpRawGammaWindow)
    
    fKeyTrain$XmaxLinInterpA[wIndex] <- max(tmpRawXWindowLinInterp)
    fKeyTrain$YmaxLinInterpA[wIndex] <- max(tmpRawYWindowLinInterp)
    fKeyTrain$ZmaxLinInterpA[wIndex] <- max(tmpRawZWindowLinInterp)
    fKeyTrain$XmaxLinInterpG[wIndex] <- max(tmpRawAWindowLinInterp)
    fKeyTrain$YmaxLinInterpG[wIndex] <- max(tmpRawBWindowLinInterp)
    fKeyTrain$ZmaxLinInterpG[wIndex] <- max(tmpRawCWindowLinInterp)
    fKeyTrain$XmaxLinInterpO[wIndex] <- max(tmpRawAlphaWindowLinInterp)
    fKeyTrain$YmaxLinInterpO[wIndex] <- max(tmpRawBetaWindowLinInterp)
    fKeyTrain$ZmaxLinInterpO[wIndex] <- max(tmpRawGammaWindowLinInterp)
    
    fKeyTrain$XmaxPolInterpA[wIndex] <- max(tmpRawXWindowPolInterp)
    fKeyTrain$YmaxPolInterpA[wIndex] <- max(tmpRawYWindowPolInterp)
    fKeyTrain$ZmaxPolInterpA[wIndex] <- max(tmpRawZWindowPolInterp)
    fKeyTrain$XmaxPolInterpG[wIndex] <- max(tmpRawAWindowPolInterp)
    fKeyTrain$YmaxPolInterpG[wIndex] <- max(tmpRawBWindowPolInterp)
    fKeyTrain$ZmaxPolInterpG[wIndex] <- max(tmpRawCWindowPolInterp)
    fKeyTrain$XmaxPolInterpO[wIndex] <- max(tmpRawAlphaWindowPolInterp)
    fKeyTrain$YmaxPolInterpO[wIndex] <- max(tmpRawBetaWindowPolInterp)
    fKeyTrain$ZmaxPolInterpO[wIndex] <- max(tmpRawGammaWindowPolInterp)
    
    fKeyTrain$XmaxCubInterpA[wIndex] <- max(tmpRawXWindowCubInterp)
    fKeyTrain$YmaxCubInterpA[wIndex] <- max(tmpRawYWindowCubInterp)
    fKeyTrain$ZmaxCubInterpA[wIndex] <- max(tmpRawZWindowCubInterp)
    fKeyTrain$XmaxCubInterpG[wIndex] <- max(tmpRawAWindowCubInterp)
    fKeyTrain$YmaxCubInterpG[wIndex] <- max(tmpRawBWindowCubInterp)
    fKeyTrain$ZmaxCubInterpG[wIndex] <- max(tmpRawCWindowCubInterp)
    fKeyTrain$XmaxCubInterpO[wIndex] <- max(tmpRawAlphaWindowCubInterp)
    fKeyTrain$YmaxCubInterpO[wIndex] <- max(tmpRawBetaWindowCubInterp)
    fKeyTrain$ZmaxCubInterpO[wIndex] <- max(tmpRawGammaWindowCubInterp)
    
    fKeyTrain$XmaxMeanNormA[wIndex] <- max(tmpRawXWindowMeanNorm)
    fKeyTrain$YmaxMeanNormA[wIndex] <- max(tmpRawYWindowMeanNorm)
    fKeyTrain$ZmaxMeanNormA[wIndex] <- max(tmpRawZWindowMeanNorm)
    fKeyTrain$XmaxMeanNormG[wIndex] <- max(tmpRawAWindowMeanNorm)
    fKeyTrain$YmaxMeanNormG[wIndex] <- max(tmpRawBWindowMeanNorm)
    fKeyTrain$ZmaxMeanNormG[wIndex] <- max(tmpRawCWindowMeanNorm)
    fKeyTrain$XmaxMeanNormO[wIndex] <- max(tmpRawAlphaWindowMeanNorm)
    fKeyTrain$YmaxMeanNormO[wIndex] <- max(tmpRawBetaWindowMeanNorm)
    fKeyTrain$ZmaxMeanNormO[wIndex] <- max(tmpRawGammaWindowMeanNorm)
    
    #max magnitude
    fKeyTrain$MmaxA[wIndex] <- max(tmpRawMagnAWindow)
    fKeyTrain$MmaxG[wIndex] <- max(tmpRawMagnGWindow)
    fKeyTrain$MmaxO[wIndex] <- max(tmpRawMagnOWindow)
    
    fKeyTrain$MmaxLinInterpA[wIndex] <- max(tmpRawMagnAWindowLinInterp)
    fKeyTrain$MmaxLinInterpG[wIndex] <- max(tmpRawMagnGWindowLinInterp)
    fKeyTrain$MmaxLinInterpO[wIndex] <- max(tmpRawMagnOWindowLinInterp)
    
    fKeyTrain$MmaxPolInterpA[wIndex] <- max(tmpRawMagnAWindowPolInterp)
    fKeyTrain$MmaxPolInterpG[wIndex] <- max(tmpRawMagnGWindowPolInterp)
    fKeyTrain$MmaxPolInterpO[wIndex] <- max(tmpRawMagnOWindowPolInterp)
    
    fKeyTrain$MmaxCubInterpA[wIndex] <- max(tmpRawMagnAWindowCubInterp)
    fKeyTrain$MmaxCubInterpG[wIndex] <- max(tmpRawMagnGWindowCubInterp)
    fKeyTrain$MmaxCubInterpO[wIndex] <- max(tmpRawMagnOWindowCubInterp)
    
    fKeyTrain$MmaxMeanNormA[wIndex] <- max(tmpRawMagnAWindowMeanNorm)
    fKeyTrain$MmaxMeanNormG[wIndex] <- max(tmpRawMagnGWindowMeanNorm)
    fKeyTrain$MmaxMeanNormO[wIndex] <- max(tmpRawMagnOWindowMeanNorm)
    
    # Mean
    fKeyTrain$XmeanA[wIndex] <- mean(tmpRawXWindow)
    fKeyTrain$YmeanA[wIndex] <- mean(tmpRawYWindow)
    fKeyTrain$ZmeanA[wIndex] <- mean(tmpRawZWindow)
    fKeyTrain$XmeanG[wIndex] <- mean(tmpRawAWindow)
    fKeyTrain$YmeanG[wIndex] <- mean(tmpRawBWindow)
    fKeyTrain$ZmeanG[wIndex] <- mean(tmpRawCWindow)
    fKeyTrain$XmeanO[wIndex] <- mean(tmpRawAlphaWindow)
    fKeyTrain$YmeanO[wIndex] <- mean(tmpRawBetaWindow)
    fKeyTrain$ZmeanO[wIndex] <- mean(tmpRawGammaWindow)
    
    fKeyTrain$XmeanLinInterpA[wIndex] <- mean(tmpRawXWindowLinInterp)
    fKeyTrain$YmeanLinInterpA[wIndex] <- mean(tmpRawYWindowLinInterp)
    fKeyTrain$ZmeanLinInterpA[wIndex] <- mean(tmpRawZWindowLinInterp)
    fKeyTrain$XmeanLinInterpG[wIndex] <- mean(tmpRawAWindowLinInterp)
    fKeyTrain$YmeanLinInterpG[wIndex] <- mean(tmpRawBWindowLinInterp)
    fKeyTrain$ZmeanLinInterpG[wIndex] <- mean(tmpRawCWindowLinInterp)
    fKeyTrain$XmeanLinInterpO[wIndex] <- mean(tmpRawAlphaWindowLinInterp)
    fKeyTrain$YmeanLinInterpO[wIndex] <- mean(tmpRawBetaWindowLinInterp)
    fKeyTrain$ZmeanLinInterpO[wIndex] <- mean(tmpRawGammaWindowLinInterp)
    
    fKeyTrain$XmeanPolInterpA[wIndex] <- mean(tmpRawXWindowPolInterp)
    fKeyTrain$YmeanPolInterpA[wIndex] <- mean(tmpRawYWindowPolInterp)
    fKeyTrain$ZmeanPolInterpA[wIndex] <- mean(tmpRawZWindowPolInterp)
    fKeyTrain$XmeanPolInterpG[wIndex] <- mean(tmpRawAWindowPolInterp)
    fKeyTrain$YmeanPolInterpG[wIndex] <- mean(tmpRawBWindowPolInterp)
    fKeyTrain$ZmeanPolInterpG[wIndex] <- mean(tmpRawCWindowPolInterp)
    fKeyTrain$XmeanPolInterpO[wIndex] <- mean(tmpRawAlphaWindowPolInterp)
    fKeyTrain$YmeanPolInterpO[wIndex] <- mean(tmpRawBetaWindowPolInterp)
    fKeyTrain$ZmeanPolInterpO[wIndex] <- mean(tmpRawGammaWindowPolInterp)
    
    fKeyTrain$XmeanCubInterpA[wIndex] <- mean(tmpRawXWindowCubInterp)
    fKeyTrain$YmeanCubInterpA[wIndex] <- mean(tmpRawYWindowCubInterp)
    fKeyTrain$ZmeanCubInterpA[wIndex] <- mean(tmpRawZWindowCubInterp)
    fKeyTrain$XmeanCubInterpG[wIndex] <- mean(tmpRawAWindowCubInterp)
    fKeyTrain$YmeanCubInterpG[wIndex] <- mean(tmpRawBWindowCubInterp)
    fKeyTrain$ZmeanCubInterpG[wIndex] <- mean(tmpRawCWindowCubInterp)
    fKeyTrain$XmeanCubInterpO[wIndex] <- mean(tmpRawAlphaWindowCubInterp)
    fKeyTrain$YmeanCubInterpO[wIndex] <- mean(tmpRawBetaWindowCubInterp)
    fKeyTrain$ZmeanCubInterpO[wIndex] <- mean(tmpRawGammaWindowCubInterp)
    
    fKeyTrain$XmeanMeanNormA[wIndex] <- mean(tmpRawXWindowMeanNorm)
    fKeyTrain$YmeanMeanNormA[wIndex] <- mean(tmpRawYWindowMeanNorm)
    fKeyTrain$ZmeanMeanNormA[wIndex] <- mean(tmpRawZWindowMeanNorm)
    fKeyTrain$XmeanMeanNormG[wIndex] <- mean(tmpRawAWindowMeanNorm)
    fKeyTrain$YmeanMeanNormG[wIndex] <- mean(tmpRawBWindowMeanNorm)
    fKeyTrain$ZmeanMeanNormG[wIndex] <- mean(tmpRawCWindowMeanNorm)
    fKeyTrain$XmeanMeanNormO[wIndex] <- mean(tmpRawAlphaWindowMeanNorm)
    fKeyTrain$YmeanMeanNormO[wIndex] <- mean(tmpRawBetaWindowMeanNorm)
    fKeyTrain$ZmeanMeanNormO[wIndex] <- mean(tmpRawGammaWindowMeanNorm)
    
    # mean magn
    fKeyTrain$MmeanA[wIndex] <- mean(tmpRawMagnAWindow)
    fKeyTrain$MmeanG[wIndex] <- mean(tmpRawMagnGWindow)
    fKeyTrain$MmeanO[wIndex] <- mean(tmpRawMagnOWindow)
    
    fKeyTrain$MmeanLinInterpA[wIndex] <- mean(tmpRawMagnAWindowLinInterp)
    fKeyTrain$MmeanLinInterpG[wIndex] <- mean(tmpRawMagnGWindowLinInterp)
    fKeyTrain$MmeanLinInterpO[wIndex] <- mean(tmpRawMagnOWindowLinInterp)
    
    fKeyTrain$MmeanPolInterpA[wIndex] <- mean(tmpRawMagnAWindowPolInterp)
    fKeyTrain$MmeanPolInterpG[wIndex] <- mean(tmpRawMagnGWindowPolInterp)
    fKeyTrain$MmeanPolInterpO[wIndex] <- mean(tmpRawMagnOWindowPolInterp)
    
    fKeyTrain$MmeanCubInterpA[wIndex] <- mean(tmpRawMagnAWindowCubInterp)
    fKeyTrain$MmeanCubInterpG[wIndex] <- mean(tmpRawMagnGWindowCubInterp)
    fKeyTrain$MmeanCubInterpO[wIndex] <- mean(tmpRawMagnOWindowCubInterp)
    
    fKeyTrain$MmeanMeanNormA[wIndex] <- mean(tmpRawMagnAWindowMeanNorm)
    fKeyTrain$MmeanMeanNormG[wIndex] <- mean(tmpRawMagnGWindowMeanNorm)
    fKeyTrain$MmeanMeanNormO[wIndex] <- mean(tmpRawMagnOWindowMeanNorm)
    
    # Median
    fKeyTrain$XmedianA[wIndex] <- median(tmpRawXWindow)
    fKeyTrain$YmedianA[wIndex] <- median(tmpRawYWindow)
    fKeyTrain$ZmedianA[wIndex] <- median(tmpRawZWindow)
    fKeyTrain$XmedianG[wIndex] <- median(tmpRawAWindow)
    fKeyTrain$YmedianG[wIndex] <- median(tmpRawBWindow)
    fKeyTrain$ZmedianG[wIndex] <- median(tmpRawCWindow)
    fKeyTrain$XmedianO[wIndex] <- median(tmpRawAlphaWindow)
    fKeyTrain$YmedianO[wIndex] <- median(tmpRawBetaWindow)
    fKeyTrain$ZmedianO[wIndex] <- median(tmpRawGammaWindow)
    
    fKeyTrain$XmedianLinInterpA[wIndex] <- median(tmpRawXWindowLinInterp)
    fKeyTrain$YmedianLinInterpA[wIndex] <- median(tmpRawYWindowLinInterp)
    fKeyTrain$ZmedianLinInterpA[wIndex] <- median(tmpRawZWindowLinInterp)
    fKeyTrain$XmedianLinInterpG[wIndex] <- median(tmpRawAWindowLinInterp)
    fKeyTrain$YmedianLinInterpG[wIndex] <- median(tmpRawBWindowLinInterp)
    fKeyTrain$ZmedianLinInterpG[wIndex] <- median(tmpRawCWindowLinInterp)
    fKeyTrain$XmedianLinInterpO[wIndex] <- median(tmpRawAlphaWindowLinInterp)
    fKeyTrain$YmedianLinInterpO[wIndex] <- median(tmpRawBetaWindowLinInterp)
    fKeyTrain$ZmedianLinInterpO[wIndex] <- median(tmpRawGammaWindowLinInterp)
    
    fKeyTrain$XmedianPolInterpA[wIndex] <- median(tmpRawXWindowPolInterp)
    fKeyTrain$YmedianPolInterpA[wIndex] <- median(tmpRawYWindowPolInterp)
    fKeyTrain$ZmedianPolInterpA[wIndex] <- median(tmpRawZWindowPolInterp)
    fKeyTrain$XmedianPolInterpG[wIndex] <- median(tmpRawAWindowPolInterp)
    fKeyTrain$YmedianPolInterpG[wIndex] <- median(tmpRawBWindowPolInterp)
    fKeyTrain$ZmedianPolInterpG[wIndex] <- median(tmpRawCWindowPolInterp)
    fKeyTrain$XmedianPolInterpO[wIndex] <- median(tmpRawAlphaWindowPolInterp)
    fKeyTrain$YmedianPolInterpO[wIndex] <- median(tmpRawBetaWindowPolInterp)
    fKeyTrain$ZmedianPolInterpO[wIndex] <- median(tmpRawGammaWindowPolInterp)
    
    fKeyTrain$XmedianCubInterpA[wIndex] <- median(tmpRawXWindowCubInterp)
    fKeyTrain$YmedianCubInterpA[wIndex] <- median(tmpRawYWindowCubInterp)
    fKeyTrain$ZmedianCubInterpA[wIndex] <- median(tmpRawZWindowCubInterp)
    fKeyTrain$XmedianCubInterpG[wIndex] <- median(tmpRawAWindowCubInterp)
    fKeyTrain$YmedianCubInterpG[wIndex] <- median(tmpRawBWindowCubInterp)
    fKeyTrain$ZmedianCubInterpG[wIndex] <- median(tmpRawCWindowCubInterp)
    fKeyTrain$XmedianCubInterpO[wIndex] <- median(tmpRawAlphaWindowCubInterp)
    fKeyTrain$YmedianCubInterpO[wIndex] <- median(tmpRawBetaWindowCubInterp)
    fKeyTrain$ZmedianCubInterpO[wIndex] <- median(tmpRawGammaWindowCubInterp)
    
    fKeyTrain$XmedianMeanNormA[wIndex] <- median(tmpRawXWindowMeanNorm)
    fKeyTrain$YmedianMeanNormA[wIndex] <- median(tmpRawYWindowMeanNorm)
    fKeyTrain$ZmedianMeanNormA[wIndex] <- median(tmpRawZWindowMeanNorm)
    fKeyTrain$XmedianMeanNormG[wIndex] <- median(tmpRawAWindowMeanNorm)
    fKeyTrain$YmedianMeanNormG[wIndex] <- median(tmpRawBWindowMeanNorm)
    fKeyTrain$ZmedianMeanNormG[wIndex] <- median(tmpRawCWindowMeanNorm)
    fKeyTrain$XmedianMeanNormO[wIndex] <- median(tmpRawAlphaWindowMeanNorm)
    fKeyTrain$YmedianMeanNormO[wIndex] <- median(tmpRawBetaWindowMeanNorm)
    fKeyTrain$ZmedianMeanNormO[wIndex] <- median(tmpRawGammaWindowMeanNorm)
    
    # median magn
    fKeyTrain$MmedianA[wIndex] <- median(tmpRawMagnAWindow)
    fKeyTrain$MmedianG[wIndex] <- median(tmpRawMagnGWindow)
    fKeyTrain$MmedianO[wIndex] <- median(tmpRawMagnOWindow)
    
    fKeyTrain$MmedianLinInterpA[wIndex] <- median(tmpRawMagnAWindowLinInterp)
    fKeyTrain$MmedianLinInterpG[wIndex] <- median(tmpRawMagnGWindowLinInterp)
    fKeyTrain$MmedianLinInterpO[wIndex] <- median(tmpRawMagnOWindowLinInterp)
    
    fKeyTrain$MmedianPolInterpA[wIndex] <- median(tmpRawMagnAWindowPolInterp)
    fKeyTrain$MmedianPolInterpG[wIndex] <- median(tmpRawMagnGWindowPolInterp)
    fKeyTrain$MmedianPolInterpO[wIndex] <- median(tmpRawMagnOWindowPolInterp)
    
    fKeyTrain$MmedianCubInterpA[wIndex] <- median(tmpRawMagnAWindowCubInterp)
    fKeyTrain$MmedianCubInterpG[wIndex] <- median(tmpRawMagnGWindowCubInterp)
    fKeyTrain$MmedianCubInterpO[wIndex] <- median(tmpRawMagnOWindowCubInterp)
    
    fKeyTrain$MmedianMeanNormA[wIndex] <- median(tmpRawMagnAWindowMeanNorm)
    fKeyTrain$MmedianMeanNormG[wIndex] <- median(tmpRawMagnGWindowMeanNorm)
    fKeyTrain$MmedianMeanNormO[wIndex] <- median(tmpRawMagnOWindowMeanNorm)
    
    # Standard Deviation
    fKeyTrain$XsdA[wIndex] <- sd(tmpRawXWindow)
    fKeyTrain$YsdA[wIndex] <- sd(tmpRawYWindow)
    fKeyTrain$ZsdA[wIndex] <- sd(tmpRawZWindow)
    fKeyTrain$XsdG[wIndex] <- sd(tmpRawAWindow)
    fKeyTrain$YsdG[wIndex] <- sd(tmpRawBWindow)
    fKeyTrain$ZsdG[wIndex] <- sd(tmpRawCWindow)
    fKeyTrain$XsdO[wIndex] <- sd(tmpRawAlphaWindow)
    fKeyTrain$YsdO[wIndex] <- sd(tmpRawBetaWindow)
    fKeyTrain$ZsdO[wIndex] <- sd(tmpRawGammaWindow)
    
    fKeyTrain$XsdLinInterpA[wIndex] <- sd(tmpRawXWindowLinInterp)
    fKeyTrain$YsdLinInterpA[wIndex] <- sd(tmpRawYWindowLinInterp)
    fKeyTrain$ZsdLinInterpA[wIndex] <- sd(tmpRawZWindowLinInterp)
    fKeyTrain$XsdLinInterpG[wIndex] <- sd(tmpRawAWindowLinInterp)
    fKeyTrain$YsdLinInterpG[wIndex] <- sd(tmpRawBWindowLinInterp)
    fKeyTrain$ZsdLinInterpG[wIndex] <- sd(tmpRawCWindowLinInterp)
    fKeyTrain$XsdLinInterpO[wIndex] <- sd(tmpRawAlphaWindowLinInterp)
    fKeyTrain$YsdLinInterpO[wIndex] <- sd(tmpRawBetaWindowLinInterp)
    fKeyTrain$ZsdLinInterpO[wIndex] <- sd(tmpRawGammaWindowLinInterp)
    
    fKeyTrain$XsdPolInterpA[wIndex] <- sd(tmpRawXWindowPolInterp)
    fKeyTrain$YsdPolInterpA[wIndex] <- sd(tmpRawYWindowPolInterp)
    fKeyTrain$ZsdPolInterpA[wIndex] <- sd(tmpRawZWindowPolInterp)
    fKeyTrain$XsdPolInterpG[wIndex] <- sd(tmpRawAWindowPolInterp)
    fKeyTrain$YsdPolInterpG[wIndex] <- sd(tmpRawBWindowPolInterp)
    fKeyTrain$ZsdPolInterpG[wIndex] <- sd(tmpRawCWindowPolInterp)
    fKeyTrain$XsdPolInterpO[wIndex] <- sd(tmpRawAlphaWindowPolInterp)
    fKeyTrain$YsdPolInterpO[wIndex] <- sd(tmpRawBetaWindowPolInterp)
    fKeyTrain$ZsdPolInterpO[wIndex] <- sd(tmpRawGammaWindowPolInterp)
    
    fKeyTrain$XsdCubInterpA[wIndex] <- sd(tmpRawXWindowCubInterp)
    fKeyTrain$YsdCubInterpA[wIndex] <- sd(tmpRawYWindowCubInterp)
    fKeyTrain$ZsdCubInterpA[wIndex] <- sd(tmpRawZWindowCubInterp)
    fKeyTrain$XsdCubInterpG[wIndex] <- sd(tmpRawAWindowCubInterp)
    fKeyTrain$YsdCubInterpG[wIndex] <- sd(tmpRawBWindowCubInterp)
    fKeyTrain$ZsdCubInterpG[wIndex] <- sd(tmpRawCWindowCubInterp)
    fKeyTrain$XsdCubInterpO[wIndex] <- sd(tmpRawAlphaWindowCubInterp)
    fKeyTrain$YsdCubInterpO[wIndex] <- sd(tmpRawBetaWindowCubInterp)
    fKeyTrain$ZsdCubInterpO[wIndex] <- sd(tmpRawGammaWindowCubInterp)
    
    fKeyTrain$XsdMeanNormA[wIndex] <- sd(tmpRawXWindowMeanNorm)
    fKeyTrain$YsdMeanNormA[wIndex] <- sd(tmpRawYWindowMeanNorm)
    fKeyTrain$ZsdMeanNormA[wIndex] <- sd(tmpRawZWindowMeanNorm)
    fKeyTrain$XsdMeanNormG[wIndex] <- sd(tmpRawAWindowMeanNorm)
    fKeyTrain$YsdMeanNormG[wIndex] <- sd(tmpRawBWindowMeanNorm)
    fKeyTrain$ZsdMeanNormG[wIndex] <- sd(tmpRawCWindowMeanNorm)
    fKeyTrain$XsdMeanNormO[wIndex] <- sd(tmpRawAlphaWindowMeanNorm)
    fKeyTrain$YsdMeanNormO[wIndex] <- sd(tmpRawBetaWindowMeanNorm)
    fKeyTrain$ZsdMeanNormO[wIndex] <- sd(tmpRawGammaWindowMeanNorm)
    
    #sd magn
    fKeyTrain$MsdA[wIndex] <- sd(tmpRawMagnAWindow)
    fKeyTrain$MsdG[wIndex] <- sd(tmpRawMagnGWindow)
    fKeyTrain$MsdO[wIndex] <- sd(tmpRawMagnOWindow)
    
    fKeyTrain$MsdLinInterpA[wIndex] <- sd(tmpRawMagnAWindowLinInterp)
    fKeyTrain$MsdLinInterpG[wIndex] <- sd(tmpRawMagnGWindowLinInterp)
    fKeyTrain$MsdLinInterpO[wIndex] <- sd(tmpRawMagnOWindowLinInterp)
    
    fKeyTrain$MsdPolInterpA[wIndex] <- sd(tmpRawMagnAWindowPolInterp)
    fKeyTrain$MsdPolInterpG[wIndex] <- sd(tmpRawMagnGWindowPolInterp)
    fKeyTrain$MsdPolInterpO[wIndex] <- sd(tmpRawMagnOWindowPolInterp)
    
    fKeyTrain$MsdCubInterpA[wIndex] <- sd(tmpRawMagnAWindowCubInterp)
    fKeyTrain$MsdCubInterpG[wIndex] <- sd(tmpRawMagnGWindowCubInterp)
    fKeyTrain$MsdCubInterpO[wIndex] <- sd(tmpRawMagnOWindowCubInterp)
    
    fKeyTrain$MsdMeanNormA[wIndex] <- sd(tmpRawMagnAWindowMeanNorm)
    fKeyTrain$MsdMeanNormG[wIndex] <- sd(tmpRawMagnGWindowMeanNorm)
    fKeyTrain$MsdMeanNormO[wIndex] <- sd(tmpRawMagnOWindowMeanNorm)
    
    # Variance
    fKeyTrain$XvarA[wIndex] <- var(tmpRawXWindow)
    fKeyTrain$YvarA[wIndex] <- var(tmpRawYWindow)
    fKeyTrain$ZvarA[wIndex] <- var(tmpRawZWindow)
    fKeyTrain$XvarG[wIndex] <- var(tmpRawAWindow)
    fKeyTrain$YvarG[wIndex] <- var(tmpRawBWindow)
    fKeyTrain$ZvarG[wIndex] <- var(tmpRawCWindow)
    fKeyTrain$XvarO[wIndex] <- var(tmpRawAlphaWindow)
    fKeyTrain$YvarO[wIndex] <- var(tmpRawBetaWindow)
    fKeyTrain$ZvarO[wIndex] <- var(tmpRawGammaWindow)
    
    fKeyTrain$XvarLinInterpA[wIndex] <- var(tmpRawXWindowLinInterp)
    fKeyTrain$YvarLinInterpA[wIndex] <- var(tmpRawYWindowLinInterp)
    fKeyTrain$ZvarLinInterpA[wIndex] <- var(tmpRawZWindowLinInterp)
    fKeyTrain$XvarLinInterpG[wIndex] <- var(tmpRawAWindowLinInterp)
    fKeyTrain$YvarLinInterpG[wIndex] <- var(tmpRawBWindowLinInterp)
    fKeyTrain$ZvarLinInterpG[wIndex] <- var(tmpRawCWindowLinInterp)
    fKeyTrain$XvarLinInterpO[wIndex] <- var(tmpRawAlphaWindowLinInterp)
    fKeyTrain$YvarLinInterpO[wIndex] <- var(tmpRawBetaWindowLinInterp)
    fKeyTrain$ZvarLinInterpO[wIndex] <- var(tmpRawGammaWindowLinInterp)
    
    fKeyTrain$XvarPolInterpA[wIndex] <- var(tmpRawXWindowPolInterp)
    fKeyTrain$YvarPolInterpA[wIndex] <- var(tmpRawYWindowPolInterp)
    fKeyTrain$ZvarPolInterpA[wIndex] <- var(tmpRawZWindowPolInterp)
    fKeyTrain$XvarPolInterpG[wIndex] <- var(tmpRawAWindowPolInterp)
    fKeyTrain$YvarPolInterpG[wIndex] <- var(tmpRawBWindowPolInterp)
    fKeyTrain$ZvarPolInterpG[wIndex] <- var(tmpRawCWindowPolInterp)
    fKeyTrain$XvarPolInterpO[wIndex] <- var(tmpRawAlphaWindowPolInterp)
    fKeyTrain$YvarPolInterpO[wIndex] <- var(tmpRawBetaWindowPolInterp)
    fKeyTrain$ZvarPolInterpO[wIndex] <- var(tmpRawGammaWindowPolInterp)
    
    fKeyTrain$XvarCubInterpA[wIndex] <- var(tmpRawXWindowCubInterp)
    fKeyTrain$YvarCubInterpA[wIndex] <- var(tmpRawYWindowCubInterp)
    fKeyTrain$ZvarCubInterpA[wIndex] <- var(tmpRawZWindowCubInterp)
    fKeyTrain$XvarCubInterpG[wIndex] <- var(tmpRawAWindowCubInterp)
    fKeyTrain$YvarCubInterpG[wIndex] <- var(tmpRawBWindowCubInterp)
    fKeyTrain$ZvarCubInterpG[wIndex] <- var(tmpRawCWindowCubInterp)
    fKeyTrain$XvarCubInterpO[wIndex] <- var(tmpRawAlphaWindowCubInterp)
    fKeyTrain$YvarCubInterpO[wIndex] <- var(tmpRawBetaWindowCubInterp)
    fKeyTrain$ZvarCubInterpO[wIndex] <- var(tmpRawGammaWindowCubInterp)
    
    fKeyTrain$XvarMeanNormA[wIndex] <- var(tmpRawXWindowMeanNorm)
    fKeyTrain$YvarMeanNormA[wIndex] <- var(tmpRawYWindowMeanNorm)
    fKeyTrain$ZvarMeanNormA[wIndex] <- var(tmpRawZWindowMeanNorm)
    fKeyTrain$XvarMeanNormG[wIndex] <- var(tmpRawAWindowMeanNorm)
    fKeyTrain$YvarMeanNormG[wIndex] <- var(tmpRawBWindowMeanNorm)
    fKeyTrain$ZvarMeanNormG[wIndex] <- var(tmpRawCWindowMeanNorm)
    fKeyTrain$XvarMeanNormO[wIndex] <- var(tmpRawAlphaWindowMeanNorm)
    fKeyTrain$YvarMeanNormO[wIndex] <- var(tmpRawBetaWindowMeanNorm)
    fKeyTrain$ZvarMeanNormO[wIndex] <- var(tmpRawGammaWindowMeanNorm)
    
    #var magn
    fKeyTrain$MvarA[wIndex] <- var(tmpRawMagnAWindow)
    fKeyTrain$MvarG[wIndex] <- var(tmpRawMagnGWindow)
    fKeyTrain$MvarO[wIndex] <- var(tmpRawMagnOWindow)
    
    fKeyTrain$MvarLinInterpA[wIndex] <- var(tmpRawMagnAWindowLinInterp)
    fKeyTrain$MvarLinInterpG[wIndex] <- var(tmpRawMagnGWindowLinInterp)
    fKeyTrain$MvarLinInterpO[wIndex] <- var(tmpRawMagnOWindowLinInterp)
    
    fKeyTrain$MvarPolInterpA[wIndex] <- var(tmpRawMagnAWindowPolInterp)
    fKeyTrain$MvarPolInterpG[wIndex] <- var(tmpRawMagnGWindowPolInterp)
    fKeyTrain$MvarPolInterpO[wIndex] <- var(tmpRawMagnOWindowPolInterp)
    
    fKeyTrain$MvarCubInterpA[wIndex] <- var(tmpRawMagnAWindowCubInterp)
    fKeyTrain$MvarCubInterpG[wIndex] <- var(tmpRawMagnGWindowCubInterp)
    fKeyTrain$MvarCubInterpO[wIndex] <- var(tmpRawMagnOWindowCubInterp)
    
    fKeyTrain$MvarMeanNormA[wIndex] <- var(tmpRawMagnAWindowMeanNorm)
    fKeyTrain$MvarMeanNormG[wIndex] <- var(tmpRawMagnGWindowMeanNorm)
    fKeyTrain$MvarMeanNormO[wIndex] <- var(tmpRawMagnOWindowMeanNorm)
    
    # RMS  Accelerometer
    fKeyTrain$XrmsA[wIndex] <- sqrt(sum((tmpRawXWindow) ^ 2) / wSize)
    fKeyTrain$YrmsA[wIndex] <- sqrt(sum((tmpRawYWindow) ^ 2) / wSize)
    fKeyTrain$ZrmsA[wIndex] <- sqrt(sum((tmpRawZWindow) ^ 2) / wSize)
    fKeyTrain$XrmsG[wIndex] <- sqrt(sum((tmpRawAWindow) ^ 2) / wSize)
    fKeyTrain$YrmsG[wIndex] <- sqrt(sum((tmpRawBWindow) ^ 2) / wSize)
    fKeyTrain$ZrmsG[wIndex] <- sqrt(sum((tmpRawCWindow) ^ 2) / wSize)
    fKeyTrain$XrmsO[wIndex] <- sqrt(sum((tmpRawAlphaWindow) ^ 2) / wSize)
    fKeyTrain$YrmsO[wIndex] <- sqrt(sum((tmpRawBetaWindow) ^ 2) / wSize)
    fKeyTrain$ZrmsO[wIndex] <- sqrt(sum((tmpRawGammaWindow) ^ 2) / wSize)
    
    fKeyTrain$XrmsLinInterpA[wIndex] <- sqrt(sum((tmpRawXWindowLinInterp) ^ 2) / wSize)
    fKeyTrain$YrmsLinInterpA[wIndex] <- sqrt(sum((tmpRawYWindowLinInterp) ^ 2) / wSize)
    fKeyTrain$ZrmsLinInterpA[wIndex] <- sqrt(sum((tmpRawZWindowLinInterp) ^ 2) / wSize)
    fKeyTrain$XrmsLinInterpG[wIndex] <- sqrt(sum((tmpRawAWindowLinInterp) ^ 2) / wSize)
    fKeyTrain$YrmsLinInterpG[wIndex] <- sqrt(sum((tmpRawBWindowLinInterp) ^ 2) / wSize)
    fKeyTrain$ZrmsLinInterpG[wIndex] <- sqrt(sum((tmpRawCWindowLinInterp) ^ 2) / wSize)
    fKeyTrain$XrmsLinInterpO[wIndex] <- sqrt(sum((tmpRawAlphaWindowLinInterp) ^ 2) / wSize)
    fKeyTrain$YrmsLinInterpO[wIndex] <- sqrt(sum((tmpRawBetaWindowLinInterp) ^ 2) / wSize)
    fKeyTrain$ZrmsLinInterpO[wIndex] <- sqrt(sum((tmpRawGammaWindowLinInterp) ^ 2) / wSize)
    
    fKeyTrain$XrmsPolInterpA[wIndex] <- sqrt(sum((tmpRawXWindowPolInterp) ^ 2) / wSize)
    fKeyTrain$YrmsPolInterpA[wIndex] <- sqrt(sum((tmpRawYWindowPolInterp) ^ 2) / wSize)
    fKeyTrain$ZrmsPolInterpA[wIndex] <- sqrt(sum((tmpRawZWindowPolInterp) ^ 2) / wSize)
    fKeyTrain$XrmsPolInterpG[wIndex] <- sqrt(sum((tmpRawAWindowPolInterp) ^ 2) / wSize)
    fKeyTrain$YrmsPolInterpG[wIndex] <- sqrt(sum((tmpRawBWindowPolInterp) ^ 2) / wSize)
    fKeyTrain$ZrmsPolInterpG[wIndex] <- sqrt(sum((tmpRawCWindowPolInterp) ^ 2) / wSize)
    fKeyTrain$XrmsPolInterpO[wIndex] <- sqrt(sum((tmpRawAlphaWindowPolInterp) ^ 2) / wSize)
    fKeyTrain$YrmsPolInterpO[wIndex] <- sqrt(sum((tmpRawBetaWindowPolInterp) ^ 2) / wSize)
    fKeyTrain$ZrmsPolInterpO[wIndex] <- sqrt(sum((tmpRawGammaWindowPolInterp) ^ 2) / wSize)
    
    fKeyTrain$XrmsCubInterpA[wIndex] <- sqrt(sum((tmpRawXWindowCubInterp) ^ 2) / wSize)
    fKeyTrain$YrmsCubInterpA[wIndex] <- sqrt(sum((tmpRawYWindowCubInterp) ^ 2) / wSize)
    fKeyTrain$ZrmsCubInterpA[wIndex] <- sqrt(sum((tmpRawZWindowCubInterp) ^ 2) / wSize)
    fKeyTrain$XrmsCubInterpG[wIndex] <- sqrt(sum((tmpRawAWindowCubInterp) ^ 2) / wSize)
    fKeyTrain$YrmsCubInterpG[wIndex] <- sqrt(sum((tmpRawBWindowCubInterp) ^ 2) / wSize)
    fKeyTrain$ZrmsCubInterpG[wIndex] <- sqrt(sum((tmpRawCWindowCubInterp) ^ 2) / wSize)
    fKeyTrain$XrmsCubInterpO[wIndex] <- sqrt(sum((tmpRawAlphaWindowCubInterp) ^ 2) / wSize)
    fKeyTrain$YrmsCubInterpO[wIndex] <- sqrt(sum((tmpRawBetaWindowCubInterp) ^ 2) / wSize)
    fKeyTrain$ZrmsCubInterpO[wIndex] <- sqrt(sum((tmpRawGammaWindowCubInterp) ^ 2) / wSize)
    
    fKeyTrain$XrmsMeanNormA[wIndex] <- sqrt(sum(((tmpRawXWindowMeanNorm) ^ 2)) / wSize)
    fKeyTrain$YrmsMeanNormA[wIndex] <- sqrt(sum(((tmpRawYWindowMeanNorm) ^ 2)) / wSize)
    fKeyTrain$ZrmsMeanNormA[wIndex] <- sqrt(sum(((tmpRawZWindowMeanNorm) ^ 2)) / wSize)
    fKeyTrain$XrmsMeanNormG[wIndex] <- sqrt(sum(((tmpRawAWindowMeanNorm) ^ 2)) / wSize)
    fKeyTrain$YrmsMeanNormG[wIndex] <- sqrt(sum(((tmpRawBWindowMeanNorm) ^ 2)) / wSize)
    fKeyTrain$ZrmsMeanNormG[wIndex] <- sqrt(sum(((tmpRawCWindowMeanNorm) ^ 2)) / wSize)
    fKeyTrain$XrmsMeanNormO[wIndex] <- sqrt(sum(((tmpRawAlphaWindowMeanNorm) ^ 2)) / wSize)
    fKeyTrain$YrmsMeanNormO[wIndex] <- sqrt(sum(((tmpRawBetaWindowMeanNorm) ^ 2)) / wSize)
    fKeyTrain$ZrmsMeanNormO[wIndex] <- sqrt(sum(((tmpRawGammaWindowMeanNorm) ^ 2)) / wSize)
    
    # Root Mean Square of the magnitude
    fKeyTrain$MagnRmsA[wIndex] <- sqrt((sum((tmpRawMagnAWindow) ^ 2)) / wSize)
    fKeyTrain$MagnRmsG[wIndex] <- sqrt((sum((tmpRawMagnGWindow) ^ 2)) / wSize)
    fKeyTrain$MagnRmsO[wIndex] <- sqrt((sum((tmpRawMagnOWindow) ^ 2)) / wSize)
    
    fKeyTrain$MagnRmsLinInterpA[wIndex] <- sqrt((sum((tmpRawMagnAWindowLinInterp) ^ 2)) / wSize)
    fKeyTrain$MagnRmsLinInterpG[wIndex] <- sqrt((sum((tmpRawMagnGWindowLinInterp) ^ 2)) / wSize)
    fKeyTrain$MagnRmsLinInterpO[wIndex] <- sqrt((sum((tmpRawMagnOWindowLinInterp) ^ 2)) / wSize)
    
    fKeyTrain$MagnRmsPolInterpA[wIndex] <- sqrt((sum((tmpRawMagnAWindowPolInterp) ^ 2)) / wSize)
    fKeyTrain$MagnRmsPolInterpG[wIndex] <- sqrt((sum((tmpRawMagnGWindowPolInterp) ^ 2)) / wSize)
    fKeyTrain$MagnRmsPolInterpO[wIndex] <- sqrt((sum((tmpRawMagnOWindowPolInterp) ^ 2)) / wSize)
    
    fKeyTrain$MagnRmsCubInterpA[wIndex] <- sqrt((sum((tmpRawMagnAWindowCubInterp) ^ 2)) / wSize)
    fKeyTrain$MagnRmsCubInterpG[wIndex] <- sqrt((sum((tmpRawMagnGWindowCubInterp) ^ 2)) / wSize)
    fKeyTrain$MagnRmsCubInterpO[wIndex] <- sqrt((sum((tmpRawMagnOWindowCubInterp) ^ 2)) / wSize)
    
    fKeyTrain$MrmsMeanNormA[wIndex] <- sqrt(sum(((tmpRawMagnAWindowMeanNorm) ^ 2)) / wSize)
    fKeyTrain$MrmsMeanNormG[wIndex] <- sqrt(sum(((tmpRawMagnGWindowMeanNorm) ^ 2)) / wSize)
    fKeyTrain$MrmsMeanNormO[wIndex] <- sqrt(sum(((tmpRawMagnOWindowMeanNorm) ^ 2)) / wSize)
    
    keyCount <- length(tmpWindowCopy$belongsToKey[tmpWindowCopy$belongsToKey == TRUE])
    if(keyCount > 0) {
      tmpKey <- keytrain[keytrain$DownTime <= tmpWindowCopy$Timestamp[tmpWindowCopy$belongsToKey == TRUE][1] & keytrain$EventTime >= tmpWindowCopy$Timestamp[tmpWindowCopy$belongsToKey == TRUE][1],]
      
      fKeyTrain$IsKeyProb[wIndex] <- keyCount/tmpKey$WindowSize
    } else {
      fKeyTrain$IsKeyProb[wIndex] <- 0
    }
    
    fKeyTrain$IsKeyProb[wIndex] <- length(tmpWindowCopy$belongsToKey[tmpWindowCopy$belongsToKey == TRUE]) / wSize
    
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
fKeyTrain$XskewO <- 3 * (fKeyTrain$XmeanO - fKeyTrain$XmedianO) / fKeyTrain$XsdO
fKeyTrain$YskewO <- 3 * (fKeyTrain$YmeanO - fKeyTrain$YmedianO) / fKeyTrain$YsdO
fKeyTrain$ZskewO <- 3 * (fKeyTrain$ZmeanO - fKeyTrain$ZmedianO) / fKeyTrain$ZsdO

fKeyTrain$XskewLinInterpA <- 3 * (fKeyTrain$XmeanLinInterpA - fKeyTrain$XmedianLinInterpA) / fKeyTrain$XsdLinInterpA
fKeyTrain$YskewLinInterpA <- 3 * (fKeyTrain$YmeanLinInterpA - fKeyTrain$YmedianLinInterpA) / fKeyTrain$YsdLinInterpA
fKeyTrain$ZskewLinInterpA <- 3 * (fKeyTrain$ZmeanLinInterpA - fKeyTrain$ZmedianLinInterpA) / fKeyTrain$ZsdLinInterpA
fKeyTrain$XskewLinInterpG <- 3 * (fKeyTrain$XmeanLinInterpG - fKeyTrain$XmedianLinInterpG) / fKeyTrain$XsdLinInterpG
fKeyTrain$YskewLinInterpG <- 3 * (fKeyTrain$YmeanLinInterpG - fKeyTrain$YmedianLinInterpG) / fKeyTrain$YsdLinInterpG
fKeyTrain$ZskewLinInterpG <- 3 * (fKeyTrain$ZmeanLinInterpG - fKeyTrain$ZmedianLinInterpG) / fKeyTrain$ZsdLinInterpG
fKeyTrain$XskewLinInterpO <- 3 * (fKeyTrain$XmeanLinInterpO - fKeyTrain$XmedianLinInterpO) / fKeyTrain$XsdLinInterpO
fKeyTrain$YskewLinInterpO <- 3 * (fKeyTrain$YmeanLinInterpO - fKeyTrain$YmedianLinInterpO) / fKeyTrain$YsdLinInterpO
fKeyTrain$ZskewLinInterpO <- 3 * (fKeyTrain$ZmeanLinInterpO - fKeyTrain$ZmedianLinInterpO) / fKeyTrain$ZsdLinInterpO

fKeyTrain$XskewPolInterpA <- 3 * (fKeyTrain$XmeanPolInterpA - fKeyTrain$XmedianPolInterpA) / fKeyTrain$XsdPolInterpA
fKeyTrain$YskewPolInterpA <- 3 * (fKeyTrain$YmeanPolInterpA - fKeyTrain$YmedianPolInterpA) / fKeyTrain$YsdPolInterpA
fKeyTrain$ZskewPolInterpA <- 3 * (fKeyTrain$ZmeanPolInterpA - fKeyTrain$ZmedianPolInterpA) / fKeyTrain$ZsdPolInterpA
fKeyTrain$XskewPolInterpG <- 3 * (fKeyTrain$XmeanPolInterpG - fKeyTrain$XmedianPolInterpG) / fKeyTrain$XsdPolInterpG
fKeyTrain$YskewPolInterpG <- 3 * (fKeyTrain$YmeanPolInterpG - fKeyTrain$YmedianPolInterpG) / fKeyTrain$YsdPolInterpG
fKeyTrain$ZskewPolInterpG <- 3 * (fKeyTrain$ZmeanPolInterpG - fKeyTrain$ZmedianPolInterpG) / fKeyTrain$ZsdPolInterpG
fKeyTrain$XskewPolInterpO <- 3 * (fKeyTrain$XmeanPolInterpO - fKeyTrain$XmedianPolInterpO) / fKeyTrain$XsdPolInterpO
fKeyTrain$YskewPolInterpO <- 3 * (fKeyTrain$YmeanPolInterpO - fKeyTrain$YmedianPolInterpO) / fKeyTrain$YsdPolInterpO
fKeyTrain$ZskewPolInterpO <- 3 * (fKeyTrain$ZmeanPolInterpO - fKeyTrain$ZmedianPolInterpO) / fKeyTrain$ZsdPolInterpO

fKeyTrain$XskewCubInterpA <- 3 * (fKeyTrain$XmeanCubInterpA - fKeyTrain$XmedianCubInterpA) / fKeyTrain$XsdCubInterpA
fKeyTrain$YskewCubInterpA <- 3 * (fKeyTrain$YmeanCubInterpA - fKeyTrain$YmedianCubInterpA) / fKeyTrain$YsdCubInterpA
fKeyTrain$ZskewCubInterpA <- 3 * (fKeyTrain$ZmeanCubInterpA - fKeyTrain$ZmedianCubInterpA) / fKeyTrain$ZsdCubInterpA
fKeyTrain$XskewCubInterpG <- 3 * (fKeyTrain$XmeanCubInterpG - fKeyTrain$XmedianCubInterpG) / fKeyTrain$XsdCubInterpG
fKeyTrain$YskewCubInterpG <- 3 * (fKeyTrain$YmeanCubInterpG - fKeyTrain$YmedianCubInterpG) / fKeyTrain$YsdCubInterpG
fKeyTrain$ZskewCubInterpG <- 3 * (fKeyTrain$ZmeanCubInterpG - fKeyTrain$ZmedianCubInterpG) / fKeyTrain$ZsdCubInterpG
fKeyTrain$XskewCubInterpO <- 3 * (fKeyTrain$XmeanCubInterpO - fKeyTrain$XmedianCubInterpO) / fKeyTrain$XsdCubInterpO
fKeyTrain$YskewCubInterpO <- 3 * (fKeyTrain$YmeanCubInterpO - fKeyTrain$YmedianCubInterpO) / fKeyTrain$YsdCubInterpO
fKeyTrain$ZskewCubInterpO <- 3 * (fKeyTrain$ZmeanCubInterpO - fKeyTrain$ZmedianCubInterpO) / fKeyTrain$ZsdCubInterpO

fKeyTrain$XskewMeanNormA <- 3 * (fKeyTrain$XmeanMeanNormA - fKeyTrain$XmedianMeanNormA) / fKeyTrain$XsdMeanNormA
fKeyTrain$YskewMeanNormA <- 3 * (fKeyTrain$YmeanMeanNormA - fKeyTrain$YmedianMeanNormA) / fKeyTrain$YsdMeanNormA
fKeyTrain$ZskewMeanNormA <- 3 * (fKeyTrain$ZmeanMeanNormA - fKeyTrain$ZmedianMeanNormA) / fKeyTrain$ZsdMeanNormA
fKeyTrain$XskewMeanNormG <- 3 * (fKeyTrain$XmeanMeanNormG - fKeyTrain$XmedianMeanNormG) / fKeyTrain$XsdMeanNormG
fKeyTrain$YskewMeanNormG <- 3 * (fKeyTrain$YmeanMeanNormG - fKeyTrain$YmedianMeanNormG) / fKeyTrain$YsdMeanNormG
fKeyTrain$ZskewMeanNormG <- 3 * (fKeyTrain$ZmeanMeanNormG - fKeyTrain$ZmedianMeanNormG) / fKeyTrain$ZsdMeanNormG
fKeyTrain$XskewMeanNormO <- 3 * (fKeyTrain$XmeanMeanNormO - fKeyTrain$XmedianMeanNormO) / fKeyTrain$XsdMeanNormO
fKeyTrain$YskewMeanNormO <- 3 * (fKeyTrain$YmeanMeanNormO - fKeyTrain$YmedianMeanNormO) / fKeyTrain$YsdMeanNormO
fKeyTrain$ZskewMeanNormO <- 3 * (fKeyTrain$ZmeanMeanNormO - fKeyTrain$ZmedianMeanNormO) / fKeyTrain$ZsdMeanNormO

# skewness magn
fKeyTrain$MskewA <- 3 * (fKeyTrain$MmeanA - fKeyTrain$MmedianA) / fKeyTrain$MsdA
fKeyTrain$MskewG <- 3 * (fKeyTrain$MmeanG - fKeyTrain$MmedianG) / fKeyTrain$MsdG
fKeyTrain$MskewO <- 3 * (fKeyTrain$MmeanO - fKeyTrain$MmedianO) / fKeyTrain$MsdO

fKeyTrain$MskewLinInterpA <- 3 * (fKeyTrain$MmeanLinInterpA - fKeyTrain$MmedianLinInterpA) / fKeyTrain$MsdLinInterpA
fKeyTrain$MskewLinInterpG <- 3 * (fKeyTrain$MmeanLinInterpG - fKeyTrain$MmedianLinInterpG) / fKeyTrain$MsdLinInterpG
fKeyTrain$MskewLinInterpO <- 3 * (fKeyTrain$MmeanLinInterpO - fKeyTrain$MmedianLinInterpO) / fKeyTrain$MsdLinInterpO

fKeyTrain$MskewPolInterpA <- 3 * (fKeyTrain$MmeanPolInterpA - fKeyTrain$MmedianPolInterpA) / fKeyTrain$MsdPolInterpA
fKeyTrain$MskewPolInterpG <- 3 * (fKeyTrain$MmeanPolInterpG - fKeyTrain$MmedianPolInterpG) / fKeyTrain$MsdPolInterpG
fKeyTrain$MskewPolInterpO <- 3 * (fKeyTrain$MmeanPolInterpO - fKeyTrain$MmedianPolInterpO) / fKeyTrain$MsdPolInterpO

fKeyTrain$MskewCubInterpA <- 3 * (fKeyTrain$MmeanCubInterpA - fKeyTrain$MmedianCubInterpA) / fKeyTrain$MsdCubInterpA
fKeyTrain$MskewCubInterpG <- 3 * (fKeyTrain$MmeanCubInterpG - fKeyTrain$MmedianCubInterpG) / fKeyTrain$MsdCubInterpG
fKeyTrain$MskewCubInterpO <- 3 * (fKeyTrain$MmeanCubInterpO - fKeyTrain$MmedianCubInterpO) / fKeyTrain$MsdCubInterpO

fKeyTrain$MskewMeanNormA <- 3 * (fKeyTrain$MmeanMeanNormA - fKeyTrain$MmedianMeanNormA) / fKeyTrain$MsdMeanNormA
fKeyTrain$MskewMeanNormG <- 3 * (fKeyTrain$MmeanMeanNormG - fKeyTrain$MmedianMeanNormG) / fKeyTrain$MsdMeanNormG
fKeyTrain$MskewMeanNormO <- 3 * (fKeyTrain$MmeanMeanNormO - fKeyTrain$MmedianMeanNormO) / fKeyTrain$MsdMeanNormO


write.csv(
  fsd,
  "C:\\git\\data-thesis\\R\\datasets\\17011020-dataset-training-tap-detection.csv",
  row.names = FALSE
)
