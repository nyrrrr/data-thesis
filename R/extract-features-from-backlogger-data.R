rawdata <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\transferred-17011020-victim-data.csv",
    header = TRUE
  )

options(digits=20)
# --- preprocessing

rawdata$id <- seq_along(rawdata$Timestamp)

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

# # slope dy/dx
# rawdata$XslopeA[1] <- rawdata$xA[1]
# rawdata$XslopeA[2:length(rawdata$Timestamp)] <- (((rawdata$Timestamp[-1] - rawdata$Timestamp[-length(rawdata$Timestamp)]))/1000000000)/(rawdata$xA[-1] - rawdata$xA[-length(rawdata$xA)])
# rawdata$YslopeA[1] <- rawdata$yA[1]
# rawdata$YslopeA[2:length(rawdata$Timestamp)] <- (((rawdata$Timestamp[-1] - rawdata$Timestamp[-length(rawdata$Timestamp)]))/1000000000)/(rawdata$yA[-1] - rawdata$yA[-length(rawdata$yA)])
# rawdata$ZslopeA[1] <- rawdata$zA[1]
# rawdata$ZslopeA[2:length(rawdata$Timestamp)] <- (((rawdata$Timestamp[-1] - rawdata$Timestamp[-length(rawdata$Timestamp)]))/1000000000)/(rawdata$zA[-1] - rawdata$zA[-length(rawdata$zA)])
# rawdata$XslopeG[1] <- rawdata$xG[1]
# rawdata$XslopeG[2:length(rawdata$Timestamp)] <- (((rawdata$Timestamp[-1] - rawdata$Timestamp[-length(rawdata$Timestamp)]))/1000000000)/(rawdata$xG[-1] - rawdata$xG[-length(rawdata$xG)])
# rawdata$YslopeG[1] <- rawdata$yG[1]
# rawdata$YslopeG[2:length(rawdata$Timestamp)] <- (((rawdata$Timestamp[-1] - rawdata$Timestamp[-length(rawdata$Timestamp)]))/1000000000)/(rawdata$yG[-1] - rawdata$yG[-length(rawdata$yG)])
# rawdata$ZslopeG[1] <- rawdata$zG[1]
# rawdata$ZslopeG[2:length(rawdata$Timestamp)] <- (((rawdata$Timestamp[-1] - rawdata$Timestamp[-length(rawdata$Timestamp)]))/1000000000)/(rawdata$zG[-1] - rawdata$zG[-length(rawdata$zG)])
# rawdata$XslopeO[1] <- rawdata$alpha[1]
# rawdata$XslopeO[2:length(rawdata$Timestamp)] <- (((rawdata$Timestamp[-1] - rawdata$Timestamp[-length(rawdata$Timestamp)]))/1000000000)/(rawdata$alpha[-1] - rawdata$alpha[-length(rawdata$alpha)])
# rawdata$YslopeO[1] <- rawdata$beta[1]
# rawdata$YslopeO[2:length(rawdata$Timestamp)] <- (((rawdata$Timestamp[-1] - rawdata$Timestamp[-length(rawdata$Timestamp)]))/1000000000)/(rawdata$beta[-1] - rawdata$beta[-length(rawdata$beta)])
# rawdata$ZslopeO[1] <- rawdata$gamma[1]
# rawdata$ZslopeO[2:length(rawdata$Timestamp)] <- (((rawdata$Timestamp[-1] - rawdata$Timestamp[-length(rawdata$Timestamp)]))/1000000000)/(rawdata$gamma[-1] - rawdata$gamma[-length(rawdata$gamma)])
# 
# rawdata$XslopeA[rawdata$XslopeA == Inf] <- 0
# rawdata$YslopeA[rawdata$YslopeA == Inf] <- 0
# rawdata$ZslopeA[rawdata$ZslopeA == Inf] <- 0
# rawdata$XslopeG[rawdata$XslopeG == Inf] <- 0
# rawdata$YslopeG[rawdata$YslopeG == Inf] <- 0
# rawdata$ZslopeG[rawdata$ZslopeG == Inf] <- 0
# rawdata$XslopeO[rawdata$XslopeO == Inf] <- 0
# rawdata$YslopeO[rawdata$YslopeO == Inf] <- 0
# rawdata$ZslopeO[rawdata$ZslopeO == Inf] <- 0

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

rawdata$MnormMeanA <- rawdata$MagnA - mean(rawdata$MagnA)
rawdata$MnormMeanG <- rawdata$MagnG - mean(rawdata$MagnG)
rawdata$MnormMeanO <- rawdata$MagnO - mean(rawdata$MagnO)

# # distance between Value and previous value
# rawdata$XdeltaA[1] <- rawdata$xA[1]
# rawdata$YdeltaA[1] <- rawdata$yA[1]
# rawdata$ZdeltaA[1] <- rawdata$zA[1]
# rawdata$XdeltaG[1] <- rawdata$xG[1]
# rawdata$YdeltaG[1] <- rawdata$yG[1]
# rawdata$ZdeltaG[1] <- rawdata$zG[1]
# rawdata$XdeltaO[1] <- rawdata$alpha[1]
# rawdata$YdeltaO[1] <- rawdata$beta[1]
# rawdata$ZdeltaO[1] <- rawdata$gamma[1]
# rawdata$XdeltaA[2:length(rawdata$xA)] <- rawdata$xA[-1] - rawdata$xA[-length(rawdata$xA)]
# rawdata$YdeltaA[2:length(rawdata$xA)] <- rawdata$yA[-1] - rawdata$yA[-length(rawdata$yA)]
# rawdata$ZdeltaA[2:length(rawdata$xA)] <- rawdata$zA[-1] - rawdata$zA[-length(rawdata$zA)]
# rawdata$XdeltaG[2:length(rawdata$xA)] <- rawdata$xG[-1] - rawdata$xG[-length(rawdata$xG)]
# rawdata$YdeltaG[2:length(rawdata$xA)] <- rawdata$yG[-1] - rawdata$yG[-length(rawdata$yG)]
# rawdata$ZdeltaG[2:length(rawdata$xA)] <- rawdata$zG[-1] - rawdata$zG[-length(rawdata$zG)]
# rawdata$XdeltaO[2:length(rawdata$xA)] <- rawdata$alpha[-1] - rawdata$alpha[-length(rawdata$alpha)]
# rawdata$YdeltaO[2:length(rawdata$xA)] <- rawdata$beta[-1] - rawdata$beta[-length(rawdata$beta)]
# rawdata$ZdeltaO[2:length(rawdata$xA)] <- rawdata$gamma[-1] - rawdata$gamma[-length(rawdata$gamma)]
# 
# rawdata$MdeltaA[1] <- rawdata$MagnA[1]
# rawdata$MdeltaG[1] <- rawdata$MagnG[1]
# rawdata$MdeltaO[1] <- rawdata$MagnO[1]
# rawdata$MdeltaA[2:length(rawdata$MagnA)] <- rawdata$MagnA[-1] - rawdata$MagnA[-length(rawdata$MagnA)]
# rawdata$MdeltaG[2:length(rawdata$MagnG)] <- rawdata$MagnG[-1] - rawdata$MagnG[-length(rawdata$MagnG)]
# rawdata$MdeltaO[2:length(rawdata$MagnO)] <- rawdata$MagnO[-1] - rawdata$MagnO[-length(rawdata$MagnO)]

# # low pass filter: y[i] := y[i-1] + α * (x[i] - x[i-1])
# filterfactor <- 0.9
# for(i in seq_along(rawdata$xA)) {
#   if(i == 1) {
#     rawdata$XlpA[1] <- rawdata$xA[1]
#     rawdata$YlpA[1] <- rawdata$yA[1]
#     rawdata$ZlpA[1] <- rawdata$zA[1]
#     rawdata$XlpG[1] <- rawdata$xG[1]
#     rawdata$YlpG[1] <- rawdata$yG[1]
#     rawdata$ZlpG[1] <- rawdata$zG[1]
#     rawdata$XlpO[1] <- rawdata$alpha[1]
#     rawdata$YlpO[1] <- rawdata$beta[1]
#     rawdata$ZlpO[1] <- rawdata$gamma[1]
#   }
#   else {
#     rawdata$XlpA[i] <- rawdata$XlpA[i-1] + filterfactor * rawdata$XdeltaA[i]
#     rawdata$YlpA[i] <- rawdata$YlpA[i-1] + filterfactor * rawdata$YdeltaA[i]
#     rawdata$ZlpA[i] <- rawdata$ZlpA[i-1] + filterfactor * rawdata$ZdeltaA[i]
#     rawdata$XlpG[i] <- rawdata$XlpG[i-1] + filterfactor * rawdata$XdeltaG[i]
#     rawdata$YlpG[i] <- rawdata$YlpG[i-1] + filterfactor * rawdata$YdeltaG[i]
#     rawdata$ZlpG[i] <- rawdata$ZlpG[i-1] + filterfactor * rawdata$ZdeltaG[i]
#     rawdata$XlpO[i] <- rawdata$XlpO[i-1] + filterfactor * rawdata$XdeltaO[i]
#     rawdata$YlpO[i] <- rawdata$YlpO[i-1] + filterfactor * rawdata$YdeltaO[i]
#     rawdata$ZlpO[i] <- rawdata$ZlpO[i-1] + filterfactor * rawdata$ZdeltaO[i]
#   }
#   rawdata$id[i] <- i
# }
# # magnitude of low pass filtered values
# rawdata$MagnLpA <- sqrt(rawdata$XlpA^2 + rawdata$YlpA^2 + rawdata$ZlpA^2)
# rawdata$MagnLpG <- sqrt(rawdata$XlpG^2 + rawdata$YlpG^2 + rawdata$ZlpG^2)
# rawdata$MagnLpO <- sqrt(rawdata$XlpO^2 + rawdata$YlpO^2 + rawdata$ZlpO^2)

# ------ DEBUG ONLY

keytrain <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\17011020-key-dataset-test-raw.csv",
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
          "C:\\git\\data-thesis\\R\\datasets\\transferred-17011020-victim-data-preprocessed.csv",
          row.names = FALSE)

# --- feature extraction


# window size
wSize <- read.csv(
  "C:\\git\\data-thesis\\R\\datasets\\17011020-dataset-training.csv",
  header = TRUE
)
wSize <- ceiling(median(wSize$WindowSize))
# wSize <- ceiling(mean(wSize$WindowSize))
# wSize <- ceiling(median(wSize$WindowSize[wSize$IsKey == TRUE]))
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
    
    fsd$WindowSize[wIndex] <- wSize
    
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
    fsd$Xd0A[wIndex] <- tmpRawPolyMod$coefficients[1]
    fsd$Xd1A[wIndex] <- tmpRawPolyMod$coefficients[2]
    fsd$Xd2A[wIndex] <- tmpRawPolyMod$coefficients[3]
    fsd$Xd3A[wIndex] <- tmpRawPolyMod$coefficients[4]
    
    tmpRawPolyMod <- lm(tmpRawYWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
    tmpRawYWindowPolInterp <- predict(tmpRawPolyMod)
    fsd$Yd0A[wIndex] <- tmpRawPolyMod$coefficients[1]
    fsd$Yd1A[wIndex] <- tmpRawPolyMod$coefficients[2]
    fsd$Yd2A[wIndex] <- tmpRawPolyMod$coefficients[3]
    fsd$Yd3A[wIndex] <- tmpRawPolyMod$coefficients[4]
    
    tmpRawPolyMod <- lm(tmpRawZWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
    tmpRawZWindowPolInterp <- predict(tmpRawPolyMod)
    fsd$Zd0A[wIndex] <- tmpRawPolyMod$coefficients[1]
    fsd$Zd1A[wIndex] <- tmpRawPolyMod$coefficients[2]
    fsd$Zd2A[wIndex] <- tmpRawPolyMod$coefficients[3]
    fsd$Zd3A[wIndex] <- tmpRawPolyMod$coefficients[4]
    
    tmpRawPolyMod <- lm(tmpRawYWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
    tmpRawAWindowPolInterp <- predict(tmpRawPolyMod)
    fsd$Xd0G[wIndex] <- tmpRawPolyMod$coefficients[1]
    fsd$Xd1G[wIndex] <- tmpRawPolyMod$coefficients[2]
    fsd$Xd2G[wIndex] <- tmpRawPolyMod$coefficients[3]
    fsd$Xd3G[wIndex] <- tmpRawPolyMod$coefficients[4]
    
    tmpRawPolyMod <- lm(tmpRawBWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
    tmpRawBWindowPolInterp <- predict(tmpRawPolyMod)
    fsd$Yd0G[wIndex] <- tmpRawPolyMod$coefficients[1]
    fsd$Yd1G[wIndex] <- tmpRawPolyMod$coefficients[2]
    fsd$Yd2G[wIndex] <- tmpRawPolyMod$coefficients[3]
    fsd$Yd3G[wIndex] <- tmpRawPolyMod$coefficients[4]
    
    tmpRawPolyMod <- lm(tmpRawCWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
    tmpRawCWindowPolInterp <- predict(tmpRawPolyMod)
    fsd$Zd0G[wIndex] <- tmpRawPolyMod$coefficients[1]
    fsd$Zd1G[wIndex] <- tmpRawPolyMod$coefficients[2]
    fsd$Zd2G[wIndex] <- tmpRawPolyMod$coefficients[3]
    fsd$Zd3G[wIndex] <- tmpRawPolyMod$coefficients[4]
    
    tmpRawPolyMod <- lm(tmpRawAlphaWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
    tmpRawAlphaWindowPolInterp <- predict(tmpRawPolyMod)
    fsd$Xd0O[wIndex] <- tmpRawPolyMod$coefficients[1]
    fsd$Xd1O[wIndex] <- tmpRawPolyMod$coefficients[2]
    fsd$Xd2O[wIndex] <- tmpRawPolyMod$coefficients[3]
    fsd$Xd3O[wIndex] <- tmpRawPolyMod$coefficients[4]
    
    tmpRawPolyMod <- lm(tmpRawBetaWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
    tmpRawBetaWindowPolInterp <- predict(tmpRawPolyMod)
    fsd$Yd0O[wIndex] <- tmpRawPolyMod$coefficients[1]
    fsd$Yd1O[wIndex] <- tmpRawPolyMod$coefficients[2]
    fsd$Yd2O[wIndex] <- tmpRawPolyMod$coefficients[3]
    fsd$Yd3O[wIndex] <- tmpRawPolyMod$coefficients[4]
    
    tmpRawPolyMod <- lm(tmpRawGammaWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
    tmpRawGammaWindowPolInterp <- predict(tmpRawPolyMod)
    fsd$Zd0O[wIndex] <- tmpRawPolyMod$coefficients[1]
    fsd$Zd1O[wIndex] <- tmpRawPolyMod$coefficients[2]
    fsd$Zd2O[wIndex] <- tmpRawPolyMod$coefficients[3]
    fsd$Zd3O[wIndex] <- tmpRawPolyMod$coefficients[4]
    
    tmpRawPolyMod <- lm(tmpRawMagnAWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
    tmpRawMagnAWindowPolInterp <- predict(tmpRawPolyMod)
    fsd$Md0A[wIndex] <- tmpRawPolyMod$coefficients[1]
    fsd$Md1A[wIndex] <- tmpRawPolyMod$coefficients[2]
    fsd$Md2A[wIndex] <- tmpRawPolyMod$coefficients[3]
    fsd$Md3A[wIndex] <- tmpRawPolyMod$coefficients[4]
    
    tmpRawPolyMod <- lm(tmpRawMagnGWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
    tmpRawMagnGWindowPolInterp <- predict(tmpRawPolyMod)
    fsd$Md0G[wIndex] <- tmpRawPolyMod$coefficients[1]
    fsd$Md1G[wIndex] <- tmpRawPolyMod$coefficients[2]
    fsd$Md2G[wIndex] <- tmpRawPolyMod$coefficients[3]
    fsd$Md3G[wIndex] <- tmpRawPolyMod$coefficients[4]
    
    tmpRawPolyMod <- lm(tmpRawMagnOWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
    tmpRawMagnOWindowPolInterp <- predict(tmpRawPolyMod)
    fsd$Md0O[wIndex] <- tmpRawPolyMod$coefficients[1]
    fsd$Md1O[wIndex] <- tmpRawPolyMod$coefficients[2]
    fsd$Md2O[wIndex] <- tmpRawPolyMod$coefficients[3]
    fsd$Md3O[wIndex] <- tmpRawPolyMod$coefficients[4]
    
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
    fsd$XminA[wIndex] <- min(tmpRawXWindow)
    fsd$YminA[wIndex] <- min(tmpRawYWindow)
    fsd$ZminA[wIndex] <- min(tmpRawZWindow)
    fsd$XminG[wIndex] <- min(tmpRawAWindow)
    fsd$YminG[wIndex] <- min(tmpRawBWindow)
    fsd$ZminG[wIndex] <- min(tmpRawCWindow)
    fsd$XminO[wIndex] <- min(tmpRawAlphaWindow)
    fsd$YminO[wIndex] <- min(tmpRawBetaWindow)
    fsd$ZminO[wIndex] <- min(tmpRawGammaWindow)
    
    fsd$XminLinInterpA[wIndex] <- min(tmpRawXWindowLinInterp)
    fsd$YminLinInterpA[wIndex] <- min(tmpRawYWindowLinInterp)
    fsd$ZminLinInterpA[wIndex] <- min(tmpRawZWindowLinInterp)
    fsd$XminLinInterpG[wIndex] <- min(tmpRawAWindowLinInterp)
    fsd$YminLinInterpG[wIndex] <- min(tmpRawBWindowLinInterp)
    fsd$ZminLinInterpG[wIndex] <- min(tmpRawCWindowLinInterp)
    fsd$XminLinInterpO[wIndex] <- min(tmpRawAlphaWindowLinInterp)
    fsd$YminLinInterpO[wIndex] <- min(tmpRawBetaWindowLinInterp)
    fsd$ZminLinInterpO[wIndex] <- min(tmpRawGammaWindowLinInterp)
    
    fsd$XminPolInterpA[wIndex] <- min(tmpRawXWindowPolInterp)
    fsd$YminPolInterpA[wIndex] <- min(tmpRawYWindowPolInterp)
    fsd$ZminPolInterpA[wIndex] <- min(tmpRawZWindowPolInterp)
    fsd$XminPolInterpG[wIndex] <- min(tmpRawAWindowPolInterp)
    fsd$YminPolInterpG[wIndex] <- min(tmpRawBWindowPolInterp)
    fsd$ZminPolInterpG[wIndex] <- min(tmpRawCWindowPolInterp)
    fsd$XminPolInterpO[wIndex] <- min(tmpRawAlphaWindowPolInterp)
    fsd$YminPolInterpO[wIndex] <- min(tmpRawBetaWindowPolInterp)
    fsd$ZminPolInterpO[wIndex] <- min(tmpRawGammaWindowPolInterp)
    
    fsd$XminCubInterpA[wIndex] <- min(tmpRawXWindowCubInterp)
    fsd$YminCubInterpA[wIndex] <- min(tmpRawYWindowCubInterp)
    fsd$ZminCubInterpA[wIndex] <- min(tmpRawZWindowCubInterp)
    fsd$XminCubInterpG[wIndex] <- min(tmpRawAWindowCubInterp)
    fsd$YminCubInterpG[wIndex] <- min(tmpRawBWindowCubInterp)
    fsd$ZminCubInterpG[wIndex] <- min(tmpRawCWindowCubInterp)
    fsd$XminCubInterpO[wIndex] <- min(tmpRawAlphaWindowCubInterp)
    fsd$YminCubInterpO[wIndex] <- min(tmpRawBetaWindowCubInterp)
    fsd$ZminCubInterpO[wIndex] <- min(tmpRawGammaWindowCubInterp)
    
    fsd$XminMeanNormA[wIndex] <- min(tmpRawXWindowMeanNorm)
    fsd$YminMeanNormA[wIndex] <- min(tmpRawYWindowMeanNorm)
    fsd$ZminMeanNormA[wIndex] <- min(tmpRawZWindowMeanNorm)
    fsd$XminMeanNormG[wIndex] <- min(tmpRawAWindowMeanNorm)
    fsd$YminMeanNormG[wIndex] <- min(tmpRawBWindowMeanNorm)
    fsd$ZminMeanNormG[wIndex] <- min(tmpRawCWindowMeanNorm)
    fsd$XminMeanNormO[wIndex] <- min(tmpRawAlphaWindowMeanNorm)
    fsd$YminMeanNormO[wIndex] <- min(tmpRawBetaWindowMeanNorm)
    fsd$ZminMeanNormO[wIndex] <- min(tmpRawGammaWindowMeanNorm)
    
    #min magnitude
    fsd$MminA[wIndex] <- min(tmpRawMagnAWindow)
    fsd$MminG[wIndex] <- min(tmpRawMagnGWindow)
    fsd$MminO[wIndex] <- min(tmpRawMagnOWindow)
    
    fsd$MminLinInterpA[wIndex] <- min(tmpRawMagnAWindowLinInterp)
    fsd$MminLinInterpG[wIndex] <- min(tmpRawMagnGWindowLinInterp)
    fsd$MminLinInterpO[wIndex] <- min(tmpRawMagnOWindowLinInterp)
    
    fsd$MminPolInterpA[wIndex] <- min(tmpRawMagnAWindowPolInterp)
    fsd$MminPolInterpG[wIndex] <- min(tmpRawMagnGWindowPolInterp)
    fsd$MminPolInterpO[wIndex] <- min(tmpRawMagnOWindowPolInterp)
    
    fsd$MminCubInterpA[wIndex] <- min(tmpRawMagnAWindowCubInterp)
    fsd$MminCubInterpG[wIndex] <- min(tmpRawMagnGWindowCubInterp)
    fsd$MminCubInterpO[wIndex] <- min(tmpRawMagnOWindowCubInterp)
    
    fsd$MminMeanNormA[wIndex] <- min(tmpRawMagnAWindowMeanNorm)
    fsd$MminMeanNormG[wIndex] <- min(tmpRawMagnGWindowMeanNorm)
    fsd$MminMeanNormO[wIndex] <- min(tmpRawMagnOWindowMeanNorm)
    
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
    
    fsd$XmaxLinInterpA[wIndex] <- max(tmpRawXWindowLinInterp)
    fsd$YmaxLinInterpA[wIndex] <- max(tmpRawYWindowLinInterp)
    fsd$ZmaxLinInterpA[wIndex] <- max(tmpRawZWindowLinInterp)
    fsd$XmaxLinInterpG[wIndex] <- max(tmpRawAWindowLinInterp)
    fsd$YmaxLinInterpG[wIndex] <- max(tmpRawBWindowLinInterp)
    fsd$ZmaxLinInterpG[wIndex] <- max(tmpRawCWindowLinInterp)
    fsd$XmaxLinInterpO[wIndex] <- max(tmpRawAlphaWindowLinInterp)
    fsd$YmaxLinInterpO[wIndex] <- max(tmpRawBetaWindowLinInterp)
    fsd$ZmaxLinInterpO[wIndex] <- max(tmpRawGammaWindowLinInterp)
    
    fsd$XmaxPolInterpA[wIndex] <- max(tmpRawXWindowPolInterp)
    fsd$YmaxPolInterpA[wIndex] <- max(tmpRawYWindowPolInterp)
    fsd$ZmaxPolInterpA[wIndex] <- max(tmpRawZWindowPolInterp)
    fsd$XmaxPolInterpG[wIndex] <- max(tmpRawAWindowPolInterp)
    fsd$YmaxPolInterpG[wIndex] <- max(tmpRawBWindowPolInterp)
    fsd$ZmaxPolInterpG[wIndex] <- max(tmpRawCWindowPolInterp)
    fsd$XmaxPolInterpO[wIndex] <- max(tmpRawAlphaWindowPolInterp)
    fsd$YmaxPolInterpO[wIndex] <- max(tmpRawBetaWindowPolInterp)
    fsd$ZmaxPolInterpO[wIndex] <- max(tmpRawGammaWindowPolInterp)
    
    fsd$XmaxCubInterpA[wIndex] <- max(tmpRawXWindowCubInterp)
    fsd$YmaxCubInterpA[wIndex] <- max(tmpRawYWindowCubInterp)
    fsd$ZmaxCubInterpA[wIndex] <- max(tmpRawZWindowCubInterp)
    fsd$XmaxCubInterpG[wIndex] <- max(tmpRawAWindowCubInterp)
    fsd$YmaxCubInterpG[wIndex] <- max(tmpRawBWindowCubInterp)
    fsd$ZmaxCubInterpG[wIndex] <- max(tmpRawCWindowCubInterp)
    fsd$XmaxCubInterpO[wIndex] <- max(tmpRawAlphaWindowCubInterp)
    fsd$YmaxCubInterpO[wIndex] <- max(tmpRawBetaWindowCubInterp)
    fsd$ZmaxCubInterpO[wIndex] <- max(tmpRawGammaWindowCubInterp)
    
    fsd$XmaxMeanNormA[wIndex] <- max(tmpRawXWindowMeanNorm)
    fsd$YmaxMeanNormA[wIndex] <- max(tmpRawYWindowMeanNorm)
    fsd$ZmaxMeanNormA[wIndex] <- max(tmpRawZWindowMeanNorm)
    fsd$XmaxMeanNormG[wIndex] <- max(tmpRawAWindowMeanNorm)
    fsd$YmaxMeanNormG[wIndex] <- max(tmpRawBWindowMeanNorm)
    fsd$ZmaxMeanNormG[wIndex] <- max(tmpRawCWindowMeanNorm)
    fsd$XmaxMeanNormO[wIndex] <- max(tmpRawAlphaWindowMeanNorm)
    fsd$YmaxMeanNormO[wIndex] <- max(tmpRawBetaWindowMeanNorm)
    fsd$ZmaxMeanNormO[wIndex] <- max(tmpRawGammaWindowMeanNorm)
    
    #max magnitude
    fsd$MmaxA[wIndex] <- max(tmpRawMagnAWindow)
    fsd$MmaxG[wIndex] <- max(tmpRawMagnGWindow)
    fsd$MmaxO[wIndex] <- max(tmpRawMagnOWindow)
    
    fsd$MmaxLinInterpA[wIndex] <- max(tmpRawMagnAWindowLinInterp)
    fsd$MmaxLinInterpG[wIndex] <- max(tmpRawMagnGWindowLinInterp)
    fsd$MmaxLinInterpO[wIndex] <- max(tmpRawMagnOWindowLinInterp)
    
    fsd$MmaxPolInterpA[wIndex] <- max(tmpRawMagnAWindowPolInterp)
    fsd$MmaxPolInterpG[wIndex] <- max(tmpRawMagnGWindowPolInterp)
    fsd$MmaxPolInterpO[wIndex] <- max(tmpRawMagnOWindowPolInterp)
    
    fsd$MmaxCubInterpA[wIndex] <- max(tmpRawMagnAWindowCubInterp)
    fsd$MmaxCubInterpG[wIndex] <- max(tmpRawMagnGWindowCubInterp)
    fsd$MmaxCubInterpO[wIndex] <- max(tmpRawMagnOWindowCubInterp)
    
    fsd$MmaxMeanNormA[wIndex] <- max(tmpRawMagnAWindowMeanNorm)
    fsd$MmaxMeanNormG[wIndex] <- max(tmpRawMagnGWindowMeanNorm)
    fsd$MmaxMeanNormO[wIndex] <- max(tmpRawMagnOWindowMeanNorm)
    
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
    
    fsd$XmeanLinInterpA[wIndex] <- mean(tmpRawXWindowLinInterp)
    fsd$YmeanLinInterpA[wIndex] <- mean(tmpRawYWindowLinInterp)
    fsd$ZmeanLinInterpA[wIndex] <- mean(tmpRawZWindowLinInterp)
    fsd$XmeanLinInterpG[wIndex] <- mean(tmpRawAWindowLinInterp)
    fsd$YmeanLinInterpG[wIndex] <- mean(tmpRawBWindowLinInterp)
    fsd$ZmeanLinInterpG[wIndex] <- mean(tmpRawCWindowLinInterp)
    fsd$XmeanLinInterpO[wIndex] <- mean(tmpRawAlphaWindowLinInterp)
    fsd$YmeanLinInterpO[wIndex] <- mean(tmpRawBetaWindowLinInterp)
    fsd$ZmeanLinInterpO[wIndex] <- mean(tmpRawGammaWindowLinInterp)
    
    fsd$XmeanPolInterpA[wIndex] <- mean(tmpRawXWindowPolInterp)
    fsd$YmeanPolInterpA[wIndex] <- mean(tmpRawYWindowPolInterp)
    fsd$ZmeanPolInterpA[wIndex] <- mean(tmpRawZWindowPolInterp)
    fsd$XmeanPolInterpG[wIndex] <- mean(tmpRawAWindowPolInterp)
    fsd$YmeanPolInterpG[wIndex] <- mean(tmpRawBWindowPolInterp)
    fsd$ZmeanPolInterpG[wIndex] <- mean(tmpRawCWindowPolInterp)
    fsd$XmeanPolInterpO[wIndex] <- mean(tmpRawAlphaWindowPolInterp)
    fsd$YmeanPolInterpO[wIndex] <- mean(tmpRawBetaWindowPolInterp)
    fsd$ZmeanPolInterpO[wIndex] <- mean(tmpRawGammaWindowPolInterp)
    
    fsd$XmeanCubInterpA[wIndex] <- mean(tmpRawXWindowCubInterp)
    fsd$YmeanCubInterpA[wIndex] <- mean(tmpRawYWindowCubInterp)
    fsd$ZmeanCubInterpA[wIndex] <- mean(tmpRawZWindowCubInterp)
    fsd$XmeanCubInterpG[wIndex] <- mean(tmpRawAWindowCubInterp)
    fsd$YmeanCubInterpG[wIndex] <- mean(tmpRawBWindowCubInterp)
    fsd$ZmeanCubInterpG[wIndex] <- mean(tmpRawCWindowCubInterp)
    fsd$XmeanCubInterpO[wIndex] <- mean(tmpRawAlphaWindowCubInterp)
    fsd$YmeanCubInterpO[wIndex] <- mean(tmpRawBetaWindowCubInterp)
    fsd$ZmeanCubInterpO[wIndex] <- mean(tmpRawGammaWindowCubInterp)
    
    fsd$XmeanMeanNormA[wIndex] <- mean(tmpRawXWindowMeanNorm)
    fsd$YmeanMeanNormA[wIndex] <- mean(tmpRawYWindowMeanNorm)
    fsd$ZmeanMeanNormA[wIndex] <- mean(tmpRawZWindowMeanNorm)
    fsd$XmeanMeanNormG[wIndex] <- mean(tmpRawAWindowMeanNorm)
    fsd$YmeanMeanNormG[wIndex] <- mean(tmpRawBWindowMeanNorm)
    fsd$ZmeanMeanNormG[wIndex] <- mean(tmpRawCWindowMeanNorm)
    fsd$XmeanMeanNormO[wIndex] <- mean(tmpRawAlphaWindowMeanNorm)
    fsd$YmeanMeanNormO[wIndex] <- mean(tmpRawBetaWindowMeanNorm)
    fsd$ZmeanMeanNormO[wIndex] <- mean(tmpRawGammaWindowMeanNorm)
    
    # mean magn
    fsd$MmeanA[wIndex] <- mean(tmpRawMagnAWindow)
    fsd$MmeanG[wIndex] <- mean(tmpRawMagnGWindow)
    fsd$MmeanO[wIndex] <- mean(tmpRawMagnOWindow)
    
    fsd$MmeanLinInterpA[wIndex] <- mean(tmpRawMagnAWindowLinInterp)
    fsd$MmeanLinInterpG[wIndex] <- mean(tmpRawMagnGWindowLinInterp)
    fsd$MmeanLinInterpO[wIndex] <- mean(tmpRawMagnOWindowLinInterp)
    
    fsd$MmeanPolInterpA[wIndex] <- mean(tmpRawMagnAWindowPolInterp)
    fsd$MmeanPolInterpG[wIndex] <- mean(tmpRawMagnGWindowPolInterp)
    fsd$MmeanPolInterpO[wIndex] <- mean(tmpRawMagnOWindowPolInterp)
    
    fsd$MmeanCubInterpA[wIndex] <- mean(tmpRawMagnAWindowCubInterp)
    fsd$MmeanCubInterpG[wIndex] <- mean(tmpRawMagnGWindowCubInterp)
    fsd$MmeanCubInterpO[wIndex] <- mean(tmpRawMagnOWindowCubInterp)
    
    fsd$MmeanMeanNormA[wIndex] <- mean(tmpRawMagnAWindowMeanNorm)
    fsd$MmeanMeanNormG[wIndex] <- mean(tmpRawMagnGWindowMeanNorm)
    fsd$MmeanMeanNormO[wIndex] <- mean(tmpRawMagnOWindowMeanNorm)
    
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
    
    fsd$XmedianLinInterpA[wIndex] <- median(tmpRawXWindowLinInterp)
    fsd$YmedianLinInterpA[wIndex] <- median(tmpRawYWindowLinInterp)
    fsd$ZmedianLinInterpA[wIndex] <- median(tmpRawZWindowLinInterp)
    fsd$XmedianLinInterpG[wIndex] <- median(tmpRawAWindowLinInterp)
    fsd$YmedianLinInterpG[wIndex] <- median(tmpRawBWindowLinInterp)
    fsd$ZmedianLinInterpG[wIndex] <- median(tmpRawCWindowLinInterp)
    fsd$XmedianLinInterpO[wIndex] <- median(tmpRawAlphaWindowLinInterp)
    fsd$YmedianLinInterpO[wIndex] <- median(tmpRawBetaWindowLinInterp)
    fsd$ZmedianLinInterpO[wIndex] <- median(tmpRawGammaWindowLinInterp)
    
    fsd$XmedianPolInterpA[wIndex] <- median(tmpRawXWindowPolInterp)
    fsd$YmedianPolInterpA[wIndex] <- median(tmpRawYWindowPolInterp)
    fsd$ZmedianPolInterpA[wIndex] <- median(tmpRawZWindowPolInterp)
    fsd$XmedianPolInterpG[wIndex] <- median(tmpRawAWindowPolInterp)
    fsd$YmedianPolInterpG[wIndex] <- median(tmpRawBWindowPolInterp)
    fsd$ZmedianPolInterpG[wIndex] <- median(tmpRawCWindowPolInterp)
    fsd$XmedianPolInterpO[wIndex] <- median(tmpRawAlphaWindowPolInterp)
    fsd$YmedianPolInterpO[wIndex] <- median(tmpRawBetaWindowPolInterp)
    fsd$ZmedianPolInterpO[wIndex] <- median(tmpRawGammaWindowPolInterp)
    
    fsd$XmedianCubInterpA[wIndex] <- median(tmpRawXWindowCubInterp)
    fsd$YmedianCubInterpA[wIndex] <- median(tmpRawYWindowCubInterp)
    fsd$ZmedianCubInterpA[wIndex] <- median(tmpRawZWindowCubInterp)
    fsd$XmedianCubInterpG[wIndex] <- median(tmpRawAWindowCubInterp)
    fsd$YmedianCubInterpG[wIndex] <- median(tmpRawBWindowCubInterp)
    fsd$ZmedianCubInterpG[wIndex] <- median(tmpRawCWindowCubInterp)
    fsd$XmedianCubInterpO[wIndex] <- median(tmpRawAlphaWindowCubInterp)
    fsd$YmedianCubInterpO[wIndex] <- median(tmpRawBetaWindowCubInterp)
    fsd$ZmedianCubInterpO[wIndex] <- median(tmpRawGammaWindowCubInterp)
    
    fsd$XmedianMeanNormA[wIndex] <- median(tmpRawXWindowMeanNorm)
    fsd$YmedianMeanNormA[wIndex] <- median(tmpRawYWindowMeanNorm)
    fsd$ZmedianMeanNormA[wIndex] <- median(tmpRawZWindowMeanNorm)
    fsd$XmedianMeanNormG[wIndex] <- median(tmpRawAWindowMeanNorm)
    fsd$YmedianMeanNormG[wIndex] <- median(tmpRawBWindowMeanNorm)
    fsd$ZmedianMeanNormG[wIndex] <- median(tmpRawCWindowMeanNorm)
    fsd$XmedianMeanNormO[wIndex] <- median(tmpRawAlphaWindowMeanNorm)
    fsd$YmedianMeanNormO[wIndex] <- median(tmpRawBetaWindowMeanNorm)
    fsd$ZmedianMeanNormO[wIndex] <- median(tmpRawGammaWindowMeanNorm)
    
    # median magn
    fsd$MmedianA[wIndex] <- median(tmpRawMagnAWindow)
    fsd$MmedianG[wIndex] <- median(tmpRawMagnGWindow)
    fsd$MmedianO[wIndex] <- median(tmpRawMagnOWindow)
    
    fsd$MmedianLinInterpA[wIndex] <- median(tmpRawMagnAWindowLinInterp)
    fsd$MmedianLinInterpG[wIndex] <- median(tmpRawMagnGWindowLinInterp)
    fsd$MmedianLinInterpO[wIndex] <- median(tmpRawMagnOWindowLinInterp)
    
    fsd$MmedianPolInterpA[wIndex] <- median(tmpRawMagnAWindowPolInterp)
    fsd$MmedianPolInterpG[wIndex] <- median(tmpRawMagnGWindowPolInterp)
    fsd$MmedianPolInterpO[wIndex] <- median(tmpRawMagnOWindowPolInterp)
    
    fsd$MmedianCubInterpA[wIndex] <- median(tmpRawMagnAWindowCubInterp)
    fsd$MmedianCubInterpG[wIndex] <- median(tmpRawMagnGWindowCubInterp)
    fsd$MmedianCubInterpO[wIndex] <- median(tmpRawMagnOWindowCubInterp)
    
    fsd$MmedianMeanNormA[wIndex] <- median(tmpRawMagnAWindowMeanNorm)
    fsd$MmedianMeanNormG[wIndex] <- median(tmpRawMagnGWindowMeanNorm)
    fsd$MmedianMeanNormO[wIndex] <- median(tmpRawMagnOWindowMeanNorm)
    
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
    
    fsd$XsdLinInterpA[wIndex] <- sd(tmpRawXWindowLinInterp)
    fsd$YsdLinInterpA[wIndex] <- sd(tmpRawYWindowLinInterp)
    fsd$ZsdLinInterpA[wIndex] <- sd(tmpRawZWindowLinInterp)
    fsd$XsdLinInterpG[wIndex] <- sd(tmpRawAWindowLinInterp)
    fsd$YsdLinInterpG[wIndex] <- sd(tmpRawBWindowLinInterp)
    fsd$ZsdLinInterpG[wIndex] <- sd(tmpRawCWindowLinInterp)
    fsd$XsdLinInterpO[wIndex] <- sd(tmpRawAlphaWindowLinInterp)
    fsd$YsdLinInterpO[wIndex] <- sd(tmpRawBetaWindowLinInterp)
    fsd$ZsdLinInterpO[wIndex] <- sd(tmpRawGammaWindowLinInterp)
    
    fsd$XsdPolInterpA[wIndex] <- sd(tmpRawXWindowPolInterp)
    fsd$YsdPolInterpA[wIndex] <- sd(tmpRawYWindowPolInterp)
    fsd$ZsdPolInterpA[wIndex] <- sd(tmpRawZWindowPolInterp)
    fsd$XsdPolInterpG[wIndex] <- sd(tmpRawAWindowPolInterp)
    fsd$YsdPolInterpG[wIndex] <- sd(tmpRawBWindowPolInterp)
    fsd$ZsdPolInterpG[wIndex] <- sd(tmpRawCWindowPolInterp)
    fsd$XsdPolInterpO[wIndex] <- sd(tmpRawAlphaWindowPolInterp)
    fsd$YsdPolInterpO[wIndex] <- sd(tmpRawBetaWindowPolInterp)
    fsd$ZsdPolInterpO[wIndex] <- sd(tmpRawGammaWindowPolInterp)
    
    fsd$XsdCubInterpA[wIndex] <- sd(tmpRawXWindowCubInterp)
    fsd$YsdCubInterpA[wIndex] <- sd(tmpRawYWindowCubInterp)
    fsd$ZsdCubInterpA[wIndex] <- sd(tmpRawZWindowCubInterp)
    fsd$XsdCubInterpG[wIndex] <- sd(tmpRawAWindowCubInterp)
    fsd$YsdCubInterpG[wIndex] <- sd(tmpRawBWindowCubInterp)
    fsd$ZsdCubInterpG[wIndex] <- sd(tmpRawCWindowCubInterp)
    fsd$XsdCubInterpO[wIndex] <- sd(tmpRawAlphaWindowCubInterp)
    fsd$YsdCubInterpO[wIndex] <- sd(tmpRawBetaWindowCubInterp)
    fsd$ZsdCubInterpO[wIndex] <- sd(tmpRawGammaWindowCubInterp)
    
    fsd$XsdMeanNormA[wIndex] <- sd(tmpRawXWindowMeanNorm)
    fsd$YsdMeanNormA[wIndex] <- sd(tmpRawYWindowMeanNorm)
    fsd$ZsdMeanNormA[wIndex] <- sd(tmpRawZWindowMeanNorm)
    fsd$XsdMeanNormG[wIndex] <- sd(tmpRawAWindowMeanNorm)
    fsd$YsdMeanNormG[wIndex] <- sd(tmpRawBWindowMeanNorm)
    fsd$ZsdMeanNormG[wIndex] <- sd(tmpRawCWindowMeanNorm)
    fsd$XsdMeanNormO[wIndex] <- sd(tmpRawAlphaWindowMeanNorm)
    fsd$YsdMeanNormO[wIndex] <- sd(tmpRawBetaWindowMeanNorm)
    fsd$ZsdMeanNormO[wIndex] <- sd(tmpRawGammaWindowMeanNorm)
    
    #sd magn
    fsd$MsdA[wIndex] <- sd(tmpRawMagnAWindow)
    fsd$MsdG[wIndex] <- sd(tmpRawMagnGWindow)
    fsd$MsdO[wIndex] <- sd(tmpRawMagnOWindow)
    
    fsd$MsdLinInterpA[wIndex] <- sd(tmpRawMagnAWindowLinInterp)
    fsd$MsdLinInterpG[wIndex] <- sd(tmpRawMagnGWindowLinInterp)
    fsd$MsdLinInterpO[wIndex] <- sd(tmpRawMagnOWindowLinInterp)
    
    fsd$MsdPolInterpA[wIndex] <- sd(tmpRawMagnAWindowPolInterp)
    fsd$MsdPolInterpG[wIndex] <- sd(tmpRawMagnGWindowPolInterp)
    fsd$MsdPolInterpO[wIndex] <- sd(tmpRawMagnOWindowPolInterp)
    
    fsd$MsdCubInterpA[wIndex] <- sd(tmpRawMagnAWindowCubInterp)
    fsd$MsdCubInterpG[wIndex] <- sd(tmpRawMagnGWindowCubInterp)
    fsd$MsdCubInterpO[wIndex] <- sd(tmpRawMagnOWindowCubInterp)
    
    fsd$MsdMeanNormA[wIndex] <- sd(tmpRawMagnAWindowMeanNorm)
    fsd$MsdMeanNormG[wIndex] <- sd(tmpRawMagnGWindowMeanNorm)
    fsd$MsdMeanNormO[wIndex] <- sd(tmpRawMagnOWindowMeanNorm)
    
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
    
    fsd$XvarLinInterpA[wIndex] <- var(tmpRawXWindowLinInterp)
    fsd$YvarLinInterpA[wIndex] <- var(tmpRawYWindowLinInterp)
    fsd$ZvarLinInterpA[wIndex] <- var(tmpRawZWindowLinInterp)
    fsd$XvarLinInterpG[wIndex] <- var(tmpRawAWindowLinInterp)
    fsd$YvarLinInterpG[wIndex] <- var(tmpRawBWindowLinInterp)
    fsd$ZvarLinInterpG[wIndex] <- var(tmpRawCWindowLinInterp)
    fsd$XvarLinInterpO[wIndex] <- var(tmpRawAlphaWindowLinInterp)
    fsd$YvarLinInterpO[wIndex] <- var(tmpRawBetaWindowLinInterp)
    fsd$ZvarLinInterpO[wIndex] <- var(tmpRawGammaWindowLinInterp)
    
    fsd$XvarPolInterpA[wIndex] <- var(tmpRawXWindowPolInterp)
    fsd$YvarPolInterpA[wIndex] <- var(tmpRawYWindowPolInterp)
    fsd$ZvarPolInterpA[wIndex] <- var(tmpRawZWindowPolInterp)
    fsd$XvarPolInterpG[wIndex] <- var(tmpRawAWindowPolInterp)
    fsd$YvarPolInterpG[wIndex] <- var(tmpRawBWindowPolInterp)
    fsd$ZvarPolInterpG[wIndex] <- var(tmpRawCWindowPolInterp)
    fsd$XvarPolInterpO[wIndex] <- var(tmpRawAlphaWindowPolInterp)
    fsd$YvarPolInterpO[wIndex] <- var(tmpRawBetaWindowPolInterp)
    fsd$ZvarPolInterpO[wIndex] <- var(tmpRawGammaWindowPolInterp)
    
    fsd$XvarCubInterpA[wIndex] <- var(tmpRawXWindowCubInterp)
    fsd$YvarCubInterpA[wIndex] <- var(tmpRawYWindowCubInterp)
    fsd$ZvarCubInterpA[wIndex] <- var(tmpRawZWindowCubInterp)
    fsd$XvarCubInterpG[wIndex] <- var(tmpRawAWindowCubInterp)
    fsd$YvarCubInterpG[wIndex] <- var(tmpRawBWindowCubInterp)
    fsd$ZvarCubInterpG[wIndex] <- var(tmpRawCWindowCubInterp)
    fsd$XvarCubInterpO[wIndex] <- var(tmpRawAlphaWindowCubInterp)
    fsd$YvarCubInterpO[wIndex] <- var(tmpRawBetaWindowCubInterp)
    fsd$ZvarCubInterpO[wIndex] <- var(tmpRawGammaWindowCubInterp)
    
    fsd$XvarMeanNormA[wIndex] <- var(tmpRawXWindowMeanNorm)
    fsd$YvarMeanNormA[wIndex] <- var(tmpRawYWindowMeanNorm)
    fsd$ZvarMeanNormA[wIndex] <- var(tmpRawZWindowMeanNorm)
    fsd$XvarMeanNormG[wIndex] <- var(tmpRawAWindowMeanNorm)
    fsd$YvarMeanNormG[wIndex] <- var(tmpRawBWindowMeanNorm)
    fsd$ZvarMeanNormG[wIndex] <- var(tmpRawCWindowMeanNorm)
    fsd$XvarMeanNormO[wIndex] <- var(tmpRawAlphaWindowMeanNorm)
    fsd$YvarMeanNormO[wIndex] <- var(tmpRawBetaWindowMeanNorm)
    fsd$ZvarMeanNormO[wIndex] <- var(tmpRawGammaWindowMeanNorm)
    
    #var magn
    fsd$MvarA[wIndex] <- var(tmpRawMagnAWindow)
    fsd$MvarG[wIndex] <- var(tmpRawMagnGWindow)
    fsd$MvarO[wIndex] <- var(tmpRawMagnOWindow)
    
    fsd$MvarLinInterpA[wIndex] <- var(tmpRawMagnAWindowLinInterp)
    fsd$MvarLinInterpG[wIndex] <- var(tmpRawMagnGWindowLinInterp)
    fsd$MvarLinInterpO[wIndex] <- var(tmpRawMagnOWindowLinInterp)
    
    fsd$MvarPolInterpA[wIndex] <- var(tmpRawMagnAWindowPolInterp)
    fsd$MvarPolInterpG[wIndex] <- var(tmpRawMagnGWindowPolInterp)
    fsd$MvarPolInterpO[wIndex] <- var(tmpRawMagnOWindowPolInterp)
    
    fsd$MvarCubInterpA[wIndex] <- var(tmpRawMagnAWindowCubInterp)
    fsd$MvarCubInterpG[wIndex] <- var(tmpRawMagnGWindowCubInterp)
    fsd$MvarCubInterpO[wIndex] <- var(tmpRawMagnOWindowCubInterp)
    
    fsd$MvarMeanNormA[wIndex] <- var(tmpRawMagnAWindowMeanNorm)
    fsd$MvarMeanNormG[wIndex] <- var(tmpRawMagnGWindowMeanNorm)
    fsd$MvarMeanNormO[wIndex] <- var(tmpRawMagnOWindowMeanNorm)
    
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
    
    fsd$XrmsLinInterpA[wIndex] <- sqrt(sum((tmpRawXWindowLinInterp) ^ 2) / wSize)
    fsd$YrmsLinInterpA[wIndex] <- sqrt(sum((tmpRawYWindowLinInterp) ^ 2) / wSize)
    fsd$ZrmsLinInterpA[wIndex] <- sqrt(sum((tmpRawZWindowLinInterp) ^ 2) / wSize)
    fsd$XrmsLinInterpG[wIndex] <- sqrt(sum((tmpRawAWindowLinInterp) ^ 2) / wSize)
    fsd$YrmsLinInterpG[wIndex] <- sqrt(sum((tmpRawBWindowLinInterp) ^ 2) / wSize)
    fsd$ZrmsLinInterpG[wIndex] <- sqrt(sum((tmpRawCWindowLinInterp) ^ 2) / wSize)
    fsd$XrmsLinInterpO[wIndex] <- sqrt(sum((tmpRawAlphaWindowLinInterp) ^ 2) / wSize)
    fsd$YrmsLinInterpO[wIndex] <- sqrt(sum((tmpRawBetaWindowLinInterp) ^ 2) / wSize)
    fsd$ZrmsLinInterpO[wIndex] <- sqrt(sum((tmpRawGammaWindowLinInterp) ^ 2) / wSize)
    
    fsd$XrmsPolInterpA[wIndex] <- sqrt(sum((tmpRawXWindowPolInterp) ^ 2) / wSize)
    fsd$YrmsPolInterpA[wIndex] <- sqrt(sum((tmpRawYWindowPolInterp) ^ 2) / wSize)
    fsd$ZrmsPolInterpA[wIndex] <- sqrt(sum((tmpRawZWindowPolInterp) ^ 2) / wSize)
    fsd$XrmsPolInterpG[wIndex] <- sqrt(sum((tmpRawAWindowPolInterp) ^ 2) / wSize)
    fsd$YrmsPolInterpG[wIndex] <- sqrt(sum((tmpRawBWindowPolInterp) ^ 2) / wSize)
    fsd$ZrmsPolInterpG[wIndex] <- sqrt(sum((tmpRawCWindowPolInterp) ^ 2) / wSize)
    fsd$XrmsPolInterpO[wIndex] <- sqrt(sum((tmpRawAlphaWindowPolInterp) ^ 2) / wSize)
    fsd$YrmsPolInterpO[wIndex] <- sqrt(sum((tmpRawBetaWindowPolInterp) ^ 2) / wSize)
    fsd$ZrmsPolInterpO[wIndex] <- sqrt(sum((tmpRawGammaWindowPolInterp) ^ 2) / wSize)
    
    fsd$XrmsCubInterpA[wIndex] <- sqrt(sum((tmpRawXWindowCubInterp) ^ 2) / wSize)
    fsd$YrmsCubInterpA[wIndex] <- sqrt(sum((tmpRawYWindowCubInterp) ^ 2) / wSize)
    fsd$ZrmsCubInterpA[wIndex] <- sqrt(sum((tmpRawZWindowCubInterp) ^ 2) / wSize)
    fsd$XrmsCubInterpG[wIndex] <- sqrt(sum((tmpRawAWindowCubInterp) ^ 2) / wSize)
    fsd$YrmsCubInterpG[wIndex] <- sqrt(sum((tmpRawBWindowCubInterp) ^ 2) / wSize)
    fsd$ZrmsCubInterpG[wIndex] <- sqrt(sum((tmpRawCWindowCubInterp) ^ 2) / wSize)
    fsd$XrmsCubInterpO[wIndex] <- sqrt(sum((tmpRawAlphaWindowCubInterp) ^ 2) / wSize)
    fsd$YrmsCubInterpO[wIndex] <- sqrt(sum((tmpRawBetaWindowCubInterp) ^ 2) / wSize)
    fsd$ZrmsCubInterpO[wIndex] <- sqrt(sum((tmpRawGammaWindowCubInterp) ^ 2) / wSize)
    
    fsd$XrmsMeanNormA[wIndex] <- sqrt(sum(((tmpRawXWindowMeanNorm) ^ 2)) / wSize)
    fsd$YrmsMeanNormA[wIndex] <- sqrt(sum(((tmpRawYWindowMeanNorm) ^ 2)) / wSize)
    fsd$ZrmsMeanNormA[wIndex] <- sqrt(sum(((tmpRawZWindowMeanNorm) ^ 2)) / wSize)
    fsd$XrmsMeanNormG[wIndex] <- sqrt(sum(((tmpRawAWindowMeanNorm) ^ 2)) / wSize)
    fsd$YrmsMeanNormG[wIndex] <- sqrt(sum(((tmpRawBWindowMeanNorm) ^ 2)) / wSize)
    fsd$ZrmsMeanNormG[wIndex] <- sqrt(sum(((tmpRawCWindowMeanNorm) ^ 2)) / wSize)
    fsd$XrmsMeanNormO[wIndex] <- sqrt(sum(((tmpRawAlphaWindowMeanNorm) ^ 2)) / wSize)
    fsd$YrmsMeanNormO[wIndex] <- sqrt(sum(((tmpRawBetaWindowMeanNorm) ^ 2)) / wSize)
    fsd$ZrmsMeanNormO[wIndex] <- sqrt(sum(((tmpRawGammaWindowMeanNorm) ^ 2)) / wSize)
    
    # Root Mean Square of the magnitude
    fsd$MagnRmsA[wIndex] <- sqrt((sum((tmpRawMagnAWindow) ^ 2)) / wSize)
    fsd$MagnRmsG[wIndex] <- sqrt((sum((tmpRawMagnGWindow) ^ 2)) / wSize)
    fsd$MagnRmsO[wIndex] <- sqrt((sum((tmpRawMagnOWindow) ^ 2)) / wSize)
    
    fsd$MagnRmsLinInterpA[wIndex] <- sqrt((sum((tmpRawMagnAWindowLinInterp) ^ 2)) / wSize)
    fsd$MagnRmsLinInterpG[wIndex] <- sqrt((sum((tmpRawMagnGWindowLinInterp) ^ 2)) / wSize)
    fsd$MagnRmsLinInterpO[wIndex] <- sqrt((sum((tmpRawMagnOWindowLinInterp) ^ 2)) / wSize)
    
    fsd$MagnRmsPolInterpA[wIndex] <- sqrt((sum((tmpRawMagnAWindowPolInterp) ^ 2)) / wSize)
    fsd$MagnRmsPolInterpG[wIndex] <- sqrt((sum((tmpRawMagnGWindowPolInterp) ^ 2)) / wSize)
    fsd$MagnRmsPolInterpO[wIndex] <- sqrt((sum((tmpRawMagnOWindowPolInterp) ^ 2)) / wSize)
    
    fsd$MagnRmsCubInterpA[wIndex] <- sqrt((sum((tmpRawMagnAWindowCubInterp) ^ 2)) / wSize)
    fsd$MagnRmsCubInterpG[wIndex] <- sqrt((sum((tmpRawMagnGWindowCubInterp) ^ 2)) / wSize)
    fsd$MagnRmsCubInterpO[wIndex] <- sqrt((sum((tmpRawMagnOWindowCubInterp) ^ 2)) / wSize)
    
    fsd$MrmsMeanNormA[wIndex] <- sqrt(sum(((tmpRawMagnAWindowMeanNorm) ^ 2)) / wSize)
    fsd$MrmsMeanNormG[wIndex] <- sqrt(sum(((tmpRawMagnGWindowMeanNorm) ^ 2)) / wSize)
    fsd$MrmsMeanNormO[wIndex] <- sqrt(sum(((tmpRawMagnOWindowMeanNorm) ^ 2)) / wSize)
    
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

fsd$XskewLinInterpA <- 3 * (fsd$XmeanLinInterpA - fsd$XmedianLinInterpA) / fsd$XsdLinInterpA
fsd$YskewLinInterpA <- 3 * (fsd$YmeanLinInterpA - fsd$YmedianLinInterpA) / fsd$YsdLinInterpA
fsd$ZskewLinInterpA <- 3 * (fsd$ZmeanLinInterpA - fsd$ZmedianLinInterpA) / fsd$ZsdLinInterpA
fsd$XskewLinInterpG <- 3 * (fsd$XmeanLinInterpG - fsd$XmedianLinInterpG) / fsd$XsdLinInterpG
fsd$YskewLinInterpG <- 3 * (fsd$YmeanLinInterpG - fsd$YmedianLinInterpG) / fsd$YsdLinInterpG
fsd$ZskewLinInterpG <- 3 * (fsd$ZmeanLinInterpG - fsd$ZmedianLinInterpG) / fsd$ZsdLinInterpG
fsd$XskewLinInterpO <- 3 * (fsd$XmeanLinInterpO - fsd$XmedianLinInterpO) / fsd$XsdLinInterpO
fsd$YskewLinInterpO <- 3 * (fsd$YmeanLinInterpO - fsd$YmedianLinInterpO) / fsd$YsdLinInterpO
fsd$ZskewLinInterpO <- 3 * (fsd$ZmeanLinInterpO - fsd$ZmedianLinInterpO) / fsd$ZsdLinInterpO

fsd$XskewPolInterpA <- 3 * (fsd$XmeanPolInterpA - fsd$XmedianPolInterpA) / fsd$XsdPolInterpA
fsd$YskewPolInterpA <- 3 * (fsd$YmeanPolInterpA - fsd$YmedianPolInterpA) / fsd$YsdPolInterpA
fsd$ZskewPolInterpA <- 3 * (fsd$ZmeanPolInterpA - fsd$ZmedianPolInterpA) / fsd$ZsdPolInterpA
fsd$XskewPolInterpG <- 3 * (fsd$XmeanPolInterpG - fsd$XmedianPolInterpG) / fsd$XsdPolInterpG
fsd$YskewPolInterpG <- 3 * (fsd$YmeanPolInterpG - fsd$YmedianPolInterpG) / fsd$YsdPolInterpG
fsd$ZskewPolInterpG <- 3 * (fsd$ZmeanPolInterpG - fsd$ZmedianPolInterpG) / fsd$ZsdPolInterpG
fsd$XskewPolInterpO <- 3 * (fsd$XmeanPolInterpO - fsd$XmedianPolInterpO) / fsd$XsdPolInterpO
fsd$YskewPolInterpO <- 3 * (fsd$YmeanPolInterpO - fsd$YmedianPolInterpO) / fsd$YsdPolInterpO
fsd$ZskewPolInterpO <- 3 * (fsd$ZmeanPolInterpO - fsd$ZmedianPolInterpO) / fsd$ZsdPolInterpO

fsd$XskewCubInterpA <- 3 * (fsd$XmeanCubInterpA - fsd$XmedianCubInterpA) / fsd$XsdCubInterpA
fsd$YskewCubInterpA <- 3 * (fsd$YmeanCubInterpA - fsd$YmedianCubInterpA) / fsd$YsdCubInterpA
fsd$ZskewCubInterpA <- 3 * (fsd$ZmeanCubInterpA - fsd$ZmedianCubInterpA) / fsd$ZsdCubInterpA
fsd$XskewCubInterpG <- 3 * (fsd$XmeanCubInterpG - fsd$XmedianCubInterpG) / fsd$XsdCubInterpG
fsd$YskewCubInterpG <- 3 * (fsd$YmeanCubInterpG - fsd$YmedianCubInterpG) / fsd$YsdCubInterpG
fsd$ZskewCubInterpG <- 3 * (fsd$ZmeanCubInterpG - fsd$ZmedianCubInterpG) / fsd$ZsdCubInterpG
fsd$XskewCubInterpO <- 3 * (fsd$XmeanCubInterpO - fsd$XmedianCubInterpO) / fsd$XsdCubInterpO
fsd$YskewCubInterpO <- 3 * (fsd$YmeanCubInterpO - fsd$YmedianCubInterpO) / fsd$YsdCubInterpO
fsd$ZskewCubInterpO <- 3 * (fsd$ZmeanCubInterpO - fsd$ZmedianCubInterpO) / fsd$ZsdCubInterpO

fsd$XskewMeanNormA <- 3 * (fsd$XmeanMeanNormA - fsd$XmedianMeanNormA) / fsd$XsdMeanNormA
fsd$YskewMeanNormA <- 3 * (fsd$YmeanMeanNormA - fsd$YmedianMeanNormA) / fsd$YsdMeanNormA
fsd$ZskewMeanNormA <- 3 * (fsd$ZmeanMeanNormA - fsd$ZmedianMeanNormA) / fsd$ZsdMeanNormA
fsd$XskewMeanNormG <- 3 * (fsd$XmeanMeanNormG - fsd$XmedianMeanNormG) / fsd$XsdMeanNormG
fsd$YskewMeanNormG <- 3 * (fsd$YmeanMeanNormG - fsd$YmedianMeanNormG) / fsd$YsdMeanNormG
fsd$ZskewMeanNormG <- 3 * (fsd$ZmeanMeanNormG - fsd$ZmedianMeanNormG) / fsd$ZsdMeanNormG
fsd$XskewMeanNormO <- 3 * (fsd$XmeanMeanNormO - fsd$XmedianMeanNormO) / fsd$XsdMeanNormO
fsd$YskewMeanNormO <- 3 * (fsd$YmeanMeanNormO - fsd$YmedianMeanNormO) / fsd$YsdMeanNormO
fsd$ZskewMeanNormO <- 3 * (fsd$ZmeanMeanNormO - fsd$ZmedianMeanNormO) / fsd$ZsdMeanNormO

# skewness magn
fsd$MskewA <- 3 * (fsd$MmeanA - fsd$MmedianA) / fsd$MsdA
fsd$MskewG <- 3 * (fsd$MmeanG - fsd$MmedianG) / fsd$MsdG
fsd$MskewO <- 3 * (fsd$MmeanO - fsd$MmedianO) / fsd$MsdO

fsd$MskewLinInterpA <- 3 * (fsd$MmeanLinInterpA - fsd$MmedianLinInterpA) / fsd$MsdLinInterpA
fsd$MskewLinInterpG <- 3 * (fsd$MmeanLinInterpG - fsd$MmedianLinInterpG) / fsd$MsdLinInterpG
fsd$MskewLinInterpO <- 3 * (fsd$MmeanLinInterpO - fsd$MmedianLinInterpO) / fsd$MsdLinInterpO

fsd$MskewPolInterpA <- 3 * (fsd$MmeanPolInterpA - fsd$MmedianPolInterpA) / fsd$MsdPolInterpA
fsd$MskewPolInterpG <- 3 * (fsd$MmeanPolInterpG - fsd$MmedianPolInterpG) / fsd$MsdPolInterpG
fsd$MskewPolInterpO <- 3 * (fsd$MmeanPolInterpO - fsd$MmedianPolInterpO) / fsd$MsdPolInterpO

fsd$MskewCubInterpA <- 3 * (fsd$MmeanCubInterpA - fsd$MmedianCubInterpA) / fsd$MsdCubInterpA
fsd$MskewCubInterpG <- 3 * (fsd$MmeanCubInterpG - fsd$MmedianCubInterpG) / fsd$MsdCubInterpG
fsd$MskewCubInterpO <- 3 * (fsd$MmeanCubInterpO - fsd$MmedianCubInterpO) / fsd$MsdCubInterpO

fsd$MskewMeanNormA <- 3 * (fsd$MmeanMeanNormA - fsd$MmedianMeanNormA) / fsd$MsdMeanNormA
fsd$MskewMeanNormG <- 3 * (fsd$MmeanMeanNormG - fsd$MmedianMeanNormG) / fsd$MsdMeanNormG
fsd$MskewMeanNormO <- 3 * (fsd$MmeanMeanNormO - fsd$MmedianMeanNormO) / fsd$MsdMeanNormO

write.csv(
  fsd,
  "C:\\git\\data-thesis\\R\\datasets\\17011020-dataset-test-wsize-median.csv",
  row.names = FALSE
)