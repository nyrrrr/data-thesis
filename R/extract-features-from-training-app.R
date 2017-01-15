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

options(digits=20)
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

# # slope dy/dx
# sensortrain$XslopeA[1] <- sensortrain$xA[1]
# sensortrain$XslopeA[2:length(sensortrain$Timestamp)] <- (((sensortrain$Timestamp[-1] - sensortrain$Timestamp[-length(sensortrain$Timestamp)]))/1000000000)/(sensortrain$xA[-1] - sensortrain$xA[-length(sensortrain$xA)])
# sensortrain$YslopeA[1] <- sensortrain$yA[1]
# sensortrain$YslopeA[2:length(sensortrain$Timestamp)] <- (((sensortrain$Timestamp[-1] - sensortrain$Timestamp[-length(sensortrain$Timestamp)]))/1000000000)/(sensortrain$yA[-1] - sensortrain$yA[-length(sensortrain$yA)])
# sensortrain$ZslopeA[1] <- sensortrain$zA[1]
# sensortrain$ZslopeA[2:length(sensortrain$Timestamp)] <- (((sensortrain$Timestamp[-1] - sensortrain$Timestamp[-length(sensortrain$Timestamp)]))/1000000000)/(sensortrain$zA[-1] - sensortrain$zA[-length(sensortrain$zA)])
# sensortrain$XslopeG[1] <- sensortrain$xG[1]
# sensortrain$XslopeG[2:length(sensortrain$Timestamp)] <- (((sensortrain$Timestamp[-1] - sensortrain$Timestamp[-length(sensortrain$Timestamp)]))/1000000000)/(sensortrain$xG[-1] - sensortrain$xG[-length(sensortrain$xG)])
# sensortrain$YslopeG[1] <- sensortrain$yG[1]
# sensortrain$YslopeG[2:length(sensortrain$Timestamp)] <- (((sensortrain$Timestamp[-1] - sensortrain$Timestamp[-length(sensortrain$Timestamp)]))/1000000000)/(sensortrain$yG[-1] - sensortrain$yG[-length(sensortrain$yG)])
# sensortrain$ZslopeG[1] <- sensortrain$zG[1]
# sensortrain$ZslopeG[2:length(sensortrain$Timestamp)] <- (((sensortrain$Timestamp[-1] - sensortrain$Timestamp[-length(sensortrain$Timestamp)]))/1000000000)/(sensortrain$zG[-1] - sensortrain$zG[-length(sensortrain$zG)])
# sensortrain$XslopeO[1] <- sensortrain$alpha[1]
# sensortrain$XslopeO[2:length(sensortrain$Timestamp)] <- (((sensortrain$Timestamp[-1] - sensortrain$Timestamp[-length(sensortrain$Timestamp)]))/1000000000)/(sensortrain$alpha[-1] - sensortrain$alpha[-length(sensortrain$alpha)])
# sensortrain$YslopeO[1] <- sensortrain$beta[1]
# sensortrain$YslopeO[2:length(sensortrain$Timestamp)] <- (((sensortrain$Timestamp[-1] - sensortrain$Timestamp[-length(sensortrain$Timestamp)]))/1000000000)/(sensortrain$beta[-1] - sensortrain$beta[-length(sensortrain$beta)])
# sensortrain$ZslopeO[1] <- sensortrain$gamma[1]
# sensortrain$ZslopeO[2:length(sensortrain$Timestamp)] <- (((sensortrain$Timestamp[-1] - sensortrain$Timestamp[-length(sensortrain$Timestamp)]))/1000000000)/(sensortrain$gamma[-1] - sensortrain$gamma[-length(sensortrain$gamma)])
# 
# sensortrain$XslopeA[sensortrain$XslopeA == Inf] <- 0
# sensortrain$YslopeA[sensortrain$YslopeA == Inf] <- 0
# sensortrain$ZslopeA[sensortrain$ZslopeA == Inf] <- 0
# sensortrain$XslopeG[sensortrain$XslopeG == Inf] <- 0
# sensortrain$YslopeG[sensortrain$YslopeG == Inf] <- 0
# sensortrain$ZslopeG[sensortrain$ZslopeG == Inf] <- 0
# sensortrain$XslopeO[sensortrain$XslopeO == Inf] <- 0
# sensortrain$YslopeO[sensortrain$YslopeO == Inf] <- 0
# sensortrain$ZslopeO[sensortrain$ZslopeO == Inf] <- 0

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

# # distance between Value and previous value
# sensortrain$XdeltaA[1] <- sensortrain$xA[1]
# sensortrain$YdeltaA[1] <- sensortrain$yA[1]
# sensortrain$ZdeltaA[1] <- sensortrain$zA[1]
# sensortrain$XdeltaG[1] <- sensortrain$xG[1]
# sensortrain$YdeltaG[1] <- sensortrain$yG[1]
# sensortrain$ZdeltaG[1] <- sensortrain$zG[1]
# sensortrain$XdeltaO[1] <- sensortrain$alpha[1]
# sensortrain$YdeltaO[1] <- sensortrain$beta[1]
# sensortrain$ZdeltaO[1] <- sensortrain$gamma[1]
# sensortrain$XdeltaA[2:length(sensortrain$xA)] <- sensortrain$xA[-1] - sensortrain$xA[-length(sensortrain$xA)]
# sensortrain$YdeltaA[2:length(sensortrain$xA)] <- sensortrain$yA[-1] - sensortrain$yA[-length(sensortrain$yA)]
# sensortrain$ZdeltaA[2:length(sensortrain$xA)] <- sensortrain$zA[-1] - sensortrain$zA[-length(sensortrain$zA)]
# sensortrain$XdeltaG[2:length(sensortrain$xA)] <- sensortrain$xG[-1] - sensortrain$xG[-length(sensortrain$xG)]
# sensortrain$YdeltaG[2:length(sensortrain$xA)] <- sensortrain$yG[-1] - sensortrain$yG[-length(sensortrain$yG)]
# sensortrain$ZdeltaG[2:length(sensortrain$xA)] <- sensortrain$zG[-1] - sensortrain$zG[-length(sensortrain$zG)]
# sensortrain$XdeltaO[2:length(sensortrain$xA)] <- sensortrain$alpha[-1] - sensortrain$alpha[-length(sensortrain$alpha)]
# sensortrain$YdeltaO[2:length(sensortrain$xA)] <- sensortrain$beta[-1] - sensortrain$beta[-length(sensortrain$beta)]
# sensortrain$ZdeltaO[2:length(sensortrain$xA)] <- sensortrain$gamma[-1] - sensortrain$gamma[-length(sensortrain$gamma)]
# 
# sensortrain$MdeltaA[1] <- sensortrain$MagnA[1]
# sensortrain$MdeltaG[1] <- sensortrain$MagnG[1]
# sensortrain$MdeltaO[1] <- sensortrain$MagnO[1]
# sensortrain$MdeltaA[2:length(sensortrain$MagnA)] <- sensortrain$MagnA[-1] - sensortrain$MagnA[-length(sensortrain$MagnA)]
# sensortrain$MdeltaG[2:length(sensortrain$MagnG)] <- sensortrain$MagnG[-1] - sensortrain$MagnG[-length(sensortrain$MagnG)]
# sensortrain$MdeltaO[2:length(sensortrain$MagnO)] <- sensortrain$MagnO[-1] - sensortrain$MagnO[-length(sensortrain$MagnO)]

# # low pass filter: y[i] := y[i-1] + α * (x[i] - x[i-1])
# filterfactor <- 0.9
# for(i in seq_along(sensortrain$xA)) {
#   if(i == 1) {
#     sensortrain$XlpA[1] <- sensortrain$xA[1]
#     sensortrain$YlpA[1] <- sensortrain$yA[1]
#     sensortrain$ZlpA[1] <- sensortrain$zA[1]
#     sensortrain$XlpG[1] <- sensortrain$xG[1]
#     sensortrain$YlpG[1] <- sensortrain$yG[1]
#     sensortrain$ZlpG[1] <- sensortrain$zG[1]
#     sensortrain$XlpO[1] <- sensortrain$alpha[1]
#     sensortrain$YlpO[1] <- sensortrain$beta[1]
#     sensortrain$ZlpO[1] <- sensortrain$gamma[1]
#   }
#   else {
#     sensortrain$XlpA[i] <- sensortrain$XlpA[i-1] + filterfactor * sensortrain$XdeltaA[i]
#     sensortrain$YlpA[i] <- sensortrain$YlpA[i-1] + filterfactor * sensortrain$YdeltaA[i]
#     sensortrain$ZlpA[i] <- sensortrain$ZlpA[i-1] + filterfactor * sensortrain$ZdeltaA[i]
#     sensortrain$XlpG[i] <- sensortrain$XlpG[i-1] + filterfactor * sensortrain$XdeltaG[i]
#     sensortrain$YlpG[i] <- sensortrain$YlpG[i-1] + filterfactor * sensortrain$YdeltaG[i]
#     sensortrain$ZlpG[i] <- sensortrain$ZlpG[i-1] + filterfactor * sensortrain$ZdeltaG[i]
#     sensortrain$XlpO[i] <- sensortrain$XlpO[i-1] + filterfactor * sensortrain$XdeltaO[i]
#     sensortrain$YlpO[i] <- sensortrain$YlpO[i-1] + filterfactor * sensortrain$YdeltaO[i]
#     sensortrain$ZlpO[i] <- sensortrain$ZlpO[i-1] + filterfactor * sensortrain$ZdeltaO[i]
#   }
#   sensortrain$id[i] <- i
# }
# # magnitude of low pass filtered values
# sensortrain$MagnLpA <- sqrt(sensortrain$XlpA^2 + sensortrain$YlpA^2 + sensortrain$ZlpA^2)
# sensortrain$MagnLpG <- sqrt(sensortrain$XlpG^2 + sensortrain$YlpG^2 + sensortrain$ZlpG^2)
# sensortrain$MagnLpO <- sqrt(sensortrain$XlpO^2 + sensortrain$YlpO^2 + sensortrain$ZlpO^2)

write.csv(sensortrain,
          "C:\\git\\data-thesis\\R\\datasets\\17011417-sensor-dataset-training-preprocessed.csv",
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
    tmpWindowCopy <- sensortrain[sensortrain$Timestamp >= ftrain$DownTime[i] & sensortrain$Timestamp <= ftrain$EventTime[i],]
  } else {
    # create windows for aggregation over the sensor data between two key presses
    tmpWindowCopy <- sensortrain[sensortrain$Timestamp < ftrain$DownTime[i] & sensortrain$Timestamp > c(0, ftrain$DownTime)[i],]
    
    #fix label
    ftrain$Keypress[i] <- "NONE"
    
    # fix timestamps
    if(i == 1) varDownTime[1] <- tmpWindowCopy$Timestamp[1]
    else varDownTime[i] <- ftrain$EventTime[i-1] + 1
    
    varEventTime[i] <- ftrain$DownTime[i] - 1
  }
  
  ftrain$WindowSize[i] <- length(tmpWindowCopy$Timestamp)
  wSize <- ftrain$WindowSize[i]
  
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
  ftrain$Xd0A[i] <- tmpRawPolyMod$coefficients[1]
  ftrain$Xd1A[i] <- tmpRawPolyMod$coefficients[2]
  ftrain$Xd2A[i] <- tmpRawPolyMod$coefficients[3]
  ftrain$Xd3A[i] <- tmpRawPolyMod$coefficients[4]
  
  tmpRawPolyMod <- lm(tmpRawYWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
  tmpRawYWindowPolInterp <- predict(tmpRawPolyMod)
  ftrain$Yd0A[i] <- tmpRawPolyMod$coefficients[1]
  ftrain$Yd1A[i] <- tmpRawPolyMod$coefficients[2]
  ftrain$Yd2A[i] <- tmpRawPolyMod$coefficients[3]
  ftrain$Yd3A[i] <- tmpRawPolyMod$coefficients[4]
  
  tmpRawPolyMod <- lm(tmpRawZWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
  tmpRawZWindowPolInterp <- predict(tmpRawPolyMod)
  ftrain$Zd0A[i] <- tmpRawPolyMod$coefficients[1]
  ftrain$Zd1A[i] <- tmpRawPolyMod$coefficients[2]
  ftrain$Zd2A[i] <- tmpRawPolyMod$coefficients[3]
  ftrain$Zd3A[i] <- tmpRawPolyMod$coefficients[4]
  
  tmpRawPolyMod <- lm(tmpRawYWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
  tmpRawAWindowPolInterp <- predict(tmpRawPolyMod)
  ftrain$Xd0G[i] <- tmpRawPolyMod$coefficients[1]
  ftrain$Xd1G[i] <- tmpRawPolyMod$coefficients[2]
  ftrain$Xd2G[i] <- tmpRawPolyMod$coefficients[3]
  ftrain$Xd3G[i] <- tmpRawPolyMod$coefficients[4]
  
  tmpRawPolyMod <- lm(tmpRawBWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
  tmpRawBWindowPolInterp <- predict(tmpRawPolyMod)
  ftrain$Yd0G[i] <- tmpRawPolyMod$coefficients[1]
  ftrain$Yd1G[i] <- tmpRawPolyMod$coefficients[2]
  ftrain$Yd2G[i] <- tmpRawPolyMod$coefficients[3]
  ftrain$Yd3G[i] <- tmpRawPolyMod$coefficients[4]
  
  tmpRawPolyMod <- lm(tmpRawCWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
  tmpRawCWindowPolInterp <- predict(tmpRawPolyMod)
  ftrain$Zd0G[i] <- tmpRawPolyMod$coefficients[1]
  ftrain$Zd1G[i] <- tmpRawPolyMod$coefficients[2]
  ftrain$Zd2G[i] <- tmpRawPolyMod$coefficients[3]
  ftrain$Zd3G[i] <- tmpRawPolyMod$coefficients[4]
  
  tmpRawPolyMod <- lm(tmpRawAlphaWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
  tmpRawAlphaWindowPolInterp <- predict(tmpRawPolyMod)
  ftrain$Xd0O[i] <- tmpRawPolyMod$coefficients[1]
  ftrain$Xd1O[i] <- tmpRawPolyMod$coefficients[2]
  ftrain$Xd2O[i] <- tmpRawPolyMod$coefficients[3]
  ftrain$Xd3O[i] <- tmpRawPolyMod$coefficients[4]
  
  tmpRawPolyMod <- lm(tmpRawBetaWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
  tmpRawBetaWindowPolInterp <- predict(tmpRawPolyMod)
  ftrain$Yd0O[i] <- tmpRawPolyMod$coefficients[1]
  ftrain$Yd1O[i] <- tmpRawPolyMod$coefficients[2]
  ftrain$Yd2O[i] <- tmpRawPolyMod$coefficients[3]
  ftrain$Yd3O[i] <- tmpRawPolyMod$coefficients[4]
  
  tmpRawPolyMod <- lm(tmpRawGammaWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
  tmpRawGammaWindowPolInterp <- predict(tmpRawPolyMod)
  ftrain$Zd0O[i] <- tmpRawPolyMod$coefficients[1]
  ftrain$Zd1O[i] <- tmpRawPolyMod$coefficients[2]
  ftrain$Zd2O[i] <- tmpRawPolyMod$coefficients[3]
  ftrain$Zd3O[i] <- tmpRawPolyMod$coefficients[4]
  
  tmpRawPolyMod <- lm(tmpRawMagnAWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
  tmpRawMagnAWindowPolInterp <- predict(tmpRawPolyMod)
  ftrain$Md0A[i] <- tmpRawPolyMod$coefficients[1]
  ftrain$Md1A[i] <- tmpRawPolyMod$coefficients[2]
  ftrain$Md2A[i] <- tmpRawPolyMod$coefficients[3]
  ftrain$Md3A[i] <- tmpRawPolyMod$coefficients[4]
  
  tmpRawPolyMod <- lm(tmpRawMagnGWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
  tmpRawMagnGWindowPolInterp <- predict(tmpRawPolyMod)
  ftrain$Md0G[i] <- tmpRawPolyMod$coefficients[1]
  ftrain$Md1G[i] <- tmpRawPolyMod$coefficients[2]
  ftrain$Md2G[i] <- tmpRawPolyMod$coefficients[3]
  ftrain$Md3G[i] <- tmpRawPolyMod$coefficients[4]
  
  tmpRawPolyMod <- lm(tmpRawMagnOWindow ~ poly(tmpWindowCopy$Timestamp, polInterpN))
  tmpRawMagnOWindowPolInterp <- predict(tmpRawPolyMod)
  ftrain$Md0O[i] <- tmpRawPolyMod$coefficients[1]
  ftrain$Md1O[i] <- tmpRawPolyMod$coefficients[2]
  ftrain$Md2O[i] <- tmpRawPolyMod$coefficients[3]
  ftrain$Md3O[i] <- tmpRawPolyMod$coefficients[4]

  
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
  ftrain$XminA[i] <- min(tmpRawXWindow)
  ftrain$YminA[i] <- min(tmpRawYWindow)
  ftrain$ZminA[i] <- min(tmpRawZWindow)
  ftrain$XminG[i] <- min(tmpRawAWindow)
  ftrain$YminG[i] <- min(tmpRawBWindow)
  ftrain$ZminG[i] <- min(tmpRawCWindow)
  ftrain$XminO[i] <- min(tmpRawAlphaWindow)
  ftrain$YminO[i] <- min(tmpRawBetaWindow)
  ftrain$ZminO[i] <- min(tmpRawGammaWindow)
  
  ftrain$XminLinInterpA[i] <- min(tmpRawXWindowLinInterp)
  ftrain$YminLinInterpA[i] <- min(tmpRawYWindowLinInterp)
  ftrain$ZminLinInterpA[i] <- min(tmpRawZWindowLinInterp)
  ftrain$XminLinInterpG[i] <- min(tmpRawAWindowLinInterp)
  ftrain$YminLinInterpG[i] <- min(tmpRawBWindowLinInterp)
  ftrain$ZminLinInterpG[i] <- min(tmpRawCWindowLinInterp)
  ftrain$XminLinInterpO[i] <- min(tmpRawAlphaWindowLinInterp)
  ftrain$YminLinInterpO[i] <- min(tmpRawBetaWindowLinInterp)
  ftrain$ZminLinInterpO[i] <- min(tmpRawGammaWindowLinInterp)
  
  ftrain$XminPolInterpA[i] <- min(tmpRawXWindowPolInterp)
  ftrain$YminPolInterpA[i] <- min(tmpRawYWindowPolInterp)
  ftrain$ZminPolInterpA[i] <- min(tmpRawZWindowPolInterp)
  ftrain$XminPolInterpG[i] <- min(tmpRawAWindowPolInterp)
  ftrain$YminPolInterpG[i] <- min(tmpRawBWindowPolInterp)
  ftrain$ZminPolInterpG[i] <- min(tmpRawCWindowPolInterp)
  ftrain$XminPolInterpO[i] <- min(tmpRawAlphaWindowPolInterp)
  ftrain$YminPolInterpO[i] <- min(tmpRawBetaWindowPolInterp)
  ftrain$ZminPolInterpO[i] <- min(tmpRawGammaWindowPolInterp)
  
  ftrain$XminCubInterpA[i] <- min(tmpRawXWindowCubInterp)
  ftrain$YminCubInterpA[i] <- min(tmpRawYWindowCubInterp)
  ftrain$ZminCubInterpA[i] <- min(tmpRawZWindowCubInterp)
  ftrain$XminCubInterpG[i] <- min(tmpRawAWindowCubInterp)
  ftrain$YminCubInterpG[i] <- min(tmpRawBWindowCubInterp)
  ftrain$ZminCubInterpG[i] <- min(tmpRawCWindowCubInterp)
  ftrain$XminCubInterpO[i] <- min(tmpRawAlphaWindowCubInterp)
  ftrain$YminCubInterpO[i] <- min(tmpRawBetaWindowCubInterp)
  ftrain$ZminCubInterpO[i] <- min(tmpRawGammaWindowCubInterp)
  
  ftrain$XminMeanNormA[i] <- min(tmpRawXWindowMeanNorm)
  ftrain$YminMeanNormA[i] <- min(tmpRawYWindowMeanNorm)
  ftrain$ZminMeanNormA[i] <- min(tmpRawZWindowMeanNorm)
  ftrain$XminMeanNormG[i] <- min(tmpRawAWindowMeanNorm)
  ftrain$YminMeanNormG[i] <- min(tmpRawBWindowMeanNorm)
  ftrain$ZminMeanNormG[i] <- min(tmpRawCWindowMeanNorm)
  ftrain$XminMeanNormO[i] <- min(tmpRawAlphaWindowMeanNorm)
  ftrain$YminMeanNormO[i] <- min(tmpRawBetaWindowMeanNorm)
  ftrain$ZminMeanNormO[i] <- min(tmpRawGammaWindowMeanNorm)
  
  #min magnitude
  ftrain$MminA[i] <- min(tmpRawMagnAWindow)
  ftrain$MminG[i] <- min(tmpRawMagnGWindow)
  ftrain$MminO[i] <- min(tmpRawMagnOWindow)
  
  ftrain$MminMeanNormA[i] <- min(tmpRawMagnAWindowMeanNorm)
  ftrain$MminMeanNormG[i] <- min(tmpRawMagnGWindowMeanNorm)
  ftrain$MminMeanNormO[i] <- min(tmpRawMagnOWindowMeanNorm)
  
  ftrain$MminLinInterpA[i] <- min(tmpRawMagnAWindowLinInterp)
  ftrain$MminLinInterpG[i] <- min(tmpRawMagnGWindowLinInterp)
  ftrain$MminLinInterpO[i] <- min(tmpRawMagnOWindowLinInterp)
  
  ftrain$MminPolInterpA[i] <- min(tmpRawMagnAWindowPolInterp)
  ftrain$MminPolInterpG[i] <- min(tmpRawMagnGWindowPolInterp)
  ftrain$MminPolInterpO[i] <- min(tmpRawMagnOWindowPolInterp)
  
  ftrain$MminCubInterpA[i] <- min(tmpRawMagnAWindowCubInterp)
  ftrain$MminCubInterpG[i] <- min(tmpRawMagnGWindowCubInterp)
  ftrain$MminCubInterpO[i] <- min(tmpRawMagnOWindowCubInterp)
  
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
  
  ftrain$XmaxLinInterpA[i] <- max(tmpRawXWindowLinInterp)
  ftrain$YmaxLinInterpA[i] <- max(tmpRawYWindowLinInterp)
  ftrain$ZmaxLinInterpA[i] <- max(tmpRawZWindowLinInterp)
  ftrain$XmaxLinInterpG[i] <- max(tmpRawAWindowLinInterp)
  ftrain$YmaxLinInterpG[i] <- max(tmpRawBWindowLinInterp)
  ftrain$ZmaxLinInterpG[i] <- max(tmpRawCWindowLinInterp)
  ftrain$XmaxLinInterpO[i] <- max(tmpRawAlphaWindowLinInterp)
  ftrain$YmaxLinInterpO[i] <- max(tmpRawBetaWindowLinInterp)
  ftrain$ZmaxLinInterpO[i] <- max(tmpRawGammaWindowLinInterp)
  
  ftrain$XmaxPolInterpA[i] <- max(tmpRawXWindowPolInterp)
  ftrain$YmaxPolInterpA[i] <- max(tmpRawYWindowPolInterp)
  ftrain$ZmaxPolInterpA[i] <- max(tmpRawZWindowPolInterp)
  ftrain$XmaxPolInterpG[i] <- max(tmpRawAWindowPolInterp)
  ftrain$YmaxPolInterpG[i] <- max(tmpRawBWindowPolInterp)
  ftrain$ZmaxPolInterpG[i] <- max(tmpRawCWindowPolInterp)
  ftrain$XmaxPolInterpO[i] <- max(tmpRawAlphaWindowPolInterp)
  ftrain$YmaxPolInterpO[i] <- max(tmpRawBetaWindowPolInterp)
  ftrain$ZmaxPolInterpO[i] <- max(tmpRawGammaWindowPolInterp)
  
  ftrain$XmaxCubInterpA[i] <- max(tmpRawXWindowCubInterp)
  ftrain$YmaxCubInterpA[i] <- max(tmpRawYWindowCubInterp)
  ftrain$ZmaxCubInterpA[i] <- max(tmpRawZWindowCubInterp)
  ftrain$XmaxCubInterpG[i] <- max(tmpRawAWindowCubInterp)
  ftrain$YmaxCubInterpG[i] <- max(tmpRawBWindowCubInterp)
  ftrain$ZmaxCubInterpG[i] <- max(tmpRawCWindowCubInterp)
  ftrain$XmaxCubInterpO[i] <- max(tmpRawAlphaWindowCubInterp)
  ftrain$YmaxCubInterpO[i] <- max(tmpRawBetaWindowCubInterp)
  ftrain$ZmaxCubInterpO[i] <- max(tmpRawGammaWindowCubInterp)
  
  ftrain$XmaxMeanNormA[i] <- max(tmpRawXWindowMeanNorm)
  ftrain$YmaxMeanNormA[i] <- max(tmpRawYWindowMeanNorm)
  ftrain$ZmaxMeanNormA[i] <- max(tmpRawZWindowMeanNorm)
  ftrain$XmaxMeanNormG[i] <- max(tmpRawAWindowMeanNorm)
  ftrain$YmaxMeanNormG[i] <- max(tmpRawBWindowMeanNorm)
  ftrain$ZmaxMeanNormG[i] <- max(tmpRawCWindowMeanNorm)
  ftrain$XmaxMeanNormO[i] <- max(tmpRawAlphaWindowMeanNorm)
  ftrain$YmaxMeanNormO[i] <- max(tmpRawBetaWindowMeanNorm)
  ftrain$ZmaxMeanNormO[i] <- max(tmpRawGammaWindowMeanNorm)
  
  #max magnitude
  ftrain$MmaxA[i] <- max(tmpRawMagnAWindow)
  ftrain$MmaxG[i] <- max(tmpRawMagnGWindow)
  ftrain$MmaxO[i] <- max(tmpRawMagnOWindow)
  
  ftrain$MmaxLinInterpA[i] <- max(tmpRawMagnAWindowLinInterp)
  ftrain$MmaxLinInterpG[i] <- max(tmpRawMagnGWindowLinInterp)
  ftrain$MmaxLinInterpO[i] <- max(tmpRawMagnOWindowLinInterp)
  
  ftrain$MmaxPolInterpA[i] <- max(tmpRawMagnAWindowPolInterp)
  ftrain$MmaxPolInterpG[i] <- max(tmpRawMagnGWindowPolInterp)
  ftrain$MmaxPolInterpO[i] <- max(tmpRawMagnOWindowPolInterp)
  
  ftrain$MmaxCubInterpA[i] <- max(tmpRawMagnAWindowCubInterp)
  ftrain$MmaxCubInterpG[i] <- max(tmpRawMagnGWindowCubInterp)
  ftrain$MmaxCubInterpO[i] <- max(tmpRawMagnOWindowCubInterp)
  
  ftrain$MmaxMeanNormA[i] <- max(tmpRawMagnAWindowMeanNorm)
  ftrain$MmaxMeanNormG[i] <- max(tmpRawMagnGWindowMeanNorm)
  ftrain$MmaxMeanNormO[i] <- max(tmpRawMagnOWindowMeanNorm)
  
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
  
  ftrain$XmeanLinInterpA[i] <- mean(tmpRawXWindowLinInterp)
  ftrain$YmeanLinInterpA[i] <- mean(tmpRawYWindowLinInterp)
  ftrain$ZmeanLinInterpA[i] <- mean(tmpRawZWindowLinInterp)
  ftrain$XmeanLinInterpG[i] <- mean(tmpRawAWindowLinInterp)
  ftrain$YmeanLinInterpG[i] <- mean(tmpRawBWindowLinInterp)
  ftrain$ZmeanLinInterpG[i] <- mean(tmpRawCWindowLinInterp)
  ftrain$XmeanLinInterpO[i] <- mean(tmpRawAlphaWindowLinInterp)
  ftrain$YmeanLinInterpO[i] <- mean(tmpRawBetaWindowLinInterp)
  ftrain$ZmeanLinInterpO[i] <- mean(tmpRawGammaWindowLinInterp)
  
  ftrain$XmeanPolInterpA[i] <- mean(tmpRawXWindowPolInterp)
  ftrain$YmeanPolInterpA[i] <- mean(tmpRawYWindowPolInterp)
  ftrain$ZmeanPolInterpA[i] <- mean(tmpRawZWindowPolInterp)
  ftrain$XmeanPolInterpG[i] <- mean(tmpRawAWindowPolInterp)
  ftrain$YmeanPolInterpG[i] <- mean(tmpRawBWindowPolInterp)
  ftrain$ZmeanPolInterpG[i] <- mean(tmpRawCWindowPolInterp)
  ftrain$XmeanPolInterpO[i] <- mean(tmpRawAlphaWindowPolInterp)
  ftrain$YmeanPolInterpO[i] <- mean(tmpRawBetaWindowPolInterp)
  ftrain$ZmeanPolInterpO[i] <- mean(tmpRawGammaWindowPolInterp)
  
  ftrain$XmeanCubInterpA[i] <- mean(tmpRawXWindowCubInterp)
  ftrain$YmeanCubInterpA[i] <- mean(tmpRawYWindowCubInterp)
  ftrain$ZmeanCubInterpA[i] <- mean(tmpRawZWindowCubInterp)
  ftrain$XmeanCubInterpG[i] <- mean(tmpRawAWindowCubInterp)
  ftrain$YmeanCubInterpG[i] <- mean(tmpRawBWindowCubInterp)
  ftrain$ZmeanCubInterpG[i] <- mean(tmpRawCWindowCubInterp)
  ftrain$XmeanCubInterpO[i] <- mean(tmpRawAlphaWindowCubInterp)
  ftrain$YmeanCubInterpO[i] <- mean(tmpRawBetaWindowCubInterp)
  ftrain$ZmeanCubInterpO[i] <- mean(tmpRawGammaWindowCubInterp)
  
  ftrain$XmeanMeanNormA[i] <- mean(tmpRawXWindowMeanNorm)
  ftrain$YmeanMeanNormA[i] <- mean(tmpRawYWindowMeanNorm)
  ftrain$ZmeanMeanNormA[i] <- mean(tmpRawZWindowMeanNorm)
  ftrain$XmeanMeanNormG[i] <- mean(tmpRawAWindowMeanNorm)
  ftrain$YmeanMeanNormG[i] <- mean(tmpRawBWindowMeanNorm)
  ftrain$ZmeanMeanNormG[i] <- mean(tmpRawCWindowMeanNorm)
  ftrain$XmeanMeanNormO[i] <- mean(tmpRawAlphaWindowMeanNorm)
  ftrain$YmeanMeanNormO[i] <- mean(tmpRawBetaWindowMeanNorm)
  ftrain$ZmeanMeanNormO[i] <- mean(tmpRawGammaWindowMeanNorm)
  
  # mean magn
  ftrain$MmeanA[i] <- mean(tmpRawMagnAWindow)
  ftrain$MmeanG[i] <- mean(tmpRawMagnGWindow)
  ftrain$MmeanO[i] <- mean(tmpRawMagnOWindow)
  
  ftrain$MmeanLinInterpA[i] <- mean(tmpRawMagnAWindowLinInterp)
  ftrain$MmeanLinInterpG[i] <- mean(tmpRawMagnGWindowLinInterp)
  ftrain$MmeanLinInterpO[i] <- mean(tmpRawMagnOWindowLinInterp)
  
  ftrain$MmeanPolInterpA[i] <- mean(tmpRawMagnAWindowPolInterp)
  ftrain$MmeanPolInterpG[i] <- mean(tmpRawMagnGWindowPolInterp)
  ftrain$MmeanPolInterpO[i] <- mean(tmpRawMagnOWindowPolInterp)
  
  ftrain$MmeanCubInterpA[i] <- mean(tmpRawMagnAWindowCubInterp)
  ftrain$MmeanCubInterpG[i] <- mean(tmpRawMagnGWindowCubInterp)
  ftrain$MmeanCubInterpO[i] <- mean(tmpRawMagnOWindowCubInterp)
  
  ftrain$MmeanMeanNormA[i] <- mean(tmpRawMagnAWindowMeanNorm)
  ftrain$MmeanMeanNormG[i] <- mean(tmpRawMagnGWindowMeanNorm)
  ftrain$MmeanMeanNormO[i] <- mean(tmpRawMagnOWindowMeanNorm)
  
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
  
  ftrain$XmedianLinInterpA[i] <- median(tmpRawXWindowLinInterp)
  ftrain$YmedianLinInterpA[i] <- median(tmpRawYWindowLinInterp)
  ftrain$ZmedianLinInterpA[i] <- median(tmpRawZWindowLinInterp)
  ftrain$XmedianLinInterpG[i] <- median(tmpRawAWindowLinInterp)
  ftrain$YmedianLinInterpG[i] <- median(tmpRawBWindowLinInterp)
  ftrain$ZmedianLinInterpG[i] <- median(tmpRawCWindowLinInterp)
  ftrain$XmedianLinInterpO[i] <- median(tmpRawAlphaWindowLinInterp)
  ftrain$YmedianLinInterpO[i] <- median(tmpRawBetaWindowLinInterp)
  ftrain$ZmedianLinInterpO[i] <- median(tmpRawGammaWindowLinInterp)
  
  ftrain$XmedianPolInterpA[i] <- median(tmpRawXWindowPolInterp)
  ftrain$YmedianPolInterpA[i] <- median(tmpRawYWindowPolInterp)
  ftrain$ZmedianPolInterpA[i] <- median(tmpRawZWindowPolInterp)
  ftrain$XmedianPolInterpG[i] <- median(tmpRawAWindowPolInterp)
  ftrain$YmedianPolInterpG[i] <- median(tmpRawBWindowPolInterp)
  ftrain$ZmedianPolInterpG[i] <- median(tmpRawCWindowPolInterp)
  ftrain$XmedianPolInterpO[i] <- median(tmpRawAlphaWindowPolInterp)
  ftrain$YmedianPolInterpO[i] <- median(tmpRawBetaWindowPolInterp)
  ftrain$ZmedianPolInterpO[i] <- median(tmpRawGammaWindowPolInterp)
  
  ftrain$XmedianCubInterpA[i] <- median(tmpRawXWindowCubInterp)
  ftrain$YmedianCubInterpA[i] <- median(tmpRawYWindowCubInterp)
  ftrain$ZmedianCubInterpA[i] <- median(tmpRawZWindowCubInterp)
  ftrain$XmedianCubInterpG[i] <- median(tmpRawAWindowCubInterp)
  ftrain$YmedianCubInterpG[i] <- median(tmpRawBWindowCubInterp)
  ftrain$ZmedianCubInterpG[i] <- median(tmpRawCWindowCubInterp)
  ftrain$XmedianCubInterpO[i] <- median(tmpRawAlphaWindowCubInterp)
  ftrain$YmedianCubInterpO[i] <- median(tmpRawBetaWindowCubInterp)
  ftrain$ZmedianCubInterpO[i] <- median(tmpRawGammaWindowCubInterp)
  
  ftrain$XmedianMeanNormA[i] <- median(tmpRawXWindowMeanNorm)
  ftrain$YmedianMeanNormA[i] <- median(tmpRawYWindowMeanNorm)
  ftrain$ZmedianMeanNormA[i] <- median(tmpRawZWindowMeanNorm)
  ftrain$XmedianMeanNormG[i] <- median(tmpRawAWindowMeanNorm)
  ftrain$YmedianMeanNormG[i] <- median(tmpRawBWindowMeanNorm)
  ftrain$ZmedianMeanNormG[i] <- median(tmpRawCWindowMeanNorm)
  ftrain$XmedianMeanNormO[i] <- median(tmpRawAlphaWindowMeanNorm)
  ftrain$YmedianMeanNormO[i] <- median(tmpRawBetaWindowMeanNorm)
  ftrain$ZmedianMeanNormO[i] <- median(tmpRawGammaWindowMeanNorm)
  
  # median magn
  ftrain$MmedianA[i] <- median(tmpRawMagnAWindow)
  ftrain$MmedianG[i] <- median(tmpRawMagnGWindow)
  ftrain$MmedianO[i] <- median(tmpRawMagnOWindow)
  
  ftrain$MmedianLinInterpA[i] <- median(tmpRawMagnAWindowLinInterp)
  ftrain$MmedianLinInterpG[i] <- median(tmpRawMagnGWindowLinInterp)
  ftrain$MmedianLinInterpO[i] <- median(tmpRawMagnOWindowLinInterp)
  
  ftrain$MmedianPolInterpA[i] <- median(tmpRawMagnAWindowPolInterp)
  ftrain$MmedianPolInterpG[i] <- median(tmpRawMagnGWindowPolInterp)
  ftrain$MmedianPolInterpO[i] <- median(tmpRawMagnOWindowPolInterp)
  
  ftrain$MmedianCubInterpA[i] <- median(tmpRawMagnAWindowCubInterp)
  ftrain$MmedianCubInterpG[i] <- median(tmpRawMagnGWindowCubInterp)
  ftrain$MmedianCubInterpO[i] <- median(tmpRawMagnOWindowCubInterp)
  
  ftrain$MmedianMeanNormA[i] <- median(tmpRawMagnAWindowMeanNorm)
  ftrain$MmedianMeanNormG[i] <- median(tmpRawMagnGWindowMeanNorm)
  ftrain$MmedianMeanNormO[i] <- median(tmpRawMagnOWindowMeanNorm)
  
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
  
  ftrain$XsdLinInterpA[i] <- sd(tmpRawXWindowLinInterp)
  ftrain$YsdLinInterpA[i] <- sd(tmpRawYWindowLinInterp)
  ftrain$ZsdLinInterpA[i] <- sd(tmpRawZWindowLinInterp)
  ftrain$XsdLinInterpG[i] <- sd(tmpRawAWindowLinInterp)
  ftrain$YsdLinInterpG[i] <- sd(tmpRawBWindowLinInterp)
  ftrain$ZsdLinInterpG[i] <- sd(tmpRawCWindowLinInterp)
  ftrain$XsdLinInterpO[i] <- sd(tmpRawAlphaWindowLinInterp)
  ftrain$YsdLinInterpO[i] <- sd(tmpRawBetaWindowLinInterp)
  ftrain$ZsdLinInterpO[i] <- sd(tmpRawGammaWindowLinInterp)
  
  ftrain$XsdPolInterpA[i] <- sd(tmpRawXWindowPolInterp)
  ftrain$YsdPolInterpA[i] <- sd(tmpRawYWindowPolInterp)
  ftrain$ZsdPolInterpA[i] <- sd(tmpRawZWindowPolInterp)
  ftrain$XsdPolInterpG[i] <- sd(tmpRawAWindowPolInterp)
  ftrain$YsdPolInterpG[i] <- sd(tmpRawBWindowPolInterp)
  ftrain$ZsdPolInterpG[i] <- sd(tmpRawCWindowPolInterp)
  ftrain$XsdPolInterpO[i] <- sd(tmpRawAlphaWindowPolInterp)
  ftrain$YsdPolInterpO[i] <- sd(tmpRawBetaWindowPolInterp)
  ftrain$ZsdPolInterpO[i] <- sd(tmpRawGammaWindowPolInterp)
  
  ftrain$XsdCubInterpA[i] <- sd(tmpRawXWindowCubInterp)
  ftrain$YsdCubInterpA[i] <- sd(tmpRawYWindowCubInterp)
  ftrain$ZsdCubInterpA[i] <- sd(tmpRawZWindowCubInterp)
  ftrain$XsdCubInterpG[i] <- sd(tmpRawAWindowCubInterp)
  ftrain$YsdCubInterpG[i] <- sd(tmpRawBWindowCubInterp)
  ftrain$ZsdCubInterpG[i] <- sd(tmpRawCWindowCubInterp)
  ftrain$XsdCubInterpO[i] <- sd(tmpRawAlphaWindowCubInterp)
  ftrain$YsdCubInterpO[i] <- sd(tmpRawBetaWindowCubInterp)
  ftrain$ZsdCubInterpO[i] <- sd(tmpRawGammaWindowCubInterp)
  
  ftrain$XsdMeanNormA[i] <- sd(tmpRawXWindowMeanNorm)
  ftrain$YsdMeanNormA[i] <- sd(tmpRawYWindowMeanNorm)
  ftrain$ZsdMeanNormA[i] <- sd(tmpRawZWindowMeanNorm)
  ftrain$XsdMeanNormG[i] <- sd(tmpRawAWindowMeanNorm)
  ftrain$YsdMeanNormG[i] <- sd(tmpRawBWindowMeanNorm)
  ftrain$ZsdMeanNormG[i] <- sd(tmpRawCWindowMeanNorm)
  ftrain$XsdMeanNormO[i] <- sd(tmpRawAlphaWindowMeanNorm)
  ftrain$YsdMeanNormO[i] <- sd(tmpRawBetaWindowMeanNorm)
  ftrain$ZsdMeanNormO[i] <- sd(tmpRawGammaWindowMeanNorm)
  
  #sd magn
  ftrain$MsdA[i] <- sd(tmpRawMagnAWindow)
  ftrain$MsdG[i] <- sd(tmpRawMagnGWindow)
  ftrain$MsdO[i] <- sd(tmpRawMagnOWindow)
  
  ftrain$MsdLinInterpA[i] <- sd(tmpRawMagnAWindowLinInterp)
  ftrain$MsdLinInterpG[i] <- sd(tmpRawMagnGWindowLinInterp)
  ftrain$MsdLinInterpO[i] <- sd(tmpRawMagnOWindowLinInterp)
  
  ftrain$MsdPolInterpA[i] <- sd(tmpRawMagnAWindowPolInterp)
  ftrain$MsdPolInterpG[i] <- sd(tmpRawMagnGWindowPolInterp)
  ftrain$MsdPolInterpO[i] <- sd(tmpRawMagnOWindowPolInterp)
  
  ftrain$MsdCubInterpA[i] <- sd(tmpRawMagnAWindowCubInterp)
  ftrain$MsdCubInterpG[i] <- sd(tmpRawMagnGWindowCubInterp)
  ftrain$MsdCubInterpO[i] <- sd(tmpRawMagnOWindowCubInterp)
  
  ftrain$MsdMeanNormA[i] <- sd(tmpRawMagnAWindowMeanNorm)
  ftrain$MsdMeanNormG[i] <- sd(tmpRawMagnGWindowMeanNorm)
  ftrain$MsdMeanNormO[i] <- sd(tmpRawMagnOWindowMeanNorm)
  
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
  
  ftrain$XvarMeanNormA[i] <- var(tmpRawXWindowMeanNorm)
  ftrain$YvarMeanNormA[i] <- var(tmpRawYWindowMeanNorm)
  ftrain$ZvarMeanNormA[i] <- var(tmpRawZWindowMeanNorm)
  ftrain$XvarMeanNormG[i] <- var(tmpRawAWindowMeanNorm)
  ftrain$YvarMeanNormG[i] <- var(tmpRawBWindowMeanNorm)
  ftrain$ZvarMeanNormG[i] <- var(tmpRawCWindowMeanNorm)
  ftrain$XvarMeanNormO[i] <- var(tmpRawAlphaWindowMeanNorm)
  ftrain$YvarMeanNormO[i] <- var(tmpRawBetaWindowMeanNorm)
  ftrain$ZvarMeanNormO[i] <- var(tmpRawGammaWindowMeanNorm)
  
  ftrain$XvarLinInterpA[i] <- var(tmpRawXWindowLinInterp)
  ftrain$YvarLinInterpA[i] <- var(tmpRawYWindowLinInterp)
  ftrain$ZvarLinInterpA[i] <- var(tmpRawZWindowLinInterp)
  ftrain$XvarLinInterpG[i] <- var(tmpRawAWindowLinInterp)
  ftrain$YvarLinInterpG[i] <- var(tmpRawBWindowLinInterp)
  ftrain$ZvarLinInterpG[i] <- var(tmpRawCWindowLinInterp)
  ftrain$XvarLinInterpO[i] <- var(tmpRawAlphaWindowLinInterp)
  ftrain$YvarLinInterpO[i] <- var(tmpRawBetaWindowLinInterp)
  ftrain$ZvarLinInterpO[i] <- var(tmpRawGammaWindowLinInterp)
  
  ftrain$XvarPolInterpA[i] <- var(tmpRawXWindowPolInterp)
  ftrain$YvarPolInterpA[i] <- var(tmpRawYWindowPolInterp)
  ftrain$ZvarPolInterpA[i] <- var(tmpRawZWindowPolInterp)
  ftrain$XvarPolInterpG[i] <- var(tmpRawAWindowPolInterp)
  ftrain$YvarPolInterpG[i] <- var(tmpRawBWindowPolInterp)
  ftrain$ZvarPolInterpG[i] <- var(tmpRawCWindowPolInterp)
  ftrain$XvarPolInterpO[i] <- var(tmpRawAlphaWindowPolInterp)
  ftrain$YvarPolInterpO[i] <- var(tmpRawBetaWindowPolInterp)
  ftrain$ZvarPolInterpO[i] <- var(tmpRawGammaWindowPolInterp)
  
  ftrain$XvarCubInterpA[i] <- var(tmpRawXWindowCubInterp)
  ftrain$YvarCubInterpA[i] <- var(tmpRawYWindowCubInterp)
  ftrain$ZvarCubInterpA[i] <- var(tmpRawZWindowCubInterp)
  ftrain$XvarCubInterpG[i] <- var(tmpRawAWindowCubInterp)
  ftrain$YvarCubInterpG[i] <- var(tmpRawBWindowCubInterp)
  ftrain$ZvarCubInterpG[i] <- var(tmpRawCWindowCubInterp)
  ftrain$XvarCubInterpO[i] <- var(tmpRawAlphaWindowCubInterp)
  ftrain$YvarCubInterpO[i] <- var(tmpRawBetaWindowCubInterp)
  ftrain$ZvarCubInterpO[i] <- var(tmpRawGammaWindowCubInterp)
  
  #var magn
  ftrain$MvarA[i] <- var(tmpRawMagnAWindow)
  ftrain$MvarG[i] <- var(tmpRawMagnGWindow)
  ftrain$MvarO[i] <- var(tmpRawMagnOWindow)
  
  ftrain$MvarLinInterpA[i] <- var(tmpRawMagnAWindowLinInterp)
  ftrain$MvarLinInterpG[i] <- var(tmpRawMagnGWindowLinInterp)
  ftrain$MvarLinInterpO[i] <- var(tmpRawMagnOWindowLinInterp)
  
  ftrain$MvarPolInterpA[i] <- var(tmpRawMagnAWindowPolInterp)
  ftrain$MvarPolInterpG[i] <- var(tmpRawMagnGWindowPolInterp)
  ftrain$MvarPolInterpO[i] <- var(tmpRawMagnOWindowPolInterp)
  
  ftrain$MvarCubInterpA[i] <- var(tmpRawMagnAWindowCubInterp)
  ftrain$MvarCubInterpG[i] <- var(tmpRawMagnGWindowCubInterp)
  ftrain$MvarCubInterpO[i] <- var(tmpRawMagnOWindowCubInterp)
  
  ftrain$MvarMeanNormA[i] <- var(tmpRawMagnAWindowMeanNorm)
  ftrain$MvarMeanNormG[i] <- var(tmpRawMagnGWindowMeanNorm)
  ftrain$MvarMeanNormO[i] <- var(tmpRawMagnOWindowMeanNorm)
  
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
  
  ftrain$XrmsMeanNormA[i] <- sqrt(sum(((tmpRawXWindowMeanNorm) ^ 2)) / wSize)
  ftrain$YrmsMeanNormA[i] <- sqrt(sum(((tmpRawYWindowMeanNorm) ^ 2)) / wSize)
  ftrain$ZrmsMeanNormA[i] <- sqrt(sum(((tmpRawZWindowMeanNorm) ^ 2)) / wSize)
  ftrain$XrmsMeanNormG[i] <- sqrt(sum(((tmpRawAWindowMeanNorm) ^ 2)) / wSize)
  ftrain$YrmsMeanNormG[i] <- sqrt(sum(((tmpRawBWindowMeanNorm) ^ 2)) / wSize)
  ftrain$ZrmsMeanNormG[i] <- sqrt(sum(((tmpRawCWindowMeanNorm) ^ 2)) / wSize)
  ftrain$XrmsMeanNormO[i] <- sqrt(sum(((tmpRawAlphaWindowMeanNorm) ^ 2)) / wSize)
  ftrain$YrmsMeanNormO[i] <- sqrt(sum(((tmpRawBetaWindowMeanNorm) ^ 2)) / wSize)
  ftrain$ZrmsMeanNormO[i] <- sqrt(sum(((tmpRawGammaWindowMeanNorm) ^ 2)) / wSize)
  
  ftrain$XrmsLinInterpA[i] <- sqrt(sum((tmpRawXWindowLinInterp) ^ 2) / wSize)
  ftrain$YrmsLinInterpA[i] <- sqrt(sum((tmpRawYWindowLinInterp) ^ 2) / wSize)
  ftrain$ZrmsLinInterpA[i] <- sqrt(sum((tmpRawZWindowLinInterp) ^ 2) / wSize)
  ftrain$XrmsLinInterpG[i] <- sqrt(sum((tmpRawAWindowLinInterp) ^ 2) / wSize)
  ftrain$YrmsLinInterpG[i] <- sqrt(sum((tmpRawBWindowLinInterp) ^ 2) / wSize)
  ftrain$ZrmsLinInterpG[i] <- sqrt(sum((tmpRawCWindowLinInterp) ^ 2) / wSize)
  ftrain$XrmsLinInterpO[i] <- sqrt(sum((tmpRawAlphaWindowLinInterp) ^ 2) / wSize)
  ftrain$YrmsLinInterpO[i] <- sqrt(sum((tmpRawBetaWindowLinInterp) ^ 2) / wSize)
  ftrain$ZrmsLinInterpO[i] <- sqrt(sum((tmpRawGammaWindowLinInterp) ^ 2) / wSize)
  
  ftrain$XrmsPolInterpA[i] <- sqrt(sum((tmpRawXWindowPolInterp) ^ 2) / wSize)
  ftrain$YrmsPolInterpA[i] <- sqrt(sum((tmpRawYWindowPolInterp) ^ 2) / wSize)
  ftrain$ZrmsPolInterpA[i] <- sqrt(sum((tmpRawZWindowPolInterp) ^ 2) / wSize)
  ftrain$XrmsPolInterpG[i] <- sqrt(sum((tmpRawAWindowPolInterp) ^ 2) / wSize)
  ftrain$YrmsPolInterpG[i] <- sqrt(sum((tmpRawBWindowPolInterp) ^ 2) / wSize)
  ftrain$ZrmsPolInterpG[i] <- sqrt(sum((tmpRawCWindowPolInterp) ^ 2) / wSize)
  ftrain$XrmsPolInterpO[i] <- sqrt(sum((tmpRawAlphaWindowPolInterp) ^ 2) / wSize)
  ftrain$YrmsPolInterpO[i] <- sqrt(sum((tmpRawBetaWindowPolInterp) ^ 2) / wSize)
  ftrain$ZrmsPolInterpO[i] <- sqrt(sum((tmpRawGammaWindowPolInterp) ^ 2) / wSize)
  
  ftrain$XrmsCubInterpA[i] <- sqrt(sum((tmpRawXWindowCubInterp) ^ 2) / wSize)
  ftrain$YrmsCubInterpA[i] <- sqrt(sum((tmpRawYWindowCubInterp) ^ 2) / wSize)
  ftrain$ZrmsCubInterpA[i] <- sqrt(sum((tmpRawZWindowCubInterp) ^ 2) / wSize)
  ftrain$XrmsCubInterpG[i] <- sqrt(sum((tmpRawAWindowCubInterp) ^ 2) / wSize)
  ftrain$YrmsCubInterpG[i] <- sqrt(sum((tmpRawBWindowCubInterp) ^ 2) / wSize)
  ftrain$ZrmsCubInterpG[i] <- sqrt(sum((tmpRawCWindowCubInterp) ^ 2) / wSize)
  ftrain$XrmsCubInterpO[i] <- sqrt(sum((tmpRawAlphaWindowCubInterp) ^ 2) / wSize)
  ftrain$YrmsCubInterpO[i] <- sqrt(sum((tmpRawBetaWindowCubInterp) ^ 2) / wSize)
  ftrain$ZrmsCubInterpO[i] <- sqrt(sum((tmpRawGammaWindowCubInterp) ^ 2) / wSize)
  
  # Root Mean Square of the magnitude
  ftrain$MagnRmsA[i] <- sqrt((sum((tmpRawMagnAWindow) ^ 2)) / wSize)
  ftrain$MagnRmsG[i] <- sqrt((sum((tmpRawMagnGWindow) ^ 2)) / wSize)
  ftrain$MagnRmsO[i] <- sqrt((sum((tmpRawMagnOWindow) ^ 2)) / wSize)
  
  ftrain$MagnRmsLinInterpA[i] <- sqrt((sum((tmpRawMagnAWindowLinInterp) ^ 2)) / wSize)
  ftrain$MagnRmsLinInterpG[i] <- sqrt((sum((tmpRawMagnGWindowLinInterp) ^ 2)) / wSize)
  ftrain$MagnRmsLinInterpO[i] <- sqrt((sum((tmpRawMagnOWindowLinInterp) ^ 2)) / wSize)
  
  ftrain$MagnRmsPolInterpA[i] <- sqrt((sum((tmpRawMagnAWindowPolInterp) ^ 2)) / wSize)
  ftrain$MagnRmsPolInterpG[i] <- sqrt((sum((tmpRawMagnGWindowPolInterp) ^ 2)) / wSize)
  ftrain$MagnRmsPolInterpO[i] <- sqrt((sum((tmpRawMagnOWindowPolInterp) ^ 2)) / wSize)
  
  ftrain$MagnRmsCubInterpA[i] <- sqrt((sum((tmpRawMagnAWindowCubInterp) ^ 2)) / wSize)
  ftrain$MagnRmsCubInterpG[i] <- sqrt((sum((tmpRawMagnGWindowCubInterp) ^ 2)) / wSize)
  ftrain$MagnRmsCubInterpO[i] <- sqrt((sum((tmpRawMagnOWindowCubInterp) ^ 2)) / wSize)
  
  ftrain$MrmsMeanNormA[i] <- sqrt(sum(((tmpRawMagnAWindowMeanNorm) ^ 2)) / wSize)
  ftrain$MrmsMeanNormG[i] <- sqrt(sum(((tmpRawMagnGWindowMeanNorm) ^ 2)) / wSize)
  ftrain$MrmsMeanNormO[i] <- sqrt(sum(((tmpRawMagnOWindowMeanNorm) ^ 2)) / wSize)
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

ftrain$XskewLinInterpA <- 3 * (ftrain$XmeanLinInterpA - ftrain$XmedianLinInterpA) / ftrain$XsdLinInterpA
ftrain$YskewLinInterpA <- 3 * (ftrain$YmeanLinInterpA - ftrain$YmedianLinInterpA) / ftrain$YsdLinInterpA
ftrain$ZskewLinInterpA <- 3 * (ftrain$ZmeanLinInterpA - ftrain$ZmedianLinInterpA) / ftrain$ZsdLinInterpA
ftrain$XskewLinInterpG <- 3 * (ftrain$XmeanLinInterpG - ftrain$XmedianLinInterpG) / ftrain$XsdLinInterpG
ftrain$YskewLinInterpG <- 3 * (ftrain$YmeanLinInterpG - ftrain$YmedianLinInterpG) / ftrain$YsdLinInterpG
ftrain$ZskewLinInterpG <- 3 * (ftrain$ZmeanLinInterpG - ftrain$ZmedianLinInterpG) / ftrain$ZsdLinInterpG
ftrain$XskewLinInterpO <- 3 * (ftrain$XmeanLinInterpO - ftrain$XmedianLinInterpO) / ftrain$XsdLinInterpO
ftrain$YskewLinInterpO <- 3 * (ftrain$YmeanLinInterpO - ftrain$YmedianLinInterpO) / ftrain$YsdLinInterpO
ftrain$ZskewLinInterpO <- 3 * (ftrain$ZmeanLinInterpO - ftrain$ZmedianLinInterpO) / ftrain$ZsdLinInterpO

ftrain$XskewPolInterpA <- 3 * (ftrain$XmeanPolInterpA - ftrain$XmedianPolInterpA) / ftrain$XsdPolInterpA
ftrain$YskewPolInterpA <- 3 * (ftrain$YmeanPolInterpA - ftrain$YmedianPolInterpA) / ftrain$YsdPolInterpA
ftrain$ZskewPolInterpA <- 3 * (ftrain$ZmeanPolInterpA - ftrain$ZmedianPolInterpA) / ftrain$ZsdPolInterpA
ftrain$XskewPolInterpG <- 3 * (ftrain$XmeanPolInterpG - ftrain$XmedianPolInterpG) / ftrain$XsdPolInterpG
ftrain$YskewPolInterpG <- 3 * (ftrain$YmeanPolInterpG - ftrain$YmedianPolInterpG) / ftrain$YsdPolInterpG
ftrain$ZskewPolInterpG <- 3 * (ftrain$ZmeanPolInterpG - ftrain$ZmedianPolInterpG) / ftrain$ZsdPolInterpG
ftrain$XskewPolInterpO <- 3 * (ftrain$XmeanPolInterpO - ftrain$XmedianPolInterpO) / ftrain$XsdPolInterpO
ftrain$YskewPolInterpO <- 3 * (ftrain$YmeanPolInterpO - ftrain$YmedianPolInterpO) / ftrain$YsdPolInterpO
ftrain$ZskewPolInterpO <- 3 * (ftrain$ZmeanPolInterpO - ftrain$ZmedianPolInterpO) / ftrain$ZsdPolInterpO

ftrain$XskewCubInterpA <- 3 * (ftrain$XmeanCubInterpA - ftrain$XmedianCubInterpA) / ftrain$XsdCubInterpA
ftrain$YskewCubInterpA <- 3 * (ftrain$YmeanCubInterpA - ftrain$YmedianCubInterpA) / ftrain$YsdCubInterpA
ftrain$ZskewCubInterpA <- 3 * (ftrain$ZmeanCubInterpA - ftrain$ZmedianCubInterpA) / ftrain$ZsdCubInterpA
ftrain$XskewCubInterpG <- 3 * (ftrain$XmeanCubInterpG - ftrain$XmedianCubInterpG) / ftrain$XsdCubInterpG
ftrain$YskewCubInterpG <- 3 * (ftrain$YmeanCubInterpG - ftrain$YmedianCubInterpG) / ftrain$YsdCubInterpG
ftrain$ZskewCubInterpG <- 3 * (ftrain$ZmeanCubInterpG - ftrain$ZmedianCubInterpG) / ftrain$ZsdCubInterpG
ftrain$XskewCubInterpO <- 3 * (ftrain$XmeanCubInterpO - ftrain$XmedianCubInterpO) / ftrain$XsdCubInterpO
ftrain$YskewCubInterpO <- 3 * (ftrain$YmeanCubInterpO - ftrain$YmedianCubInterpO) / ftrain$YsdCubInterpO
ftrain$ZskewCubInterpO <- 3 * (ftrain$ZmeanCubInterpO - ftrain$ZmedianCubInterpO) / ftrain$ZsdCubInterpO

ftrain$XskewMeanNormA <- 3 * (ftrain$XmeanMeanNormA - ftrain$XmedianMeanNormA) / ftrain$XsdMeanNormA
ftrain$YskewMeanNormA <- 3 * (ftrain$YmeanMeanNormA - ftrain$YmedianMeanNormA) / ftrain$YsdMeanNormA
ftrain$ZskewMeanNormA <- 3 * (ftrain$ZmeanMeanNormA - ftrain$ZmedianMeanNormA) / ftrain$ZsdMeanNormA
ftrain$XskewMeanNormG <- 3 * (ftrain$XmeanMeanNormG - ftrain$XmedianMeanNormG) / ftrain$XsdMeanNormG
ftrain$YskewMeanNormG <- 3 * (ftrain$YmeanMeanNormG - ftrain$YmedianMeanNormG) / ftrain$YsdMeanNormG
ftrain$ZskewMeanNormG <- 3 * (ftrain$ZmeanMeanNormG - ftrain$ZmedianMeanNormG) / ftrain$ZsdMeanNormG
ftrain$XskewMeanNormO <- 3 * (ftrain$XmeanMeanNormO - ftrain$XmedianMeanNormO) / ftrain$XsdMeanNormO
ftrain$YskewMeanNormO <- 3 * (ftrain$YmeanMeanNormO - ftrain$YmedianMeanNormO) / ftrain$YsdMeanNormO
ftrain$ZskewMeanNormO <- 3 * (ftrain$ZmeanMeanNormO - ftrain$ZmedianMeanNormO) / ftrain$ZsdMeanNormO

# skewness magn
ftrain$MskewA <- 3 * (ftrain$MmeanA - ftrain$MmedianA) / ftrain$MsdA
ftrain$MskewG <- 3 * (ftrain$MmeanG - ftrain$MmedianG) / ftrain$MsdG
ftrain$MskewO <- 3 * (ftrain$MmeanO - ftrain$MmedianO) / ftrain$MsdO

ftrain$MskewLinInterpA <- 3 * (ftrain$MmeanLinInterpA - ftrain$MmedianLinInterpA) / ftrain$MsdLinInterpA
ftrain$MskewLinInterpG <- 3 * (ftrain$MmeanLinInterpG - ftrain$MmedianLinInterpG) / ftrain$MsdLinInterpG
ftrain$MskewLinInterpO <- 3 * (ftrain$MmeanLinInterpO - ftrain$MmedianLinInterpO) / ftrain$MsdLinInterpO

ftrain$MskewPolInterpA <- 3 * (ftrain$MmeanPolInterpA - ftrain$MmedianPolInterpA) / ftrain$MsdPolInterpA
ftrain$MskewPolInterpG <- 3 * (ftrain$MmeanPolInterpG - ftrain$MmedianPolInterpG) / ftrain$MsdPolInterpG
ftrain$MskewPolInterpO <- 3 * (ftrain$MmeanPolInterpO - ftrain$MmedianPolInterpO) / ftrain$MsdPolInterpO

ftrain$MskewCubInterpA <- 3 * (ftrain$MmeanCubInterpA - ftrain$MmedianCubInterpA) / ftrain$MsdCubInterpA
ftrain$MskewCubInterpG <- 3 * (ftrain$MmeanCubInterpG - ftrain$MmedianCubInterpG) / ftrain$MsdCubInterpG
ftrain$MskewCubInterpO <- 3 * (ftrain$MmeanCubInterpO - ftrain$MmedianCubInterpO) / ftrain$MsdCubInterpO

ftrain$MskewMeanNormA <- 3 * (ftrain$MmeanMeanNormA - ftrain$MmedianMeanNormA) / ftrain$MsdMeanNormA
ftrain$MskewMeanNormG <- 3 * (ftrain$MmeanMeanNormG - ftrain$MmedianMeanNormG) / ftrain$MsdMeanNormG
ftrain$MskewMeanNormO <- 3 * (ftrain$MmeanMeanNormO - ftrain$MmedianMeanNormO) / ftrain$MsdMeanNormO

#fix timestamps pt. 2
ftrain$DownTime <- varDownTime
ftrain$EventTime <- varEventTime

# fix keys
ftrain$IsKey <- ftrain$Keypress != "NONE"

# time of window
ftrain$DownTime <- floor(ftrain$DownTime / 1000000)
ftrain$EventTime <- floor(ftrain$EventTime / 1000000)
ftrain$TotalTime <- (ftrain$EventTime - ftrain$DownTime)

ftrain$Keypress <- gsub("KEYCODE_", "KEY_", ftrain$Keypress)

write.csv(
  ftrain,
  "C:\\git\\data-thesis\\R\\datasets\\17011020-dataset-training.csv",
  row.names = FALSE
)