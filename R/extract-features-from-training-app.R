sensortrain <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\17011417-sensor-dataset-training-raw.csv",
    header = TRUE,
    stringsAsFactors=FALSE
  )

keytrain <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\17011417-key-dataset-training-raw.csv",
    header = TRUE,
    stringsAsFactors=FALSE
  )

options(digits=20)
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
  
  # tmpWindowCopy <- tmpWindowCopy[,c(4:length(tmpWindowCopy)-1)]
  
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
  
  tmpRawXWindowLinInterp <- approx(tmpWindowCopy$xA, tmpWindowCopy$Timestamp, n=14)$x
  tmpRawYWindowLinInterp <- approx(tmpWindowCopy$yA, tmpWindowCopy$Timestamp, n=14)$x
  tmpRawZWindowLinInterp <- approx(tmpWindowCopy$zA, tmpWindowCopy$Timestamp, n=14)$x
  tmpRawAWindowLinInterp <- approx(tmpWindowCopy$xG, tmpWindowCopy$Timestamp, n=14)$x
  tmpRawBWindowLinInterp <- approx(tmpWindowCopy$yG, tmpWindowCopy$Timestamp, n=14)$x
  tmpRawCWindowLinInterp <- approx(tmpWindowCopy$zG, tmpWindowCopy$Timestamp, n=14)$x
  tmpRawAlphaWindowLinInterp <- approx(tmpWindowCopy$alpha, tmpWindowCopy$Timestamp, n=14)$x
  tmpRawBetaWindowLinInterp <- approx(tmpWindowCopy$beta, tmpWindowCopy$Timestamp, n=14)$x
  tmpRawGammaWindowLinInterp <- approx(tmpWindowCopy$gamma, tmpWindowCopy$Timestamp, n=14)$x
  tmpRawMagnAWindowLinInterp <- approx(tmpWindowCopy$MagnA, tmpWindowCopy$Timestamp, n=14)$x
  tmpRawMagnGWindowLinInterp <- approx(tmpWindowCopy$MagnG, tmpWindowCopy$Timestamp, n=14)$x
  tmpRawMagnOWindowLinInterp <- approx(tmpWindowCopy$MagnO, tmpWindowCopy$Timestamp, n=14)$x
  tmpRawSqSumAWindowLinInterp <- approx(tmpWindowCopy$SqSumA, tmpWindowCopy$Timestamp, n=14)$x
  tmpRawSqSumGWindowLinInterp <- approx(tmpWindowCopy$SqSumG, tmpWindowCopy$Timestamp, n=14)$x
  tmpRawSqSumOWindowLinInterp <- approx(tmpWindowCopy$SqSumO, tmpWindowCopy$Timestamp, n=14)$x

  
  # # tmpPreWindowX <- sensortrain$xA[c(0, ftrain$DownTime[i-1])]
  # 
  # if(i == 1) {
  #   ftrain$PreXminA[i] <- 0
  #   ftrain$PreYminA[i] <- 0
  #   ftrain$PreZminA[i] <- 0
  #   ftrain$PreXminG[i] <- 0
  #   ftrain$PreYminG[i] <- 0
  #   ftrain$PreZminG[i] <- 0
  #   ftrain$PreXminO[i] <- 0
  #   ftrain$PreYminO[i] <- 0
  #   ftrain$PreZminO[i] <- 0
  #   
  #   ftrain$PreXmaxA[i] <- 0
  #   ftrain$PreYmaxA[i] <- 0
  #   ftrain$PreZmaxA[i] <- 0
  #   ftrain$PreXmaxG[i] <- 0
  #   ftrain$PreYmaxG[i] <- 0
  #   ftrain$PreZmaxG[i] <- 0
  #   ftrain$PreXmaxO[i] <- 0
  #   ftrain$PreYmaxO[i] <- 0
  #   ftrain$PreZmaxO[i] <- 0
  #   
  #   ftrain$PreXdeltaMinA[i] <- 0
  #   ftrain$PreYdeltaMinA[i] <- 0
  #   ftrain$PreZdeltaMinA[i] <- 0
  #   ftrain$PreXdeltaMinG[i] <- 0
  #   ftrain$PreZdeltaMinG[i] <- 0
  #   ftrain$PreYdeltaMinG[i] <- 0
  #   ftrain$PreXdeltaMinO[i] <- 0
  #   ftrain$PreYdeltaMinO[i] <- 0
  #   ftrain$PreZdeltaMinO[i] <- 0
  #   
  #   ftrain$PreXdeltaMaxA[i] <- 0
  #   ftrain$PreYdeltaMaxA[i] <- 0
  #   ftrain$PreZdeltaMaxA[i] <- 0
  #   ftrain$PreXdeltaMaxG[i] <- 0
  #   ftrain$PreYdeltaMaxG[i] <- 0
  #   ftrain$PreZdeltaMaxG[i] <- 0
  #   ftrain$PreXdeltaMaxO[i] <- 0
  #   ftrain$PreYdeltaMaxO[i] <- 0
  #   ftrain$PreZdeltaMaxO[i] <- 0
  # }
  # else {
  #   tmpPreWindow <- sensortrain[sensortrain$id < (tmpWindowCopy$id[1]) & sensortrain$id >= (tmpWindowCopy$id[1]-9),]
  #   
  #   # some stats of the 9 entries before the actual window
  #   ftrain$PreXminA[i] <- min(tmpPreWindow$xA)
  #   ftrain$PreYminA[i] <- min(tmpPreWindow$yA)
  #   ftrain$PreZminA[i] <- min(tmpPreWindow$zA)
  #   ftrain$PreXminG[i] <- min(tmpPreWindow$xG)
  #   ftrain$PreYminG[i] <- min(tmpPreWindow$yG)
  #   ftrain$PreZminG[i] <- min(tmpPreWindow$zG)
  #   ftrain$PreXminO[i] <- min(tmpPreWindow$alpha)
  #   ftrain$PreYminO[i] <- min(tmpPreWindow$beta)
  #   ftrain$PreZminO[i] <- min(tmpPreWindow$gamma)
  #   
  #   ftrain$PreXmaxA[i] <- max(tmpPreWindow$xA)
  #   ftrain$PreYmaxA[i] <- max(tmpPreWindow$yA)
  #   ftrain$PreZmaxA[i] <- max(tmpPreWindow$zA)
  #   ftrain$PreXmaxG[i] <- max(tmpPreWindow$xG)
  #   ftrain$PreYmaxG[i] <- max(tmpPreWindow$yG)
  #   ftrain$PreZmaxG[i] <- max(tmpPreWindow$zG)
  #   ftrain$PreXmaxO[i] <- max(tmpPreWindow$alpha)
  #   ftrain$PreYmaxO[i] <- max(tmpPreWindow$beta)
  #   ftrain$PreZmaxO[i] <- max(tmpPreWindow$gamma)
  #   
  #   ftrain$PreXdeltaMinA[i] <- min(tmpPreWindow$XdeltaA)
  #   ftrain$PreYdeltaMinA[i] <- min(tmpPreWindow$YdeltaA)
  #   ftrain$PreZdeltaMinA[i] <- min(tmpPreWindow$ZdeltaA)
  #   ftrain$PreXdeltaMinG[i] <- min(tmpPreWindow$XdeltaG)
  #   ftrain$PreYdeltaMinG[i] <- min(tmpPreWindow$XdeltaG)
  #   ftrain$PreZdeltaMinG[i] <- min(tmpPreWindow$ZdeltaG)
  #   ftrain$PreXdeltaMinO[i] <- min(tmpPreWindow$XdeltaO)
  #   ftrain$PreYdeltaMinO[i] <- min(tmpPreWindow$YdeltaO)
  #   ftrain$PreZdeltaMinO[i] <- min(tmpPreWindow$ZdeltaO)
  #   
  #   ftrain$PreXdeltaMaxA[i] <- max(tmpPreWindow$XdeltaA)
  #   ftrain$PreYdeltaMaxA[i] <- max(tmpPreWindow$YdeltaA)
  #   ftrain$PreZdeltaMaxA[i] <- max(tmpPreWindow$ZdeltaA)
  #   ftrain$PreXdeltaMaxG[i] <- max(tmpPreWindow$XdeltaG)
  #   ftrain$PreYdeltaMaxG[i] <- max(tmpPreWindow$XdeltaG)
  #   ftrain$PreZdeltaMaxG[i] <- max(tmpPreWindow$ZdeltaG)
  #   ftrain$PreXdeltaMaxO[i] <- max(tmpPreWindow$XdeltaO)
  #   ftrain$PreYdeltaMaxO[i] <- max(tmpPreWindow$YdeltaO)
  #   ftrain$PreZdeltaMaxO[i] <- max(tmpPreWindow$ZdeltaO)
  #   
  #   #sd of SquareSUm of pre window
  #   ftrain$PreSqSumSdA[i] <- sd(tmpPreWindow$xA^2 + tmpPreWindow$yA^2 + tmpPreWindow$zA^2)
  #   ftrain$PreSqSumSdG[i] <- sd(tmpPreWindow$xG^2 + tmpPreWindow$yG^2 + tmpPreWindow$zG^2)
  #   ftrain$PreSqSumSdO[i] <- sd(tmpPreWindow$alpha^2 + tmpPreWindow$beta^2 + tmpPreWindow$gamma^2)
  # }
  
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

  #min magnitude
  ftrain$MminA[i] <- min(tmpRawMagnAWindow)
  ftrain$MminG[i] <- min(tmpRawMagnGWindow)
  ftrain$MminO[i] <- min(tmpRawMagnOWindow)
  
  ftrain$MminLinInterpA[i] <- min(tmpRawMagnAWindowLinInterp)
  ftrain$MminLinInterpG[i] <- min(tmpRawMagnGWindowLinInterp)
  ftrain$MminLinInterpO[i] <- min(tmpRawMagnOWindowLinInterp)

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
  
  ftrain$XmeanLinInterpA[i] <- mean(tmpRawXWindowLinInterp)
  ftrain$YmeanLinInterpA[i] <- mean(tmpRawYWindowLinInterp)
  ftrain$ZmeanLinInterpA[i] <- mean(tmpRawZWindowLinInterp)
  ftrain$XmeanLinInterpG[i] <- mean(tmpRawAWindowLinInterp)
  ftrain$YmeanLinInterpG[i] <- mean(tmpRawBWindowLinInterp)
  ftrain$ZmeanLinInterpG[i] <- mean(tmpRawCWindowLinInterp)
  ftrain$XmeanLinInterpO[i] <- mean(tmpRawAlphaWindowLinInterp)
  ftrain$YmeanLinInterpO[i] <- mean(tmpRawBetaWindowLinInterp)
  ftrain$ZmeanLinInterpO[i] <- mean(tmpRawGammaWindowLinInterp)

  # mean magn
  ftrain$MmeanA[i] <- mean(tmpRawMagnAWindow)
  ftrain$MmeanG[i] <- mean(tmpRawMagnGWindow)
  ftrain$MmeanO[i] <- mean(tmpRawMagnOWindow)
  
  ftrain$MmeanLinInterpA[i] <- mean(tmpRawMagnAWindow)
  ftrain$MmeanLinInterpG[i] <- mean(tmpRawMagnGWindow)
  ftrain$MmeanLinInterpO[i] <- mean(tmpRawMagnOWindow)

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

  # median magn
  ftrain$MmedianA[i] <- median(tmpRawMagnAWindow)
  ftrain$MmedianG[i] <- median(tmpRawMagnGWindow)
  ftrain$MmedianO[i] <- median(tmpRawMagnOWindow)
  
  ftrain$MmedianLinInterpA[i] <- median(tmpRawMagnAWindow)
  ftrain$MmedianLinInterpG[i] <- median(tmpRawMagnGWindow)
  ftrain$MmedianLinInterpO[i] <- median(tmpRawMagnOWindow)

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

  #sd magn
  ftrain$MsdA[i] <- sd(tmpRawMagnAWindow)
  ftrain$MsdG[i] <- sd(tmpRawMagnGWindow)
  ftrain$MsdO[i] <- sd(tmpRawMagnOWindow)
  
  ftrain$MsdLinInterpA[i] <- sd(tmpRawMagnAWindow)
  ftrain$MsdLinInterpG[i] <- sd(tmpRawMagnGWindow)
  ftrain$MsdLinInterpO[i] <- sd(tmpRawMagnOWindow)

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
  
  ftrain$XvarLinInterpA[i] <- var(tmpRawXWindowLinInterp)
  ftrain$YvarLinInterpA[i] <- var(tmpRawYWindowLinInterp)
  ftrain$ZvarLinInterpA[i] <- var(tmpRawZWindowLinInterp)
  ftrain$XvarLinInterpG[i] <- var(tmpRawAWindowLinInterp)
  ftrain$YvarLinInterpG[i] <- var(tmpRawBWindowLinInterp)
  ftrain$ZvarLinInterpG[i] <- var(tmpRawCWindowLinInterp)
  ftrain$XvarLinInterpO[i] <- var(tmpRawAlphaWindowLinInterp)
  ftrain$YvarLinInterpO[i] <- var(tmpRawBetaWindowLinInterp)
  ftrain$ZvarLinInterpO[i] <- var(tmpRawGammaWindowLinInterp)

  #var magn
  ftrain$MvarA[i] <- var(tmpRawMagnAWindow)
  ftrain$MvarG[i] <- var(tmpRawMagnGWindow)
  ftrain$MvarO[i] <- var(tmpRawMagnOWindow)
  
  ftrain$MvarLinInterpA[i] <- var(tmpRawMagnAWindow)
  ftrain$MvarLinInterpG[i] <- var(tmpRawMagnGWindow)
  ftrain$MvarLinInterpO[i] <- var(tmpRawMagnOWindow)

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

# skewness magn
ftrain$MskewLinInterpA <- 3 * (ftrain$MmeanLinInterpA - ftrain$MmedianLinInterpA) / ftrain$MsdLinInterpA
ftrain$MskewLinInterpG <- 3 * (ftrain$MmeanLinInterpG - ftrain$MmedianLinInterpG) / ftrain$MsdLinInterpG
ftrain$MskewLinInterpO <- 3 * (ftrain$MmeanLinInterpO - ftrain$MmedianLinInterpO) / ftrain$MsdLinInterpO

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
  "C:\\git\\data-thesis\\R\\datasets\\17011417-dataset-training.csv",
  row.names = FALSE
)