set.seed(123)
options(digits=20)

source("extract-features-class.R")

flist <- c(16122802, 17011020)

for ( i in flist ) {
  fKeyTrain <- feature.extraction(boolTraining = TRUE,stringFileTimestamp = i,boolLabeled = TRUE, windowSize = 42)
  fKeyTrain2 <- feature.extraction(boolTraining = FALSE,stringFileTimestamp = i,boolLabeled = TRUE, windowSize = 42)
}
