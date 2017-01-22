set.seed(123)
options(digits=20)

source("extract-features-class.R")

flist <- c(17012223)#17012220)
windowSize <- 33

for ( i in flist ) {
  fKeyTrain <- feature.extraction(boolTraining = TRUE,stringFileTimestamp = i,boolLabeled = TRUE, windowSize = windowSize)
  fKeyTest <- feature.extraction(boolTraining = FALSE,stringFileTimestamp = i,boolLabeled = TRUE, windowSize = windowSize)
}
