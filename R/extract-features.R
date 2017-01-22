set.seed(123)
options(digits=20)

source("extract-features-class.R")

flist <- c(17011020)#, 16122802)

for ( i in flist ) {
  # feature.extraction(boolTraining = TRUE,stringFileTimestamp = i,boolLabeled = TRUE)
  fKeyTrain <- feature.extraction(boolTraining = FALSE,stringFileTimestamp = i,boolLabeled = TRUE)
}
