set.seed(123)
options(digits=20)

source("extract-features-class.R")

flist <- c(17011417)#, 17011205, 17011020, 16122802)

for ( i in flist ) {
  feature.extraction(boolTraining = TRUE,stringFileTimestamp = i,boolLabeled = TRUE)
  #if(i != 17011906) feature.extraction(boolTraining = FALSE,stringFileTimestamp = i,boolLabeled = TRUE)
}

