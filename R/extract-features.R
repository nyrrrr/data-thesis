if(exists("feature.extraction", mode = "function")) {
  source("extract-features-class.R")
}

flist <- c(17011716, 17011417, 17011205, 17011020, 16122802)

for ( i in flist ) {
  feature.extraction(boolTraining = TRUE,stringFileTimestamp = i,boolLabeled = TRUE)
  if(i != 17011716) feature.extraction(boolTraining = FALSE,stringFileTimestamp = i,boolLabeled = TRUE)
}

