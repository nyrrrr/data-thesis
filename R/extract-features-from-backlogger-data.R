rawdata <- read.csv("C:\\git\\data-thesis\\R\\datasets\\transferred-16121319-victim-data-raw.csv", header=TRUE)
wSize <- 5;
fsd <- NULL # feature data set

for(i in 1:nrow(rawdata)) {
  fsd$Timestamp[i] <- rawdata$Timestamp[i]
}
write.csv(fsd, "C:\\git\\data-thesis\\R\\datasets\\new.csv", row.names=FALSE)