rawdata <-
  read.csv(
    "C:\\git\\data-thesis\\R\\datasets\\transferred-16121319-victim-data-raw.csv",
    header = TRUE
  )
fsd <- NULL; # feature data set;

wSize <- 5; #window size
wIndex <- 0;

wJumper <- as.integer(wSize / 2);


for (i in 1:nrow(rawdata)) {
  if (i > 0 & i %% wJumper == 0 & i <= nrow(rawdata) - wJumper) {
    fsd$Timestamp[wIndex] <- # timestamp
      as.character(rawdata$Timestamp[i - wJumper - 1]);
    
    
    fsd$XrmsA[wIndex] <- sqrt(sum((rawdata$x[rawdata$id >= i - wJumper - 1 & rawdata$id < i + wJumper])^2)/wSize);
    fsd$YrmsA[wIndex] <- sqrt(sum((rawdata$y[rawdata$id >= i - wJumper - 1 & rawdata$id < i + wJumper])^2)/wSize);
    fsd$ZrmsA[wIndex] <- sqrt(sum((rawdata$z[rawdata$id >= i - wJumper - 1 & rawdata$id < i + wJumper])^2)/wSize);
    fsd$XrmsG[wIndex] <- sqrt(sum((rawdata$alpha[rawdata$id >= i - wJumper - 1 & rawdata$id < i + wJumper])^2)/wSize);
    fsd$XrmsG[wIndex] <- sqrt(sum((rawdata$beta[rawdata$id >= i - wJumper - 1 & rawdata$id < i + wJumper])^2)/wSize);
    fsd$XrmsG[wIndex] <- sqrt(sum((rawdata$gamma[rawdata$id >= i - wJumper - 1 & rawdata$id < i + wJumper])^2)/wSize);
    wIndex <- wIndex + 1;
  }
  i <- i + wJumper;
  
}
write.csv(fsd,
          "C:\\git\\data-thesis\\R\\datasets\\new.csv",
          row.names = FALSE)