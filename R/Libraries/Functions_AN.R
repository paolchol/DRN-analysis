#Functions for the analysis


# Drought Cycle Analysis --------------------------------------------------

meanx <- function(x) mean(x, na.rm = TRUE)

VD <- function(df, maxcap, skip = 1){
  skip <- skip + 1
  
  VD <- data.frame(date = df$date)
  for(i in skip:ncol(df)){
    VD$new <- (df[, i] - maxcap[i-skip+1]/2)/(maxcap[i-skip+1]/2)
    names(VD)[names(VD) == "new"] <- colnames(df)[i]
  }
  return(VD)
}

quadrant_attribution = function(PI, WSI, dates){
  #PI: Precipitation Index
  #WSI: Water Scarcity Index
  
  DCA <- data.frame(dates, PI, WSI)
  names(DCA) <- c("date","PI", "WSI")
  DCA$quadrant <- NA
  for(i in seq_len(nrow(DCA))){
    if(!is.na(DCA$PI[i]) & !is.na(DCA$WSI[i])){
      if(DCA$PI[i] > 0 & DCA$WSI[i] > 0)      DCA$quadrant[i] <- 1
      else if(DCA$PI[i] < 0 & DCA$WSI[i] > 0) DCA$quadrant[i] <- 2
      else if(DCA$PI[i] < 0 & DCA$WSI[i] < 0) DCA$quadrant[i] <- 3
      else if(DCA$PI[i] > 0 & DCA$WSI[i] < 0) DCA$quadrant[i] <- 4
    }
  }
  return(DCA)
}
