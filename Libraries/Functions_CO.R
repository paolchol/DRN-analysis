# Functions for the comparison of the time series

change_date_WASA_input = function(date_col){
  #From the format ot the date in the WASA input, return a date column
  #in the Date format (YYYY-MM-DD)
  
  n_digits <- floor(log10(date_col)) + 1
  date_col[which(n_digits == 7)] <- paste0("0", date_col[which(n_digits == 7)])
  date_col <- as.Date(date_col, format = "%d%m%Y")
  return(date_col)
}

plot_subbasins_df = function(df, y = "Value", label = "Title", interactive = FALSE, file = "plot",
                             save = FALSE, path = "nopath", doy = TRUE, line = FALSE){
  require(reshape2)
  if(doy) df[,2] <- NULL
  df <- melt(df, id.vars = 'date', variable.name = 'Subbasins')
  if(line){
    p <- ggplot(df, aes(date, value)) + geom_line(aes(colour = Subbasins), alpha = 0.5, size = 1.5) +
      xlab("Date") + ylab(y) + ggtitle(label)
  }else{
    p <- ggplot(df, aes(date, value)) + geom_point(aes(colour = Subbasins), alpha = 0.5) +
      xlab("Date") + ylab(y) + ggtitle(label)
  }
  if(interactive){
    ggplotly(p)
    if(save){
      #Saves an interactive version
      if(path == "nopath") htmlwidgets::saveWidget(plotly::ggplotly(p), paste0(file,".html"))
      else htmlwidgets::saveWidget(plotly::ggplotly(p), paste0(path, "/",file,".html"))
    }
  }else if(save){
    #Saves a static version
    ggsave(
      filename = paste0(file,".png"),
      plot = p,
      device = "png",
      path = path,
      scale = 1,
      width = 40,
      height = 30,
      units = "cm",
      dpi = 300,
      limitsize = TRUE
    )
  }else{
    print(p)
  }
}

RMSE = function(m, o){
  #m: model values
  #o: observed values
  sqrt(mean((m - o)^2, na.rm = TRUE))
}

compare_TS_stat = function(gen_TS, alex_TS, start = "2005-01-01", end = "2010-12-31", skip = 2){
  #A function to compare the two time series statistically
  #gen_TS and alex_TS are in the format of WASA input files
  #The first column has to be changed into a date column BEFORE entering the function
  #Returns a dataframe/report containing:
  # - RMSE between the two time series
  # - Statistics of the two time series (mean, median and sd)
  skip <- skip + 1
  #Select only values in the specified interval
  time_index<-seq(as.Date(start), as.Date(end), by = "days")
  gen_TS <- gen_TS[gen_TS[,1] %in% time_index, ]
  alex_TS <- alex_TS[alex_TS[,1] %in% time_index, ]
  if(nrow(gen_TS) != nrow(alex_TS)){
    gen_TS <- gen_TS[gen_TS[,1] %in% alex_TS[,1], ]
    alex_TS <- alex_TS[alex_TS[,1] %in% gen_TS[,1], ]
  }
  #Create the report structure
  report<-data.frame(colnames(gen_TS)[skip:ncol(gen_TS)])
  names(report)<-"SubbasinID"
  report$RMSE <- NA; report$range <- NA; report$NRMSE <- NA
  #Fill in the report
  j<-1
  for(i in skip:ncol(gen_TS)){
    report$RMSE[j] <- RMSE(gen_TS[,i], alex_TS[,i])
    m <- min(c(min(gen_TS[,i], na.rm = TRUE), min(alex_TS[,i], na.rm = TRUE)))
    M <- max(c(max(gen_TS[,i], na.rm = TRUE), max(alex_TS[,i], na.rm = TRUE)))
    report$range[j] <- paste0(m, " - ", M)
    report$NRMSE[j] <- report$RMSE[j]/(M - m) 
    j<-j+1
  }
  mean_rm = function(x){return(mean(x, na.rm = TRUE))}
  report$mean_gen <- apply(gen_TS[skip:ncol(gen_TS)], 2, mean_rm)
  report$mean_alex <- apply(alex_TS[skip:ncol(alex_TS)], 2, mean_rm)
  median_rm = function(x){return(median(x, na.rm = TRUE))}
  report$median_gen <- apply(gen_TS[skip:ncol(gen_TS)], 2, median_rm)
  report$median_alex <- apply(alex_TS[skip:ncol(alex_TS)], 2, median_rm)
  sd_rm = function(x){return(sd(x, na.rm = TRUE))}
  report$sd_gen <- apply(gen_TS[skip:ncol(gen_TS)], 2, sd_rm)
  report$sd_alex <- apply(alex_TS[skip:ncol(alex_TS)], 2, sd_rm)
  return(report)
}

compare_TS_vis = function(gen_TS, alex_TS, skip = 2, y = "Value", label = "Title", interactive = FALSE, file = "plot",
                          save = FALSE, path){
  skip <- skip +1
  for(i in skip:ncol(gen_TS)){
    vis1 <- data.frame(gen_TS[,1], gen_TS[,i], rep("Generated", nrow(gen_TS)))
    names(vis1) <- c("date", "value", "ID")
    vis2 <- data.frame(alex_TS[,1], alex_TS[,i], rep("Alexandre", nrow(alex_TS)))
    names(vis2) <- c("date", "value", "ID")
    df<-rbind(vis1, vis2)
    p <- ggplot(df, aes(date, value)) + geom_point(aes(colour = ID), alpha = 0.5) +
      xlab("Date") + ylab(y) + ggtitle(paste0(label, " - ", colnames(gen_TS)[i]))
    if(interactive){
      #ggplotly(p)
      #Saves an interactive version
      htmlwidgets::saveWidget(plotly::ggplotly(p), paste0(file,".html"))
    }else if(save){
      #Saves a static version
      ggsave(
        filename = paste0(file,"_", colnames(gen_TS)[i],".png"),
        plot = p,
        device = "png",
        path = path,
        scale = 1,
        width = 40,
        height = 30,
        units = "cm",
        dpi = 300,
        limitsize = TRUE
      )
    }else{
      print(p)
    }
  }
}

