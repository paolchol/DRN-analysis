#Functions for data processing
#Outlier removal and NA filling

check_outlier_IQR = function(dataframe, skip = 1, maxcol = 0){
  #skip: number of columns to skip
  skip <- skip+1
  maxcol<-ifelse(maxcol!=0, maxcol, ncol(dataframe))
  for(i in skip:maxcol){
    Q1<-quantile(dataframe[,i],0.25, na.rm = TRUE)
    Q3<-quantile(dataframe[,i],0.75, na.rm = TRUE)
    IQR<-abs(Q3-Q1)
    upper_limit<-Q3 + 1.5*IQR
    lower_limit<-Q1 - 1.5*IQR
    print(paste0("Column: ", colnames(dataframe)[i], " \ Number of upper outliers: ",
                 sum(dataframe[,i] > upper_limit, na.rm = TRUE), " \ Number of lower outliers: ",
                 sum(dataframe[,i] < lower_limit, na.rm = TRUE)))
    print(paste0("Outlier percentage: ", 
                 round((sum(dataframe[,i] > upper_limit, na.rm = TRUE)+sum(dataframe[,i] < lower_limit, na.rm = TRUE))/
                   sum(!is.na(dataframe[,i])), 2)*100, " %"))
  }
}

check_outlier_3sd = function(dataframe, skip = 1, maxcol = 0){
  #skip: number of columns to skip
  skip <- skip+1
  maxcol<-ifelse(maxcol!=0, maxcol, ncol(dataframe))
  for(i in skip:maxcol){
    mean<-mean(dataframe[,i], na.rm = TRUE)
    sd<-sd(dataframe[,i], na.rm = TRUE)
    upper_limit <- 3*sd+mean
    lower_limit <- -3*sd+mean
    print(paste0("Column: ", colnames(dataframe)[i], " \ Number of upper outliers: ",
                 sum(dataframe[,i] > upper_limit, na.rm = TRUE), " \ Number of lower outliers: ",
                 sum(dataframe[,i] < lower_limit, na.rm = TRUE)))
    print(paste0("Outlier percentage: ", 
                 round((sum(dataframe[,i] > upper_limit, na.rm = TRUE)+sum(dataframe[,i] < lower_limit, na.rm = TRUE))/
                         sum(!is.na(dataframe[,i])), 2)*100, " %"))  }
}

remove_outlier_IQR = function(dataframe, skip = 1, fill_val = NA, maxcol = 0){
  skip <- skip+1
  maxcol<-ifelse(maxcol!=0, maxcol, ncol(dataframe))
  for(i in skip:maxcol){
      Q1<-quantile(dataframe[,i],0.25, na.rm = TRUE)
      Q3<-quantile(dataframe[,i],0.75, na.rm = TRUE)
      IQR<-abs(Q3-Q1)
      upper_limit<-Q3 + 1.5*IQR
      lower_limit<-Q1 - 1.5*IQR
      dataframe[,i][dataframe[,i] > upper_limit]<-fill_val
      dataframe[,i][dataframe[,i] < lower_limit]<-fill_val
  }
  return(dataframe)
}

remove_outlier_3sd = function(dataframe, skip = 1, fill_val = NA, maxcol = 0){
  skip <- skip+1
  maxcol<-ifelse(maxcol!=0, maxcol, ncol(dataframe))
  for(i in skip:maxcol){
    mean<-mean(dataframe[,i], na.rm = TRUE)
    sd<-sd(dataframe[,i], na.rm = TRUE)
    upper_limit <- 3*sd+mean
    lower_limit <- -3*sd+mean
    dataframe[,i][dataframe[,i] > upper_limit]<-fill_val
    dataframe[,i][dataframe[,i] < lower_limit]<-fill_val
  }
  return(dataframe)
}

check_outlier_stations = function(input_list, IQR = TRUE, sd3 = TRUE, skip = 3, maxcol = 4, plott = FALSE){
  #This function returns the results of a first check of the stations outliers,
  #both for IQR and 3sd analysis
  #If plott = TRUE returns also the plots of the outliers removal in the
  #configurations selected
  #Use it to decide which method to use to remove the outliers in the dataset
  if(IQR){
    print("IQR results below")
    for(i in 1:length(input_list[[2]])){
      print(paste0("Station ", i))
      check_outlier_IQR(input_list[[2]][[i]], skip = skip, maxcol = maxcol)
    }
  }
  if(sd3){
    print("3sd results below")
    for(i in 1:length(input_list[[2]])){
      print(paste0("Station ", i))
      check_outlier_3sd(input_list[[2]][[i]], skip = skip, maxcol = maxcol)
    }
  }
  if(plott){
    input_IQR <- input_3sd <- input_list
    for(i in 1:length(input_list[[2]])){
      if(IQR) input_IQR[[2]][[i]] <- remove_outlier_IQR(input_list[[2]][[i]], skip = 3, maxcol = 4)
      if(sd3) input_3sd[[2]][[i]] <- remove_outlier_3sd(input_list[[2]][[i]], skip = 3, maxcol = 4)
    }
    if(IQR) plot_stations(input_IQR, 4, 5, label = "IQR")
    if(sd3) plot_stations(input_3sd, 4, 5, label = "3sd")
  }
}

remove_outlier_list = function(input_list, IQR = TRUE, sd3 = FALSE, skip = 3, maxcol = 4){
  for(i in 1:length(input_list[[2]])){
    if(IQR) input_list[[2]][[i]] <- remove_outlier_IQR(input_list[[2]][[i]], skip = skip, maxcol = maxcol)
    if(sd3) input_list[[2]][[i]] <- remove_outlier_3sd(input_list[[2]][[i]], skip = skip, maxcol = maxcol)
  }
  return(input_list)
}

plot_dataframe = function(df, skip = 1, label = "Variable"){
  require(lattice)
  skip <- skip+1
  for(i in skip:ncol(df)){
    print(xyplot(df[,i]~df$date, pch=19,
                 ylab = paste0(label, " - ID ", colnames(df)[i])))
  }
}

find_anomalous_stations = function(input_list, value = 0){
  #This function checks if some stations present negative values
  j<-0
  for(i in 1:length(input_list[[2]])){
    neg <- sum(input_list[[2]][[i]][['data']] < value, na.rm = TRUE)
    if(neg > 0){
      print(paste0("Station ",i, " presents ", neg, " negative values"))
      j<-j+1
    }
  }
  print(paste0("Total number of anomalous stations: ", j))
  return(j)
}

remove_anomalous_stations = function(input_list, value = 0, w_vec = FALSE, vector){
  j<-ifelse(w_vec,length(vector), 0)
  if(!w_vec){
    x<-rep(NA, length(input_list[[2]]))
    for(i in 1:length(input_list[[2]])){
      neg <- sum(input_list[[2]][[i]][['data']] < value, na.rm = TRUE)
      if(neg > 0){
        input_list[[1]][i]<-NA
        j<-j+1
        x[j]<-i
      }
    }
  }
  if(j > 0){
    if(w_vec) x<-vector else x<-x[!is.na(x)]
    new_list <- list()
    new_list[[1]] <- input_list[[1]][!is.na(input_list[[1]])]
    new_list[[2]] <- input_list[[2]][-x]
    print(paste0(j, " stations have been removed"))
    return(new_list)
  }else{
    print(paste0("No stations to be removed"))
  }
}

plot_stations = function(input_list, x, y, label = "Variable", select = FALSE, vector = 0){
  #vector is the index of the stations you want to plot, inside input_list[[2]]
  require(lattice)
  if(!select){
    for(i in 1:length(input_list[[2]])){
      print(xyplot(input_list[[2]][[i]][,x]~input_list[[2]][[i]][,y],
                   pch = 19, main = paste0(label, " - Station ", i),
                   xlab = "Years", ylab = label))
    }
  }else{
    for(i in 1:length(vector)){
      j<-vector[i]
      print(xyplot(input_list[[2]][[j]][,x]~input_list[[2]][[i]][,y],
                   pch = 19, main = paste0(label, " - Station ", i),
                   xlab = "Years", ylab = label))
    }
  }
}

plot_stations_df = function(df, y = "Value", label = "Title", interactive = FALSE, file = "plot",
                            save = FALSE, path){
  require(reshape2)
  df<-melt(df, id.vars = 'date', variable.name = 'Stations')
  p <- ggplot(df, aes(date, value)) + geom_point(aes(colour = Stations), alpha = 0.5) +
    xlab("Date") + ylab(y) + ggtitle(label)
  if(interactive){
    #ggplotly(p)
    #Saves an interactive version
    htmlwidgets::saveWidget(plotly::ggplotly(p), paste0(file,".html"))
  }else print(p)
  
  if(save){
  #Saves a static version
    ggsave(
      filename = paste0(path,"/",file,".png"),
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
  }
}

extract_max = function(df, start_y = 1980, end_y = 2018, datecol = 1, skip = 1){
  skip<-skip+1
  maxima<-as.data.frame(matrix(NA, nrow = length(seq(start_y,end_y,1)), ncol = ncol(df)))
  names(maxima)<-c("year", colnames(df)[skip:length(colnames(df))])
  maxima$year<-seq(start_y,end_y,1)
  j<-1
  y<-start_y
  while(y <= end_y){
    for(i in skip:ncol(df)){
      maxima[j,i]<-max(df[which(lubridate::year(df[,datecol]) == y),i], na.rm = TRUE)
    }
    j<-j+1; y<-y+1
  }
  return(maxima)
}

rejection_threshold = function(df, method = "vector", v, skip = 1){
  skip<-skip+1
  count<-0
  if(method == "vector"){
    for(i in skip:ncol(df)){
      count<-count+sum(df[,i] > v[i-1], na.rm = TRUE)
      df[,i][df[,i] > v[i-1]] <- NA
    }
  }else if(method == "value"){
    count<-count + sum(df[,skip:ncol(df)] > v, na.rm = TRUE)
    df[,skip:ncol(df)][df[,skip:ncol(df)] > v] <- NA
  }
  print(paste0("Number of values changed in the entire dataset: ", count))
  return(df)
}

anomalous_prec_st = function(prec_df, threshold = 250){
  prec_maxima <- extract_max(prec_df)
  xx = function(x){x[is.infinite(x)]<-NA; return(x)}
  prec_maxima <- as.data.frame(apply(prec_maxima, 2, xx))
  xx = function(x){y<-mean(x, na.rm = TRUE); return(y)}
  prec_mean_max <- apply(prec_maxima[,2:ncol(prec_maxima)], 2, xx)
  remove_st <- which(prec_mean_max > 250) + 1
  
  print(paste0(sum(prec_mean_max > 250, na.rm = TRUE), " stations will be removed"))
  return(remove_st)
}

count_na = function(df, skipcol = 1, maxcol = 0){
  firstcol <- skipcol+1
  maxcol<-ifelse(maxcol!=0, maxcol, ncol(df))
  
  for(i in firstcol:maxcol){
    index <- min(which(!is.na(df[,i])))
    count <- sum(is.na(df[index:length(df[,i]),i]))
    print(paste0("Column: ", colnames(df)[i], " has ", count, " missing values"))
  }
}

fill_na = function(df, skipcol = 1, k = 7, maxgap = 31, maxcol = 0){
  #skipcol: number of columns to skip
  firstcol<-1+skipcol
  maxcol<-ifelse(maxcol!=0, maxcol, ncol(df))
  for(i in firstcol:maxcol){
    index <- min(which(!is.na(df[,i])))
    df[index:length(df[,i]),i]<-imputeTS::na_ma(df[index:length(df[,i]),i], k = k, maxgap = maxgap)
  }
  return(df)
}

mean_previous_years = function(df, skipcol = 2, maxcol = 0){
  firstcol<-1+skipcol
  maxcol<-ifelse(maxcol!=0, maxcol, ncol(df))
  for(i in firstcol:maxcol){
    index <- min(which(!is.na(df[,i])))
    
    month<-lubridate::month(df[,1])
    day<-lubridate::day(df[,1])
    month_day<-data.frame(month, day)
    
    date <- df[index:length(df[,i]),1][which(is.na(df[index:length(df[,i]),i]))]
    data <- df[index:length(df[,i]),i][which(is.na(df[index:length(df[,i]),i]))]
    missing_values <- data.frame(date, data)
    
    for(j in 1:nrow(missing_values)){
      month <- lubridate::month(missing_values$date[j])
      day <- lubridate::day(missing_values$date[j])
      missing_values$data[j] <- mean(df[,i][lubridate::month(df[,1]) == month &
                                              lubridate::day(df[,1]) == day],
                                     na.rm = TRUE)
    }
    
    df[index:length(df[,i]),i][which(is.na(df[index:length(df[,i]),i]))] <-
      missing_values$data
  }
  return(df)
}

change_date_format = function(dates, format_i = "%Y-%m-%d"){
  dates <- as.Date(dates, format = format_i)
  return(dates)
}
