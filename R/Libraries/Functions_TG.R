#Functions to execute the time series generation

create_date_vector <- function(start_y, end_y, start_m = 01, end_m = 12,
                              start_d = 01, end_d = 31) {
  start <- paste0(start_d, "/0", start_m, "/", start_y)
  end <- paste0(end_d, "/", end_m, "/", end_y)
  date <- seq(as.Date(start, "%d/%m/%Y"), as.Date(end, "%d/%m/%Y"), by = "days")
  return(date)
}

create_data_list <- function(path, names, skip = 6, f = read.table, s = "") {
  #skip = 6 for the FUNCEME files in .txt
  #skip = 1 for the volume observation values in .csv
  #skip = 3 for WASA input files

  files <- list.files(path = path, full.names = T, recursive = TRUE)
  data_list <- list()
  for (i in seq_len(length(files))) {
    tryCatch(data_list[[i]] <- f(files[i], header = FALSE,
                                 sep = s,col.names = names, skip = skip),
             error = function(e) {cat("ERROR :", conditionMessage(e), i, "\n")})
  }
  return(list(files, data_list))
}

create_position_vector <- function(files){
  #Positions vector
  for(i in seq_len(length(files))){
    lines<-readLines(files[i],n = 6)
    ID<-as.numeric(unlist(strsplit(lines[1], ":"))[2])
    name<-unlist(strsplit(lines[2], ":"))[2]
    long<-as.numeric(unlist(strsplit(lines[5], ":"))[2])
    lat<-as.numeric(unlist(strsplit(lines[6], ":"))[2])
    p<-data.frame(ID,name,long,lat)
    if(i > 1) positions<-rbind(positions,p,make.row.names=FALSE)
    else positions<-p
  }
  positions$ID<-paste0("St",positions$ID)
  return(positions)
}

add_date_column <- function(data_list, complete = FALSE){
  #Creates a date column inside each station dataframe
  if (!complete) {
    for(i in seq_len(length(data_list[]))){
      d <- as.Date(paste0(data_list[[i]]$year,"-",data_list[[i]]$month,"-",
                        data_list[[i]]$day))
      data_list[[i]]$date<-d
    }
  }else{
    for(i in seq_len(length(data_list[]))){
      data_list[[i]]$date <- as.Date(data_list[[i]]$date, "%Y-%m-%d")
    }
  }
  return(data_list)
}

create_main_dataframe <- function(date, data_list, positions,
                                  timecol = 5, datacol = 4){
  main_dataframe<-data.frame(date)
  index<-matrix(NA,ncol = 2,nrow = length(data_list[]))
  j<-1
  
  for(i in seq_len(length(data_list[]))){
    check<-data_list[[i]][which(data_list[[i]]$date %in% main_dataframe$date),]
    if(dim(check)[1]!=0){
      # main_dataframe$new<-rep(NA,nrow(main_dataframe))
      new_v <- sistemadati_DCA(vettore = data_list[[i]],
                              lung = nrow(main_dataframe),
                              g = date, coltime = timecol, coldata = datacol)
      main_dataframe$new<-new_v[,2]
      names(main_dataframe)[names(main_dataframe) == "new"]<-positions$ID[i]
    }else{
      index[j,1]<-i
      index[j,2]<-positions$ID[i]
      j<-j+1
    }
  }
  return(main_dataframe)
  # index_clean <- subset(index, (!is.na(index[,1])) & (!is.na(index[,2])))
  # return(main_dataframe, index_clean)
  #Uncomment it to see if there are stations that have been removed
  #because they don't have data in the specified date range
}

WASA_input_format <- function(main_df, output, header, path, name){
  year<-lubridate::year(main_df$date)
  month<-lubridate::month(main_df$date)
  month[which(month < 10)]<-paste0("0",month[which(month < 10)])
  day<-lubridate::day(main_df$date)
  day[which(day < 10)]<-paste0("0",day[which(day < 10)])
  #Insert the number of days
  #output$`number of days`<-lubridate::yday(main_df$date)
  output[,2] <- seq(1,nrow(output), 1)
  #Insert the date in the WASA format
  output$date<-paste0(day,month,year)
  #Round the values
  output[,3:ncol(output)]<-round(output[,3:ncol(output)], digits = 1)
  my.write(output, file = paste0(path,"/", name,".dat"),
           header = header, f = write.table,sepset = FALSE)
  return(output)
}