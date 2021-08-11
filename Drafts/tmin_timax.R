
setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/meteorological_stations_ceara")

#FUNCEME dataset
input<-list.files(path="./Meteorological_stations_FUNCEME/t2mmin",full.names = T,recursive = TRUE)

temp_input<-list()
for(i in 1:length(input)){
  tryCatch(temp_input[[i]]<-read.table(input[i],header = FALSE,
                                       sep = "",col.names = c("year","month","day","data"),
                                       skip=6),
           error=function(e){cat("ERROR :",conditionMessage(e),i, "\n")})
}



#Positions vector
for(i in 1:length(input)){
  lines<-readLines(input[i],n = 6)
  ID<-as.numeric(unlist(strsplit(lines[1], ":"))[2])
  name<-unlist(strsplit(lines[2], ":"))[2]
  long<-as.numeric(unlist(strsplit(lines[5], ":"))[2])
  lat<-as.numeric(unlist(strsplit(lines[6], ":"))[2])
  p<-data.frame(ID,name,long,lat)
  if(i > 1) positions_temp<-rbind(positions_temp,p,make.row.names=FALSE)
  else positions_temp<-p
}
positions_temp$ID<-paste0("St",positions_temp$ID)

#Unified dataframe

start_y <- 1980
end_y <- 2020
start_m <- 01
end_m <- 12

start<-paste0("01/0",start_m,"/",start_y)
end<-paste0("31/",end_m,"/",end_y)
date<-seq(as.Date(start,"%d/%m/%Y"), as.Date(end,"%d/%m/%Y"), by="days")

##Create a date column inside each station dataframe
for(i in 1:length(temp_input[])){
  d<-as.Date(paste0(temp_input[[i]]$year,"-",temp_input[[i]]$month,"-",temp_input[[i]]$day))
  temp_input[[i]]$date<-d
}

tic("Unified dataframe Temperature - Optimized")

main_dataframe_temp<-data.frame(date)
index<-matrix(NA,ncol = 2,nrow = 744)
j<-1

for(i in 1:length(temp_input[])){
  check<-temp_input[[i]][which(temp_input[[i]]$date %in% main_dataframe_temp$date),]
  if(dim(check)[1]!=0){
    #main_dataframe_temp$new<-rep(NA,nrow(main_dataframe_temp))
    new_v<-sistemadati(vettore = temp_input[[i]],lung = nrow(main_dataframe_temp),g = date)
    main_dataframe_temp$new<-new_v[,2]
    names(main_dataframe_temp)[names(main_dataframe_temp) == "new"]<-positions_temp$ID[i]
  }else{
    index[j,1]<-i
    index[j,2]<-positions_temp$ID[i]
    j<-j+1
  }
}
index_clean<-subset(index, (!is.na(index[,1])) & (!is.na(index[,2])))
toc() #154.5 sec = 2.575 min

'''
x_max<-rep(1,10)
for(i in 0:10){
  na_count<-sum(is.na(main_dataframe_temp[which(lubridate::year(main_dataframe_temp$date)==1985+i),]))
  non_na_count<-sum(!is.na(main_dataframe_temp[which(lubridate::year(main_dataframe_temp$date)==1985+i),]))
  x_max[i]<-na_count/(non_na_count+na_count)
}
View(x_max)
'''
x_min<-rep(1,10)
for(i in 0:10){
  na_count<-sum(is.na(main_dataframe_temp[which(lubridate::year(main_dataframe_temp$date)==1985+i),]))
  non_na_count<-sum(!is.na(main_dataframe_temp[which(lubridate::year(main_dataframe_temp$date)==1985+i),]))
  x_min[i]<-na_count/(non_na_count+na_count)
}
View(x_min)

View(x_min*100)