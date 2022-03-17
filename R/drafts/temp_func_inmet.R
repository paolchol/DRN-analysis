#Temperature with only FUNCEME stations
setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/meteorological_stations_ceara")

for(kk in 1:2){
if(kk == 1){
  #FUNCEME dataset
  input<-list.files(path="./Meteorological_stations_FUNCEME/t2m_FUNCEME",full.names = T,recursive = TRUE)
  
  temp_input<-list()
  for(i in 1:length(input)){
    tryCatch(temp_input[[i]]<-read.table(input[i],header = FALSE,
                                         sep = "",col.names = c("year","month","day","data"),
                                         skip=6),
             error=function(e){cat("ERROR :",conditionMessage(e),i, "\n")})
  }
}else{
  #INMET dataset
  input<-list.files(path="./Meteorological_stations_FUNCEME/t2m_INMET",full.names = T,recursive = TRUE)
  
  temp_input<-list()
  for(i in 1:length(input)){
    tryCatch(temp_input[[i]]<-read.table(input[i],header = FALSE,
                                         sep = "",col.names = c("year","month","day","data"),
                                         skip=6),
             error=function(e){cat("ERROR :",conditionMessage(e),i, "\n")})
  }
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

remove(index,j,new_v)
#View(index_clean)

path <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/time_series_generation/main_datasets"
if(kk == 1){
  write.table(main_dataframe_temp,paste0(path,"/main_dataframe_temperature_FUNCEME_v1.txt"),
              quote=FALSE,sep="\t",row.names = FALSE)
}else{
  write.table(main_dataframe_temp,paste0(path,"/main_dataframe_temperature_INMET_v1.txt"),
              quote=FALSE,sep="\t",row.names = FALSE)
}

#Import of the subbasins shapefile
path<-"C:/Thesis_fortran/WASA/shapefile/streamgauges/Subbasins.shp"
subbasins<-readOGR(path)

tic("Output generation - Temperature - With function")
output_temp<-create_time_series(main_dataframe = main_dataframe_temp,
                                positions = positions_temp,
                                layer = subbasins,
                                dates = date)
toc()

#Insert the date
year<-lubridate::year(main_dataframe_temp$date)
month<-lubridate::month(main_dataframe_temp$date)
month[which(month < 10)]<-paste0("0",month[which(month < 10)])
day<-lubridate::day(main_dataframe_temp$date)
day[which(day < 10)]<-paste0("0",day[which(day < 10)])

output_temp$date<-paste0(day,month,year)

#Insert the number of days
output_temp$`number of days`<-lubridate::yday(main_dataframe_temp$date)

#Save it as it is for future use
path<-"C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/time_series_generation/output_time_series_gen"
if(kk == 1){
  write.table(output_temp,paste0(path,"/output_temp_FUNCEME.txt"),quote = FALSE,sep="\t",row.names = FALSE)
}else{
  write.table(output_temp,paste0(path,"/output_temp_INMET.txt"),quote = FALSE,sep="\t",row.names = FALSE)
}
}