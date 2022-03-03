#Time series update

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/meteorological_stations_ceara")
library(tidyverse)
library(tictoc)

#Upload precipitation data

input<-list.files(path="./Prec_stations",full.names = T,recursive = TRUE)

prec_input<-list()
for(i in 1:length(input)){
  prec_input[[i]]<-read.table(input[i],header = TRUE,sep = ";")
}

#Clean the data

for(i in 1:length(prec_input[])){
  #If coordinates are 0, remove the station
  if(is.na(prec_input[[i]]$Latitude[1]) || prec_input[[i]]$Latitude[1] == 0){
    prec_input[[i]]<-NULL
  }
  if(i >= length(prec_input[])) break
}
for(i in 1:length(prec_input[])){
    #Put NA instead of 888
    prec_input[[i]]<-na_if(prec_input[[i]],888)
    prec_input[[i]]<-na_if(prec_input[[i]],999)
    #Create a unique ID for each station
    ID<-rep(paste0("Station ",i),length(prec_input[[i]]$Municipios))
    prec_input[[i]]<-data.frame(prec_input[[i]],ID)
}

#Give the stations' ID to the list
for(i in 1:length(prec_input[])){
  names(prec_input)[i]<-prec_input[[i]]$ID[1]
}

#Create the matrix and the positions vector

#Positions vector

for(i in 1:length(prec_input[])){
  name<-prec_input[[i]]$Postos[1]
  ID<-paste0("Station ",i)
  lat<-prec_input[[i]]$Latitude[1]
  long<-prec_input[[i]]$Longitude[1]
  if(i !=1 ){
    positions<-rbind(positions, data.frame(name,ID,long,lat))
  }else{
    positions<-data.frame(name,ID,long,lat)
  }
}

'''
library(sp)
library(rgdal)
library(tmap)

#Create a spatial vector
coordinates(positions)<-c("long","lat")
crs.pos<-CRS("+proj=longlat") #definition of the projection
proj4string(positions)<-crs.pos
summary(positions)
plot(positions, pch = 20, col='steelblue')
'''

#Data matrix

#Don't care about last three months (2021)

start_y<-1980; start_m<-1
end_y<-2010;   end_m<-12
test1 <- as.Date("1980-01-01")
test2 <- as.Date("2010-12-31")
test2-test1
ndays<-11322  #result of the difference above

## Matrix construction
st<-0
for(i in 1:length(prec_input[])){
  v<-prec_input[[i]][which(prec_input[[i]]$Anos == start_y & prec_input[[i]]$Meses == start_m),
                     9:ncol(prec_input[[i]])-1]
  if(dim(v)[1] != 0){
    if(st != 0){
      matrix<-data.frame(matrix,t(v))
      st<-append(st,prec_input[[i]]$ID[1])
    }else{
      matrix<-t(v)
      st<-prec_input[[i]]$ID[1]
    }
  }
}

#start_y e strat_m devono aumentare!

names(matrix)<-st
#This matrix contains the rainfall of the first month of the series, for the stations that present data
#The loop will have to be updated to consider all months of all years


#Interpolate the data

library(rgdal)
library(spatstat)  # Used for the dirichlet tessellation function
library(maptools)  # Used for conversion from SPDF to ppp
library(raster)    # Used to clip out thiessen polygons

path<-"C:/Thesis_fortran/WASA/shapefile/streamgauges/Subbasins.shp"
subbasins<-readOGR(path)

for(i in 1:length(matrix)){
    
    #First day data, with coordinates associated
    #NA are removed, and stations with NA on that day are not considered
    shape<-data.frame(rep(0,length(matrix)))
    shape$data<-t(matrix[i,][which(!is.na(matrix[i,]))]) #data
    col<-colnames(matrix[i,][which(!is.na(matrix[i,]))])
    shape$long<-positions$long[which(positions$ID %in% col)]
    shape$lat<-positions$lat[which(positions$ID %in% col)]
    shape<-shape[,2:4]
    
    #Creation of a spatial element
    coordinates(shape)<-c("long","lat")
    crs.pos<-CRS("+proj=longlat") #definition of the projection
    proj4string(shape)<-crs.pos
    shape <- spTransform(shape, crs(subbasins))
    shape@bbox <- subbasins@bbox
    
    #Interpolate to create a raster
    th <- as(dirichlet(as.ppp(shape)), "SpatialPolygons")
    proj4string(th) <- proj4string(shape)
    th.z <- over(th, shape)
    th.z$data[is.na(th.z$data)] <- 0
    th.spdf <- SpatialPolygonsDataFrame(th, th.z)
    
    #Superimpose the subbasin shapefile and obtain a mean of the values in the 
    #Thiessen polygons for each subbasin
    
    data_sub<-over(subbasins,th.spdf,fn=mean)
    #at the spatial locations of object x retrieves the indexes
    #or attributes from spatial object y
    
    #Save the values in a pre-made data.frame
    output[i,3:ncol(output)]<-t(data_sub)
    
    #in questo modo ho ottenuto i dati per ogni subbasin per il primo giorno
    #andando avanti con le righe di matrix, avanzo verso i giorni successivi dello stesso mese
    #finita la matrice, dovrò cambiarla con il mese successivo
    #controllare tutti gli indici i
}

#Ora
#-unire i cicli facendo un ciclo solo con sottocicli
#-prendendo solo !is.na sulle righe, negli anni bisestili/mesi che non hanno 31 giorni, shape$data sarà vuoto
#> inserire una condizione per fare in modo che non vada a sminchiare il data.frame



plot(subbasins)
proj4string(shape)<-proj4string(subbasins)
summary(shape)
summary(subbasins)

#Create the rasters (one raster for each day)

#Import the subbasin layer
#prima direttamente quello completo dello stato di Ceara

#Obtain the values relative to each subbasin (mean of the values in the subbasins areas)

#Create the input file

#Upload Alexandre's input file (precipitation)

#Check the two series, visually and maybe statistically

#If the results are good, repeat the procedures for the other data sources
