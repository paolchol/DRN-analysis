#Test on the over function to create the time series

#Create the file list
setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/meteorological_stations_ceara")

#FUNCEME dataset
input<-list.files(path="./Meteorological_stations_FUNCEME/precipitation_subset",full.names = T,recursive = TRUE)

prec_input<-list()
for(i in 1:length(input)){
  tryCatch(prec_input[[i]]<-read.table(input[i],header = FALSE,
                                       sep = "",col.names = c("year","month","day","data"),
                                       skip=6),
           error=function(e){cat("ERROR :",conditionMessage(e),i, "\n")})
}

#Create the position vector
#Positions vector
for(i in 1:length(input)){
  lines<-readLines(input[i],n = 6)
  ID<-as.numeric(unlist(strsplit(lines[1], ":"))[2])
  name<-unlist(strsplit(lines[2], ":"))[2]
  long<-as.numeric(unlist(strsplit(lines[5], ":"))[2])
  lat<-as.numeric(unlist(strsplit(lines[6], ":"))[2])
  p<-data.frame(ID,name,long,lat)
  if(i > 1) positions_prec<-rbind(positions_prec,p,make.row.names=FALSE)
  else positions_prec<-p
}
positions_prec$ID<-paste0("St",positions_prec$ID)
remove(lines, ID, name, long, lat, p, input)

#Upload the main dataframe of precipitation data
path<-"C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/time_series_generation/main_datasets"
main_dataframe_prec<-read.table(paste0(path,"/main_dataframe_precipitation_opt_v2.txt"),
                                header = TRUE, sep="\t")

#Import of the subbasins shapefile
path<-"C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/shapefile/streamgauges/Subbasins_geomfix.shp"
subbasins<-readOGR(path)

main_dataframe = main_dataframe_prec
positions = positions_prec
layer = subbasins
dates = date

  #main_dataframe: the main dataframe with all stations observations
  #positions: the dataframe containing the positions of the stations
  #layer: the layer to superimpose to the Voronoi diagram
  #dates: vector containing the dates to consider when creating the output

#Boundaries of the Voronoi diagram
corners<-c(-41.64, -37.26, -7.85, -2.78) #(xmin, xmax, ymin, ymax)
  
i<-4
tic()
#Extract the data for the ith day
first<-main_dataframe[i,which(!is.na(main_dataframe[i,]))]
if(!is.null(dim(first)[1]) && dim(first)[2] > 2){
#Extract the positions of the stations and associate them to the data
col<-colnames(first); col<-col[2:length(col)]
second<-first[,2:ncol(first)]
shape<-data.frame(t(second),
                        positions$long[which(positions$ID %in% col)],
                        positions$lat[which(positions$ID %in% col)])
names(shape)<-c("data","long","lat")
#Create the Voronoi diagram
thiessen<-voronoipolygons(shape, corners=corners)
proj4string(thiessen)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
thiessen <- spTransform(thiessen, raster::crs(layer))
#summary(thiessen)
      
#Overlay the layer layer to extract the mean precipitation
#over function: at the spatial locations of object x retrieves the indexes
#or attributes from spatial object y

#data_sub<-over(layer, thiessen, fn = mean)
data_sub<-intersect(layer, thiessen)
data_sub_mean<-tapply(data_sub@data$z, data_sub@data$SubbasinID, mean)

View(mean)
}
toc()

library(rgdal)

#Export
path<-"C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/time_series_generation/test_interpolation"
#Export the precipitation stations    
coordinates(shape)<-c("long","lat")
proj4string(shape)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
shape <- spTransform(shape, raster::crs(subbasins))
raster::shapefile(shape,paste0(path,"/prec_stations_test1.shp"))

#Export the interpolated precipitation
try<-data_sub
coordinates(try)<-c("x","y")
proj4string(try)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
try <- spTransform(try, raster::crs(subbasins))
raster::shapefile(try,paste0(path,"/prec_interp_test1.shp"),overwrite=TRUE)

#Export the thiessen polygons shapefile
raster::shapefile(thiessen,paste0(path,"/prec_thiessen_test1.shp"),overwrite=TRUE)  
    
#Export the intersected layer
raster::shapefile(data_sub,paste0(path,"/prec_intersect_test2.shp"),overwrite=TRUE)

#Test
#1: the current algorithm, using over in its standard setting
#2: using intersect instead of over. Using the fixed subbasins shapefile
#3: over using the fixed subbasins shapefile




    
