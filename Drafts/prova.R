#Data matrix
library(tictoc)
start_y<-1980; start_m<-1
end_y<-2010;   end_m<-12
test1 <- as.Date("1980-01-01")
test2 <- as.Date("2010-12-31")
test2-test1
ndays<-11322  #result of the difference above

#Import of the subbasins shapefile
path<-"C:/Thesis_fortran/WASA/shapefile/streamgauges/Subbasins.shp"
subbasins<-readOGR(path)

#Output dataframe creation
output<-data.frame(matrix(NA,nrow=ndays,ncol=2+length(subbasins@data$SubbasinID)))
names(output)<-c("date","number of days",t(subbasins@data$SubbasinID))

tic("Output generation")
j<-1
for(year in start_y:end_y){
  for(month in start_m:end_m){
    #Creation of a matrix containing the data for all the stations for 1 month
    st<-0
    for(i in 1:length(prec_input[])){
      v<-prec_input[[i]][which(prec_input[[i]]$Anos == year & prec_input[[i]]$Meses == month),
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
    names(matrix)<-st
    for(i in 1:nrow(matrix)){
      #First day data, with coordinates associated
      #NA are removed, and stations with NA on that day are not considered
      check<-matrix[i,][which(!is.na(matrix[i,]))]
      if(ncol(check)>0){
        shape<-data.frame(rep(0,length(check)))
        shape$data<-t(check) #data
        col<-colnames(check)
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
        output[j,3:ncol(output)]<-t(data_sub)
        j <- j+1
        #}else if(i!=nrow(matrix)){
        #  j <- j+1
      }
    }
    remove(shape,th,th.z,th.spdf,data_sub)
  }
}
toc()
#First try: it took 3h 38m

write.table(output,"rain_v1.txt",quote = FALSE,sep="\t",row.names = FALSE)

#Comparison between generated and Alexandre's time series

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis")

source("Libraries.R")
source("Functions.R")
library(plotly)
library(hrbrthemes)

path<-"C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/meteorological_stations_ceara"
rain_gen<-read.table(paste0(path,"/rain_v1.txt"),skip = 1)
path<-"C:/Thesis_fortran/WASA/Input/Time_series"
rain_alex<-read.table(paste0(path,"/rain_daily.dat"),skip = 3)
rain_gen<-rain_gen[1:11323,]
rain_alex<-rain_alex[1:11323,] #end in 2010

#Visualization of the time series

#Usual area chart
res<-c(123,156,160)
time_index<-seq(as.Date("1980-01-01"),as.Date("2010-12-31"),by="days")
for(i in 1:length(res)){
  #Creation of the dataset for the visualization
  n<-res[i]+2
  visual1<-data.frame(rain_alex[,n],rep("Alexandre",length(rain_alex[,n])),time_index)
  names(visual1)<-c("data","id","time")
  visual2<-data.frame(rain_gen[,n],rep("Generated",length(rain_gen[,n])),time_index)
  names(visual2)<-c("data","id","time")
  visual<-cbind(rbind(visual1,visual2))
  #Usual area chart
  Legend<-visual$id
  p<-ggplot(data = visual,
            aes(x = `time`,
                y = `data`))+
    geom_area(aes(colour = Legend),alpha = 0.5) +
    labs(x = "Date",
         y = "Precipitation", 
         title = paste0("Subbasin ",res[i])) +
    theme_ipsum()
  print(p)
  path<-"C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/plots"
  ggsave(
    filename = paste0("gen_plot_",res[i],".png"),
    plot = p,
    device = "png",
    path = path,
    scale = 1,
    width = NA,
    height = NA,
    units = c("in", "cm", "mm"),
    dpi = 300,
    limitsize = TRUE
  )
  #Histograms
  p <- visual %>%
    ggplot( aes(x=data, fill=id)) +
    geom_histogram( color="#e9ecef", alpha=0.5, position = 'identity') +
    #scale_fill_manual(values=c("#69b3a2", "#404080")) +
    labs(x = "Precipitation",
         y = "Count",
         title = paste0("Subbasin ",res[i]))+
    theme_ipsum()
  print(p)
  ggsave(
    filename = paste0("hist_gen_",res[i],".png"),
    plot = p,
    device = "png",
    path = path,
    scale = 1,
    width = NA,
    height = NA,
    units = c("in", "cm", "mm"),
    dpi = 300,
    limitsize = TRUE
  )
}


# Usual area chart
p <- visual %>%
  ggplot( aes(x = time, y = data)) +
  geom_area(aes(colour = Legend), alpha=0.5) +
  geom_line(color="#69b3a2") +
  theme_ipsum()

# Turn it interactive with ggplotly
p <- ggplotly(p)
p

vis<-data.frame(visual1$data,visual2$data)
names(vis)<-c("vis1","vis2")
p <- ggplot(vis, aes(x = x) ) +
  # Top
  geom_density( aes(x = vis1, y = ..density..)) +
  geom_label( aes(x=4.5, y=1, label="variable1")) +
  # Bottom
  geom_density( aes(x = vis2, y = -..density..)) +
  geom_label( aes(x=4.5, y=-1, label="variable2")) +
  theme_ipsum() +
  xlab("value of x")
p
