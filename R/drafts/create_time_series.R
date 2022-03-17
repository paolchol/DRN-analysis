create_time_series = function(main_dataframe, positions, layer, dates){
  #main_dataframe: the main dataframe with all stations observations
  #positions: the dataframe containing the positions of the stations
  #layer: the layer to superimpose to the Voronoi diagram
  #dates: vector containing the dates to consider when creating the output
  
  output<-data.frame(matrix(NA, nrow = length(dates),
                            ncol=2+length(layer@data$SubbasinID)))
  names(output)<-c("date","number of days",t(layer@data$SubbasinID))
  j<-1
  
  #Boundaries of the Voronoi diagram
  corners<-c(-41.64, -37.26, -7.85, -2.78) #(xmin, xmax, ymin, ymax)
  
  for(i in 1:nrow(main_dataframe)){
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
      data_sub<-over(layer, thiessen, fn=mean)
      
      #Save the values in a pre-made data.frame
      output[j,3:ncol(output)]<-t(data_sub[,3])
    }else if(!is.null(dim(first)[1]) && dim(first)[2] == 2){
      output[j,3:ncol(output)]<-rep(first[,2],length(output[j,3:ncol(output)]))
    }
    j <- j+1
  }
  output
}


