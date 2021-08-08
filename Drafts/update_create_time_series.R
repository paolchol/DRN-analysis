main_dataframe <- main_dataframe_hum
positions <- positions_hum
layer <- subbasins
i<-1

create_time_series_Wmean = function(main_dataframe, positions, layer){
  #main_dataframe: the main dataframe with all stations observations
  #positions: the dataframe containing the positions of the stations
  #layer: the layer to superimpose to the Voronoi diagram
  
  require(deldir)
  require(sp)
  output<-data.frame(matrix(NA, nrow = nrow(main_dataframe),
                            ncol=2+length(layer@data$SubbasinID)))
  names(output)<-c("date","number of days",t(layer@data$SubbasinID))
  j<-1
  
  #Boundaries of the Voronoi diagram
  #They are fixed over the whole Cearà state, in order to take into
  #account every station available
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
      thiessen <- spTransform(thiessen, layer@proj4string@projargs)
      #summary(thiessen)
      
      data_sub<-raster::intersect(layer, thiessen)
      areas<-0
      for(k in 1:length(data_sub)){
        areas[k]<-data_sub@polygons[[k]]@area
      }
      weighted_value<-tapply(data_sub@data$z*areas, data_sub@data$SubbasinID, sum)
      sum_area<-tapply(areas, data_sub@data$SubbasinID, sum)
      data_sub_Wmean<-weighted_value/sum_area
      remove(areas)
      
      #Save the values in a pre-made data.frame
      #Since the subbasins are sorted for growing values, the interpolated values
      #are ordered the same way, and we can directly store them into the output
      #dataframe
      output[j,3:ncol(output)]<-t(data_sub_Wmean)
    }else if(!is.null(dim(first)[1]) && dim(first)[2] == 2){
      output[j,3:ncol(output)]<-rep(first[,2],length(output[j,3:ncol(output)]))
    }
    j <- j+1
  }
  output
}
