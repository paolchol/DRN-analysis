#Functions

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

time_series_variance = function(main_dataframe, positions, layer){
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
  i<-1
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
      
      data_sub<-raster::intersect(layer, thiessen)
      areas<-0
      for(k in 1:length(data_sub)){
        areas[k]<-data_sub@polygons[[k]]@area
      }
      #Compute the weighted (unbiased) variance
      #If the result is "Inf", it means that only one station
      #has been used to obtain that data
      weighted_value<-tapply((data_sub@data$z - mean(data_sub@data$z))^2*areas, data_sub@data$SubbasinID, sum)
      sum_area<-tapply(areas, data_sub@data$SubbasinID, sum)
      num<-sum_area^2
      den<-num-tapply(areas^2, data_sub@data$SubbasinID, sum)
      unbiased_variance<-(weighted_value/sum_area)*(num/den)
      remove(areas)
      
      #Save the values in a pre-made data.frame
      #Since the subbasins are sorted for growing values, the interpolated values
      #are ordered the same way, and we can directly store them into the output
      #dataframe
      output[j,3:ncol(output)]<-t(unbiased_variance)
    }else if(!is.null(dim(first)[1]) && dim(first)[2] == 2){
      output[j,3:ncol(output)]<-rep(first[,2],length(output[j,3:ncol(output)]))
    }
    j <- j+1
  }
  output
}

create_time_series = function(main_dataframe, positions, layer, dates){
  #main_dataframe: the main dataframe with all stations observations
  #positions: the dataframe containing the positions of the stations
  #layer: the layer to superimpose to the Voronoi diagram
  #dates: vector containing the dates to consider when creating the output
  require(deldir)
  require(sp)
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
      data_sub<-over(layer, thiessen, fn = mean)
      
      #data_sub<-intersect(layer, thiessen)
      #data_sub_mean<-tapply(data_sub@data$z, data_sub@data$SubbasinID, mean)
      #data_sub_over == data_sub_mean
      #The result is the same
      
      #Save the values in a pre-made data.frame
      #Since the subbasins are sorted for growing values, the interpolated values
      #are ordered the same way, and we can directly store them into the output
      #dataframe
      output[j,3:ncol(output)]<-t(data_sub[,3])
    }else if(!is.null(dim(first)[1]) && dim(first)[2] == 2){
      output[j,3:ncol(output)]<-rep(first[,2],length(output[j,3:ncol(output)]))
    }
    j <- j+1
  }
  output
}

my.write <- function(x, file, header, f, sepset=TRUE, nafill=FALSE){
  #sepset = TRUE: sep = "\t", FALSE: sep = " "
  #create and open the file connection
  datafile <- file(file, open = 'wt')
  #close on exit
  on.exit(close(datafile))
  #if a header is defined, write it to the file
  if(!missing(header)) writeLines(header,con=datafile)
  #write the file using the defined function and required addition arguments  
  s<-ifelse(sepset,"\t"," ")
  if(nafill){
	f(x, datafile, sep = s, row.names = FALSE,
              col.names = FALSE, quote = FALSE, na = "")
	}else{
	f(x, datafile, sep = s, row.names = FALSE,
              col.names = FALSE, quote = FALSE)
	}
}

obt_monthly_values = function(dataset, total = TRUE){
  #dataset needs to have a column called date, where dates are stored
  #total = TRUE will add a new column containing the sum of the columns
  
  #Separate months and years in the precipitation dataframe
  dataset$year<-lubridate::year(dataset$date)
  dataset$month<-lubridate::month(dataset$date)
  
  #Create a new dataframe with monthly sums of daily precipitation
  for(i in 0:(end_y-start_y)){
    year<-rep(start_y+i,12)
    month<-seq(1,12,1)
    if(i != 0){
      month_dataset<-rbind(month_dataset,data.frame(year,month))
    }else  month_dataset<-data.frame(year, month)
  }
  
  #Add the monthly sums for each reservoir
  for(i in 2:(ncol(dataset)-2)){
    month_dataset$new<-0
    for(j in 1:nrow(month_dataset)){
      month_dataset$new[j]<-
        sum(dataset[,i][which(dataset$year == month_dataset$year[j] &
                                dataset$month == month_dataset$month[j])])
    }
    names(month_dataset)[names(month_dataset) == "new"]<-
      colnames(dataset)[i]
  }
  if(total){
    month_dataset$total<-0
    for(i in 1:nrow(month_dataset)){
      month_dataset$total[i]<-sum(month_dataset[i,3:ncol(month_dataset)], na.rm = TRUE) 
    }
  }
  month_dataset
}

remove_list_elements = function(list){
  #Removes elements of type "character" from a list
  i<-1
  while(i <= length(list[])){
    if(is.character(list[[i]])){
      list[[i]]<-NULL
    }else{
      i<-i+1
    }
  }
  list
}

sistemadati = function(vettore,lung,g,zero=FALSE){
  #Legenda
  #vettore = vettore dal quale prendere i dati
  #lung = lunghezza del nuovo vettore
  #g = serie di date da inserire nel nuovo vettore
  #zero = se 0, rimuove pone a 0 valori minori di 0
  
  #vettore[,2]<-numeri(vettore[,2])
  if(zero){
    vettore[,2]<-pulisci_dati(vettore[,2])
  }
  
  vett_n<-matrix(NA,nrow=lung,ncol=2)
  vett_n[,1]<-g
  
  j<-1
  vettore<-vettore[which(vettore[,5] %in% vett_n[,1]),]
  for(i in 1:nrow(vettore)){
    repeat{
      if(vett_n[j,1]==vettore[i,5]){
        vett_n[j,2]<-vettore[i,4]
        break
      }else{
        j<-j+1
      }
    }
  }
  vett_n
}

sistemadati_DCA = function(vettore, lung, g, zero=FALSE, coltime = 1, coldata = 2){
  #Legenda
  #vettore = vettore dal quale prendere i dati
  #lung = lunghezza del nuovo vettore
  #g = serie di date da inserire nel nuovo vettore
  #zero = se 0, rimuove pone a 0 valori minori di 0
  
  #vettore[,2]<-numeri(vettore[,2])
  if(zero){
    vettore[,2]<-pulisci_dati(vettore[,2])
  }
  
  vett_n<-data.frame(g)
  vett_n$data<-NA
  vett_n[which(vett_n[,1] %in% vettore[,coltime]),2]<-
    vettore[which(vettore[,coltime] %in% vett_n[,1]),coldata]
  
  #Old inefficient version, kept for the memories
  # j<-1
  # vettore<-vettore[which(vettore[,coltime] %in% vett_n[,1]),coldata]
  # for(i in 1:nrow(vettore)){
  #   repeat{
  #     if(vett_n[j,1] == vettore[i,coltime]){
  #       vett_n[j,2]<-vettore[i,coldata]
  #       break
  #     }else{
  #       j<-j+1
  #     }
  #   }
  # }
  return(vett_n)
}

time_index = function(dataframe){
  #Function to create a dates vector that can then be used to plot the data
  start <- dataframe$year[1]
  start<-paste0(start,"-01-01")
  end_year<-dataframe$year[length(dataframe$year)]
  end_year<-paste0(end_year,"-01-01")
  end_day<-dataframe$day[length(dataframe$day)]-1
  end<-as.Date(end_day, origin = end_year)
  out<-seq(as.Date(start), as.Date(end), by="days")
  out
}

voronoipolygons = function(x, corners=NULL) {
  if (.hasSlot(x, 'coords')) {
    crds <- x@coords  
  } else crds <- x
  op <- deldir(x=crds[,2], y= crds[,3], z=crds[,1], rw=corners)
  w <- tile.list(op)
  polys <- vector(mode='list', length=length(w))
  for (i in seq(along=polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP <- SpatialPolygons(polys)
  voronoi <- SpatialPolygonsDataFrame(SP, data=data.frame(x=op$summary$x,
             y=op$summary$y,z=op$summary$z, row.names=sapply(slot(SP, 'polygons'),
            function(x) slot(x, 'ID'))))
}