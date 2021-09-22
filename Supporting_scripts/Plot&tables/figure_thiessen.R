#Plot the Thiessen polygons method

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")

source("./Libraries/Libraries.R")
source("./Libraries/Functions.R")
source("./Libraries/Functions_TG.R")
source("./Libraries/Functions_DP.R")

subbasins<-readOGR("./Data/Shapefile/Subbasins_geomfix.shp")

#Load the stations
path = "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/meteorological_stations_ceara/Meteorological_stations_FUNCEME/precipitation_subset"
prec_input = create_data_list(path, c("year","month","day","data"))

#Remove anomalous stations (more information: "prec_out_rem.R")
n <- find_anomalous_stations(prec_input)
if(n > 0) prec_input <- remove_anomalous_stations(prec_input)
date <- create_date_vector(1980, 2018)
prec_pos <- create_position_vector(prec_input[[1]])
prec_input[[2]] <- add_date_column(prec_input[[2]])
prec_df <- create_main_dataframe(date, prec_input[[2]], prec_pos)
#Other anomalous stations removal (more information: "prec_out_rem.R")
remove_st <- anomalous_prec_st(prec_df, 250)
prec_df <- prec_df[-remove_st]
#Outlier removal (more information: "prec_out_rem.R")
prec_df <- rejection_threshold(prec_df, method = "value", 250)

main_dataframe <- prec_df
layer <- subbasins
positions <- prec_pos

require(deldir)
require(sp)

j <- 1

corners<-c(-41.64, -37.26, -7.85, -2.78) #(xmin, xmax, ymin, ymax)

i <- 7306 #1st January 2000

first<-main_dataframe[i,which(!is.na(main_dataframe[i,]))]


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


coordinates(shape) = ~long+lat

writeOGR(thiessen, dsn = "./Data/Plot_table", layer = "thiessen_plot", driver = "ESRI Shapefile")
writeOGR(shape, dsn = "./Data/Plot_table", layer = "stations", driver = "ESRI Shapefile")



