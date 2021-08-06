#Stations outliers rejection experiments and analysis
#In this script the qualitative and quantitative analysis on the outlier rejection
#are made in order to obtain:
# - Which one of the methods to use to remove the outliers (IQR or 3sd)
# - How many iterations to make in order to obtain a satisfactory clean dataset (n)


# Setup -------------------------------------------------------------------

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis")

source("Libraries.R")
source("Functions.R")
source("Functions_TG.R")
source("Functions_DP.R")


# Temperature INMET -------------------------------------------------------

path = "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/meteorological_stations_ceara/Meteorological_stations_FUNCEME/t2m_INMET"
temp_input = create_data_list(path, c("year","month","day","data"))

#Check and remove the stations which present negative values
n <- find_anomalous_stations(temp_input)
if(n > 0) temp_input <- remove_anomalous_stations(temp_input)

#Create a dataframe to plot the stations data to check anomalous behavior
date <- create_date_vector(2005, 2018)
temp_pos <- create_position_vector(temp_input[[1]])
temp_input[[2]] <- add_date_column(temp_input[[2]])
temp_df <- create_main_dataframe(date, temp_input[[2]], temp_pos)
plot_stations_df(temp_df, y = "°C", label = "INMET Temperature Stations")
#St168 and St590 have values ranging around 20°C for all the time series
#This seem an anomalous behaviour, let's check where these stations are in the region

#Export a shapefile of the stations positions to visualize the stations in QGIS
shape_temp <- temp_pos
coordinates(shape_temp)=~long+lat
proj4string(shape_temp)<- CRS("+proj=longlat +datum=WGS84")
path<-"C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/time_series_generation/prove_shape"
writeOGR(shape_temp,dsn = path, layer = "stations_temp_INMET", driver = "ESRI Shapefile")
#They are both in the same mountain area, really close to each other
#For this reason, they should not be held accountable, and removed from the dataframe

#Remove other anomalous stations from the dataframe
temp_df$St168<-NULL
temp_df$St590<-NULL

#Check the outliers and the two methods performances
check_outlier_IQR(temp_df)
check_outlier_3sd(temp_df)
#Comments:
#From the comparison on the two methods based on the visualization of the stations
#with the most visually identifiable outliers (St353, St640 and St868), the 3sd method
#presents more reliable results, so it's the one that will be used

#Remove the outliers
n <- 0 #Number of IQR iterations
temp_df <- remove_outlier_3sd(temp_df); n<-n+1
print(paste0("Number of iterations: ", n))
check_outlier_IQR(temp_df)
check_outlier_3sd(temp_df)
plot_stations_df(temp_df, y = "°C",
                 label = paste0("INMET Temperature Stations - After ", n, " 3sd"))
#Comments: after one iteration the results are already satisfying, since after it
#other outliers are found, another iteration is performed
print(n)
#n = 2

# Temperature FUNCEME -----------------------------------------------------

path = "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/meteorological_stations_ceara/Meteorological_stations_FUNCEME/t2m_FUNCEME"
temp_input = create_data_list(path, c("year","month","day","data"))

#Check and remove the stations which present negative values
n <- find_anomalous_stations(temp_input)
if(n > 0) temp_input <- remove_anomalous_stations(temp_input)

#Create a dataframe to plot the stations data and check anomalous behaviour
date <- create_date_vector(2005, 2018)
temp_pos <- create_position_vector(temp_input[[1]])
temp_input[[2]] <- add_date_column(temp_input[[2]])
temp_df <- create_main_dataframe(date, temp_input[[2]], temp_pos)
f<-2
l<-21
for(i in 1:round(ncol(temp_df)/20)){
  vis_temp <- data.frame(temp_df$date, temp_df[,f:l])
  names(vis_temp) <- c("date", names(temp_df[,f:l]))
  plot_stations_df(vis_temp, y = "°C", label = paste0("FUNCEME Temperature Stations: ", f-1, " to ", l-1))
  f<-f+19
  l<-l+19
  l<-ifelse(l > ncol(temp_df), ncol(temp_df), l)
  f<-ifelse(f > l, l-19, f)
}
#No substantial anomalies, but diffent stations present a high plateau which
#has to be removed

#Check the outliers and compare the two methods performances
f<-2
l<-21
for(i in 1:round(ncol(temp_df)/20)){
  print(paste0("IQR OUTLIER ANALYSIS BELOW - STATIONS ", f-1, "TO", l-1))
  check_outlier_IQR(data.frame(temp_df$date, temp_df[,f:l]))
  print(paste0("3SD OUTLIER ANALYSIS BELOW - STATIONS ", f-1, "TO", l-1))
  check_outlier_3sd(data.frame(temp_df$date, temp_df[,f:l]))
  f<-f+19
  l<-l+19
  l<-ifelse(l > ncol(temp_df), ncol(temp_df), l)
  f<-ifelse(f > l, l-19, f)
}
#Comments:
#Since IQR finds more outliers in critical stations which present anomalous behavior
#like St41 and St44, this method will be the one used below

#Remove the outliers
n <- 0 #Number of IQR iterations
temp_df <- remove_outlier_IQR(temp_df); n<-n+1
f<-2
l<-21
for(i in 1:round(ncol(temp_df)/20)){
  print(paste0("IQR OUTLIER ANALYSIS BELOW - STATIONS ", f-1, "TO", l-1))
  check_outlier_IQR(data.frame(temp_df$date, temp_df[,f:l]))
  vis_temp <- data.frame(temp_df$date, temp_df[,f:l])
  names(vis_temp) <- c("date", names(temp_df[,f:l]))
  plot_stations_df(vis_temp, y = "°C", label = paste0("FUNCEME Temperature Stations: ", f-1, " to ", l-1))
  f<-f+19
  l<-l+19
  l<-ifelse(l > ncol(temp_df), ncol(temp_df), l)
  f<-ifelse(f > l, l-19, f)
}
print(paste0("Number of iterations: ", n))
#Comments:
#After one iterations, most of the anomalies are removed. Some remains in St83.
#Even after a second iteration, St83 presents an anomalous behavior which is not detected by the IQR. The station is then removed from the dataframe
print(n)
#n = 2
temp_df$St83<-NULL

#Re-check everything
f<-2
l<-21
for(i in 1:round(ncol(temp_df)/20)){
  vis_temp <- data.frame(temp_df$date, temp_df[,f:l])
  names(vis_temp) <- c("date", names(temp_df[,f:l]))
  plot_stations_df(vis_temp, y = "°C", label = paste0("FUNCEME Temperature Stations: ", f-1, " to ", l-1))
  f<-f+l-1
  l<-l+l-1
  l<-ifelse(l > ncol(temp_df), ncol(temp_df), l)
  f<-ifelse(f > l, l-19, f)
}
#Now some anomalies are visible in stations St34, St36 and St37
#Export a shapefile of the stations positions to visualize them in QGIS
shape_temp <- temp_pos
coordinates(shape_temp)=~long+lat
proj4string(shape_temp)<- CRS("+proj=longlat +datum=WGS84")
path<-"C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/time_series_generation/prove_shape"
writeOGR(shape_temp,dsn = path, layer = "stations_temp_FUNCEME", driver = "ESRI Shapefile")
#St34 is in a mountain area, the other two stations are nearby but not in a mountain
#area. Since they are distant from the study area, they are all removed from the 
#dataframe
temp_df$St34<-NULL; temp_df$St36<-NULL; temp_df$St37<-NULL


# Humidity INMET ----------------------------------------------------------

path = "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/meteorological_stations_ceara/Meteorological_stations_FUNCEME/humidity_subset/INMET"
hum_input = create_data_list(path, c("year","month","day","data"))

#Check and remove the stations which present negative values
n <- find_anomalous_stations(hum_input)
if(n > 0) hum_input <- remove_anomalous_stations(hum_input)
#No stations to be removed

#Plot the stations data
hum_input[[2]] <- add_date_column(hum_input[[2]])
plot_stations(hum_input, 4, 5, label = "INMET Humidity Stations")

#Check the outliers and the two methods performances
check_outlier_stations(hum_input, plott = TRUE)
#From the visualization, the IQR method finds some outliers for station 7,
#while the 3sd doesn't find any
#Let's proceed to remove the outliers with the IQR method
IQR = TRUE; sd3 = FALSE

#Remove the outliers
n <- 0 #Number of IQR iterations
hum_input <- remove_outlier_list(hum_input, IQR = TRUE, sd3 = FALSE); n<-n+1
print(paste0("Number of iterations: ", n))
check_outlier_stations(hum_input, sd3 = FALSE, plott = FALSE)
check_outlier_stations(hum_input, IQR = FALSE, plott = FALSE)
plot_stations(hum_input, 4, 5, label = paste0("Humidity INMET - After ", n," IQR"))
#After the first iteration, no outliers are found anymore, so one iteration is enough
print(n)

# Humidity FUNCEME ----------------------------------------------------------

path = "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/meteorological_stations_ceara/Meteorological_stations_FUNCEME/humidity_subset/FUNCEME"
hum_input = create_data_list(path, c("year","month","day","data"))

#Check and remove the stations which present negative values
n <- find_anomalous_stations(hum_input)
if(n > 0) hum_input <- remove_anomalous_stations(hum_input)
#No stations to be removed

#Create a dataframe to plot the stations data and check anomalous behaviour
date <- create_date_vector(2005, 2018)
hum_pos <- create_position_vector(hum_input[[1]])
hum_input[[2]] <- add_date_column(hum_input[[2]])
hum_df <- create_main_dataframe(date, hum_input[[2]], hum_pos)
f<-2
l<-21
for(i in 1:round(ncol(hum_df)/20)){
  vis_temp <- data.frame(hum_df$date, hum_df[,f:l])
  names(vis_temp) <- c("date", names(hum_df[,f:l]))
  plot_stations_df(vis_temp, y = "%", label = paste0("FUNCEME Humidity Stations: ", f-1, " to ", l-1))
  f<-f+19
  l<-l+19
  l<-ifelse(l > ncol(hum_df), ncol(hum_df), l)
  f<-ifelse(f > l, l-19, f)
}
#Comments:
#There are some anomalies, with stations registering low values around 2015
#Let's check if these values are outliers or not

#Check the outliers and compare the two methods performances
f<-2
l<-21
for(i in 1:round(ncol(hum_df)/20)){
  print(paste0("IQR OUTLIER ANALYSIS BELOW - STATIONS ", f-1, "TO", l-1))
  check_outlier_IQR(data.frame(hum_df$date, hum_df[,f:l]))
  print(paste0("3SD OUTLIER ANALYSIS BELOW - STATIONS ", f-1, "TO", l-1))
  check_outlier_3sd(data.frame(hum_df$date, hum_df[,f:l]))
  f<-f+19
  l<-l+19
  l<-ifelse(l > ncol(hum_df), ncol(hum_df), l)
  f<-ifelse(f > l, l-19, f)
}
#Comments:
#3sd seems to have a more stable approach throughout the stations,
#so it will be the one used below

#Remove the outliers
n <- 0 #Number of 3sd iterations
hum_df <- remove_outlier_3sd(hum_df); n<-n+1
f<-2
l<-21
for(i in 1:round(ncol(hum_df)/20)){
  print(paste0("3sd OUTLIER ANALYSIS BELOW - STATIONS ", f-1, " TO ", l-1))
  check_outlier_3sd(data.frame(hum_df$date, hum_df[,f:l]))
  vis_temp <- data.frame(hum_df$date, hum_df[,f:l])
  names(vis_temp) <- c("date", names(hum_df[,f:l]))
  plot_stations_df(vis_temp, y = "%", label = paste0("FUNCEME Humidity Stations: ", f-1, " to ", l-1))
  f<-f+19
  l<-l+19
  l<-ifelse(l > ncol(hum_df), ncol(hum_df), l)
  f<-ifelse(f > l, l-19, f)
}
print(paste0("Number of iterations: ", n))
#Comments:
#After two iterations, all anomalies are removed, apart from St19 and St44, and St81
#which present a similar behavior around 2015, and St55 which present an anomalous
#behavior in 2018. The outlier analysis doesn't identify these
#anomalies as outliers, so the stations will be removed after checking their location in QGIS
#Another iteration is done to remove the last outliers identified
print(n)
#n = 3

#Check the stations that have to be removed
f<-2; l<-20 #St19 is in the 1-20 range
f<-21; l<-40 #St44 is in the 20-40 range
f<-60; l<-80 #St81 is in the 60-80 range
f<-40; l<-60 #St55 is in the 40-60 range

vis_temp <- data.frame(hum_df$date, hum_df[,f:l])
names(vis_temp) <- c("date", names(hum_df[,f:l]))
plot_stations_df(vis_temp, y = "%", label = paste0("FUNCEME Humidity Stations: ", f-1, " to ", l-1),
                 interactive = TRUE, file = "humidity_FUNCEME_59_77")
#Export a shapefile of the stations positions to visualize them in QGIS
shape_hum <- hum_pos
coordinates(shape_hum)=~long+lat
proj4string(shape_hum)<- CRS("+proj=longlat +datum=WGS84")
path<-"C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/time_series_generation/prove_shape"
writeOGR(shape_hum,dsn = path, layer = "stations_hum_FUNCEME", driver = "ESRI Shapefile")
#St19 is close to St20 and St4701. For these two stations the anomalous data
#around 2015 has been eliminated by the outlier rejection. St19 will then be eliminated
#from the dataset
#St44 is close to St4701 and St84. These two stations don't present the same behavior.
#St44 will then be eliminated from the dataset
#St55 is close to St64, which doesn't present the same behavior. St55 will then be
#removed from the dataset
hum_df$St19 <- NULL; hum_df$St44 <- NULL; hum_df$St55 <- NULL; hum_df$St81 <- NULL


# Radiation INMET ---------------------------------------------------------

#Load the stations
path = "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/meteorological_stations_ceara/Meteorological_stations_FUNCEME/radiation_KJ_subset/INMET"
rad_input = create_data_list(path, c("year","month","day","data"))

#Check and remove the stations which present negative values
n <- find_anomalous_stations(rad_input)
if(n > 0) rad_input <- remove_anomalous_stations(rad_input)

#Plot the stations data
rad_input[[2]] <- add_date_column(rad_input[[2]])
plot_stations(rad_input, 4, 5, label = "INMET Radiation Stations")

#Check the outliers and the two methods performances
check_outlier_stations(rad_input, plott = TRUE)
#From the visualization, the IQR method seems to perform well, so it will be used
#to remove the outliers
IQR = TRUE; sd3 = FALSE

#Remove the outliers
n <- 0 #Number of IQR iterations
rad_input <- remove_outlier_list(rad_input, IQR = TRUE, sd3 = FALSE); n<-n+1
print(paste0("Number of iterations: ", n))
check_outlier_stations(rad_input, sd3 = FALSE, plott = FALSE)
check_outlier_stations(rad_input, IQR = FALSE, plott = FALSE)
plot_stations(rad_input, 4, 5, label = paste0("Radiation - After ", n," IQR"))
#The results are satisfactory after 3 iterations
#Some outliers remain while checking with IQR, but the 3sd doesn't find anything,
#so 3 iterations are satisfactory
print(n)







