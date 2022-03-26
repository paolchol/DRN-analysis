#Time series generation script

# Setup ---------------------------------------------------------------

setwd("C:/Directory_thesis_codes")
path_export <- "./Data/Generated/WASA_time_series"

source("./R/Libraries/Libraries.R")
source("./R/Libraries/Functions.R")
source("./R/Libraries/Functions_TG.R")
source("./R/Libraries/Functions_DP.R")

#Load the subbasins' shapefile
path <- "./Data/Shapefile"
subbasins <- readOGR(paste0(path, "/subbasins_cut_geomfix.shp"))

# Precipitation FUNCEME ----------------------------------------------------

#Load the stations
path <- "./Data/Meteorological_data/precipitation_subset_FUNCEME"
prec_input <- create_data_list(path, c("year", "month", "day", "data"))

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

tic("Precipitation FUNCEME - Creation of the WASA input file")
output_prec <- create_time_series_Wmean(main_dataframe = prec_df,
                                        positions = prec_pos,
                                        layer = subbasins)
h = "Daily average precipitation [mmd] for each subasin, ordered according Map-IDs\nDate No. of days Subasin-ID\n0 0 123 125 126 127 134 137 138 139 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160"
output_prec <- WASA_input_format(prec_df, output_prec, header = h, path = path_export, name = "precipitation_FUNCEME_2006")
write.table(output_prec,paste0(path_export,"/output_prec_FUNCEME_save_2006.txt"),quote = FALSE,sep="\t",row.names = FALSE)
toc() #2h 55 minutes

# Temperature INMET -------------------------------------------------------

path <- "./Data/Meteorological_data/t2m_INMET"
temp_input <- create_data_list(path, c("year", "month", "day", "data"))

#Remove anomalous stations (more information: "stations_outlier_removal.R")
n <- find_anomalous_stations(temp_input)
if(n > 0) temp_input <- remove_anomalous_stations(temp_input)
date <- create_date_vector(2005, 2018)
temp_pos <- create_position_vector(temp_input[[1]])
temp_input[[2]] <- add_date_column(temp_input[[2]])
temp_df <- create_main_dataframe(date, temp_input[[2]], temp_pos)
#Remove other anomalous stations from the dataframe
# (more information: "stations_outlier_removal.R")
temp_df$St168 <- NULL; temp_df$St590 <- NULL
#Remove outliers (more information: "stations_outlier_removal.R")
num_iter <- 2
for(n in 1:num_iter)  temp_df <- remove_outlier_3sd(temp_df)

tic("Temperature INMET - Interpolation")
output_temp <- create_time_series_Wmean(main_dataframe = temp_df,
                                        positions = temp_pos,
                                        layer = subbasins)
h <- "Daily average temperature (in degree Celcius) for each subasin, ordered according Map-IDs\nDate No. of days Subasin-ID\n0 0 123 125 126 127 134 137 138 139 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160" # nolint
output_temp <- WASA_input_format(temp_df, output_temp, header = h, path = path_export, name = "temperature_INMET_2006")
write.table(output_temp, paste0(path_export, "/output_temp_INMET_save_2006.txt"),
            quote = FALSE, sep = "\t", row.names = FALSE)
toc() #19 minutes

# Temperature FUNCEME -----------------------------------------------------

path <- "./Data/Meteorological_data/t2m_FUNCEME"
temp_input <- create_data_list(path, c("year", "month", "day", "data"))

#Remove anomalous stations (more information: "stations_outlier_removal.R")
n <- find_anomalous_stations(temp_input)
if(n > 0) temp_input <- remove_anomalous_stations(temp_input)
date <- create_date_vector(2005, 2018)
temp_pos <- create_position_vector(temp_input[[1]])
temp_input[[2]] <- add_date_column(temp_input[[2]])
temp_df <- create_main_dataframe(date, temp_input[[2]], temp_pos)
#Remove other anomalous stations from the dataframe (more information: "stations_outlier_removal.R")
temp_df$St83<-NULL; temp_df$St34<-NULL; temp_df$St36<-NULL; temp_df$St37<-NULL
#Remove outliers (more information: "stations_outlier_removal.R")
num_iter <- 2
for(n in 1:num_iter)  temp_df <- remove_outlier_IQR(temp_df)

tic("Temperature FUNCEME - Interpolation")
output_temp <- create_time_series_Wmean(main_dataframe = temp_df,
                                        positions = temp_pos,
                                        layer = subbasins)
h = "Daily average temperature (in degree Celcius) for each subasin, ordered according Map-IDs\nDate No. of days Subasin-ID\n0 0 123 125 126 127 134 137 138 139 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160"
output_temp <- WASA_input_format(temp_df, output_temp, header = h, path = path_export, name = "temperature_FUNCEME_2006")
write.table(output_temp,paste0(path_export,"/output_temp_FUNCEME_save_2006.txt"),quote = FALSE,sep="\t",row.names = FALSE)
toc() #23 minutes

# Humidity INMET ----------------------------------------------------------

path <- "./Data/Meteorological_data/humidity_subset_INMET"
hum_input <- create_data_list(path, c("year", "month", "day", "data"))

#Remove anomalous stations and outliers
# (more information: "stations_outlier_removal.R")
n <- find_anomalous_stations(hum_input)
if(n > 0) hum_input <- remove_anomalous_stations(hum_input)
num_iter <- 1
for(n in 1:num_iter)  hum_input <- remove_outlier_list(hum_input, IQR = TRUE, sd3 = FALSE)

tic("Humidity - INMET - Interpolation")
positions_hum <- create_position_vector(hum_input[[1]])
hum_input[[2]] <- add_date_column(hum_input[[2]])
date <- create_date_vector(2005, 2018)
main_dataframe_hum <- create_main_dataframe(date, hum_input[[2]], positions_hum)
output_hum <- create_time_series_Wmean(main_dataframe = main_dataframe_hum,
                                       positions = positions_hum,
                                       layer = subbasins)
h = "Daily average humidity [in %] for each subasin, ordered according Map-IDs\nDate No. of days Subasin-ID\n0 0 123 125 126 127 134 137 138 139 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160"
output_hum <- WASA_input_format(main_dataframe_hum, output_hum, header = h, path = path_export, name = "hum_INMET_2006")
write.table(output_hum,paste0(path_export,"/output_hum_INMET_save_2006.txt"),quote = FALSE,sep="\t",row.names = FALSE)
toc() #14 minutes

# Humidity FUNCEME --------------------------------------------------------

path <- "./Data/Meteorological_data/humidity_subset_FUNCEME"
hum_input <- create_data_list(path, c("year", "month", "day", "data"))

#Remove anomalous stations and outliers
# (more information: "stations_outlier_removal.R")
n <- find_anomalous_stations(hum_input)
if(n > 0) hum_input <- remove_anomalous_stations(hum_input)
date <- create_date_vector(2005, 2018)
hum_pos <- create_position_vector(hum_input[[1]])
hum_input[[2]] <- add_date_column(hum_input[[2]])
hum_df <- create_main_dataframe(date, hum_input[[2]], hum_pos)
#Remove other anomalous stations from the dataframe
# (more information: "stations_outlier_removal.R")
hum_df$St19 <- NULL; hum_df$St44 <- NULL; hum_df$St55 <- NULL; hum_df$St81 <- NULL
#Remove outliers (more information: "stations_outlier_removal.R")
num_iter <- 3
for(n in 1:num_iter)  hum_df <- remove_outlier_3sd(hum_df)

tic("Humidity - FUNCEME - Interpolation")
output_hum <- create_time_series_Wmean(main_dataframe = hum_df,
                                       positions = hum_pos,
                                       layer = subbasins)
h = "Daily average humidity [in %] for each subasin, ordered according Map-IDs\nDate No. of days Subasin-ID\n0 0 123 125 126 127 134 137 138 139 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160"
output_hum <- WASA_input_format(hum_df, output_hum, header = h, path = path_export, name = "hum_FUNCEME_2006")
write.table(output_hum,paste0(path_export,"/output_hum_FUNCEME_save_2006.txt"),quote = FALSE,sep="\t",row.names = FALSE)
toc() #28 minutes

# Radiation INMET ---------------------------------------------------------

#Load the stations
path <- "./Data/Meteorological_data/radiation_KJ_subset_INMET"
rad_input <- create_data_list(path, c("year", "month", "day", "data"))

#Remove anomalous stations and outliers
# (more information: "stations_outlier_removal.R")
n <- find_anomalous_stations(rad_input)
if(n > 0) rad_input <- remove_anomalous_stations(rad_input)
num_iter <- 3
for(n in 1:num_iter)  rad_input <- remove_outlier_list(rad_input, IQR = TRUE, sd3 = FALSE)

tic("Radiation INMET - Interpolation")
positions_rad <- create_position_vector(rad_input[[1]])
rad_input[[2]] <- add_date_column(rad_input[[2]])
date <- create_date_vector(2005, 2018)
main_dataframe_rad <- create_main_dataframe(date, rad_input[[2]], positions_rad)
output_rad <- create_time_series_Wmean(main_dataframe = main_dataframe_rad,
                                       positions = positions_rad,
                                       layer = subbasins)
#Change the measuring unit from KJ/(m2*h) to W/(m2*d)
output_rad[, 3:ncol(output_rad)]<-output_rad[, 3:ncol(output_rad)]*1000/(24*3600)
h = "Daily average shortwave radiation [in Wm2] for each subasin, ordered according Map-IDs\nDate No. of days Subasin-ID\n0 0 123 125 126 127 134 137 138 139 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160"
output_rad <- WASA_input_format(main_dataframe_rad, output_rad, header = h, path = path_export, name = "radiation_INMET_2006")
write.table(output_rad,paste0(path_export,"/output_rad_INMET_save_2006.txt"),quote = FALSE,sep="\t",row.names = FALSE)
toc() #13 minutes
