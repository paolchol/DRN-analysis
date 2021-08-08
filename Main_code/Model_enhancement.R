#Model enhancement #1
#Increase the observation period, adding the observations for 

# Setup -------------------------------------------------------------------

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")
path_export <- "./Inputs/Model_input/Model_enhancement/Time_series_update"

source("./Libraries/Libraries.R")
source("./Libraries/Functions.R")
source("./Libraries/Functions_TG.R")
source("./Libraries/Functions_DP.R")
source("./Libraries/Functions_CO.R")
source("./Libraries/Functions_MC.R")

#Load the subbasins' shapefile
path<-"C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/shapefile/Banabuiu"
subbasins<-readOGR(paste0(path,"/subbasins_cut_geomfix.shp"))

# Temperature -------------------------------------------------------------

path = "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/FUNCEME_update/inmet_temperatura_do_ar_a_2m_ceara"
temp_input = create_data_list(path, c("date","value"), s = ',')

tic("Temperature - 2018-2020 - Interpolation")
positions_temp <- create_position_vector(temp_input[[1]])
temp_input[[2]] <- add_date_column(temp_input[[2]], complete = TRUE)
date <- create_date_vector(2019, 2020)
main_dataframe_temp <- create_main_dataframe(date, temp_input[[2]], positions_temp,
                                            timecol = 1, datacol = 2)
output_temp <- create_time_series_Wmean(main_dataframe = main_dataframe_temp,
                                       positions = positions_temp,
                                       layer = subbasins)
h = "Daily average temperature (in degree Celcius) for each subasin, ordered according Map-IDs\nDate No. of days Subasin-ID\n0 0 123 125 126 127 134 137 138 139 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160"
output_temp <- WASA_input_format(main_dataframe_temp, output_temp, header = h, path = path_export, name = "temperature_20192020")
save(output_temp, file = paste0(path_export, "/temperature_20192020.RData"))
toc()

# Humidity ----------------------------------------------------------------

path = "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/FUNCEME_update/inmet_relative_humidity_ceara"
hum_input = create_data_list(path, c("date","value"), s = ',')

tic("Humidity - 2018-2020 - Interpolation")
positions_hum <- create_position_vector(hum_input[[1]])
hum_input[[2]] <- add_date_column(hum_input[[2]], complete = TRUE)
date <- create_date_vector(2019, 2020)
main_dataframe_hum <- create_main_dataframe(date, hum_input[[2]], positions_hum,
                                            timecol = 1, datacol = 2)
#Anomalous behavior for St116
main_dataframe_hum$St116[main_dataframe_hum$date <= as.Date("2018-10-23")] <- NA 

output_hum <- create_time_series_Wmean(main_dataframe = main_dataframe_hum,
                                       positions = positions_hum,
                                       layer = subbasins)
h = "Daily average humidity [in %] for each subasin, ordered according Map-IDs\nDate No. of days Subasin-ID\n0 0 123 125 126 127 134 137 138 139 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160"
output_hum <- WASA_input_format(main_dataframe_hum, output_hum, header = h, path = path_export, name = "humidity_20192020")
save(output_hum, file = paste0(path_export, "/humidity_20192020.RData"))
toc() #3 minutes

# Radiation ---------------------------------------------------------------
#Radiation data had missing data from June 2018, so here I will extract
#2018 as well, and then replace it in the actual file

path = "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/FUNCEME_update/inmet_downwelling_shortwave_radiation_ceara"
rad_input = create_data_list(path, c("date","value"), s = ',')

tic("Radiation - 2018-2020 - Interpolation")
positions_rad <- create_position_vector(rad_input[[1]])
rad_input[[2]] <- add_date_column(rad_input[[2]], complete = TRUE)
date <- create_date_vector(2018, 2020)
main_dataframe_rad <- create_main_dataframe(date, rad_input[[2]], positions_rad,
                                            timecol = 1, datacol = 2)
output_rad <- create_time_series_Wmean(main_dataframe = main_dataframe_rad,
                                       positions = positions_rad,
                                       layer = subbasins)
h = "Daily average shortwave radiation [in Wm2] for each subasin, ordered according Map-IDs\nDate No. of days Subasin-ID\n0 0 123 125 126 127 134 137 138 139 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160"
output_rad <- WASA_input_format(main_dataframe_rad, output_rad, header = h, path = path_export, name = "radiation_20182020")
save(output_rad, file = paste0(path_export, "/radiation_20182020.RData"))
toc() #3 minutes


# Precipitation ------------------------------------------------------------

tic('Precipitation')
#Load the data
path = "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/FUNCEME_update"
prec2019 <- read.table(paste0(path, "/pr_daily_funceme_20190101_20191231.asc"))
prec2020 <- read.table(paste0(path, "/pr_daily_funceme_20200101_20201231.asc"))

prec2019 <- data.frame(t(prec2019))
prec2020 <- data.frame(t(prec2020))

#2019
positions_prec2019 <- data.frame(ID = names(prec2019), name = '-', long = 0, lat = 0)
positions_prec2019$long <- t(prec2019[2, ])
positions_prec2019$lat <- t(prec2019[3, ])
prec2019 <- prec2019[-c(1, 2, 3), ]
prec2019[prec2019 == -999] <- NA

prec2019$date <- create_date_vector(2019, 2019)
prec2019 <- prec2019 %>% relocate(date, .before = X1)
output_prec2019 <- create_time_series_Wmean(main_dataframe = prec2019,
                                            positions = positions_prec2019,
                                            layer = subbasins)

#2020
positions_prec2020 <- data.frame(ID = names(prec2020), name = '-', long = 0, lat = 0)
positions_prec2020$long <- t(prec2020[2, ])
positions_prec2020$lat <- t(prec2020[3, ])
prec2020 <- prec2020[-c(1, 2, 3), ]
prec2020[prec2020 == -999] <- NA

prec2020$date <- create_date_vector(2020, 2020)
prec2020 <- prec2020 %>% relocate(date, .before = X1)
output_prec2020 <- create_time_series_Wmean(main_dataframe = prec2020,
                                            positions = positions_prec2020,
                                            layer = subbasins)

#Join them
output_prec <- rbind(output_prec2019, output_prec2020)
output_prec$date <- create_date_vector(2019, 2020)

#Save
h = "Daily average precipitation [mmd] for each subasin, ordered according Map-IDs\nDate No. of days Subasin-ID\n0 0 123 125 126 127 134 137 138 139 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160"
output_prec <- WASA_input_format(output_prec, output_prec, header = h, path = path_export, name = "prec_20192020")
save(output_rad, file = paste0(path_export, "/prec_20192020.RData"))
toc()

# Join the new with the actual dataset ----------------------------------------

#Actual dataset
prec <- read.table("./Inputs/Model_input/Base/Time_series/rain_daily.dat", skip = 3)
temp <- read.table("./Inputs/Model_input/Base/Time_series/temperature.dat", skip = 3)
hum <- read.table("./Inputs/Model_input/Base/Time_series/humidity.dat", skip = 3)
rad <- read.table("./Inputs/Model_input/Base/Time_series/radiation.dat", skip = 3)

columns <- names(output_prec)
#c("date", "doy", "ID123", "ID125", "ID126", "ID127", "ID134", "ID137", "ID138", "ID139", "ID142", "ID143", "ID144", "ID145", "ID146", "ID147", "ID148", "ID149", "ID150", "ID151", "ID152", "ID153", "ID154", "ID155", "ID156", "ID157", "ID158", "ID159", "ID160")
names(prec) <- columns
names(temp) <- columns
names(hum) <- columns
names(rad) <- columns

#New
load(paste0(path_export, "/temperature_20192020.RData"))
load(paste0(path_export, "/humidity_20192020.RData"))
load(paste0(path_export, "/radiation_20182020.RData"))
load(paste0(path_export, "/prec_20192020.RData"))

#Join the datasets
prec_f <- rbind(prec, output_prec)
temp_f <- rbind(temp, output_temp)
hum_f <- rbind(hum, output_hum)

replace_d <- create_date_vector(2018, 2018)
rad$date <- change_date_WASA_input(rad$date)
output_rad$date <- change_date_WASA_input(output_rad$date)
rad[rad$date %in% replace_d, ] <- output_rad[output_rad$date %in% replace_d, ]
output_rad <- output_rad[-which(output_rad$date %in% replace_d), ]
rad_f <- rbind(rad, output_rad)

#Save the datasets
date_df <- data.frame(date = create_date_vector(1980, 2020))

h = "Daily average precipitation [mmd] for each subasin, ordered according Map-IDs\nDate No. of days Subasin-ID\n0 0 123 125 126 127 134 137 138 139 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160"
prec_f <- WASA_input_format(date_df, prec_f, header = h, path = path_export, name = "rain_daily")

h = "Daily average temperature (in degree Celcius) for each subasin, ordered according Map-IDs\nDate No. of days Subasin-ID\n0 0 123 125 126 127 134 137 138 139 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160"
temp_f <- WASA_input_format(date_df, temp_f, header = h, path = path_export, name = "temperature")

h = "Daily average humidity [in %] for each subasin, ordered according Map-IDs\nDate No. of days Subasin-ID\n0 0 123 125 126 127 134 137 138 139 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160"
hum_f <- WASA_input_format(date_df, hum_f, header = h, path = path_export, name = "humidity")

h = "Daily average shortwave radiation [in Wm2] for each subasin, ordered according Map-IDs\nDate No. of days Subasin-ID\n0 0 123 125 126 127 134 137 138 139 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160"
rad_f <- WASA_input_format(date_df, rad_f, header = h, path = path_export, name = "radiation")




