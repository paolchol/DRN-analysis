#Script to visualize the generated time series and then compare them with
#Alexandre's ones


# Setup -------------------------------------------------------------------

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis")
path_input_gen <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/time_series_generation/input_WASA_gen/gen_2006"
path_input_alex <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/time_series_generation/input_WASA_gen/Alex_Time_series"
columns <- c("date", "doy", "ID123", "ID125", "ID126", "ID127", "ID134", "ID137", "ID138", "ID139", "ID142", "ID143", "ID144", "ID145", "ID146", "ID147", "ID148", "ID149", "ID150", "ID151", "ID152", "ID153", "ID154", "ID155", "ID156", "ID157", "ID158", "ID159", "ID160")

source("Libraries.R")
source("Functions.R")
source("Functions_TG.R")
source("Functions_DP.R")
source("Functions_CO.R")

# Load the generated time series ------------------------------------------

prec_gen_F <- read.table(paste0(path_input_gen, "/precipitation_FUNCEME_2006.dat"), skip = 3); names(prec_gen_F) <- columns
temp_gen_F <- read.table(paste0(path_input_gen, "/temperature_FUNCEME_2006.dat"), skip = 3); names(temp_gen_F) <- columns
temp_gen_I <- read.table(paste0(path_input_gen, "/temperature_INMET_2006.dat"), skip = 3); names(temp_gen_I) <- columns
hum_gen_F <- read.table(paste0(path_input_gen, "/hum_FUNCEME_2006.dat"), skip = 3); names(hum_gen_F) <- columns
hum_gen_I <- read.table(paste0(path_input_gen, "/hum_INMET_2006.dat"), skip = 3); names(hum_gen_I) <- columns
rad_gen_I <- read.table(paste0(path_input_gen, "/radiation_INMET_2006.dat"), skip = 3); names(rad_gen_I) <- columns

prec_gen_F$date <- change_date_WASA_input(prec_gen_F$date)
temp_gen_F$date <- change_date_WASA_input(temp_gen_F$date)
temp_gen_I$date <- change_date_WASA_input(temp_gen_I$date)
hum_gen_F$date <- change_date_WASA_input(hum_gen_F$date)
hum_gen_I$date <- change_date_WASA_input(hum_gen_I$date)
rad_gen_I$date <- change_date_WASA_input(rad_gen_I$date)

# Visualize them ----------------------------------------------------------

path_plot <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/plots/plot_gen_2006"

plot_subbasins_df(prec_gen_F, y = "mm", label = "Precipitation FUNCEME - All subbasins",
                  save = TRUE, file = "prec_F", path = path_plot)
plot_subbasins_df(temp_gen_F, y = "°C", label = "Temperature FUNCEME - All subbasins",
                  save = TRUE, file = "temp_F", path = path_plot)
plot_subbasins_df(temp_gen_I, y = "°C", label = "Temperature INMET - All subbasins",
                  save = TRUE, file = "temp_I", path = path_plot)
plot_subbasins_df(hum_gen_F, y = "%", label = "Humidity FUNCEME - All subbasins",
                  save = TRUE, file = "hum_F", path = path_plot)
plot_subbasins_df(hum_gen_I, y = "%", label = "Humidity INMET - All subbasins",
                  save = TRUE, file = "hum_I", path = path_plot)
plot_subbasins_df(rad_gen_I, y = "W/m2", label = "Radiation INMET - All subbasins",
                  save = TRUE, file = "rad_I", path = path_plot)

# Fill the NAs ------------------------------------------------------------

#Check if NAs are present in the datasets
count_na(prec_gen_F)
count_na(temp_gen_F)
count_na(temp_gen_I)
count_na(hum_gen_F)
count_na(hum_gen_I)
count_na(rad_gen_I)
#temp_gen_F, hum_gen_F, hum_gen_I and rad_gen_I present missing data

#Fill the NAs in the datasets which present them
temp_gen_F <- fill_na(temp_gen_F, skipcol = 2)
hum_gen_F <- fill_na(hum_gen_F, skipcol = 2)
hum_gen_I <- fill_na(hum_gen_I, skipcol = 2)

#rad_gen_I data end in June 2018, thus creating a hole at the end of the time
#series. The NA filling method creates a unique value for the whole hole,
#which is not ideal (results and plot in "NAs_filling_experiments.R")
#To fill this hole, waiting for the updated radiation data from FUNCEME, the mean
#of the same days in the previous years is taken and placed instead of the missing
#value. This approach can be justified since radiation data does not present a high
#inter-annual variability
rad_gen_I <- mean_previous_years(rad_gen_I)
plot_subbasins_df(rad_gen_I, y = "W/m2", label = "Radiation INMET - Last six months extrapolated",
                  save = TRUE, file = "rad_I_extra", path = path_plot)
#The result is smoothed as expected with respect to the previous years
#For now this data can be used, and then improved when the new data will be available

# Compare the generated time series with Alexandre's ones -----------------

#Load Alexandre's time series
prec_alex <- read.table(paste0(path_input_alex, "/rain_daily.dat"), skip = 3); names(prec_alex) <- columns
temp_alex <- read.table(paste0(path_input_alex, "/temperature.dat"), skip = 3); names(temp_alex) <- columns
hum_alex <- read.table(paste0(path_input_alex, "/humidity.dat"), skip = 3); names(hum_alex) <- columns
rad_alex <- read.table(paste0(path_input_alex, "/radiation.dat"), skip = 3); names(rad_alex) <- columns

time_alex <- seq(as.Date("1980-01-01"), as.Date("2016-12-31"), by = "days")

prec_alex$date <- time_alex
temp_alex$date <- time_alex
hum_alex$date <- time_alex
rad_alex$date <- time_alex

#Statistical comparison
#Over a common window of 5 years (2005 - 2010)

report_prec_F <- compare_TS_stat(prec_gen_F, prec_alex)
report_temp_F <- compare_TS_stat(temp_gen_F, temp_alex)
report_temp_I <- compare_TS_stat(temp_gen_I, temp_alex)
report_hum_F <- compare_TS_stat(hum_gen_F, hum_alex)
report_hum_I <- compare_TS_stat(hum_gen_I, hum_alex)
report_rad_I <- compare_TS_stat(rad_gen_I, rad_alex)

mean(report_rad_I$NRMSE)

#Save them
path_report <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/time_series_generation/input_WASA_gen/report_comparison"
write.csv(report_prec_F, paste0(path_report,"/report_prec_FUNCEME.csv"), row.names =  FALSE, quote = FALSE)
write.csv(report_temp_F, paste0(path_report,"/report_temp_FUNCEME.csv"), row.names =  FALSE, quote = FALSE)
write.csv(report_hum_F, paste0(path_report,"/report_hum_FUNCEME.csv"), row.names =  FALSE, quote = FALSE)
write.csv(report_rad_I, paste0(path_report,"/report_rad_INMET.csv"), row.names =  FALSE, quote = FALSE)

#Visual comparison
#Overall

path_plot <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/plots/comparisons/precipitation"
compare_TS_vis(prec_gen_F, prec_alex, y = "mm", label = "Precipitation - FUNCEME",
               save = TRUE, file = "prec_F", path = path_plot)
path_plot <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/plots/comparisons/temperature"
compare_TS_vis(temp_gen_F, temp_alex, y = "°C", label = "Temperature - FUNCEME",
               save = TRUE, file = "temp_F", path = path_plot)
path_plot <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/plots/comparisons/temperature"
compare_TS_vis(temp_gen_I, temp_alex, y = "°C", label = "Temperature - INMET",
               save = TRUE, file = "temp_I", path = path_plot)
path_plot <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/plots/comparisons/humidity"
compare_TS_vis(hum_gen_F, hum_alex, y = "%", label = "Humidity - FUNCEME",
               save = TRUE, file = "hum_F", path = path_plot)
path_plot <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/plots/comparisons/humidity"
compare_TS_vis(hum_gen_I, hum_alex, y = "%", label = "Humidity - INMET",
               save = TRUE, file = "hum_I", path = path_plot)
path_plot <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/plots/comparisons/radiation"
compare_TS_vis(rad_gen_I, rad_alex, y = "W/m2", label = "Radiation - INMET",
               save = TRUE, file = "rad_I", path = path_plot)


# Dataset selection -------------------------------------------------------
# Temperature dataset selected: FUNCEME
# Humidity dataset selected: FUNCEME
# Explanation below

# Humidity datasets comparison
#Compare the mean Normalized RMSE (NRMSE)
mean(report_hum_F$NRMSE)
mean(report_hum_I$NRMSE)
#Compare the mean difference between the means
abs(mean(report_hum_F$mean_gen - report_hum_F$mean_alex))
abs(mean(report_hum_I$mean_gen - report_hum_I$mean_alex))
#Compare the mean difference between the medians
abs(mean(report_hum_F$median_gen - report_hum_F$median_alex))
abs(mean(report_hum_I$median_gen - report_hum_I$median_alex))
#Compare the mean difference between the standard deviations
abs(mean(report_hum_F$sd_gen - report_hum_F$sd_alex))
abs(mean(report_hum_I$sd_gen - report_hum_I$sd_alex))
#The humidity obtained using the FUNCEME dataset performs better than the one
#obtained with the INMET dataset in all comparison considered. For this reason,
#it will be the one used in the WASA model

# Temperature datasets comparison
#Compare the mean Normalized RMSE (NRMSE)
mean(report_temp_F$NRMSE)
mean(report_temp_I$NRMSE)
#Compare the mean difference between the means
abs(mean(report_temp_F$mean_gen - report_temp_F$mean_alex))
abs(mean(report_temp_I$mean_gen - report_temp_I$mean_alex))
#Compare the mean difference between the medians
abs(mean(report_temp_F$median_gen - report_temp_F$median_alex))
abs(mean(report_temp_I$median_gen - report_temp_I$median_alex))
#Compare the mean difference between the standard deviations
abs(mean(report_temp_F$sd_gen - report_temp_F$sd_alex))
abs(mean(report_temp_I$sd_gen - report_temp_I$sd_alex))
#The humidity obtained using the FUNCEME dataset performs better than the one
#obtained with the INMET dataset in all comparison considered, apart for a 
#slightly greater standard deviation. For this reason, it will be the one used in the WASA model


# Overlay the time series needed ------------------------------------------

keep_alex <- seq(as.Date("1980-01-01"), as.Date("2009-12-31"), by = "days")
keep_alex_temp <- seq(as.Date("1980-01-01"), as.Date("2004-12-31"), by = "days")
keep_gen <- seq(as.Date("2010-01-01"), as.Date("2018-12-31"), by = "days")
keep_gen_temp <- seq(as.Date("2005-01-01"), as.Date("2018-12-31"), by = "days")

hum_pt1 <- hum_alex[hum_alex[,1] %in% keep_alex, ]
temp_pt1 <- temp_alex[temp_alex[,1] %in% keep_alex_temp, ]
rad_pt1 <- rad_alex[rad_alex[,1] %in% keep_alex, ]

hum_pt2 <- hum_gen_F[hum_gen_F[,1] %in% keep_gen, ]
temp_pt2 <- temp_gen_F[temp_gen_F[,1] %in% keep_gen_temp, ]
rad_pt2 <- rad_gen_I[rad_gen_I[,1] %in% keep_gen, ]

output_hum <- rbind(hum_pt1, hum_pt2)
output_temp <- rbind(temp_pt1, temp_pt2)
output_rad <- rbind(rad_pt1, rad_pt2)


# Visualize the new datasets and save the plots -----------------------------

path_plot <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/plots/input_WASA"

plot_subbasins_df(prec_gen_F, y = "mm", label = "Precipitation - All subbasins - WASA input",
                  save = TRUE, file = "prec_WASA", path = path_plot)
plot_subbasins_df(output_hum, y = "%", label = "Humidity - All subbasins - WASA input",
                  save = TRUE, file = "hum_WASA", path = path_plot)
plot_subbasins_df(output_temp, y = "°C", label = "Temperature - All subbasins - WASA input",
                  save = TRUE, file = "temp_WASA_F2005", path = path_plot)
plot_subbasins_df(output_rad, y = "W/m2", label = "Radiation - All subbasins - WASA input",
                  save = TRUE, file = "rad_WASA", path = path_plot)


# Export the time series as WASA input files ------------------------------

path_export <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/time_series_generation/input_WASA_gen/WASA_Time_Series"

h = "Daily average precipitation [mmd] for each subasin, ordered according Map-IDs\nDate No. of days Subasin-ID\n0 0 123 125 126 127 134 137 138 139 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160"
output_prec <- WASA_input_format(prec_gen_F, prec_gen_F, header = h, path = path_export, name = "rain_daily")
h = "Daily average temperature (in degree Celcius) for each subasin, ordered according Map-IDs\nDate No. of days Subasin-ID\n0 0 123 125 126 127 134 137 138 139 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160"
output_temp <- WASA_input_format(output_temp, output_temp, header = h, path = path_export, name = "temperature_F2005")
h = "Daily average humidity [in %] for each subasin, ordered according Map-IDs\nDate No. of days Subasin-ID\n0 0 123 125 126 127 134 137 138 139 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160"
output_hum <- WASA_input_format(output_hum, output_hum, header = h, path = path_export, name = "humidity")
h = "Daily average shortwave radiation [in Wm2] for each subasin, ordered according Map-IDs\nDate No. of days Subasin-ID\n0 0 123 125 126 127 134 137 138 139 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160"
output_rad <- WASA_input_format(output_rad, output_rad, header = h, path = path_export, name = "radiation")



