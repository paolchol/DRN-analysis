#Script to make experiments about the methods to fill the missing values
#in the generated time series

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

# Load the data -----------------------------------------------------------

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
#which is not ideal
rad_gen_I_fill <- fill_na(rad_gen_I, maxgap = 210)
plot_subbasins_df(rad_gen_I_fill, y = "W/m2", label = "Radiation INMET - With fill_na")

#To fill this hole, waiting for the updated radiation data from FUNCEME, the mean
#of the same days in the previous years is taken and placed instead of the missing
#value
#This approach can be justified since radiation data does not present and high
#inter-annual variability
rad_gen_I_fix <- mean_previous_years(rad_gen_I)
plot_subbasins_df(rad_gen_I_fix, y = "W/m2", label = "Radiation INMET - Last six months extrapolated")
#The result is smoothed as expected with respect to the previous years
#For now this data can be used, and then improved when the new data will be available
