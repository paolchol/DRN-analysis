#Radiation dataset cleaning

# Setup -------------------------------------------------------------------

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis")

source("Libraries.R")
source("Functions.R")
source("Functions_DP.R")
source("Functions_TG.R")
source("Functions_CO.R")

# Load the precipitation time series --------------------------------------

path <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/time_series_generation/input_WASA_gen/WASA_Time_Series"
rad <- read.table(paste0(path, "/radiation.dat"), skip = 3)
columns <- c("date", "n_days", "ID123", "ID125", "ID126", "ID127", "ID134", "ID137", "ID138", "ID139", "ID142", "ID143", "ID144", "ID145", "ID146", "ID147", "ID148", "ID149", "ID150", "ID151", "ID152", "ID153", "ID154", "ID155", "ID156", "ID157", "ID158", "ID159", "ID160")
names(rad) <- columns
rad$date <- change_date_WASA_input(rad$date)


# Visualize the data and find the anomalous subbasins ---------------------

plot_subbasins_df(rad, y = "W/m2", label = "Radiation INMET - All subbasins",
                  interactive = TRUE, file = "radiation_anomalies")
#All subbasins present anomalies

# Remove the anomalies ----------------------------------------------------



