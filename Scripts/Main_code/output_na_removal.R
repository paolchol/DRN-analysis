#Outlier removal and NA filling for the generated time series

# Setup ---------------------------------------------------------------

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis")
path_export <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/time_series_generation/input_WASA_gen/processed"

source("Libraries.R")
source("Functions.R")
source("Functions_DP.R")

starting_date<-"1990-01-01"
ending_date<-"2018-12-31"
time_index<-seq(as.Date(starting_date),as.Date(ending_date),by="days")

# Load the generated time series ------------------------------------------
#Load, assign the column names and change the date format

path_import <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/time_series_generation/input_WASA_gen"
columns <- c("date", "doy", 123, 125, 126, 127, 134, 137, 138, 139, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160)

prec <- read.table(paste0(path_import, "/precipitation_FUNCEME_1706.dat"), skip = 3)
names(prec) <- columns
prec$date<-time_index
temp_I <- read.table(paste0(path_import, "/temperature_INMET_1706.dat"), skip = 3)
names(temp_I) <- columns
temp_I$date<-time_index
temp_F <- read.table(paste0(path_import, "/temperature_FUNCEME_1706.dat"), skip = 3)
names(temp_F) <- columns
temp_F$date<-time_index
temp_B <- read.table(paste0(path_import, "/temperature_BOTH_1706.dat"), skip = 3)
names(temp_B) <- columns
temp_B$date<-time_index
hum <- read.table(paste0(path_import, "/humidity_1706.dat"), skip = 3)
names(hum) <- columns
hum$date<-time_index
rad_F <- read.table(paste0(path_import, "/radiation_FUNCEME_1706.dat"), skip = 3)
names(rad_F) <- columns
rad_F$date<-time_index
rad_I <- read.table(paste0(path_import, "/radiation_INMET_1706.dat"), skip = 3)
names(rad_I) <- columns
rad_I$date<-time_index


# Outliers removal -------------------------------------

#Check if outliers are present
#With the IQR method
#Temperature
check_outlier_IQR(temp_F, skip = 2)
check_outlier_IQR(temp_I, skip = 2)
check_outlier_IQR(temp_B, skip = 2)
#Humidity
check_outlier_IQR(hum, skip = 2)
#Radiation
check_outlier_IQR(rad_F, skip = 2)
check_outlier_IQR(rad_I, skip = 2)

#With the 3sd method
# check_outlier_3sd(temp_F, skip = 2)
# check_outlier_3sd(temp_I, skip = 2)
# check_outlier_3sd(temp_B, skip = 2)
# check_outlier_3sd(hum, skip = 2)
# check_outlier_3sd(rad_I, skip = 2)
# check_outlier_3sd(rad_F, skip = 2)

#Remove the outliers
temp_F_IQR <- remove_outlier_IQR(temp_F, skip = 2)
temp_I_IQR <- remove_outlier_IQR(temp_I, skip = 2)
temp_B_IQR <- remove_outlier_IQR(temp_B, skip = 2)
hum_IQR <- remove_outlier_IQR(hum, skip = 2)
rad_F_IQR <- remove_outlier_IQR(rad_F, skip = 2)
rad_I_IQR <- remove_outlier_IQR(rad_I, skip = 2)

#Visualize the differences between the two methods
plot_dataframe(temp_B,skip = 2,label = "Temperature")

plot_dataframe(temp_B_IQR, skip = 2, label = "Temperature B - IQR")

temp_B_3sd <- remove_outlier_3sd(temp_B, skip = 2)
plot_dataframe(temp_B_3sd, skip = 2, label = "Temperature B - 3sd")
#The IQR method seems to remove better the anomalies in the data, so I'll stick
#with it

# NA filling --------------------------------------------------------------

#temp_F_IQR <- fill_na(temp_F_IQR, skipcol = 2) 
#it has a big hole at the start
#add in fill_na an index that will consider the dataframe only from the first non-na value 
temp_I_IQR <- fill_na(temp_I_IQR, skipcol = 2)
temp_B_IQR <- fill_na(temp_B_IQR, skipcol = 2, maxgap = 180)
#hum_IQR <- fill_na(hum_IQR, skipcol = 2)
#rad_F_IQR <- fill_na(rad_F_IQR, skipcol = 2)
rad_I_IQR <- fill_na(rad_I_IQR, skipcol = 2)
#they all have huge holes at the start which can't be filled

# Precipitation ------------------------------------------------------------
#The precipitation dataset needs a different approach to the outlier rejection

plot_dataframe(prec, skip = 2, label = "Precipitation")
