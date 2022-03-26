#Validation script


# Setup -------------------------------------------------------------------

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")

source("./Libraries/Libraries.R")
source("./Libraries/Functions.R")
source("./Libraries/Functions_TG.R")
source("./Libraries/Functions_DP.R")
source("./Libraries/Functions_CO.R")
source("./Libraries/Functions_MC.R")

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")

# Load the actual dataset -------------------------------------------------

#Weather data
prec <- read.table("./Inputs/Model_input/Base/Time_series/rain_daily.dat", skip = 3)
temp <- read.table("./Inputs/Model_input/Base/Time_series/temperature.dat", skip = 3)
hum <- read.table("./Inputs/Model_input/Base/Time_series/humidity.dat", skip = 3)
rad <- read.table("./Inputs/Model_input/Base/Time_series/radiation.dat", skip = 3)

columns <- c("date", "doy", "ID123", "ID125", "ID126", "ID127", "ID134", "ID137", "ID138", "ID139", "ID142", "ID143", "ID144", "ID145", "ID146", "ID147", "ID148", "ID149", "ID150", "ID151", "ID152", "ID153", "ID154", "ID155", "ID156", "ID157", "ID158", "ID159", "ID160")
names(prec) <- columns
names(temp) <- columns
names(hum) <- columns
names(rad) <- columns

prec$date <- change_date_WASA_input(prec$date)
temp$date <- change_date_WASA_input(temp$date)
hum$date <- change_date_WASA_input(hum$date)
rad$date <- change_date_WASA_input(rad$date)

cal_prec <- prec[prec$date <= as.Date("2006-12-31"), ]
cal_temp <- temp[temp$date <= as.Date("2006-12-31"), ]
cal_hum <- hum[hum$date <= as.Date("2006-12-31"), ]
cal_rad <- rad[rad$date <= as.Date("2006-12-31"), ]

val_prec <- prec[prec$date >= as.Date("2007-01-01"), ]
val_temp <- temp[temp$date >= as.Date("2007-01-01"), ]
val_hum <- hum[hum$date >= as.Date("2007-01-01"), ]
val_rad <- rad[rad$date >= as.Date("2007-01-01"), ]

#Intake
intake <- read.table("./Inputs/Model_input/Base/Time_series/intake.dat", skip = 2)
intake[,1] <- change_date_WASA_input(intake[,1])

cal_intake <- intake[intake[,1] <= as.Date("2006-12-31"), ]
val_intake <- intake[intake[,1] >= as.Date("2007-01-01"), ]
names(cal_intake)[1] <- "date"
names(val_intake)[1] <- "date"

#Rainy season
rainy_s <- read.table("./Inputs/Model_input/Base/Hillslope/rainy_season.dat", skip = 3)

cal_rs <- rainy_s[rainy_s[,2] <= 2006, ]
val_rs <- rainy_s[rainy_s[,2] >= 2007, ]

# Save --------------------------------------------------------------------

cal_path <- "./Inputs/Model_input/Calibration"
val_path <- "./Inputs/Model_input/Validation"

h = "Daily average precipitation [mmd] for each subasin, ordered according Map-IDs\nDate No. of days Subasin-ID\n0 0 123 125 126 127 134 137 138 139 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160"
cal_prec <- WASA_input_format(cal_prec, cal_prec, header = h, path = cal_path, name = "rain_daily")
val_prec <- WASA_input_format(val_prec, val_prec, header = h, path = val_path, name = "rain_daily")

h = "Daily average temperature (in degree Celcius) for each subasin, ordered according Map-IDs\nDate No. of days Subasin-ID\n0 0 123 125 126 127 134 137 138 139 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160"
cal_temp <- WASA_input_format(cal_temp, cal_temp, header = h, path = cal_path, name = "temperature")
val_temp <- WASA_input_format(val_temp, val_temp, header = h, path = val_path, name = "temperature")

h = "Daily average humidity [in %] for each subasin, ordered according Map-IDs\nDate No. of days Subasin-ID\n0 0 123 125 126 127 134 137 138 139 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160"
cal_hum <- WASA_input_format(cal_hum, cal_hum, header = h, path = cal_path, name = "humidity")
val_hum <- WASA_input_format(val_hum, val_hum, header = h, path = val_path, name = "humidity")

h = "Daily average shortwave radiation [in Wm2] for each subasin, ordered according Map-IDs\nDate No. of days Subasin-ID\n0 0 123 125 126 127 134 137 138 139 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160"
cal_rad <- WASA_input_format(cal_rad, cal_rad, header = h, path = cal_path, name = "radiation")
val_rad <- WASA_input_format(val_rad, val_rad, header = h, path = val_path, name = "radiation")

#Rainy season
h <- "Specification of the rainy season (per year)\nfor the interpolation o temporal distribution of  vegetation characteristics (Rootdepth, height, lai, albedo)\nSubasin Year (Start-30) Startday Endday (End+30)"
my.write(cal_rs, paste0(cal_path, "/rainy_season.dat"), header = h, f = write.table)
my.write(val_rs, paste0(val_path, "/rainy_season.dat"), header = h, f = write.table)

#Intake
WASA_input_format_intake = function(df, header, path, name){
  year<-lubridate::year(df$date)
  month<-lubridate::month(df$date)
  month[which(month < 10)]<-paste0("0",month[which(month < 10)])
  day<-lubridate::day(df$date)
  #day[which(day < 10)]<-paste0("0",day[which(day < 10)])
  #Insert the number of days
  df$doy<-lubridate::yday(df$date)
  #Insert the date in the WASA format
  df$date<-paste0(day,month,year)
  my.write(df, file = paste0(path,"/", name,".dat"),
           header = header, f = write.table, sepset = TRUE)
  return(df)
}

h <- "# Specification of controlled release through reservoir's intake devices in [m3/s]\nDate,\tDoy,\t123,\t125,\t126,\t127,\t138,\t142,\t143,\t145,\t146,\t147,\t148,\t149,\t150,\t151,\t152,\t153,\t154,\t156,\t160"
cal_intake <- WASA_input_format_intake(cal_intake, h, cal_path, "intake")
val_intake <- WASA_input_format_intake(val_intake, h, val_path, "intake")
