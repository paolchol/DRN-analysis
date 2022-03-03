#rainy_season.dat update


# Setup -------------------------------------------------------------------

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")
# path_export <- "./Inputs/Model_input/Model_enhancement/Time_series_update"
path_export <- "./Inputs/Model_input/Model_enhancement/Time_series_reanalysis"

source("./Libraries/Libraries.R")
source("./Libraries/Functions.R")
source("./Libraries/Functions_CO.R")

# Load the precipitation time series --------------------------------------

# path <- "./Inputs/Model_input/Model_enhancement/Time_series_update"
path <- "./Inputs/Model_input/Model_enhancement/Time_series_reanalysis"
prec <- read.table(paste0(path, "/rain_daily.dat"), skip = 3)
columns <- c("date", "n_days", "ID123", "ID125", "ID126", "ID127", "ID134", "ID137", "ID138", "ID139", "ID142", "ID143", "ID144", "ID145", "ID146", "ID147", "ID148", "ID149", "ID150", "ID151", "ID152", "ID153", "ID154", "ID155", "ID156", "ID157", "ID158", "ID159", "ID160")
names(prec) <- columns
prec$date <- change_date_WASA_input(prec$date)

# Compute the rainy season file -------------------------------------------
#For each subbasin, extract the first day of the year with precipitation > 0
#and the last day of the year with precipitation > 0, for each year

get_rainy_season = function(df, years, date_col = 1, skip = 2){
  #The rainy season in North-East Brazil goes from January to June.
  #This function will extract the first occurrence of rain in January and the last
  #occurrence of rain in June for the precipitation data in input (df).
  #If no rain is found in January, the first day is searched in the following
  #months. If no rain is found in June, the last day is searched in the previous
  #months.
  
  start_m <- 1 #January
  end_m <- 6 #June
  skip <- skip + 1
  rainy_season <- data.frame(rep(colnames(df)[skip:ncol(df)], length(years)),
                             year = 0, start_30 = 0, start = 0, end = 0, end_30 = 0)
  names(rainy_season) <- c('ID', 'year', 'start_30', 'start', 'end', 'end_30')
  k <- 1
  for(j in 1:length(years)){
    for(i in skip:ncol(df)){
      v <- df[which(lubridate::year(df[,date_col]) == years[j] &
                      lubridate::month(df[, date_col]) == start_m), c(1,i)]
      while(is_empty(which(v[,2] > 0)) & start_m <= 12){
        start_m <- start_m + 1
        v <- df[which(lubridate::year(df[,date_col]) == years[j] &
                        lubridate::month(df[, date_col]) == start_m), c(1,i)]
      }
      rainy_season$year[k] <- years[j]
      rainy_season$start[k] <- lubridate::yday(v[which(v[,2] > 0)[1], 1])
      v <- df[which(lubridate::year(df[,date_col]) == years[j] &
                      lubridate::month(df[, date_col]) == end_m), c(1,i)]
      while(is_empty(which(v[,2] > 0)) & end_m >= 1){
        end_m <- end_m - 1
        v <- df[which(lubridate::year(df[,date_col]) == years[j] &
                        lubridate::month(df[, date_col]) == end_m), c(1,i)]
      }
      rainy_season$end[k] <- lubridate::yday(v[which(v[,2] > 0)[length(which(v[,2] > 0))], 1])
      k <- k + 1
      start_m <- 1
      end_m <- 6
    }
  }
  rainy_season$start_30 <- rainy_season$start - 30
  rainy_season$end_30 <- rainy_season$end + 30
  return(rainy_season)
}

years <- seq(1981, 2020, 1)
rainy_season <- get_rainy_season(prec, years)

# Export in the WASA input format -----------------------------------------

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
rainy_season$ID <- substrRight(rainy_season$ID,3)

#path_export <- path
h <- "Specification of the rainy season (per year)\nfor the interpolation o temporal distribution of  vegetation characteristics (Rootdepth, height, lai, albedo)\nSubasin Year (Start-30) Startday Endday (End+30)"
rseason_out <- my.write(rainy_season, paste0(path_export, "/rainy_season.dat"), header = h, f = write.table)






