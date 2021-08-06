# Precipitation outlier analyisis ------------------------------------------
#Since precipitation data has a skewed distribution, expecially in semi-arid
#areas as North-East Brazil, a normal outlier rejection wouldn't work properly.
#Here an analysis on the maxima is performed in order to define a threshold
#above which an observation can be considered as an outlier.

# Results:
#The threshold for the outlier detection has been put equal 250 mm,
#an approximated estimation of the 100 years return period in
#Rodrigues, D. T. et al. Spatial distribution of the level of return of extreme precipitation events in Northeast Brazil. (2020)
#doi:10.1002/joc.6507.
#So:
#   - Stations with exceptionally high outliers have been completely removed from
#     the dataframe, considering them not reliable
#   - Singular observations above this threshold have been removed from the dataframe


# Setup -------------------------------------------------------------------

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis")
path_export <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/time_series_generation/input_WASA_gen"

source("Libraries.R")
source("Functions.R")
source("Functions_TG.R")
source("Functions_DP.R")


# Precipitation FUNCEME ---------------------------------------------------

#Load the stations
path = "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/meteorological_stations_ceara/Meteorological_stations_FUNCEME/precipitation_subset"
prec_input = create_data_list(path, c("year","month","day","data"))

#Check and remove the stations which present negative values
n <- find_anomalous_stations(prec_input)
if(n > 0) prec_input <- remove_anomalous_stations(prec_input)

#Create a dataframe to plot the stations data and check anomalous behaviour
date <- create_date_vector(1980, 2018)
prec_pos <- create_position_vector(prec_input[[1]])
prec_input[[2]] <- add_date_column(prec_input[[2]])
prec_df <- create_main_dataframe(date, prec_input[[2]], prec_pos)
f<-2
l<-21
for(i in 1:round(ncol(prec_df)/20)){
  vis_prec <- data.frame(prec_df$date, prec_df[,f:l])
  names(vis_prec) <- c("date", names(prec_df[,f:l]))
  plot_stations_df(vis_prec, y = "mm", label = paste0("FUNCEME Precipitation Stations: ", f-1, " to ", l-1))
  f<-f+19
  l<-l+19
  l<-ifelse(l > ncol(prec_df), ncol(prec_df), l)
  f<-ifelse(f > l, l-19, f)
}
#Comments:
#No anomalous behavior visible. Some stations have observations well above 200 mm,
#which may mean they are outliers to be removed. In order to check this, below 
#an analysis on the precipitation maxima is done, in order to obtain a threshold over
#which an observation has to be considered as an outlier

# Analysis on the maxima --------------------------------------------------

#For each station, extract the maximum for each year of observations
#Put NA if no observations are registered that year

prec_maxima <- extract_max(prec_df)
xx = function(x){x[is.infinite(x)]<-NA; return(x)}
prec_maxima <- as.data.frame(apply(prec_maxima, 2, FUN = xx))
View(prec_maxima)
#For each station, remove the maxima that are clearly errors
#The threshold for errors is 250 mm, the approximated estimated 100 years return period in
#Rodrigues, D. T. et al. Spatial distribution of the level of return of extreme precipitation events in Northeast Brazil. (2020)
#doi:10.1002/joc.6507.

prec_maxima[,2:ncol(prec_maxima)][prec_maxima[,2:ncol(prec_maxima)] > 250] <- NA
View(prec_maxima)

#For each station, perform the IQR to identify if outliers are present in the maxima
#Check outliers

check_outlier_IQR(prec_maxima)

#For each station, remove the outliers using the IQR method

prec_maxima_IQR <- remove_outlier_IQR(prec_maxima)
#The result is too variable, values aroud 150 mm can be removed by this method,
#so this outlier rejection will not be done

#For each station, obtain the maximum of the remaining maxima
#These will be the threshold for the outlier rejection in the original dataset

xx = function(x){y<-max(x, na.rm = TRUE); return(y)}
prec_threshold <- apply(prec_maxima[,2:ncol(prec_maxima)], 2, xx)
prec_threshold <- t(data.frame(prec_threshold))
names(prec_threshold) <- colnames(prec_maxima)[2:ncol(prec_maxima)]

#St24748 and St4705 have no maxima left after having removed the maxima above
#250 mm
#Let's check again the maxima in these stations
prec_maxima <- extract_max(prec_df)
xx = function(x){x[is.infinite(x)]<-NA; return(x)}
prec_maxima <- as.data.frame(apply(prec_maxima, 2, FUN = xx))
View(prec_maxima$St24748) #there is no maxima already
View(prec_maxima$St4705) #there is no maxima already
sum(!is.na(prec_df$St24748)) #that's because there is no data in the stations!
sum(!is.na(prec_df$St4705))
#Leave the stations there, they would do nothing anyway and they could have data
#from 2019

# Outlier rejection -------------------------------------------------------

#Having the thresholds for each station, remove all observations for that station
#which are above the threshold

# prec_df <- rejection_threshold(prec_df, method = "vector", v = prec_threshold)
#On second thought, this method has a flaw. If I remove the maxima over the 250 mm
#threshold, and then take as a threshold the maximum of the remaining maxima,
#I'm not considering the "second best" in the original observations.

#For this reason, I think it's just the simplest option to remove the values
#above 250 mm from the original observations, although this analysis on the maxima
#remains somewhat valid if this flaw is fixed (e.g. by replacing the removed maxima
#with the second best)

#But, since some of the stations have really absurd measurement, they will be removed
#Below the mean of the maxima of each station is computed, and then the stations with
#a mean of the maxima higher than 250 mm are removed from the dataframe
xx = function(x){y<-mean(x, na.rm = TRUE); return(y)}
prec_mean_max <- apply(prec_maxima[,2:ncol(prec_maxima)], 2, xx)
sum(prec_mean_max > 250, na.rm = TRUE)
#Only nine stations registered these kind of values, showing better how these are
#outliers and not particularly extreme events
View(sort(prec_threshold))

#Remove anomalous stations
remove_st <- which(prec_mean_max > 250) + 1
prec_df <- prec_df[-remove_st]

#Remove every value higher than 250 mm
prec_df <- rejection_threshold(prec_df, method = "value", 250)

# Function to obtain remove_st --------------------------------------------

anomalous_prec_st = function(prec_df, threshold = 250){
  prec_maxima <- extract_max(prec_df)
  xx = function(x){x[is.infinite(x)]<-NA; return(x)}
  prec_maxima <- as.data.frame(apply(prec_maxima, 2, xx))
  xx = function(x){y<-mean(x, na.rm = TRUE); return(y)}
  prec_mean_max <- apply(prec_maxima[,2:ncol(prec_maxima)], 2, xx)
  remove_st <- which(prec_mean_max > 250) + 1
  
  print(paste0(sum(prec_mean_max > 250, na.rm = TRUE), " stations will be removed"))
  return(remove_st)
}





