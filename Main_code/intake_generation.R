#intake.dat generation

# Setup -------------------------------------------------------------------

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")

source("./Libraries/Libraries.R")
source("./Libraries/Functions.R")

path_export <- "./Inputs/Model_input/Model_enhancement/Time_series_update"

# Load files --------------------------------------------------------------

path<-"C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/waterdrawal_banabuiu_basin/renamed"
index_files <- data.frame(list.files(path = path, full.names = T, recursive = TRUE))
names(index_files) <- "path"
index_files$SubbasinID <- gsub("\\..*","",basename(index_files$path))

withdrawal_list<-list()
for(i in 1:nrow(index_files)){
  name<-paste0("res_",index_files$SubbasinID[i])
  withdrawal_list[[name]]<-read.csv(index_files$path[i], header = TRUE, sep = ",")
}

#Change the format of the date column to date
for(i in 1:length(withdrawal_list[])){
  withdrawal_list[[i]][["date"]] <- as.Date(withdrawal_list[[i]][["date"]])
}

#Check the percentage of NAs in the observations
for(i in 1:length(withdrawal_list[])){
  print(sum(is.na(withdrawal_list[[i]]$flow))/length(withdrawal_list[[i]]$flow))
}

# Create the dataframe ----------------------------------------------------

#Specify the date range
#Create the date vector
start_y <- 1980
end_y <- 2020 #2018
start_m <- 01
end_m <- 12
start<-paste0("01/0",start_m,"/",start_y)
end<-paste0("31/",end_m,"/",end_y)
date<-seq(as.Date(start,"%d/%m/%Y"), as.Date(end,"%d/%m/%Y"), by="days")

#Create the dataframe
withdrawal_df<-data.frame(date)
withdrawal_df$doy<-NA

for(i in 1:length(withdrawal_list[])){
  new_v<-sistemadati_DCA(vettore = withdrawal_list[[i]], lung = nrow(withdrawal_df), g = date,
                         coltime = 1, coldata = 2)
  withdrawal_df$new<-new_v[,2]
  names(withdrawal_df)[names(withdrawal_df) == "new"]<-index_files$SubbasinID[i]
}

#Plot to check anomalies
# source("Functions_CO.R")
# path_plot <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/Model_calibration"
# plot_subbasins_df(withdrawal_df, y = "m3/s", label = "Withdrawal",
#                   interactive = TRUE, file = "withdrawal_line", path = path_plot, line = TRUE)

#Change the NA values to -999
withdrawal_df[,3:ncol(withdrawal_df)][is.na(withdrawal_df[,3:ncol(withdrawal_df)])] <- -999

#Put the dataframe in the WASA format and save it

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

#path_export <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/time_series_generation/input_WASA_gen"
h <- "# Specification of controlled release through reservoir's intake devices in [m3/s]\nDate,\tDoy,\t123,\t125,\t126,\t127,\t138,\t142,\t143,\t145,\t146,\t147,\t148,\t149,\t150,\t151,\t152,\t153,\t154,\t156,\t160"
withdrawal_df_WASA <- WASA_input_format_intake(withdrawal_df, h, path_export, "intake")



