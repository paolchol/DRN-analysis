#Model enhancement


# Script setup -------------------------------------------------------------------

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")

source("./Libraries/Libraries.R")
source("./Libraries/Functions.R")
source("./Libraries/Functions_TG.R")
source("./Libraries/Functions_DP.R")
source("./Libraries/Functions_CO.R")
source("./Libraries/Functions_MC.R")

#Load hydroGOF the hard way
files_sources = list.files("./Libraries/hydroGOF-master/R", full.names = T)
sapply(files_sources, source)

#Model paths
path_WASA_input <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Input"
path_WASA_output <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Output"

# Evaluation setup --------------------------------------------------------

#Load the maximum capacities of the reservoirs
path_maxcap <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Input/Reservoir/reservoir.dat"
reservoirs <- read.table(path_maxcap, skip = 2, sep = "\t")
maxcap <- reservoirs[,c(1,5)]
names(maxcap) <- c('ID','max')
maxcap[,2] <- maxcap[,2]*1000 #m3

#Load the reservoirs' IDs
load("./Inputs/General/IDs.RData")

#Path for scaling_factor file
path_scaling_in <- "./Inputs/Calibration/uncalibrated_scaling_factor.dat"
path_scaling_out <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Input/Others"

#Load the scaling factors
sf_load <- "./Inputs/Calibration/calibrated_scaling_factor.dat"
cal_mod <- read.table(sf_load, sep = "\t", header = TRUE)
uncal_mod <- read.table(path_scaling_in, sep = '\t', header = TRUE)

# 1 - Time series update --------------------------------------------------

ME1_results <- list()

#Take the files to copy
ME1_files <- list.files("./Inputs/Model_input/MEs/1_Time_series_update", ".dat$", full.names = T)
#do.dat
file.copy(from = ME1_files[1], to = path_WASA_input, overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
#rainy_season
file.copy(from = ME1_files[6], to = paste0(path_WASA_input, "/Hillslope"), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
#time series
file.copy(from = ME1_files[c(2:5, 7)], to = paste0(path_WASA_input, "/Time_series"), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)

#Form the new obs_df
path_obs <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/Observed_volumes/volume_series_banabuiu_reservoirs_ID"
obs_input <- create_data_list(path_obs, c('date', 'volume'), skip = 1, f = read.csv, s = ",")
date <- create_date_vector(1980, 2020)
IDs <- data.frame(ID = gsub("\\..*","",basename(obs_input[[1]])))
for(i in 1:length(obs_input[[2]])){
  obs_input[[2]][[i]]$date <- change_date_format(obs_input[[2]][[i]]$date)
}
obs_df <- create_main_dataframe(date, obs_input[[2]], IDs, timecol = 1, datacol = 2)
obs_df <- remove_high_values(obs_df, maxcap$max)
obs_df <- monthly_scale(obs_df, f = sumx)

#Run the model
#Calibrated
modify_scaling_factor(path_scaling_in, path_scaling_out, cal_mod[,1], cal_mod[,2])
system2("C:/Thesis_fortran/WASA_Thesis/WASA_Thesis/x64/Release/WASA_Thesis.exe")
results <- WASA_calibration_evaluation(path_WASA_output, IDs, maxcap$max, obs_df,
                                           code = 'ME1_cal', complete = TRUE, start_obs = TRUE,
                                           keep_mean = FALSE, st_date = 1980, end_date = 2020)
ME1_results[["ME1_cal"]] <- results$complete

#Uncalibrated
modify_scaling_factor(path_scaling_in, path_scaling_out, uncal_mod[,1], uncal_mod[,2])
system2("C:/Thesis_fortran/WASA_Thesis/WASA_Thesis/x64/Release/WASA_Thesis.exe")
results <- WASA_calibration_evaluation(path_WASA_output, IDs, maxcap$max, obs_df,
                                       code = 'ME1_uncal', complete = TRUE, start_obs = TRUE,
                                       keep_mean = FALSE, st_date = 1980, end_date = 2020)
ME1_results[["ME1_uncal"]] <- results$complete

mean_performance(ME1_results[["ME1_cal"]], c(138, 143, 146))
mean_performance(ME1_results[["ME1_uncal"]], c(138, 143, 146))


# 2 - Time series reanalysis ----------------------------------------------

ME2_results <- list()

#Take the files to copy
ME2_files <- list.files("./Inputs/Model_input/MEs/2_Time_series_reanalysis", ".dat$", full.names = T)
#do.dat
file.copy(from = ME2_files[1], to = path_WASA_input, overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
#rainy_season
file.copy(from = ME2_files[6], to = paste0(path_WASA_input, "/Hillslope"), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
#time series
file.copy(from = ME2_files[c(2:5, 7)], to = paste0(path_WASA_input, "/Time_series"), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)

#Form the new obs_df
obs_input <- create_data_list(path_obs, c('date', 'volume'), skip = 1, f = read.csv, s = ",")
date <- create_date_vector(1981, 2020)
IDs <- data.frame(ID = gsub("\\..*","",basename(obs_input[[1]])))
for(i in 1:length(obs_input[[2]])){
  obs_input[[2]][[i]]$date <- change_date_format(obs_input[[2]][[i]]$date)
}
obs_df <- create_main_dataframe(date, obs_input[[2]], IDs, timecol = 1, datacol = 2)
obs_df <- remove_high_values(obs_df, maxcap$max)
obs_df <- monthly_scale(obs_df, f = sumx)

#Run the model
#Calibrated
modify_scaling_factor(path_scaling_in, path_scaling_out, cal_mod[,1], cal_mod[,2])
system2("C:/Thesis_fortran/WASA_Thesis/WASA_Thesis/x64/Release/WASA_Thesis.exe")
results <- WASA_calibration_evaluation(path_WASA_output, IDs, maxcap$max, obs_df,
                                       code = 'ME2_cal', complete = TRUE, start_obs = TRUE,
                                       keep_mean = FALSE, st_date = 1981, end_date = 2020)
ME2_results[["ME2_cal"]] <- results$complete

#Uncalibrated
modify_scaling_factor(path_scaling_in, path_scaling_out, uncal_mod[,1], uncal_mod[,2])
results <- data.frame(mean = 0)
for(i in 1:3){
  skip <- FALSE
  system2("C:/Thesis_fortran/WASA_Thesis/WASA_Thesis/x64/Release/WASA_Thesis.exe")
  tryCatch(results <- WASA_calibration_evaluation(path_WASA_output, IDs, maxcap$max, obs_df,
                                         code = 'ME2_uncal', complete = TRUE, start_obs = TRUE,
                                         keep_mean = FALSE, st_date = 1981, end_date = 2020)
           , error = function(e){skip <<- TRUE})
  if(results$mean != 0) break
  if(skip){next}
}
ME2_results[["ME2_uncal"]] <- results$complete

mean_performance(ME2_results[["ME2_cal"]], c(138, 143, 146))
mean_performance(ME2_results[["ME2_uncal"]], c(138, 143, 146))


# Select the best configuration between #1 and #2 -------------------------

#The best configuration is ME1_cal

# 3 - Add points to the CAV curve -----------------------------------------

#Take the file to copy
ME3_files <- list.files("./Inputs/Model_input/MEs/3_Reservoirs_par", ".dat$", full.names = T)
#cav.dat
file.copy(from = ME3_files[1], to = paste0(path_WASA_input,"/Reservoir"), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)

#Take the files to copy
ME1_files <- list.files("./Inputs/Model_input/MEs/1_Time_series_update", ".dat$", full.names = T)
#do.dat
file.copy(from = ME1_files[1], to = path_WASA_input, overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
#rainy_season
file.copy(from = ME1_files[6], to = paste0(path_WASA_input, "/Hillslope"), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
#time series
file.copy(from = ME1_files[c(2:5, 7)], to = paste0(path_WASA_input, "/Time_series"), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)

#Form the new obs_df
path_obs <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/Observed_volumes/volume_series_banabuiu_reservoirs_ID"
obs_input <- create_data_list(path_obs, c('date', 'volume'), skip = 1, f = read.csv, s = ",")
date <- create_date_vector(1980, 2020)
IDs <- data.frame(ID = gsub("\\..*","",basename(obs_input[[1]])))
for(i in 1:length(obs_input[[2]])){
  obs_input[[2]][[i]]$date <- change_date_format(obs_input[[2]][[i]]$date)
}
obs_df <- create_main_dataframe(date, obs_input[[2]], IDs, timecol = 1, datacol = 2)
obs_df <- remove_high_values(obs_df, maxcap$max)
obs_df <- monthly_scale(obs_df, f = sumx)

#Run the model
#Calibrated
modify_scaling_factor(path_scaling_in, path_scaling_out, cal_mod[,1], cal_mod[,2])
system2("C:/Thesis_fortran/WASA_Thesis/WASA_Thesis/x64/Release/WASA_Thesis.exe")
results <- WASA_calibration_evaluation(path_WASA_output, IDs, maxcap$max, obs_df,
                                       code = 'ME1_cal', complete = TRUE, start_obs = TRUE,
                                       keep_mean = FALSE, st_date = 1980, end_date = 2020)
ME3_results <- results$complete
mean_performance(ME3_results, c(138, 143, 146))



