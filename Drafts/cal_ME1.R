
#Directory
setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")
#Path to the original observations
path_obs <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/Observed_volumes/volume_series_banabuiu_reservoirs_ID"
#Path to the WASA results defining the baseline
path_baseline <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Output_Second_Run"
#Path to save the intermediate results
path_save <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/Model_calibration/Automatic_calibration"

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

#Load the maximum capacities of the reservoirs
path_maxcap <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Input/Reservoir/reservoir.dat"
reservoirs <- read.table(path_maxcap, skip = 2, sep = "\t")
maxcap <- reservoirs[,c(1,5)]
names(maxcap) <- c('ID','max')
maxcap[,2] <- maxcap[,2]*1000 #m3


path_scaling_in <- "./Inputs/Calibration/uncalibrated_scaling_factor.dat"
path_scaling_out <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Input/Others"


#Obs
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

#Cal + ME1 + ME3

# #ME1 - Take the files to copy
# ME1_files <- list.files("./Inputs/Model_input/MEs/1_Time_series_update", ".dat$", full.names = T)
# #do.dat
# file.copy(from = ME1_files[1], to = path_WASA_input, overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
# #rainy_season
# file.copy(from = ME1_files[6], to = paste0(path_WASA_input, "/Hillslope"), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
# #time series
# file.copy(from = ME1_files[c(2:5, 7)], to = paste0(path_WASA_input, "/Time_series"), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)

#Load the scaling factors
sf_load <- "./Inputs/Calibration/calibrated_scaling_factor.dat"
cal_mod <- read.table(sf_load, sep = "\t", header = TRUE)
modify_scaling_factor(path_scaling_in, path_scaling_out, cal_mod[,1], cal_mod[,2])

#Run
code <- 'calME1ME3'
results <- data.frame(mean = 0)
for(i in 1:3){
  skip <- FALSE
  system2("C:/Thesis_fortran/WASA_Thesis/WASA_Thesis/x64/Release/WASA_Thesis.exe")
  tryCatch(results <- WASA_calibration_evaluation(path_WASA_output, IDs, maxcap$max, obs_df,
                                                  code = code, complete = TRUE, start_obs = TRUE,
                                                  keep_mean = FALSE, st_date = 1980, end_date = 2020)
           , error = function(e){skip <<- TRUE})
  if(results$mean != 0) break
  if(skip){next}
}

plot_stat_calibration(results$complete, code)
print("Number of runs needed:")
print(i)

mean_performance(results$complete, c(138, 143, 146))

#Plot comparison in the selected basin
#Load the output
date <- create_date_vector(1980, 2020)
mod_df <- load_WASA_results(path_WASA_output, IDs, dates = date)
mod_df <- remove_high_values(mod_df, maxcap$max)
mod_df <- monthly_scale(mod_df, f = sumx)
#Write the subbasins to visualize
ID_plot <- 156
plot_calibration(it = mod_df, obs = obs_df, ID = ID_plot, code = code, nobase = TRUE)

#Confront with the calibrated

load('./Inputs/Calibration/df_calibrated.RData')
load('./Inputs/Calibration/df_observations.RData')

plot_calibration(it = mod_df, obs = obs_df, ID = ID_plot, code = code, nobase = TRUE)




#New baseline

cal_mod <- data.frame(IDs, val = 1)
modify_scaling_factor(path_scaling_in, path_scaling_out, cal_mod[,1], cal_mod[,2])


code <- 'new_baseline'
results <- data.frame(mean = 0)
for(i in 1:3){
  skip <- FALSE
  system2("C:/Thesis_fortran/WASA_Thesis/WASA_Thesis/x64/Release/WASA_Thesis.exe")
  tryCatch(results <- WASA_calibration_evaluation(path_WASA_output, IDs, maxcap$max, obs_df,
                                                  code = code, complete = TRUE, start_obs = TRUE,
                                                  keep_mean = FALSE, st_date = 1980, end_date = 2020)
           , error = function(e){skip <<- TRUE})
  if(results$mean != 0) break
  if(skip){next}
}

plot_stat_calibration(results$complete, code)
print("Number of runs needed:")
print(i)

date <- create_date_vector(1980, 2020)
base_df <- load_WASA_results(path_WASA_output, IDs, dates = date)
base_df <- remove_high_values(mod_df, maxcap$max)
base_df <- monthly_scale(mod_df, f = sumx)

save(base_df, file = "./Inputs/Calibration/cal_ME1_ME3/df_baseline.RData")

for(i in 1:7){
  skip <- FALSE
  if(i==4) tryCatch(print(b), error = function(e){skip <<- TRUE})
  if(skip){print("i uguale 4")}
  print(i)
}






