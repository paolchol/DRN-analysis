#Validation


# Setup -------------------------------------------------------------------

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")

source("./Libraries/Libraries.R")
source("./Libraries/Functions.R")
source("./Libraries/Functions_TG.R")
source("./Libraries/Functions_DP.R")
source("./Libraries/Functions_CO.R")
source("./Libraries/Functions_MC.R")

#Load the maximum capacities of the reservoirs
path_maxcap <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Input/Reservoir/reservoir.dat"
reservoirs <- read.table(path_maxcap, skip = 2, sep = "\t")
maxcap <- reservoirs[,c(1,5)]
names(maxcap) <- c('ID','max')
maxcap[,2] <- maxcap[,2]*1000 #m3

#Load the reservoirs' IDs
load("./Inputs/General/IDs.RData")

#Load the observations
load("./Inputs/Calibration/df_observations.RData")

# Definition of calibration and validation windows ------------------------

#Following the 1a point in the hierarchical scheme proposed by Klemes, 1986,
#the model is calibrated over 2/3 of the dataset and then validated over 1/3 of
#the dataset
#So, the two windws result as it follows:
# - Calibration window: 1980 - 2006
# - Validation window: 2007 - 2018

# Load the calibrated configuration ------------------------------------
#Load the calibrated scaling factors and set them as input to the model

#Path for scaling_factor file and output
path_scaling_in <- "./Inputs/Calibration/uncalibrated_scaling_factor.dat"
path_scaling_out <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Input/Others" 
path_WASA_output <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Output"

#Load the scaling factors and modify the input one
sf_load <- "./Inputs/Calibration/calibrated_scaling_factor.dat"
scaling_factors <- read.table(sf_load, sep = "\t", header = TRUE)
subb <- scaling_factors[,1]; val <- scaling_factors[,2]
modify_scaling_factor(path_scaling_in, path_scaling_out, subb, val)

# Launch on the calibration window ----------------------------------------

#Modify the do.dat file
#tstart: 1980
#tstop: 2006

#Launch the model
system2("C:/Thesis_fortran/WASA_Thesis/WASA_Thesis/x64/Release/WASA_Thesis.exe")

# Evaluation of the calibration results --------------------------------------

#Reservoirs IDs
#In 2006, 138 and 142 had not been built yet, so they are exluded
cal_IDs <- data.frame(ID = IDs$ID[!IDs$ID %in% c(138, 142)])

#Calibration observations
cal_obs <- obs_df[, !names(obs_df) %in% c(138, 142)]
cal_obs <- cal_obs[cal_obs$date <= as.Date("2006-12-01"), ]

#Calibration maxvalues
cal_maxval <- maxcap$max[maxcap$ID %in% cal_IDs]

#Compute the evaluation
cal_results <- WASA_calibration_evaluation(path_WASA_output, cal_IDs, cal_maxval, cal_obs,
                                           code = 'calibration', complete = TRUE, start_obs = TRUE,
                                           keep_mean = FALSE, st_date = 1980, end_date = 2006)

plot_stat_calibration(cal_results$complete, 'Calibration')

# Launch on the validation window -----------------------------------------

#Modify the do.dat file
#tstart: 2007
#tstop: 2018

#Launch the model
system2("C:/Thesis_fortran/WASA_Thesis/WASA_Thesis/x64/Release/WASA_Thesis.exe")

# Evaluation of the validation results --------------------------------------

#Reservoirs IDs
#In 2006, 138 and 142 had not been built yet, so they are exluded
val_IDs <- IDs

#Validation observations
val_obs <- obs_df[obs_df$date >= as.Date("2007-01-01"),]

#Validation maxvalues
val_maxval <- maxcap$max

#Compute the evaluation
val_results <- WASA_calibration_evaluation(path_WASA_output, val_IDs, val_maxval, val_obs,
                                           code = 'validation', complete = TRUE, start_obs = TRUE,
                                           keep_mean = FALSE, st_date = 2007, end_date = 2018)

plot_stat_calibration(val_results$complete, 'validation')

# Mean results ------------------------------------------------------------

tests <- c("R2", "NSE", "PBIAS", "KGE", "NRMSE")
cal_mean <- data.frame(ID = tests, value = 0)
val_mean <- data.frame(ID = tests, value = 0)

remove_res <- c(138, 143, 146)

cal_mean[1, 2] <- mean(cal_results$complete$r2[,2][!(cal_results$complete$r2$ID %in% remove_res)])
cal_mean[2, 2] <- mean(cal_results$complete$NSE[,2][!(cal_results$complete$NSE$ID %in% remove_res)])
cal_mean[3, 2] <- mean(cal_results$complete$PBIAS[,2][!(cal_results$complete$PBIAS$ID %in% remove_res)])
cal_mean[4, 2] <- mean(cal_results$complete$KGE[,2][!(cal_results$complete$KGE$ID %in% remove_res)])
cal_mean[5, 2] <- mean(cal_results$complete$NRMSE[,2][!(cal_results$complete$NRMSE$ID %in% remove_res)])

val_mean[1, 2] <- mean(val_results$complete$r2[,2][!(val_results$complete$r2$ID %in% remove_res)])
val_mean[2, 2] <- mean(val_results$complete$NSE[,2][!(val_results$complete$NSE$ID %in% remove_res)])
val_mean[3, 2] <- mean(val_results$complete$PBIAS[,2][!(val_results$complete$PBIAS$ID %in% remove_res)])
val_mean[4, 2] <- mean(val_results$complete$KGE[,2][!(val_results$complete$KGE$ID %in% remove_res)])
val_mean[5, 2] <- mean(val_results$complete$NRMSE[,2][!(val_results$complete$NRMSE$ID %in% remove_res)])

print(cal_mean)
print(val_mean)

#Similar performances!
#Ok like this








