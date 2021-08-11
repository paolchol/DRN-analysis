#Automatic calibration

# Setup -------------------------------------------------------------------

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

#Load the maximum capacities of the reservoirs
path_maxcap <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Input/Reservoir/reservoir.dat"
reservoirs <- read.table(path_maxcap, skip = 2, sep = "\t")
maxcap <- reservoirs[,c(1,5)]
names(maxcap) <- c('ID','max')
maxcap[,2] <- maxcap[,2]*1000 #m3

# Load the observations ---------------------------------------------------

#From scratch
obs_input <- create_data_list(path_obs, c('date', 'volume'), skip = 1, f = read.csv, s = ",")
date <- create_date_vector(1980, 2018)
IDs <- data.frame(ID = gsub("\\..*","",basename(obs_input[[1]])))
for(i in 1:length(obs_input[[2]])){
  obs_input[[2]][[i]]$date <- change_date_format(obs_input[[2]][[i]]$date)
}
obs_df <- create_main_dataframe(date, obs_input[[2]], IDs, timecol = 1, datacol = 2)
obs_df <- remove_high_values(obs_df, maxcap$max)
obs_df <- monthly_scale(obs_df, f = sumx)

#From the saved df
load('./Inputs/Calibration/df_observations.RData')

# Load the baseline -------------------------------------------------------
#The baseline is the un-calibrated model after adding the release observations

#From scratch
date <- create_date_vector(1980, 2018)
base_df <- load_WASA_results(path_baseline, IDs, dates = date)
base_df <- remove_high_values(base_df, maxcap$max)
base_df <- monthly_scale(base_df, f = sumx)

#From the saved df
load('./Inputs/Calibration/df_uncalibrated.RData')

# Calibration -------------------------------------------------------------

#Path for scaling_factor file and output
path_scaling_in <- "./Inputs/Calibration/uncalibrated_scaling_factor.dat"
path_scaling_out <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Input/Others" 
path_WASA_output <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Output"

#Definition of the calibration order
left_branch <- c(154, 150, 145, 155,
                 153, 149, 144,
                 127, 123,
                 146, 156,
                 126,
                 125,
                 138)
right_branch <- c(142, 157,
                  158,
                  151,
                  152, 159, 160,
                  148,
                  147,
                  143, 156) #re-calibration of 156 after the results of the right branch
subbasins <- c(left_branch, right_branch)
no_res_sub <- c(134, 137, 139, 144, 155, 157, 158, 159)

#Automatic calibration
sub_to_calibrate <- 125 #right_branch
par_range <- seq(5, 10, 0.2) #seq(0.2, 7, 0.2)

#First launch, on the left branch
# scaling_factors <- data.frame(IDs, val = 1)
#Second launch, on the right branch, using the results on the left
scaling_factors <- read.table("./Inputs/Calibration/automatic_scaling_factor_v1_left.dat", sep = "\t",
                             col.names = c('ID', 'val'), header = TRUE)
list_performance <- list()

tic('Right branch - Automatic calibration')
restart <- 1
for(i in restart:length(sub_to_calibrate)){
  sub <- sub_to_calibrate[i]
  performance <- data.frame(IDs)
  for(j in 1:length(par_range)){
    #Modify the scaling factor file
    par <- par_range[j]
    scaling_factors$val[scaling_factors$ID == sub] <- par
    code <- gen_code(sub, par)
    modify_scaling_factor(path_scaling_in, path_scaling_out, scaling_factors$ID, scaling_factors$val)
    #Launch the model
    system2("C:/Thesis_fortran/WASA_Thesis/WASA_Thesis/x64/Release/WASA_Thesis.exe")
    #Evaluate the results
    results <- WASA_calibration_evaluation(path_WASA_output, IDs, maxcap$max, obs_df,
                                           code = code, complete = TRUE, start_obs = TRUE,
                                           keep_mean = FALSE)
    performance$new <- results$complete$KGE$unname.KGE_index.
    names(performance)[names(performance) == "new"] <- code
  }
  
  #Get the best configuration
  if(sub %in% no_res_sub) k <- which(performance$ID == choose_sub(sub))
  else k <- which(performance$ID == sub)
  pos <- which(performance[k, ] == max(performance[k, 2:ncol(performance)])) - 1
  par_max <- par_range[pos]
  
  #Modify the overall scaling_factors
  scaling_factors$val[scaling_factors$ID == sub] <- par_max
  
  #Save the performance
  # list_performance[[paste0("s", sub)]] <- performance
  # write.table(performance, paste0(path_save, "/Performances/Single_change_perf/",
  #             sub, "_KGEperformance.txt"), sep = "\t", quote = FALSE,
  #             row.names = FALSE)
  # modify_scaling_factor(path_scaling_in, paste0(path_save, "/Scaling_factors/Iterations/"),
  #                       scaling_factors$ID, scaling_factors$val,
  #                       name = paste0("automatic_scaling_factor_", sub))
}
toc()

#Save the scaling_factors
modify_scaling_factor(path_scaling_in, paste0(path_save, "/Scaling_factors/"), scaling_factors$ID, scaling_factors$val,
                      name = "automatic_scaling_factor_right_branch")
#Save the performances
list.save(list_performance, paste0(paste0(path_save, "/Performances/"), "/list_performance_right.RData"))
toc()
#Left branch - Automatic calibration - Restart from 4: 9 hours


#Load the obtained scaling factors
# scaling_factors <- read.table(paste0(path_save, "/automatic_scaling_factor_left_branch.dat"), sep = "\t",
#                              col.names = c('ID', 'val'), header = TRUE)
#Use this one to start the right branch calibration!
#Load all the performances


# Check one configuration -------------------------------------------------

#Define the configuration
# subb <- 154
# val <- 2.4
#or load the best configuration obtained
sf_load <- "./Inputs/Calibration/calibrated_scaling_factor.dat"
scaling_factor <- read.table(sf_load, sep = "\t", header = TRUE)
subb <- scaling_factor[,1]; val <- scaling_factor[,2]
code <- gen_code(subb, val)
modify_scaling_factor(path_scaling_in, path_scaling_out, subb, val)
system2("C:/Thesis_fortran/WASA_Thesis/WASA_Thesis/x64/Release/WASA_Thesis.exe")
results <- WASA_calibration_evaluation(path_WASA_output, IDs, maxcap$max, obs_df,
                                       code = code, complete = TRUE, start_obs = TRUE,
                                       keep_mean = FALSE)

#Statistical indicators
plot_stat_calibration(results$complete, code)

#Save the plot
# plot_stat_calibration(results$complete, 'End of the calibration', save = TRUE, path = path_performance, file = 'results')

#Plot comparison in the selected basin
#Load the output
date <- create_date_vector(1980, 2018)
mod_df <- load_WASA_results(path_WASA_output, IDs, dates = date)
mod_df <- remove_high_values(mod_df, maxcap$max)
mod_df <- monthly_scale(mod_df, f = sumx)
#Write the subbasins to visualize
ID_plot <- 156
plot_calibration(mod_df, base_df, obs_df, ID_plot, code)


# Mean results ------------------------------------------------------------

remove_res <- c(138, 143, 146)
mean(results$complete$r2[,2][!(results$complete$r2$ID %in% remove_res)])
mean(results$complete$NSE[,2][!(results$complete$NSE$ID %in% remove_res)])
mean(results$complete$PBIAS[,2][!(results$complete$PBIAS$ID %in% remove_res)])
mean(results$complete$KGE[,2][!(results$complete$KGE$ID %in% remove_res)])
mean(results$complete$NRMSE[,2][!(results$complete$NRMSE$ID %in% remove_res)])

# Notes -------------------------------------------------------------------

# 156 and 125 reach the highest parameter in the range
# After finishing the calibration like this also for the right branch,
# try changing their parameters to higher values (e.g. 7, 8, 9)

