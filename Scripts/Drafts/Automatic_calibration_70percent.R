#Automatic calibration on 70% of the dataset

# Setup -------------------------------------------------------------------

#Directory
setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")
#Path to save the intermediate results
path_save <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/Model_calibration/Automatic_calibration"

load("./Inputs/General/IDs.RData")
load('./Inputs/Calibration/df_observations.RData')
load('./Inputs/Calibration/df_uncalibrated.RData')

source("./Libraries/Libraries.R")
source("./Libraries/Functions.R")
source("./Libraries/Functions_TG.R")
source("./Libraries/Functions_DP.R")
source("./Libraries/Functions_CO.R")
source("./Libraries/Functions_MC.R")

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")

#Load the maximum capacities of the reservoirs
path_maxcap <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Input/Reservoir/reservoir.dat"
reservoirs <- read.table(path_maxcap, skip = 2, sep = "\t")
maxcap <- reservoirs[,c(1,5)]
names(maxcap) <- c('ID','max')
maxcap[,2] <- maxcap[,2]*1000 #m3

# Calibration -------------------------------------------------------------

#Modify the do.dat file
#tstart: 1980
#tstop: 2006

#Reservoirs IDs
#In 2006, 138 and 142 had not been built yet, so they are exluded
cal_IDs <- data.frame(ID = IDs$ID[!IDs$ID %in% c(138, 142)])

#Calibration observations
cal_obs <- obs_df[, !names(obs_df) %in% c(138, 142)]
cal_obs <- cal_obs[cal_obs$date <= as.Date("2006-12-01"), ]

#Calibration maxvalues
cal_maxval <- maxcap$max[maxcap$ID %in% cal_IDs$ID]

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
                 125)
                 #138) removed from the calibration scenario
right_branch <- c(142, 157,
                  158,
                  151,
                  152, 159, 160,
                  148,
                  147,
                  143, 156) #re-calibration of 156 after the results of the right branch
subbasins <- c(left_branch, right_branch)
no_res_sub <- c(134, 137, 139, 144, 155, 157, 158, 159,
                142) #142 is without reservoirs only in the calibration scenario

#Automatic calibration
sub_to_calibrate <- subbasins
par_range <- seq(0.2, 7, 0.2)

scaling_factors <- read.table("./Inputs/Calibration/uncalibrated_scaling_factor.dat", sep = "\t",
                              col.names = c('ID', 'val'), header = TRUE)
list_performance <- list()

tic('Automatic calibration on 70% of the dataset')
restart <- 1
for(i in restart:length(sub_to_calibrate)){
  sub <- sub_to_calibrate[i]
  performance <- data.frame(cal_IDs)
  for(j in 1:length(par_range)){
    #Modify the scaling factor file
    par <- par_range[j]
    scaling_factors$val[scaling_factors$ID == sub] <- par
    code <- gen_code(sub, par)
    modify_scaling_factor(path_scaling_in, path_scaling_out, scaling_factors$ID, scaling_factors$val)
    #Launch the model
    system2("C:/Thesis_fortran/WASA_Thesis/WASA_Thesis/x64/Release/WASA_Thesis.exe")
    #Evaluate the results
    results <- WASA_calibration_evaluation(path_WASA_output, IDs = cal_IDs, maxcap = cal_maxval, obs = cal_obs,
                                           code = code, complete = TRUE, start_obs = TRUE,
                                           keep_mean = FALSE, st_date = 1980, end_date = 2006)
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
  list_performance[[paste0("s", sub)]] <- performance
  write.table(performance, paste0(path_save, "/Performances/Single_change_perf/70_percent/",
                                  sub, "_KGEperformance.txt"), sep = "\t", quote = FALSE,
              row.names = FALSE)
  modify_scaling_factor(path_scaling_in, paste0(path_save, "/Scaling_factors/Iterations/70_percent/"),
                        scaling_factors$ID, scaling_factors$val,
                        name = paste0("automatic_scaling_factor_", sub))
}

#Save the scaling_factors
modify_scaling_factor(path_scaling_in, paste0(path_save, "/Scaling_factors/"), scaling_factors$ID, scaling_factors$val,
                      name = "70_percent_automatic")
#Save the performances
list.save(list_performance, paste0(paste0(path_save, "/Performances/"), "/list_performance_70percent.RData"))
toc()
















