#Model calibration script
# In this script, the evaluation of the model results will be performed

# Definition of the evaluation indicators of the model's performance --------
#The evaluation will be done on a monthly time scale

## R2 (Coefficient of determination)

## NSE (Nash-Sutcliffe efficiency)
#N.B.: Calculated on the discharge. Ask for this data
#In the meantime use it on the volume data as well
#NSE_index wil be a vector containing the NSE for each column

## PBIAS (% of bias)
#PBIAS wil be a vector containing the NSE for each column

## KGE (Kling-Gupta Efficiency)
#N.B.: same as NSE
#KGE_index wil be a vector containing the NSE for each column

## RMSE and NRMSE

# Setup -------------------------------------------------------------------

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis")
path_obs <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/Observed_volumes/volume_series_banabuiu_reservoirs_ID"

source("Libraries.R")
source("Functions.R")
source("Functions_TG.R")
source("Functions_DP.R")
source("Functions_CO.R")
source("Functions_MC.R")

#Load the maximum capacities of the reservoirs
path_maxcap <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Input/Reservoir/reservoir.dat"
reservoirs <- read.table(path_maxcap, skip = 2, sep = "\t")
maxcap <- reservoirs[,c(1,5)]
names(maxcap) <- c('ID','max')
maxcap[,2] <- maxcap[,2]*1000 #m3

# Load the observations and place them in a dataframe ---------------------

obs_input <- create_data_list(path_obs, c('date', 'volume'), skip = 1, f = read.csv, s = ",")
date <- create_date_vector(1980, 2018)
IDs <- data.frame(ID = gsub("\\..*","",basename(obs_input[[1]])))
for(i in 1:length(obs_input[[2]])){
  obs_input[[2]][[i]]$date <- change_date_format(obs_input[[2]][[i]]$date)
}
obs_df <- create_main_dataframe(date, obs_input[[2]], IDs, timecol = 1, datacol = 2)
#Remove values higher than maximum capacity
obs_df <- remove_high_values(obs_df, maxcap$max)

#Plot the observations
# path_plot <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/Model_calibration"
# plot_subbasins_df(obs_df, y = "m3", label = "Observations - Daily",
#    interactive = TRUE, file = "obs_d_removed", path = path_plot, doy = FALSE, line = TRUE)

#Return the data on a monthly scale
obs_df <- monthly_scale(obs_df, f = sumx)

#Plot the observations
# path_plot <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/Model_calibration"
# plot_subbasins_df(obs_df, y = "m3", label = "Observations - Monthly",
#   interactive = TRUE, file = "obs_m_new", path = path_plot, doy = FALSE, line = TRUE)

# Baseline definition -----------------------------------------------------

# First run of the model
#No intake.dat file considered (file containing the withdrawal time series from
#the reservoirs), because this data can introduce uncertainty

#Load the resulting reservoirs' volumes
path_WASA_out <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Output_First_Run"
date <- create_date_vector(1980, 2018)
mod_df <- load_WASA_results(path_WASA_out, IDs, dates = date)
#Remove values higher than maximum capacity
mod_df <- remove_high_values(mod_df, maxcap$max)
#Rescale the value in monthly scale
mod_df <- monthly_scale(mod_df, f = sumx)

#Compute the evaluation indicators
r2_FR <- r2_computation(mod_df, obs_df)
NSE_index_FR <- NSE(mod_df[,2:ncol(mod_df)], obs_df[,2:ncol(obs_df)])
PBIAS_FR <- pbias(mod_df[,2:ncol(mod_df)], obs_df[,2:ncol(obs_df)])
KGE_index_FR <- KGE(mod_df[,2:ncol(mod_df)], obs_df[,2:ncol(obs_df)])
R_NRMSE_indexes_FR <- RMSE_NRMSE(mod_df, obs_df)

#Visualize and save the graphs
# path_plot <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/Model_calibration/First_run/plots"
# plot_comparison(mod_df, obs_df, y = "m3", label = "Volume comparison", 
#                 save = TRUE, path = path_plot, file = "volume")

# Second run of the model
#The intake.dat file is introduced

#Load the resulting reservoirs' volumes
path_WASA_out <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Output_Second_Run"
date <- create_date_vector(1980, 2018)
mod_df <- load_WASA_results(path_WASA_out, IDs, dates = date)
#Remove values higher than maximum capacity
mod_df <- remove_high_values(mod_df, maxcap$max)
#Rescale the value in monthly scale
mod_df <- monthly_scale(mod_df, f = sumx)

#Compute the evaluation indicators
r2_SR <- r2_computation(mod_df, obs_df)
NSE_index_SR <- NSE(mod_df[,2:ncol(mod_df)], obs_df[,2:ncol(obs_df)])
PBIAS_SR <- pbias(mod_df[,2:ncol(mod_df)], obs_df[,2:ncol(obs_df)])
KGE_index_SR <- KGE(mod_df[,2:ncol(mod_df)], obs_df[,2:ncol(obs_df)])
R_NRMSE_indexes_SR <- RMSE_NRMSE(mod_df, obs_df)

#Visualize and save the graphs
# path_plot <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/Model_calibration/Second_run/plots_2ndtry"
# plot_comparison(mod_df, obs_df, y = "m3", label = "Volume comparison", 
#                 save = TRUE, path = path_plot, file = "volume")

# Comparison of the first two runs ----------------------------------------
#From the comparison, the second run is defined as the baseline, thus including
#the release observations inside the model inputs

#Mean comparison
mean_comp <- data.frame(first_run = rep(0,5), second_run = rep(0,5))
rownames(mean_comp) <- c("R2", "NSE", "PBIAS", "KGE", "NRMSE")
mean_comp$first_run[1] <- mean(r2_FR$result)
mean_comp$second_run[1] <- mean(r2_SR$result)
mean_comp$first_run[2] <- mean(NSE_index_FR)
mean_comp$second_run[2] <- mean(NSE_index_SR)
mean_comp$first_run[3] <- mean(PBIAS_FR)
mean_comp$second_run[3] <- mean(PBIAS_SR)
mean_comp$first_run[4] <- mean(KGE_index_FR)
mean_comp$second_run[4] <- mean(KGE_index_SR)
mean_comp$first_run[5] <- mean(R_NRMSE_indexes_FR$NRMSE)
mean_comp$second_run[5] <- mean(R_NRMSE_indexes_SR$NRMSE)

format(mean_comp, scientific = F)

#Save the results
path <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/Model_calibration"
write.table(mean_comp, paste0(path, "/mean_results_baseline.txt"), sep = "\t", quote = FALSE, row.names = FALSE)

#Plot the results
path_plot <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/Model_calibration/plots_stat_comp2"

plot_comp_FR_SR(r2_FR, r2_SR, interactive = TRUE, file = "R2", path = path_plot,
                label = "R2 - First and Second run", named = FALSE, colID = 1, colval = 2)
plot_comp_FR_SR(NSE_index_FR, NSE_index_SR, interactive = TRUE, file = "NSE", path = path_plot,
                label = "Nash Sutcliffe Efficiency - First and Second run")
names(PBIAS_FR) = names(PBIAS_SR) <- names(NSE_index_FR)
plot_comp_FR_SR(PBIAS_FR, PBIAS_SR, interactive = TRUE, file = "PBIAS", path = path_plot,
                label = "Percentage of Bias - First and Second run")
plot_comp_FR_SR(KGE_index_FR, KGE_index_SR, interactive = TRUE, file = "KGE", path = path_plot,
                label = "Kling-Gupta Efficiency - First and Second run")
plot_comp_FR_SR(R_NRMSE_indexes_FR, R_NRMSE_indexes_SR, interactive = TRUE, file = "NRMSE", path = path_plot,
                label = "NRMSE - First and Second run", named = FALSE, colID = 1, colval = 3)

#Mean results
require(reshape2)
mean_comp$test <- rownames(mean_comp)
df<-melt(mean_comp, id.vars = 'test', variable.name = 'Run')
p <- ggplot(df, aes(test, value)) + geom_point(aes(colour = Run), alpha = 0.5, size = 6) +
  xlab("Test") + ylab("Mean result") + ggtitle("Mean results of the first and second run")
htmlwidgets::saveWidget(plotly::ggplotly(p), paste0(path_plot, "/mean_res.html"))


