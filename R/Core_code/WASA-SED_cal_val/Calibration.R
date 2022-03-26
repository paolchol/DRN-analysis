#Model calibration
# "Active" script, to operate a semi-automatic model calibration
# The scaling factors of the subbasins can be changed through modify_scaling_factor,
# which will create a new file with the selected parameters
# 

# Setup -------------------------------------------------------------------

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis")
path_obs <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/Observed_volumes/volume_series_banabuiu_reservoirs_ID"
path_baseline <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Output_baseline_left_end"

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

# Load the observations ---------------------------------------------------

obs_input <- create_data_list(path_obs, c('date', 'volume'), skip = 1, f = read.csv, s = ",")
date <- create_date_vector(1980, 2018)
IDs <- data.frame(ID = gsub("\\..*","",basename(obs_input[[1]])))
for(i in seq_len(length(obs_input[[2]]))){
  obs_input[[2]][[i]]$date <- change_date_format(obs_input[[2]][[i]]$date)
}
obs_df <- create_main_dataframe(date, obs_input[[2]], IDs, timecol = 1, datacol = 2)
obs_df <- remove_high_values(obs_df, maxcap$max)
obs_df <- monthly_scale(obs_df, f = sumx)

# Load the baseline and its performance ---------------------------------------

date <- create_date_vector(1980, 2018)
base_df <- load_WASA_results(path_baseline, IDs, dates = date)
base_df <- remove_high_values(base_df, maxcap$max)
base_df <- monthly_scale(base_df, f = sumx)

path_results <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/Model_calibration"
m_results <- read.table(paste0(path,"/mean_results_baseline_correct.txt"), sep = "\t", header = TRUE)
rownames(m_results) <- c("R2", "NSE", "PBIAS", "KGE", "NRMSE")

#Load the results until now
# path <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/Model_calibration"
# m_results <- read.table(paste0(path,"/mean_results_calibration.txt"), sep = "\t", header = TRUE)

results <- list(mean = m_results)

# Calibration -------------------------------------------------------------

#Path for scaling_factor file and output
path_scaling_in <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/Model_calibration/Iterations/starting_scaling_factor.dat"
path_scaling_out <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Input/Others" 
path_WASA_output <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Output"

#Modify the scaling factor
subb <- c(123, 125, 126, 127, 144, 145, 146, 149, 150, 153, 154, 155, 156, #left branch
          142, 157) #right branch
val <- c(2, 2.5, 1, 0.2, 4, 2, 0.2, 2, 1.2, 1, 2.5, 1.2, 1.2, #left branch
         1.5, 0.5)  #right branch
modif <- data.frame(subb, val)
modif <- modif[order(modif$subb),]
n_subb <- c(142, 157) #Write the subbasins changed with respect to the baseline
n_val <- c(1.5, 0.5)
code <- gen_code(n_subb, n_val)
modify_scaling_factor(path_scaling_in, path_scaling_out, modif$subb, modif$val)

#From Visual Studio, launch the model
#approximately 1 minute

system2("C:/Thesis_fortran/WASA_Thesis/WASA_Thesis/x64/Release/WASA_Thesis.exe")

#Compute the evaluation of the results
results <- WASA_calibration_evaluation(path_WASA_output, IDs, maxcap$max, obs_df,
                                        code = code, mean_results = results$mean, complete = TRUE,
                                        start_obs = TRUE)
print(code)
View(results$mean)

#Save the results
path <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/Model_calibration"
write.table(results$mean, paste0(path, "/mean_results_calibration_2307.txt"), sep = "\t", quote = FALSE, row.names = TRUE)

# Visualize ---------------------------------------------------------------
#Visualize configurations with a better performance

#Statistical indicators for each subbasin
plot_stat_calibration(results$complete, code)
#Difference with the baseline
plot_diff(comp = TRUE, base = baseline_res$complete, it = results$complete)

#Load the iteration results
date <- create_date_vector(1980, 2018)
mod_df <- load_WASA_results(path_WASA_output, IDs, dates = date)
mod_df <- remove_high_values(mod_df, maxcap$max)
mod_df <- monthly_scale(mod_df, f = sumx)

#Plot comparison in selected basins
ID_plot <- subb
ID_plot <- 152
plot_calibration(mod_df, base_df, obs_df, ID_plot, code)

#Save the comparison plot for each subbasin
# path_plot <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/Model_calibration/plots/Runs"
# path_plot <- paste0(path_plot,"/", code)
# dir.create(path_plot)
# plot_comparison(mod_df, obs_df, y = "m3", label = "Volume comparison", 
#                  save = TRUE, path = path_plot, file = code)

# Visualize the differences with the baseline ----------------------------------

#Compute the performances of the baseline
baseline_res <- WASA_calibration_evaluation(path_baseline, IDs, maxcap$max, obs_df,
                                            code = "baseline", mean_results = m_results, complete = TRUE,
                                            start_obs = TRUE)
#Plot the performance
plot_stat_calibration(baseline_res$complete, 'baseline')
#Compute the difference with the performances of the iteration
diff_b <- diff_baseline(baseline_res$complete, results$complete)
plot_diff(diff_ = diff_b)

View(baseline_res$mean)

#Se diff < 0
#bene per r2
#bene per NSE
#bene per KGE
#male per PBIAS
#male per NRMSE


# Create a new baseline ---------------------------------------------------

#New baseline WASA files

#New baseline performance file
# write.table(results$mean, paste0(path_results, "/mean_results_baseline_correct.txt"), sep = "\t", quote = FALSE, row.names = TRUE)


""" # nolint
Save the codes

O = one changed
T = two changed
x1 = number of the repetition
y1 = value assigned

y1 = 0.8
y2 = 1.2
y3 = 2
y3.5 = 3
y4 = 4
y5 = 7

Most upstream reservoirs
Ox1y1 = 152, 0.8 - Worse result
Ox2y1 = 142, 0.8 - Worse result
Ox3y1 = 147, 0.8 - Worse result everywhere but better in KGE
Ox4y1 = 158, 0.8 - Worse result
Ox5y1 = 127, 0.8 - Worse result
Ox6y1 = 154, 0.8 - Worse result
Ox7y1 = 145, 0.8 - Worse result
Ox8y1 = 146, 0.8 - Worse but similar result
Ox9y1 = 126, 0.8 - Worse result
Ox10y1 = 125, 0.8 - Worse result
Ox11y1 = 138, 0.8 - Almost identical results, slightly worse

Ox12y2 = 138, 1.2 - Worse result
Ox13y2 = 154, 1.2 - Slight improvement
Ox14y3 = 154, 2 - Decise improvement
Ox15y4 = 154, 4 - PBIAS improvement but worse in every other indicator
Ox16y3.5 = 154, 3 - Worse result
Ox17y3 = 142, 2 - Worse result
Ox18y2 = 142, 1.2 - Worse result
Ox19y2 = 145, 1.2 - Better
Ox20y3 = 145, 2 - Decise improvement
Ox21y4 = 145, 4 - Worse

Tx22y3y3 = 154, 145, 2 - Decise improvement
"""
