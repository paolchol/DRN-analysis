# Scenarios generation
#Real scenario: scenario including the HDNR
#No HDNR scenario: scenario withouth the HDNR

# Setup the script ------------------------------------------------------------

#Directory and paths
setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")
path_scaling_in <- "./Inputs/Calibration/uncalibrated_scaling_factor.dat"
path_scaling_out <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Input/Others" 
path_WASA_input <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Input"
path_WASA_output <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Output"
path_scenario_data <- "./Data/Scenarios"

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
load('./Inputs/General/IDs.RData')

# Setup the model ---------------------------------------------------------

#Use the scaling factors resulting from the calibration
sf_load <- "./Inputs/Calibration/calibrated_scaling_factor.dat"
scaling_factors <- read.table(sf_load, sep = "\t", header = TRUE)
modify_scaling_factor(path_scaling_in, path_scaling_out, scaling_factors[,1], scaling_factors[,2])

# Real scenario generation ------------------------------------------------

#Move lake_number.dat
file.copy(from = paste0(path_scenario_data, "/Real/lake_number.dat"),
          to = paste0(path_WASA_input, "/Reservoir"), overwrite = TRUE,
          recursive = FALSE, copy.mode = TRUE)
#Launch the model
system2("C:/Thesis_fortran/WASA_Thesis/WASA_Thesis/x64/Release/WASA_Thesis.exe")
#Load the results
path_real_sc <- "./Data/Scenarios/Real/Model_output"
date <- create_date_vector(1980, 2018)
real_df <- load_WASA_results(path_real_sc, IDs, dates = date)
real_df <- remove_high_values(real_df, maxcap$max)
#Save the daily dataframe
save(real_df, file = './Data/Scenarios/Real/real_daily.RData')
real_df <- monthly_scale(real_df, f = sumx)
#Save the monthly dataframe
save(real_df, file = './Data/Scenarios/Real/real_volumes.RData')

# No HDNR scenario generation ---------------------------------------------

#Move lake_number.dat
file.copy(from = paste0(path_scenario_data, "/No_HDNR/lake_number.dat"),
          to = paste0(path_WASA_input, "/Reservoir"), overwrite = TRUE,
          recursive = FALSE, copy.mode = TRUE)
#Launch the model
system2("C:/Thesis_fortran/WASA_Thesis/WASA_Thesis/x64/Release/WASA_Thesis.exe")
#Load the results
path_noH_sc <- "./Data/Scenarios/No_HDNR/Model_output"
date <- create_date_vector(1980, 2018)
noH_df <- load_WASA_results(path_noH_sc, IDs, dates = date)
noH_df <- remove_high_values(noH_df, maxcap$max)
#Save the daily dataframe
save(noH_df, file = './Data/Scenarios/No_HDNR/noH_daily.RData')
noH_df <- monthly_scale(noH_df, f = sumx)
#Save the dataframe
save(noH_df, file = './Data/Scenarios/No_HDNR/nohdnr_volumes.RData')


# Check the differences ----------------------------------------------------

df <- data.frame(date = real_df$date, real = real_df["123"], noH = noH_df["123"])
names(df) <- c("date", "real", "noH")
plot_df_interactive(df)





