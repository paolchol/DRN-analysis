

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

#Load the maximum capacities of the reservoirs
path_maxcap <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Input/Reservoir/reservoir.dat"
reservoirs <- read.table(path_maxcap, skip = 2, sep = "\t")
maxcap <- reservoirs[,c(1,5)]
names(maxcap) <- c('ID','max')
maxcap[,2] <- maxcap[,2]*1000 #m3


load('./Inputs/General/IDs.RData')
load('./Inputs/Calibration/df_observations.RData')
load('./Inputs/Calibration/df_uncalibrated.RData')

path <- './Data/Scenarios/Real/Model_output'
code <- 'final'
results <- WASA_calibration_evaluation(path, IDs, maxcap$max, obs_df,
                                       code = code, complete = TRUE, start_obs = TRUE,
                                       keep_mean = FALSE)

df <- data.frame(ID = results$complete$r2$ID,
                 r2 = results$complete$r2$result,
                 NSE = results$complete$NSE$unname.NSE_index.,
                 PBIAS = results$complete$PBIAS$PBIAS,
                 KGE = results$complete$KGE$unname.KGE_index.,
                 NRMSE = results$complete$NRMSE$NRMSE)

df[, 2:ncol(df)] <- round(df[, 2:ncol(df)], 2)

write.table(df, './Data/Plot_table/subb_performances.txt', quote = FALSE, row.names = FALSE)
