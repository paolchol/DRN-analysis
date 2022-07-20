#Drought Cycle Analysis


# Script setup ------------------------------------------------------------

#Directory and paths
setwd("C:/Directory_thesis_codes")
path_maxcap <- "./Input/Model_input/Base/Reservoir/reservoir.dat"

source("./R/Libraries/Libraries.R")
source("./R/Libraries/Functions.R")
# source("./R/Libraries/Functions_TG.R")
# source("./R/Libraries/Functions_DP.R")
source("./R/Libraries/Functions_CO.R")
source("./R/Libraries/Functions_MC.R")
source("./R/Libraries/Functions_AN.R")

#Load the IDs of the subbasins in the Banabuiu region
load("./Data/Generated/General/IDs.RData")
subID <- read.table("./Data/Generated/General/reservoir_name_ID.txt", header = TRUE, sep = "\t")

# Analysis setup ----------------------------------------------------------

#Load precipitation data
prec <- read.table("./Input/Model_input/Base/Time_series/rain_daily.dat", skip = 3)
columns <- c("date", "doy", subID$SubbasinID)
names(prec) <- columns
prec$date <- change_date_WASA_input(prec$date)
prec$doy <- NULL
prec <- monthly_scale(prec, f = sumx)
#Save the resulting dataframe
# save(prec, file = "./Data/precipitation.RData")

#Load reservoirs' volumes
## Real scenario
load("./Data/Scenarios/Real/real_daily.RData")
## No HDNR scenario
load("./Data/Scenarios/No_HDNR/noH_daily.RData")
## Observations
load("./Data/volume_obs.RData")

#Load the reservoirs' maximum capacities
reservoirs <- read.table(path_maxcap, skip = 2, sep = "\t")
maxcap <- reservoirs[,c(1,5)]
names(maxcap) <- c('ID','max')
maxcap[,2] <- maxcap[,2]*1000 #m3

# Create a new "Banabuiu" column --------------------------------------------
#Generate a new column in precipitation's dataframe by summing all the columns
#Call it "Banabuiu"

prec$Banabuiu <- apply(prec[, 2:ncol(prec)], 1, function(x) sum(x))

# SPI - Standardized Precipitation Index ----------------------------------

library(SPEI)

list_SPI <- list()
list_SPI <- apply(prec[, 2:ncol(prec)], 2, function(x) spi(x, 12))

# VD - Volume Deficit -----------------------------------------------------
#Compute the VD on the daily volumes, then take the monthly mean
#Add Banabuiu VD by taking the mean of all VD available (mean, nar.rm = TRUE)

VD_real <- VD(real_df[, 1:ncol(real_df)], maxcap$max)
VD_real <- monthly_scale(VD_real, f = meanx)
VD_real$Banabuiu <- apply(VD_real[, 2:ncol(VD_real)], 1, FUN = meanx)

VD_noH <- VD(noH_df[, 1:ncol(noH_df)], maxcap$max)
VD_noH <- monthly_scale(VD_noH, f = meanx)
VD_noH$Banabuiu <- apply(VD_noH[, 2:ncol(VD_noH)], 1, FUN = meanx)

VD_obs <- VD(obs_df[, 1:ncol(obs_df)], maxcap$max)
VD_obs <- monthly_scale(VD_obs, f = meanx)
VD_obs$Banabuiu <- apply(VD_obs[, 2:ncol(VD_obs)], 1, FUN = meanx)

# Quadrant attribution ----------------------------------------------------

DCA_real <- list()
DCA_noH <- list()
DCA_obs <- list()
obj_names <- names(VD_real)[2:ncol(VD_real)]

for(i in 1:length(obj_names)){
  name <- obj_names[i]
  DCA_real[[obj_names[i]]] <- quadrant_attribution(list_SPI[[name]][["fitted"]], VD_real[name], VD_real["date"])
  DCA_noH[[obj_names[i]]] <- quadrant_attribution(list_SPI[[name]][["fitted"]], VD_noH[name], VD_noH["date"])
  DCA_obs[[obj_names[i]]] <- quadrant_attribution(list_SPI[[name]][["fitted"]], VD_obs[name], VD_obs["date"])
}

# Save the results --------------------------------------------------------

list.save(DCA_real, "./Data/DCA/DCA_real2.RData")
list.save(DCA_noH, "./Data/DCA/DCA_noH2.RData")
list.save(DCA_obs, "./Data/DCA/DCA_obs2.RData")

