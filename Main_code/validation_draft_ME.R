#Validation script


# Setup -------------------------------------------------------------------

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")

source("./Libraries/Libraries.R")
source("./Libraries/Functions.R")
source("./Libraries/Functions_TG.R")
source("./Libraries/Functions_DP.R")
source("./Libraries/Functions_CO.R")
source("./Libraries/Functions_MC.R")


# Load the actual dataset -------------------------------------------------

#Weather data
prec <- read.table("./Inputs/Model_input/Base/Time_series/rain_daily.dat"), skip = 3)
temp <- read.table("./Inputs/Model_input/Base/Time_series/temperature.dat"), skip = 3)
hum <- read.table("./Inputs/Model_input/Base/Time_series/humidity.dat"), skip = 3)
rad <- <- read.table("./Inputs/Model_input/Base/Time_series/radiation.dat"), skip = 3)

columns <- c("date", "doy", "ID123", "ID125", "ID126", "ID127", "ID134", "ID137", "ID138", "ID139", "ID142", "ID143", "ID144", "ID145", "ID146", "ID147", "ID148", "ID149", "ID150", "ID151", "ID152", "ID153", "ID154", "ID155", "ID156", "ID157", "ID158", "ID159", "ID160")
names(prec) <- columns
names

#Intake


#Rainy season














