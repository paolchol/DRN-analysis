

# Setup -------------------------------------------------------------------

setwd("C:/Users/paolo/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")

source("./Libraries/Libraries.R")
source("./Libraries/Functions.R")
source("./Libraries/Functions_TG.R")
source("./Libraries/Functions_DP.R")
source("./Libraries/Functions_CO.R")
source("./Libraries/Functions_MC.R")

#Load the maximum capacities of the reservoirs
path_maxcap <- "./Inputs/Model_input/Directory_WASA_Banabuiu/Input/Reservoir/reservoir.dat"
reservoirs <- read.table(path_maxcap, skip = 2, sep = "\t")
maxcap <- reservoirs[,c(1,5)]
names(maxcap) <- c('ID','max')
maxcap[,2] <- maxcap[,2]*1000 #m3

#Load the reservoirs' IDs
load("./Inputs/General/IDs.RData")

#Load the observations and the scenarios
load("./Inputs/Calibration/df_observations.RData") #name: obs_df
load("./Data/Scenarios/No_HDNR/nohdnr_volumes.RData") #name: noH_df
load("./Data/Scenarios/Real/real_volumes.RData") #name: real_df

#Names of the WASA outputs columns
columns<-c("Subasin-ID", "year", "day", "hour", "qlateral",
           "transposition", "inflow", "evap",
           "prec", "intake", "overflow",
           "qbottom", "qout", "withdrawal", "elevation", "area", "volume")



# Compare noHDNR ----------------------------------------------------------
#Compare the run with the intake time series with the run without
#the intake time series

path <- "./Data/Scenarios/No_HDNR/Model_output_nointake"
date <- create_date_vector(1980, 2018)

filepath <- paste0(path, "/res_156_watbal.out")
ALvol <- read.table(filepath, skip = 3, header = FALSE)
names(ALvol) <- columns
ALvol <- date_col_WASA_out(ALvol)
listt <- list(ALvol)
ALvol <- create_main_dataframe(date, listt, IDs, timecol = 18, datacol = 17)
ALvol <- monthly_scale(df, f = sumx)

dd2 <- data.frame(date = noH_df$date, intake = noH_df[["156"]], noint = ALvol[,2], real = real_df[["156"]], obs = obs_df[["156"]])
plot_df_interactive(dd2)

#Keep the noHDNR scenario with the intakes, the one without the intake has a lower volume
#than the Real scenario

# New scenarios -----------------------------------------------------------

AL_extract = function(path, columns, IDs){
  filepath <- paste0(path, "/res_156_watbal.out")
  date <- create_date_vector(1980, 2018)
  
  ALvol <- read.table(filepath, skip = 3, header = FALSE)
  names(ALvol) <- columns
  ALvol <- date_col_WASA_out(ALvol)
  listt <- list(ALvol)
  ALvol <- create_main_dataframe(date, listt, IDs, timecol = 18, datacol = 17)
  ALvol <- monthly_scale(ALvol, f = sumx)
  names(ALvol) <- c("date", "v156")
  return(ALvol)
}

#Load AL volume for the two new scenarios
pathAL_only <- "./Data/Scenarios/AL_only/Model_output"
pathAL_HDNR <- "./Data/Scenarios/AL_HDNR/Model_output"

AL_only <- AL_extract(pathAL_only, columns, IDs)
AL_HDNR <- AL_extract(pathAL_HDNR, columns, IDs)

#Check the differences on these two new scenarios
df <- data.frame(date = AL_only$date, alone = AL_only$v156, wHDNR = AL_HDNR$v156)
plot_df_interactive(df)
#The volume with the HDNR is lower than the one without

#Add the volume in the Real and noHDNR scenarios
df2 <- data.frame(date = AL_only$date,
                  alone = AL_only$v156,
                  wHDNR = AL_HDNR$v156,
                  Real = real_df[["156"]],
                  noHDNR = noH_df[["156"]],
                  obs = obs_df[["156"]])
plot_df_interactive(df2)
#The two new scenarios have a lower volume than Real and noHDNR after
#2010

#> run again the two new scenarios with the new intake file (done only for 156)
#> see if the new time series keep the same behavior (higher than Real before 2010) and
#they become higher than Real also after 2010

#If they don't, keep these two runs, and justify the lower volume in 2010
#based on the fact that the model was not provided with a release time series, so
#it had to extract it

#Runs with intake
pathAL_only_i <- "./Data/Scenarios/AL_only/Model_output_intake"
pathAL_HDNR_i <- "./Data/Scenarios/AL_HDNR/Model_output_intake"
AL_only_int <- AL_extract(pathAL_only_i, columns, IDs)
AL_HDNR_int <- AL_extract(pathAL_HDNR_i, columns, IDs)


#Add the volume in the Real and noHDNR scenarios
df2 <- data.frame(date = AL_only$date,
                  alone = AL_only$v156,
                  wHDNR = AL_HDNR$v156,
                  alone_i = AL_only_int$v156,
                  wHDNR_i = AL_HDNR_int$v156,
                  Real = real_df[["156"]],
                  noHDNR = noH_df[["156"]],
                  obs = obs_df[["156"]])
plot_df_interactive(df2)

#Use the output of the runs using the intake time series
#Because the dynamic is the same, the only thing that changes is the
#magnitude, but after 2010 it is correctly higher than the Real and noHDNR scenarios



