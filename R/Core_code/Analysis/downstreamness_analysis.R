#Downsreamness analysis computation

# Setup -------------------------------------------------------------------

setwd("C:/Directory_thesis_codes")

source("./R/Libraries/Libraries.R")
source("./R/Libraries/Functions.R")
source("./R/Libraries/Functions_MC.R")

get_mean_volume = function(df, df_class, df_vol, c){
  ss <- names(df_class)[2:ncol(df_class)]
  for(i in seq_len(length(ss))){
    s <- ss[i]
    n <- sum(df$class == c & df$SubbasinID == as.numeric(s))
    id_select <- df$id[which(df$class == c & df$SubbasinID == as.numeric(s))]
    
    vol <- df_class[s]/n
    df_vol[, names(df_vol) %in% id_select] <- vol
  }
  return(df_vol)
}

# Load the reservoirs shapefiles ----------------------------------------------

HDNR <- readOGR("./Data/DRN/HDNR.shp")
res <- readOGR("./Data/Shapefile/Reservoirs/centralized_res.shp")

#Create dfs from the shapefiles attribute tables
HDNR_df <- HDNR@data
res_df <- res@data

#Remove the shapefiles (not needed anymore)
remove(HDNR)
remove(res)

# Load the model output ---------------------------------------------------
#Needed to compute the monthly Dsv

#Centralized
#Real scenario
load("./Data/Scenarios/AR/real_volumes.RData")
#NoH scenario
load("./Data/Scenarios/LR/nohdnr_volumes.RData")

#HDNR
#Real scenario
load("./Data/Scenarios/AR/class1.RData")
load("./Data/Scenarios/AR/class2.RData")
load("./Data/Scenarios/AR/class3.RData")
load("./Data/Scenarios/AR/class4.RData")
load("./Data/Scenarios/AR/class5.RData")

# Dsc ---------------------------------------------------------------------
#Attention: this result doesn't use any simulated result, only observational data

res_df$year[order(res_df$name)] <- c(1966, 1988, 1906, 1992, 2007, 1996, 1997, 1998, 1988, 1978,
                                     2000, 1956, 1966, 1988, 1992, 1995, 1992, 2011, 1988)
res_df$numC <- res_df$capacity * res_df$dx

#Real scenario
#Assuming same number of small reservoirs across the years
#Strong assumption, a decrease coefficient could be applied
#Anyway, here are considered 7.000 less reservoirs than the ones identified by FUNCEME
#since there weren't connections to JRC water bodies, so the results are still sound
numH <- HDNR_df$capacity * HDNR_df$dx

date <- seq(1980, 2018, 1)
Dsc_r <- data.frame(date = date, Dsc = 0)
for(i in seq_len(nrow(Dsc))){
  rescapdx <- sum(res_df$numC[res_df$year <= Dsc$date[i]])
  rescap <- sum(res_df$capacity[res_df$year <= Dsc$date[i]])
  Dsc_r$Dsc[i] <- (sum(numH) + sum(rescapdx))/(sum(HDNR_df$capacity) + sum(rescap))
}
plot(Dsc_r)

#NoH scenario
Dsc_nH <- data.frame(date = date, Dsc = 0)
for(i in seq_len(nrow(Dsc_nH))){
  rescapdx <- sum(res_df$numC[res_df$year <= Dsc$date[i]])
  rescap <- sum(res_df$capacity[res_df$year <= Dsc$date[i]])
  Dsc_nH$Dsc[i] <- rescapdx/rescap
}
plot(Dsc_nH)

save(Dsc_r, file = "./Data/Downstreamness/Dsc_r.Rdata")
save(Dsc_nH, file = "./Data/Downstreamness/Dsc_nH.Rdata")

# Dsv ---------------------------------------------------------------------

#Real scenario
#Create the df containing each small reservoir's volume
HDNR_vol <- data.frame(matrix(NA, nrow(class1), nrow(HDNR_df)))
names(HDNR_vol) <- HDNR_df$id
HDNR_vol$date <- class1$date
col_order <- c("date", HDNR_df$id)
HDNR_vol <- HDNR_vol[, col_order]

#Include here the code used to produce the classes (supporting_code/lake_volume_handling)

HDNR_vol <- get_mean_volume(HDNR_df, class1, HDNR_vol, 1)
HDNR_vol <- get_mean_volume(HDNR_df, class2, HDNR_vol, 2)
HDNR_vol <- get_mean_volume(HDNR_df, class3, HDNR_vol, 3)
HDNR_vol <- get_mean_volume(HDNR_df, class4, HDNR_vol, 4)
HDNR_vol <- get_mean_volume(HDNR_df, class5, HDNR_vol, 5)

save(HDNR_vol, file = "./Data/Analysis/Downstreamness/DRN_AR_vol_sum.Rdata")

#Compute the Dsv
dx_hdnr <- HDNR_df$dx
dx_res <- res_df$dx[order(res_df$subID)]

mean(HDNR_df$dx)

tic("Dsv AR computation")
Dsv_r <- data.frame(date = real_df$date)
Dsv_r$Dsv <- 0
for(i in seq_len(nrow(Dsv_r))){
  num <- sum(HDNR_vol[i, 2:ncol(HDNR_vol)]*dx_hdnr) + sum(real_df[i, 2:ncol(real_df)]*dx_res)
  den <- sum(HDNR_vol[i, 2:ncol(HDNR_vol)]) + sum(real_df[i, 2:ncol(real_df)])
  Dsv_r$Dsv[i] <- num/den
}

Dsv_r$noH <- 0
for(i in seq_len(nrow(Dsv_r))){
  num <- sum(real_df[i, 2:ncol(real_df)]*dx_res)
  den <- sum(real_df[i, 2:ncol(real_df)])
  Dsv_r$noH[i] <- num/den
}
toc()
plot_df_interactive(Dsv_r)

#LR scenario
tic("Dsv LR scenario computation")
Dsv_nH <- data.frame(date = real_df$date)
Dsv_nH$Dsv <- 0
for(i in seq_len(nrow(Dsv_nH))){
  num <- sum(noH_df[i, 2:ncol(noH_df)] * dx_res)
  den <- sum(noH_df[i, 2:ncol(noH_df)])
  Dsv_nH$Dsv[i] <- num / den
}
toc()
plot_df_interactive(Dsv_nH)

mean(Dsv_r$Dsv)
mean(Dsv_nH$Dsv)

Dsv_Alt <- Dsv_r
save(Dsv_Alt, file = "./Data/Analysis/Downstreamness/Dsv_Alt.Rdata")


#Save
save(Dsv_r, file = "./Data/Downstreamness/Dsv_r_v2.Rdata")
save(Dsv_nH, file = "./Data/Downstreamness/Dsv_nH.Rdata")

# Notes -------------------------------------------------------------------

#The downstreamness of the small reservoirs is really low, this reduces the
#product between the volume and the dx
#Anyway, the downstreamness in the noH configuration is higher than the real
#case, showing that even if the relative weight is small, the network provides
#for a less downstream distribution