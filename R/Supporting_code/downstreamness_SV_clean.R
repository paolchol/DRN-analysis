setwd("C:/Directory_thesis_codes")
source("./R/Libraries/Libraries.R")
source("./R/Libraries/Functions.R")
source("./R/Libraries/Functions_MC.R")

downstreamness_SV = function(SV, dx, SV2 = 0, dx2 = 0, coeff = 0){
  num <- sum(SV*dx, na.rm = T) + sum(SV2*dx2*coeff, na.rm = T)
  den <- sum(SV, na.rm = T) + sum(SV2*coeff, na.rm = T)
  return(num/den)
}

perc_increment = function(xi, xf){
  return((xf-xi)/xi * 100)
}

# Obtain needed values ----------------------------------------------------

#SVperc
lake_maxvol <- read.table("./Data/Scenarios/AR/lake_maxvol.dat", skip = 2)
lake_number <- read.table("./Data/Scenarios/AR/lake_number.dat", skip = 2)
sbs <- lake_number[, 1]

totcap <- 0
for (class in 1:5){
  for (sb in sbs){
    idx1 <- lake_number[, 1] == sb
    idx2 <- lake_maxvol[, 1] == sb
    totcap <- totcap + lake_number[idx1, class]*lake_maxvol[idx2, class]
  }
}

load("./Data/Analysis/Downstreamness/DRN_AR_vol_mean.Rdata")
DRN_AR_vol <- apply(DRN_AR_vol_mean[, 2:ncol(DRN_AR_vol_mean)], 1, sumx)
SVperc <- mean(perc_increment(totcap, DRN_AR_vol))

#Dataframes
load("./Data/Scenarios/AR/AR_daily.RData") #AR
load("./Data/Scenarios/LR/LR_daily.RData") #LR
AR <- monthly_scale(df = AR_daily)
LR <- monthly_scale(df = LR_daily)

#DRN AR scenario
load("./Data/Analysis/Downstreamness/DRN_AR_vol_sum.Rdata")
DRN_AR <- HDNR_vol

#Dx
library(rgdal)
DRN <- readOGR("./Data/DRN/HDNR.shp")
res <- readOGR("./Data/Shapefile/Reservoirs/centralized_res.shp")

#Create dfs from the shapefiles attribute tables
DRN_df <- DRN@data
res_df <- res@data
dx_drn <- DRN_df$dx
dx_res <- res_df$dx[order(res_df$subID)]

# Obtain Dsv --------------------------------------------------------------

tic("Obtain Dsv AR and LR")
Dsv_AR <- Dsv_LR <- data.frame(date = AR$date)
Dsv_AR$Dsv <- Dsv_AR$Dsv_modified <- Dsv_AR$noDRN <- Dsv_LR$Dsv <- 0
coeff <- 100/(100 + SVperc)

for (i in seq_len(nrow(Dsv_AR))) {
  Dsv_AR$Dsv[i] <- downstreamness_SV(AR[i, 2:ncol(AR)], dx_res,
                                  DRN_AR[i, 2:ncol(DRN_AR)], dx_drn)
  Dsv_AR$Dsv_modified[i] <- downstreamness_SV(AR[i, 2:ncol(AR)], dx_res,
                                  DRN_AR[i, 2:ncol(DRN_AR)], dx_drn, coeff)
  Dsv_AR$noDRN[i] <- downstreamness_SV(AR[i, 2:ncol(AR)], dx_res)
  
  Dsv_LR$Dsv[i] <- downstreamness_SV(LR[i, 2:ncol(LR)], dx_res)
}
toc()

save(Dsv_AR, file = "./Data/Analysis/Downstreamness/Dsv_AR_new.Rdata")
save(Dsv_LR, file = "./Data/Analysis/Downstreamness/Dsv_LR_new.Rdata")
plot_df_interactive(Dsv_AR)