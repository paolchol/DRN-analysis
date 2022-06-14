setwd("C:/Directory_thesis_codes")
source("./R/Libraries/Libraries.R")
source("./R/Libraries/Functions.R")
source("./R/Libraries/Functions_MC.R")

#Meteorological drought period
date_ranges_SPI <- data.frame(
  from = as.Date(c("1980-12-01", "1987-05-01", "1992-05-01", "1997-05-01", "2001-02-01", "2005-02-01", "2010-05-01", "2012-05-01", "2014-06-01")),
  to = as.Date(c("1984-03-01", "1988-04-01", "1994-04-01", "1999-12-01", "2002-02-01", "2005-12-01", "2010-12-01", "2014-01-01", "2018-12-01"))
)

rename_scenarios = function(df){
  #df BEFORE reshape::melt!
  names(df)[names(df) == "Real"] <- "AR"
  names(df)[names(df) == "noHDNR"] <- "LR"
  return(df)
}

perc_increment = function(xi, xf){
  return((xf-xi)/xi * 100)
}

# Stored volume --------------------------------------------------------------

load("./Data/Analysis/Downstreamness/Dsv_AR_prova.Rdata")

prova <- Dsv_AR

load("./Data/Analysis/Downstreamness/Dsv_AR.Rdata")
load("./Data/Analysis/Downstreamness/DRN_AR_vol_mean.Rdata") #DRN stored volume
load("./Data/Analysis/Downstreamness/DRN_SR_vol_mean.Rdata")
load("./Data/Scenarios/AR/AR_daily.RData") #AR
load("./Data/Scenarios/LR/LR_daily.RData") #LR
load("./Data/Scenarios/N/N_daily.RData") #N
load("./Data/Scenarios/SR/SR_daily.RData") #N

AR <- monthly_scale(df = AR_daily, f = mean)
LR <- monthly_scale(df = LR_daily, f = mean)
SR <- monthly_scale(df = SR_daily, f = mean)
N <- monthly_scale(df = N_daily, f = mean)

res_AR_vol <- apply(AR[, 2:ncol(AR)], 1, sumx)
res_LR_vol <- apply(LR[, 2:ncol(LR)], 1, sumx)
res_SR_vol <- SR[, 2]
res_N_vol <- N[, 2]

DRN_AR_vol <- apply(DRN_AR_vol_mean[, 2:ncol(DRN_AR_vol_mean)], 1, sumx)
DRN_SR_vol <- apply(DRN_SR_vol_mean[, 2:ncol(DRN_SR_vol_mean)], 1, sumx)

#Stored volume
SV_AR <- res_AR_vol + DRN_AR_vol
SV_LR <- res_LR_vol
SV_SR <- res_SR_vol + DRN_SR_vol
SV_N <- res_N_vol

df <- data.frame(date = Dsv_AR$date,
                 AR = SV_AR, LR = SV_LR,
                 SR = SV_SR, N = SV_N,
                 DRN_AR = DRN_AR_vol, DRN_SR = DRN_SR_vol)
plot_df_interactive(df, t = 'Stored volumes')

plot(perc_increment(DRN_SR_vol, DRN_AR_vol))



date_ranges <- date_ranges_SPI

options(scipen = 0)
ylabel <- (expression(paste("Stored Volume (monthly mean) [ ",m^3," ]", sep="")))
#leg1 <- expression(paste("HDNR volume [ 100 *",m^3," ]", sep=""))
col <- c("DRN volume [0.01 * m3]" = "#2F86A6")
p <- ggplot() +
  #Add the drought rectangles
  geom_rect(data = date_ranges, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4) +
  #Add the Stored volume lines
  geom_line(data = df, aes(x = date, y = value, linetype = Scenario), alpha = 0.5, size = 1.25) +
  #Add the volume stored in the HDNR
  geom_line(data = data.frame(date = Dsv_r$date, value = HDNR_vol*100),
            aes(x = date, y = value, color = "DRN volume [0.01 * m3]"), alpha = 0.5, size = 1.25) +
  scale_y_continuous(name = ylabel) +
  scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y', limits = as.Date(c("1980-01-01", "2018-12-01")), expand = c(0,0)) +
  theme(axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        legend.position = "bottom") +
  #Add the legend
  scale_color_manual(name = "Legend", values = col)

print(p)
plot.save(p, width = 1280, height = 657, filename = "./Plots/stored_vol_pres.png")
