#Downstreamness plots

# Setup -------------------------------------------------------------------

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

# Dsc results -------------------------------------------------------------

#Decrease the interval of x axis, to 1 or 5 years
#Plot the two lines in the same graph

load("./Data/Downstreamness/Dsc_r.Rdata")
load("./Data/Downstreamness/Dsc_nH.Rdata")

df <- data.frame(date = Dsc_r$date, Real = Dsc_r$Dsc, noHDNR = Dsc_nH$Dsc)
df <- rename_scenarios(df)
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Scenario')

p <- df %>%
  ggplot(aes(date, value)) + 
  geom_line(aes(linetype = Scenario), size = 1.5, alpha = 0.5) +
  #Add lines at the reservoirs construction
  geom_vline(xintercept = 1992, size = 1.25, alpha = 0.5, color = '#2F86A6') +
  geom_text(aes(1992, 54.5, label = "Cipoada", hjust = -0.25)) +
  geom_vline(xintercept = 1996, size = 1.25, alpha = 0.5, color = '#2F86A6') +
  geom_text(aes(1996, 54.5, label = "Fogareiro", hjust = -0.25)) +
  geom_vline(xintercept = 1988, size = 1.25, alpha = 0.5, color = '#2F86A6') +
  geom_text(aes(1988, 54.5, label = "Patu", hjust = -0.25)) +
  geom_vline(xintercept = 2000, size = 1.25, alpha = 0.5, color = '#2F86A6') +
  geom_text(aes(2000, 54.5, label = "Pirabibu", hjust = -0.25)) +
  geom_vline(xintercept = 2011, size = 1.25, alpha = 0.5, color = '#2F86A6') +
  geom_text(aes(2011, 54.5, label = "Umari", hjust = -0.25)) +
  geom_vline(xintercept = 2007, size = 1.25, alpha = 0.5, color = '#2F86A6') +
  geom_text(aes(2007, 54.5, label = "Curral Velho", hjust = -0.25)) +
  scale_x_continuous(name = "Date", breaks = seq(1980, 2018, 2), limits = c(1980, 2018)) +
  scale_y_continuous(name = expression(paste('D'['SC'], " [%]")), limits = c(40, 55),
                     breaks = seq(40, 55, 2.5)) +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

print(p)

plot.save(p, width = 1280, height = 657, filename = "./Plots/Dsc_pres.png")

min(Dsc_r$Dsc)
max(Dsc_nH$Dsc)

df$noHDNR - df$Real
mean(df$noHDNR - df$Real)

# Dsv results -------------------------------------------------------------

#Plot: stored volume in hdnr vs stored volume in centralized
#sum all centr and all hdnr, plot on the same graph

load("./Data/Downstreamness/Dsv_r_v2.Rdata")
load("./Data/Downstreamness/Dsv_nH.Rdata")

df <- data.frame(date = Dsv_r$date, AR = Dsv_r$Dsv, LR = Dsv_nH$Dsv, noh = Dsv_r$noH)
#df <- rename_scenarios(df)
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Scenario')
df$Scenario <- as.character(df$Scenario)
df['Scenario'][df['Scenario'] == "noh"] <- "AR (DRN influence removed)"
#To check the columns: str(df)

date_ranges <- date_ranges_SPI
col <- c("#181D31", "#FFC947", "#678983")
p <- ggplot() +
  geom_rect(data = date_ranges, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4) +
  geom_line(data = df, aes(x = date, y = value, colour = Scenario, linetype = Scenario), size = 1.25, alpha = 0.8) +
  scale_y_continuous(name = expression(paste('D'['SV'], " [%]")), limits = c(25, 65)) +
  scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y', limits = as.Date(c("1980-01-01", "2018-12-01")), expand = c(0,0)) +
  scale_color_manual(values = col) +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

print(p)

plot.save(p, width = 1280, height = 657, filename = "./Plots/Dsv_v4.png")

RMSE(Dsv_r$Dsv, Dsv_r$noH)
min(Dsv_r$Dsv)
max(Dsv_r$noH)

mean(Dsv_r$Dsv)
mean(Dsv_nH$Dsv)

#Coefficient of variation
sd(Dsv_r$Dsv) / mean(Dsv_r$Dsv) #AR
sd(Dsv_nH$Dsv) / mean(Dsv_nH$Dsv) #LR

mean(Dsv_r$Dsv[Dsv_r$date < "2000-01-01"]) - mean(Dsv_nH$Dsv[Dsv_r$date < "2000-01-01"])

# Stored volume --------------------------------------------------------------

load("./Data/Downstreamness/Dsv_r.Rdata")
load("./Data/Downstreamness/HDNR_vol_mean.Rdata")
load("./Data/Scenarios/Real/real_daily.RData")
load("./Data/Scenarios/No_HDNR/noH_daily.RData")

real_df <- monthly_scale(df = real_df, f = mean)
noH_df <- monthly_scale(df = noH_df, f = mean)

HDNR_vol <- apply(HDNR_vol_mean[, 2:ncol(HDNR_vol_mean)], 1, sumx)
res_r_vol <- apply(real_df[, 2:ncol(real_df)], 1, sumx)
res_nH_vol <- apply(noH_df[, 2:ncol(noH_df)], 1, sumx)

#Stored volume
SV_R <- HDNR_vol + res_r_vol
SV_nH <- res_nH_vol
df <- data.frame(date = Dsv_r$date, Real = SV_R, noHDNR = SV_nH) #, hdnr = HDNR_vol*500)
#plot_df_interactive(df, t = 'Stored volumes')
df <- rename_scenarios(df)
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Scenario')

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


# Dsc vs Dsv --------------------------------------------------------------

load("./Data/Downstreamness/Dsc_r.Rdata")
load("./Data/Downstreamness/Dsc_nH.Rdata")

load("./Data/Downstreamness/Dsv_r.Rdata")
load("./Data/Downstreamness/Dsv_nH.Rdata")

dt <- Dsv_r$date
rdsc <- data.frame(date = dt, dsc = 0)
nhdsc <- data.frame(date = dt, dsc = 0)

for(i in seq_len(length(Dsc_r$date))){
  rdsc$dsc[lubridate::year(rdsc$date) == Dsc_r$date[i]] <- Dsc_r$Dsc[i]
  nhdsc$dsc[lubridate::year(nhdsc$date) == Dsc_nH$date[i]] <- Dsc_nH$Dsc[i]
}

df <- data.frame(
  date = Dsv_r$date,
  'Dsv AR' = Dsv_r$Dsv,
  'Dsv LR' = Dsv_nH$Dsv,
  'Dsc AR' = rdsc$dsc,
  'Dsc AL' = nhdsc$dsc
)
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Legend')

date_ranges <- date_ranges_SPI

#fix 4 colours: 2 similar for the Dsv, 2 similar for the Dsc
#or 2 colors: 1 for real, one for noHDNR, but with different linetypes

p <- ggplot() +
  #Add drought rectangles
  geom_rect(data = date_ranges, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4) +
  #Add Dsv and Dsc
  geom_line(data = df, aes(x = date, y = value, colour = Legend, linetype = Legend), size = 1.25, alpha = 0.8) +
  #Layout
  scale_y_continuous(name = expression(paste('D'['SV'], ", D"['SC'], " [%]")), limits = c(25, 65)) +
  scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y', limits = as.Date(c("1980-01-01", "2018-12-01")), expand = c(0,0)) +
  scale_color_manual(values = c("salmon1", "salmon3", "steelblue2", "steelblue3")) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
  
print(p)

plot.save(p, width = 1280, height = 657, filename = "./Plots/dsc_dsv_v3.png")

# Dsv: all scenarios considered -------------------------------------------

load("./Data/Downstreamness/Dsv_r_v2.Rdata")
load("./Data/Downstreamness/Dsv_nH.Rdata")
load("./Data/Downstreamness/Dsv_N.Rdata")
load("./Data/Downstreamness/Dsv_SR.Rdata")

df <- data.frame(date = Dsv_SR$date,
                 AR = Dsv_r$Dsv, LR = Dsv_nH$Dsv, noh = Dsv_r$noH,
                 SR = Dsv_SR$Dsv, N = Dsv_N$Dsv)
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Scenario')
df$Scenario <- as.character(df$Scenario)
df['Scenario'][df['Scenario'] == "noh"] <- "AR (DRN influence removed)"
#To check the columns: str(df)

date_ranges <- date_ranges_SPI

#E2703A
#Colours
#         AR      AR(...)     LR        N       SR
#col <- c("#046582", "#FFC947", "#BB8082", "#6E7582", "#F39189")
col <- c("#181D31", "#FFC947", "#678983", "#E2703A", "#046582")
p <- ggplot() +
  geom_rect(data = date_ranges, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.3) +
  geom_line(data = df, aes(x = date, y = value, colour = Scenario, linetype = Scenario), size = 1.25, alpha = 1) +
  scale_y_continuous(name = expression(paste('D'['SV'], " [%]")), limits = c(25, 75)) +
  scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y', limits = as.Date(c("1980-01-01", "2018-12-01")), expand = c(0,0)) +
  scale_color_manual(values = col) +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16))

print(p)

plot.save(p, width = 1280, height = 657, filename = "./Plots/Dsv_allSC_pres.png")

col <- c("#181D31", "#FFC947", "#678983", "#E2703A", "#046582")
p <- ggplot() +
  #geom_rect(data = date_ranges, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.3) +
  geom_line(data = df, aes(x = date, y = value, colour = Scenario, linetype = Scenario), size = 2, alpha = 1) +
  scale_y_continuous(name = " ", limits = c(71, 74)) +
  scale_x_date(name = "", date_breaks = "5 years", date_labels = '%Y', limits = as.Date(c("1980-01-01", "2018-12-01")), expand = c(0,0)) +
  scale_color_manual(values = col) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 20),
    legend.position = "none"
  )

print(p)

plot.save(p, width = 1280, height = 657, filename = "./Plots/Dsv_allSC_2_pres.png")

#Coefficient of variation
(sd(Dsv_SR$Dsv) / mean(Dsv_SR$Dsv) - sd(Dsv_N$Dsv) / mean(Dsv_N$Dsv))*100 #N


Dsv_N$date[x]
max(Dsv_N$Dsv - Dsv_SR$Dsv)

y <- Dsv_N$Dsv - Dsv_SR$Dsv
y <- y[3:length(y)]
x <- which(y == max(y))





