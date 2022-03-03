#Other plots

# Setup -------------------------------------------------------------------

setwd("C:/Users/paolo/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")
source("./Libraries/Libraries.R")
source("./Libraries/Functions.R")

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

# VD comparison: Real, noHDNR, observations ----------------------------------

DCA_real <- list.load("./Data/DCA/DCA_real.RData")
DCA_noH <- list.load("./Data/DCA/DCA_noH.RData")
DCA_obs <- list.load("./Data/DCA/DCA_obs.RData")

#Arrojado Lisboa
df <- data.frame(date = DCA_real[['156']]$date, Real = DCA_real[['156']]$WSI,
                 noHDNR = DCA_noH[['156']]$WSI, Observations = DCA_obs[['156']]$WSI)
plot_df_interactive(df)
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Scenario')

date_ranges <- date_ranges_SPI

p <- ggplot() +
  #Add the drought rectangles
  geom_rect(data = date_ranges, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4) +
  #Add the VD lines
  geom_line(data = df, aes(x = date, y = value, linetype = Scenario, color = Scenario), alpha = 0.8, size = 1.25) +
  #VD = 0 line
  geom_hline(yintercept = 0, color = 'tomato2', linetype = "dashed", size = 1, alpha = 0.8) +
  #geom_text(aes(as.Date("1985-01-01"), 0, label = "VD = 0", vjust = -1, hjust = 0)) +
  scale_y_continuous(name = 'Volume Deviation (VD)', limits = c(-1, 1)) +
  scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y', limits = as.Date(c("1980-01-01", "2018-12-01")), expand = c(0,0)) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  scale_color_manual(values = c("salmon1", "tan1", "royalblue"))

print(p)

#Mean
df <- data.frame(date = DCA_real[['Banabuiu']]$date, Real = DCA_real[['Banabuiu']]$WSI,
                 noHDNR = DCA_noH[['Banabuiu']]$WSI, Observations = DCA_obs[['Banabuiu']]$WSI)
plot_df_interactive(df)
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Scenario')

date_ranges <- date_ranges_SPI

p <- ggplot() +
  #Add the drought rectangles
  geom_rect(data = date_ranges, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4) +
  #Add the VD lines
  geom_line(data = df, aes(x = date, y = value, linetype = Scenario, color = Scenario), alpha = 0.8, size = 1.25) +
  #VD = 0 line
  geom_hline(yintercept = 0, color = 'tomato2', linetype = "dashed", size = 1, alpha = 0.8) +
  #geom_text(aes(as.Date("1985-01-01"), 0, label = "VD = 0", vjust = -1, hjust = 0)) +
  scale_y_continuous(name = 'Volume Deviation (VD)', limits = c(-1, 1)) +
  scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y', limits = as.Date(c("1980-01-01", "2018-12-01")), expand = c(0,0)) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  scale_color_manual(values = c("salmon1", "tan1", "royalblue"))

print(p)
#ggplotly(p)

# VD of Arrojado Lisboa + extractions -------------------------------------
#Stored volume + extractions
#The stored volume is obtained by inverting the VD

DCA_real <- list.load("./Data/DCA/DCA_real.RData")
DCA_noH <- list.load("./Data/DCA/DCA_noH.RData")
DCA_obs <- list.load("./Data/DCA/DCA_obs.RData")

#Arrojado Lisboa
maxcap <- 1601000000
df <- data.frame(date = DCA_real[['156']]$date, Real = DCA_real[['156']]$WSI,
                 noHDNR = DCA_noH[['156']]$WSI, Observations = DCA_obs[['156']]$WSI)
plot_df_interactive(df)
df <- rename_scenarios(df)
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Scenario')

date_ranges <- date_ranges_SPI

ylabel1 <- expression('Monthly volume (mean) [ km'^3*' ]')
p1 <- ggplot() +
  #Add the drought rectangles
  geom_rect(data = date_ranges, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4) +
  #Add the VD lines
  geom_line(data = df, aes(x = date, y = (value*(maxcap/2)+maxcap/2)/1e+9,
                           linetype = Scenario, color = Scenario), alpha = 0.8, size = 1.25) +
  #Layout
  scale_y_continuous(name = ylabel1) +
  scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y', limits = as.Date(c("1980-01-01", "2018-12-01")), expand = c(0,0)) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  scale_color_manual(values = c("salmon1", "tan1", "royalblue"))

# print(p1)

load("./Inputs/General/withdrawal.RData")
withdrawal_df <- withdrawal_df[, c(1,20)]
names(withdrawal_df) <- c("date", "value")

#splined <- as.data.frame(spline(withdrawal_df$date, withdrawal_df$value))

ylabel2 <- expression(paste("Water withdrawal (monthly mean) [ ",km^3," ]", sep=""))
p2 <- ggplot() +
  #geom_line(data = splined, aes(x = x, y = y)) +
  geom_line(data = withdrawal_df, aes(x = date, y = value*3600*24*30/1e+9)) +
  #geom_point(data = withdrawal_df, aes(x = date, y = value)) +
  scale_y_continuous(name = ylabel2) +
  scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y', limits = as.Date(c("1980-01-01", "2018-12-01")), expand = c(0,0)) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))

# print(p2)

p1 <- p1 + theme(legend.position = "top")
p3 <- grid.arrange(p2, p1, nrow = 2)
plot.save(p3, width = 1280, height = 657, filename = "./Plots/vd_extractions_v3.png")

# Figures: Single reservoirs model performance ----------------------------

plot_calibration_static = function(it, base, obs, ID, code, ret = FALSE){
  ID <- ID[which(ID %in% colnames(it))]
  vis_df <- data.frame(it$date, it[, which(colnames(it) %in% ID)],
                       base[, which(colnames(base) %in% ID)],
                       obs[, which(colnames(obs) %in% ID)])
  names(vis_df) <- c('date', "Calibrated", "Uncalibrated"
                     , "Observations")
  df <- reshape2::melt(vis_df, id.vars = 'date', variable.name = 'Legend')
  p <- ggplot(df, aes(date, value/1e+9)) +
    geom_line(aes(linetype = Legend, color = Legend), alpha = 0.8, size = 1.25) +
    xlab('Date') + ylab(expression('Monthly volume (mean) [ km'^3*' ]')) +
    theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
    scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y') +
    scale_color_manual(values = c("salmon1", "grey", "royalblue"))
  if(ret) return(p) else print(p)
}

source("./Libraries/Functions_MC.R")
load("./Data/volume_obs.RData")
load('./Inputs/Calibration/uncal_daily.RData')
load("./Data/Scenarios/Real/real_daily.RData")

obs_df <- monthly_scale(obs_df, f = mean)
real_df <- monthly_scale(real_df, f = mean)
uncal <- monthly_scale(uncal, f = mean)

plot_calibration_static(real_df, uncal, obs_df, 156, 'Arrojado Lisboa')
plot_calibration_static(real_df, uncal, obs_df, 123, 'Pedras Brancas')
plot_calibration_static(real_df, uncal, obs_df, 160, 'Fogareiro')


# VD comparison: upstream vs downstream -----------------------------------

DCA_real <- list.load("./Data/DCA/DCA_real.RData")
DCA_noH <- list.load("./Data/DCA/DCA_noH.RData")

df <- data.frame(date = DCA_real[['156']]$date,
                 Real156 = DCA_real[['156']]$WSI, noHDNR156 = DCA_noH[['156']]$WSI,
                 Real160 = DCA_real[['160']]$WSI, noHDNR160 = DCA_noH[['160']]$WSI,
                 Real142 = DCA_real[['142']]$WSI, noHDNR142 = DCA_noH[['142']]$WSI,
                 Real123 = DCA_real[['123']]$WSI, noHDNR123 = DCA_noH[['123']]$WSI)
plot_df_interactive(df)

# Percentages of drought phases -------------------------------------------

table <- data.frame(matrix(NA, 5, 4))
names(table) <- c("AR%", "LR%", "SR%", "N%")
for(i in 1:4){
  table$`AR%`[i] <- sum(DCA_real[['156']]$quadrant == i, na.rm = TRUE)
  table$`LR%`[i] <- sum(DCA_noH[['156']]$quadrant == i, na.rm = TRUE)
  table$`SR%`[i] <- sum(DCA_SR$quadrant == i, na.rm = TRUE)
  table$`N%`[i] <- sum(DCA_N$quadrant == i, na.rm = TRUE)
}
table[5, ] <- apply(table, 2, function(x) sum(x, na.rm = TRUE))
table <- apply(table, 2, function(x) x/457*100)
table <- as.data.frame(table)
table <- table[-5,]
names(table) <- c("AR", "LR", "SR", "N")
table$Phase <- c("1. Non-occurrence of drought",
                 "2. Meteorological",
                 "3. Hydro-meteorological",
                 "4. Hydrological")

table <- pivot_longer(table, cols = 1:4, names_to = "Scenario", values_to = "Percentage")

ggplot(table, aes(fill = Phase, y = Percentage, x = Scenario)) + 
  geom_bar(position = "stack", stat = "identity") +
  #scale_fill_manual(values = c())
  scale_fill_manual(values = c("cyan1", "yellow1", "red1", "purple2")) +
  theme_classic() +
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        legend.position = "bottom")

#Other possible colors
#azzurro #B2DEEC #3EDBF0
#giallo #FDCA40 #F7FD04
#rosso #FB3640 #FA1E0E
#viola #542E71 #9D0191

# DRN capacities ripartition ----------------------------------------------

table <- read.table(file = "./Data/Plot_table/small_res_rip.txt")
table <- table[-6, ]
names(table)[1] <- "Class"
names(table)[4] <- "Number"
names(table)[6] <- "Capacity"
table <- pivot_longer(table, cols = c(4, 6), names_to = "Variable", values_to = "Percentage")

ggplot(table, aes(fill = Variable, y = Percentage, x = Class)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#B1D0E0", "blue4")) + 
  theme_classic()+
  theme(axis.title = element_text(size = 24),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.title = element_text(size = 22, face = "bold"),
        legend.text = element_text(size = 20),
        legend.position = "bottom")

