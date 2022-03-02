#New scenarios with only Arrojado Lisboa considered

# Script setup ------------------------------------------------------------

#Directory and paths
setwd("C:/Users/paolo/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")
path_maxcap <- "./Inputs/Model_input/Base/Reservoir/reservoir.dat"

source("./Libraries/Libraries.R")
source("./Libraries/Functions.R")
# source("./Libraries/Functions_TG.R")
# source("./Libraries/Functions_DP.R")
source("./Libraries/Functions_CO.R")
source("./Libraries/Functions_MC.R")
source("./Libraries/Functions_AN.R")

#Load the IDs of the subbasins in the Banabuiu region
load("./Inputs/General/IDs.RData")
subID <- read.table("./Inputs/General/reservoir_name_ID.txt", header = TRUE, sep = "\t")

# Analysis setup ----------------------------------------------------------

#Load precipitation data
prec <- read.table("./Inputs/Model_input/Base/Time_series/rain_daily.dat", skip = 3)
columns <- c("date", "doy", subID$SubbasinID)
names(prec) <- columns
prec$date <- change_date_WASA_input(prec$date)
prec$doy <- NULL
prec <- monthly_scale(prec, f = sumx)
#Save the resulting dataframe
# save(prec, file = "./Data/precipitation.RData")

#Load reservoirs' volumes
## Real scenario
load("./Data/Scenarios/N/ALonly_daily.RData")
## No HDNR scenario
load("./Data/Scenarios/SR/ALhdnr_daily.RData")

#Load the reservoirs' maximum capacities
reservoirs <- read.table(path_maxcap, skip = 2, sep = "\t")
maxcap <- reservoirs[,c(1,5)]
names(maxcap) <- c('ID','max')
maxcap[,2] <- maxcap[,2]*1000 #m3

#Place values higher than the maximum capacity equal to the maximum
#capacity
ALonly$v156[ALonly$v156 > maxcap[18,2]] <- maxcap[18,2]
ALhdnr$v156[ALhdnr$v156 > maxcap[18,2]] <- maxcap[18,2]


# DCA operations ----------------------------------------------------------

#SPI
library(SPEI)
SPI_AL <- spi(prec[['156']], 12)

#VD
date <- create_date_vector(1980, 2018)
VD_ALonly <- (ALonly$v156 - maxcap[18,2]/2)/(maxcap[18,2]/2)
df <- data.frame(date = date, VD_ALonly)
VD_ALonly <- monthly_scale(df, f = meanx)

VD_ALhdnr <- (ALhdnr$v156 - maxcap[18,2]/2)/(maxcap[18,2]/2)
df <- data.frame(date = date, VD_ALhdnr)
VD_ALhdnr <- monthly_scale(df, f = meanx)

#Quadrant
DCA_N <- quadrant_attribution(SPI_AL$fitted, VD_ALonly$VD_ALonly, VD_ALonly$date)
DCA_SR <- quadrant_attribution(SPI_AL$fitted, VD_ALhdnr$VD_ALhdnr, VD_ALhdnr$date)

#Save
list.save(DCA_N, "./Data/DCA/DCA_N.RData")
list.save(DCA_SR, "./Data/DCA/DCA_SR.RData")

# VD plot -----------------------------------------------------------------

DCA_N <- list.load("./Data/DCA/DCA_N.RData")
DCA_SR <- list.load("./Data/DCA/DCA_SR.RData")

df <- data.frame(date = DCA_SR$date, SR = DCA_SR$WSI, N = DCA_N$WSI)
plot_df_interactive(df)
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Scenario')
str(df)
date_ranges <- data.frame(
  from = as.Date(c("1980-12-01", "1987-05-01", "1992-05-01", "1997-05-01", "2001-02-01", "2005-02-01", "2010-05-01", "2012-05-01", "2014-06-01")),
  to = as.Date(c("1984-03-01", "1988-04-01", "1994-04-01", "1999-12-01", "2002-02-01", "2005-12-01", "2010-12-01", "2014-01-01", "2018-12-01"))
)
p <- ggplot() +
  #Add the drought rectangles
  geom_rect(data = date_ranges, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4) +
  #Add the VD lines
  geom_line(data = df, aes(x = date, y = value, linetype = Scenario), alpha = 0.5, size = 1.25) +
  #VD = 0 line
  geom_hline(yintercept = 0, color = 'tomato2', linetype = "dashed", size = 1, alpha = 0.8) +
  #geom_text(aes(as.Date("1985-01-01"), 0, label = "VD = 0", vjust = -1, hjust = 0)) +
  scale_y_continuous(name = 'Volume Deviation (VD)', limits = c(-1, 1)) +
  scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y', limits = as.Date(c("1980-01-01", "2018-12-01")), expand = c(0,0)) +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

print(p)
plot.save(p, width = 1280, height = 657, filename = "./Plots/vd_NSR_pres.png")

#Overall mean difference
mean(DCA_ALonly$WSI - DCA_ALhdnr$WSI) #0.17852
mean(DCA_noH[['156']]$WSI - DCA_real[['156']]$WSI) #0.11226

# Preparation for droughtwheel plots --------------------------------------

#VD
write.table(DCA_ALonly$WSI, "./Data/Germanos/wsi_ALonly.txt", quote = FALSE, row.names = FALSE)
write.table(DCA_ALhdnr$WSI, "./Data/Germanos/wsi_ALhdnr.txt", quote = FALSE, row.names = FALSE)

#SPI
write.table(DCA_ALonly$PI, "./Data/Germanos/pi_ALonly.txt", quote = FALSE, row.names = FALSE)

#First event
only <- DCA_ALonly$quadrant[DCA_ALonly$date >= as.Date('1992-01-01') & DCA_ALonly$date <= as.Date('1994-08-01')]
real <- DCA_ALhdnr$quadrant[DCA_ALhdnr$date >= as.Date('1992-01-01') & DCA_ALhdnr$date <= as.Date('1994-08-01')]
write.table(data.frame(only = only, real = real), "./Data/Germanos/ALfirst_event.txt", quote = FALSE, row.names = FALSE)
#Second event
only <- DCA_ALonly$quadrant[DCA_ALonly$date >= as.Date('1997-01-01') & DCA_ALonly$date <= as.Date('2002-06-01')]
real <- DCA_ALhdnr$quadrant[DCA_ALhdnr$date >= as.Date('1997-01-01') & DCA_ALhdnr$date <= as.Date('2002-06-01')]
write.table(data.frame(only = only, real = real), "./Data/Germanos/ALsecond_event.txt", quote = FALSE, row.names = FALSE)
#Third event
only <- DCA_ALonly$quadrant[DCA_ALonly$date >= as.Date('2010-01-01') & DCA_ALonly$date <= as.Date('2018-12-01')]
real <- DCA_ALhdnr$quadrant[DCA_ALhdnr$date >= as.Date('2010-01-01') & DCA_ALhdnr$date <= as.Date('2018-12-01')]
write.table(data.frame(only = only, real = real), "./Data/Germanos/ALthird_event.txt", quote = FALSE, row.names = FALSE)

