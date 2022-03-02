#Plots and tables for the DCA analysis

# Setup -------------------------------------------------------------------

setwd("C:/Users/paolo/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")
source("./Libraries/Libraries.R")
source("./Libraries/Functions.R")

DCA_real <- list.load("./Data/DCA/DCA_real.RData")
DCA_noH <- list.load("./Data/DCA/DCA_noH.RData")
DCA_obs <- list.load("./Data/DCA/DCA_obs.RData")
DCA_SR <- list.load("./Data/DCA/DCA_SR.RData")
DCA_N <- list.load("./Data/DCA/DCA_N.RData")

rename_scenarios = function(df){
  #df BEFORE reshape::melt!
  names(df)[names(df) == "Real"] <- "AR"
  names(df)[names(df) == "noHDNR"] <- "LR"
  return(df)
}

#Meteorological drought periods
#Based on Marengo et al., 2016
date_ranges_Marengo <- data.frame(
  from = as.Date(c("1981-01-01", "1982-01-01", "1992-01-01", "1997-01-01", "2001-01-01", "2005-01-01", "2007-01-01", "2010-01-01")),
  to = as.Date(c("1981-12-01", "1983-12-01", "1993-12-01", "1998-12-01", "2002-12-01", "2005-12-01", "2007-12-01", "2018-12-01"))
)
#Based on SPI
# negSPI <- DCA_real$Banabuiu$date[DCA_real$Banabuiu$PI < 0]
# negSPI
date_ranges_SPI <- data.frame(
  from = as.Date(c("1980-12-01", "1987-05-01", "1992-05-01", "1997-05-01", "2001-02-01", "2005-02-01", "2010-05-01", "2012-05-01", "2014-06-01")),
  to = as.Date(c("1984-03-01", "1988-04-01", "1994-04-01", "1999-12-01", "2002-02-01", "2005-12-01", "2010-12-01", "2014-01-01", "2018-12-01"))
)

# Table: Cumulative months of drought phases ------------------------------

table1 <- data.frame(matrix(NA, 5, 3))
names(table1) <- c("real", "noH", "obs")
for(i in 1:4){
  table1$real[i] <- sum(DCA_real[['156']]$quadrant == i, na.rm = TRUE)
  table1$noH[i] <- sum(DCA_noH[['156']]$quadrant == i, na.rm = TRUE)
  table1$obs[i] <- sum(DCA_obs[['156']]$quadrant == i, na.rm = TRUE)
}
table1[5, ] <- apply(table1, 2, function(x) sum(x, na.rm = TRUE))
View(table1)

table2 <- table1/table1[5,1]
table2$obs <- table1$obs/table1[5,3]
View(table2)
#The observations have less actual months in drought phases both totally and relatively
#but the behavior is the same

table2$obs <- NULL
table2 <- round(table2, 2)
table2$real_tot <- table1$real
table2$noH_tot <- table1$noH
#Re-order the columns
table2 <- table2[, c(3,4,1,2)]
names(table2) <- c("real_tot", "noH_tot", "real_perc", "noH_perc")
View(table2)

write.table(table2, file = "./Data/Plot_table/cumulative_phases_AL.txt", sep = "\t", quote = FALSE, 
            row.names = TRUE)

sum(table2$real_tot[3:4]) - sum(table2$noH_tot[3:4])

#Table with N and SR
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

# Figure: SPI for each subbasin -------------------------------------------

find_bounds = function(df, x = 1, skip = 0){
  minx <- function(x) min(x, na.rm = TRUE)
  maxx <- function(x) max(x, na.rm = TRUE)
  skip <- skip + 1
  lowbound <- apply(df[, skip:ncol(df)], x, minx)
  highbound <- apply(df[, skip:ncol(df)], x, maxx)
  bounds <- data.frame(date = df$date, low = lowbound, high = highbound)
  return(bounds)
}

SPI_df <- data.frame(date = DCA_real$Banabuiu$date)
for(i in 1:length(DCA_real)){
  SPI_df$new <- DCA_real[[i]]$PI
  names(SPI_df)[names(SPI_df) == "new"] <- names(DCA_real)[i]
}

newdf <- SPI_df[,1:ncol(SPI_df)-1]
bounds <- find_bounds(newdf, 1, 1)
bounds[bounds == Inf | bounds == -Inf] <- 0

date_ranges <- date_ranges_Marengo

df <- SPI_df[, c(1,ncol(SPI_df))]
col <- c("Mean SPI-12"="royalblue2", "Sub-basins variation"="sandybrown")
p <- ggplot() +
  #Add the droughts rectangles
  geom_rect(data = date_ranges, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4) +
  #Add the sub-basins variation
  geom_ribbon(data = bounds, aes(x = date, ymin = low, ymax = high, fill = "Sub-basins variation"), alpha = 0.5) +
  #Add the mean line
  geom_line(data = df, aes(x = date, y = Banabuiu, colour = "Mean SPI-12"), size = 1) +
  #Add the drought indicator lines
  geom_hline(yintercept = 0, color = 'lightblue', linetype = "dashed", size = 1.25, alpha = 0.8) +
  geom_text(aes(as.Date("1985-01-01"), 0, label = "Normal", vjust = -0.5, hjust = 0), size = 5) +
  geom_hline(yintercept = -1, color = 'yellow', linetype = "dashed", size = 1.25, alpha = 0.8) +
  geom_text(aes(as.Date("1985-01-01"), -1, label = "Moderately dry", vjust = -0.5, hjust = 0), size = 5) +
  geom_hline(yintercept = -1.5, color = 'orange', linetype = "dashed", size = 1.25, alpha = 0.8) +
  geom_text(aes(as.Date("1985-01-01"), -1.5, label = "Severely dry", vjust = -0.5, hjust = 0), size = 5) +
  geom_hline(yintercept = -2, color = 'red', linetype = "dashed", size = 1.25, alpha = 0.8) +
  geom_text(aes(as.Date("1985-01-01"), -2, label = "Extremely dry", vjust = -0.5, hjust = 0), size = 5) +
  #Modify the axis
  scale_y_continuous(name = 'SPI-12', limits = c(-4.5, 4), breaks = seq(-4, 4, 1)) +
  scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y', limits = as.Date(c("1980-01-01", "2018-12-01")), expand = c(0,0)) +
  #scale_colour_grey(start = 0.9, end = 0.1) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  #Add the legend
  scale_color_manual(name = "", values = col) +
  scale_fill_manual(name = "", values = col)

print(p)

plot.save(p, width = 1280, height = 657, filename = "./Plots/spi_v4.png")

# Figure: VD comparison, Arrojado Lisboa -------------------------------------------

#Arrojado Lisboa
df <- data.frame(date = DCA_real[['156']]$date, Real = DCA_real[['156']]$WSI, noHDNR = DCA_noH[['156']]$WSI)
plot_df_interactive(df)

df <- rename_scenarios(df)
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Scenario')

date_ranges <- date_ranges_SPI

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
  #scale_color_manual(values = c("royalblue3", "steelblue"))
  #scale_color_manual(values = c("salmon1", "royalblue"))

print(p)
plot.save(p, width = 1280, height = 657, filename = "./Plots/vd_ARLR_present.png")

#Pedras Brancas
df <- data.frame(date = DCA_real[['123']]$date, Real = DCA_real[['123']]$WSI, noHDNR = DCA_noH[['123']]$WSI, obs = DCA_obs[['123']]$WSI)
plot_df_interactive(df)

# Figure: VD comparison, mean (without AL) --------------------------------

mean_no_AL = function(list){
  index <- c(seq(1,17,1), 19)
  WSI <- 0
  for(i in index){
    list[[i]]$WSI[is.na(list[[i]]$WSI)] <- 0
    WSI <- WSI + list[[i]]$WSI
  }
  WSI <- WSI/length(index)
  return(WSI)
}

#Mean without AL
mean_r <- mean_no_AL(DCA_real)
mean_nH <- mean_no_AL(DCA_noH)
df <- data.frame(date = DCA_real[['Banabuiu']]$date, Real = mean_r, noHDNR = mean_nH)
df <- rename_scenarios(df)
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Scenario')

date_ranges <- date_ranges_SPI

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
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))

print(p)

plot.save(p, width = 1280, height = 657, filename = "./Plots/vd_mean_v3.png")

# Preparation for Germano's plots -----------------------------------------

#First two scenarios
#VD
write.table(DCA_real[["156"]]$WSI, "./Data/Germanos/wsi_real.txt", quote = FALSE, row.names = FALSE)
write.table(DCA_noH[["156"]]$WSI, "./Data/Germanos/wsi_noH.txt", quote = FALSE, row.names = FALSE)

#SPI
write.table(DCA_real[["156"]]$PI, "./Data/Germanos/pi.txt", quote = FALSE, row.names = FALSE)

#First event
noH <- DCA_noH[["156"]]$quadrant[DCA_noH[["156"]]$date >= as.Date('1992-01-01') & DCA_noH[["156"]]$date <= as.Date('1994-08-01')]
real <- DCA_real[["156"]]$quadrant[DCA_real[["156"]]$date >= as.Date('1992-01-01') & DCA_real[["156"]]$date <= as.Date('1994-08-01')]
write.table(data.frame(noH = noH, real = real), "./Data/Germanos/first_event.txt", quote = FALSE, row.names = FALSE)
#Second event
noH <- DCA_noH[["156"]]$quadrant[DCA_noH[["156"]]$date >= as.Date('1997-01-01') & DCA_noH[["156"]]$date <= as.Date('2002-06-01')]
real <- DCA_real[["156"]]$quadrant[DCA_real[["156"]]$date >= as.Date('1997-01-01') & DCA_real[["156"]]$date <= as.Date('2002-06-01')]
write.table(data.frame(noH = noH, real = real), "./Data/Germanos/second_event.txt", quote = FALSE, row.names = FALSE)
#Third event
noH <- DCA_noH[["156"]]$quadrant[DCA_noH[["156"]]$date >= as.Date('2010-01-01') & DCA_noH[["156"]]$date <= as.Date('2018-12-01')]
real <- DCA_real[["156"]]$quadrant[DCA_real[["156"]]$date >= as.Date('2010-01-01') & DCA_real[["156"]]$date <= as.Date('2018-12-01')]
write.table(data.frame(noH = noH, real = real), "./Data/Germanos/third_event.txt", quote = FALSE, row.names = FALSE)

#Checks
#Comparison of the quadrants
df <- data.frame(date = DCA_real[["156"]]$date, real = DCA_real[["156"]]$quadrant, noh = DCA_noH[["156"]]$quadrant)
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Scenario')
p <- ggplot(df) +
  geom_line(aes(date, value, colour = Scenario))
print(p)

#VD scatterplot
plot(DCA_noH$Banabuiu$WSI, DCA_real$Banabuiu$WSI)
plot(DCA_noH[["156"]]$WSI, DCA_real[["156"]]$WSI)

# Heatmap of variation -------------------------------------



