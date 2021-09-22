#Plots and tables for the DCA analysis


# Setup -------------------------------------------------------------------

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")
source("./Libraries/Libraries.R")

DCA_real <- list.load("./Data/DCA/DCA_real.RData")
DCA_noH <- list.load("./Data/DCA/DCA_noH.RData")
DCA_obs <- list.load("./Data/DCA/DCA_obs.RData")

# Table: Cumulative months of drought phases ------------------------------

table1 <- data.frame(matrix(NA, 5, 3))
names(table1) <- c("real", "noH", "obs")
for(i in 1:4){
  table1$real[i] <- sum(DCA_real$Banabuiu$quadrant == i, na.rm = TRUE)
  table1$noH[i] <- sum(DCA_noH$Banabuiu$quadrant == i, na.rm = TRUE)
  table1$obs[i] <- sum(DCA_obs$Banabuiu$quadrant == i, na.rm = TRUE)
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
table2 <- table2[, c(3,1,4,2)]
names(table2) <- c("real_tot", "real_perc", "noH_tot", "noH_perc")

write.table(table2, file = "./Data/Plot_table/cumulative_phases.txt", sep = "\t", quote = FALSE, 
            row.names = TRUE)

# Figure: SPI for each subbasin -------------------------------------------

SPI_df <- data.frame(date = DCA_real$Banabuiu$date)
for(i in 1:length(DCA_real)){
  SPI_df$new <- DCA_real[[i]]$PI
  names(SPI_df)[names(SPI_df) == "new"] <- names(DCA_real)[i]
}

newdf <- SPI_df[,1:ncol(SPI_df)-1]
lowbound <- apply(newdf[,2:ncol(newdf)], 1, min)
highbound <- apply(newdf[,2:ncol(newdf)], 1, max)
bounds <- data.frame(date = newdf$date, low = lowbound, high = highbound)

df <- SPI_df[, c(1,ncol(SPI_df))]

date_ranges <- data.frame(
  from = as.Date(c("1981-01-01", "1982-01-01", "1992-01-01", "1997-01-01", "2001-01-01", "2005-01-01", "2007-01-01", "2010-01-01")),
  to = as.Date(c("1981-12-01", "1983-12-01", "1993-12-01", "1998-12-01", "2002-12-01", "2005-12-01", "2007-12-01", "2018-12-01"))
)

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
  geom_text(aes(as.Date("1985-01-01"), 0, label = "Normal", vjust = -1, hjust = 0)) +
  geom_hline(yintercept = -1, color = 'yellow', linetype = "dashed", size = 1.25, alpha = 0.8) +
  geom_text(aes(as.Date("1985-01-01"), -1, label = "Moderately dry", vjust = -1, hjust = 0)) +
  geom_hline(yintercept = -1.5, color = 'orange', linetype = "dashed", size = 1.25, alpha = 0.8) +
  geom_text(aes(as.Date("1985-01-01"), -1.5, label = "Severely dry", vjust = -1, hjust = 0)) +
  geom_hline(yintercept = -2, color = 'red', linetype = "dashed", size = 1.25, alpha = 0.8) +
  geom_text(aes(as.Date("1985-01-01"), -2, label = "Extremely dry", vjust = -1, hjust = 0)) +
  #Modify the axis
  scale_y_continuous(name = 'SPI-12', limits = c(-4.5, 4), breaks = seq(-4, 4, 1)) +
  scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y', limits = as.Date(c("1980-01-01", "2018-12-01")), expand = c(0,0)) +
  #scale_colour_grey(start = 0.9, end = 0.1) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  #Add the legend
  scale_color_manual(name = "", values = col) +
  scale_fill_manual(name="", values = col)

print(p)

# Figure: VD mean comparison -------------------------------------------

#df <- data.frame(date = DCA_real[['Banabuiu']]$date, Real = DCA_real[['Banabuiu']]$WSI, noHDNR = DCA_noH[['Banabuiu']]$WSI)

df <- data.frame(date = DCA_real[['156']]$date, Real = DCA_real[['156']]$WSI, noHDNR = DCA_noH[['156']]$WSI)
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Scenario')

date_ranges <- data.frame(
  from = as.Date(c("1981-01-01", "1982-01-01", "1992-01-01", "1997-01-01", "2001-01-01", "2005-01-01", "2007-01-01", "2010-01-01")),
  to = as.Date(c("1981-12-01", "1983-12-01", "1993-12-01", "1998-12-01", "2002-12-01", "2005-12-01", "2007-12-01", "2018-12-01"))
)

p <- ggplot() +
  geom_line(data = df, aes(x = date, y = value, linetype = Scenario), alpha = 0.5, size = 1.25) +
  geom_rect(data = date_ranges, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4) +
  scale_y_continuous(name = 'Volume Deficit (VD)', limits = c(-1, 1)) +
  scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y', limits = as.Date(c("1980-01-01", "2018-12-01")), expand = c(0,0)) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))

print(p)

# Preparation for Germano's plots -----------------------------------------

write.table(DCA_real$Banabuiu$WSI, "wsi_real.txt", quote = FALSE, row.names = FALSE)
write.table(DCA_noH$Banabuiu$WSI, "wsi_noH.txt", quote = FALSE, row.names = FALSE)

minx = function(x) min(x,na.rm = TRUE)
maxx = function(x) max(x,na.rm = TRUE)
PI <- 2*(DCA_real$Banabuiu$PI - minx(DCA_real$Banabuiu$PI))/(maxx(DCA_real$Banabuiu$PI)-minx(DCA_real$Banabuiu$PI)) - 1

#write.table(PI, "pi_real.txt", quote = FALSE, row.names = FALSE)
write.table(DCA_real$Banabuiu$PI, "pi_real.txt", quote = FALSE, row.names = FALSE)

#First event
DCA_noH$Banabuiu$quadrant[DCA_noH$Banabuiu$date >= as.Date('1992-05-01') & DCA_noH$Banabuiu$date <= as.Date('1994-08-01')]
DCA_real$Banabuiu$quadrant[DCA_real$Banabuiu$date >= as.Date('1992-05-01') & DCA_real$Banabuiu$date <= as.Date('1994-08-01')]
#Second event
DCA_noH$Banabuiu$quadrant[DCA_noH$Banabuiu$date >= as.Date('1997-05-01') & DCA_noH$Banabuiu$date <= as.Date('2002-06-01')]
DCA_real$Banabuiu$quadrant[DCA_real$Banabuiu$date >= as.Date('1997-05-01') & DCA_real$Banabuiu$date <= as.Date('2002-06-01')]
#Third event
DCA_noH$Banabuiu$quadrant[DCA_noH$Banabuiu$date >= as.Date('2010-01-01') & DCA_noH$Banabuiu$date <= as.Date('2018-12-01')]
DCA_real$Banabuiu$quadrant[DCA_real$Banabuiu$date >= as.Date('2010-01-01') & DCA_real$Banabuiu$date <= as.Date('2018-12-01')]

#Comparison of the quadrants
df <- data.frame(date = DCA_real$Banabuiu$date, real = DCA_real$Banabuiu$quadrant, noh = DCA_noH$Banabuiu$quadrant)
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Scenario')
p <- ggplot(df) +
  geom_line(aes(date, value, colour = Scenario))
print(p)

#VD scatterplot
plot(DCA_noH$Banabuiu$WSI, DCA_real$Banabuiu$WSI)
plot(DCA_noH[["156"]]$WSI, DCA_real[["156"]]$WSI)

# Heatmap of variation -------------------------------------

