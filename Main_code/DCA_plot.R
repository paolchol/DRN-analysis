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

df <- reshape2::melt(SPI_df, id.vars = 'date', variable.name = 'Subbasins')
df$color <- rep(0, nrow(df))
df$color[df$Subbasins == 'Banabuiu'] <- 1

p <-  ggplot(df, aes(date, value)) +
      geom_line(aes(colour = Subbasins), alpha = 0.5, size = 1) +
      xlab('Date') + ylab('SPI-12')
plot(p)

#Salvalo così e passa a downstr
ggsave(
  filename = "./Data/Plot_table/spi_v1.1.png",
  plot = p,
  width = 20,
  height = 8,
  units = 'cm'
)

#Plot the single sub-basins spi as a "cloud", with the same colour, and the banabuiu
#with another color that can emerge

# Figure: VD mean comparison -------------------------------------------

df <- data.frame(date = DCA_real[['Banabuiu']]$date, Real = DCA_real[['Banabuiu']]$WSI, noHDNR = DCA_noH[['Banabuiu']]$WSI)
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Scenario')


p <- ggplot(df, aes(date, value)) + 
      geom_line(aes(linetype = Scenario), alpha = 0.5, size = 1.25) +
      xlab('Date') + ylab('VD') + 
  scale_y_continuous(name = 'Volume Deficit (VD)', limits = c(-1, 1)) +
  scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y')
plot(p)

#Increment the size of the axis labels

# Table: switch from phase to another? ------------------------------------


