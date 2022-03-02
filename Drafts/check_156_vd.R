table1 <- data.frame(matrix(NA, 5, 3))
names(table1) <- c("real", "noH", "obs")
for(i in 1:4){
  table1$real[i] <- sum(DCA_real[["156"]]$quadrant == i, na.rm = TRUE)
  table1$noH[i] <- sum(DCA_noH[["156"]]$quadrant == i, na.rm = TRUE)
  table1$obs[i] <- sum(DCA_obs[["156"]]$quadrant == i, na.rm = TRUE)
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

write.table(table2, file = "./Data/Plot_table/cumulative_phases_156.txt", sep = "\t", quote = FALSE, 
            row.names = TRUE)


df <- data.frame(date = DCA_real[["156"]]$date, real = DCA_real[["156"]]$WSI, noH = DCA_noH[["156"]]$WSI)
plot_df_interactive(df)

df <- data.frame(date = DCA_real[["Banabuiu"]]$date, real = DCA_real[["Banabuiu"]]$WSI, noH = DCA_noH[["Banabuiu"]]$WSI)
plot_df_interactive(df)

