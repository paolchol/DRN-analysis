#Calculations

# NRMSE of VD before and after 1999 ---------------------------------------

#NMRSE function: in Functions_MC
#Data needed: DCA_plot

#Arrojado Lisboa
df <- data.frame(date = DCA_real[['156']]$date, Real = DCA_real[['156']]$WSI, noHDNR = DCA_noH[['156']]$WSI)
b1999 <- df[df$date <= "1999-12-01", ]
a1999 <- df[df$date > "1999-12-01", ]
RMSE(b1999$Real, b1999$noHDNR)
RMSE(a1999$Real, a1999$noHDNR)

#Mean of the upstream reservoirs
mean_r <- mean_no_AL(DCA_real)
mean_nH <- mean_no_AL(DCA_noH)
df <- data.frame(date = DCA_real[['Banabuiu']]$date, Real = mean_r, noHDNR = mean_nH)
b1999 <- df[df$date <= "1999-12-01", ]
a1999 <- df[df$date > "1999-12-01", ]
RMSE(b1999$Real, b1999$noHDNR)
RMSE(a1999$Real, a1999$noHDNR)
