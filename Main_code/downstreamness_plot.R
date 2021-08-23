#Downstreamness plots

# Setup -------------------------------------------------------------------

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")
source("./Libraries/Libraries.R")

# Dsc results -------------------------------------------------------------

#Decrease the interval of x axis, to 1 or 5 years
#Plot the two lines in the same graph

load("./Data/Downstreamness/Dsc_r.Rdata")
load("./Data/Downstreamness/Dsc_nH.Rdata")

df <- data.frame(date = Dsc_r$date, Real = Dsc_r$Dsc, noHDNR = Dsc_nH$Dsc)
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Scenario')

p <- df %>%
  ggplot(aes(date, value)) + 
  geom_line(aes(linetype = Scenario), size = 1.5, alpha = 0.5) +
  scale_x_continuous(name = "Date", breaks = seq(1980, 2018, 2)) +
  scale_y_continuous(name = expression('D'['SC']), limits = c(40, 55),
                     breaks = seq(40, 55, 2.5))

min(Dsc_r$Dsc)
max(Dsc_nH$Dsc)


# Dsv results -------------------------------------------------------------

#Plot: stored volume in hdnr vs stored volume in centralized
#sum all centr and all hdnr, plot on the same graph

load("./Data/Downstreamness/Dsv_r.Rdata")
load("./Data/Downstreamness/Dsv_nH.Rdata")

df <- data.frame(date = Dsv_r$date, Real = Dsv_r$Dsv, noHDNR = Dsv_nH$Dsv)
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Scenario')

p <- df %>%
  ggplot(aes(date, value)) + 
  geom_line(aes(linetype = Scenario), size = 1.5, alpha = 0.5) +
  scale_y_continuous(name = expression('D'['SV']), limits = c(25, 65)) +
  scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y')

plot(p)

#Mettere linee verticali in corrispondenza di siccità

#Dsv results vs volume stored in centralized and hdnr

load("./Data/Downstreamness/HDNR_vol.Rdata")
#Centralized
#Real scenario
load("./Data/Scenarios/Real/real_volumes.RData")
#NoH scenario
load("./Data/Scenarios/No_HDNR/nohdnr_volumes.RData")



HDNR_vol <- apply(HDNR_vol[, 2:ncol(HDNR_vol)], 1, sum)
res_r_vol <- apply(real_df[, 2:ncol(real_df)], 1, sum)
res_nH_vol <- apply(noH_df[, 2:ncol(noH_df)], 1, sum)

df <- data.frame(date = Dsv_r$date, HDNR = HDNR_vol, C.Real = res_r_vol, C.noHDNR = res_nH_vol)
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Legend')

df2 <- data.frame(date = Dsv_r$date, Real = Dsv_r$Dsv, noHDNR = Dsv_nH$Dsv)
df2 <- reshape2::melt(df2, id.vars = 'date', variable.name = 'Legend')

df <- rbind(df2, df)

df$vol[df$Legend == 'HDNR'] <- 1
df$vol[df$Legend == 'C.Real'] <- 1
df$vol[df$Legend == 'C.noHDNR'] <- 1

df$vol[is.na(df$vol)] <- 0

p <- df %>%
  ggplot(aes(date, value)) + 
  geom_line(aes(linetype = Legend), size = 1.5, alpha = 0.5) +
  facet_wrap(~vol, nrow = 2)
print(p)

#The scales have to be set up

  # scale_y_continuous(name = expression('D'['SV']), limits = c(25, 65)) +
  # scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y')


