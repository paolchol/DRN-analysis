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
  #Add lines at the reservoirs construction
  geom_vline(xintercept = 1992, size = 1.25, alpha = 0.5, color = 'cornflowerblue') +
  geom_text(aes(1992, 55, label = "Cipoada", hjust = -0.25)) +
  geom_vline(xintercept = 1996, size = 1.25, alpha = 0.5, color = 'cornflowerblue') +
  geom_text(aes(1996, 55, label = "Fogareiro", hjust = -0.25)) +
  geom_vline(xintercept = 1988, size = 1.25, alpha = 0.5, color = 'cornflowerblue') +
  geom_text(aes(1988, 55, label = "Patu", hjust = -0.25)) +
  geom_vline(xintercept = 2000, size = 1.25, alpha = 0.5, color = 'cornflowerblue') +
  geom_text(aes(2000, 55, label = "Pirabibu", hjust = -0.25)) +
  geom_vline(xintercept = 2011, size = 1.25, alpha = 0.5, color = 'cornflowerblue') +
  geom_text(aes(2011, 55, label = "Umari", hjust = -0.25)) +
  geom_vline(xintercept = 2007, size = 1.25, alpha = 0.5, color = 'cornflowerblue') +
  geom_text(aes(2007, 55, label = "Curral Velho", hjust = -0.25)) +
  scale_x_continuous(name = "Date", breaks = seq(1980, 2018, 2), limits = c(1980, 2018)) +
  scale_y_continuous(name = expression('D'['SC']), limits = c(40, 55),
                     breaks = seq(40, 55, 2.5)) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
print(p)

min(Dsc_r$Dsc)
max(Dsc_nH$Dsc)

df$noHDNR - df$Real
mean(df$noHDNR - df$Real)

# Dsv results -------------------------------------------------------------

#Plot: stored volume in hdnr vs stored volume in centralized
#sum all centr and all hdnr, plot on the same graph

load("./Data/Downstreamness/Dsv_r.Rdata")
load("./Data/Downstreamness/Dsv_nH.Rdata")

df <- data.frame(date = Dsv_r$date, Real = Dsv_r$Dsv, noHDNR = Dsv_nH$Dsv)
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Scenario')

date_ranges <- data.frame(
  from = as.Date(c("1981-01-01", "1982-01-01", "1992-01-01", "1997-01-01", "2001-01-01", "2005-01-01", "2007-01-01", "2010-01-01")),
  to = as.Date(c("1981-12-01", "1983-12-01", "1993-12-01", "1998-12-01", "2002-12-01", "2005-12-01", "2007-12-01", "2018-12-01"))
)

p <- ggplot() +
  geom_line(data = df, aes(x = date, y = value, linetype = Scenario), size = 1.5, alpha = 0.5) +
  geom_rect(data = date_ranges, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4) +
  scale_y_continuous(name = expression('D'['SV']), limits = c(25, 65)) +
  scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y', limits = as.Date(c("1980-01-01", "2018-12-01")), expand = c(0,0)) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
print(p)

mean(Dsv_r$Dsv)
mean(Dsv_nH$Dsv)

# Volume stored in centralized and hdnr ---------

load("./Data/Downstreamness/Dsv_r.Rdata")
load("./Data/Downstreamness/HDNR_vol.Rdata")
#Centralized
#Real scenario
load("./Data/Scenarios/Real/real_volumes.RData")
#NoH scenario
load("./Data/Scenarios/No_HDNR/nohdnr_volumes.RData")

HDNR_vol <- apply(HDNR_vol[, 2:ncol(HDNR_vol)], 1, sum)
res_r_vol <- apply(real_df[, 2:ncol(real_df)], 1, sum)
res_nH_vol <- apply(noH_df[, 2:ncol(noH_df)], 1, sum)


sum(HDNR_vol*mean(HDNR_df$dx)) + sum(res_r_vol*mean(res_df$dx)) / (sum(HDNR_vol) + sum(res_r_vol))


df <- data.frame(date = Dsv_r$date, HDNR = HDNR_vol*100, C.Real = res_r_vol, C.noHDNR = res_nH_vol)
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Legend')

p <- df %>%
  ggplot(aes(date, value)) + 
  geom_line(aes(colour = Legend), size = 1.5, alpha = 0.5)
#facet_wrap(~vol, nrow = 2)
print(p)

#The scales have to be set up

# scale_y_continuous(name = expression('D'['SV']), limits = c(25, 65)) +
# scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y')

#Drafts to show also the Dsv together
df2 <- data.frame(date = Dsv_r$date, Real = Dsv_r$Dsv, noHDNR = Dsv_nH$Dsv)
df2 <- reshape2::melt(df2, id.vars = 'date', variable.name = 'Legend')

df <- rbind(df2, df)

df$vol[df$Legend == 'HDNR'] <- 1
df$vol[df$Legend == 'C.Real'] <- 1
df$vol[df$Legend == 'C.noHDNR'] <- 1
df$vol[is.na(df$vol)] <- 0

# Dsc vs Dsv --------------------------------------------------------------

load("./Data/Downstreamness/Dsc_r.Rdata")
load("./Data/Downstreamness/Dsc_nH.Rdata")

load("./Data/Downstreamness/Dsv_r.Rdata")
load("./Data/Downstreamness/Dsv_nH.Rdata")

dt <- Dsv_r$date
rdsc <- data.frame(date = dt, dsc = 0)
nhdsc <- data.frame(date = dt, dsc = 0)

for(i in 1:length(Dsc_r$date)){
  rdsc$dsc[lubridate::year(rdsc$date) == Dsc_r$date[i]] <- Dsc_r$Dsc[i]
  nhdsc$dsc[lubridate::year(nhdsc$date) == Dsc_nH$date[i]] <- Dsc_nH$Dsc[i]
}

df <- data.frame(
  date = Dsv_r$date,
  'Dsv-Real' = Dsv_r$Dsv,
  'Dsv-noHDNR' = Dsv_nH$Dsv,
  'Dsc-Real' = rdsc$dsc,
  'Dsc-noHDNR' = nhdsc$dsc
)
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Legend')


date_ranges <- data.frame(
  from = as.Date(c("1981-01-01", "1982-01-01", "1992-01-01", "1997-01-01", "2001-01-01", "2005-01-01", "2007-01-01", "2010-01-01")),
  to = as.Date(c("1981-12-01", "1983-12-01", "1993-12-01", "1998-12-01", "2002-12-01", "2005-12-01", "2007-12-01", "2018-12-01"))
)

p <- ggplot() +
  geom_line(data = df, aes(x = date, y = value, colour = Legend), size = 1.5, alpha = 0.5) +
  geom_rect(data = date_ranges, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4) +
  scale_y_continuous(name = expression('D'['SV']), limits = c(25, 65)) +
  scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y', limits = as.Date(c("1980-01-01", "2018-12-01")), expand = c(0,0)) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
#facet_wrap(~vol, nrow = 2)
print(p)




