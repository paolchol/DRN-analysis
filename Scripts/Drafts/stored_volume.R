
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
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Scenario')

date_ranges <- date_ranges_SPI
ylabel <- (expression(paste("Stored Volume (monthly mean) [ ",m^3," ]", sep="")))
p <- ggplot() +
  #Add the drought rectangles
  geom_rect(data = date_ranges, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4) +
  #Add the Stored volume lines
  geom_line(data = df, aes(x = date, y = value, linetype = Scenario), alpha = 0.5, size = 1.25) +
  scale_y_continuous(name = ylabel) + #, limits = c(-1, 1)) +
  scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y', limits = as.Date(c("1980-01-01", "2018-12-01")), expand = c(0,0)) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))

print(p)

#add 



df <- data.frame(date = Dsv_r$date, centr = res_r_vol,
                 hdnrX100 = HDNR_vol*100, centr_noH = res_nH_vol,
                 tot_r = SV_R)
p <- plot_df_interactive(df, t = 'Stored volume visualization', y = "Monthly sum, m3", 
                         x = "Date", ret = TRUE)
p <- plotly::ggplotly(p)
htmlwidgets::saveWidget(p, "./stored_volume.html")

#Check on the order of magnitude of the ratio between
#HDNR and centralized reservoirs
Hmm <- HDNR_vol/30 #monthly mean
Rmm <- res_r_vol/30 #monthly mean
nHmm <- res_nH_vol/30

mean(Rmm/Hmm)/10^6
mean(Rmm)/10^9
mean(Hmm)/10^6

mean(Rmm)/mean(Hmm)/10^2
mean(Hmm)/mean(Rmm)

tot<-mean(res_r_vol)+mean(HDNR_vol)
mean(res_r_vol)/tot*100
2.75*10^9/(2.1*10^8)/100

#Drafts
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
