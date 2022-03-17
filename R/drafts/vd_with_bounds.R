#Arrojado Lisboa
df <- data.frame(date = DCA_real[['156']]$date, Real = DCA_real[['156']]$WSI, noHDNR = DCA_noH[['156']]$WSI)
plot_df_interactive(df)
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Scenario')

#Ranges of variation
vd_var_real <- data.frame(date = DCA_real[["156"]]$date)
vd_var_noH <- data.frame(date = DCA_noH[["156"]]$date)
for(i in 1:(length(DCA_real)-1)){
  vd_var_real$new <- DCA_real[[i]]$WSI
  vd_var_noH$new <- DCA_noH[[i]]$WSI
  names(vd_var_real)[names(vd_var_real) == "new"] <- names(DCA_real)[i]
  names(vd_var_noH)[names(vd_var_noH) == "new"] <- names(DCA_noH)[i]
}

bounds_real <- find_bounds(vd_var_real, 1, 1)
bounds_noH <- find_bounds(vd_var_noH, 1, 1)

#Drought periods from Marengo 2016
date_ranges <- data.frame(
  from = as.Date(c("1981-01-01", "1982-01-01", "1992-01-01", "1997-01-01", "2001-01-01", "2005-01-01", "2007-01-01", "2010-01-01")),
  to = as.Date(c("1981-12-01", "1983-12-01", "1993-12-01", "1998-12-01", "2002-12-01", "2005-12-01", "2007-12-01", "2018-12-01"))
)

col <- c("Sub-basins variation (Real)" = "plum", "Sub-basins variation (noHDNR)" = "plum1")
p <- ggplot() +
  #Add the drought rectangles
  geom_rect(data = date_ranges, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4) +
  #Add sub-basins
  geom_line(vd_var_real)
  #Add the VD lines
  geom_line(data = df, aes(x = date, y = value, linetype = Scenario), alpha = 0.5, size = 1.25) +
  #Modify the axis
  scale_y_continuous(name = 'Volume Deficit (VD)', limits = c(-1, 1)) +
  scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y', limits = as.Date(c("1980-01-01", "2018-12-01")), expand = c(0,0)) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  scale_fill_manual(name = "", values = col)

print(p)
