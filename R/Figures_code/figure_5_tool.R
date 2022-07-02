#Figure 5 tool

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