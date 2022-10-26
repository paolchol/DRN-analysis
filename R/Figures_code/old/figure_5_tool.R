#Figure 5 tool
setwd("C:/Directory_thesis_codes")
source("./R/Libraries/Libraries.R")
source("./R/Libraries/Functions.R")
source("./R/Libraries/Functions_MC.R")
source("./R/Libraries/Libraries_plot.R")

DCA_AR <- list.load("./Data/Analysis/DCA/DCA_AR.RData")

# df$Banabuiu <- NULL


#Based on Marengo et al., 2016
date_ranges <- data.frame(
  from = as.Date(c("1981-01-01", "1982-01-01", "1992-01-01", "1997-01-01", "2001-01-01", "2005-01-01", "2007-01-01", "2010-01-01")),
  to = as.Date(c("1981-12-01", "1983-12-01", "1993-12-01", "1998-12-01", "2002-12-01", "2005-12-01", "2007-12-01", "2018-12-01"))
)

# library(SPEI)
# load("./Data/Observations/precipitation.Rdata")
# 
# avgprec <- apply(prec[, 2:ncol(prec)], 1, mean)
# avgSPI <- spi(avgprec, 12)
# 
# df <- data.frame(date = DCA_AR$Banabuiu$date, avgSPI =avgSPI$fitted, old = DCA_AR$Banabuiu$PI)
# plot_df_interactive(df)
# 


#per fare il boxplot, rimuovere Banabuiu dal df

ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 2)}

find_bounds = function(df, x = 1, skip = 0, ma = FALSE, n = 5){
  minx <- function(x) min(x, na.rm = TRUE)
  maxx <- function(x) max(x, na.rm = TRUE)
  skip <- skip + 1
  lowbound <- apply(df[, skip:ncol(df)], x, minx)
  highbound <- apply(df[, skip:ncol(df)], x, maxx)
  if(ma){
    lowbound <- ma(lowbound, n)
    highbound <- ma(highbound, n)
  }
  bounds <- data.frame(date = df$date, low = lowbound, high = highbound)
  return(bounds)
}

df <- data.frame(date = DCA_AR$Banabuiu$date)
for(i in seq_len(length(DCA_AR))){
  df$new <- DCA_AR[[i]]$PI
  names(df)[names(df) == "new"] <- names(DCA_AR)[i]
}
newdf <- df[, 1:ncol(df)-1]
bounds <- find_bounds(newdf, 1, 1, ma = TRUE, 6)
bounds[bounds == Inf | bounds == -Inf] <- 0
df <- df[, c(1, ncol(df))]
df$lowbound <- bounds$low
df$highbound <- bounds$high
df$ma <- ma(df$Banabuiu, 6)

df <- df[14:nrow(df), ]

plot_df_interactive(df)

#https://stackoverflow.com/questions/4913117/gradient-in-geom-ribbon



col <- c("Mean precipitation" = "royalblue2",
         "Sub-basin variation" = "sandybrown")

col <- c("Mean precipitation" = "#293462",
         "Sub-basin variation" = "#F24C4C")
p <- ggplot() +
  #Add the droughts rectangles
  # geom_rect(data = date_ranges, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4) +
  #Add the sub-basins variation
  geom_ribbon(data = df, aes(x = date, ymin = lowbound, ymax = highbound, fill = "Sub-basin variation"), alpha = 0.5) +
  #Add the mean line
  geom_line(data = df, aes(x = date, y = ma, colour = "Mean precipitation"), size = 1.25) +
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
  scale_y_continuous(name = 'SPI-12', limits = c(-4, 4), breaks = seq(-4, 4, 1)) +
  scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y', limits = as.Date(c("1980-01-01", "2018-12-01")), expand = c(0,0)) +
  #Add the legend
  scale_color_manual(name = "", values = col) +
  scale_fill_manual(name = "", values = col) +
  theme(
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.position = "bottom",
        panel.background = element_rect(fill = "white", colour = "grey50")
        )

print(p)

plot.save(p, width = 1280, height = 657, filename = "./Plots/spi_v4.png")


#boxplot examples

# create a dataset
data <- data.frame(
  name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
  value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)

# Boxplot basic
data %>%
  ggplot( aes(x=name, y=value, fill=name)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Basic boxplot") +
  xlab("")

# Violin basic
data %>%
  ggplot( aes(x=name, y=value, fill=name)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart") +
  xlab("")
