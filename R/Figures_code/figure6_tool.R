setwd("C:/Directory_thesis_codes")
DCA_AR <- list.load("./Data/Analysis/DCA/DCA_AR.RData")
DCA_LR <- list.load("./Data/Analysis/DCA/DCA_LR.RData")
DCA_SR <- list.load("./Data/Analysis/DCA/DCA_SR.RData")
DCA_N <- list.load("./Data/Analysis/DCA/DCA_N.RData")

library(paletteer)

#Drought date ranges (retrieved from SPI results)
date_ranges <- data.frame(
  from = as.Date(c("1980-12-01", "1987-05-01", "1992-05-01", "1997-05-01", "2001-02-01", "2005-02-01", "2010-05-01", "2012-05-01", "2014-06-01")),
  to = as.Date(c("1984-03-01", "1988-04-01", "1994-04-01", "1999-12-01", "2002-02-01", "2005-12-01", "2010-12-01", "2014-01-01", "2018-12-01"))
)

df <- data.frame(date = DCA_AR[['156']]$date,
                 AR = DCA_AR[['156']]$WSI, LR = DCA_LR[['156']]$WSI,
                 SR = DCA_SR$WSI, N = DCA_N$WSI)
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Scenario')

p <- ggplot() +
  #Add the drought rectangles
  geom_rect(data = date_ranges, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.2) +
  #Add the VD lines
  geom_line(data = df, aes(x = date, y = value, linetype = Scenario, col = Scenario), alpha = 0.9, size = 1.25) +
  #VD = 0 line
  geom_hline(yintercept = 0, color = 'tomato2', linetype = "dashed", size = 1, alpha = 0.8) +
  scale_y_continuous(name = 'Volume Deviation (VD)', limits = c(-1, 1)) +
  scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y', limits = as.Date(c("1980-01-01", "2018-12-01")), expand = c(0,0)) +
  scale_color_paletteer_d("colorBlindness::paletteMartin") +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.position = "bottom")
print(p)

plot.save(p, width = 1920, height = 1080, filename = "./Figures and tables/paper/Figure_6.png")
