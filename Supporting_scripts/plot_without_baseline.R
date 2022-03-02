
source("./Libraries/Functions_MC.R")
load("./Data/volume_obs.RData")
load("./Data/Scenarios/Real/real_daily.RData")

obs <- monthly_scale(obs_df, f = mean)
it <- monthly_scale(real_df, f = mean)
ID <- 156
code <- "Arrojado Lisboa"


ID <- ID[which(ID %in% colnames(it))]
vis_df <- data.frame(it$date, it[, which(colnames(it) %in% ID)],
                     obs[, which(colnames(obs) %in% ID)])
names(vis_df) <- c('date', "Calibrated", "Observations")
df <- reshape2::melt(vis_df, id.vars = 'date', variable.name = 'Legend')
p <- ggplot(df, aes(date, value/1e+9)) +
  geom_line(aes(linetype = Legend, color = Legend), alpha = 0.8, size = 1.25) +
  xlab('Date') + ylab(expression('Monthly volume (mean) [ km'^3*' ]')) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y') +
  scale_color_manual(values = c("salmon1", "royalblue")) +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        legend.position = "bottom")
plot(p)
