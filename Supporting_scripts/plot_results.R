#Script to plot the volumes resulting from the calibration
#compared with the observations and the baseline (non-calibrated model)
#Author: Paolo Colombo

# Libraries and function needed ---------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(plotly)

#If you don't have them installed, run the following lines
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("plotly")

plot_calibration = function(it, base, obs, ID, code){
  ID <- ID[which(ID %in% colnames(it))]
  vis_df <- data.frame(it$date, it[, which(colnames(it) %in% ID)],
                       base[, which(colnames(base) %in% ID)],
                       obs[, which(colnames(obs) %in% ID)])
  names(vis_df) <- c('date', paste0(ID, "_it"), paste0(ID, "_base")
                     , paste0(ID, "_obs"))
  df <- reshape2::melt(vis_df, id.vars = 'date', variable.name = 'Runs')
  p <- ggplot(df, aes(date, value)) + geom_line(aes(colour = Runs), alpha = 0.5, size = 1.2) +
    xlab('Date') + ylab('Value') + ggtitle(paste0('Baseline vs ', code))
  ggplotly(p)
}

plot_calibration_static = function(it, base, obs, ID, code){
  ID <- ID[which(ID %in% colnames(it))]
  vis_df <- data.frame(it$date, it[, which(colnames(it) %in% ID)],
                       base[, which(colnames(base) %in% ID)],
                       obs[, which(colnames(obs) %in% ID)])
  names(vis_df) <- c('date', "Calibrated", "Baseline"
                     , "Obervations")
  df <- reshape2::melt(vis_df, id.vars = 'date', variable.name = 'Legend')
  p <- ggplot(df, aes(date, value)) +
    geom_line(aes(linetype = Legend), alpha = 0.5, size = 1.2) +
    xlab('Date') + ylab(expression('Monthly volume [m'^3*']')) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y')
  print(p)
}

# Setup -------------------------------------------------------------------

path <- 'here insert the path to the folder where .Rdata is stored'
# Example:
path <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes"
setwd(path)

#Load the necessary data
load('./Inputs/Calibration/df_observations.RData')
load('./Inputs/Calibration/df_uncalibrated.RData')
load('./Data/Scenarios/Real/real_volumes.RData')
mod_df <- real_df

# Plot --------------------------------------------------------------------

#Write the reservoir you want to plot
ID_plot <- 156
plot_calibration(mod_df, base_df, obs_df, ID_plot, paste0('End of calibration - ', ID_plot))

plot_calibration_static(mod_df, base_df, obs_df, ID_plot, paste0('End of calibration - ', ID_plot))















