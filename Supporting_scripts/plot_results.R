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

# Setup -------------------------------------------------------------------

path <- 'here insert the path to the folder where .Rdata is stored'
setwd(path)
# Example:
# path <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Share_Folder/Calibration_results"

#Load the necessary data
load('./observations.RData')
load('./baseline.RData')
load('./modelled.RData')

# Plot --------------------------------------------------------------------

#Write the reservoir you want to plot
ID_plot <- 125
plot_calibration(mod_df, base_df, obs_df, ID_plot, paste0('End of calibration - ', ID_plot))
