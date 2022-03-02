#Show the evolution of drought phases

# Setup -------------------------------------------------------------------

setwd("C:/Users/paolo/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")
source("./Libraries/Libraries.R")
source("./Libraries/Functions.R")

DCA_real <- list.load("./Data/DCA/DCA_real.RData")
DCA_noH <- list.load("./Data/DCA/DCA_noH.RData")


# Function ----------------------------------------------------------------

phase_perc = function(input){
  #for(y in df$date[]) kept, just for the sintax
  input$date <- lubridate::year(input$date)
  df <- data.frame(date = seq(1981, 2018, 1), p1 = 0, p2 = 0, p3 = 0, p4 = 0)
  for(i in 1:4){
    c <- input %>%
      filter(date >= 1981) %>%
      group_by(date) %>%
      filter(value == i) %>%
      summarise(n = n())
    df[[i+1]][df$date %in% c$date] <- c$n
  }
  df[, 2:5] <- df[, 2:5]/12
  return(df)
}

rename_phase = function(df){
  df$Phase[df$Phase == "p1"] <- "1. Non occurrence"
  df$Phase[df$Phase == "p2"] <- "2. Meteorological"
  df$Phase[df$Phase == "p3"] <- "3. Hydro-meteorological"
  df$Phase[df$Phase == "p4"] <- "4. Hydrological"
  return(df)
}

# Stacked area plot -------------------------------------------------------
#Use the % of months in the different drought phases extracted from Arrojado Lisboa
#time series

#Compute the % of drought phases for each year
real <- data.frame(date = DCA_real[['156']]$date, value = DCA_real[['156']]$quadrant)
noh <- data.frame(date = DCA_noH[['156']]$date, value = DCA_noH[['156']]$quadrant)

df_real <- phase_perc(real)
df_real <- pivot_longer(df_real, cols = 2:5, names_to = "Phase", values_to = "Percentage")
df_real <- rename_phase(df_real)

df_noh <- phase_perc(noh)
df_noh <- pivot_longer(df_noh, cols = 2:5, names_to = "Phase", values_to = "Percentage")
df_noh <- rename_phase(df_noh)

#Plot (area)
plot_R <- ggplot(df_real, aes(x = date, y = Percentage, fill = Phase)) +
  geom_area(alpha = 0.6, size = 1, colour = 'black') +
  labs(title = "Real scenario") +
  scale_x_continuous(name = "Date", breaks = seq(1981, 2018, 2)) +
  theme(legend.position = "bottom") +
  guides(x = guide_axis(n.dodge = 2)) +
  scale_fill_brewer(palette = "Purples")

plot_nH <- ggplot(df_noh, aes(x = date, y = Percentage, fill = Phase)) +
  geom_area(alpha = 0.6, size = 1, colour = 'black') +
  labs(title = "noHDNR scenario") +
  scale_x_continuous(name = "Date", breaks = seq(1981, 2018, 2)) +
  theme(legend.position = "bottom") +
  guides(x = guide_axis(n.dodge = 2)) +
  scale_fill_brewer(palette = "Purples")
p3 <- grid.arrange(plot_R, plot_nH, ncol = 2)
plot.save(p3, width = 1280, height = 657, filename = "./Plots/drought_evolution.png")


RColorBrewer::display.brewer.all()
#Plot (bars)

#doesn't work
ggplot(df_noh, aes(fill = Phase, y = Percentage, x = date)) +
  geom_bar(position = "fill")

