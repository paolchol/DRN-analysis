#Different influence of the HDNR between downstream and upstream reservoirs

setwd("C:/Users/paolo/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")
source("./Libraries/Libraries.R")
source("./Libraries/Functions.R")

DCA_real <- list.load("./Data/DCA/DCA_real.RData")
DCA_noH <- list.load("./Data/DCA/DCA_noH.RData")

res <- readOGR("./Data/Reservoirs/centralized_res.shp")
res <- res@data

# Compute the % of drought phases for each reservoir ----------------------------

#Problem: each reservoir has a different SPI associated
#> different meteorological conditions can influence more the number of months
#> show also the cumulative number of months in a meteorological drought condition
# ordered by downstreamness

realDP <- nohDP <- data.frame(matrix(0, 4, length(res$subID)))
names(realDP) <- names(nohDP) <- res$subID
for(i in names(realDP)){
  for(q in 1:4){
    realDP[[i]][q] <- sum(DCA_real[[i]]$quadrant == q, na.rm = TRUE)
    nohDP[[i]][q] <- sum(DCA_noH[[i]]$quadrant == q, na.rm = TRUE)
  }
}
names(realDP) <- names(nohDP) <- paste0("x", res$subID)
realDP$x138 <- nohDP$x138 <- NULL
dum <- unname(apply(realDP, 2, function(x) sum(x)))
for(i in 1:ncol(realDP)){
  realDP[,i] <- realDP[,i]/dum[i]
  nohDP[,i] <- nohDP[,i]/dum[i]
}

# Compute the % of increase in each reservoir -----------------------------
#% of increase: in the hydrological drought phases (3 and 4)
#Here the different SPI doesn't bother the results, since they are increases between
#two scenarios, thus independent from each other
#Cosa fa geom_abline e cosa fa
#geom_smooth

increase <- realDP - nohDP
increase <- data.frame(ID = res$subID[res$subID != 138],
                       Dx = res$dx[res$subID != 138],
                       phase3 = t(increase[3,]*100),
                       phase4 = t(increase[4,]*100))
rownames(increase) <- NULL
names(increase) <- c('ID', 'Dx', 'Phase3', 'Phase4')
#change into "Hydro-meteorological" and "Hydrological"

#Scatterplot: % of increase vs downstreamness
df <- pivot_longer(increase, cols = 3:4, names_to = "Phase", values_to = "Increase")

df %>%
  ggplot(aes(Dx, Increase)) +
  geom_point(size = 3, alpha = 0.5) +
  geom_text(aes(label = ID, hjust = 0, vjust = 0)) +
  geom_smooth(method = lm, formula = y ~ x, se = T) +
  facet_wrap(~Phase) +
  labs(title = "Increase in hydrological related droughts from noHDNR to Real scenarios in each reservoir compared to its Dx",
       x = expression(paste('Downstreamness ','(D'['X'], ") [%]")),
       y = "Increase [%]") +
  theme_bw()

#Scatterplot: % of increase vs volume of small reservoirs
HDNR <- readOGR("./Data/HDNR/HDNR.shp")
HDNR <- HDNR@data
HDNR <- HDNR %>%
  group_by(SubbasinID) %>%
  summarise(tot = sum(capacity))
names(HDNR) <- c("ID", "volume")
increase$volume[order(increase$ID)] <- HDNR$volume[HDNR$ID %in% increase$ID]

df <- pivot_longer(increase, cols = 3:4, names_to = "Phase", values_to = "Increase")

df %>%
  ggplot(aes(volume/1e+07, Increase)) +
  geom_point(size = 3, alpha = 0.5) +
  geom_text(aes(label = ID, hjust = 0, vjust = 0)) +
  geom_smooth(method = lm, formula = y ~ x, se = T) +
  facet_wrap(~Phase) +
  labs(title = "Increase in hydrological related droughts from noHDNR to Real scenarios in each reservoir compared to the storage capacity of the HDNR in its subbasin",
       x = expression('Subbasin HDNR storage capacity [10'^7*' m'^3*']'),
       y = "Increase [%]") +
  theme_bw()

#Check 152
check <- data.frame(date = DCA_real[['152']]$date,
                 Real = DCA_real[['152']]$WSI, noHDNR = DCA_noH[['152']]$WSI)
plot_df_interactive(check)
