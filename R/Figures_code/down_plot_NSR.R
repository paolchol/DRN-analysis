#Downstreamness on N and SR scenarios

# Setup -------------------------------------------------------------------

#Directory and paths
setwd("C:/Directory_thesis_codes")
path_maxcap <- "./Input/Model_input/Base/Reservoir/reservoir.dat"

source("./Libraries/Libraries.R")
source("./Libraries/Functions.R")
# source("./Libraries/Functions_TG.R")
# source("./Libraries/Functions_DP.R")
source("./Libraries/Functions_CO.R")
source("./Libraries/Functions_MC.R")
source("./Libraries/Functions_AN.R")

#Load the IDs of the subbasins in the Banabuiu region
load("./Input/General/IDs.RData")
subID <- read.table("./Inputs/General/reservoir_name_ID.txt", header = TRUE, sep = "\t")

get_mean_volume = function(df, df_class, df_vol, c){
  ss <- names(df_class)[2:ncol(df_class)]
  for (i in seq_len(length(ss))){
    s <- ss[i]
    n <- sum(df$class == c & df$SubbasinID == as.numeric(s))
    id_select <- df$id[which(df$class == c & df$SubbasinID == as.numeric(s))]
    vol <- df_class[s]/n
    df_vol[, names(df_vol) %in% id_select] <- vol
  }
  return(df_vol)
}

# Load the reservoirs shapefiles ----------------------------------------------

DRN <- readOGR("./Data/DRN/DRN.shp")
res <- readOGR("./Data/Reservoirs/centralized_res.shp")

#Create dfs from the shapefiles attribute tables
DRN_df <- DRN@data
res_df <- res@data

#Remove the shapefiles (not needed anymore)
remove(DRN)
remove(res)

# Load the model output ---------------------------------------------------
#Needed to compute the monthly Dsv

#Centralized
#SR scenario
load("./Data/Scenarios/SR/ALhdnr_volumes.RData")
#N scenario
load("./Data/Scenarios/N/ALonly_volumes.RData")

#DNR
#SR scenario
load("./Data/Scenarios/SR/class1.RData")
load("./Data/Scenarios/SR/class2.RData")
load("./Data/Scenarios/SR/class3.RData")
load("./Data/Scenarios/SR/class4.RData")
load("./Data/Scenarios/SR/class5.RData")

HDNR_vol <- data.frame(matrix(NA, nrow(class1), nrow(HDNR_df)))
names(HDNR_vol) <- HDNR_df$id
HDNR_vol$date <- class1$date
col_order <- c("date", HDNR_df$id)
HDNR_vol <- HDNR_vol[, col_order]

HDNR_vol <- get_mean_volume(HDNR_df, class1, HDNR_vol, 1)
HDNR_vol <- get_mean_volume(HDNR_df, class2, HDNR_vol, 2)
HDNR_vol <- get_mean_volume(HDNR_df, class3, HDNR_vol, 3)
HDNR_vol <- get_mean_volume(HDNR_df, class4, HDNR_vol, 4)
HDNR_vol <- get_mean_volume(HDNR_df, class5, HDNR_vol, 5)


# Dsv ---------------------------------------------------------------------

dx_hdnr <- HDNR_df$dx
dx_res <- res_df$dx[res_df$subID == 156]

tic("Dsv SR computation")
Dsv_SR <- data.frame(date = ALhdnr$date)
Dsv_SR$Dsv <- 0
for(i in seq_len(nrow(Dsv_SR))){
  num <- sum(HDNR_vol[i, 2:ncol(HDNR_vol)]*dx_hdnr) + sum(ALhdnr[i, 2] * dx_res)
  den <- sum(HDNR_vol[i, 2:ncol(HDNR_vol)]) + sum(ALhdnr[i, 2])
  Dsv_SR$Dsv[i] <- num / den
}

Dsv_SR$noH <- 0
for(i in seq_len(nrow(Dsv_SR))){
  num <- sum(ALhdnr[i, 2:ncol(ALhdnr)]*dx_res)
  den <- sum(ALhdnr[i, 2:ncol(ALhdnr)])
  Dsv_SR$noH[i] <- num/den
}
toc()
plot_df_interactive(Dsv_SR)

tic("Dsv noH computation")
Dsv_N <- data.frame(date = ALonly$date)
Dsv_N$Dsv <- 0
for(i in seq_len(nrow(Dsv_N))){
  num <- sum(ALonly[i, 2]*dx_res)
  den <- sum(ALonly[i, 2])
  Dsv_N$Dsv[i] <- num/den
}
toc()
plot(Dsv_N)

#Save
save(Dsv_N, file = "./Data/Downstreamness/Dsv_N.Rdata")
save(Dsv_SR, file = "./Data/Downstreamness/Dsv_SR.Rdata")

# Plot --------------------------------------------------------------------

load("./Data/Downstreamness/Dsv_N.Rdata")
load("./Data/Downstreamness/Dsv_SR.Rdata")

df <- data.frame(date = Dsv_SR$date, SR = Dsv_SR$Dsv, N = Dsv_N$Dsv, noh = Dsv_SR$noH)
#df <- data.frame(date = Dsv_SR$date, SR = Dsv_SR$Dsv, N = Dsv_N$Dsv)
#df <- rename_scenarios(df)
df <- reshape2::melt(df, id.vars = 'date', variable.name = 'Scenario')
df$Scenario <- as.character(df$Scenario)
df['Scenario'][df['Scenario'] == "noh"] <- "SR (DRN influence removed)"
#To check the columns: str(df)

date_ranges <- data.frame(
  from = as.Date(c("1980-12-01", "1987-05-01", "1992-05-01", "1997-05-01", "2001-02-01", "2005-02-01", "2010-05-01", "2012-05-01", "2014-06-01")),
  to = as.Date(c("1984-03-01", "1988-04-01", "1994-04-01", "1999-12-01", "2002-02-01", "2005-12-01", "2010-12-01", "2014-01-01", "2018-12-01"))
)
col <- c("#181D31", "#FFC947", "#678983")
#col <- c("#E2703A", "#046582")
p <- ggplot() +
  geom_rect(data = date_ranges, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), alpha = 0.4) +
  geom_line(data = df, aes(x = date, y = value, colour = Scenario, linetype = Scenario), size = 1.25, alpha = 0.8) +
  scale_y_continuous(name = expression(paste('D'['SV'], " [%]")), limits = c(70, 75)) +
  scale_x_date(name = 'Date', date_breaks = "2 years", date_labels = '%Y', limits = as.Date(c("1980-01-01", "2018-12-01")), expand = c(0,0)) +
  scale_color_manual(values = col) +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))

print(p)

plot.save(p, width = 1280, height = 657, filename = "./Plots/Dsv_NSR.png")













