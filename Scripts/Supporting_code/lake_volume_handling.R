#Handle the small reservoirs results of the WASA model

# Setup -------------------------------------------------------------------

setwd("C:/Users/paolo/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")

source("./Libraries/Libraries.R")
source("./Libraries/Functions.R")
source("./Libraries/Functions_MC.R")

# New function -----------------------------------------------------------

extract_class = function(df, class_n, n_col = 4){
  df <- df[df[, n_col] == class_n, ]
  return(df)
}

# Load the results of the model ------------------------------------------
#path <- "./Data/Scenarios/Real/Model_output"
path <- "./Data/Scenarios/SR/Model_output"
WASA_hdnr <- read.table(paste0(path, "/lake_volume.out"), skip = 2)
col <- unlist(strsplit(readLines(paste0(path, "/lake_volume.out"),n = 3)[2], "            "))
col <- c("year", "month", "day", "class", col[4:length(col)])
names(WASA_hdnr) <- col

#Extract each class
class1 <- extract_class(WASA_hdnr, 1)
class2 <- extract_class(WASA_hdnr, 2)
class3 <- extract_class(WASA_hdnr, 3)
class4 <- extract_class(WASA_hdnr, 4)
class5 <- extract_class(WASA_hdnr, 5)

c <- rep(4, 12)
sum(c[1:6])
sum(c[7:12])

#Obtain monthly values (sum)
date <- create_date_vector(1980, 2018)
class1$year <- date; class1$month <- NULL; class1$day <- NULL; class1$class <- NULL
names(class1)[1] <- "date"
class2$year <- date; class2$month <- NULL; class2$day <- NULL; class2$class <- NULL
names(class2)[1] <- "date"
class3$year <- date; class3$month <- NULL; class3$day <- NULL; class3$class <- NULL
names(class3)[1] <- "date"
class4$year <- date; class4$month <- NULL; class4$day <- NULL; class4$class <- NULL
names(class4)[1] <- "date"
class5$year <- date; class5$month <- NULL; class5$day <- NULL; class5$class <- NULL
names(class5)[1] <- "date"

class1 <- monthly_scale(class1)
class2 <- monthly_scale(class2)
class3 <- monthly_scale(class3)
class4 <- monthly_scale(class4)
class5 <- monthly_scale(class5)

#Order the columns
col_order <- c("date", sort(names(class1)[2:ncol(class1)]))
class1 <- class1[, col_order]
class2 <- class2[, col_order]
class3 <- class3[, col_order]
class4 <- class4[, col_order]
class5 <- class5[, col_order]

# Save the results --------------------------------------------------------

#path_save <- "./Data/Scenarios/Real"
path_save <- "./Data/Scenarios/SR"
save(class1, file = paste0(path_save, "/class1.RData"))
save(class2, file = paste0(path_save, "/class2.RData"))
save(class3, file = paste0(path_save, "/class3.RData"))
save(class4, file = paste0(path_save, "/class4.RData"))
save(class5, file = paste0(path_save, "/class5.RData"))

# Mean volumes ------------------------------------------------------------

get_mean_volume = function(df, df_class, df_vol, c){
  ss <- names(df_class)[2:ncol(df_class)]
  for(i in 1:length(ss)){
    s <- ss[i]
    n <- sum(df$class == c & df$SubbasinID == as.numeric(s))
    id_select <- df$id[which(df$class == c & df$SubbasinID == as.numeric(s))]
    
    vol <- df_class[s]/n
    df_vol[, names(df_vol) %in% id_select] <- vol
  }
  return(df_vol)
}

#Obtain monthly values (mean)
date <- create_date_vector(1980, 2018)
class1$year <- date; class1$month <- NULL; class1$day <- NULL; class1$class <- NULL
names(class1)[1] <- "date"
class2$year <- date; class2$month <- NULL; class2$day <- NULL; class2$class <- NULL
names(class2)[1] <- "date"
class3$year <- date; class3$month <- NULL; class3$day <- NULL; class3$class <- NULL
names(class3)[1] <- "date"
class4$year <- date; class4$month <- NULL; class4$day <- NULL; class4$class <- NULL
names(class4)[1] <- "date"
class5$year <- date; class5$month <- NULL; class5$day <- NULL; class5$class <- NULL
names(class5)[1] <- "date"

class1 <- monthly_scale(class1, f = mean)
class2 <- monthly_scale(class2, f = mean)
class3 <- monthly_scale(class3, f = mean)
class4 <- monthly_scale(class4, f = mean)
class5 <- monthly_scale(class5, f = mean)

#Order the columns
col_order <- c("date", sort(names(class1)[2:ncol(class1)]))
class1 <- class1[, col_order]
class2 <- class2[, col_order]
class3 <- class3[, col_order]
class4 <- class4[, col_order]
class5 <- class5[, col_order]

HDNR <- readOGR("./Data/HDNR/HDNR.shp")
HDNR_df <- HDNR@data
remove(HDNR)

HDNR_vol_mean <- data.frame(matrix(NA, nrow(class1), nrow(HDNR_df)))
names(HDNR_vol_mean) <- HDNR_df$id
HDNR_vol_mean$date <- class1$date
col_order <- c("date", HDNR_df$id)
HDNR_vol_mean <- HDNR_vol_mean[, col_order]

HDNR_vol_mean <- get_mean_volume(HDNR_df, class1, HDNR_vol_mean, 1)
HDNR_vol_mean <- get_mean_volume(HDNR_df, class2, HDNR_vol_mean, 2)
HDNR_vol_mean <- get_mean_volume(HDNR_df, class3, HDNR_vol_mean, 3)
HDNR_vol_mean <- get_mean_volume(HDNR_df, class4, HDNR_vol_mean, 4)
HDNR_vol_mean <- get_mean_volume(HDNR_df, class5, HDNR_vol_mean, 5)

save(HDNR_vol_mean, file = "./Data/Downstreamness/HDNR_vol_mean.Rdata")

