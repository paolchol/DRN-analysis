# SPI calculation for each subbasin
setwd("C:/Directory_thesis_codes")
source("./R/Libraries/Functions_CO.R")
library(rlist)

prec <- read.table("./Input/Model_input/Base/Time_series/rain_daily.dat", skip = 3)
columns <- c("date", "doy", subID$SubbasinID)
names(prec) <- columns
prec$date <- change_date_WASA_input(prec$date)
prec$doy <- NULL
prec <- monthly_scale(prec, f = sumx)

library(SPEI)

list_SPI <- list()
list_SPI <- apply(prec[, 2:ncol(prec)], 2, function(x) spi(x, 12))

list.save(list_SPI, "./Data/Analysis/DCA/SPI_list.RData")
