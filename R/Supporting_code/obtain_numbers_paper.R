setwd("C:/Directory_thesis_codes")
source("./R/Libraries/Libraries.R")
source("./R/Libraries/Functions.R")

# Percentage increment ----------------------------------------------------

# Calculate the percentage increment in the drought phases ----------------

DCA_AR <- list.load("./Data/Analysis/DCA/DCA_real2.RData")
DCA_LR <- list.load("./Data/Analysis/DCA/DCA_noH2.RData")
DCA_SR <- list.load("./Data/Analysis/DCA/DCA_SR.RData")
DCA_N <- list.load("./Data/Analysis/DCA/DCA_N.RData")
perc_increment = function(xi, xf){
  return((xf-xi)/xi * 100)
}

perc_increment(10, 19)

#Table of number of months in the phases
table <- data.frame(matrix(NA, 4, 4))
names(table) <- c("AR", "LR", "SR", "N")
for(i in 1:4){
  table$`AR`[i] <- sum(DCA_AR[['156']]$quadrant == i, na.rm = TRUE)
  table$`LR`[i] <- sum(DCA_LR[['156']]$quadrant == i, na.rm = TRUE)
  table$`SR`[i] <- sum(DCA_SR$quadrant == i, na.rm = TRUE)
  table$`N`[i] <- sum(DCA_N$quadrant == i, na.rm = TRUE)
}

#Table of percentage increment
table_perc <- data.frame(matrix(NA, 4, 4))
names(table_perc) <- c("AR", "LR", "SR", "N")
for (i in 1:4){
  table_perc[i, 1] <- perc_increment(table[i, 2], table[i, 1])
  table_perc[i, 3] <- perc_increment(table[i, 4], table[i, 3])
}

#Print overall increase
for (i in c(1, 3)){
  xi <- table[3, i+1] + table[4, i+1]
  xf <- table[3, i] + table[4, i]
  print(paste0(names(table)[i], ' increase: ', perc_increment(xi, xf)))
}

# Percentage increment in the Dsc and Dsv ---------------------------------

#Dsc
load("./Data/Analysis/Downstreamness/Dsc_AR.Rdata")
load("./Data/Analysis/Downstreamness/Dsc_LR.Rdata")

Dsc_AR <- Dsc_r
Dsc_LR <- Dsc_nH

pi <- perc_increment(Dsc_LR$Dsc, Dsc_AR$Dsc)
print(paste0('Average percentage variation of AR Dsc compared to LR: ',mean(pi)))

max(pi)
min(pi)

#Dsv
load("./Data/Analysis/Downstreamness/Dsv_AR.Rdata")
load("./Data/Analysis/Downstreamness/Dsv_LR.Rdata")
load("./Data/Analysis/Downstreamness/Dsv_SR.Rdata")
load("./Data/Analysis/Downstreamness/Dsv_N.Rdata")

#AR and LR
Dsv_AR <- Dsv_r
Dsv_LR <- Dsv_nH
pi_ARLR <- as.data.frame(perc_increment(Dsv_LR$Dsv, Dsv_AR$Dsv))
names(pi_ARLR) <- 'value'
pi_ARLR$date <- Dsv_AR$date
plot(pi_ARLR$date, pi_ARLR$value)
plot_df_interactive(pi_ARLR)
#Before 2000
mean(pi_ARLR$value[which(pi_ARLR$date < '2000-01-01')])
pi_ARLR$date[which(pi_ARLR$value == min(pi_ARLR$value))]
#After 2000
mean(pi_ARLR$value[which(pi_ARLR$date > '2000-01-01')])
x <- min(pi_ARLR$value[which(pi_ARLR$date > '2000-01-01')])
pi_ARLR$date[which(pi_ARLR$value == x)]

#SR and N
pi_NSR <- as.data.frame(perc_increment(Dsv_N$Dsv, Dsv_SR$Dsv))
names(pi_NSR) <- 'value'
pi_NSR$date <- Dsv_SR$date

plot_df_interactive(pi_NSR)
mean(pi_NSR$value)
pi_NSR$date[which(pi_NSR$value == min(pi_NSR$value))]

# Transition acceleration --------------------------------------------------

drought_periods <- data.frame(
  from = as.Date(c("1980-12-01", "1987-05-01", "1992-05-01", "1997-05-01", "2001-02-01", "2005-02-01", "2010-05-01", "2012-05-01", "2014-06-01")),
  to = as.Date(c("1984-03-01", "1988-04-01", "1994-04-01", "1999-12-01", "2002-02-01", "2005-12-01", "2010-12-01", "2014-01-01", "2018-12-01"))
)

transition_acceleration = function(DRN, noDRN){
  if (sum(noDRN < 0) != 0){
    return((sum(DRN < 0) - sum(noDRN < 0))/sum(noDRN < 0) * 100)
  }else{
    return(0)
  }
}

accs <- matrix(NA, dim(drought_periods)[1], 1)
for (dp in seq_len(dim(drought_periods)[1])){
  from <- drought_periods$from[dp]
  to <- drought_periods$to[dp]
  
  date <- DCA_AR$`156`$date[which(DCA_AR$`156`$date < to & DCA_AR$`156`$date >= from)]
  AR <- DCA_AR$`156`$WSI[which(DCA_AR$`156`$date < to & DCA_AR$`156`$date >= from)]
  LR <- DCA_LR$`156`$WSI[which(DCA_LR$`156`$date < to & DCA_LR$`156`$date >= from)]
  
  # df <- data.frame(date, AR, LR)
  # plot(plot_df_interactive(df, ret = TRUE, t = paste0('drought period ', dp)))
  acc <- transition_acceleration(AR, LR)
  print(paste0(from, ' - ', to))
  print(acc)
  accs[dp] <- acc
}

print(paste0('Mean acceleration in transition in AR: ', mean(accs)))
mean(accs[accs > 0])


# Recharge extension --------------------------------------------------------

drought_recovery <- drought_periods[1:8, ]
# names(drought_recovery) <- c('from', 'to')
for (i in seq_len(dim(drought_recovery)[1])){
  drought_recovery$from[i] <- drought_periods$to[i]
  drought_recovery$to[i] <- drought_periods$from[i+1]
}

recharge_extension = function(DRN, noDRN){
  if (sum(noDRN > 0) != 0){
    return((sum(DRN > 0) - sum(noDRN > 0))/sum(noDRN > 0) * 100)
  }else{
    return(0)
  }
}

means <- matrix(NA, dim(drought_recovery)[1], 1)
for (dp in seq_len(dim(drought_recovery)[1])){
  from <- drought_recovery$from[dp]
  to <- drought_recovery$to[dp]
  
  date <- DCA_AR$`156`$date[which(DCA_AR$`156`$date < to & DCA_AR$`156`$date >= from)]
  AR <- DCA_AR$`156`$WSI[which(DCA_AR$`156`$date < to & DCA_AR$`156`$date >= from)]
  LR <- DCA_LR$`156`$WSI[which(DCA_LR$`156`$date < to & DCA_LR$`156`$date >= from)]
  
  df <- data.frame(date, AR, LR)
  plot(plot_df_interactive(df, ret = TRUE, t = paste0('recharge period ', dp)))
  
  AR_diff <- data.frame(date[which(diff(AR) > 0) + 1], diff(AR)[diff(AR) > 0])
  LR_diff <- data.frame(date[which(diff(LR) > 0) + 1], diff(LR)[diff(LR) > 0])
  names(AR_diff) <- names(LR_diff) <- c('date', 'value')
  
  pi <- data.frame(perc_increment(LR_diff$value[LR_diff$date %in% AR_diff$date],AR_diff$value[AR_diff$date %in% LR_diff$date]))
  names(pi) <- 'value'
  pi$date <- AR_diff$date[AR_diff$date %in% LR_diff$date]
  
  print(paste0(from, ' - ', to))
  print(mean(pi$value))
  means[dp] <- mean(pi$value)
}
means <- means[means < 0]
print(paste0("Mean decrease of recharge' speed: ", mean(means)))

# Dsc vs Dsv --------------------------------------------------------------

Dsc_long <- data.frame(Dsv_AR$date)
names(Dsc_long) <- 'date'
Dsc_long$value <- NA

for (year in Dsc_AR$date){
  value <- Dsc_AR$Dsc[Dsc_AR$date == year]
  Dsc_long$value[format(as.POSIXct(Dsc_long$date), '%Y') %in% year] <- value
}

sum(Dsc_long$value > Dsv_AR$Dsv)/length(Dsv_AR$Dsv)*100


# Dsv modified ------------------------------------------------------------

load("./Data/Analysis/Downstreamness/Dsv_AR_new.Rdata")
load("./Data/Analysis/Downstreamness/Dsv_LR_new.Rdata")
load("./Data/Analysis/Downstreamness/Dsv_N_new.Rdata")
load("./Data/Analysis/Downstreamness/Dsv_SR_new.Rdata")

idx <- which(Dsv_AR$date > "2000-01-01")
mod <- sum(Dsv_AR$Dsv_modified[idx] < Dsv_LR$Dsv[idx])/length(Dsv_AR$date[idx])*100
or <- sum(Dsv_AR$Dsv[idx] < Dsv_LR$Dsv[idx])/length(Dsv_AR$date[idx])*100

perc_increment(or, mod)
perc_increment(sum(Dsv_AR$Dsv[idx] < Dsv_LR$Dsv[idx]), sum(Dsv_AR$Dsv_modified[idx] < Dsv_LR$Dsv[idx]))

mean(perc_increment(Dsv_N$Dsv, Dsv_SR$Dsv))

mean(perc_increment(Dsv_LR$Dsv, Dsv_AR$Dsv_modified))
mean(perc_increment(Dsv_LR$Dsv, Dsv_AR$Dsv))
mean(perc_increment(Dsv_N$Dsv, Dsv_SR$Dsv_modified))


# Dsc increases -----------------------------------------------------------

load("./Data/Analysis/Downstreamness/Dsc_AR.Rdata")

Dsc_r$increase <- 0
for (i in 2:nrow(Dsc_r)){
  Dsc_r$increase[i] <- perc_increment(Dsc_r$Dsc[i-1], Dsc_r$Dsc[i])
}

Dsc_r[order(Dsc_r$increase, decreasing = F), 1]

write.table(Dsc_r, './Data/Analysis/Downstreamness/Dsc_res.txt')

perc_increment(Dsc_r$Dsc[Dsc_r$date == 1995], Dsc_r$Dsc[Dsc_r$date == 2000])

