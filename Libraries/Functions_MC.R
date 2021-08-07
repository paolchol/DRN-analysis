#Functions for Model calibration

#Other functions needed
setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis")
source("Functions_TG.R")

#Library needed
library(hydroGOF)

# Dataframe operations -----------------------------------------------------

remove_high_values = function(df, maxval, skip = 1){
  skip = skip + 1
  for(i in skip:ncol(df)){
    df[,i][df[,i] > maxval[i-skip+1]] <- maxval[i-skip+1]
  }
  return(df)
}

date_col_WASA_out = function(df){
  start <- as.Date(df$day[1]-1, origin = paste0(df$year[1], "-01-01"))
  end <- as.Date(df$day[nrow(df)]-1, origin = paste0(df$year[nrow(df)], "-01-01"))
  dates <- create_date_vector(lubridate::year(start), lubridate::year(end),
                              lubridate::month(start), lubridate::month(end),
                              lubridate::day(start), lubridate::day(end))
  df$date <- dates
  return(df)
}

load_WASA_results = function(path, IDs, dates){
  #position<-c(0,7,13,19,25,35,45,55,68,81,91,101,111,121,131,141,155,169)
  #widths<-diff(position)
  columns<-c("Subasin-ID", "year", "day", "hour", "qlateral",
             "transposition", "inflow", "evap",
             "prec", "intake", "overflow",
             "qbottom", "qout", "withdrawal", "elevation", "area", "volume")
  files <- paste0(path, "/res_", IDs$ID,"_watbal.out")
  data_list<-list()
  for(i in 1:length(files)){
    data_list[[i]]<-read.table(files[i], skip = 3, header = FALSE)
    names(data_list[[i]]) <- columns
  }
  #Add the date column to the vector
  for(i in 1:length(data_list[])){
    data_list[[i]] <- date_col_WASA_out(data_list[[i]])
  }
  df <- create_main_dataframe(dates, data_list, IDs, timecol = 18, datacol = 17)
  return(df)
}

sumx = function(x){ return(sum(x, na.rm = TRUE)) }

monthly_scale = function(df, skip = 1, f = sum){
  skip = skip + 1
  df[,1] <- lubridate::floor_date(df[,1], unit = "month")
  month_date <- df %>%
    group_by(date) %>%
    summarize()
  month_df <- data.frame(month_date)
  for(i in skip:ncol(df)){
    month_df$new <- 0
    for(j in 1:nrow(month_df)){
      month_df$new[j] <- f(df[,i][df[,1] == month_df$date[j]])
    }
    names(month_df)[names(month_df) == "new"]<-colnames(df)[i]
  }
  return(month_df)
}

# Visual comparison -------------------------------------------------------

plot_comparison = function(mod_df, obs_df, skip = 1, y = "Value", label = "Title", interactive = FALSE, file = "plot",
                          save = FALSE, path){
  skip <- skip +1
  for(i in skip:ncol(mod_df)){
    vis1 <- data.frame(mod_df[,1], mod_df[,i], rep("Modelled", nrow(mod_df)))
    names(vis1) <- c("date", "value", "ID")
    vis2 <- data.frame(obs_df[,1], obs_df[,i], rep("Observed", nrow(obs_df)))
    names(vis2) <- c("date", "value", "ID")
    df<-rbind(vis1, vis2)
    p <- ggplot(df, aes(date, value)) + geom_line(aes(colour = ID), alpha = 0.5, size = 2) +
      xlab("Date") + ylab(y) + ggtitle(paste0(label, " - ", colnames(mod_df)[i]))
    if(interactive){
      #ggplotly(p)
      #Saves an interactive version
      htmlwidgets::saveWidget(plotly::ggplotly(p), paste0(file,".html"))
    }else if(save){
      #Saves a static version
      ggsave(
        filename = paste0(file,"_", colnames(mod_df)[i],".png"),
        plot = p,
        device = "png",
        path = path,
        scale = 1,
        width = 40,
        height = 30,
        units = "cm",
        dpi = 300,
        limitsize = TRUE
      )
    }else{
      print(p)
    }
  }
}

plot_comp_FR_SR = function(res1, res2, y = "Value", label = "Title", interactive = FALSE, file = "plot",
                           save = FALSE, path = "nopath", named = TRUE, colID, colval){
  options(scipen = 999)
  if(named){
    vis1 <- data.frame(names(res1), unname(res1), rep("First Run", length(res1)))
    names(vis1) <- c("Subbasin", "value", "ID")
    vis2 <- data.frame(names(res2), unname(res2), rep("Second Run", length(res2)))
    names(vis2) <- c("Subbasin", "value", "ID") 
  }else{
    vis1 <- data.frame(res1[,colID], res1[,colval], rep("First Run", nrow(res1)))
    names(vis1) <- c("Subbasin", "value", "ID")
    vis2 <- data.frame(res2[,colID], res2[,colval], rep("Second Run", nrow(res2)))
    names(vis2) <- c("Subbasin", "value", "ID")
  }
  
  df<-rbind(vis1, vis2)
  p <- ggplot(df, aes(Subbasin, value)) + geom_point(aes(colour = ID), alpha = 0.5, size = 6) +
    xlab("Subbasin") + ylab(y) + ggtitle(label)
  if(interactive){
    #ggplotly(p)
    #Saves an interactive version
    if(path == "nopath") htmlwidgets::saveWidget(plotly::ggplotly(p), paste0(file,".html"))
    else htmlwidgets::saveWidget(plotly::ggplotly(p), paste0(path, "/",file,".html"))
  }else if(save){
    #Saves a static version
    ggsave(
      filename = paste0(file,".png"),
      plot = p,
      device = "png",
      path = path,
      scale = 1,
      width = 40,
      height = 30,
      units = "cm",
      dpi = 300,
      limitsize = TRUE
    )
  }else{
    print(p)
  }
}

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

plot_stat_calibration = function(list, code, path, file, save = FALSE){
  options(scipen = 999)
  for(i in 1:length(list)){
    if(i == 1) df <- data.frame(list[[i]]$ID)
    df <- data.frame(df, list[[i]][,2])
    if(i == length(list)) names(df) <- c("ID",names(list))
  }
  df <- reshape2::melt(df, id.vars = 'ID', variable.name = 'Indicator')
  p <- ggplot(df, aes(ID, value)) + geom_point(aes(colour = Indicator), alpha = 0.5, size = 2) +
    xlab('Sub-basin') + ylab('Value') + ggtitle(paste0("Performance - ",code))
  #print(p)
  if(save) htmlwidgets::saveWidget(plotly::ggplotly(p), paste0(path, "/",file,".html"))
  else ggplotly(p)
}

plot_diff = function(diff_, comp = FALSE, base, it){
  options(scipen = 999)
  if(comp) diff_ <- diff_baseline(base, it)
  for(i in 1:length(diff_)){
    if(i == 1) df_diff <- data.frame(diff_$r2$ID)
    df_diff <- data.frame(df_diff, diff_[[i]]$diff)
    if(i == length(diff_)) names(df_diff) <- c("ID",names(diff_))
  }
  df <- reshape2::melt(df_diff, id.vars = 'ID', variable.name = 'Indicator')
  p <- ggplot(df, aes(ID, value)) + geom_point(aes(colour = Indicator), alpha = 0.5, size = 2) +
    xlab('Sub-basin') + ylab('Value') + ggtitle(paste0('Baseline vs ', code))
  ggplotly(p)
}

# Performance evaluation --------------------------------------------------

## R2 (Coefficient of determination)

rsq <- function (x, y) cor(x, y) ^ 2

r2_computation <- function(mod_df, obs_df, skip = 1){
  skip <- skip + 1
  r2 <- data.frame(ID = colnames(mod_df)[skip:ncol(mod_df)], result = 0)
  for(i in skip:ncol(mod_df)){
    r2$result[i-skip+1] <- rsq(mod_df[,i], obs_df[,i])
  }
  return(r2)
}

## NSE (Nash-Sutcliffe efficiency)
#N.B.: Calculated on the discharge. Ask for this data
#In the meantime use it on the volume data as well
#NSE_index wil be a vector containing the NSE for each column
#Function: NSE, in the hydroGOF package

## PBIAS (% of bias)
#PBIAS wil be a vector containing the NSE for each column
#Function: pbias, in the hydroGOF package

## KGE (Kling-Gupta Efficiency)
#N.B.: same as NSE
#KGE_index wil be a vector containing the NSE for each column
#Function: KGE, in the hydroGOF package

## RMSE and NRMSE

RMSE_NRMSE = function(mod_df, obs_df, skip = 1, df = TRUE){
  RMSE = function(m, o){
    #m: model values
    #o: observed values
    sqrt(mean((m - o)^2, na.rm = TRUE))
  }
  if(df){
    skip <- skip + 1
    report <- data.frame(ID = colnames(mod_df)[skip:ncol(mod_df)], RMSE = 0, NRMSE = 0)
    for(i in skip:ncol(mod_df)){
      report$RMSE[i-skip+1] <- RMSE(mod_df[,i], obs_df[,i])
      m <- min(obs_df[,i], na.rm = TRUE)
      M <- max(obs_df[,i], na.rm = TRUE)
      report$NRMSE[i-skip+1] <- report$RMSE[i-skip+1]/(M - m)
    }
  }else{
    report <- data.frame(ID = 'var', RMSE = 0, NRMSE = 0)
    report$RMSE <- RMSE(mod_df, obs_df)
    m <- min(obs_df, na.rm = TRUE)
    M <- max(obs_df, na.rm = TRUE)
    report$NRMSE <- report$RMSE/(M - m)
  }
  return(report)
}

# Semi-automatic performance evaluation -----------------------------------

gen_code = function(s, v){
  code <- paste0("s",s,"v",v)
  if(length(code) > 1){
    cc <- code[1]
    for(i in 2:length(code)){
      cc <- paste0(cc, code[i])
    }
  }else{
    cc <- code
  }
  return(cc)
}

choose_sub = function(sub){
  to156 <- c(155, 144)
  to160 <- c(157, 158, 159)
  if(sub %in% to156) sub <- 156
  else if(sub %in% to160) sub <- 160
  return(sub)
}

modify_scaling_factor = function(pathin, pathout, subID, values,
                                 name = "scaling_factor"){
  #Modifies the scaling factor
  #Load the basic file
  sc <- read.table(pathin, sep = "\t", skip = 1)
  names(sc) <- c("ID", "value")
  #Change the values
  sc$value[sc$ID %in% subID] <- values
  #Saves the new file in the Input folder
  h <- "Subasin-ID.\tmean kf-calib-factor"
  my.write(sc, paste0(pathout, "/", name, ".dat"), h, write.table)
}

WASA_calibration_evaluation = function(path, IDs, maxcap, obs, code, results,
                                       mean_results, complete = FALSE, start_obs = FALSE,
                                       keep_mean = TRUE, st_date = 1980, end_date = 2018){
  #Load the output data from the path (path)
  #Compute the indicators using the observations (obs)
  #Save the results in a new column in "results", with "code" as a name 
  
  #Load
  date <- create_date_vector(st_date, end_date)
  df <- load_WASA_results(path, IDs, dates = date)
  df <- remove_high_values(df, maxcap)
  df <- monthly_scale(df, f = sumx)
  
  #Evaluate
  r2 <- r2_computation(df, obs)
  NSE_index <- NSE(df[,2:ncol(df)], obs[,2:ncol(obs)])
  PBIAS <- pbias(df[,2:ncol(df)], obs[,2:ncol(obs)])
  KGE_index <- KGE(df[,2:ncol(df)], obs[,2:ncol(obs)])
  R_NRMSE_indexes <- RMSE_NRMSE(df, obs)
  
  if(start_obs){
    for(i in 2:ncol(obs)){
      #Extract the column
      #Obtain the year of the first non-na value of the column
      x <- min(which(obs[,i] != 0)) #condition to find the first non-na value
      date <- obs$date[x:nrow(obs)]
      val <- obs[x:nrow(obs), i]
      obs_i <- data.frame(date, val)
      #obtain obs_i by taking the column
      #Cut the obs and df column starting from that year
      df_i <- df[which(df$date %in% obs_i$date), i]
      #Compute the indicators' values by calling the functions on single vectors
      #and placing the results in the dataframes created above
      r2$result[i-1] <- rsq(df_i, obs_i$val)
      NSE_index[i-1] <- NSE(df_i, obs_i$val)
      PBIAS[i-1] <- pbias(df_i, obs_i$val)
      KGE_index[i-1] <- KGE(df_i, obs_i$val)
      rep <- RMSE_NRMSE(df_i, obs_i$val, df = FALSE)
      R_NRMSE_indexes$NRMSE[i-1] <- rep$NRMSE
    }
  }
  
  #Save the results
  #Mean results
  if(keep_mean){
    mean_results$new <- 0
    mean_results$new[1] <- mean(r2$result)
    mean_results$new[2] <- mean(NSE_index)
    mean_results$new[3] <- mean(abs(PBIAS))
    mean_results$new[4] <- mean(KGE_index)
    mean_results$new[5] <- mean(R_NRMSE_indexes$NRMSE)
    names(mean_results)[names(mean_results) == "new"] <- code
    format(mean_results, scientific = F)
  }else{
    mean_results <- "No mean results kept"
  }
  #Complete results
  if(complete){
    NSE_index <- data.frame(IDs, unname(NSE_index))
    PBIAS <- data.frame(IDs, PBIAS)
    KGE_index <- data.frame(IDs, unname(KGE_index))
    R_NRMSE_indexes[,2] <- NULL
    complete_results = list(r2 = r2, NSE = NSE_index, PBIAS = PBIAS,
                            KGE = KGE_index, NRMSE = R_NRMSE_indexes)
    return(list(mean = mean_results, complete = complete_results))
  }else{
    return(mean_results)
  }
}

diff_baseline = function(base, it){
  diff_base <- list()
  ID <- base[[1]]$ID
  for(i in 1:length(base)){
    name <- names(base)[i]
    diff <- base[[i]][,2] - it[[i]][,2]
    diff_base[[name]] <- data.frame(ID, diff)
  }
  return(diff_base)
}
