#Data pre-processing

#NA management
# imputeTS::na_ma()

# Upload the main dataframe of temperature data
path <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/time_series_generation/main_datasets"
main_dataframe_temp<-read.table(paste0(path,"/main_dataframe_temperature_v2.txt"),
                                header = TRUE, sep="\t")

library(tictoc)
tic("NA removal with na_ma")
x<-imputeTS::na_ma(main_dataframe_temp$St356, k = 7, maxgap = 31)
plot(x,type='l',col='blue')
lines(main_dataframe_temp$St356,col='red')
toc() #47.98 sec


sum(is.na(main_dataframe_temp$St356))

#Usare la funzione sotto su tutti i main dataframe

remove_na = function(dataframe, skipcol = 1, k = 7, maxgap = 31){
  firstcol<-1+skipcol
  for(i in firstcol:ncol(dataframe)){
    dataframe[,i]<-imputeTS::na_ma(dataframe[,i], k = 7, maxgap = 31)
  }
  dataframe
}

tic("NA removal whole dataframe")
dat_noNA<-remove_na(main_dataframe_temp)
toc()
