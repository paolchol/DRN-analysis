#Observation daily dataframe generation

path_obs <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/Observed_volumes/volume_series_banabuiu_reservoirs_ID"
obs_input <- create_data_list(path_obs, c('date', 'volume'), skip = 1, f = read.csv, s = ",")
date <- create_date_vector(1980, 2018)
for(i in 1:length(obs_input[[2]])){
  obs_input[[2]][[i]]$date <- change_date_format(obs_input[[2]][[i]]$date)
}
obs_df <- create_main_dataframe(date, obs_input[[2]], IDs, timecol = 1, datacol = 2)
obs_df <- remove_high_values(obs_df, maxcap$max)

save(obs_df, file = "./Data/volume_obs.RData")
