#New functions to create

#Check stations who present negative values, print their names

#Remove the stations which present negative values

#Maxima analysis for precipitation data



#Radiation: they could have values lower than 1000
#place NA when it happens

find_anomalous_stations(rad_input, 1000)

replace_value = function(input_list, value){
  j <- find_anomalous_stations(input_list, value)
  if(j > 0){
    for(i in 1:length(input_list[[2]])){
      input_list[[2]][[i]]$data[input_list[[2]][[i]]$data < value] <- NA
    }
  }else{
    print("There are no anomalous stations")
  }
  return(input_list)
}

rad_input_fix <- replace_value(rad_input_fix, 1000)


for(i in 1:length(rad_input_fix[[2]])){
  check_outlier_IQR(rad_input_fix[[2]][[i]], skip = 3)
}


rad_input_fix[[2]] <- add_date_column(rad_input_fix[[2]])



plot_stations(rad_input_fix, 4, 5, label = "Radiation")

rad_input[[2]] <- add_date_column(rad_input[[2]])
plot_stations(rad_input, 4, 5, label = "Radiation")

for(i in 1:length(rad_input[[2]])){
  print(paste0("Station ", i))
  check_outlier_3sd(rad_input[[2]][[i]], skip = 3)
}


for(i in 1:length(rad_input[[2]])){
  print(paste0("Station ", i))
  check_outlier_IQR(rad_input[[2]][[i]], skip = 3, 4)
}

for(i in 1:length(rad_input_fix[[2]])){
  rad_input[[2]][[i]] <- remove_outlier_IQR(rad_input_fix[[2]][[i]], skip = 3)
}