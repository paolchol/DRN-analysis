#Temperature comparison between FUNCEME, INMET, both and Alexandre

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis")

source("Libraries.R")
source("Functions.R")

#Load Banabuiu reservoirs' names and IDs
path<-"C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis"
reservoirs<-read.table(paste0(path,"/reservoir_name_ID.txt"), header = TRUE, sep = "\t")

#Upload
path<-"C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/time_series_generation/output_time_series_gen"
temp_gen_both<-read.table(paste0(path,"/output_temp_save_v2.txt"), header = TRUE, sep = "\t")
temp_gen_FUNCEME<-read.table(paste0(path,"/output_temp_FUNCEME.txt"), header = TRUE, sep = "\t")
temp_gen_INMET<-read.table(paste0(path,"/output_temp_INMET.txt"), header = TRUE, sep = "\t")

path<-"C:/Thesis_fortran/WASA/Input/Time_series"
temp_alex<-read.table(paste0(path,"/temperature.dat"),skip = 3)
temp_gen_both<-temp_gen_both[1:11323,]
temp_gen_FUNCEME<-temp_gen_FUNCEME[1:11323,]
temp_gen_INMET<-temp_gen_INMET[1:11323,]
temp_alex<-temp_alex[1:11323,] #end in 2010

#Plots
time_index<-seq(as.Date("1980-01-01"),as.Date("2010-12-31"),by="days")
for(i in 1:nrow(reservoirs)){
  #Creation of the dataset for the visualization
  n<-as.numeric(reservoirs$SubbasinID[i])+2
  visual1<-data.frame(temp_alex[,n],rep("Alexandre",length(temp_alex[,n])),time_index)
  names(visual1)<-c("data","id","time")
  visual2<-data.frame(temp_gen_both[,n],rep("Both stations",length(temp_gen_both[,n])),time_index)
  names(visual2)<-c("data","id","time")
  visual3<-data.frame(temp_gen_FUNCEME[,n],rep("FUNCEME",length(temp_gen_FUNCEME[,n])),time_index)
  names(visual3)<-c("data","id","time")
  visual4<-data.frame(temp_gen_INMET[,n],rep("INMET",length(temp_gen_INMET[,n])),time_index)
  names(visual4)<-c("data","id","time")
  visual<-cbind(rbind(visual1,visual4,visual3,visual2))
  Legend<-visual$id
  p<-ggplot(data = visual,
            aes(x = `time`,
                y = `data`))+
    geom_line(aes(colour = Legend), size = 0.75, alpha = 0.4) +
    labs(x = "Date",
         y = "Temperature", 
         title = paste0("Subbasin ",reservoirs$SubbasinID[i]," - ",reservoirs$Reservoir_name[i]))+
    theme_ipsum()
  print(p)
  path<-"C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/plots/temperature_comparison/v2"
  ggsave(
    filename = paste0("0517_temp_",reservoirs$SubbasinID[i],".png"),
    plot = p,
      device = "png",
      path = path,
      scale = 1,
      width = 30,
      height = 20,
      units = c("cm"),
      dpi = 300,
      limitsize = TRUE
    )
}