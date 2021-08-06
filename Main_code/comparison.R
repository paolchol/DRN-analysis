#Comparison


# Setup -------------------------------------------------------------------

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis")

source("Libraries.R")
source("Functions.R")

#Load Banabuiu reservoirs' names and IDs
path<-"C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis"
reservoirs<-read.table(paste0(path,"/reservoir_name_ID.txt"), header = TRUE, sep = "\t")


# Visual comparison -------------------------------------------------------

#Load what you want to compare

path_import1 <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/time_series_generation/input_WASA_gen"
path_import2 <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Input/Time_series"
columns <- c("date", "doy", 123, 125, 126, 127, 134, 137, 138, 139, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160)

rad_gen_F<-read.table(paste0(path_import1, "/radiation_FUNCEME_1706.dat"), skip = 3)
rad_gen_I<-read.table(paste0(path_import1, "/radiation_INMET_1706.dat"), skip = 3)
rad_alex<-read.table(paste0(path_import2,"/radiation.dat"),skip = 3)

names(rad_alex) <- names(rad_gen_F) <- names(rad_gen_I) <- columns

rad_gen_F <- rad_gen_F[1:9711,]
rad_gen_I <- rad_gen_I[1:9711,]
rad_alex <- rad_alex[3805:nrow(rad_alex),]

#Since Alexandre's time series end in 2016, we need to cut the generated time series

#Plots

time_index<-seq(as.Date("1990-01-01"),as.Date("2016-08-02"),by="days")
for(i in 1:nrow(reservoirs)){
  #Creation of the dataset for the visualization
  n<-i+2
  visual1<-data.frame(rad_alex[,n],rep("Alexandre",length(rad_alex[,n])),time_index)
  names(visual1)<-c("data","id","time")
  visual2<-data.frame(rad_gen_I[,n],rep("INMET",length(rad_gen_I[,n])),time_index)
  names(visual2)<-c("data","id","time")
  #visual3<-data.frame(rad_gen_I[,n],rep("INMET",length(rad_gen_I[,n])),time_index)
  #names(visual3)<-c("data","id","time")
  visual<-cbind(rbind(visual1,visual2))#,visual3))
  Legend<-visual$id
  p<-ggplot(data = visual,
            aes(x = `time`,
                y = `data`))+
    geom_line(aes(colour = Legend),alpha = 0.5) +
    labs(x = "Date",
         y = "Precipitation", 
         title = paste0("Subbasin ",reservoirs$SubbasinID[i]," - ",reservoirs$Reservoir_name[i])) +
    theme_ipsum()
  print(p)
  # path<-"C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Analysis/plots/radiation"
  # ggsave(
  #   filename = paste0("radiation_",reservoirs$SubbasinID[i],".png"),
  #   plot = p,
  #   device = "png",
  #   path = path,
  #   scale = 1,
  #   width = 40,
  #   height = 30,
  #   units = "cm",
  #   dpi = 300,
  #   limitsize = TRUE
  # )
}
