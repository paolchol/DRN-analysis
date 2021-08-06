#Script to download the DEM of the Banabuiu basin

path<-"C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/NASA_DEM/1112536144-download.txt"
links<-read.table(path)
for(i in 1:nrow(links)){
  browseURL(links[i,1])
}

