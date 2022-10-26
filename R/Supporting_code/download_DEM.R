#Script to download the DEM of the Banabuiu basin
#from NASA database

path <- "Data/1112536144-download.txt"
links<-read.table(path)
for(i in seq_len(nrow(links))){
  browseURL(links[i,1])
}