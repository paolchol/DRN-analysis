#Script to download the DEM of the Banabuiu basin

path <- "C:/Directory_thesis_codes/Data/1112536144-download.txt"
links <- read.table(path)
for(i in 1:nrow(links)){
  browseURL(links[i,1])
}

