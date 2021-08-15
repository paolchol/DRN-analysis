#Create a unique shapefile with all the informations included

#Load the HDNR shapefile created after the R processing



# Setup -------------------------------------------------------------------

setwd("C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")

source("./Libraries/Libraries.R")
source("./Libraries/Functions.R")

# Load HDNR ---------------------------------------------------------------

path <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/HDRN/HDRN_afterR_v2.shp"
HDNR <- readOGR(path)

#Remove unnecessary columns from the attribute table
HDNR@data$join_DN <- NULL
HDNR@data$join_fid <- NULL
HDNR@data$join_layer <- NULL
HDNR@data$join_path <- NULL
HDNR@data$distance <- NULL
names(HDNR@data)[2] <- "area"

#Add the coordinates in the attribute table
HDNR@data$X <- HDNR@coords[, 1]
HDNR@data$Y <- HDNR@coords[, 2]

#Save the new shapefile
writeOGR(HDNR, dsn = "./Data/HDNR", layer = "HDNR_Banabuiu", driver = "ESRI Shapefile")

# Centralized reservoirs --------------------------------------------------

#Load the maximum capacities of the reservoirs
path_maxcap <- "C:/Thesis_fortran/Directory_WASA_Banabuiu/Input/Reservoir/reservoir.dat"
reservoirs <- read.table(path_maxcap, skip = 2, sep = "\t")
maxcap <- reservoirs[,c(1,5)]
names(maxcap) <- c('ID','max')
maxcap[,2] <- maxcap[,2]*1000 #m3

#Load the new shapefile
path <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes/Data/Reservoirs"
new_main <- readOGR(paste0(path, "/main_reservoirs.shp"))

#Load old shapefiles
path <- "C:/Users/Utente/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Data/shapefile/Banabuiu"
old_I <- readOGR(paste0(path, "/mainreservoirsI_cut_ref.shp"))
old_II <- readOGR(paste0(path, "/mainreservoirsII_cut_ref.shp"))

subID <- c(156,
154,
127,
126,
160,
146,
152,
149,
123,
147,
125,
143,
148,
145,
150,
153,
151)

new_main@data$subID <- 0
new_main@data[order(new_main@data$name), 4] <- subID
new_main@data[order(new_main@data$subID), 3] <- maxcap$max[maxcap$ID %in% new_main@data$subID]
View(new_main@data)


old_II@data$FID_ <- c(18, 19)
old_II@data$E__WGS84_ <- NULL
old_II@data$N <- NULL
old_II@data$BhidrÃ.uli <- NULL
old_II@data$MunicÃ.pio <- NULL
names(old_II@data) <- c("id", "name", "capacity", "subID")
old_II@data$name <- c("Curral Velho", "Umari")
old_II@data$capacity <- maxcap$max[maxcap$ID %in% old_II@data$subID]
View(old_II@data)


path <- "./Data/Reservoirs"
writeOGR(new_main, dsn = "./Data/Reservoirs", layer = "to_reproject", driver = "ESRI Shapefile")
writeOGR(old_II, dsn = "./Data/Reservoirs/intermediate", layer = "to_merge", driver = "ESRI Shapefile")



# load --------------------------------------------------------------------

HDNR <- readOGR("./Data/HDNR/HDNR_reproj.shp")

res_class<-c(
  5000, #m3
  25000,
  50000,
  100000,
  max(HDNR$capacity)+1
)

obtain_class <- function(res_class, capacity){
  
  classes <- capacity
  classes[capacity < res_class[1]] <- 1
  classes[capacity < res_class[2] & capacity >= res_class[1]] <- 2
  classes[capacity < res_class[3] & capacity >= res_class[2]] <- 3
  classes[capacity < res_class[4] & capacity >= res_class[3]] <- 4
  classes[capacity > res_class[4]] <- 5
  
  return(classes)
}


HDNR$class <- obtain_class(res_class, HDNR$capacity)

writeOGR(HDNR, dsn = "./Data/HDNR", layer = "HDNR", driver = "ESRI Shapefile")

res <- readOGR("./Data/Reservoirs/main_reservoirs.shp")



