#Spatial DCA
#Drafts and trials to obtain the network chart


# Setup -------------------------------------------------------------------

setwd("C:/Users/paolo/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")

#igraph
library("igraph")

#ggnet
library(GGally)
library(network)
library(sna)
library(ggplot2)


# Network chart -----------------------------------------------------------

links <- data.frame(
  source = c(154, 153, 150, 145, 149, 155, 146, 144,
             142, 157, 158, 151, 152, 159, 148, 147, 143, 160),
  target = c(150, 149, 155, 155, 144, 144, 156, 156,
             157, 160, 160, 160, 159, 160, 143, 143, 156, 143)
)

#igraph
network <- graph_from_data_frame(d = links, directed = F)
plot(layout_nicely(network))
plot(network)

#ggnet
net <- network(links, directed = TRUE)
ggnet2(net)

ggnet2(net, mode = "kamadakawai")

left_branch <- c(154, 150, 145, 155,
                 153, 149, 144,
                 127, 123,
                 146, 156,
                 126,
                 125,
                 138)
right_branch <- c(142, 157,
                  158,
                  151,
                  152, 159, 160,
                  148,
                  147,
                  143, 156) #re-calibration of 156 after the results of the right branch
subbasins <- c(left_branch, right_branch)
no_res_sub <- c(134, 137, 139, 144, 155, 157, 158, 159)

links_res <- data.frame(
  source = c(154, 153, 145, 149, 146, 150,
             142, 151, 152, 148, 147, 160, 143),
  target = c(150, 149, 156, 156, 156, 156,
             160, 160, 160, 143, 143, 143, 156)
)
net <- network(links_res, directed = TRUE)
ggnet2(net)

# Map ---------------------------------------------------------------------

install.packages("cartography")
library(cartography)
library(rgdal)

subbasins<-readOGR("./Data/Shapefile/subbasins_cut_geomfix.shp")




