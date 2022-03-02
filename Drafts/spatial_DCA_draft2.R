
setwd("C:/Users/paolo/OneDrive - Politecnico di Milano/Backup PC/Uni/Thesis/Directory_thesis_codes")

library(rgdal)
library(ggplot2)
library(rgeos)
library(tidyr)
library(dplyr)


subs <- readOGR("./Data/Shapefile/subbasins_cut_geomfix.shp")

subs_f <- fortify(subs)
head(subs_f)
subs$id <- row.names(subs)
subs_f <- left_join(subs_f, subs@data)

p <- ggplot(data = subs_f,
            aes(x = long, y = lat, fill = SubbasinID, group = group)) +
  geom_polygon() +
  geom_path(colour = 'black', lwd = 0.05) +
  coord_equal() +
  #facet_wrap(~) +
  scale_fill_gradient2(low = "blue", mid = "grey", high = "red",
                       midpoint = 150, name = "Code")

print(p)

#manually fill the subbasins
#Make a scheme of what needs to be done