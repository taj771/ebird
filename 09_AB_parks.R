rm(list = ls())
library(rgdal)
library(ggplot2)
library(raster)
library(sf)
library(data.table)
library(maptools)
library(maps)
library(rgeos)
library(tidyverse)
library(GISTools)
library(sp)
library(rgeos)
library(polyclip)
library(spatialEco)
library(dplyr)

#read shape file
# please make working directry as "parks_and_protected_areas_alberta
shp <- readOGR(dsn = ".","Parks_Protected_Areas_Alberta" )
shp <-  spTransform(shp,CRS("+proj=longlat +datum=WGS84 +no_defs"))
st_crs(shp)
parks_df <- data.frame(shp)
write.csv(parks_df, "./parks_alberta.csv")

#read data table
hot_spot = fread("ab-ebd-hotspot-locations.csv")
hot_spot = as.data.table(hot_spot)
Points_Shape = SpatialPoints(hot_spot[,c("longitude","latitude")],proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
Points_Shape = SpatialPointsDataFrame(Points_Shape,hot_spot)
plot(Points_Shape)
plot(shp)
points(Points_Shape, pch = 20, col = "orange")

pts.poly <- point.in.poly(Points_Shape, shp)
head(pts.poly@data)
write.csv(pts.poly, "./pts_poly.csv")
parks <- read.csv("./pts_poly.csv")


parks = parks%>%
  mutate(TYPE = case_when(TYPE == "NP"~1, #national parks
                           TYPE == "WA"~2, #wilderness area
                           TYPE == "ER"~3, #ecological reserve
                           TYPE == "WP"~4, #willmore Wilderness park
                           TYPE == "WPP"~5, #wildland park
                           TYPE == "PP"~6, #provincial park
                           TYPE == "HR"~7, #Heritage Rangeland 
                           TYPE == "NA"~8, #natural area
                           TYPE == "PRA"~9 )) #Provincial recreation area

parks[is.na(parks)] = 0

install.packages("dplyr")
library(dplyr)
parks <- parks%>%
  select(locality_id,TYPE)

write.csv(parks, "./type_of_hotspt.csv")
