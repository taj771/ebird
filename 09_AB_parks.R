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

#aggregate parks into unique parks types based on objective of park type

parks = parks%>%
  mutate(TYPE = case_when(TYPE == "NP"~1, #national parks
                          TYPE == "WA"~3, #wilderness area
                          TYPE == "ER"~3, #ecological reserve
                          TYPE == "WP"~3, #willmore Wilderness park
                          TYPE == "WPP"~3, #wildland provincial park park
                          TYPE == "PP"~2, #provincial park
                          TYPE == "HR"~3, #Heritage Rangeland 
                          TYPE == "NA"~3, #natural area
                          TYPE == "PRA"~2 )) #Provincial recreation area



parks[is.na(parks)] = 0

install.packages("dplyr")
library(dplyr)
parks <- parks%>%
  select(locality_id,TYPE)

write.csv(parks, "./type_of_hotspt.csv")

# create binary variables for each category
# National parks
df <- read.csv("./data/processed/type_of_hotspt.csv")
df = df%>%
  mutate(TYPE = case_when(TYPE == "1"~1, #national parks
                          TYPE == "3"~0, #wilderness area
                          TYPE == "3"~0, #ecological reserve
                          TYPE == "3"~0, #willmore Wilderness park
                          TYPE == "3"~0, #wildland provincial park park
                          TYPE == "2"~0, #provincial park
                          TYPE == "3"~0, #Heritage Rangeland 
                          TYPE == "3"~0, #natural area
                          TYPE == "2"~0, #Provincial recreation area
                          TYPE == "0"~0 )) # non-park sites
df=rename(df,"nat_bin" ="TYPE")
write_csv(df, "./data/processed/nat_parks.csv")

# provincial park
df <- read.csv("./data/processed/type_of_hotspt.csv")
df = df%>%
  mutate(TYPE = case_when(TYPE == "1"~0, #national parks
                          TYPE == "3"~0, #wilderness area
                          TYPE == "3"~0, #ecological reserve
                          TYPE == "3"~0, #willmore Wilderness park
                          TYPE == "3"~0, #wildland provincial park park
                          TYPE == "2"~1, #provincial park
                          TYPE == "3"~0, #Heritage Rangeland 
                          TYPE == "3"~0, #natural area
                          TYPE == "2"~1, #Provincial recreation area
                          TYPE == "0"~0 )) # non-park sites
df=rename(df,"prov_bin" ="TYPE")
write_csv(df, "./data/processed/prov_parks.csv")

# other parks
df <- read.csv("./data/processed/type_of_hotspt.csv")
df = df%>%
  mutate(TYPE = case_when(TYPE == "1"~0, #national parks
                          TYPE == "3"~1, #wilderness area
                          TYPE == "3"~1, #ecological reserve
                          TYPE == "3"~1, #willmore Wilderness park
                          TYPE == "3"~1, #wildland provincial park park
                          TYPE == "2"~0, #provincial park
                          TYPE == "3"~1, #Heritage Rangeland 
                          TYPE == "3"~1, #natural area
                          TYPE == "2"~0, #Provincial recreation area
                          TYPE == "0"~0 )) # non-park sites
df=rename(df,"otr_bin" ="TYPE")
write_csv(df, "./data/processed/other_parks.csv")

# bird siting out of the park boundary 

df <- read.csv("./data/processed/type_of_hotspt.csv")
df = df%>%
  mutate(TYPE = case_when(TYPE == "1"~0, #national parks
                          TYPE == "3"~0, #wilderness area
                          TYPE == "3"~0, #ecological reserve
                          TYPE == "3"~0, #willmore Wilderness park
                          TYPE == "3"~0, #wildland provincial park park
                          TYPE == "2"~0, #provincial park
                          TYPE == "3"~0, #Heritage Rangeland 
                          TYPE == "3"~0, #natural area
                          TYPE == "2"~0, #Provincial recreation area
                          TYPE == "0"~1 )) # non-park sites
df=rename(df,"out_bin" ="TYPE")
write_csv(df, "./data/processed/out_parks.csv")


