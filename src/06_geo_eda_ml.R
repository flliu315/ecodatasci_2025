# --------------------------------------------
# Script Name: geodata EDA and modeling
# Purpose: Here is the script about how to conduct EDA of geodata 
#          and model the spatial pattern, using doubs river fishes
#          as example to show how to extract env information along 
#          doubs river with the integration of R and QGIS. Also,
#          writing the code includes how to create spatial lag features 
#          using a spatial weight matrix and how to include these 
#          spatial features in a machine learning model.

# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2025-03-09
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

##################################################

# 01-access basic data of AOI (area of interest)

# A) river data and sampling coordinates

# import the .csv file of coordinate x, y

doubs <- read.csv("data/data_db/DoubsSpa.csv", row.names = 1)

# write.csv(doubs, "data/geo_data/coordinates_utm.csv")

# convert these coordinates to epsg=4326 using qgis
# // converting xy.csv to xy.png with qgis
# "Add Layer" > "Add Delimited Text Layer" >> shp
# "Project" > "Import/Export" > "Export map as Image" >> png
#
# // install geocoding and quickOSM plugins to get river map
#    a) quickservice > metasearch > add default 
#    b) quickOSM→waterway and river→doubs→runs

# // install freehand geoferencer to georeference png
# #  a) load rive map > AD (adding png) > geroreferencing
#    https://www.youtube.com/watch?v=fzz8jw7Qp18  
#    b) extracting long and lat of georefered points 
#    https://www.youtube.com/watch?v=aEnKHx14LnU

# // digitizing river and smapling points 
#    https://www.youtube.com/watch?v=ZgsXKw2ZjlA

library(tidyverse)
coordinates_geo <- read_csv("data/geo_data/coordinates_geo.csv")
coordinates_geo

# # # Comparing them with georeferring by ourselves
# load("data/geo_data/Doubs.RData",  Doubs <- new.env())
# ls.str(Doubs)
# latlong <- Doubs$latlong
# latlong

# Doubs <- load("data/geo_data/Doubs.RData")
# head(Doubs)

# create sf object
library(sf)
points_sf <- read.csv("data/geo_data/coordinates_geo.csv", 
                      sep = ",") |> # set ";" if x.y is ";"
  st_as_sf(coords=c("X","Y"), crs=4326) 

# st_write(points_sf, "data/geo_data/sample_points.shp")

# B) get other public data
# get DEM data covered by Le Doubs river

## load the shapefile of Ld Doubs rive 

doubs_river <- st_read("data/geo_data/doubs_river.shp") # 05_eda
head(doubs_river)

# # install.packages("remotes")
# remotes::install_github("rspatial/geodata")

# library(elevatr)
# elevation <- get_elev_raster(doubs_river, z = 10) # z resolution
# elevation
# terra::writeRaster(elevation, "data/geo_data/doubs_dem.tif",
#                    filetype = "GTiff", overwrite = TRUE)

# Visualizing river, locations and dem

library(terra)

doubs_dem <- terra::rast("data/geo_data/doubs_dem.tif")
doubs_river <- sf::st_read("data/geo_data/doubs_river.shp")
doubs_pts <- sf::st_read("data/geo_data/sample_points.shp")

plot(doubs_dem, main="")
plot(doubs_pts, add = TRUE, col = "red")
plot(doubs_river, add = TRUE, col = "blue")

########################################################
# 02-extracting spatial data to add originals as predictors
########################################################
## A) set a 5-km buffer along rive

library(terra)
library(sf)

doubs_dem <- terra::rast("data/geo_data/doubs_dem.tif")
doubs_river <- sf::st_read("data/geo_data/doubs_river.shp")
doubs_pts <- sf::st_read("data/geo_data/sample_points.shp")

# re-projecting the vector data of river
doubs_river_utm <- st_transform(doubs_river, 
                                32631) 

# creating and visualizing the buffer

doubs_river_buff <- st_buffer(doubs_river_utm, dis = 8000)
plot(st_geometry(doubs_river_buff), axes = TRUE)

library(ggplot2)
ggplot() + geom_sf(data = doubs_river_buff)

# B) Clip or intersect dem covered by river buffer
# reprojecting raster data
doubs_dem <- terra::rast("data/geo_data/doubs_dem.tif")
terra::crs(doubs_dem) # get CRS
utm_crs <- "EPSG:32631" # set CRS
doubs_dem_utm <- terra::project(doubs_dem,utm_crs)
terra::crs(doubs_dem_utm) # check crs

# Clip or intersect dem by doubs river

doubs_dem_utm_cropped = crop(doubs_dem_utm,
                             doubs_river_buff)
plot(doubs_dem_utm_cropped)
doubs_dem_utm_masked = mask(doubs_dem_utm_cropped,
                            doubs_river_buff)
# 
# writeRaster(doubs_dem_utm_masked, "data/geo_data/doubs_dem_crop.tif")

doubs_dem_crop <- terra::rast("data/geo_data/doubs_dem_crop.tif")
plot(doubs_dem_crop, axes = TRUE)

# C) extracting raster values of points as predictors
# https://r.geocompx.org/eco

library(qgisprocess)
qgis_configure()
qgis_search_algorithms("wetness") |>
  dplyr::select(provider_title, algorithm) |>
  head(2)

# catchment slope and catchment area
topo_total = qgisprocess::qgis_run_algorithm(
  alg = "sagang:sagawetnessindex",
  DEM = doubs_dem_utm_masked,
  SLOPE_TYPE = 1, 
  SLOPE = tempfile(fileext = ".sdat"),
  AREA = tempfile(fileext = ".sdat"),
  .quiet = TRUE)

topo_select <- topo_total[c("AREA", "SLOPE")] |>
  unlist() |>
  rast() #  catchment area and slope
names(topo_select) = c("carea", "cslope") # assign names
origin(topo_select) = origin(doubs_dem_crop) # the same origin

topo_char = c(doubs_dem_crop, topo_select) # add dem to SpatRaster

# reprojecting points to utm

doubs_pts_utm <- sf::st_transform(doubs_pts, 32631)
dim(doubs_pts_utm)
# st_write(doubs_pts_utm,"data/SPdata/doubs_pts_utm.shp")

# extracting raster values

topo_env <- terra::extract(topo_char, doubs_pts_utm, ID = FALSE)

# aggregating topo and water chemical env

water_env <- env
doubs_env = cbind(doubs_pts_utm, topo_env, water_env) # convert dataframe to SpatRaster

# sf::st_write(doubs_env,  paste0("data/geo_data", "/", 
#                                 "doubs_env.shp"))
Doubs <- load("data/geo_data/Doubs.RData", Doubs <- new.env())
Doubs
spe 
spe_clean <- spe[!(rowSums(spe) == 0),]
dim(spe_clean)

# species diversity 
library(vegan)
# ?diversity
N0 <- rowSums(spe_clean > 0) # Species richness
H <- diversity(spe_clean) # Shannon entropy
N1 <- exp(H) # Shannon diversity (number of abundant species)
N2 <- diversity(spe_clean, "inv") # Simpson diversity (number of dominant species)
J <- H/log(N0) # Pielou evenness
E10 <- N1/N0 # Shannon evenness (Hill's ratio)
E20 <- N2/N0 # Simpson evenness (Hill's ratio)
(div <- data.frame(N0, H, N1, N2, E10, E20, J))

spe_clean$N1 <- rowSums(spe_clean)
spe_clean

env
env_clean <- env[-8,]
latlong
latlong_clean <- latlong[-8,]

xy_env_spe <- cbind(latlong_clean, env_clean, N1)
dim(xy_env_spe)
xy_env_spe1 <- xy_env_spe |>
  dplyr::rename(lat = LatitudeN,
                lon = LongitudeE,
                shan_div = N1)

xy_env_spe_sf <- st_as_sf(xy_env_spe1,
                          coords = c("lon", "lat"),
                          crs = 4326) |>
  st_transform(crs = 32631)


sf::st_write(xy_env_spe_sf, paste0("data/geo_data", "/",
                                "xy_env_spe.shp"))

# ########################################################
# # 03-ESDA of spatial dependence of polygons for ML model
# #######################################################
# 
# # https://bookdown.org/lexcomber/GEOG3195/spatial-models-spatial-autocorrelation-and-cluster-analysis.html
# 
# # A) the global spatial autocorrelation 
# 
# library(spData)
# library(sf)
# library(spdep)
# library(ggplot2)
# 
# map <- st_read(system.file("shapes/columbus.shp",
#                            package = "spData"), quiet = TRUE)
# plot(st_geometry(map), border = "lightgray")
# 
# map$vble <- map$CRIME # the focusing variable
# # mapview(map, zcol = "vble")
# 
# p_vble = # create the map
#   ggplot(map) + 
#   geom_sf(aes(fill = map$vble)) +
#   scale_fill_gradient2(midpoint = 0.5, low = "red", high = "blue") +
#   theme_minimal()
# 
# p_vble 
# 
# 
# library(spdep)
# nb <- poly2nb(map, queen = TRUE) # determine adjacency 
# 
# library(tmap)
# # examine zero links locations
# map$rn = rownames(map) 
# tmap_mode("view")
# tm_shape(map) + 
#   tm_borders() +
#   tm_text(text = "rn") +
#   tm_basemap("OpenStreetMap")
# tmap_mode("plot")
# 
# # Create a line layer showing Queen's case contiguity
# gg_net <- nb2lines(nb,coords=st_geometry(st_centroid(map)), 
#                    as_sf = F) 
# # Plot the contiguity and the map layer
# p_adj = 
#   ggplot(map) + geom_sf(fill = NA, lwd = 0.1) + 
#   geom_sf(data = gg_net, col='red', alpha = 0.5, lwd = 0.2) +
#   theme_minimal() + labs(subtitle =  "Adj")
# p_adj
# 
# # spatial weights Matrix and the lagged means
# 
# nbw <- spdep::nb2listw(nb, style = "W") # compute weight matrix from nb
# nbw$weights[1:3]
# map$lagged_means <- lag.listw(nbw, map$vble) # compute lagged means
# p_lagged = 
#   ggplot(map) + geom_sf(aes(fill = lagged_means)) +
#   scale_fill_gradient2(midpoint = 0.5, low = "red", high = "blue") +
#   theme_minimal()
# 
# cowplot::plot_grid(p_vble, p_lagged)
# 
# p_lm = # create a lagged mean plot 
#   ggplot(data = map, aes(x = vble, y = lagged_means)) +
#   geom_point(shape = 1, alpha = 0.5) +
#   geom_hline(yintercept = mean(map$lagged_means), lty = 2) +
#   geom_vline(xintercept = mean(map$vble), lty = 2) +
#   geom_abline() +
#   coord_equal()
# p_lm
# 
# # create a Moran plot and statistic test using weighted list
# moran.plot(x = map$vble, listw = nbw, asp = 1)
# 
# moran.test(x = map$vble, listw = nbw) # for Moran’s I for statistic test
# 
# moran.range <- function(lw) {
#   wmat <- listw2mat(lw)
#   return(range(eigen((wmat + t(wmat))/ 2) $values))
# } 
# 
# moran.range(nbw) # strongly clustered
# 
# # B) local spatial autocorrelation and clusters
# 
# # Compute the local Moran’s I
# map$lI <- localmoran(x = map$vble, listw = nbw)[, 1] 
# 
# p_lisa = # create the map
#   ggplot(map) +
#   geom_sf(aes(fill= lI), lwd = 0.1) +
#   scale_fill_gradient2(midpoint = 0, name = "Local\nMoran's I",
#                        high = "darkgreen", low = "white") +
#   theme_minimal()
# p_lisa # print the map
# 
# # Create the local p values
# map$pval <- localmoran(map$vble,nbw)[, 5]
# map$pval
# 
# p_lisa_pval = 
#   ggplot(map) +
#   geom_sf(aes(fill= pval), lwd = 0.1) +
#   scale_fill_gradient2(midpoint = 0.05, 
#                        name = "p-values", 
#                        high = "red", low = "white") +
#   theme_minimal()
# 
# p_lisa_pval # print the map
# 
# cowplot::plot_grid(p_lisa + theme(legend.position = "bottom"), 
#           p_lisa_pval + theme(legend.position = "bottom"),
#           ncol = 2)
# 
# index  = map$pval <= 0.05
# p_vble + geom_sf(data = map[index,], fill = NA, 
#                  col = "black", lwd = 0.5)
# 
# # Getis-Ord G statistic
# 
# map$gstat <- as.numeric(localG(map$vble, nbw))
# 
# p_geto = 
#   ggplot(map) + 
#   geom_sf(aes(fill = gstat)) + 
#   scale_fill_gradient2(midpoint = 0.5, low = "red", high = "blue", 
#                        name = "G Statistic")+
#   theme_minimal()
# p_geto
# 
# # C) Incorporating spatial autocorrelation into ML
# 
# # a) Simple Linear Regression
# 
# formula <- "vble ~ AREA + PERIMETER + HOVAL + INC + OPEN + X + Y"
# # compute model
# model1 <- lm(formula = formula, data = map)
# # view model statistics
# summary(model1)
# 
# # b) Spatial Regression Models accounting for autocorrelation
# 
# model2 <- spatialreg::lagsarlm( # lag model
#   formula = formula, 
#   data = map, 
#   listw = nbw
# )
# 
# model3 <- spatialreg::errorsarlm( # error model
#   formula = formula, 
#   data = map, 
#   listw = nbw
# )
# 
# jtools::export_summs(model1, model2, model3) # compare to linear model
# 
# spdep::moran.test(model2$residuals, nbw) # model2 and models Moran's I test
# spdep::moran.test(model3$residuals, nbw)
# 
# # c) Geographically Weighted Regression accounting for heterogeneity
# # load packages
# library(SpatialML)
# library(sf)
# library(tidyverse)
# 
# library(GWmodel)
# map_sp <- map %>% # convert to sp object
#   as_Spatial()
# 
# library(tidyverse)
# library(sf)
# system.file("gpkg/nc.gpkg", package="sf") |>
#   read_sf() -> nc
# 
# nc1 <- nc |> mutate(SID = SID74/BIR74, NWB = NWBIR74/BIR74) 
# lm(SID ~ NWB, nc1) |>
#   predict(nc1, interval = "prediction") -> pr
# bind_cols(nc, pr) |> names()

########################################################
# 04-ESDA of spatial dependence of points for ML model
#######################################################

# A) creating Thiessen/voronoi polygons as an sf object
# https://spatialanalysis.github.io/handsonspatialdata/global-spatial-autocorrelation-1.html

library(tmap)
library(sf)
library(sp)
library(spdep)
library(ggplot2)
library(tidyverse)

doubs_sf <- st_read("data/geo_data/xy_env_spe.shp")

# Extract coordinates
coords <- st_coordinates(doubs_sf)
doubs_sfxy <- doubs_sf |>
  mutate(
    lon = coords[,1],  # Extract longitude
    lat = coords[,2]   # Extract latitude
  )

library(deldir)
vtess <- deldir(doubs_sfxy$lon, 
                doubs_sfxy$lat) # voronoi polygons
plot(vtess, wlines="tess", wpoints="none",
     lty=1)

voronoipolygons_sp = function(thiess) {# convert voronoi polygons to sp
  w = tile.list(thiess)
  polys = vector(mode='list', length=length(w))
  for (i in seq(along=polys)) {
    pcrds = cbind(w[[i]]$x, w[[i]]$y)
    pcrds = rbind(pcrds, pcrds[1,])
    polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP = SpatialPolygons(polys)
  voronoi = SpatialPolygonsDataFrame(SP, data=data.frame(dummy = seq(length(SP)), row.names=sapply(slot(SP, 'polygons'), 
                                                                                                   function(x) slot(x, 'ID'))))
}

vtess_sp <- voronoipolygons_sp(vtess)
plot(vtess_sp)

vtess_sf <- st_as_sf(vtess_sp) # converting sp to sf 
plot(vtess_sf$geometry)

# B) computing the lagged means for lagged mean plot
# creating the queen contiguity for nb and w

st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")
queen_sgbp <- st_queen(vtess_sf)
class(queen_sgbp)

as_nb_sgbp <- function(x, ...) {# converting sgbp to nb
  attrs <- attributes(x)
  x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )
  attributes(x) <- attrs
  class(x) <- "nb"
  x
}

queen_nb <- as_nb_sgbp(queen_sgbp) # convert neighbor types 

queen_weights <- nb2listw(queen_nb) # from neighbors to weights

# Check the spatial weights matrix
summary(queen_weights)

# computing the lagged means for plot 
# https://bookdown.org/lexcomber/GEOG3195/spatial-models-spatial-autocorrelation-and-cluster-analysis.html

doubs_sfxy$lagged.means <- lag.listw(queen_weights, 
                                     doubs_sfxy$shan_div)
plot_lm = 
  ggplot(data = doubs_sfxy, aes(x = shan_div, y = lagged.means)) +
  geom_point(shape = 1, alpha = 0.5) +
  geom_hline(yintercept = mean(doubs_sfxy$lagged.means), lty = 2) +
  geom_vline(xintercept = mean(doubs_sfxy$spe_abund), lty = 2) +
  geom_abline() +
  coord_equal()
plot_lm

# C) Moran plot and test if statistically significant
# http://www.geo.hunter.cuny.edu/~ssun/R-Spatial/spregression.html

spdep::moran.test(doubs_sfxy$shan_div, 
                  queen_weights, 
                  zero.policy = TRUE)

spdep::moran.plot(doubs_sfxy$shan_div, 
                  queen_weights, 
                  zero.policy = TRUE, 
                  xlab = 'sampling point',
                  ylab = 'Lagged spe-abund (of Neighbors)',
                  pch=20)

# # C) Global moran's I and spatial lag for Moran's plot
# 
# moran <- moran(doubs_sfxy$spe_abund, 
#                queen_weights, length(queen_nb), 
#                Szero(queen_weights))
# 
# moran$I # Moran’s I  
# 
# doubs_sfxy$lagged_spe_abund <- lag.listw(queen_weights,
#                                          doubs_sfxy$spe_abund)
# doubs_sfxy$lagged_spe_abund
# 
# doubs_sfxy$standardized_spe_abund <- # standardized lag and spe_abund
#   robustHD::standardize(doubs_sfxy$spe_abund)
# doubs_sfxy$standardized_lag_spe_abund <- 
#   robustHD::standardize(doubs_sfxy$lagged_spe_abund)
# 
# ggplot(data = doubs_sfxy, aes(x=standardized_spe_abund, 
#                               y = standardized_lag_spe_abund)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE) +
#   geom_hline(yintercept = 0, lty = 2) +
#   geom_vline(xintercept = 0, lty = 2) +
#   xlim(-10,10) +
#   ylim(-10,10) + 
#   ggtitle("global moran scatter plot")
# 
# # test if global Moran’s I statistic are significant
# # https://rpubs.com/laubert/SACtutorial
# 
# spe_abund_gI <- moran.test(doubs_sfxy$spe_abund, 
#                            queen_weights, zero.policy = TRUE)
# spe_abund_gI
# 
# moran.range <- function(lw) {
#   wmat <- listw2mat(lw)
#   return(range(eigen((wmat + t(wmat))/2)$values))
# }
# 
# moran.range(queen_weights) # random if between min-max, else cluster or dispersed
# 
# #Calculate Z-score
# mI <- spe_abund_gI$estimate[[1]] # global MI
# eI <- spe_abund_gI$estimate[[2]] #Expected MI
# var <- spe_abund_gI$estimate[[3]] #Variance of values
# zscore <- (mI-eI)/var**0.5 # -1.96 <zscore <1.96, no spatial correlation

# D) Local Spatial Autocorrelation and test
# http://www.geo.hunter.cuny.edu/~ssun/R-Spatial/spregression.html#spatial-autocorrelation

spe_div_lI <- localmoran(doubs_sfxy$shan_div, queen_weights)

# Extracting Moran’s I and appending to dataset

doubs_sfxy$lI <- spe_div_lI[,1]
doubs_sfxy$ElI <- spe_div_lI[,2]
doubs_sfxy$VarlI <- spe_div_lI[,3]
doubs_sfxy$ZlI <- spe_div_lI[,4] # standard deviate of lI
doubs_sfxy$PlI <- spe_div_lI[,5]

# derive the cluster/outlier types 
significanceLevel <- 0.05
meanVal <- mean(doubs_sfxy$spe_div)

Lisa_coType <- spe_abund_lI %>% tibble::as_tibble() %>%
  magrittr::set_colnames(c("Ii","E.Ii","Var.Ii","Z.Ii","Pr()")) %>%
  dplyr::mutate(coType = dplyr::case_when(
    `Pr()` > 0.05 ~ "Insignificant",
    `Pr()` <= 0.05 & Ii >= 0 & doubs_sfxy$shan_div >= meanVal ~ "HH",
    `Pr()` <= 0.05 & Ii >= 0 & doubs_sfxy$shan_div < meanVal ~ "LL",
    `Pr()` <= 0.05 & Ii < 0 & doubs_sfxy$shan_div >= meanVal ~ "HL",
    `Pr()` <= 0.05 & Ii < 0 & doubs_sfxy$shan_div < meanVal ~ "LH"
  ))

# Now add this coType to original sf
doubs_sfxy$coType <- Lisa_coType$coType %>% 
  tidyr::replace_na("Insignificant")

ggplot(doubs_sfxy) +
  geom_sf(aes(fill=coType),color = 'lightgrey') +
  scale_fill_manual(values = c('red','brown','NA','blue','cyan'), name='Clusters & \nOutliers') +
  labs(title = "Biodiversity level")

# E) Spatial model with machine learning

library(sf)
library(tidyverse)
doubs_sf <- st_read("data/geo_data/doubs_env_spe.shp")

data <- doubs_sf |>
  st_drop_geometry() |> # remove geometry
  na.omit() |>  # omit NA
  select(-1)

str(data)

# Simple Linear Regression
library(spatialreg)

formula = shan_div ~.
model1 <- lm(formula = formula, data = data)
summary(model1) # degree of freedom: m =independent, n-m-1

# Spatial Regression Models

queen_weights <- nb2listw(queen_nb) # from neighbors to weights

# lag model
lag_model <- spatialreg::lagsarlm(
  formula = formula, 
  data = data, 
  listw = queen_weights,
  zero.policy = TRUE
)

summary(lag_model)

# error model
error_model <- spatialreg::errorsarlm(
  formula = formula, 
  data = data, 
  listw = queen_weights
)

summary(error_model)

# compare simple linear model with SAG models
jtools::export_summs(model1, lag_model, error_model)

