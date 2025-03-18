# --------------------------------------------
# Script Name: geodata EDA and modeling
# Purpose: Here is the script about how to conduct EDA of geodata 
#          and model the spatial pattern, using doubs river fishes
#          as example to show how to extract env information along 
#          doubs river with the integration of R and QGIS.

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

# # Comparing them with georeferring by ourselves
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

doubs_river <- st_read("data/geo_data/doubs_river.shp")
head(doubs_river)

# # install.packages("remotes")
# remotes::install_github("rspatial/geodata")

library(elevatr)
elevation <- get_elev_raster(doubs_river, z = 10)
elevation
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

writeRaster(doubs_dem_utm_masked, "data/geo_data/doubs_dem_crop.tif")
plot(doubs_dem_utm_masked, axes = TRUE)

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
origin(topo_select) = origin(doubs_dem_utm_masked) # the same origin

topo_char = c(doubs_dem_utm_masked, topo_select) # add dem to SpatRaster

# reprojecting points to utm

doubs_pts_utm <- sf::st_transform(doubs_pts, 32631)
dim(doubs_pts_utm)
# st_write(doubs_pts_utm,"data/SPdata/doubs_pts_utm.shp")

# extracting raster values

topo_env <- terra::extract(topo_char, doubs_pts_utm, ID = FALSE)

# aggregating topo and water chemical env
Doubs <- load("data/geo_data/Doubs.RData")
Doubs
water_env <- env
doubs_env = cbind(doubs_pts_utm, topo_env, water_env) # convert dataframe to SpatRaster

# sf::st_write(doubs_env,  paste0("data/geo_data", "/", 
#                                 "doubs_env.shp"))

Doubs <- load("data/geo_data/Doubs.RData")
head(Doubs)
spe$abund <- rowSums(spe)
doubs_env_spe <- cbind(doubs_env, spe$abund)
dplyr::rename(doubs_env_spe, abund=spe.abund)
# sf::st_write(doubs_env_spe,  paste0("data/geo_data", "/",
#                                 "doubs_env_spe.shp"))

########################################################
# 03-ESDA of spatial dependence for polygons
#######################################################

# https://bookdown.org/lexcomber/GEOG3195/spatial-models-spatial-autocorrelation-and-cluster-analysis.html

# A) the global spatial autocorrelation 

library(spData)
library(sf)
library(spdep)
library(ggplot2)

map <- st_read(system.file("shapes/columbus.shp",
                           package = "spData"), quiet = TRUE)
plot(st_geometry(map), border = "lightgray")

map$vble <- map$CRIME # the focusing variable
# mapview(map, zcol = "vble")

p_vble = # create the map
  ggplot(map) + 
  geom_sf(aes(fill = map$vble)) +
  scale_fill_gradient2(midpoint = 0.5, low = "red", high = "blue") +
  theme_minimal()

p_vble 


library(spdep)
nb <- poly2nb(map, queen = TRUE) # determine adjacency 

library(tmap)
# examine zero links locations
map$rn = rownames(map) 
tmap_mode("view")
tm_shape(map) + 
  tm_borders() +
  tm_text(text = "rn") +
  tm_basemap("OpenStreetMap")
tmap_mode("plot")

# Create a line layer showing Queen's case contiguity
gg_net <- nb2lines(nb,coords=st_geometry(st_centroid(map)), 
                   as_sf = F) 
# Plot the contiguity and the map layer
p_adj = 
  ggplot(map) + geom_sf(fill = NA, lwd = 0.1) + 
  geom_sf(data = gg_net, col='red', alpha = 0.5, lwd = 0.2) +
  theme_minimal() + labs(subtitle =  "Adj")
p_adj

# spatial weights Matrix and the lagged means

nbw <- spdep::nb2listw(nb, style = "W") # compute weight matrix from nb
nbw$weights[1:3]
map$lagged_means <- lag.listw(nbw, map$vble) # compute lagged means
p_lagged = 
  ggplot(map) + geom_sf(aes(fill = lagged_means)) +
  scale_fill_gradient2(midpoint = 0.5, low = "red", high = "blue") +
  theme_minimal()

cowplot::plot_grid(p_vble, p_lagged)

p_lm = # create a lagged mean plot 
  ggplot(data = map, aes(x = vble, y = lagged_means)) +
  geom_point(shape = 1, alpha = 0.5) +
  geom_hline(yintercept = mean(map$lagged_means), lty = 2) +
  geom_vline(xintercept = mean(map$vble), lty = 2) +
  geom_abline() +
  coord_equal()
p_lm

# create a Moran plot and statistic test using weighted list
moran.plot(x = map$vble, listw = nbw, asp = 1)

moran.test(x = map$vble, listw = nbw) # for Moran’s I for statistic test

moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/ 2) $values))
} 

moran.range(nbw) # strongly clustered

# B) local spatial autocorrelation and clusters

# Compute the local Moran’s I
map$lI <- localmoran(x = map$vble, listw = nbw)[, 1] 

p_lisa = # create the map
  ggplot(map) +
  geom_sf(aes(fill= lI), lwd = 0.1) +
  scale_fill_gradient2(midpoint = 0, name = "Local\nMoran's I",
                       high = "darkgreen", low = "white") +
  theme_minimal()
p_lisa # print the map

# Create the local p values
map$pval <- localmoran(map$vble,nbw)[, 5]
map$pval

p_lisa_pval = 
  ggplot(map) +
  geom_sf(aes(fill= pval), lwd = 0.1) +
  scale_fill_gradient2(midpoint = 0.05, 
                       name = "p-values", 
                       high = "red", low = "white") +
  theme_minimal()

p_lisa_pval # print the map

cowplot::plot_grid(p_lisa + theme(legend.position = "bottom"), 
          p_lisa_pval + theme(legend.position = "bottom"),
          ncol = 2)

index  = map$pval <= 0.05
p_vble + geom_sf(data = map[index,], fill = NA, 
                 col = "black", lwd = 0.5)

# Getis-Ord G statistic

map$gstat <- as.numeric(localG(map$vble, nbw))

p_geto = 
  ggplot(map) + 
  geom_sf(aes(fill = gstat)) + 
  scale_fill_gradient2(midpoint = 0.5, low = "red", high = "blue", 
                       name = "G Statistic")+
  theme_minimal()
p_geto

########################################################
# 04-Incorporating spatial autocorrelation into ML
#######################################################

# A) Simple Linear Regression

formula <- "vble ~ AREA + PERIMETER + HOVAL + INC + OPEN + X + Y"
# compute model
model1 <- lm(formula = formula, data = map)
# view model statistics
summary(model1)

# B) Spatial Regression Models accounting for autocorrelation

model2 <- spatialreg::lagsarlm( # lag model
  formula = formula, 
  data = map, 
  listw = nbw
)

model3 <- spatialreg::errorsarlm( # error model
  formula = formula, 
  data = map, 
  listw = nbw
)

jtools::export_summs(model1, model2, model3) # compare to linear model

spdep::moran.test(model2$residuals, nbw) # model2 and models Moran's I test
spdep::moran.test(model3$residuals, nbw)

# C) Geographically Weighted Regression accounting for heterogeneity
# load packages
library(SpatialML)
library(sf)
library(tidyverse)

library(GWmodel)
map_sp <- map %>% # convert to sp object
  as_Spatial()

library(tidyverse)
library(sf)
system.file("gpkg/nc.gpkg", package="sf") |>
  read_sf() -> nc

nc1 <- nc |> mutate(SID = SID74/BIR74, NWB = NWBIR74/BIR74) 
lm(SID ~ NWB, nc1) |>
  predict(nc1, interval = "prediction") -> pr
bind_cols(nc, pr) |> names()

########################################################
# 05-ESDA of spatial points and building a predict model
#######################################################

# A) Creating Thiessen polygons from a point layer

library(sf)
Doubs <- st_read("data/geo_data/doubs_env_spe.shp")
Doubs_nogeometry <- st_drop_geometry(Doubs)

coords <- read.csv("data/geo_data/coordinates_utm.csv")
coordsxy <- coords[, -1]
doubs_xy_env_spe <- cbind(Doubs_nogeometry, coordsxy)
doubs_sf <- st_as_sf(doubs_xy_env_spe, 
                     coords = c("x", "y"))|>
  st_set_crs(32631)
doubs_sf <- doubs_sf |>
  dplyr::mutate(lon = sf::st_coordinates(doubs_sf$geometry)[,1],
                lat = sf::st_coordinates(doubs_sf$geometry)[,2])

# st_write(doubs_sf, "data/geo_data/doubs_sf.shp", append=FALSE)

# construct voronoi or Thiessen polygons
library(dplyr)
doubs_sf <- st_read("data/geo_data/doubs_sf.shp")
doubs_sfxy <- doubs_sf |>
  rename(x=lon, y = lat)

library(deldir)
library(sp)
vtess <- deldir(doubs_sfxy$x, doubs_sfxy$y)
plot(vtess, wlines="tess", wpoints="none",
     lty=1)

voronoipolygons = function(thiess) {
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

v <- voronoipolygons(vtess)
plot(v)
class(v)
vtess_sf <- st_as_sf(v)
plot(vtess_sf$geometry)

# B) Contiguity-based spatial weights for Thiessen polygons
st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")
queen_sgbp <- st_queen(vtess_sf) # convert to class nb
as.nb.sgbp <- function(x, ...) {
  attrs <- attributes(x)
  x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )
  attributes(x) <- attrs
  class(x) <- "nb"
  x
}

queen_nb <- as.nb.sgbp(queen_sgbp)

# the distribution of the number of neighbors
library(spdep)
queen_nb_card <- card(queen_nb)
library(ggplot2)
ggplot() +
  geom_histogram(aes(x=queen_nb_card)) +
  xlab("Number of Neighbors")
summary(queen_nb)

plot(queen_nb,coords, lwd=.2, col="blue", cex = .5)

library(sfdep)
doubs_lags <- doubs_sfxy |> 
  mutate(
    nb = st_contiguity(vtess_sf$geometry),
    wt = st_weights(queen_nb),
    vtess_lag = st_lag(doubs_sfxy$spe_abund, nb, wt)
  ) 

library(ggplot2)
gg_doubs_lag <- ggplot(doubs_lags, aes(fill = doubs_sfxy$spe_abund)) +
  geom_sf(color = "black", lwd = 0.15) +
  scale_fill_viridis_c(limits = range(doubs_sfxy$spe_abund)) +
  theme_void()
gg_doubs_lag 

gg_doubs_obs <- ggplot(doubs_sfxy, aes(fill = doubs_sfxy$spe_abund)) +
  geom_sf(color = "black", lwd = 0.15) +
  scale_fill_viridis_c(limits = range(doubs_sfxy$spe_abund)) +
  theme_void()
gg_doubs_obs

patchwork::wrap_plots(gg_doubs_obs, gg_doubs_lag)

# C) Global and Local spatial autocorrelation, clusters

##-----------------------------------------------------
## As for the points
# https://spatialanalysis.github.io/handsonspatialdata/global-spatial-autocorrelation-1.html

library(sf)
library(spdep)
library(ggplot2)
library(deldir)
library(robustHD)
library(Hmisc)
library(tidyverse)
library(gap)
library(gridExtra)
library(geodaData)

doubs_sf <- st_read("data/geo_data/doubs_sf.shp")
doubs_sfxy <- doubs_sf |>
  rename(x=lon, y = lat)

# A) constructING Thiessen/voronoi polygons from point data

vtess <- deldir(doubs_sfxy$x, doubs_sfxy$y)
plot(vtess, wlines="tess", wpoints="none",
     lty=1)

voronoipolygons = function(thiess) {
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

v <- voronoipolygons(vtess)
plot(v)

# further convert sp to sf 
vtess_sf <- st_as_sf(v)
plot(vtess_sf$geometry)

# creating the queen contiguity

st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")
queen_sgbp <- st_queen(vtess_sf)
class(queen_sgbp)

# converts type sgbp to nb

as_nb_sgbp <- function(x, ...) {
  attrs <- attributes(x)
  x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )
  attributes(x) <- attrs
  class(x) <- "nb"
  x
}

queen_nb <- as_nb_sgbp(queen_sgbp)

queen_weights <- nb2listw(queen_nb) # get row standardized weights

# B) Calculating global moran I and testing significance

moran <- moran(doubs_sfxy$spe_abund, 
               queen_weights, length(queen_nb), 
               Szero(queen_weights))
moran$I 


doubs_sfxy$lagged_spe_abund <- lag.listw(queen_weights,
                                         doubs_sfxy$spe_abund)
doubs_sfxy$lagged_spe_abund

# standardized lag and spe_abund variables

doubs_sfxy$standardized_spe_abund <- 
  robustHD::standardize(doubs_sfxy$spe_abund)
doubs_sfxy$standardized_lag_spe_abund <- 
  robustHD::standardize(doubs_sfxy$lagged_spe_abund)

ggplot(data = doubs_sfxy, aes(x=standardized_spe_abund, 
                              y = standardized_lag_spe_abund)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  xlim(-10,10) +
  ylim(-10,10) + 
  ggtitle("global moran scatter plot")

# test if Moran’s I statistic are statistically significant
# https://rpubs.com/laubert/SACtutorial
spe_abund_gI <- moran.test(doubs_sfxy$spe_abund, 
                           queen_weights, zero.policy = TRUE)
spe_abund_gI

moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}

# I is between min-max, then random, otherwise cluster or dispersed

moran.range(queen_weights) 

#Calculate Z-score
mI <- spe_abund_gI$estimate[[1]] # global MI
eI <- spe_abund_gI$estimate[[2]] #Expected MI
var <- spe_abund_gI$estimate[[3]] #Variance of values
zscore <- (mI-eI)/var**0.5 # -1.96 <zscore <1.96, no spatial correlation

# C) Local Spatial Autocorrelation (hot-spots and outliers) and test

spe_abund_lI <- localmoran(doubs_sfxy$spe_abund, queen_weights)

# Extracting Local Moran’s I and appending it to the dataset

doubs_sfxy$lI <- spe_abund_lI[,1]
doubs_sfxy$ElI <- spe_abund_lI[,2]
doubs_sfxy$VarlI <- spe_abund_lI[,3]
doubs_sfxy$ZlI <- spe_abund_lI[,4] # standard deviate of lI
doubs_sfxy$PlI <- spe_abund_lI[,5]

# visualizing local moran's I

map_lI <- tm_shape(doubs_sfxy) + tm_lines()
tm_sf(col = "ZlI",
      title = "Local Moran's I - spe_abund",
      style = "fixed",
      breaks = c(-Inf, -1.96, 1.96, Inf),
      labels = c("Negative SAC", "Random SAC", "Positive SAC"),
      palette = "RdBu", n = 3,
      midpoint = NA,
      border.alpha = 0.3) +
  map_lI

# http://www.geo.hunter.cuny.edu/~ssun/R-Spatial/spregression.html#local-indicators-of-spatial-autocorrelation

# derive the cluster/outlier types 
significanceLevel <- 0.05
meanVal <- mean(doubs_sfxy$spe_abund)

spe_abund_lI %>% tibble::as_tibble() %>%
  magrittr::set_colnames(c("Ii","E.Ii","Var.Ii","Z.Ii","Pr()")) %>%
  dplyr::mutate(coType = dplyr::case_when(
    `Pr()` > 0.05 ~ "Insignificant",
    `Pr()` <= 0.05 & Ii >= 0 & doubs_sfxy$spe_abund >= meanVal ~ "HH",
    `Pr()` <= 0.05 & Ii >= 0 & doubs_sfxy$spe_abund < meanVal ~ "LL",
    `Pr()` <= 0.05 & Ii < 0 & doubs_sfxy$spe_abund >= meanVal ~ "HL",
    `Pr()` <= 0.05 & Ii < 0 & doubs_sfxy$spe_abund < meanVal ~ "LH"
  ))

## Spatial outliers detection
## https://gis.stackexchange.com/questions/219255/spatial-outliers-detection-in-r
#  
library(sp)
library(spdep) # create spatial weights matrix objects
library(RANN) # nearest neighbors for Euclidean metric
library(spatialEco) # spatial data manipulation

# looking at the modified z-score on a-spatial

DES_sf <- st_read("data/geo_data/doubs_env_spe.shp")
DES_sp <- as(DES_sf, "Spatial") # convert sf to sp
names(DES_sp)

# calculating global outliers and local z-score and variance

(DES_sp$Zscore <- # global outliers
    spatialEco::outliers(DES_sp$spe_abund))  
spplot(DES_sp, "Zscore", 
       col.regions=cm.colors(10))

dnn <- RANN::nn2(coordinates(DES_sp), # local outliers
                 searchtype="radius", 
                 radius = 1000)$nn.idx
var_spe_abund <- rep(NA,nrow(DES_sp))
z_spe_abund <- rep(NA,nrow(DES_sp))  

for(i in 1:nrow(dnn)){
  dnn.idx <- dnn[i,] 
  var.spe_abund[i] <- var(DES_sp[dnn.idx[dnn.idx != 0],]$spe_abund, na.rm=TRUE)
  z.spe_abund[i] <- outliers(DES_sp[dnn.idx[dnn.idx != 0],]$spe_abund)[1]
}

var.spe_abund[!is.finite(var.spe_abund)] <- 0 
z.spe_abund[!is.finite(z.spe_abund)] <- 0 

DES_sp$var.spe_abund <- var.spe_abund
spplot(DES_sp, "var.spe_abund", col.regions=cm.colors(10))

DES_sp$z.spe_abund <- z.spe_abund
spplot(DES_sp, "z.spe_abund", col.regions=cm.colors(10))

# local autocorrelation (Local Moran's-I or LISA)

all.linked <- 
  max(unlist(nbdists(knn2nb(knearneigh(coordinates(DES_sp))), 
                     coordinates(DES_sp))))
nb <- dnearneigh(DES_sp, 0, all.linked)

mI <- localmoran(DES_sp@data[,"spe_abund"], nb2listw(nb, style="W"))
LocalI <- DES_sp
LocalI@data <- data.frame(ID=rownames(LocalI@data), as.data.frame(mI))
names(LocalI@data)[6] <- "Pr"
spplot(LocalI, "Z.Ii", xlab="Local Morans-I", col.regions=topo.colors(30))   

cat(nrow( LocalI[LocalI@data[,"Pr"] < 0.05 ,]), "obs of", 
    nrow(LocalI), "are significant at p=0.05","\n")

plot(LocalI, pch=19)
points(LocalI[which(LocalI$Pr <= 0.05),], 
       pch=19,col="red") # Red=hot spots or spatial outliers

LocalI@data <- 
  data.frame(LocalI@data, 
             HotSpots=ifelse(mI[,5] <= 0.05 & mI[,4] >= mean(mI[,4]), 1, 0) )
LocalI@data$HotSpots <- as.factor(LocalI@data$HotSpots)

spplot(LocalI, "HotSpots", 
       xlab="Local Moran’s-I Hot Spots", 
       col.regions=c("blue","red"))
