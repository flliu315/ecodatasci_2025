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
# 03-ESDA focusing on SN, SWM and SA beyond EDA
#######################################################

# A) Neighbors based on contiguity (Queen vs Rook)

# a) the global spatial autocorrelation 

# https://bookdown.org/lexcomber/GEOG3195/spatial-models-spatial-autocorrelation-and-cluster-analysis.html
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

# examine zero links locations
map$rn = rownames(map) 
tmap_mode("view")
tm_shape(map) + 
  tm_borders() +
  tm_text(text = "rn") +
  tm_basemap("OpenStreetMap")
tmap_mode("plot")

# Create a line layer showing Queen's case contiguities
gg_net <- nb2lines(nb,coords=st_geometry(st_centroid(map)), 
                   as_sf = F) 
# Plot the contiguity and the map layer
p_adj = 
  ggplot(map) + geom_sf(fill = NA, lwd = 0.1) + 
  geom_sf(data = gg_net, col='red', alpha = 0.5, lwd = 0.2) +
  theme_minimal() + labs(subtitle =  "Adj")
p_adj

# spatial weights Matrix and the lagged means

nbw <- spdep::nb2listw(nb, style = "W") # compute the weighted list from nb object
nbw$weights[1:3]
map$lagged_means <- lag.listw(nbw, map$vble) # compute the lagged means
p_lagged = 
  ggplot(map) + geom_sf(aes(fill = lagged_means)) +
  scale_fill_gradient2(midpoint = 0.5, low = "red", high = "blue") +
  theme_minimal()

cowplot::plot_grid(p_vble, p_lagged)

p_lm = 
  ggplot(data = map, aes(x = vble, y = lagged_means)) +
  geom_point(shape = 1, alpha = 0.5) +
  geom_hline(yintercept = mean(map$lagged_means), lty = 2) +
  geom_vline(xintercept = mean(map$vble), lty = 2) +
  geom_abline() +
  coord_equal()
p_lm

# create Moran plot and statistic test using weighted list
moran.plot(x = map$vble, listw = nbw, asp = 1)

moran.test(x = map$vble, listw = nbw) # for Moran’s I for statistic test

moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/ 2) $values))
} 

moran.range(nbw) # strongly clustered

# b) local spatial autocorrelation and clusters

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

##----------------------------------------------
# C) Creating Thiessen polygons from a point layer

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

# Contiguity-based spatial weights for Thiessen polygons
st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")
queen.sgbp <- st_queen(vtess.sf) # convert to class nb
as.nb.sgbp <- function(x, ...) {
  attrs <- attributes(x)
  x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )
  attributes(x) <- attrs
  class(x) <- "nb"
  x
}

queen.nb <- as.nb.sgbp(queen.sgbp)

# the distribution of the number of neighbors
library(spdep)
queen.nb.card <- card(queen.nb)
library(ggplot2)
ggplot() +
  geom_histogram(aes(x=queen.nb.card)) +
  xlab("Number of Neighbors")
summary(queen.nb)

plot(queen.nb,coords, lwd=.2, col="blue", cex = .5)

library(sfdep)
doubs_lags <- doubs_sfxy |> 
  mutate(
    nb = st_contiguity(vtess.sf$geometry),
    wt = st_weights(queen.nb),
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

# B) Global and Local spatial autocorrelation, clusters

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

# construct voronoi polygons of sp and sf

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

queen_nb <- as.nb.sgbp(queen_sgbp)

queen_weights <- nb2listw(queen_nb) # get row standardized weights

# Creating a Moran scatter plot

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

moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
moran.range(queen_weights) # min-max=random, otherwise cluster or dispersed

#Calculate Z-score
mI <- spe_abund_gI$estimate[[1]] # global MI
eI <- spe_abund_gI$estimate[[2]] #Expected MI
var <- spe_abund_gI$estimate[[3]] #Variance of values
zscore <- (mI-eI)/var**0.5 # -1.96 <zscore <1.96, no spatial correlation

# # Assessing significance
# set.seed(2024)
# draw <- sample(doubs_sfxy$spe_abund, size = length(doubs_sfxy$spe_abund))
# draw

# library(robustHD)
# lag1 <- lag.listw(queen.weights,draw)
# lmfit <- lm(standardize(lag1) ~ standardize(draw))
# summary(lmfit)

# randomized_moran <- rep(NA, 999)
# for(i in 1:999){
#   draw <- sample(doubs_sfxy$spe_abund, size = length(doubs_sfxy$spe_abund))
#   lag <- lag.listw(queen.weights,draw)
#   lmfit <- lm(standardize(lag) ~ standardize(draw))
#   randomized_moran[i] <- lmfit$coefficients[2] 
# }

# summary(randomized_moran)
# sd(randomized_moran)

# # the number of samples had higher Moran’s I statistic 
# # than the observed value
# 
# length(which(randomized_moran > .133))

# only 1 value in all of the permutations that is 
# higher than the test statistic, so p=(1+R)/(1+M)
# = 0.002 (HERE R=1, M=999)

# # visualize the distribution of moran I
# 
# df <- data.frame(moran = randomized_moran)
# ggplot(data = df,aes(x=moran)) +
#   geom_density() +
#   geom_vline(xintercept = moran[[1]], col = "green") +
#   geom_vline(xintercept = mean(randomized_moran), col = "blue")
# 
# # Chow test Moran’s I scatterplot
# 
# mid_x <- mean(doubs_sfxy$x)
# mid_y <- mean(doubs_sfxy$y)
# doubs_sfxy<- doubs_sfxy |> 
#   mutate(bottom_left = if_else((x < mid_x & y < mid_y),"Select", "Rest"))
# 
# ggplot(doubs_sfxy, aes(x=standardized_spe_abund,y=standardized_lag_spe_abund)) +
#   geom_point(aes(color=bottom_left)) +
#   geom_smooth(aes(color=bottom_left), method = lm, se = FALSE) +
#   geom_smooth(method=lm,se = FALSE, color = "black") +
#   scale_color_manual(values=c("blue","red"))  +
#   labs(color="Selection") +
#   geom_hline(yintercept = 0, lty = 2) +
#   geom_vline(xintercept = 0, lty = 2) +
#   ggtitle("Chow test Moran Scatterplot")
# 
# doubs.select <- doubs_sfxy %>% filter(bottom_left == "Select")
# doubs.rest <- doubs_sfxy %>% filter(bottom_left == "Rest")
# 
# reg.select <- lm(standardized_lag_spe_abund~standardized_spe_abund, 
#                  data=doubs.select)
# reg.rest <- lm(standardized_lag_spe_abund~standardized_spe_abund, 
#                data=doubs.rest)
# summary(reg.select)
# summary(reg.rest)
# 
# chow <- chow.test(doubs.select$standardized_lag_spe_abund, 
#                   doubs.select$standardized_spe_abund, 
#                   doubs.rest$standardized_lag_spe_abund, 
#                   doubs.rest$standardized_spe_abund)
# chow

# Spatial Correlogram
# 
coords <- cbind(doubs_sfxy$x, doubs_sfxy$y)
dist_band_nb <- dnearneigh(coords,
                           0, 37576.59) # critical.threshold
sp <- sp.correlogram(dist.band.nb,
                     doubs_sfxy$spe_abund,
                     order = 10,
                     method = "I",
                     style = "W",
                     randomisation = TRUE,
                     spChk = NULL,
                     zero.policy = TRUE)
plot(sp)

morans <- sp$res[,1]
df <- data.frame(Morans_I = morans,lags = 1:10 )
ggplot(data = df, aes(x=lags,y=Morans_I)) +
  geom_point() +
  geom_smooth(col = "purple", se = FALSE) +
  geom_hline(yintercept = 0) +
  ylim(-.5,.5) 

df$euclidean_distance <- df$lags * 37576.59
ggplot(data = df, aes(x=euclidean_distance,y=Morans_I)) +
  geom_point() +
  geom_smooth(col = "purple", se = FALSE) +
  geom_hline(yintercept = 0) +
  ylim(-.5,.5) +
  scale_x_continuous(breaks = df$euclidean_distance)

pairs <- rep(NA, 10)

for (i in 1:10){
  nb <- dnearneigh(coords, (i - 1) * 37576.59, i * 37576.59)
  pairs[i] <- sum(card(nb)) / 2
}

df <- data.frame(lag_order = 1:10, auto_corr = morans, num_pairs = pairs)
df$euclidean_distance <- df$lag_order * 37576.59

p1 <- ggplot(data = df, aes(x = euclidean_distance,y = auto_corr)) +
  geom_point() +
  geom_smooth(col = "purple", se = FALSE) +
  geom_hline(yintercept = 0) +
  ylim(-1,1) +
  scale_x_continuous(breaks = df$euclidean_distance)
p2 <- ggplot(data = df, aes(x=euclidean_distance,y = num_pairs, fill = as.factor(euclidean_distance))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Paired") +
  theme(legend.position = "none") +
  geom_text(aes(label=num_pairs), position = position_dodge(width = .9), vjust=-.25) +
  ylim(0, 1.2 * max(pairs)) +
  scale_x_continuous(breaks = df$euclidean_distance)
p1
p2

grid.arrange(p1,p2,ncol = 1)

# The dot corresponds with distances between 0 and 4823 feet. 
# The dashed line indicates a spatial autocorrelation of 0. 
# The autocorrelation starts positive and trend the line.

#########################################################
# 03-Local Spatial Autocorrelation (hot-spots and outliers)

spe_abund_lI <- localmoran(doubs_sfxy$spe_abund, queen.weights)

#Extracting Local Moran’s I and appending it to the dataset

doubs_sfxy$lI <- spe_abund_lI[,1]
doubs_sfxy$ElI <- spe_abund_lI[,2]
doubs_sfxy$VarlI <- spe_abund_lI[,3]
doubs_sfxy$ZlI <- spe_abund_lI[,4]
doubs_sfxy$PlI <- spe_abund_lI[,5]

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

# A) Hot-spot Analysis 
# https://github.com/mpjashby/sfhotspot

doubs <- st_read('data/SPdata/doubs_env_spe.shp')
head(doubs)
sum(is.na(doubs))
doubs_clean <- na.omit(doubs)
names(doubs_clean)
doubs_relevant <- dplyr::select(doubs_clean, c("points", "spe_abund", "geometry"))
head(doubs_relevant)

library(sfhotspot)
library(tidyverse)

fishes_hotspots <- hotspot_gistar(doubs_relevant)
fishes_hotspots %>% 
  filter(gistar > 0, pvalue < 0.05) %>% 
  ggplot(aes(colour = kde, fill = kde)) +
  geom_sf() +
  scale_colour_distiller(aesthetics = c("colour", "fill"), direction = 1) +
  labs(title = "Density of fishes in Le Doubs") +
  theme_void()

#counts the number of points in each cell in a grid
point_counts <- hotspot_count(doubs_relevant)
point_counts

# plot that grid of cells
ggplot() +
  geom_sf(
    mapping = aes(fill = n),
    data = point_counts,
    alpha = 0.75,
    colour = NA
  ) +
  scale_fill_distiller(direction = 1)

# calculate kernel density estimates for each cell 
doubs_kde <- hotspot_kde(doubs_relevant)
doubs_kde
ggplot() +
  geom_sf(
    mapping = aes(fill = kde),
    data = doubs_kde,
    alpha = 0.75,
    colour = NA
  ) +
  scale_fill_distiller(direction = 1)

## B) Spatial outlines detection
## https://gis.stackexchange.com/questions/219255/spatial-outliers-detection-in-r
#  
library(sp)
library(spdep) # create spatial weights matrix objects
library(RANN) # nearest neighbors for Euclidean metric
library(spatialEco) # spatial data manipulation

# First, looking at the modified z-score on a-spatial

DES <- st_read("data/SPdata/doubs_env_spe.shp")
DES_sp <- st_as_sf(DES)
DES_df <- as(DES_sp, "Spatial")
names(DES_df)

# Second, calculating global outliers and then 
# calculating the local z-score and variance

(DES_df$Zscore <- # global outliers
    spatialEco::outliers(DES_df$spe_abund))  
spplot(DES_df, "Zscore", 
       col.regions=cm.colors(10))

dnn <- RANN::nn2(coordinates(DES_df), # local outliers
                 searchtype="radius", 
                 radius = 1000)$nn.idx
var.spe_abund <- rep(NA,nrow(DES_df))
z.spe_abund <- rep(NA,nrow(DES_df))  

for(i in 1:nrow(dnn)){
  dnn.idx <- dnn[i,] 
  var.spe_abund[i] <- var(DES_df[dnn.idx[dnn.idx != 0],]$spe_abund, na.rm=TRUE)
  z.spe_abund[i] <- outliers(DES_df[dnn.idx[dnn.idx != 0],]$spe_abund)[1]
}

var.spe_abund[!is.finite(var.spe_abund)] <- 0 
z.spe_abund[!is.finite(z.spe_abund)] <- 0 

DES_df$var.spe_abund <- var.spe_abund
spplot(DES_df, "var.spe_abund", col.regions=cm.colors(10))

DES_df$z.spe_abund <- z.spe_abund
spplot(DES_df, "z.spe_abund", col.regions=cm.colors(10))

# local autocorrelation (Local Moran's-I or LISA)

all.linked <- 
  max(unlist(nbdists(knn2nb(knearneigh(coordinates(DES_df))), 
                     coordinates(DES_df))))
nb <- dnearneigh(DES_df, 0, all.linked)

mI <- localmoran(DES_df@data[,"spe_abund"], nb2listw(nb, style="W"))
LocalI <- DES_df
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

