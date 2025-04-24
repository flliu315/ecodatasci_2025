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
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

##################################################

# 01-access basic data of AOI (area of interest)

# A) river data and sampling point coordinates

# import the .csv file of coordinate x, y

doubs_xy <- read.csv("data/data_db/DoubsSpa.csv", 
                     row.names = 1)

# write.csv(doubs_xy, "data/geo_data/pointcoord_utm.csv")

# getting the geocoordinates of sample points using qgis

# // The 1st step  
# creating coordinates_utm.csv to an image with qgis
# Add Layer -> Add Delimited Text Layer -> 
# Project -> Export -> Export as image (sample_points.png)

# loading doubs_river.geojson created in 06_data_eda
# as reference for georeferencing the sample_points.png

# // The 2nd step 
# a) using freehand geoferencer to georeference png
# river map -> AD (adding png) -> geroreferencing
# https://www.youtube.com/watch?v=fzz8jw7Qp18 
# b) using the georeferencer  in layer
# Layer -> # Georeferencer.. -> raster
# https://www.youtube.com/watch?v=0CT3Un9v-6Q

# // The 3rd step
# installing and enabling coordinate capture plugin 

# Loading Georeferenced Image into QGIS
# Layer > Add Layer > Add Raster Layer
# click the gear ⚙️ to choose the CRS
# Layer > Create Layer > New Shapefile Layer
# Toggle -> Add Point Feature -> Save edits
# Export > Save Features As... -> AS_XY

library(tidyverse)
pointcoord_geo <- read_csv("data/geo_data/pointcoord_geo.csv")

# # Comparing the geo-referenced with the original
# load("data/geo_data/Doubs.RData",  Doubs <- new.env())
# ls.str(Doubs)
# latlong <- Doubs$latlong
# latlong

# Doubs <- load("data/geo_data/Doubs.RData")
# head(Doubs)

# create sf object
library(sf)
points_sf <- read.csv("data/geo_data/pointcoord_geo.csv", 
                      sep = ",") |> # set ";" if x.y is ";"
  st_as_sf(coords=c("X","Y"), crs=4326) 

# st_write(points_sf, "data/geo_data/sample_points.shp")

# B) get other public data
# get DEM data covered by Le Doubs river

## load the shapefile of Ld Doubs rive 

doubs_river <- st_read("data/geo_data/doubs_river.shp") # 06_eda
head(doubs_river)

# # install.packages("remotes")
# remotes::install_github("rspatial/geodata")

# library(elevatr)
# doubs_elev <- get_elev_raster(doubs_river, z = 10) # z resolution
# doubs_elev
# terra::writeRaster(doubs_elev, "data/geo_data/doubs_dem.tif",
#                    filetype = "GTiff", overwrite = TRUE)

# Visualizing river, locations and dem

library(terra)

doubs_dem <- terra::rast("data/geo_data/doubs_dem.tif")
terra::plot(doubs_dem, main="doubs river elevation")

doubs_river <- sf::st_read("data/geo_data/doubs_river.shp")
doubs_pts <- sf::st_read("data/geo_data/sample_points.shp")

plot(doubs_pts, add = TRUE, cex =1.8, col = "red")
plot(doubs_river, add = TRUE, col = "yellow")

########################################################
# 02-extracting spatial features to add as predictors
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
origin(topo_select) <- # where grid begins
  terra::origin(doubs_dem_crop) # the same origin

topo_char = c(doubs_dem_crop, topo_select) # add dem to SpatRaster

# reprojecting points to utm

doubs_pts_utm <- sf::st_transform(doubs_pts, 32631)
dim(doubs_pts_utm)
# st_write(doubs_pts_utm,"data/SPdata/doubs_pts_utm.shp")

# extracting raster values

topo_env <- terra::extract(topo_char, doubs_pts_utm, ID = FALSE)

# aggregating topo and water chemical env
env <- read.csv("data/data_db/DoubsEnv.csv", 
                     row.names = 1)

water_env <- env
doubs_env = cbind(doubs_pts_utm, topo_env, water_env) # convert dataframe to SpatRaster

# sf::st_write(doubs_env,  paste0("data/geo_data", "/", 
#                                 "doubs_env.shp"))

Doubs <- load("data/geo_data/Doubs.RData")
Doubs
spe 
spe_clean <- spe[!(rowSums(spe) == 0),]
dim(spe_clean)

# species diversity 
library(vegan)
# ?diversity
N0 <- rowSums(spe_clean > 0) # Species richness
H <- vegan::diversity(spe_clean) # Shannon entropy
N1 <- exp(H) # Shannon diversity (number of abundant species)
N2 <- diversity(spe_clean, "inv") # Simpson diversity (number of dominant species)
J <- H/log(N0) # Pielou evenness
E10 <- N1/N0 # Shannon evenness (Hill's ratio)
E20 <- N2/N0 # Simpson evenness (Hill's ratio)
(div <- data.frame(N0, H, N1, N2, E10, E20, J))

env_clean <- doubs_env[-8,]

env_diversity <- cbind(env_clean, N1) |>
  rename(shan_div = N1)
class(env_diversity)

# sf::st_write(env_diversity, paste0("data/geo_data", "/",
#                                "env_diversity.shp"),
#              append=FALSE)

# # ########################################################
# # ## Methods for ESDA and ML of spatial polygons
# # #######################################################
# # # https://bookdown.org/lexcomber/GEOG3195/spatial-models-spatial-autocorrelation-and-cluster-analysis.html
# # 
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
# # C) Incorporating spatial AC and heterogeneity into ML
# 
# # a) Simple Linear Regression
# 
# formula <- "vble ~ AREA + PERIMETER + HOVAL + INC + OPEN + X + Y"
# # compute model
# model1 <- lm(formula = formula, data = map)
# # view model statistics
# summary(model1)
# 
# # b) Spatial Regress Models accounting for spatial AC
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
# # c) Geographically Weighted Regress accounting for heterogeneity
# # load packages
# library(SpatialML)
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
# 03-ESDA of spatial dependence and heterogeneity for ML
#######################################################

# A) calculating the lagged mean and visualizing it
# https://spatialanalysis.github.io/handsonspatialdata/global-spatial-autocorrelation-1.html

library(tmap)
library(sf)
library(sp)
library(spdep)
library(ggplot2)
library(tidyverse)

env_div <- st_read("data/geo_data/env_diversity.shp")
plot(st_geometry(env_div))

env_div_xy <- env_div %>% # |> does not work
  mutate(
    x = sf::st_coordinates(.)[,1],
    y = sf::st_coordinates(.)[,2]) 

p_shandiv = # the focused variable
  ggplot(env_div_xy) +
  geom_sf(aes(fill = shan_div), size = 2) +
  scale_fill_viridis_c(option = "inferno", name = "original Value") +
  theme_minimal()
p_shandiv 

env_div_xy$rn = rownames(env_div_xy)
tmap_mode("view")
tm_shape(env_div_xy) +
  tm_text(text = "rn") +
  tm_basemap("OpenStreetMap")
tmap_mode("plot")

# creating voronoi polygons and calculating nb and w

library(deldir)
?deldir
vtess <- deldir(env_div_xy$x, 
                env_div_xy$y) # voronoi polygons
plot(vtess, wlines="tess", wpoints="none",
     lty=1)

voronoipolygons_sp = function(thiess) {# voronoi polygons to sp
  w = tile.list(thiess)
  polys = vector(mode='list', length=length(w))
  for (i in seq(along=polys)) {
    pcrds = cbind(w[[i]]$x, w[[i]]$y)
    pcrds = rbind(pcrds, pcrds[1,])
    polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP = SpatialPolygons(polys)
  voronoi = SpatialPolygonsDataFrame(
    SP, 
    data=data.frame(
      dummy = seq(length(SP)), 
      row.names=sapply(slot(SP, 'polygons'), 
                       function(x) slot(x, 'ID'))))
}

vtess_sp <- voronoipolygons_sp(vtess)
plot(vtess_sp)

vtess_sf <- st_as_sf(vtess_sp) # converting sp to sf 
plot(vtess_sf$geometry)

st_queen <- function(a, b = a) { # Queen Contiguity Function
  st_relate(a, b, pattern = "F***T****") # corner and boundary
}

queen_sgbp <- st_queen(vtess_sf) # Sparse Geometry Binary Predicate
as_nb_sgbp <- function(x, ...) {# converting sgbp to nb
  attrs <- attributes(x)
  x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )
  attributes(x) <- attrs
  class(x) <- "nb"
  x
}

queen_nb <- as_nb_sgbp(queen_sgbp) #  Convert sgbp to nb

queen_w <- spdep::nb2listw(queen_nb, style = "W") # from nb to weights
queen_w$weights[1:3]
summary(queen_w)

# computing the lagged means of shan_div 
# https://bookdown.org/lexcomber/GEOG3195/spatial-models-spatial-autocorrelation-and-cluster-analysis.html

env_div_xy$lagged_means_shandiv <- 
  lag.listw(queen_w, env_div_xy$shan_div)

p_lagged <- ggplot(env_div_xy) + 
  geom_sf(aes(fill = lagged_means_shandiv), size = 2) +
  scale_fill_viridis_c(option = "inferno", name = "Lagged Value") +
  theme_minimal()

cowplot::plot_grid(p_shandiv, p_lagged)

# B) Global Moran's I and test if statistically significant
# http://www.geo.hunter.cuny.edu/~ssun/R-Spatial/spregression.html
# https://rpubs.com/laubert/SACtutorial

moran.plot(x = env_div_xy$shan_div, listw = queen_w, 
           asp = 1)
title(main = "Global Moran's Scatter Plot")

gI <- moran.test(x = env_div_xy$shan_div, listw = queen_w) # for Moran’s I for statistic test

#Calculate Z-score
mI <- gI$estimate[[1]] # global MI
eI <- gI$estimate[[2]] # Expected MI
var <- gI$estimate[[3]] # Variance of values
zscore <- (mI-eI)/var**0.5 # -1.96 <zscore <1.96, no spatial correlation
zscore 

moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/ 2) $values))
}
moran.range(queen_w) # random if between, else cluster or dispersed

# C) Local Spatial Autocorrelation and test
# http://www.geo.hunter.cuny.edu/~ssun/R-Spatial/spregression.html#spatial-autocorrelation
# https://www.kaggle.com/code/jankuper192/spatial-regression

lI <- localmoran(env_div_xy$shan_div, 
                       queen_w,
                       zero.policy = TRUE, 
                       na.action = na.omit)

head(lI)

# Extracting Moran’s I and appending to sf 

env_div_xy$lI <- lI[,1]
env_div_xy$ElI <- lI[,2]
env_div_xy$VarlI <- lI[,3]
env_div_xy$ZlI <- lI[,4] # standard deviate of lI
env_div_xy$PlI <- lI[,5]

# derive the cluster/outlier types 
significanceLevel <- 0.05
meanVal <- mean(env_div_xy$shan_div)

library(magrittr)
lisaRslt <- lI |>  
  tibble::as_tibble() |>
  magrittr::set_colnames(c("Ii","E.Ii","Var.Ii","Z.Ii","Pr()")) |>
  dplyr::mutate(coType = dplyr::case_when(
    `Pr()` > 0.05 ~ "Insignificant",
    `Pr()` <= 0.05 & Ii >= 0 & env_div_xy$shan_div >= meanVal ~ "HH",
    `Pr()` <= 0.05 & Ii >= 0 & env_div_xy$shan_div < meanVal ~ "LL",
    `Pr()` <= 0.05 & Ii < 0 & env_div_xy$shan_div >= meanVal ~ "HL",
    `Pr()` <= 0.05 & Ii < 0 & env_div_xy$shan_div < meanVal ~ "LH"
  ))

print(lisaRslt, n =29)

# Now add this coType to the original sf
env_div_xy$coType <- lisaRslt$coType |> 
  tidyr::replace_na("Insignificant")

# Standardize the variable and its spatial lag
env_div_xy$z_var <- (env_div_xy$shan_div - mean(env_div_xy$shan_div)) / sd(env_div_xy$shan_div)
env_div_xy$z_lag <- (env_div_xy$lagged_means_shandiv - mean(env_div_xy$lagged_means_shandiv)) / sd(env_div_xy$lagged_means_shandiv)

# Create a 'quadrant' variable to classify points based on z_var and z_lag
env_div_xy$quadrant <- with(env_div_xy, 
                            case_when(
                              z_var >= 0 & z_lag >= 0 ~ "High-High (HH)",
                              z_var < 0 & z_lag >= 0 ~ "Low-High (LH)",
                              z_var >= 0 & z_lag < 0 ~ "High-Low (HL)",
                              z_var < 0 & z_lag < 0 ~ "Low-Low (LL)"
                            )
)

ggplot(env_div_xy, 
       aes(x = z_var, y = z_lag)) + # Create LISA plot
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_point(aes(color = quadrant), shape = 16, alpha = 0.7, size = 2.5) +
  scale_color_manual(values = c(
    "High-High (HH)" = "#E41A1C",
    "High-Low (HL)" = "#377EB8",
    "Low-High (LH)" = "#4DAF4A",
    "Low-Low (LL)" = "#984EA3"
  )) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
  coord_equal() +
  labs(
    title = "LISA Scatter Plot with Quadrants",
    x = "Standardized Value (z-score)",
    y = "Standardized Spatial Lag",
    color = "LISA Type"
  ) +
  theme_minimal()

ggplot(env_div_xy) +
  geom_sf(aes(fill=coType),color = 'black') +
  scale_fill_manual(values = c('red','brown','NA','blue','yellow'), 
                    name='Clusters & \nOutliers') +
  labs(title = "Shannon Diversity of Fishes")

########################################################
# 04-spatial autocorrelation and heterogeneity into ML
#######################################################
# A) Loading data and spatial EDA
# https://rpubs.com/zulfiqar_stat/1131164

library(sf)
library(tidyverse)

env_div <- st_read("data/geo_data/env_diversity.shp")
plot(st_geometry(env_div))

env_div_xy <- env_div %>%
  mutate(
    x = sf::st_coordinates(.)[,1],
    y = sf::st_coordinates(.)[,2]
    ) 

env_div_df <- env_div_xy |>
  st_drop_geometry() |> # remove geometry
  na.omit() |> # omit NA
  subset(select = -points)

env_div_sf <- st_as_sf(env_div_df, 
                       coords = c("x","y"), 
                       crs = "WGS 84 / UTM zone 31N")


res <- 120 # resolution
# round extremes to resolution
(x.min <- bbox(env_div_df)[1,1]%/%res*res)
(x.max <- (bbox(env_div_df)[1,2]+res)%/%res*res) 
(y.min <- bbox(meuse)[2,1]%/%res*res)
(y.max <- (bbox(meuse)[2,2]+res)%/%res*res)
# grid of coordinates
grid <- expand.grid(x = seq(x.min, x.max, by=res),
                    y = seq(y.min, y.max, by=res))
class(grid)
coordinates(grid) <- c("x", "y")
class(grid)

gridded(grid) <- T; fullgrid(grid) <- T
class(grid)

# removing correlated variables from df without shan_div
# PerformanceAnalytics::chart.Correlation(env_div_df[, -15], 
#                                         histogram = TRUE, 
#                                         pch = 19)
cor_matrix <- cor(env_div_df[, -15], 
                  use = "complete.obs", method = "pearson")
threshold <- 0.8
highly_correlated_vars <- caret::findCorrelation(cor_matrix, 
                                          cutoff = threshold, 
                                          verbose = TRUE)
df_cleaned <- env_div_df[, -highly_correlated_vars]
print(df_cleaned)

# B) incorporating Spatial AC into machine learning

# Simple Linear Regression including X and Y
library(spatialreg)
formula = shan_div ~.
model1 <- lm(formula = formula, data = df_cleaned)
summary(model1) # degree of freedom: m =independent, n-m-1

# C) Spatial Regression Model with buffer distances

# Create a prediction grid
bb <- st_bbox(env_div_sf) # get a sf boundary 
library(terra)
r <- rast(ext = ext(bb), resolution = 5,
          crs = crs(env_div_sf))

# Break into quantiles

(q.lzn <- quantile(env_div_sf$shan_div, seq(0,1,by=0.0625)))
env_div_sf$classes.lzn.q <- cut(env_div_sf$shan_div, 
                     breaks=q.lzn, 
                     ordered_result=TRUE, 
                     include.lowest=TRUE)
levels(env_div_sf$classes.lzn.q)

# Compute buffer distances via landmap
env_div_sp <- as(env_div_sf, "Spatial") # sf → SpatialPointsDataFrame
idx <- as.integer(env_div_sf$classes.lzn.q) # Create a factor index of bins
library(landmap)
library(sf)
library(terra)
library(raster)
dist_stack <- bufferDist(x = r, y = env_div_sp, z = idx) # Use bufferDist
# GSIF version
dist_stack <- GSIF::gBufferDist(x = r, xy = env_div_df[,c("x","y")], 
                           z = env_div_sf$classes.lzn.q, 
                           proj4string = proj4string(r))

names(dist_stack2) <- levels(pts_sf$bin)


coordinates(env_div_df) <- c("x", "y") # to a SpatialPointsDataFrame
env_div_df
bbox(env_div_df)
res <- 120 # resolution
# round extremes to resolution
(x.min <- bbox(env_div_df)[1,1]%/%res*res)
(x.max <- (bbox(env_div_df)[1,2]+res)%/%res*res) 
(y.min <- bbox(meuse)[2,1]%/%res*res)
(y.max <- (bbox(meuse)[2,2]+res)%/%res*res)
# grid of coordinates
grid <- expand.grid(x = seq(x.min, x.max, by=res),
                    y = seq(y.min, y.max, by=res))
class(grid)
coordinates(grid) <- c("x", "y")
class(grid)

gridded(grid) <- T; fullgrid(grid) <- T
class(grid)
# Set up the quantiles for the shan_div
library(ggplot2)
g <- ggplot(df_cleaned, mapping=aes(x=shan_div))
g + geom_histogram(bins=16, fill="lightgreen", color="blue") + geom_rug()

(q.lzn <- quantile(df_cleaned$shan_div, seq(0,1,by=0.0625)))
classes.lzn.q <- cut(df_cleaned$shan_div, breaks=q.lzn, 
                     ordered_result=TRUE, include.lowest=TRUE)
levels(classes.lzn.q)

# Compute distance buffers to points in each quantile

coordinates(df_cleaned) <- ~x + y
grid_dist <- landmap::buffer.dist(df_cleaned["shan_div"], 
                                  meuse.grid.sp, classes.lzn.q)
summary(grid.dist.lzn)
# compare simple linear model with SAG models
jtools::export_summs(model1, lag_model, error_model)

