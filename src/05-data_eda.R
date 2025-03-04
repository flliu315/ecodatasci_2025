# --------------------------------------------
# Script Name: Exploratory data analysis
# Purpose: This section is to show how to do EDA for 
#          finding eco-patterns. The data analysis is 
#          to take the dataset of doubs as an example,
#          and do EDA on the data of a community.

# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2024-03-10
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

##################################################
# 01-Take a look at the study area -- doubs river
##################################################

# A) load and visualize river data
# using qgis to find and load river data

# Accessing OpenStreetMap data with R
# chatGPT prompt

"based on openstreetmap data, write R code to find Le Doubs
in France-Switzerland, and use mapview to visulize it on a map"

# final response
# Install and load necessary packages

library(osmdata)
library(mapview)

# Define the bounding box for France-Switzerland
bbox <- c(left = 5.5, bottom = 46.5, right = 7.5, top = 48)

# Query OpenStreetMap for waterways named "Le Doubs" within the bounding box
doubs_query <- opq(bbox = bbox) %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  add_osm_feature(key = "name", value = "Le Doubs") %>%
  osmdata_sf() 

# Visualize Le Doubs on a map
mapview(doubs_query$osm_lines)

# B) convert the osm to sf object and save it

## https://ourcodingclub.github.io/tutorials/spatial-vector-sf/
# Look at query output
library(dplyr)
doubs_query
glimpse(doubs_query$osm_lines)
glimpse(doubs_query$osm_multilines)

# bind into a sf object
library(sf)
river_sf <- bind_rows(st_cast(doubs_query$osm_lines, "MULTILINESTRING"),
                      doubs_query$osm_multilines) %>%
  select(name, osm_id, role)

head(river_sf)

# plot doubs_sf
plot(river_sf)

unique(river_sf$role)

# Filter out unneeded shapes to make sure shapes are valid
river_sf_clean <-
  river_sf %>%
  filter(is.na(role) == FALSE) %>%  # remove role NAs
  rename(doubs_type = role) %>% # rename role to doubs_type
  st_make_valid()

st_write(river_sf_clean, "data/geo_data/doubs_river.shp")


st_write(river_sf_clean,
         dsn = "data/geo_data/doubs_river.gpkg", # file path
         layer="doubsspaces", # layer name
         layer_options = c(paste0("DESCRIPTION=Contains spatial multilines for doubss, ",
                                  "Copyright OpenStreetMap constibutors. ODbL ",
                                  "https://www.openstreetmap.org/copyright")),
         # add layer description
         delete_dsn = TRUE
)

# doubs_river <- st_read(dsn = "data/SPdata/doubs_river.gpkg", 
#                       layer="doubsspaces")

doubs_river <- st_read("data/geo_data/doubs_river.shp")

library(ggplot2)
doubs_river <- ggplot(doubs_river) +
  geom_sf(color="blue")
doubs_river

ggsave("data/geo_data/doubs_river.png", doubs_river, 
       width = 8, height = 6.5)

##################################################
# 02-load the Doubs dataset for a glimpse
##################################################
# A) load Doubs data into R

# Because the dataset was uploaded into the databases
# of PostgreSQL and sqlite, so here load it from the 
# databases

# doubs <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
#                           dbname = 'ecodata',
#                           host = 'localhost',
#                           port = 5432,
#                           user = 'ecosci',
#                           password = 'ecosci')

library(DBI) # helps connecting R to database
library(RSQLite) # provides an interface with SQLite
library(dbplyr)
library(dplyr)

con <- DBI::dbConnect(RSQLite::SQLite(), 
                      "data/data_db/doubs.sqlite")
con1 <- DBI::dbConnect(RSQLite::SQLite(), 
                      "results/doubs.sqlite")
dbListTables(con1) # view the database
dbListFields(con1, "spe") # Querying table

dbListTables(con) # view the database
dbListFields(con, "DoubsSpe") # Querying table

species <- dplyr::tbl(con, "DoubsSpe") # a lazy object
species1 <- collect(species) # pull data into R
spe <- species1 |>
  select(-field1)

environment <- dplyr::tbl(con, "DoubsEnv")
environment1 <- collect(environment)
env <- environment1 |>
  select(-field1)
  
spacexy <- dplyr::tbl(con, "DoubsSpa")
spacexy1 <- collect(spacexy)
spa <- spacexy1 |>
  select(-field1)

library(tidyverse)
write_csv(spa, "results/spa.csv")
write_csv(spe, "results/spe.csv")
write_csv(env, "results/env.csv")
dbDisconnect(con) 

# B)-EDA on the data of fish community

library(vegan) 
source("src/panelutils.R") # https://figshare.com/s/65782d019a0e2af49cf6?file=4975841

dim(spe) # Dimensions of the data frame (rows, columns)
colnames(spe) # Column labels (descriptors = species)
rownames(spe) # Row labels (objects = sites)
summary(spe) # Descriptive statistics for columns

## check which sites have no data

# remove the row that the sum is zero 

spe_clean <- spe[rowSums(spe[])>0,]
dim(spe_clean)

## frequency distribution of fish abundance

range(spe_clean) 
(ab <- table(unlist(spe_clean))) # df→vector→table→output
barplot(ab,
        las = 1, # control target direction
        xlab = "Abundance class",
        ylab = "Frequency",
        col = gray(5 : 0 / 5)
)

sum(spe_clean == 0) / (nrow(spe_clean) * ncol(spe_clean))

## Visualizing fish distribution

head(spa)
plot(spa, # using plot(…) function with lines(…), 
     # points(…), text(…), polygon(…) etc. 
     # to build sophisticated graphs.
     asp = -1,
     type = "p", # a plot point data
     main = "Site Locations",
     xlab = "x coordinate (km)",
     ylab = "y coordinate (km)"
)

lines(spa, col = "light blue") # Add a line/labels/text 

text(spa, row.names(spa), cex = 0.8, col = "red")
text(40, 20, "Upstream", cex = 1.2, col = "red")
text(15, 120, "Downstream", cex = 1.2, col = "red")

par(mfrow=c(2,2))
# Plot four species
xl <- "x coordinate (km)"
yl <- "y coordinate (km)"
plot(spa, asp=1, col="brown", cex=spe$LOC, 
     main="Stone loach", xlab=xl, ylab=yl)
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, col="brown", cex=spe$CHA, 
     main="European bullhead", xlab=xl, ylab=yl)
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, col="brown", cex=spe$BAR, 
     main="Barbel", xlab=xl, ylab=yl)
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, col="brown", cex=spe$BCO, 
     main="Common bream", xlab=xl, ylab=yl)
lines(spa, col="light blue", lwd=2)
par(mfrow=c(1,1))

# Visualizing on a google map

# convert coordinates from no crs to epsg=4326 using qgis
# // converting xy.csv to xy.png with qgis
# // using geocoding and quickOSM to find doubs river
# // A) quickservice→metasearch→add default for basic map
# // B) quickOSM→waterway and river→doubs→runs
# // using freehand georeference Georeferencing an Image
# // A) digitizing river and smapling points (https://www.youtube.com/watch?v=ZgsXKw2ZjlA)
# // B) extracting long and lat of points (https://www.youtube.com/watch?v=aEnKHx14LnU)
# // extracting latlong with shp (https://www.youtube.com/watch?v=bVi7dCBu1hU)

library(tidyverse)
spa_geo <- read_csv("data/geo_data/spa_geo.csv")
spa_geo

# map with leaflet

library(leaflet)
doubs_map <- leaflet(data = spa_geo) %>% 
  addTiles() %>% # Add default OpenStreetMap map tiles
  addCircleMarkers(~X, ~Y, radius = ~spe$BCO)
doubs_map

category <- c(spe$BCO, spe$CHA, spe$BAR, spe$LOC)
factpal <- colorFactor(topo.colors(4), category)
doubs_map <- leaflet(data = spa_geo) %>% 
  addTiles() %>% # Add default OpenStreetMap map tiles
  addCircleMarkers(~X, ~Y, 
                   radius = ~c(spe$BCO, spe$CHA, spe$BAR, spe$LOC),
                   color = ~factpal(category))
doubs_map

# common species and hotspot
spe.pres <- apply(spe_clean > 0, 2, sum) # sum for column 
sort(spe.pres)
spe.relf <- 100*spe.pres/nrow(spe_clean)
round(sort(spe.relf), 1)
par(mfrow=c(1,2))
hist(spe.pres, main="Species Occurrences", right=FALSE, las=1,
     xlab="Number of occurrences", ylab="Number of species",
     breaks=seq(0,30,by=5), col="bisque")
hist(spe.relf, main="Species Relative Frequencies", right=FALSE,
     las=1, xlab="Frequency of occurrences (%)", ylab="Number of species",
     breaks=seq(0, 100, by=10), col="bisque")

site.pres <- apply(spe_clean > 0, 1, sum)
sort(site.pres)
par(mfrow=c(1,2))
plot(site.pres,type="s", las=1, col="gray",
     main="Species Richness vs. \n Upstream-Downstream Gradient",
     xlab="Positions of sites along the river", ylab="Species richness")
text(site.pres, row.names(spe_clean), cex=.8, col="red")
plot(spa, asp=1, main="Map of Species Richness", pch=21, col="white",
     bg="brown", cex=5*site.pres/max(site.pres), xlab="x coordinate (km)",
     ylab="y coordinate (km)")
lines(spa, col="light blue")

# transformation and standardization of fish data

?decostand
# Transform abundances to presence-absence
spe_clean[1:5, 2:4]
spe_pa <- decostand(spe_clean, method="pa")
spe_pa[1:5, 2:4]

# Scale abundances by dividing them by the maximum value for each species
spe_scale <- decostand(spe_clean, "max")
spe_scale[1:5,2:4]
apply(spe_scale, 2, max)
# Scale abundances by dividing them by the species totals
spe_relsp <- decostand(spe_clean, "total", MARGIN=2)
spe_relsp[1:5,2:4]
apply(spe_relsp, 2, sum)

# Scale abundances by dividing them by the site totals
spe_rel <- decostand(spe_clean, "total") # : MARGIN=1 (default value) 
spe_rel[1:5,2:4]
apply(spe_rel, 1, sum)

# chord transformation: the Euclidean distance
# Useful before PCA and K-means
spe_norm <- decostand(spe_clean, "normalize") #  (Euclidean norm)
spe_norm[1:5,2:4]

# Hellinger transformation. Apply Hellinger transformation 
# to correct for the double zero problem

spe_hel <- decostand(spe_clean, "hellinger")
spe_hel[1:5,2:4]

# Chi-square transformation
spe_chi <- decostand(spe_clean, "chi.square")
spe_chi[1:5,2:4]

# Wisconsin standardization
spe_wis <- wisconsin(spe_clean)
spe_wis[1:5,2:4]

# Boxplots of Transformed Abundances of a Common Species
par(mfrow=c(1,4))
boxplot(spe_clean$LOC, sqrt(spe_clean$LOC), log1p(spe_clean$LOC), las=1, main="Simple transformation",
        names=c("raw data", "sqrt", "log"), col="bisque")
boxplot(spe.scale$LOC, spe.relsp$LOC, las=1, main="Standardization by species",
        names=c("max", "total"), col="lightgreen")
boxplot(spe.hel$Neba, spe.rel$LOC, spe.norm$LOC, las=1, main="Standardization by sites",
        names=c("Hellinger", "total", "norm"), col="lightblue")
boxplot(spe.chi$LOC, spe.wis$LOC, las=1, main="Double standardization",
        names=c("Chi-square", "Wisconsin"), col="orange")

# C) EDA on the environment data
library(ade4)
doubs
# Bubble Maps of Some Environmental Variables
par(mfrow=c(1,4))
plot(spa, asp=1, main="Altitude", pch=21, col="white",
     bg="red", cex=5*env$alt/max(env$alt), xlab="x", ylab="y")
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, main="Discharge", pch=21, col="white",
     bg="blue", cex=5*env$deb/max(env$deb), xlab="x", ylab="y")
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, main="Oxygen", pch=21, col="white",
     bg="green3", cex=5*env$oxy/max(env$oxy), xlab="x", ylab="y")
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, main="Nitrate", pch=21, col="white",
     bg="brown", cex=5*env$nit/max(env$nit), xlab="x", ylab="y")
lines(spa, col="light blue", lwd=2)

# Examine the Variation of Some Descriptors Along the Stream
par(mfrow=c(1,4))
plot(env$das, env$alt, type="l", xlab="Distance from the source (km)",
     ylab="Altitude (m)", col="red", main="Altitude")
plot(env$das, env$deb, type="l", xlab="Distance from the source (km)",
     ylab="Discharge (m3/s)", col="blue", main="Discharge")
plot(env$das, env$oxy, type="l", xlab="Distance from the source (km)",
     ylab="Oxygen (mg/L)", col="green3", main="Oxygen")
plot(env$das, env$nit, type="l", xlab="Distance from the source (km)",
     ylab="Nitrate (mg/L)", col="brown", main="Nitrate")

# Scatter Plots for All Pairs of Environmental Variables
# to check collinearity, to which  constrained ordination 
# methods are highly sensitive

source("src/panelutils.R")
op <- par(mfrow=c(1,1), pty="s")
pairs(env, panel=panel.smooth,
      diag.panel=panel.hist,
      main="Biplots with histograms and smooth surves")
par(op)

# Simple Transformation of An Environmental Variable
# check distribution of slo
par(mfrow=c(1,4))
hist(env$pen, col="bisque", right=FALSE)
hist(log(env$pen), col="light green", right=F, main="Histogram of ln(env$pen)")
boxplot(env$pen, col="bisque", main="Boxplot of env$pen", ylab="env$pen")
boxplot(log(env$pen), col="light green", main="Boxplot of ln(env$pen)",
        ylab="log(env$pen)")

# Center and scale = standardize variables (z-scores) to reduce the
# effects of different units on analysis of any ordinations

env_z <- decostand(env, "standardize")
apply(env_z, 2, mean) # means = 0
apply(env_z, 2, sd) # standard deviations = 1

env_z1 <- as.data.frame(scale(env))# Same standardization with scale() 
env_z1

###################################################
# 03-Cluster and ordination
###################################################
# A) Q- and R-mode analysis

# Q-mode (analysis of distances/dissimilarities 
# among objects/sites or rows)

View(spe)
spe_db<-vegdist(spe_clean, method="bray") # Sorensen dissimilarity with 0-1 data
spe_dj<-vegdist(spe_clean, method="jac") # Jaccard dissimilarity
spe_dg<-vegdist(spe_clean, method="gower") # Gower dissimilarity 
spe_db_mat<-as.matrix(spe_db) #Put in matrix form (visualize or save)

View(env)
?dist # this function also compute dissimilarity matrix
env_z <- decostand(env[-8,], "standardize")
env_de<-dist(env_z, method = "euclidean") # euclidean distance matrix of the standardized environmental variables 
source("src/coldiss.R")
coldiss(env_de, diag=TRUE)

# R-mode (analysis of relationships among variables or columns)

(env_pearson<-cor(env)) # Pearson r linear correlation
round(env_pearson, 2) #Rounds the coefficients to 2 decimal points 
(env_ken<-cor(env, method="kendall")) # Kendall tau rank correlation
round(env_ken, 2)


# B) Clustering based on distance after transformed data

spe_dhel<-vegdist(spe_hel,method="euclidean") #generates the distance matrix from Hellinger transformed data

par(mfrow=c(1,1))
spe_dhel_single<-hclust(spe_dhel, method="single")
plot(spe_dhel_single, main="Single linkage clustering", hang =-1)

spe_dhel_complete <- hclust(spe_dhel, method = "complete")
plot(spe_dhel_complete, main="Complete linkage clustering", hang=-1)

# C) Unconstrained and constrained ordination

# model selection

vegan::decorana(spe[-8,])

# unconstrained ordination---PCA

# using the rda() function of vegan
spe_h_pca <- rda(spe_hel) # pca for the variable of species
summary(spe_h_pca) # overall results
# screeplot(spe_h_pca,type="lines") # Identify significant axis with Kaiser-Guttman criterion
ev <- spe_h_pca$CA$eig
ev[ev>mean(ev)]
n <- length(ev)
barplot(ev, main="Eigenvalues", col="grey", las=2)
abline(h=mean(ev), col="red") 
legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
# plot(spe_h_pca) 
# biplot(spe_h_pca, scaling =1, main="PCA scale=1") # angles between species not meaningful
# biplot(spe_h_pca, scaling =2, main="PCA scale=2") # angles between species reflect their correlations
library(ggplot2)
library(ggrepel)
str(spe_h_pca)
sitepca <- as.data.frame(spe_h_pca$CA$u[, 1:2]) # Site scores
speciespca <- as.data.frame(spe_h_pca$CA$v[, 1:2]) # Species scores

ggplot() +
  geom_point(data = sitepca,aes(x = PC1,y = PC2), size=4, shape = 21, fill = "blue", colour = "black") +
  geom_text_repel(data = sitepca, aes(PC1, PC2, label=row.names(sitepca)), size=4) +
  geom_segment(data = speciespca,aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                             type = "closed"),linetype=1, size=0.6,colour = "black")+
  geom_text_repel(data = speciespca, aes(PC1, PC2, label=row.names(speciespca)), colour = "red")+
  geom_hline(yintercept=0,linetype = 3,size = 1) +
  geom_vline(xintercept=0,linetype = 3,size = 1) +
  theme_bw() +
  theme(panel.grid=element_blank())

env_pca <- rda(env_z) # pca for the variable of environment
summary(env_pca)
ev <- env_pca$CA$eig
ev[ev>mean(ev)]
n <- length(ev)
barplot(ev, main="Eigenvalues", col="grey", las=2)
abline(h=mean(ev), col="red") 
legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")

# unconstrained ordination---NMDS
spe_nmds <- metaMDS(spe[-8,], distance = 'bray', k = 2)
spe_nmds$stress # goodness-of-fit


stressplot(spe_nmds, main = "Shepard plot")
plot(spe_nmds, type = "none",
     main = paste("NMDS/Bray - Stress =",
                  round(spe_nmds$stress, 3)),
     xlab = c("NMDS1"), ylab = "NMDS2")
points(scores(spe_nmds, display = "sites",
              choiches = c(1,2),
              pch = 21,
              col = "black",
              g = "steelblue",
              cex = 1.2))
text(scores(spe_nmds, display = "species", choices = c(1)),
     scores(spe_nmds, display = "species", choices = c(2)),
     labels = rownames(scores(spe_nmds, display = "species")),
     col = "red", cex = 0.8)

# constrained ordination---RDA
# hellinger transform species dataset: gives low weights to rare species 
# spe.hel <- decostand(species, "hellinger")
# Calculate distance matrix
spe_bchel<-vegdist(spe_hel, method="bray", binary=FALSE) 

# run RDA
env_z <- subset(env_z, select = -das)
spe_rda <- rda(spe_hel ~ ., data = env_z)
summary(spe_rda)
screeplot(spe_rda)
coef(spe_rda) # canonical coefficients
# R^2 retreived from the rda result
R2 <- RsquareAdj(spe_rda)$r.squared # unadjusted R^2 
R2 
R2adj <- RsquareAdj(spe_rda)$adj.r.squared # adjusted R^2
R2adj 

# plot RDA
# Triplot: sites, response variables and explanatory variables
# Scaling 1
plot(spe_rda, scaling=1, main="scaling 1 - wa scores")
spe_sc <- scores(spe_rda, choices=1:2, scaling=1, display="sp")
arrows(0,0,spe_sc[,1], spe_sc[,2], length=0, lty=1, col='red')

# Scaling 2
plot(spe_rda, main="scaling 2 - wa scores")
spe2_sc <- scores(spe_rda, choices=1:2, display="sp")  
arrows(0,0,spe2_sc[,1], spe2_sc[,2], length=0, lty=1, col='red')

# variance inflation factors in the RDA
vif_cca(spe_rda)
# RDA with all explanatory variables  
spe_rda_all <- rda(spe_hel ~., data=env[-8,])
# Forward selection using ordistep (accepts models with lower adjusted R2)
fwd_sel <- ordiR2step(rda(spe_hel ~ 1, data = env_z), # lower model limit (simple)
                      scope = formula(spe_rda), # upper model limit (the "full" model)
                      direction = "forward",
                      R2scope = TRUE, # not surpass the "full" model's R2
                      pstep = 1000,
                      trace = TRUE) # see the selection process
# Test of RDA result
anova.cca(spe_rda, step=1000)
# Test of all canonical axes
anova.cca(spe_rda, by='axis', step=1000)

