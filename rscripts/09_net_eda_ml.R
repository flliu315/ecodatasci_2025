# --------------------------------------------
# Script Name: Eco-network analysis
# Purpose: This script is shown how to construct a
#          eco-network and analyze the eco-network
#          properties, as well as predict possible
#          links of the econetwork.

# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2025-02-19
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

###############################################################
# 01- Econetwork graph representation and visualization
# # As the format of a spreadsheet or edgelist
# 
from <- c("1","1","2","2","3","4","4","5")
to <- c("2","4","3","4","4","5","6","6")
weight <- c(0.1,0.5,0.8,0.2,0.4,0.9,1.0,0.5)

# # Join the variables to create a data frame
links <- data.frame(from, to, weight)

# write.csv(links, "data/net_data/edgelist.csv", row.names = F)

# import a edgelist (regular spreedsheet)

edgelist <- readr::read_csv("data/net_data/edgelist.csv")
edgelist
library(dplyr) 
glimpse(edgelist)

# visualization with igraph package

library(igraph)
# Undirected Graph
edgelist_g <- graph_from_data_frame(d = edgelist, directed = F)
is_weighted(edgelist_g)

set.seed(3523) # Set random seed to ensure graph layout stays
# plot(edgelist_g, edge.width = E(edgelist_g)$weight)
tkplot(edgelist_g, edge.width = 1:10)


# B) As the format of an adjacency matrix
library(networkR)

from <- c("1","1","2","2","3","4","4","5")
to <- c("2","4","3","4","4","5","6","6")
weight <- c(0.1,0.5,0.8,0.2,0.4,0.9,1.0,0.5)

adj_mat <- adjacency(from, to, weight)
adj_mat[1:6, 1:6]

adj_mat_g <- graph_from_adjacency_matrix(adj_mat,
                                       weighted=T,
                                       mode="undirected", 
                                       diag=F)

plot(adj_mat_g) # plot without edge weights
plot(adj_mat_g, # plot with edge weights
     edge.width = E(adj_mat_g)$weight) 

# C) Convert to incidence matrix for bipartite matrices
# https://rpubs.com/lgadar/load-bipartite-graph

edgelist <- readr::read_csv("data/net_data/edgelist.csv")
class(edgelist)
edgelist_g <- graph_from_data_frame(edgelist)
is_bipartite(edgelist_g)

library(netdiffuseR)
inc_mat <- edgelist_to_adjmat(edgelist[,-3], undirected = TRUE)
inc_mat_g <- graph.incidence(inc_mat, weighted = TRUE)
is.bipartite(inc_mat_g)
plot(inc_mat_g, layout = layout_as_bipartite)

# 02-network property and exploratory analysis
# https://github.com/YongxinLiu/Note/blob/master/R/igraph/co-occurrence_network.R

# load 16s RNA sequencing data (otu)
otu_warming <- read.table("data/net_data/otu_warming.txt", 
                          head=T, row.names = 1, sep = "\t")
head(otu_warming)
dim(otu_warming) # check dataframe
sum(!is.na(otu_warming)) 

otu_control <- read.table("data/NETdata/Control.txt", head=T, 
                          row.names = 1, sep = "\t")
sum(!is.na(otu_control))

# remove the OUTs with less than 7 replicates in 14 plots
### for warming plots
otu_warming_NA <- apply(otu_warming, 
                        1, # the manipluation on rows
                        function(z) sum(is.na(z)))
# delete the OUTs 
otu_warming_clean <- otu_warming[otu_warming_NA < 7,]
head(otu_warming_clean)
dim(otu_warming_clean)

### for control plots

otu_control_NA <- apply(otu_control, 1, 
                        function(z) sum(is.na(z)))
otu_control_clean <- otu_control[otu_control_NA < 7,]
dim(otu_control_clean)

# relative abundance of each OTU in each plot
### for warming plots

otu_warming_clean_transform <- otu_warming_clean |>
  replace(is.na(otu_warming_clean), 0) |> # replace NA with 0's value
  t() # transfer to form samples x OUTS table

otu_warming_rel <- prop.table(as.matrix(otu_warming_clean_transform), 
                              margin = 1)*100 # each OTU relative abund
head(otu_warming_rel)

### for control plots

otu_control_transform <- otu_control_clean |>
  replace(is.na(otu_control_clean), 0) |>
  t()

otu_control_rel <- prop.table(as.matrix(otu_control_transform), 
                              margin = 1)*100
head(otu_control_rel)

# calculate correlation coefficient
####### for warming plots #####

library(psych) # for correlation coefficient
otu_warming_corr <- corr.test(otu_warming_rel, use = "pairwise",
                              method = "spearman", adjust = "fdr",
                              alpha = 0.05)
otu_warming_r <- otu_warming_corr$r # extracting r values
otu_warming_p <- otu_warming_corr$p # extracting p valutes
otu_warming_r[otu_warming_p >0.5 | abs(otu_warming_r)<0.70] <-0

otu_warming_r

####### for control plots #####

otu_control_corr <- corr.test(otu_control_rel, use = "pairwise",
                              method = "spearman", adjust = "fdr",
                              alpha = 0.05)
otu_control_r <- otu_control_corr$r # extracting r values
otu_control_p <- otu_control_corr$p # extracting p valutes
otu_control_r[otu_control_p >0.5 | abs(otu_control_r)<0.70] <-0

otu_control_r

# constructing molecular ecological network

###### for warming plots ##########
library(igraph)
otu_warming_g <- graph_from_adjacency_matrix(otu_warming_r,
                                             mode = "undirected",
                                             weighted = TRUE,
                                             diag = FALSE)
plot(otu_warming_g)

otu_warming_isol_vertex <- 
  V(otu_warming_g)[igraph::degree(otu_warming_g) == 0] # isolated vertices
otu_warming_g_optimal <- igraph::delete.vertices(otu_warming_g, 
                                                 otu_warming_isol_vertex)

set.seed(123)
plot(otu_warming_g_optimal, main ="co-occurrence network",
     vertex.frame.color = NA,  # Node border color
     vertex.label = NA,
     edge.width =1,
     vertex.size=5,  # Size of the node (default is 15)
     edge.lty =1,
     edge.curved =TRUE)

write_graph(otu_warming_g_optimal, "data/NETdata/otu_warming_net.txt", "edgelist")

####### for control plots ##############

otu_control_g <- graph_from_adjacency_matrix(otu_control_r,
                                             mode = "undirected",
                                             weighted = TRUE,
                                             diag = FALSE)
plot(otu_control_g)

otu_control_isol_vertex <- 
  V(otu_control_g)[igraph::degree(otu_control_g) == 0] # isolated vertices
otu_control_g_optimal <- igraph::delete.vertices(otu_control_g, 
                                                 otu_control_isol_vertex)

set.seed(123)
plot(otu_control_g_optimal, main ="co-occurrence network",
     vertex.frame.color = NA,  # Node border color
     vertex.label = NA,
     edge.width =1,
     vertex.size=5,  # Size of the node (default is 15)
     edge.lty =1,
     edge.curved =TRUE)

write_graph(otu_control_g_optimal, "data/NETdata/otu_control_net.txt", "edgelist")

################################################################

# 04-network properties and exploratory analysis

# 1) unipartite or one-mode network

# Node level properties

# A) degree and degree distribution

library(igraph)
g <-read_graph("data/NETdata/otu_warming_net.txt","edgelist") 
g <- as.undirected(g, mode = "collapse")
plot(g,vertex.frame.color=NA,vertex.label=NA,edge.width=1,
     vertex.size=5,edge.lty=1,edge.curved=F)
deg <- igraph::degree(g, mode="all") # calculate degree
deg

hist(deg, breaks=1:vcount(g)-1) # degree distribution

# B) closeness and betweenness centrality

deg=igraph::degree(g) 
lay <- layout.fruchterman.reingold(g) # fix layout
lay
fine = 500 # increase fine regulation
palette = colorRampPalette(c('blue','red')) # set color
degCol = palette(fine)[as.numeric(cut(deg,breaks = fine))]
plot(g, layout=lay, vertex.color=degCol, 
     vertex.size=deg*1.5, vertex.label=NA)

betw <- igraph::betweenness(g) # betweenness
plot(g,layout=lay, vertex.color=degCol,
     vertex.size=betw*0.8, vertex.label=NA)

clos <- igraph::closeness(g) # closeness
plot(g,layout=lay, vertex.color=degCol,
     vertex.size=clos*15000,vertex.label=NA)
ev <- igraph::evcent(g)
ev <- igraph::evcent(g)$vector
ev
plot(g,layout=lay, vertex.color=degCol,
     vertex.size=ev*10, vertex.label=NA)

# C) triads and clustering coefficients

triad <- igraph::triad_census(g)
triad
cc_global <- igraph::transitivity(g, type = "global")
cc_global
cc_local <- igraph::transitivity(g, type = "local")
cc_local

# Network level properties

# A) connectance

library(igraph)
g<-read_graph("data/NETdata/otu_warming_net.txt","edgelist")
g <- as.undirected(g, mode = "collapse")
plot(g,vertex.frame.color=NA,vertex.label=NA,edge.width=1,
     vertex.size=5,edge.lty=1,edge.curved=F)
connectance = edge_density(g,loops=FALSE)# connectance
connectance

# B) modularity

library(igraph)
g<-read_graph("data/NETdata/otu_warming_net.txt","edgelist")
g <- as.undirected(g, mode = "collapse")
ceb <- cluster_edge_betweenness(g)
modularity(ceb)
plot(ceb, g)

# C) nestedness


##----------------------------------------------------------
## 2) bipartite or two-mode network 

# https://rpubs.com/pjmurphy/317838
vv1_EL <- read.csv("data/NETdata/vv.txt", sep = " ")
vv1_EL
vv1_EL$V1 <- LETTERS[vv1_EL$V1] 

library(igraph)
# create a graph ( UNW, 10 = # of vertex, 8 = # of edges)
vv1_EL_g <- graph.data.frame(vv1_EL, directed=FALSE)

# identify two-mode network by $res=T and $type = F or T 
bipartite.mapping(vv1_EL_g)

## Add the "type" attribute to the network
V(vv1_EL_g)$type <- bipartite_mapping(vv1_EL_g)$type  

# modifying shape and color of the V and E 
V(vv1_EL_g)$color <- ifelse(V(vv1_EL_g)$type, "lightblue", "salmon")
V(vv1_EL_g)$shape <- ifelse(V(vv1_EL_g)$type, "circle", "square")
E(vv1_EL_g)$color <- "lightgray"

# adjusting sizes and colors of V labels
V(vv1_EL_g)$label.color <- "black" 
V(vv1_EL_g)$label.cex <- 1 
V(vv1_EL_g)$frame.color <-  "gray"
V(vv1_EL_g)$size <- 30

# use the bipartite-specific layout to minimize overlap
plot(vv1_EL_g, 
     edge.color="gray30",
     edge.width=E(vv1_EL_g)$weight,
     layout=layout.bipartite, 
     vertex.size=20, 
     vertex.label.cex=1.5)

# Calculating Centrality of two-mode network
vv_types <- V(vv1_EL_g)$type               
vv_deg <- igraph::degree(vv1_EL_g)
vv_bet <- igraph::betweenness(vv1_EL_g)
vv_clos <- igraph::closeness(vv1_EL_g)

vv_cent_df <- data.frame(vv_types, vv_deg, vv_bet, vv_clos)
vv_cent_df[order(vv_cent_df$vv_types, decreasing = TRUE),] 


########################################################
# 05-link predition with Keras in rstudio

library(keras)
library(igraph)
library(dplyr)

# load the MEN graph
g <- read_graph("data/NETdata/otu_warming_net.txt","edgelist")
g <- as.undirected(g, mode = "collapse")

# Get the node IDs
node_ids <- V(g)
print(node_ids)

plot(g,
     vertex.frame.color= "Forestgreen", 
     vertex.label= NA,
     edge.width=1,
     vertex.size=15,
     edge.lty=1,
     edge.curved=F)

# Get positive samples (existing edges)
positive_edges <- as_data_frame(g, what = "edges")
colnames(positive_edges) <- c("from", "to")
positive_edges$label <- 1

# Generate negative samples (non-existing edges)
possible_edges <- t(combn(V(g), 2))
existing_edges <- get.edgelist(g)
existing_edges <- apply(existing_edges, 1, 
                        function(x) paste(sort(x), collapse = "-"))
possible_edges <- apply(possible_edges, 1, 
                        function(x) paste(sort(x), collapse = "-"))
negative_edges <- setdiff(possible_edges, existing_edges)
negative_edges <- do.call(rbind, strsplit(negative_edges, "-"))
negative_edges <- as.data.frame(negative_edges, stringsAsFactors = FALSE)
colnames(negative_edges) <- c("from", "to")
negative_edges$label <- 0

# Combine and shuffle the data
edges <- rbind(positive_edges, negative_edges)
edges <- edges[sample(nrow(edges)), ]
dim(edges)

# Convert node IDs to numeric
edges$from <- as.numeric(edges$from)
edges$to <- as.numeric(edges$to)

# Create adjacency matrix
adj_matrix <- as.matrix(as_adjacency_matrix(g))

# Generate features for each edge
edge_features <- apply(edges[, 1:2], 1, function(edge) {
  adj_matrix[edge[1], ] + adj_matrix[edge[2], ]
})
edge_features <- t(edge_features)

# Define the model
model <- keras_model_sequential() |>
  layer_dense(units = 32, activation = "relu", 
              input_shape = ncol(edge_features)) |>
  layer_dense(units = 16, activation = "relu") |>
  layer_dropout(rate = 0.5) |>
  layer_dense(units = 1, activation = "sigmoid")

model |> compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_adam(),
  metrics = c("accuracy")
)

# Train the model
history <- model |> fit(
  x = edge_features,
  y = edges$label,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2
)

plot(history)

# Evaluate the model
model |> evaluate(edge_features, edges$label)

# Predict probabilities of links
predictions <- model |> predict(edge_features)
dim(predictions)

pred <- data.frame(ids = as.integer(node_ids),
                   links = as.integer(predictions))
head(pred)
sum(pred$links == 1)
write_csv(pred, "data/NETdata/pred.csv")
