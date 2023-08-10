library(corrplot)
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyverse)  
library(corrr)
library(igraph)
library(lares)
library(qgraph)
library(sand)
library(network)
library(sna)
library(ape)

data <- TEA_Final


# Ordena y pone nombres a las filas
rn <- data[,1] # toma la primera columna con los nombres de las filas
dats <- data.frame(data[,-1]) # elimina la primera columna con los nombres de las filas
rownames(dats) <- rn$ASDs # cambia los nombres de las filas

# omite NAs (es necesario quitar variables con NAs)
dats2 <- na.omit(dats)

set.seed(365)
ruido <- runif(24*220, -0.00001, 0.00001)
#plot(ruido, c(1:5280))
ruido <- matrix(ruido, nrow = 24, ncol = 220)

dats2 <- dats2 + ruido

ndf <- dats2

mm <- cor(ndf, method = "spearman")
res1 <- cor.mtest(ndf, conf.level = .95, method="spearman")
signif <- as.matrix(res1[[1]])
sig_m <- mm
sig_m[signif >= 0.05] <- 0


mat <- sig_j

# Keep only high correlations
mat[mat<0.5 & mat > 0] <- 0
mat[mat < 0 & mat > (-0.5)] <- 0

# red con valores absolutos

net_abs <- graph.adjacency(abs(mat), mode = "undirected", weighted = TRUE, diag = FALSE)

E(net_abs)$cor <- E(net_abs)$weight

E(net_abs)$cor <- ifelse(E(net_abs)$weight==1, 
                         E(net_abs)$weight - runif(length(E(network)$weight), 0.00001, 0.0001),
                         E(net_abs)$weight + 0)
E(net_abs)$cor <- ifelse(E(net_abs)$cor==-1, 
                         E(net_abs)$cor + runif(length(E(network)$cor), 0.00001, 0.0001),
                         E(net_abs)$cor + 0)

E(net_abs)$color <- ifelse(E(net_abs)$weight < 0, "#27408B","#CD0000")
# E(net_abs)$width <- 1.5*atanh(abs(E(net_abs)$cor))
# V(net_abs)$size <- 1.2*abs(rowSums(mat))

e <- get.edgelist(net_abs,names=FALSE)
l <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(net_abs))


#### Modularidad

# Red con valores absolutos
# Calculo de estructura de comunidad con caminatas aleatorias segun:
# Pascal Pons, Matthieu Latapy: Computing communities in large 
# networks using random walks, https://arxiv.org/abs/physics/0512106

wc <- cluster_walktrap(net_abs, steps = 5)

# Modularidad de acuerdo con 
# Clauset, A.; Newman, M. E. J. & Moore, 
# C. Finding community structure in very 
# large networks, Physical Review E 2004, 70, 066111

modularity(wc)
membership(wc)

# Plot de Modularidad
png("ETM1.png", width = 5000, height = 4000)
plot(wc, net_abs, vertex.label.cex=8)
dev.off()


png("dendograma_wc.png", height = 3000, width = 2000)
dendPlot(wc, mode="phylo")
dev.off()

# Hierarchical Clustering

kc <- cluster_fast_greedy(net_abs)
png("modularidad.png", height = 3000, width = 2000)
plot(kc,net_abs)
dev.off()

png("dendograma.png", height = 3000, width = 2000)
dendPlot(kc, mode="phylo")
dev.off()

modularity(kc)
membership(kc)


### Red Simplificada

gs <- net_abs


set.seed(42)
# Compute communities (clusters)
cl <- walktrap.community(gs, steps = 5)
cl$degree <- (igraph::degree(gs)[cl$names])

# Assign node with highest degree as name for each cluster
cl$cluster <- unname(ave(cl$degree, cl$membership, 
                         FUN=function(x)names(x)[which.max(x)])
)
V(gs)$name <- cl$cluster

# Contract graph ----------------------------------------------------------

# Contract vertices
E(gs)$weight <- 1
V(gs)$weight <- 1
gcon <- contract.vertices(gs, cl$membership, 
                          vertex.attr.comb = list(weight = "sum", name = function(x)x[1], "ignore"))

# Simplify edges
gcon <- igraph::simplify(gcon, edge.attr.comb = list(weight = "sum", function(x) length(x)))

gcc <- induced.subgraph(gcon, V(gcon)$weight > 0.3)
gcc <- subgraph.edges(gcc, which(E(gcc)$weight > 0.5)) #revisar el peso mínimo que se desea
V(gcc)$degree <- unname(igraph::degree(gcc))
E(gcc)$width <- 1.5*asinh(E(gcc)$weight)

#  ------------------------------------------------------------------------

set.seed(42)
png("Red_Simplificada_TEAFINAL_nueva.png", width = 2000, height = 2000)
par(mar = rep(0.1, 4)) 
g.layout <- layout.kamada.kawai(gcc)
plot.igraph(gcc, 
            edge.arrow.size = 0.5, 
            layout = g.layout, 
            vertex.size = 0.5 * (V(gcc)$degree)
            )
dev.off()
