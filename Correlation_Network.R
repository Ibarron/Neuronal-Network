library(corrplot)
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyverse)  
library(corrr)
library(igraph)
library(lares)
library(qgraph)

data <- Schiffer_completos


# Ordena y pone nombres a las filas
rn <- data[,1] # toma la primera columna con los nombres de las filas
dats <- data.frame(data[,-1]) # elimina la primera columna con los nombres de las filas
rownames(dats) <- rn$ASDs # cambia los nombres de las filas

# omite NAs (es necesario quitar variables con NAs)
dats2 <- na.omit(dats)
ruido <-runif(29*58,-0.0001,-0.0001)
plot(ruido,c(1:1682))
ruido <-matrix(ruido, nrow = 29, ncol = 58)
dats2 <- ruido

######### TOP CROSS CORRELATIONS VALUES ----------------------------------

Cross <- corr_cross(dats2, # name of dataset
                    max_pvalue = 0.05, # display only significant correlations (at 5% level)
                    top = 10, # display top 10 couples of variables (by correlation coefficient)
                    plot = FALSE,
                    method = "spearman"
)

Cross <- as.data.frame(Cross)



names_cross <- c(Cross$key, Cross$mix)
ndf <- dats2[,names_cross]

mm <- cor(ndf)
res1 <- cor.mtest(ndf, conf.level = .95)
signif <- as.matrix(res1[[1]])
sig_m <- mm
sig_m[signif >= 0.05] <- 0


mat <- sig_m

# Keep only high correlations
mat[mat<0.5 & mat > 0] <- 0
mat[mat < 0 & mat > (-0.5)] <- 0
#usarla o no


# Make an Igraph object from this matrix:
network <- graph.adjacency(mat, mode = "undirected", weighted = TRUE, diag = FALSE)
E(network)$cor <- E(network)$weight #Edges que son las lineas que conectan

E(network)$cor <- ifelse(E(network)$weight==1, 
                         E(network)$weight - runif(length(E(network)$weight), 0.00001, 0.0001),
                         E(network)$weight + 0)
E(network)$cor <- ifelse(E(network)$cor==-1, 
                         E(network)$cor + runif(length(E(network)$cor), 0.00001, 0.0001),
                         E(network)$cor + 0)

E(network)$color <- ifelse(E(network)$weight < 0, "#27408B","#CD0000") #https://r-charts.com/colors/

E(network)$width <- 1.5*atanh(abs(E(network)$cor)) #visualizaciones
V(network)$size <- 3*abs(rowSums(mat)) #vertices o nodos vertes modificarlos


library(qgraph)


e <- get.edgelist(network,names=FALSE)
l <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(network))

par(bg="#FFFFFF", mar=c(0,0,0,0))
set.seed(4)
plot(network,
     layout=l,
     vertex.size=12,
     vertex.color="#C1CDCD", #color de las bolitas
     vertex.label.cex=1.2, #nombre de las bolitas tamaño
     vertex.label.color="black",
     vertex.frame.color="transparent",
     edge.curved=.1,
     alpha=0.5
     )



################# --------------------------------------------------
### UNA VARIABLE VS LAS OTRAS

colnames(dats2)

variable <- "biochemistry_C_reactive_protein"

Cvar <- corr_var(dats2, # name of dataset
                 biochemistry_C_reactive_protein, # name of variable to focus on SIN COMILLAS
                 max_pvalue = 0.05,
                 top = 20, # display top 5 correlations
                 plot = FALSE
)


names_cross <- c(variable, Cvar$variables)
ndf <- dats2[,names_cross]

mm <- cor(ndf,  method = "spearman")
res1 <- cor.mtest(ndf, conf.level = .95, method = "spearman")
signif <- as.matrix(res1[[1]])
sig_m <- mm
sig_m[signif >= 0.05] <- 0


mat <- sig_m
mat[,2:nrow(mat)] <- 0


# Keep only high correlations
#mat[mat<0.5 & mat>-0.5] <- 0


# Make an Igraph object from this matrix:
network <- graph.adjacency(mat, mode = "undirected", weighted = TRUE, diag = FALSE)
E(network)$cor <- E(network)$weight

E(network)$cor <- ifelse(E(network)$weight==1, 
                         E(network)$weight - runif(length(E(network)$weight), 0.00001, 0.0001),
                         E(network)$weight + 0)
E(network)$cor <- ifelse(E(network)$cor==-1, 
                         E(network)$cor + runif(length(E(network)$cor), 0.00001, 0.0001),
                         E(network)$cor + 0)

E(network)$color <- ifelse(E(network)$weight < 0, "#27408B","#CD0000")
E(network)$width <- 1.5*atanh(abs(E(network)$cor))
V(network)$size <- (3*abs(rowSums(mat)))+1


library(qgraph)


e <- get.edgelist(network,names=FALSE)
l <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(network))

par(bg="#FFFFFF", mar=c(0,0,0,0))
set.seed(4)
plot(network,
     layout=l,
     vertex.size=12,
     vertex.color="#C1CDCD", 
     vertex.label.cex=1.2,
     vertex.label.color="black",
     vertex.frame.color="transparent",
     edge.curved=.1
)
