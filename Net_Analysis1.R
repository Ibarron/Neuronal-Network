library(sand)
library(qgraph)
library(igraph)
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyverse) 
library(network)
library(sna)

#### La variable "network" se crea con el script "Correlation_Network.R"

# Recall that the degree dv of a vertex v, in a network graph G = (V, E), counts the
# number of edges in E incident upon v. Given a network graph G, define fd to be the
# fraction of vertices v ∈ V with degree dv = d. The collection { fd }d≥0 is called the
# degree distribution of G, and is simply a rescaling of the set of degree frequencies,
# formed from the original degree sequence.

hist(degree(network), col="lightblue", xlim=c(0,12),
     xlab="Vertex Degree", ylab="Frequency", main="")

# For weighted networks, a useful generalization of degree is the notion of vertex
# strength, which is obtained simply by summing up the weights of edges incident to
# a given vertex. The distribution of strength—sometimes called the weighted degree
# distribution—is defined in analogy to the ordinary degree distribution.

hist(strength(network), col="pink",
       xlab="Vertex Strength", ylab="Frequency", main="")

d.ntw <- degree(network)

dd.ntw <- degree_distribution(network)

# Visualización de la distribucion de los grados log-log

d <- 1:max(d.ntw)-1
ind <- (dd.ntw != 0)
plot(d[ind], dd.ntw[ind], log="xy", col="blue",
         xlab=c("Log-Degree"), ylab=c("Log-Intensity"),
         main="Log-Log Degree Distribution")

# Beyond the degree distribution itself, it can be interesting to understand themanner
# in which vertices of different degrees are linked with each other. Useful in assessing
# this characteristic is the notion of the average degree of the neighbors of a given
# vertex.

# plot of average neighbor degree versus vertex degree

nb <- knn(network,V(network))$knn
  plot(d.ntw, nb, log="xy",
           col="goldenrod", xlab=c("Log Vertex Degree"),
           ylab=c("Log Average Neighbor Degree"))
  

   
A <- as_adjacency_matrix(network, sparse=FALSE)

g <- network::as.network.matrix(A)

# El orden de degree(g) es el mismo que el de la matriz de adyacencia A
# Hay que revisar quienes tienen el mayor grado de centralidad.

#degree
sna::degree(g)
sort(sna::degree(g))
which(sna::degree(g)==22 | sna::degree(g)==18)
colnames(A[,c(10,25)])
colores_centros <- c(rep("red", 9),"blue", rep("red", length(11:24)), "yellow")

#betweeness
bb <- round(sna::betweenness(g))
sort(bb)
which(bb==105 | bb==109)
colnames(A[,c(25,26)])
colores_centros <- c(rep("red", 24),"blue", "yellow", "red")

# Se pueden revisar los valores máximos de las otras medidas de centralidad
# para ver si coinciden o dependiendo de lo que se busca.

sna::gplot.target(g, degree(g,gmode="graph"), # degree(), closeness(), betweenness(), and evcent()
                  main="Degree", circ.lab = FALSE, 
                  circ.col="skyblue", usearrows = FALSE,
                  vertex.col= colores_centros,
                  edge.col="darkgray")


# HITS algorithm for directed graphs

# l <- layout_with_kk(aidsblog)
# plot(aidsblog, layout=l, main="Hubs", vertex.label="",
#      vertex.size=10 * sqrt(hub_score(aidsblog)$vector))
# plot(aidsblog, layout=l, main="Authorities", 
#      vertex.label="", vertex.size=10 * 
#        sqrt(authority_score(aidsblog)$vector))


# Characterizing Edges
# All of the summary measures discussed so far (i.e., degree and other, more general,
# notions of centrality) are for vertices, as it seems to be most common in practice
# that questions of importance are in regard to the vertices of a graph. But some
# questions are more naturally associated with edges. For example, we might ask
# which ties in a social network aremost important for the spread of, say, information or
# rumors. Edge betweenness centrality—which extends vertex betweenness centrality
# in a straightforwardmanner, by assigning to each edge a value that reflects the number
# of shortest paths traversing that edge—is a natural quantity to use here.


##### Si la red de correlacion tiene valores + y - hay que sacar valores
##### absolutos o este analisis no va a jalar.

net_abs <- graph.adjacency(abs(mat), mode = "undirected", weighted = TRUE, diag = FALSE)

E(net_abs)$cor <- E(network)$weight

E(net_abs)$cor <- ifelse(E(network)$weight==1, 
                         E(network)$weight - runif(length(E(network)$weight), 0.00001, 0.0001),
                         E(network)$weight + 0)
E(net_abs)$cor <- ifelse(E(network)$cor==-1, 
                         E(network)$cor + runif(length(E(network)$cor), 0.00001, 0.0001),
                         E(network)$cor + 0)

E(net_abs)$color <- ifelse(E(network)$weight < 0, "#27408B","#CD0000")
E(net_abs)$width <- 1.5*atanh(abs(E(network)$cor))
V(net_abs)$size <- 1.2*abs(rowSums(mat))

e <- get.edgelist(net_abs,names=FALSE)
l <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(net_abs))

eb <- edge_betweenness(net_abs)
E(net_abs)[order(eb, decreasing=T)[1:10]]


# Subgraphs and Censuses

# One approach to defining network cohesion is through specification of a certain
# subgraph(s) of interest. The canonical example of such a subgraph is that of a clique.
# Recall that cliques are complete subgraphs and hence are subsets of vertices that are
# fully cohesive, in the sense that all vertices within the subset are connected by edges.

data(karate)
?karate
plot(karate)
table(sapply(cliques(karate), length))
cliques(karate)[sapply(cliques(karate), length) == 5]

# A maximal clique is a clique that is not a subset of a larger clique. In the karate 
# network, the two largest cliques (formally called maximum cliques) are maximal,
# while, for example, the same can be said of only two of the 11 cliques of size 
# four.

table(sapply(max_cliques(karate), length))


# AHORA CON NUESTROS DATOS

clique_num(network) 
table(sapply(cliques(network), length))
cliques(network)[sapply(cliques(network), length) == 8]

table(sapply(max_cliques(network), length))
max_cliques(network)[sapply(max_cliques(network), length) == 5]




# # Smallworldness
# 
# set.seed(1)
# # a regular lattice. Even if the small-worldness is higher than three, the average path length is 
# # much higher than that of random networks.
# regnet<-igraph::watts.strogatz.game(dim=1, size=1000, nei=10, p=0, loops=FALSE, multiple=FALSE)
# smallworldness(regnet, B=10)
# 
# # a small-world network: the transitivity is much higher than random, the average path length is 
# # close to that of random networks
# swnet<-igraph::watts.strogatz.game(dim=1, size=1000, nei=10, p=.1, loops=FALSE, multiple=FALSE)
# smallworldness(swnet, B=10)
# 
# # a pseudorandom network: both the average path length and the transitivity are similar to random 
# # networks.
# rndnet<-igraph::watts.strogatz.game(dim=1, size=1000, nei=10, p=1, loops=FALSE, multiple=FALSE)
# smallworldness(rndnet, B=10)
#   