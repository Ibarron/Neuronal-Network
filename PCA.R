# -------------- INICIO -------------------------

# Llamar paqueterias
install.packages("FactoMineR")
install.packages("leaps")
install.packages("factoextra")
install.packages("corrplot") 
install.packages("ggplot2")

library(FactoMineR)
library(leaps)
library(factoextra)
library(corrplot)

# ////////////////////////////////////////////////////////////////////////////
# Importar los datos. Los nombres de las filas deben estar en la 1ra columna 
# ////////////////////////////////////////////////////////////////////////////

jp <- Schiffer_completos
grupos<-jp[,1]
grupo<-grupos$`Grupos`
data= jp %>% filter(grupo == "sham")
#ordena y pone nombres a las filas
rn<-data[,2]
dats <- data.frame(data[,-1:-2])
rownames(dats) <- rn$'Etiquetas de fila'#cambia los nombres
dats2 <- na.omit(dats)

# revisa si hay valores infinitos. si los hay, tendrán el valor 82382383892389328239
apply(dats2, 2, function(x) ifelse(all(is.infinite(x))==FALSE, 0, 82382383892389328239))

# revisa si hay variables con varianza = 0 y si las hay, las quita.
which(apply(dats2, 2, var)==0)
dats2 <- dats2[ , which(apply(dats2, 2, var) != 0)]


############################## PCA con prcomp() ##########################################

# PCA con la función prcomp. Se pone scale.=T para que haga el escalamiento.
pca1 <- prcomp(dats2, scale. = T)

# Explora el PCA
View(pca1)
print(pca1)
summary(pca1)
pca1$center

# Grafica el PCA
screeplot(pca1, type = "l", npcs = 11, main = "Screeplot 11 PCs")
abline(h = 1, col="red", lty=5)
fviz_eig(pca1)  

fviz_pca_var(pca1, col.var = "#00AFBB", repel = TRUE)

fviz_pca_biplot(pca1, repel = TRUE)

biplot(x = pca1, scale = 0, cex = 0.6, col = c("blue4", "brown3"))

# Rota los ejes en la visualización del PCA (opcional)
pca1$rotation <- -pca1$rotation
pca1$x        <- -pca1$x
biplot(x = pca1, scale = 0, cex = 0.6, col = c("blue4", "brown3"))

############################## PCA con FactoMineR() ##########################################

# Calcula el PCA (hace el escalado por default)
pca2 <- PCA(dats2)

# Explora el PCA
summary(pca2)

# Grafia el PCA por variables y por individuos
plot(pca2, cex=0.8, shadow=TRUE) #individuos

plot(pca2, choix = "var", shadow=TRUE) #variables todas

plot(pca2, choix = "var", shadow=TRUE, select = "contrib 5") # principales 5 variables

fviz_pca_var(pca2, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             title = "Variables Sham",
             repel = TRUE # Avoid text overlapping
)


# Extrae los resultados del PCA por individuos y por variables
var <- get_pca_var(pca2)

# Muestra la contribucion de las variables en una matriz tipo correlación 
png("lESIONPCA.png", width = 6000, height = 6000)
corrplot(var$cos2, is.corr=FALSE)
dev.off()

# Muestra la contribucion de las variables en una gráfica de barras 
# (((( la línea roja corresp. al valor esperado si la contribucion fuera uniforme ))))
png("barplot.png", width = 8000, height = 6000)
fviz_cos2(pca2, choice = "var", axes = 1:2)
dev.off()

# Muestra la contribucion de las top 10 variables en una gráfica de barras 
fviz_contrib(pca2, choice = "var", axes = 1, top = 10) # para la dim 1
fviz_contrib(pca2, choice = "var", axes = 2, top = 10) # para la dim 2
fviz_contrib(pca2, choice = "var", axes = 1:2, top = 10) # para las dim 1 y 2

# Grafica la contribucion de los individuos
fviz_pca_ind(pca2, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) # Avoid text overlapping (slow if many points)

# Grafica la contribucion de los individuos con barras
fviz_contrib(pca2, choice = "ind", axes = 1:2)


############################## kmeans VARIABLES ############################## 

# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)
set.seed(123)
res.v <- kmeans(var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.v$cluster) # aquí están por grupo en el cluster

# Color variables by groups
fviz_pca_var(pca2, col.var = grp, # con nombres de las variables
             repel = TRUE,
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             legend.title = "Cluster")

fviz_pca_var(pca2,
             geom.var = "point", # show points only (but not "text")
             col.var = grp, # color by groups
             title = "PCA PostEM",
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Cluster")

sort(grp) # ordena las variables for factor en orden ascendente

# Agrupa los datos por valor de cluster
cluster1_var <- grp[grp=="1"]
cluster2_var <- grp[grp=="2"]
cluster3_var <- grp[grp=="3"]

############################## kmeans INDIVIDUOS ############################## 

var2 <- get_pca_ind(pca2)

set.seed(456)
res.v2 <- kmeans(var2$coord, centers = 3, nstart = 25)
grp2 <- as.factor(res.v2$cluster) # aquí están por grupo en el cluster

fviz_pca_ind(pca2, col.ind = grp2, # con nombres de las variables
             repel = TRUE,
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             legend.title = "Cluster")

fviz_pca_ind(pca2,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = grp2, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "PCA Basal",
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups")

# Agrupa los datos por valor de cluster
cluster1_ind <- grp2[grp2=="1"]
cluster2_ind <- grp2[grp2=="2"]
cluster3_ind <- grp2[grp2=="3"]

# Muestra la contribucion de las top 10 de individuos en una gráfica de barras 
fviz_contrib(pca2, choice = "ind", axes = 1, top = 10) # para la dim 1
fviz_contrib(pca2, choice = "ind", axes = 2, top = 10) # para la dim 2
fviz_contrib(pca2, choice = "ind", axes = 1:2, top = 10) # para las dim 1 y 2


########################## ------ FIN ------- ############################

#-----------------------------------------------------------------------
#### UN EJEPLO DE JUGUETE
pca <- prcomp(USArrests, scale = TRUE)
biplot(x = PCA, scale = 0, cex = 0.6, col = c("blue4", "brown3"))
dev.new()
pca3 <- PCA(USArrests)
summary(pca3)
plot(pca3)
plot(pca3, cex=0.8, shadow=TRUE)
plot(pca3, choix = "var", shadow=TRUE)
plot(pca3, choix = "var", shadow=TRUE, select = "contrib 5")
#-----------------------------------------------------------------------