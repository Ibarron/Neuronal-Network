
##Prueba PCA con datos hemiparkinson

library(FactoMineR)
library(leaps)
library(factoextra)
library(corrplot)

datos<-Schiffer_completos #importo mis datos con la variable que quiera

# Ordena y pone nombres a las filas
rn <- datos[,1] # toma la primera columna con los nombres de las filas
dat<- data.frame(rn)  # elimina la primera columna con los nombres de las filas
rownames(dat) <- rn $`Etiquetas de fila`# cambia los nombres de las filas

# omite NAs (es necesario quitar variables con NAs)
dats2 <- na.omit(dat)

