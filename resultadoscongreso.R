
library(corrplot)
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr)


data <- DatosSUVacomodadosGroningen_congreso
dats2 <- na.omit(data)
dats2<-dats2[!(dats2$BASELINE == 'R40'),]
rn<-dats2[,2]
dats <- data.frame(dats2[,-1:-2])
rownames(dats) <- rn$'BASELINE'#cambia los nombres

#ORDENA EN DOS COLUMNAS EN VARIABLES Y LAS MEDICIONES
new<-gather(dats, key= 'variables', value = 'medidas')
#AGREGA LOS NOMBRES NECESARIOS EN ESTE CASO GRUPO Y NOMBRES
grupos<-dats2[,1]
nombres<-dats2[,2]
nom<-rep(nombres$`BASELINE`,60)
grupo<-rep(grupos$`grupo`,60)
new<-cbind(new,nom,grupo)


d_b = new %>% filter(grupo == "basal")
d_1 = new %>% filter(grupo == "ETM1")
d_2 = new %>% filter(grupo == "ETM2")

#Revisamos la normalización
ggplot(new, aes(x=grupo, y=medidas, colour=grupo))+
  geom_boxplot()

ggplot(new, aes(x=medidas, colour=grupo)) + #crea la gráfica: ggplot(datos adjuntos, aes(x=datos apara eje x, color= nombre del grupo o columna))
  stat_ecdf()+
  scale_x_continuous( 
    breaks = c( 2,3, 4,5, 6,7,8,9,10))+ 
  coord_cartesian(xlim=c(2,10))

###Filtrar por grupo#######
jp <- DatosSUVacomodadosGroningen_congreso
grupos<-jp[,1]
grupo<-grupos$`grupo`
data= jp %>% filter(grupo == "basal")
#ordena y pone nombres a las filas
rn<-data[,2]
dats <- data.frame(data[,-1:-2])
rownames(dats) <- rn$'BASELINE'#cambia los nombres
dats2 <- na.omit(dats)
##################################
setwd("/Users/jp23m/Documents/PPS/Estadistica/Groninger/Congreso/analisisenR")


d_b = dats2 %>% filter(grupo == "basal")
dat_b <- data.frame(d_b[,-1:-2])
dat_b <- na.omit(dat_b)
# plot matrices de correlacion
mm <- cor(dat_b, method = "spearman")
write.csv(mm, file="Matriz_basal.csv",row.names = TRUE)

png("Matriz_basal.png", width = 1000, height = 1000, units = "px")
corrplot::corrplot(mm)
col<- colorRampPalette(c("darkred", "white", "darkblue"))(100)
LabelCol = c("black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black")
corrplot(mm, col=col, tl.col=LabelCol, tl.cex=1, tl.srt=90, diag = FALSE, mar=c(2,0,3,0))

corrplot(mm,add=TRUE, 
         type="lower", 
         method="circle", 
         shade.col=NA,
         col=col, 
         diag=FALSE, 
         title = "Matriz de Correlación Basal", 
         mar=c(2,0,2,0), 
         tl.pos="n", 
         cl.pos="n")

dev.off()

#teponeelnumerodelasignificanciaylas esttrellas

res1 <- cor.mtest(dat_b, conf.level = .95,method = "spearman")

png("matriz_Basal_significancias.png", width = 1000, height = 1000, units = "px")
corrplot::corrplot(mm)
col<- colorRampPalette(c("darkred", "white", "darkblue"))(100)
LabelCol = c("black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black")
corrplot(mm, col=col, tl.col=LabelCol, tl.cex=1, tl.srt=90, diag = FALSE, mar=c(2,0,3,0))

corrplot(mm,add=TRUE, 
         type="lower", 
         method="circle", 
         p.mat = res1$p,
         insig = "label_sig",
         sig.level = c(.001, .01, .05),
         pch.cex = 1.5, #tamaño de estrellas
         pch.col = "yellow",
         shade.col=NA,
         col=col, 
         diag=FALSE, 
         title = "Matriz Basal", 
         mar=c(2,0,2,0), 
         addCoef.col = "black",
         tl.pos="n", 
         cl.pos="n")
dev.off()

d_1 = dats2 %>% filter(grupo == "ETM1")
dat_1 <- data.frame(d_1[,-1:-2])
dat_1 <- na.omit(dat_1)

jj <- cor(dat_1, method = "spearman")
write.csv(jj, file="Matriz_ETM1.csv",row.names = TRUE)
# plot matrices de correlacion
png("Matriz_ETM1.png", width = 1000, height = 1000, units = "px")
corrplot::corrplot(jj)
col<- colorRampPalette(c("darkred", "white", "darkblue"))(100)
LabelCol = c("black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black")
corrplot(jj, col=col, tl.col=LabelCol, tl.cex=1, tl.srt=90, diag = FALSE, mar=c(2,0,3,0))

corrplot(jj,add=TRUE, 
         type="lower", 
         method="circle", 
         shade.col=NA,
         col=col, 
         diag=FALSE, 
         title = "Matriz de Correlación ETM1", 
         mar=c(2,0,2,0), 
         tl.pos="n", 
         cl.pos="n")

dev.off()

#teponeelnumerodelasignificanciaylas esttrellas

res1 <- cor.mtest(dat_1, conf.level = .95,method = "spearman")

png("matriz_ETM1_significancias.png", width = 1000, height = 1000, units = "px")
corrplot::corrplot(jj)
col<- colorRampPalette(c("darkred", "white", "darkblue"))(100)
LabelCol = c("black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black")
corrplot(jj, col=col, tl.col=LabelCol, tl.cex=1, tl.srt=90, diag = FALSE, mar=c(2,0,3,0))

corrplot(jj,add=TRUE, 
         type="lower", 
         method="circle", 
         p.mat = res1$p,
         insig = "label_sig",
         sig.level = c(.001, .01, .05),
         pch.cex = 1.5, #tamaño de estrellas
         pch.col = "yellow",
         shade.col=NA,
         col=col, 
         diag=FALSE, 
         title = "Matriz ETM1", 
         mar=c(2,0,2,0), 
         addCoef.col = "black",
         tl.pos="n", 
         cl.pos="n")
dev.off()



#################################################################

d_2 = dats2 %>% filter(grupo == "ETM2")
dat_2 <- data.frame(d_2[,-1:-2])
dat_2 <- na.omit(dat_2)


pp <- cor(dat_2, method = "spearman")
write.csv(pp, file="Matriz_ETM2.csv",row.names = TRUE)
# plot matrices de correlacion
png("Matriz_ETM2.png", width = 1000, height = 1000, units = "px")
corrplot::corrplot(pp)
col<- colorRampPalette(c("darkred", "white", "darkblue"))(100)
LabelCol = c("black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black")
corrplot(pp, col=col, tl.col=LabelCol, tl.cex=1, tl.srt=90, diag = FALSE, mar=c(2,0,3,0))

corrplot(pp,add=TRUE, 
         type="lower", 
         method="circle", 
         shade.col=NA,
         col=col, 
         diag=FALSE, 
         title = "Matriz de Correlación ETM2", 
         mar=c(2,0,2,0), 
         tl.pos="n", 
         cl.pos="n")

dev.off()

#teponeelnumerodelasignificanciaylas esttrellas

res2 <- cor.mtest(dat_2, conf.level = .95,method = "spearman")

png("matriz_ETM2_significancias.png", width = 1000, height = 1000, units = "px")
corrplot::corrplot(pp)
col<- colorRampPalette(c("darkred", "white", "darkblue"))(100)
LabelCol = c("black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black")
corrplot(pp, col=col, tl.col=LabelCol, tl.cex=1, tl.srt=90, diag = FALSE, mar=c(2,0,3,0))

corrplot(pp,add=TRUE, 
         type="lower", 
         method="circle", 
         p.mat = res2$p,
         insig = "label_sig",
         sig.level = c(.001, .01, .05),
         pch.cex = 1.5, #tamaño de estrellas
         pch.col = "yellow",
         shade.col=NA,
         col=col, 
         diag=FALSE, 
         title = "Matriz ETM2", 
         mar=c(2,0,2,0), 
         addCoef.col = "black",
         tl.pos="n", 
         cl.pos="n")
dev.off()




