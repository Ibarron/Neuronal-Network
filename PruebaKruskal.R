###############################
### INTRO ESTADISTICA EN R ###
###############################

### Hemiparkinson Juan Pablo

### PRUEBAS NO PARAMÉTRICAS

jp<- Schiffer_completos
jp2<- na.omit(jp)
#ordena y pone nombres a las filas
rn<-jp[,2]
grupos<-jp[,1]
park <- data.frame(jp[,-1:-2])
rownames(park) <- rn$'Etiquetas de fila'#cambia los nombres
library(dplyr)
library(tidyr)
library(ggplot2)
new<-gather(park, key= 'variables', value = 'medidas')
nombres <- rep(rn$`Etiquetas de fila`,116)
grupo <- rep(grupos$`Grupos`,116)
new<-cbind(new,nombres,grupo)
#nom<- c('Basal','Lesion','Post')
#grupob<- rep('Basal',1682)
#grupol<-rep('Lesion',986)
#grupop<-rep('Post',696)
#grupos<-c(grupob,grupol,grupop)
#new<-cbind(new,grupos)
kruskal.test(variables ~ medidas, data = new)
kruskal.test(grupo ~ medidas, data = new)


###################PRUEBA NO PARAMETRICOS ###################

new <- select(new, -nombres) #eliminar una columna
databa = new %>% filter(grupo == "Basal")
datale = new %>% filter(grupo == "Lesion")
datleiz = new %>% filter(grupo == "Les_izq")
datleder = new %>% filter(grupo == "Les_der")
datapos = new %>% filter(grupo == "Post")
datposiz = new %>% filter(grupo == "Post_izq")
datposder = new %>% filter(grupo == "Post_der")
datasha = new %>% filter(grupo == "Sham")
datashaiz = new %>% filter(grupo == "Sham_izq")
datashader = new %>% filter(grupo == "Sham_der")


###########NORMALIDAD, Tiene que ser mayor a 0.05 el p ###########

shapiro.test(databa$medidas) #a donde quieres aplicar la prueba
shapiro.test(datale$medidas)

shapiro.test(datleiz$medidas)
shapiro.test(datleder$medidas)
shapiro.test(datapos$medidas)
shapiro.test(datposiz$medidas)
shapiro.test(datposder$medidas)
shapiro.test(datasha$medidas)
shapiro.test(datashader$medidas)
shapiro.test(datashaiz$medidas)



######SIN DISTRIBUCION NORMAL ###

ggplot(new, aes(x=grupo, y= medidas, colour=grupo))+
  geom_boxplot()

ggplot(new, aes(x=medidas, colour=grupo)) + #crea la gráfica: ggplot(datos adjuntos, aes(x=datos apara eje x, color= nombre del grupo o columna))
  stat_ecdf()+
  scale_x_continuous( 
    breaks = c(1, 2,3, 4,5, 6,7,8,9,10,11))+ 
  coord_cartesian(xlim=c(1,11)) #parametros que uno escoge para hacerla  más ilustrativa

kruskal.test(medidas ~ grupo, data = new)
#########Wilcoxon, se recomienda usarla con minimo de n=8#######
#Compara con el otro grupo

datba = databa$medidas #vectores
datle = datale$medidas 
datpos = datapos$medidas 
datsha = datasha$medidas 


wt1 <- wilcox.test(datba,datle); wt1 #indicar que se enseñen en la consola
wt2 <- wilcox.test(datba,datpos); wt2
wt3 <- wilcox.test(datpos,datsha); wt3

# Kolmogorov-Smirnov Test
ks1 <- ks.test(datba,datle); ks1
ks2 <- ks.test(datba,datpos); ks2
ks3 <- ks.test(datba,datpos); ks3
