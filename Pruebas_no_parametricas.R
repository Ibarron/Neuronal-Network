###############################
### INTRO ESTADISTICA EN R ###
###############################

### POR: ANTONIETA MARTÍNEZ-GUERRERO (2022)

### PRUEBAS NO PARAMÉTRICAS

library(dplyr)
library(tidyr)
library(ggplot2)

x <- Men  # Men or Men_Neutral
d100<-x[1:418,]

data100 = x %>% filter(Tempo == "100 bpm")
data140 = x %>% filter(Tempo == "140 bpm")
data180 = x %>% filter(Tempo == "180 bpm")

# Shapiro Test para normalidad
# si el p-value es mayor a 0.05, la distribucion es normal 

shapiro.test(data100$Reaction.Time) #a donde quieres aplicar la prueba
shapiro.test(data140$Reaction.Time)
shapiro.test(data180$Reaction.Time)

# Boxplot
# ¿Hay diferencias evidentes entre las distribuciones?

ggplot(x, aes(x=Tempo, y=Reaction.Time, colour=Tempo))+
  geom_boxplot()

# Probabilidad Acumulada
# ¿Hay diferencias evidentes entre las distribuciones?

ggplot(x, aes(x=Reaction.Time, colour=Tempo)) + #crea la gráfica: ggplot(datos adjuntos, aes(x=datos apara eje x, color= nombre del grupo o columna))
  stat_ecdf() + 
  scale_x_continuous( 
    breaks = c(0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4))+ 
  coord_cartesian(xlim=c(0.8,2.4)) #parametros que uno escoge para hacerla  más ilustrativa


# Kruskal-Wallis (al menos una es diferente, se basa en las medianas)
kruskal.test(Reaction.Time ~ Tempo, data = x)

# Wilcoxon Test
# wilcox.test(x, y, mu=0, paired=FALSE) más de 8 datos

data100 = data100$Reaction.Time #vectores
data140 = data140$Reaction.Time
data180 = data180$Reaction.Time

wt1 <- wilcox.test(data100,data140); wt1 #indicar que se enseñen en la consola
wt2 <- wilcox.test(data100,data180); wt2
wt3 <- wilcox.test(data140,data180); wt3

# Ajuste / Corrección de p-value
p.adjust(c(0.007068, 0.4971, 0.00103), method = "bonferroni")
p.adjust(c(0.007068, 0.4971, 0.00103), method = "holm")

# p.adjust.methods
# c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")

# Kolmogorov-Smirnov Test
ks1 <- ks.test(data100,data140); ks1
ks2 <- ks.test(data100,data180); ks2
ks3 <- ks.test(data140,data180); ks3


# Tied Data (datos repetidos)

set.seed(549) #ks Sí y wilcox no, perm NO
set.seed(559) #ks Sí y wilcox Sí, perm SI
set.seed(100) #ks Sí y wilcox Sí, perm NO
set.seed(122) #ks No y wilcox No, perm NO
set.seed(7003) #...

tied_dat <- c(round(rep(1:10, 2)), rep(20, 5), rep(30, 5))
not_tied_dat <- runif(30, 1, 30)

tied_df <- data.frame(tied_dat, not_tied_dat)
tied_df <- gather(data=tied_df, key = "type", value = "values")

ggplot(tied_df, aes(x=type, y=values, colour=type))+
  geom_boxplot()
dev.new()
ggplot(tied_df, aes(x=values, colour=type)) + #crea la gráfica: ggplot(datos adjuntos, aes(x=datos apara eje x, color= nombre del grupo o columna))
  stat_ecdf() 

wilcox.test(tied_dat, not_tied_dat)
ks.test(tied_dat, not_tied_dat)

### !!!!!! SI HAY PRESENCIA DE "TIES", HACER ESTO:

library(exactRankTests)
wilcox.exact(tied_dat, not_tied_dat, exact = TRUE) # el cambio es mínimo

# Revisar la homogeneidad de la varianza (homocedasticidad).
# Si p es mayor a 0.05 las varianzas son homogeneas.
bartlett.test(values ~ type, data = tied_df)

# Si la varianza es homogenea, hacer test de permutación
perm.test(tied_dat, not_tied_dat)

# Si la varianza NO es homogenea, hacer esto:
rand <- runif(30, -1e-6, 1e-6)
#plot(rand, 1:30)
tied_df[1:30,2] <- tied_df[1:30,2] + rand

wilcox.test(tied_df[1:30,2], tied_df[31:60,2])
ks.test(tied_df[1:30,2], tied_df[31:60,2])

#kruskal.test(values ~ type, data = tied_df)
