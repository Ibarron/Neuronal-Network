############################
#NORMALIZACIÓN_BASE
###########
data<- Schiffer_completos
#ordena y pone nombres a las filas
rn<-data[,1]
dats <- data.frame(data[,-1])
rownames(dats) <- rn$'Etiquetas de fila'#cambia los nombres
dats2 <- na.omit(dats)

#Obtener el resumen de las cosas
library(psych)

sm <- summary(dats2)
sm
desc <- describe(dats2)

setwd("~/Rstudio/Esta_Desc/PrimerasGra")

write.csv(desc,paste0('SHAM_izq.csv'), row.names = TRUE)


#INICIO CICLO fFOR
for(ind in 1:ncol(dats)){
  
  dat<- dats[,ind]#ind indice
  
  
  
  library(lsr)
  who()
  
  ###########Directorio de ttrabajo Primeras##########
  setwd("~/Rstudio/Esta_Desc/PrimerasGra")
  
  png(paste0('serie_lineas',ind, '.png'), width = 4, height = 3, units = "in",res=200)
  plot(dat, type = "l", xlab = "Games", ylab = "Winning Margin")#linea
  dev.off()
  
  png(paste0('datos_lineas',ind, '.png'), width = 4, height = 3, units = "in",res=200)
  plot(dat, type = "b", xlab = "Games", ylab = "Winning Margin")#linea
  dev.off()
  
  png(paste0('hist_lineas',ind, '.png'), width = 4, height = 3, units = "in",res=200)
  hist(dat, xlab = "Winning Margin", col = "orchid3", 
       border = "white", main = "Histogram of Margins")
  dev.off()
  
  # density plot : es como un histograma suavizado
  d <- density(dat)
  png('Densidad_hist.png', width = 4, height = 3, units = "in",res=200)
  plot(d)
  dev.off()
  
  #Fin folder PRIMERAS
  
  #################Medidas puntuales FOLDER##########
  
  setwd("~/Rstudio/Esta_Desc/MedidasPuntuales")
  
  
  # cuantiles = percentiles
  q2<-quantile(dat, probs = c(0.25, 0.75))
  50.50 - 12.75
  iqr <-IQR(dat) # Intercuartil
  
  # varianza
  vv<-var(dat)
  df<- data.frame(q2)
  df2<-data.frame(iqr,vv)
  
  write.csv(df,paste0('Cuartiles',ind,'.csv'), row.names = TRUE)
  write.csv(df2,paste0('IQR_var',ind,'.csv'), row.names = TRUE)
  png(paste0('boxplot',ind, '.png'), width = 4, height = 3, units = "in",res=200)
  boxplot(dat)
  dev.off()
  
  # ::::::::::::::: NORMALIDAD :::::::::::::::::::::::::
  
  setwd("~/Rstudio/Esta_Desc/Normalidad")
  
  
  d <- density(dat) # datos para graficar densidad
  
  png(paste0("QQ.plot",ind,'.png'), width = 12, height = 6, units = "in",res=200) #guardar imagen
  par(mfcol=c(1,3)) # esta linea indica que se dibujaran dos graficas juntas
  plot(d)
  hist(dat) # histograma
  qqnorm(dat) # grafica QQ plot
  dev.off()
  
  library(ggpubr)
  b <- ggqqplot(dat)
  ggarrange( b, 
             labels = c("Normal", "data"),
             label.x = 0.2, label.y = 1,
             ncol = 2, nrow = 1)
  
  
  
  # Shapiro Test para normalidad
  
  # si el p-value es mayor a 0.05, la distribucion es normal 
  s=as.numeric(shapiro.test(dat))
  
  write.csv(s,paste0('Shapiro',ind,'.csv'), row.names = TRUE)
  
  
}
