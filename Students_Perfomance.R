library(tidyverse)
library(dplyr)
library(ggplot2)
library(moments)
library(car)

data<-read.csv("C:/Users/ALAM/Downloads/StudentsPerformance.csv")
#View(data)


#An�lisis Exploratorio de Datos

#VARIABLES
#(variables a utilizar de 5 a 8)

#CUALITATIVAS
genero<-data$gender
etnia<-data$race.ethnicity
almuerzo<-data$lunch


#CUANTITATIVAS DISCRETAS
mate<-data$math.score
lectura<-data$reading.score
escritura<-data$writing.score


# hacer el llamado de Tablas y Gr�ficos correspondiente

#GR�FICAS VARIABLES CUALITATIVAS
barplot(prop.table(table(data$gender)),col=c("red","blue"),
        legend.text=c("Mujeres","Hombres"), xlim=c(0,4.3))

# Gr�fico de barras de frecuencia relativa de las etnias
# Tabla de frecuencias
mi_tabla <- table(etnia)
mi_tabla
barplot(prop.table(mi_tabla) * 100, main = "Frequencia relativa de las �tnias",
        col = rainbow(4))

# Gr�fico de barras de frecuencia absoluta del almuerzo consumido
# Tabla de frecuencias
mi_tabla1 <- table(almuerzo)
mi_tabla1
barplot(mi_tabla1, main = "Frecuencia absoluta del tipo de almuerzo",
        col = rainbow(2))


#muestra puntaje Hombres
Hombres_sc<- data %>%
  filter(gender == "male") %>%
  select("math.score") %>%
  unlist()
#Hombres_sc

#muestra puntaje Mujeres
Mujeres_sc<- data %>%
  filter(gender == "female") %>%
  select(math.score) %>%
  unlist()
#Mujeres_sc

#EXPLORACI�N DESCRIPTIVA DE LAS MUESTRAS
hist(Hombres_sc,xlab="PUNTAJE", main="Puntaje masculino de matem�ticas",ylab = "FRECUENCIA", col = 4) #histograma
qqnorm(Hombres_sc, xlab = "CUANTILES NORMAL EST�NDAR", main="Distribuci�n del 
puntaje en matem�ticas masculino", ylab= "PUNTAJES")
qqline(Hombres_sc, col=4, lwd=2,lty=2) #tendencia de normalidad
boxplot(Hombres_sc, main="Puntaje masculino de matem�ticas", col=4) #diagrama de cajas


hist(Mujeres_sc,xlab="PUNTAJE", main="Puntaje femenino de matem�ticas",ylab = "FRECUENCIA", col = 6) #histograma
qqnorm(Mujeres_sc, xlab = "CUANTILES NORMAL EST�NDAR", main="Distribuci�n del 
puntaje en matem�ticas femenino", ylab= "PUNTAJES")
qqline(Mujeres_sc, col=6, lwd=2,lty=2) #tendencia de normalidad
boxplot(Mujeres_sc, main="Puntaje femenino de matem�ticas", col=6) # diagrama de cajas

#Inferencias Estad�sticas

#Realizar tablas bivariadas
table(genero, almuerzo)#tabla bivariada del g�nero vs almuerzo
table(genero, etnia)#tabla bivariada del g�nero vs etnia
table(genero, etnia)#tabla bivariada del g�nero vs almuerzo

#Pruebas de bondad de ajuste en muestra de mujeres
#H0: el puntaje de matem�ticas en mujeres (en puntaje sobre 100) sigue una distribuci�n normal.

#H1: el puntaje de matem�ticas en mujeres (en puntaje sobre 100) no sigue una distribuci�n normal.

#Nivel de significancia: 0.05 (Hipot�tico).

#GR�FICOS EXPLORATORIOS
hist(Mujeres_sc, xlab = "Puntaje en matem�ticas", ylab = "Frecuencia", las=1, main = "Puntaje femenino en matem�ticas", col = "pink")
plot(density(Mujeres_sc), xlab = "Puntaje en matem�ticas", ylab = "Densidad", las=1, main = "Puntaje femenino en matem�ticas")
qqPlot(Mujeres_sc, xlab="Cuantiles te�ricos", ylab="Puntaje en matem�ticas", las=1,main="Puntaje femenino en matem�ticas")
#Tanto el histograma como el gr�fico de densidad parecen mostrar una distribuci�n no tan normal,
#mientras que en el gr�fico de cuantiles te�ricos vs cuantiles muestrales se observan 
#algunos puntos por fuera de las bandas de confianza.

#EN CADA PRUEBA SE DEBE MOSTRAR: (requerimiento del proyecto)
#media, error est�ndar de la media, mediana, moda, 
#desviaci�n est�ndar, sesgo
data.frame(tama�o=length(Mujeres_sc), #Tama�o
           promedio=round(mean(Mujeres_sc, na.rm = TRUE),2), #media
           mediana=median(Mujeres_sc), #mediana
           desv_est�ndar=round(sd(Mujeres_sc, na.rm = TRUE),2), #desviaci�n est�ndar
           kurtosis=round(kurtosis(Mujeres_sc, na.rm = TRUE),2), #kurtosis
           asimetr�a=round(skewness(Mujeres_sc, na.rm = TRUE),2), #asimetr�a
           m�ximo=max(Mujeres_sc, na.rm = TRUE),m�nimo=max(Mujeres_sc, na.rm = TRUE)) #m�ximo y m�nimo

#cuartiles
quantile(Mujeres_sc, na.rm = TRUE)

#Una vez hecho el an�lisis exploratorio, deben estimarse los par�metros de la 
#distribuci�n hipot�tica, que en este caso es la normal, a partir de la funci�n 
#fitdistr perteneciente al paquete MASS, que tambi�n estima par�metros de muchas 
#otras distribuciones.
require(MASS)
AjusteM<-fitdistr(Mujeres_sc, "normal")
AjusteM
#Lo que arrojar� la funci�n, ser� la estimaci�n de los par�metros de la distribuci�n 
#hipot�tica y su desviaci�n est�ndar que ser� el valor entre par�ntesis debajo de 
#cada par�metro estimado.

#Una vez fueron estimados los par�metros, se usa una prueba de bondad de ajuste 
#para finalmente concluir si la variable analizada sigue o no la distribuci�n 
#hipot�tica planteada. Debe tenerse en cuenta que si la variable en cuesti�n es 
#continua, pueden utilizarse las pruebas Kolmogorov Smirnov y Anderson Darling, 
#y si se cree en un principio que la variable sigue una distribuci�n normal, tambi�n
#puede utilizarse la prueba Shapiro Wilk. Sin embargo, cuando se quiere determinar
#la distribuci�n de una variable discreta, debe usarse la prueba Chi-cuadrado.


#En este caso, se utilizar�n las pruebas Chi-cuadrado, debido a que la variable 
#de inter�s (Puntaje femenino en matem�ticas) es discreta
chisq.test(Mujeres_sc)#ejecuci�n de la prueba de bondad de ajuste
#conclusi�n conforme al valor p obtenido 0.000 menor al nivel de significancia 
#por defecto de la prueba de 0.05 se rechaza H0 a favor de H1, entonces, con un 95%
#es posible afirmar que el puntaje de matem�ticas en mujeres (en puntaje sobre 100) 
#no sigue una distribuci�n normal.


#Pruebas de bondad de ajuste en muestra de hombres
#H0: el puntaje de matem�ticas en hombres (en puntaje sobre 100) sigue una distribuci�n normal.

#H1: el puntaje de matem�ticas en hombres (en puntaje sobre 100) no sigue una distribuci�n normal.

#Nivel de significancia: 0.05 (Hipot�tico).

#GR�FICOS EXPLORATORIOS
hist(Hombres_sc, xlab = "Puntaje en matem�ticas", ylab = "Frecuencia", las=1, main = "Puntaje masculino en matem�ticas", col = "blue")
plot(density(Hombres_sc), xlab = "Puntaje en matem�ticas", ylab = "Densidad", las=1, main = "Puntaje masculino en matem�ticas")
qqPlot(Hombres_sc, xlab="Cuantiles te�ricos", ylab="Puntaje en matem�ticas", las=1,main="Puntaje masculino en matem�ticas")
#Tanto el histograma como el gr�fico de densidad parecen mostrar una distribuci�n no tan normal,
#mientras que en el gr�fico de cuantiles te�ricos vs cuantiles muestrales se observan 
#algunos puntos por fuera de las bandas de confianza.


data.frame(tama�o=length(Hombres_sc), #Tama�o
           promedio=round(mean(Hombres_sc, na.rm = TRUE),2), #media
           mediana=median(Hombres_sc), #mediana
           desv_est�ndar=round(sd(Hombres_sc, na.rm = TRUE),2), #desviaci�n est�ndar
           kurtosis=round(kurtosis(Hombres_sc, na.rm = TRUE),2), #kurtosis
           asimetr�a=round(skewness(Hombres_sc, na.rm = TRUE),2), #asimetr�a
           m�ximo=max(Hombres_sc, na.rm = TRUE),m�nimo=max(Hombres_sc, na.rm = TRUE)) #m�ximo y m�nimo

#cuartiles
quantile(Hombres_sc, na.rm = TRUE)



require(MASS)
AjusteH<-fitdistr(Hombres_sc, "normal")
AjusteH

chisq.test(Hombres_sc) #ejecuci�n de la prueba de bondad de ajuste
#conclusi�n conforme al valor p obtenido 0.000 menor al nivel de significancia 
#por defecto de la prueba de 0.05 se rechaza H0 a favor de H1, entonces, con un 95%
#es posible afirmar que el puntaje de matem�ticas en hombres (en puntaje sobre 100) 
#no sigue una distribuci�n normal.

#Intervalos de confianza 
#La funci�n t.test se usa para calcular intervalos de confianza para la media y 
#diferencia de medias
t.test(Mujeres_sc, y = NULL,
       alternative = c("two.sided"),
       mu = 0,
       conf.level = 0.95)

t.test(Hombres_sc, y = NULL,
       alternative = c("two.sided"),
       mu = 0,
       conf.level = 0.95)

#EXTRA: REGRESI�N LINEAL M�LTIPLE
#prueba de hip�tesis (se realizan en cada supuesto de los modelos)

#PROBANDO NORMALIDAD EN LOS DATOS
#H0: La muestra proviene de una poblaci�n normal
#H1: La muestra NO proviene de una poblaci�n normal

ks.test(x=Hombres_sc,y=pnorm)
ks.test(x=Mujeres_sc,y=pnorm)
#DECISI�N: p=0.000<0.05 de significancia de la prueba, en ambos casos se rechaza H0 a favor de H1,
#por lo tanto, con un 95% de confianza se concluye que, los datos no siguen una distribuci�n homog�nea


#PROBANDO IGUALDAD DE VARIANZA EN LOS DATOS
#H0: sigma^2 Hombres_sc = sigma^2 Mujeres_sc
#H1: sigma^2 Hombres_sc != sigma^2 Mujeres_sc
var.test(x=Hombres_sc, y=Mujeres_sc, ratio=1, alternative="two.sided", conf.level=0.99)
#DECISI�N: p-value = 0.090>0.01 de significancia de la prueba, no se rechaza H0, por lo tanto,
#con un 99% de confianza se afirma que sigma^2 Hombres_sc != sigma^2 Mujeres_sc


#PROBANDO IGUALDAD DE MEDIA EN LOS DATOS (EJECUCION DE LA PRUEBA DEL OBJETIVO)
#H0: Mu_Hombres_sc = Mu_Mujeres_sc
#H1: Mu_Hombres_sc > Mu_Mujeres_sc

#EJECUCION DE LA PRUEBA
t.test(x=Hombres_sc, y=Mujeres_sc,
       alternative = "greater",
       var.equal = F,
       conf.int=T, conf.level = .99)
#DECISI�N: el valor p es 0.000<0.01 menor a la significancia de la prueba, se rechaza H0 a favor de H1,
#por lo tanto, con un 99% de confianza existe evidencia estad�stica para afirmar que las medias no son iguales,
#adem�s la media del promedio de hombres es de 68.728 mayor al promedio de mujeres de 63.633.



#ESTABLECER EL MODELO DE REGRESI�N LINEAL PARA EL PUNTAJE DE MATEM�TICAS DE 
#LOS ESTUDIANTES DE ACUERDO CON SUS PUNTAJES OBTENIDOS EN ESCRITURA Y LECTURA

#Correlaciones
c1<-cor(x=escritura, y=mate)
c2<-cor(x=lectura, y=mate)
data.frame(escritura_matem�tica= c1, lectura_matem�tica= c2)


#GR�FICA
ggplot(data,aes(escritura, mate))+
  geom_point()+
  geom_abline(color="yellow", size=1)+
  labs(title = "Gr�fica de dispersi�n",
       subtitle = "Puntaje de matem�ticas explicado por el puntaje de escritura",
       caption = "Los datos muestran una tendencia creciente positiva",
       x="ESCRITURA", y="MATEM�TICAS")


#GR�FICA
ggplot(data,aes(lectura, mate))+
  geom_point()+
  geom_abline(color="green", size=1)+
  labs(title = "Gr�fica de dispersi�n",
       subtitle = "Puntaje de matem�ticas explicado por el puntaje de lectura",
       caption = "Los datos muestran una tendencia creciente positiva",
       x="LECTURA", y="MATEM�TICAS")

#I) ANALIZAR LA ASOCIACI�N ENTRE LAS VARIABLES DE INSUMO:
#H0: Los puntajes de matem�ticas y escritura no guardan relaci�n lineal
#H1: Los puntajes de matem�ticas y escritura guardan relaci�n lineal
cor.test(na.omit(mate),na.omit(escritura))


#H0: Los puntajes de matem�ticas y lectura no guardan relaci�n lineal
#H1: Los puntajes de matem�ticas y lectura guardan relaci�n lineal
cor.test(na.omit(mate),na.omit(lectura))


#DECISI�N: en las 2 pruebas el valor p es de 0.000<0.05 de significancia de la prueba, 
#por lo tanto, se rechaza H0 a favor de H1, se concluye que tanto el puntaje de escritura 
#como el de lectura guardan relaci�n lineal con el puntaje de matem�ticas

#CREACI�N DEL MODELO
modelo_multiple<-lm(mate ~ escritura+lectura)


#COEFICIENTES DEL MODELO
summary(modelo_multiple)

#COEFICIENTES DEL MODELO
modelo_multiple$coefficients



