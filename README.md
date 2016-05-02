# 02.mayo.2016
obtener residuales y su análisis correspondiente

crepib <- read.csv("C:\\Users\\SALA-C9\\Downloads\\CRECIMIENTO DEL PIB.csv")
crepibts <- ts (crepib [,2], start = 1960, frequency = 1, end = 2015)
View (crepibts)
plot(crepibts)
## se observa en la grafica dos momentos en el crecimiento de méxico,
## de 1960b a 1980 el crecimeinto estaba entre el 5 y 10%, y del 1980 a 2015
## las tasas estan de 0 a 5%, hay dos grandes caidas, en 1994, y 2009, por la devaluacion

require (fpp)
plot(crepibts, main= "CRECIMIENTO DEL PIB EN MÉXICO", xlab = "años", ylab = "%PIB")
RES <- residuals(snaive(crepibts))
RES2 <- residuals(rwf(crepibts, drift = T))

RES

plot(RES, main= "RESIDUALES DE INGENUO ESTACIONAL", xlab = "años", ylab = "RESIDUALES")
## LA VARAIANZA NO ES CONSTANTE
Acf(RES, main = "Acf de residuales")
## NO HAY CORRELACION ENTRE LOS ERRORES DADO QUE SOLO SALE UNA LINEA DEL INTERVALO
hist(RES, main = "Histograma de Residuales")
## VEMOS QUE SE COMPORTAN DE MANERA NORMAL
## resudiales = datos observado menos dato pronosticado

plot(RES2, main= "RESIDUALES CON EL METODO DE LA DERIVA", xlab = "años", ylab = "RESIDUALES")
Acf(RES2, main = "Acf de residuales")
hist(RES2, main = "Histograma de Residuales")

RES3 <- residuals(meanf(crepibts))
plot(RES3, main= "RESIDUALES CON EL METODO DE LA MEDIA", xlab = "años", ylab = "RESIDUALES")
Acf(RES3, main = "Acf de residuales")
hist(RES3, main = "Histograma de Residuales")

RES4 <- residuals(naive(crepibts))
plot(RES4, main= "RESIDUALES CON EL METODO INGENUO", xlab = "años", ylab = "RESIDUALES")
Acf(RES4, main = "Acf de residuales")
hist(RES4, main = "Histograma de Residuales")

## analizar los residuales

Box.test(RES, lag = 10, fitdf = 0) ## box- pierce, lag son los momentos que estas pronosticando, fitdf son los grados de libertad
Box.test(RES, lag = 10, fitdf = 0, type = "Lj") ## box ljung

Box.test(RES2, lag = 10, fitdf = 0) ## box- pierce
Box.test(RES2, lag = 10, fitdf = 0, type = "Lj") ## box ljung

Box.test(RES3, lag = 10, fitdf = 0) ## box- pierce
Box.test(RES3, lag = 10, fitdf = 0, type = "Lj") ## box ljung

Box.test(RES4, lag = 10, fitdf = 0) ## box- pierce
Box.test(RES4, lag = 10, fitdf = 0, type = "Lj") ## box ljung

## si p-value es mayor a 0.05 no se rechaza la hipotesis nula, las correlaciones de nuestros
## residuos con independientes
