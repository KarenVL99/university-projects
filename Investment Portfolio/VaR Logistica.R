library("moments")
library("MASS")
library(nortest)

## Exportar los datos de excel y guardarlos en el vector rendimientos
setwd("C:/Users/HP/Documents/Met. Cuantitativos/Proyecto")
rendimientos <- c(read.csv("RendimientoPortafolio.csv",header= FALSE, sep = ","))
rendimientos <- rendimientos$V1
rendimientos <- 1257265.42*rendimientos

## Paso 1: Vemos como se ve el histograma
h <- hist(rendimientos, freq = FALSE, main = "Histograma y distribución ajustada a P&L",
          xlab = "Ganancias (Pérdidas)", col = "lightskyblue2")

## Paso 2: Probar alguna distribucion parametrica.
## Probamos con la distribucion logistica
ajusteLogistico = fitdistr(rendimientos, "logistic")

## Veamos los estimadores maximo verosimil
ajusteLogistico$estimate
## La matrz de Varianzas Covarianzas de los EMV
ajusteLogistico$vcov

## Intervalos de Confianza para los estimadores
confint(ajusteLogistico)


## Guardo los estimadores en las variables localizacion y escala
localizacion = ajusteLogistico$estimate[1]
escala = ajusteLogistico$estimate[2]


## generar 250 simulaciones de una distribucion logistica con
## parametros localizacion y escala
x = rlogis(250, localizacion, escala)

## Grafico la funcion de densidad generada por esta distribucion y la agrego 
## al histograma
curve(dlogis(x, localizacion, escala), add = T, lwd = 3, lty = 3)

ks.test(rendimientos, "plogis", localizacion, escala)

qlogis(0.01, localizacion, escala)