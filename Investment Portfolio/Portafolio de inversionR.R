#             PORTAFOLIOS DE INVERSIÓN

#             Aguilar Mancera Rosa Guadalupe
#             Casasola Sánchez Itzanamí
#             Galicia Cruz Jaqueline
#             González Eslava Rodrigo Ernesto
#             Hernández García Yesenia Inés
#             Hernández Navarrete Alejandro Daniel
#             Pérez López Lizzeth 
#             Varela López Ana Karen
#             24/06/2020


library(corrplot)
library("moments")
library("MASS")
library(nortest)

setwd("C:/Users/HP/Documents/Met. Cuantitativos/Proyecto")
rendimientos1<-  read.csv("Rendimientos de la cartera.csv", header=T)
rendimientos<- data.frame(rendimientos1$GRUMAB, rendimientos1$LABB, 
                          rendimientos1$GCC, rendimientos1$IPC, rendimientos1$CETES.356.DÍAS)
colnames(rendimientos)<-c("GRUMAB", "LABB", "GCC","IPC", "CETE")
rf<-0.0519
rendimientosPort<-rendimientos1$Ren..Port.[c(-248, -249,-250,-251,-252)]
rendimientosPort <- 1257265.42*rendimientosPort
attach(rendimientos)

#--------------------Ejercicio 1-----------------
correlacion<-cor(rendimientos[c(-4,-5)])
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot.mixed(correlacion, upper = "color", lower = "number",
               tl.pos = "lt", upper.col = col(200), 
               lower.col = "gray35", diag = NULL,
               tl.col = "black", number.cex = .75,
               family="A",order = "hclust", tl.cex = 1.1)


#--------------------Ejercicio 3------------------
robj<-mean(rendimientos$IPC)
robj<-robj+(200*(0.01/100))
r_GRUMAB<-mean(rendimientos$GRUMAB)
r_LABB<-mean(rendimientos$LABB)
r_GCC<-mean(rendimientos$GCC)

mu<-c(r_GRUMAB, r_LABB, r_GCC)
M<-matrix(mu, nrow = 3, byrow = TRUE) #Matriz de esperanzas
V<-var(rendimientos[c(-4,-5)]) #Matriz de varianzas
l<-matrix(c(1,1,1), byrow = TRUE) #Matriz de unos
W<-matrix(c(0,0,0)) #Matriz de pesos vacía
Wp<-matrix(c(0,0,0)) #Matriz de pesos considerando activos libres de riesgo vacía

A<-as.double(t(M)%*%solve(V)%*%l)
B<-as.double(t(M)%*%solve(V)%*%M)
C<-as.double(t(l)%*%solve(V)%*%l)
D<-as.double(B*C-(A^2))
g<-(1/D)*(B*(solve(V)%*%l) - A*(solve(V)%*%M))
h<-(1/D)*(C*(solve(V)%*%M)-A*(solve(V)%*%l))
H<-C*(rf^2)-2*A*rf+B
4*((A^2)-B*C)
Mp<- A/C

#Sin activo libre de riesgo
W<-g + robj*h

#Incluyendo un activo libre de riesgo (rf, CETE 1Y)
Wp<-((robj-rf)/H)*(solve(V)%*%(M-rf*l))

#Dado que sum(Wp)<1, rf_inv es la porción del capital que se invertirá en la rf
rf_inv<-(1-sum(Wp))


#Desviación estándar del portafolio
#(eqivalentes, pruebas)
varianza_mat<-t(Wp)%*%V%*%Wp
varianza<-((robj-rf)^2)/H
sigma<-abs(robj-rf)/sqrt(H)
sqrt(varianza_mat)
rinf<-robj+sigma
rsup<-robj-sigma



#Títulos de cada emisora y títulos de CETEs que tendremos en posición.
K<-2000000  #Capital inicial
r<-0.051917582  #Tasa del BI CETE 210520 a la fecha de valuación
precioCETE<-10/(1+r*(356/360)) #Precio del CETE convención ACT/360

PGRUMAB<-round((Wp[1,1]*K)/220.080002,0)
PLABB<-round((Wp[2,1]*K)/20.209999,0)
PGCC<-round((Wp[3,1]*K)/81.410004,0)
PCETE<- round((rf_inv*K)/precioCETE,0)


#SUmmary
DGRUMAB<-c(Wp[1,1], PGRUMAB)
DLABB<-c(Wp[2,1], PLABB)
DGCC<-c(Wp[3,1], PGCC)
DCETE<-c(rf_inv, PCETE)
DSIGMA<-c(sigma, "rp=[0.011,0.028]" )

suma<-data.frame(DGRUMAB, DLABB, DGCC, DCETE,DSIGMA)
colnames(suma)<-c("GRUMAB", "LABB", "GCC", "CETE", "Desv. Est." )
rownames(suma)<-c("Peso (W)", "Posición")
suma

#       RIESGO DE MERCADO

#--------------Ejercicio 1-------------------------
rm(list = ls(all.names = TRUE))
gc()

##Creamos un vector con los promedios de los rendimientos (Parametros del Movimiento Browniano Geometrico)
promedios = c(mean(GRUMAB),mean(LABB),mean(GCC),mean(CETE))
varianza = c(var(GRUMAB),var(LABB),var(GCC),var(CETE))
#promedios
#varianza

#1) Obtendremos la simulación de los precios de GRUMAB por simulación de Monte Carlo

##Obtenemos el valor inicial del Subyacente (S_0) para la acción GRUMAB
SGRUMAB = 220.08

## Creamos una matriz de 252 renglones por 100 columnas
##Simularemos 100 trayectorias de 252 dias cada una. 
simulados = matrix(nrow=252,ncol=100)

##Llenamos la matriz de trayectorias, columna por columna
for (j in 1:100){
  
  normales= rnorm(252,0,1) ## Creamos 252 numeros aleatorios distribuidos N(0,1)
  for (i in 1: 252){
    ##Segun el modelo construimos los precios historicos.
    simulados[i,j]= SGRUMAB*exp((promedios[1]-0.5*varianza[1])*(i) +sqrt(varianza[1]*(i))*normales[i])
  }
}


plot(simulados[,1],type="l",col=2,ylab="Precio",xlab="Escenario",main="Simulacion Montecarlo GRUMAB")

lines(simulados[,2],type="l",col=5)
for(h in 2:100){
  
  lines(simulados[,h],type="l",col=h)
  
}



lines(simulados[,2],type="l",col="blue")

##Calculemos la trayectoria promedio.

trayectoriaPromedio<-0

for (i in 1:252){
  
  trayectoriaPromedio[i]<- mean(simulados[i,])
}
#Graficamos la trayectoria Promedio
plot(trayectoriaPromedio,type="l",col=2,ylab="Precio",xlab="Escenario",main="Simulacion Montecarlo GRUMAB")

##Exportamos la trayectoria simulada a un archivo csv
#Write.csv(as.data.frame(trayectoriaPromedio),"SGRUMABMontecarlo.csv")

#2) Obtendremos la simulación de los precios de LABB por simulación de Monte Carlo

##Obtenemos el valor inicial del Subyacente (S_0) para la acción LABB
SLABB = 20.209999

#SLABB
## Creamos una matriz de 252 renglones por 100 columnas
##Simularemos 100 trayectorias de 252 dias cada una. 
simulados2 = matrix(nrow=252,ncol=100)

##Llenamos la matriz de trayectorias, columna por columna
for (j in 1:100){
  
  normales= rnorm(252,0,1) ## Creamos 252 numeros aleatorios distribuidos N(0,1)
  for (i in 1: 252){
    ##Segun el modelo construimos los precios historicos.
    simulados2[i,j]= SLABB*exp((promedios[2]-0.5*varianza[2])*(i) +sqrt(varianza[2]*(i))*normales[i])
  }
}


plot(simulados2[,1],type="l",col=2,ylab="Precio",xlab="Escenario",main="Simulacion Montecarlo LABB")

lines(simulados2[,2],type="l",col=5)
for(h in 2:100){
  
  lines(simulados2[,h],type="l",col=h)
  
}



lines(simulados2[,2],type="l",col="blue")

##Calculemos la trayectoria promedio.

trayectoriaPromedio2<-0

for (i in 1:252){
  
  trayectoriaPromedio2[i]<- mean(simulados2[i,])
}
#Graficamos la trayectoria Promedio
plot(trayectoriaPromedio2,type="l",col=2,ylab="Precio",xlab="Escenario",main="Simulacion Montecarlo LABB")

##Exportamos la trayectoria simulada a un archivo csv
#write.csv(as.data.frame(trayectoriaPromedio2),"LABBMontecarlo.csv")

#3) Obtendremos la simulación de los precios de GCC por simulación de Monte Carlo

##Obtenemos el valor inicial del Subyacente (S_0) para la acción GCC
SGCC = 81.410004

#SGCC
## Creamos una matriz de 252 renglones por 100 columnas
##Simularemos 100 trayectorias de 252 dias cada una. 
simulados3 = matrix(nrow=252,ncol=100)

##Llenamos la matriz de trayectorias, columna por columna
for (j in 1:100){
  
  normales= rnorm(252,0,1) ## Creamos 252 numeros aleatorios distribuidos N(0,1)
  for (i in 1: 252){
    ##Segun el modelo construimos los precios historicos.
    simulados3[i,j]= SGCC*exp((promedios[3]-0.5*varianza[3])*(i) +sqrt(varianza[3]*(i))*normales[i])
  }
}


plot(simulados3[,1],type="l",col=2,ylab="Precio",xlab="Escenario",main="Simulacion Montecarlo GCC")

lines(simulados3[,2],type="l",col=5)
for(h in 2:100){
  
  lines(simulados3[,h],type="l",col=h)
  
}



lines(simulados3[,2],type="l",col="blue")

##Calculemos la trayectoria promedio.

trayectoriaPromedio3<-0

for (i in 1:252){
  
  trayectoriaPromedio3[i]<- mean(simulados3[i,])
}
#Graficamos la trayectoria Promedio
plot(trayectoriaPromedio3,type="l",col=2,ylab="Precio",xlab="Escenario",main="Simulacion Montecarlo GCC")

##Exportamos la trayectoria simulada a un archivo csv
#write.csv(as.data.frame(trayectoriaPromedio3),"GCCMontecarlo.csv")

#4) Obtendremos la simulación de las tasas de CETES a 356 días por simulación de Monte Carlo

##Obtenemos el valor inicial del Subyacente (S_0) para los CETES a 356 días
SCETE = 0.05192
#SLABB
## Creamos una matriz de 252 renglones por 100 columnas
##Simularemos 100 trayectorias de 252 dias cada una. 
simulados4 = matrix(nrow=252,ncol=100)

##Llenamos la matriz de trayectorias, columna por columna
for (j in 1:100){
  
  normales= rnorm(252,0,1) ## Creamos 252 numeros aleatorios distribuidos N(0,1)
  for (i in 1: 252){
    ##Segun el modelo construimos las tasas historicas.
    simulados4[i,j]= SCETE*exp((promedios[4]-0.5*varianza[4])*(i) +sqrt(varianza[4]*(i))*normales[i])
  }
}


plot(simulados4[,1],type="l",col=2,ylab="Tasa",xlab="Escenario",main="Simulacion Montecarlo CETES a 356 días")

lines(simulados4[,2],type="l",col=5)
for(h in 2:100){
  
  lines(simulados4[,h],type="l",col=h)
  
}



lines(simulados4[,2],type="l",col="blue")

##Calculemos la trayectoria promedio.

trayectoriaPromedio4<-0

for (i in 1:252){
  
  trayectoriaPromedio4[i]<- mean(simulados4[i,])
}
#Graficamos la trayectoria Promedio
plot(trayectoriaPromedio4,type="l",col=2,ylab="Tasa",xlab="Escenario",main="Simulacion Montecarlo CETES a 356 días")

##Exportamos la trayectoria simulada a un archivo csv
#write.csv(as.data.frame(trayectoriaPromedio4),"CETESMontecarlo.csv")



#---------------Ejercicio 2-------------------------

## Paso 1: Vemos como se ve el histograma
h <- hist(rendimientosPort, freq = FALSE, main = "Histograma y distribuciÃ³n ajustada a P&L",
          xlab = "Ganancias (PÃ©rdidas)", col = "lightskyblue2")

## Paso 2: Probar alguna distribucion parametrica.
## Probamos con la distribucion logistica
ajusteLogistico = fitdistr(rendimientosPort, "logistic")

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

ks.test(rendimientosPort, "plogis", localizacion, escala)

qlogis(0.01, localizacion, escala)

#Obs. Se quitaron los datos más atípicos correspondientes a:los días del 14 de mayo, 27 de marzo, 
#17 de marzo, 12 de marzo y 9 de marzo

#       DERIVADOS 

#-------------------Ejercicio 2-------------------------------
### Función que calcula el valor una opción europea utilizando Black Scholes
## Parámetros ##
#### s es el precio del subyacente a tiempo t
#### k es el precio strike
#### r es la tasa libre de riesgo, tiene que ponerse en decimales
#### sigma es la volatilidad anual, tiene que ponerse en decimales
#### t es el tiempo de valuación, t está entre 0 y el vencimiento (años)
#### V es el vencimiento de la opción en años
#### tipo: "c" para call, "p" para put
#### q es la tasa de dividendos anual compuesta continuamente en decimales
##---Función tomada de los ejemplos vistos en clase

BlackScholes_t <- function(s,k,r,sigma,t=0,V,tipo="c",q=0){
  
  if(tipo=="c") j <- 1
  if(tipo=="p") j <- -1
  cat("s=",s, "r", r, "q=",q, "v=",V, "sig=", sigma, "t=", t)
  d1 <- (log(s/k)+(r+-q+(sigma^2)/2)*(V-t))/(sigma*sqrt(V-t))
  d2 <- (log(s/k)+(r-q-(sigma^2)/2)*(V-t))/(sigma*sqrt(V-t))
  cat("D1=", d1, "D2:", d2)
  f_t <- j*s*exp(-q*(V-t))*pnorm(j*d1) - j*k*exp(-r*(V-t))*pnorm(j*d2)
  cat("N(d1)=",pnorm(j*d1),"N(d2)=",pnorm(j*d2), "c=", f_t)
  
}  


rf<-0.0528
rf<-log(1+rf)
q<-0.03
q<-log(1+q)


X=BlackScholes_t(20.209999,18.510947775,0.05145328159,0.15,0,0.0505555,"c",0.02955880224)
Y=BlackScholes_t(20.209999,21.90905025,0.05145328159,0.15,0,0.0505555,"c",0.02955880224)

