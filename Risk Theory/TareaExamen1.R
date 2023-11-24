######################## TAREA - EXAMEN I ############################
### Córdoba Ríios Ingrid 
### González Eslava Rodrigo Ernesto
### Varela López Ana Karen

library(plyr)
library(readr)
library(modeest)
library(actuar) 
library(fitdistrplus)
library(psych)
library(ADGofTest) 
library(BB)
library(MASS)
library(evir)
library(evd)
library(ismev)
library(vcd)
library(lattice)
library(mixtools)
library(mclust)
library(mixdist)
library(eva)
library(nortest)
library(MASS)
library(psych)



# Ej 4A) ------------------------------------------------------------------
#Vector de observaciones
x<-c(25, 457, 82, 680, 115, 855, 126, 877, 155, 974, 161, 1193, 243, 1340, 294, 1884, 340, 2558, 384, 3476)
#Medidas descriptivas#
summary(x)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. #
#25.0   159.5   420.5   811.0  1028.8  3476.0 #
tail(x)
quantile(x)
var(x)
sd((x))
range(x)
median(x)
library(modeest)
mfv(x)
#sesgo y curtosis, moda
library(psych)
skewness(x) #Asimétirca hacia la derecha
kurtosis(x)
#histograma
hist(x,freq=F,breaks = 25,col="deeppink",ylim=)#¿de qué le veo cara?
d<-density(x)
lines(d)

#Veamos a que podemos ajustar
library(fitdistrplus)
descdist(x)

#Cómo podemos ver se encuentra cerca de una exponencial y una gamma, hagamos estimación de paramteros
#Por igualación de momentos
f_exp<-fitdist(x,"exp","mme")
plot(f_exp)
f_exp

f_gamma<-fitdist(x,"gamma","mme")
plot(f_gamma)
f_gamma

#Por máxima verosimilitud
f_exp_ml<-fitdistr(x,"exponential")
f_exp_ml

f_gamma_ml<-fitdistr(x,"gamma")
f_gamma_ml

#Pruebas de Hipótesis

######################
##Kolmogorov-Smirnof##
######################
ks.test(x,pexp,f_exp_ml$estimate[1])
ks.test(x,pgamma,f_gamma_ml$estimate[1],f_gamma_ml$estimate[2])
######################
###Anderson-Darling###
######################
ADGofTest::ad.test(x,pexp,f_exp_ml$estimate[1])
ADGofTest::ad.test(x,pgamma,f_gamma_ml$estimate[1],f_gamma_ml$estimate[2])






# Ej 4B) ------------------------------------------------------------------

#Vector de observaciones
y<-c(3 ,11, 27, 36, 47, 49, 54, 77, 78, 85,
     104,121, 130, 138, 139, 140 ,143,153, 193, 195,
     205, 207, 216, 224, 233, 237, 254, 257 ,259, 265,
     273 ,275 ,278, 281, 396, 405, 412, 423, 436, 456,
     473,475, 503, 510, 534, 565, 656, 656, 716, 734,
     743, 756, 784, 786, 819 ,826 ,841 ,842, 853, 860,
     877, 942, 942 ,945 ,998 ,1029, 1066, 1101, 1128, 1167,
     1194, 1209, 1223, 1283, 1288, 1296, 1310, 1320,1367 ,1369,
     1373 ,1382 ,1383 ,1395, 1436 ,1470 ,1512 ,1607 ,1699 ,1720,
     1772 ,1780 ,1858,1922,2042,2247, 2348,2377, 2418, 2795,
     2964 ,3156 ,3858, 3872, 4084 ,4620 ,4901, 5021, 5331, 5771,
     6240 ,6385 ,7089, 7482,8059 ,8079, 8316, 11453, 22274 ,32043)
#yi<-y/100000


#Medidas descriptivas#
summary(y)
tail(y)
quantile(y)
var(y)
sd((y))
range(y)
median(y)
mfv(y)
#sesgo y curtosis
skew(y) #Asimétirca hacia la derecha
kurtosi(y)#Leptocúrtica

#histograma
hist(y,freq=F,breaks = 50,col= "cadetblue1")#¿de qué le veo cara?
d<-density(y)
lines(d)

u<-seq(min(y),max(y),by=100)

#una pareto?
lines(u,dpareto(u,0.5,30),col="darkorange",lwd=1,lty=1)
#una gamma?
lines(u,dgamma(u,0.5,0.00047),col="green4",lwd=1,lty=2)
#una lognormal?
lines(u,dlnorm(u,6.83,1),col="deepskyblue2",lwd=1,lty=3)
lines(u,dlnorm(u,6.6,1.5),col="firebrick1",lwd=1,lty=4)
# leyenda
legend( x=20000, y=0.0006,
        legend = c("pareto(0.5,30)","gamma(0.5,0.00047)","lognormal(6.83,1)", "lognormal(6.6,1.5)"),
        col = c("darkorange", "green4","deepskyblue2","firebrick1"),
        lty=1:4, horiz = FALSE)

#Veamos a que podemos ajustar
descdist(y)

#Ajustamos una log-normal

f5p<-fitdist(y,"lnorm")
plot(f5p)
f5p
f5<-fitdistr(y,"lognormal")
f5
#pruebas de bondad f5p
ks.test(y,plnorm,f5p$estimate[1],f5p$estimate[2])
ADGofTest::ad.test(y,plnorm,f5p$estimate[1],f5p$estimate[2])
#Por lo tanto el modelo que se ajusta a nuestros dato
#es una Log-normal con media 6.62417 y var 1.51124

hist(y,freq=F,breaks = 50,col= "cadetblue1")#¿de qué le veo cara?
d<-density(y)
lines(d)
lines(u,dlnorm(u,6.62417,1.51124),col="steelblue4",lwd=2,lty=5)
# leyenda
legend( x=20000, y=0.0006,
        legend = c("lognormal(6.62417,1.51124"),
        col = c("steelblue4"),
        lty=5, horiz = FALSE)



# Ej 5) -------------------------------------------------------------------

x = c(0,1000)
curve(dpareto2(x, min = 0.5, shape = 2.5, rate = 1/150, scale = 150), 
      xlim = c(0,90), ylim=c(0,0.017), col="orange", lwd=2, ylab = " ")
par(new=TRUE)
curve(dgamma(x, 0.2, rate=500), col="green",lwd=2, ylab = " ")
title(main = "Gamma y Pareto II")

x = 0:1000
curve(dpareto(x, 2.5,150, log = FALSE), col="orange", lwd=2, ylab = " ", 
      xlim = c(-1,5000))
par(new=TRUE)
#curve(dgamma(x, 0.2, 500), xlab = "x", ylab = " ", xlim=c(0, 0.0003), 
#      col = 3, lwd = 3, main = "Gráfico distribución Pareto II vs Gamma")
curve(dgamma(x,shape = 2.5, scale = 500), ylab = " ", main="Gamma vs Pareto II", 
      lwd=2, col="green", xlim = c(-1,5000))



# Ej 7) -------------------------------------------------------------------
## Datos
data(danishuni)
dan <- danishuni
str(dan)

## Medidas descriptivas
summary(dan)

## Se usa sort para ordenar
danord <- sort(dan$Loss,decreasing = TRUE)

## Se define el vector que se usara como media
mrgo <- c()

## Se usa como umbral los valores de perdidas
for(i in 1:length(danord)-1){
  mrgo[i] = sum(danord[1:i])/i - danord[i + 1]
}
head(mrgo)

plot(danord[-(length(danord)-1)],mrgo, type = "l", main = "e(x)", xlab = "x (perdida)", ylab = "e(x)" , lwd = 2, col = "blue")

## Se modifica el orden de mrgo
reorden <- rev(mrgo)

condicion = FALSE
jc = 1

while(condicion == FALSE){
  condicion = all(diff(reorden[jc:(length(danord)-1)]) >= 0)
  alt = jc
  jc = jc + 1
}

## Para el umbral buscado
u = danord[(length(danord)-1)-alt+1]

# Graficamos con el umbral
plot(danord[-(length(danord)-1)],mrgo, type = "l", main = "Funcion e(x) con umbral 152.4132", xlab = "x", ylab = "e(x)" , lwd = 2, col = "blue")
abline(v=u)




# Ej 11) ------------------------------------------------------------------

# Realiza un análisis descriptivo a la variable Losses.in.T housands de la 
# base RModuleDay.csv.Ajusta la densidad adecuada, calcula el V aR y T V aR 
# real al 99%, el estimado (de acuerdo al modelo ajustado) y compara.

setwd("~/Teoría del riesgo/Tareas")                                                                        
datos <- read_csv("RModuleDay.csv")

# Análisis descriptivo
head(datos)
summary(datos)

losses<- datos$`Losses in Thousands`
summary(losses)
sum(is.na(losses))
skewness(losses)

hist(losses, main = "Losses in Thousands", col = "deepskyblue2", 
     xlab = "Pérdidas", ylab="Frecuencia", xlim = c(0,2500),  
     ylim = c(0,0.0025), probability = T, breaks = 65)
lines(density(losses), col ="firebrick", lwd = 3)
abline(v=345.5913, col ="firebrick2", lwd=2, lty=2)

plot(losses, type="p", main="Losses in Thousands", xlab = "Observación",
     ylab = "Monto", col= "pink3", lwd =2)
abline(h=1500, col ="deepskyblue4", lwd=2, lty=2)


count(losses>1500)

# moda:
mlv(losses, method = "meanshift")

## Sin outliers
losses<-losses[losses<1500]
summary(losses)
mlv(losses, method = "meanshift")

hist(losses, main = "Losses in Thousands without outliers", col = "deepskyblue3", 
     xlab = "Pérdidas", ylab="Frecuencia", xlim = c(0,2000),  
     ylim = c(0,0.0025), probability = T, breaks = 65)
lines(density(losses), col ="firebrick", lwd = 3)
abline(v=345.5099, col ="firebrick4", lwd=2, lty=2)

# Ajuste del modelo

# Sugerencia de ajuste:
descdist(losses,discrete=FALSE,,boot=3000,obs.pch = 19, boot.col="darkviolet")

fit.lnorm<-fitdist(losses, "lnorm",method="mse")

fit.weibull<-fitdist(losses, "weibull",method="mse")

fit.gamma<-fitdist(losses, "gamma",method="mme")


x<- seq(min(losses), max(losses), by=0.1)
hist(losses, main = "Losses in Thousands without outliers adjusted", col = "deeppink3", 
     xlab = "Pérdidas", ylab="Frecuencia", xlim = c(0,2000),  
     ylim = c(0,0.0025), probability = T, breaks = 55)
lines(x, dlnorm(x,fit.lnorm$estimate[[1]],fit.lnorm$estimate[[2]] ), col ="yellow", lwd = 3)
lines(x, dweibull(x,fit.weibull$estimate[[1]],fit.weibull$estimate[[2]] ), col ="blue", lwd = 3)
lines(x, dgamma(x,fit.gamma$estimate[[1]],fit.gamma$estimate[[2]] ), col ="green", lwd = 3)
legend(locator(1), c("Log-normal(5.74, 0.73)", "Weibull(1.65,439.40)", "Gamma(2.86, 0.007)"), col = c("yellow", "blue","green"), lwd=2, cex =.9)

## Para el calculo de QQPlot
QQ<-fitdist(rmd$Losses_in_Thousands,"lnorm")
plot(QQ)
QQ

# VaR y TVar con los datos
VaR1<-qlnorm(.95,fit.lnorm$estimate[[1]],fit.lnorm$estimate[[2]] )
count(losses>1027.005)
TVaR1<- sum(losses[losses>1027.005])/270


### Simulación para dar una medida de variabilidad al VaR y TVaR
n<-1000
m<-1000
y<-rep(0,n)
x<-matrix(0,n,m)

for(i in 1:n){
  x[i,]<-rlnorm(m,fit.lnorm$estimate[[1]],fit.lnorm$estimate[[2]])
  y[i]<-quantile(x[i,],probs=0.95)
  
}

c(mean(y),quantile(y,probs=c(0.025,0.975)))

# TVaR
k<-rep(0,n)
y2<-rep(0,n)
x2<-matrix(0,n,m)
for(i in 1:n){
  
  x2[i,]<-sort(rlnorm(m,fit.lnorm$estimate[[1]],fit.lnorm$estimate[[2]]))
  k<-floor(0.95*m)+1
  y2[i]<-sum(x2[i,][k:m])/(m-k+1)
  
}

c(mean(y2),quantile(y2,probs=c(0.025,0.975)))





# Ej 10) ------------------------------------------------------------------
n<-1000
m<-1000

CondVaR<-function(x,n,m,VaR){
  aux<-c()
  for(i in 1:n){
    for(j in 1:m){
      if(x[i,j]>VaR){
        aux[j]<-x[i,j]
      }
    }
  }
# U(0,1000)
## VaR
set.seed(123)
y1<-rep(0,n)
x1<-matrix(0,n,m) 

for(i in 1:n){
  x1[i,]<-runif(m, 0, 1000) #muestra 'i' de valores aleatorios de una U(0,1000)
  y1[i]<-quantile(x1[i,],probs=0.9) #VaR al 90% de cada muestra de tamaño  'm'
  
}


VaR1<-mean(y1)
qunif(.9,0,1000)
# TVaR
TVaR1<-CondVaR(x1,n,m,VaR1)

hist(x1, main= "Histograma de simulaciones U(0,1000)", col = "deepskyblue2", 
     xlab = "Simulaciones", ylab="Frecuencia")
abline(v=VaR1, col = "firebrick", lty = 2, lwd=2)
abline(v=TVaR1, col = "red", lty =3,, lwd=2)
legend(0, 50000, c(paste("VaR:",round(VaR1,2)), paste("TVaR:", round(TVaR1,2))), col = c("firebrick", "red"), lty =c(2,3), cex =.8)


# Parteo(1.6,2)
## VaR
library(actuar)
set.seed(123)
y2<-rep(0,n)
x2<-matrix(0,n,m) 

for(i in 1:n){
  x2[i,]<-rpareto(m,shape=1.6,scale=2) #muestra 'i' de valores aleatorios de una Pareto
  y2[i]<-quantile(x2[i,],probs=0.9) #VaR al 90% de cada muestra de tamaño  'm'
  
}

VaR2<-mean(y2)
qpareto(.9,1.6,2)
# TVaR

TVaR2<-CondVaR(x2,n,m,VaR2)

hist(x2, main= "Histograma de simulaciones Pareto(1.6,2)", col = "deeppink", 
     xlab = "Simulaciones", ylab="Frecuencia", breaks =10000, xlim= c(0,60))
abline(v=VaR2, col = "deeppink4", lty = 2, lwd=2)
abline(v=TVaR2, col = "mediumorchid4", lty =3, lwd=2)
legend(30, 4e+05, c(paste("VaR:",round(VaR2,2)), paste("TVaR:", round(TVaR2,2))),
       col = c("deeppink4", "mediumorchid4"), lty =c(2,3))


# Exp(media = 1)
## VaR
set.seed(123)
y3<-rep(0,n)
x3<-matrix(0,n,m) 

for(i in 1:n){
  x3[i,]<-rexp(m,1) #muestra 'i' de valores aleatorios de una Exponenxia
  y3[i]<-quantile(x3[i,],probs=0.95) #VaR al 95% de cada muestra de tamaño'm'
  
}

VaR3<-mean(y3)

# TVaR

TVaR3<-CondVaR(x3, n,m, VaR3)

hist(x3, main= "Histograma de simulaciones \n Exponencial(media=1000)", col = "olivedrab3", 
     xlab = "Simulaciones", ylab="Frecuencia", breaks =100, xlim= c(0,8000))
abline(v=VaR3, col = "olivedrab4", lty = 2, lwd=2)
abline(v=TVaR3, col = "orangered4", lty =3, lwd=2)
legend(4000, 150000, c(paste("VaR:",round(VaR3,2)), paste("TVaR:", round(TVaR3,2))),
       col = c("olivedrab4", "orangered4"), lty =c(2,3))


# Lognormal(?? = 5, ?? = 2)
## VaR
set.seed(123)
y4<-rep(0,n)
x4<-matrix(0,n,m) 

for(i in 1:n){
  x4[i,]<-rlnorm(m,5,2) #muestra 'i' de valores aleatorios de una Pareto
  y4[i]<-quantile(x4[i,],probs=0.9) #VaR al 90% de cada muestra de tamaño  'm'
  
}

VaR4<-mean(y4)
qlnorm(.9,5,2)
# TVaR
TVaR4<-CondVaR(x4,n,m,VaR4)

hist(x4, main= "Histograma de simulaciones \n Lognormal(5,2)", col = "darkgoldenrod1", 
     xlab = "Simulaciones", ylab="Frecuencia", breaks =10000, xlim= c(0,10500))
abline(v=VaR4, col = "darkcyan", lty = 2, lwd=2)
abline(v=TVaR4, col = "darkblue", lty =3, lwd=2)
legend(3000, 4e+05, c(paste("VaR:",round(VaR4,2)), paste("TVaR:", round(TVaR4,2))),
       col = c("darkcyan", "darkblue"), lty =c(2,3))

#N(?? = 1000, ?? = 1000)
## VaR
set.seed(123)
y5<-rep(0,n)
x5<-matrix(0,n,m) 

for(i in 1:n){
  x5[i,]<-rnorm(m,1000,1000) #muestra 'i' de valores aleatorios de una Normal
  y5[i]<-quantile(x5[i,],probs=0.9) #VaR al 90% de cada muestra de tamaño  'm'
  
}

VaR5<-mean(y5)
qnorm(.9,1000,1000)
# TVaR
TVaR5<-CondVaR(x5,n,m,VaR5)

hist(x5, main= "Histograma de simulaciones \n Normal(1000,1000)", col = "plum2", 
     xlab = "Simulaciones", ylab="Frecuencia")
abline(v=VaR5, col = "purple4", lty = 2, lwd=2)
abline(v=TVaR5, col = "turquoise4", lty =3, lwd=2)
legend(-3500, 150000, c(paste("VaR:",round(VaR5,2)), paste("TVaR:", round(TVaR5,2))),
       col = c("purple4", "turquoise4"), lty =c(2,3), cex=.5)





# Ej 12) ------------------------------------------------------------------

T1<- data.frame(
  "reclamaciones"=0:6,
  "frecuencias"=c(65623,12571,1644,148,13,1,0)
)

#Utilizamos la formula k*p_k/p_k_-1
pt1<-c()
for(i in 2:7){
  pt1[i]<-T1[i,1]*T1[i,2]/T1[i-1,2]
}
pt1
#Con los datos que tenemos grafiquemos y tratemos de ajustar alguna distirbución
plot(pt1,col="blue",ann=FALSE)
title(main = "Aproximación de la distribución por medio de las pk",
      ylab = "k*pk/p(k-1)",
      xlab = "k")

#Por el tipo de pendiente asumiremos Binomial negativa,estimemos sus parametros
x_aux=0
for (i in 1:7) {
  x_aux=x_aux+(T1[i,1]*T1[i,2])
}
x_esp=x_aux/80000

x_aux2=0
for (i in 1:7) {
  x_aux2=x_aux2+((T1[i,1]^2)*T1[i,2])
}
x_2m=x_aux2/80000
#Utlizamos la formula recursiva
beta_estimada<-((x_2m-x_esp^2)/x_esp)-1
k_estimada<-x_esp^2/(x_2m-x_esp-x_esp^2)

#Calculemos nuestra a & b
a<-beta_estimada/(1+beta_estimada)
b<-(k_estimada-1)*(beta_estimada/(1+beta_estimada))

#Ahora nuestras respectivas probabilidades
p<-matrix(nrow = 7,ncol = 1)
p[1,1]<-(1+beta_estimada)^-k_estimada
for(i in 1:6){
  p[1+i,1]<-p[i,1]*(a+b*1/i)
}
#Hacemos el producto de las probabilidades por el total y comparamos con vlaores originales
valores_estimados<-data.frame("reclamaciones"=0:6,"frecuencia"=p*80000)
valores_estimados[,-1] <-round(valores_estimados[,-1],2)


################################################
################ TABLA 2 #######################
################################################

T2<- data.frame(
  "reclamaciones"=0:7,
  "frecuencias"=c(861,121,13,3,1,0,1,0)
)

#Utilizamos la formula k*p_k/p_k_-1
pt2<-c()
for(i in 2:8){
  pt2[i]<-T2[i,1]*T2[i,2]/T2[i-1,2]
}
pt2
#Con los datos que tenemos grafiquemos y tratemos de ajustar alguna distirbución
plot(pt2,col="red",ann=FALSE)
title(main = "Aproximación de la distribución por medio de las pk",
      ylab = "k*pk/p(k-1)",
      xlab = "k")

#Por el tipo de pendiente asumiremos Binomial negativa,estimemos sus parametros
y_aux=0
for (i in 1:7) {
  y_aux=y_aux+(T2[i,1]*T2[i,2])
}
y_esp=y_aux/1000

y_aux2=0
for (i in 1:7) {
  y_aux2=y_aux2+((T2[i,1]^2)*T2[i,2])
}
y_2m=y_aux2/1000
#Utlizamos la formula recursiva
beta2_estimada<-((y_2m-y_esp^2)/y_esp)-1
k2_estimada<-y_esp^2/(y_2m-y_esp-y_esp^2)

#Calculemos nuestra a & b
a2<-beta2_estimada/(1+beta2_estimada)
b2<-(k2_estimada-1)*(beta2_estimada/(1+beta2_estimada))
#Ahora nuestras respectivas probabilidades
p2<-matrix(nrow = 8,ncol = 1)
p2[1,1]<-(1+beta2_estimada)^-k2_estimada
for(i in 1:7){
  p2[1+i,1]<-p2[i,1]*(a2+b2*1/i)
}
#Hacemos el producto de las probabilidades por el total y comparamos con valores originales
valores_estimados2<-data.frame("reclamaciones"=0:7,"frecuencia"=p2*1000)
valores_estimados2[,-1] <-round(valores_estimados2[,-1],2)




# Ej 14) ------------------------------------------------------------------

# Función de densidad
f<-dgeom(0:5, 1/3)
f
# E[X]
sum(c(0:5)*f)

# Función de distribución
f2<-pgeom(0:5, 1/3)
f2


#Funci ??on de distribuci ??on de la clase (a,b,0):
beta<-2; r<-1

a<-beta/(1+beta);b<-0

P<-numeric()

P[1]<-(1+beta)^(-r)

for(k in 1:5){P[k+1]<-P[k]*(a+b*(1/k))}

P

# E[X]
sum(c(0:5)*P)

### Distribucion truncada en cero. (PT0=0)

PT<-numeric()

PT[1]<-P[2]/(1-P[1])

for(k in 2:5){PT[k]<-(a+b/k)*PT[k-1]}

PT<-c(0,PT)

PT
# E[X]
sum(c(0:5)*PT)

#### Distribucion modificada en cero (PM0=0.6)

PM<-numeric()

PM[1]<-(1-(1/6))*P[2]/(1-P[1])


for(k in 2:5){PM[k]<-(a+b/k)*PM[k-1]}

PM<-c(1/6,PM)

PM
# E[X]
sum(c(0:5)*PM)


plot(0:5, P, type="o", col ="dodgerblue1", lwd =2, 
     main = "Funciones de densidad", xlab= "k", ylab ="P")
lines(0:5, PT, type="o", col ="turquoise4", lwd =2)
lines(0:5, PM, type="o", col ="mediumvioletred", lwd =2)
legend(3.5, .3, c("(a,b,0)", "Zero truncada", "Zero modificada" ),
       col = c("dodgerblue1", "turquoise4","mediumvioletred"), lty =1, cex=.5)

plot(0:5, f2, type="o", col ="dodgerblue1", lwd =2, 
     main = "Funciones de distribución ", xlab= "k", ylab ="P")
lines(0:5, cumsum(PT), type="o", col ="turquoise4", lwd =2)
lines(0:5, cumsum(PM), type="o", col ="mediumvioletred", lwd =2)
legend(3., .55, c("(a,b,0)", "Zero truncada", "Zero modificada" ),
       col = c("dodgerblue1", "turquoise4","mediumvioletred"), lty =1, cex=.5)

qexp(.95,1)
qnorm(.95,0,1)
1-pnorm(1.625,0,1)



# Ej 16) ------------------------------------------------------------------

par(mfrow=c(2,2))
#Para la U(5000) con limte=4500
#Funcion para la densidad de Yl
Yl_u<-coverage(dunif,punif,limit=4500,per.loss=T) 

#Graficamos la Uniforme(0,5000)
curve(dunif(x,0,5000),xlim=c(0,5000),col="deepskyblue",lwd=4)
#Graficamos la densidad de Yl
curve(Yl_u(x,0,5000),add=T, col="aquamarine",lwd=2) 
title(main="Densidades uniforme(5000) vs Yl",sub="Yl uniforme",col.sub="aquamarine" )

##Funcion para la densidad de Yp#
Yp_u<-coverage(dunif,punif,limit=4500) 
#Graficamos la Uniforme(0,5000)#
curve(dunif(x,0,5000),xlim=c(0,5000),col="deepskyblue",lwd=4) 
#Graficamos la densidad de Yp
curve(Yp_u(x,0,5000),add=T, col="cyan1",lwd=2) 
title(main="Densidades uniforme(5000) vs Yp",sub="Yp uniforme",col.sub="cyan1" )

#Funcion para la densidad de Yl
Yl_u<-coverage(dunif,punif,limit=4500,per.loss=T) 

#Graficamos la Uniforme(0,5000)
curve(dunif(x,0,5000),xlim=c(0,5000),ylim=c(0,0.1),col="deepskyblue",lwd=4)
#Graficamos la densidad de Yl
curve(Yl_u(x,0,5000),add=T, col="aquamarine",lwd=2) 
title(main="Densidades uniforme(5000) vs Yl",sub="Yl uniforme",col.sub="aquamarine" )

##Funcion para la densidad de Yp#
Yp_u<-coverage(dunif,punif,limit=4500) 
#Graficamos la Uniforme(0,5000)#
curve(dunif(x,0,5000),xlim=c(0,5000),ylim=c(0,0.1),col="deepskyblue",lwd=4) 
#Graficamos la densidad de Yp
curve(Yp_u(x,0,5000),add=T, col="cyan1",lwd=2) 
title(main="Densidades uniforme(5000) vs Yp",sub="Yp uniforme",col.sub="cyan1" )


##Para la exp(media=5000) con limite=45000##
par(mfrow=c(2,2))
#Funcion para la densidad de Yl
Yl_e<-coverage(dexp,pexp,limit=4500, per.loss=T) 
#Graficamos exp(5000)
curve(dexp(x,1/5000),xlim=c(0,4600),col="deepskyblue",lwd=4)
#Graficamos la densidad de Yl
curve(Yl_e(x,1/5000),add=T,col="darkolivegreen1")
title(main="Densidades exp(5000) vs Yl",sub="Yl exp",col.sub="darkolivegreen1" )

#Funcion para la densidad de Yp
Yp_e<-coverage(dexp,pexp,limit=4500) 
#Graficamos exp(5000)
curve(dexp(x,1/5000),xlim=c(0,4600),col="deepskyblue",lwd=4)
#Graficamos la densidad de Yp
curve(Yp_e(x,1/5000),add=T, col="darkgreen") 
title(main="Densidades exp(5000) vs Yp",sub="Yp exp",col.sub="darkgreen")


#Funcion para la densidad de Yl
Yl_e<-coverage(dexp,pexp,limit=4500, per.loss=T) 
#Graficamos exp(5000)
curve(dexp(x,1/5000),xlim=c(0,5000),ylim=c(0,0.4),col="deepskyblue",lwd=4)
#Graficamos la densidad de Yl
curve(Yl_e(x,1/5000),add=T,col="darkolivegreen1")
title(main="Densidades exp(5000) vs Yl",sub="Yl exp",col.sub="darkolivegreen1" )

#Funcion para la densidad de Yp
Yp_e<-coverage(dexp,pexp,limit=4500) 
#Graficamos exp(5000)
curve(dexp(x,1/5000),xlim=c(0,5000),ylim=c(0,0.4),col="deepskyblue",lwd=4)
#Graficamos la densidad de Yp
curve(Yp_e(x,1/5000),add=T, col="darkgreen") 
title(main="Densidades exp(5000) vs Yp",sub="Yp exp",col.sub="darkgreen")



# Ej 20) ------------------------------------------------------------------

par(mfrow=c(1,2))
#Para la Pareto(0.78,100) con d=120,u=1000,alpha=0.8, r=0.05
#Funcion para la densidad de Yl
Yl_p<-coverage(dpareto,ppareto,deductible = 120, limit=1000, inflation = 0.05 ,per.loss=T) 

#Graficamos la Pareto(0.78,100)
curve(dpareto(x,0.78,100),xlim=c(0,800),ylim=c(0,0.01),col="deepskyblue",lwd=4)
#Graficamos la densidad de Yl
curve(Yl_p(x,0.78,100),add=T, col="aquamarine",lwd=2) 
title(main="Densidades pareto(0.78,100) vs Yl",sub="Yl pareto",col.sub="aquamarine" )

##Funcion para la densidad de Yp
Yp_p<-coverage(dpareto,ppareto,deductible = 120, limit=1000, inflation = 0.05) 
#Graficamos la Pareto(0.78,100)
curve(dpareto(x,0.78,100),xlim=c(0,800),ylim=c(0,0.01),col="deepskyblue",lwd=4)
#Graficamos la densidad de Yp
curve(Yp_p(x,0.78,100),add=T, col="cyan1",lwd=2) 
title(main="Densidades pareto(0.78,100) vs Yp",sub="Yp pareto",col.sub="cyan1" )


