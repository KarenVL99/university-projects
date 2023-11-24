setwd("C:/Users/HP/Documents/Demografía/ProyectoFinal")
library(data.table)

(SLP2005<-read.csv("SLPlimpio2005.csv"))
save(SLP2005, file= "SLP2005.Rda")

(SLP2010<-read.csv("SLPlimpio2010.csv"))
save(SLP2010, file= "SLP2010.Rda")

##Información por quinquenios 2005
datos2<-data.table(SLP2005)
c2<-c(-.1,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74, 79,84,101)

v1<-datos2[,sum(Mujeres), by=(Edad=cut(Edad, c2))]
v2<-datos2[, sum(Hombres), by=(Edad=cut(Edad, c2))]

a1<-data.frame(v1)
a2<-data.frame(v2)

d1<-a1[,2]
d2<-a2[,2]
d3<-a2[,1]

quinquenios2005<-data.frame(Hombres=d2, Mujeres=d1, Edad=d3)
save(quinquenios2005, file="quinquenios2005.Rda")


##Información por quinquenios 2010
datos1<-data.table(SLP2010)
x2<-c(-.1,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74, 79,84,101)

y1<-datos1[,sum(Mujeres), by=(Edad=cut(Edad, x2))]
y2<-datos1[, sum(Hombres), by=(Edad=cut(Edad, x2))]

w1<-data.frame(y1)
w2<-data.frame(y2)

z1<-w1[,2]
z2<-w2[,2]
z3<-w2[,1]

quinquenios2010<-data.frame(Hombres=z2, Mujeres=z1, Edad=z3)
save(quinquenios2010, file="quinquenios2010.Rda")

#MÉTODO 1/16  2005
metodo16<-function(quinquenios2005){
  PobMujeres<-vector(mode='numeric', length=18)
  PobHombres<-vector(mode='numeric', length=18)
  
  metodo<-data.frame(quinquenios2005, PobMujeres, PobHombres)
  metodo
  for(i in 1:18){
    if(i>=3&i<17){
      metodo$PobMujeres[i]=round((-metodo$Mujeres[i-2]+(4*metodo$Mujeres[i-1])+(10*metodo$Mujeres[i])+(4*metodo$Mujeres[i+1])-metodo$Mujeres[i+2])/16, digits = 0)
      metodo$PobHombres[i]=round((-metodo$Hombres[i-2]+(4*metodo$Hombres[i-1])+(10*metodo$Hombres[i])+(4*metodo$Hombres[i+1])-metodo$Hombres[i+2])/16, digits = 0)
    } else {
      metodo$PobMujeres[i]=metodo$Mujeres[i]
      metodo$PobHombres[i]=metodo$Hombres[i]
    }
  }
  
  a<-metodo$Edad
  b<-metodo$PobHombres
  c<-metodo$PobMujeres
 
  correc2005<-data.frame(Hombres=b, Mujeres=c,Edad=a)
  correc2005
  save(correc2005, file="correc2005.Rda")
}
correc2005
#MÉTODO 1/16   2010
metodo16<-function(quinquenios2010){
  PobMujeres<-vector(mode='numeric', length=18)
  PobHombres<-vector(mode='numeric', length=18)
  
  metodo2<-data.frame(quinquenios2010, PobMujeres, PobHombres)
  metodo2
  for(i in 1:18){
    if(i>=3&i<17){
      metodo2$PobMujeres[i]=round((-metodo2$Mujeres[i-2]+(4*metodo2$Mujeres[i-1])+(10*metodo2$Mujeres[i])+(4*metodo2$Mujeres[i+1])-metodo2$Mujeres[i+2])/16, digits = 0)
      metodo2$PobHombres[i]=round((-metodo2$Hombres[i-2]+(4*metodo2$Hombres[i-1])+(10*metodo2$Hombres[i])+(4*metodo2$Hombres[i+1])-metodo2$Hombres[i+2])/16, digits = 0)
    } else {
      metodo2$PobMujeres[i]=metodo2$Mujeres[i]
      metodo2$PobHombres[i]=metodo2$Hombres[i]
    }
  }
  
  a<-metodo2$Edad
  b<-metodo2$PobHombres
  c<-metodo2$PobMujeres
  
  correc2010<-data.frame(Hombres=b, Mujeres=c,Edad=a)
  correc2010
  save(correc2010, file="correc2010.Rda")
}
correc2010

#DISTRIBUCIÓN NO ESPECIFICADOS 2005
NE<-function(SLP2005){
  Mujer1<-vector(mode='numeric', length=102)
  Hombre1<-vector(mode='numeric', length=102)
  
  datos2<-data.frame(SLP2005, Hombre1, Mujer1)
  
  a <- sum(datos2$Mujeres)
  a1 <- sum(datos2$Hombres)
  
  
  b <- datos2$Mujeres[102]
  b1 <- datos2$Hombres[102]
  
  coefm <- 1+b1/(a-b)
  coefh <- 1+b1/(a1-b1)
  
  datos2$Mujer1=round(datos2$Mujeres*coefm, digits = 0)
  datos2$Hombre1=round(datos2$Hombres*coefh, digits = 0)
  
  Mujeres<-datos2$Mujer1[c(-102)]
  Hombres<-datos2$Hombre1[c(-102)]
  Edad<-datos2$Edad[c(-102)]
  
  NE_2005<-data.frame(Hombres=Hombres,Mujeres=Mujeres, Edad=Edad)
  porcentaje=(b+b1)*100/(a-b+a1-b1)
  save(NE_2005, file="NE_2005.Rda")
}


#DISTRIBUCIÓN NO ESPECIFICADOS 2010
NE<-function(SLP2010){
  Mujer1<-vector(mode='numeric', length=102)
  Hombre1<-vector(mode='numeric', length=102)
  
  datos2<-data.frame(SLP2010, Hombre1, Mujer1)
  
  a <- sum(datos2$Mujeres)
  a1 <- sum(datos2$Hombres)
  
  
  b <- datos2$Mujeres[102]
  b1 <- datos2$Hombres[102]
  
  coefm <- 1+b1/(a-b)
  coefh <- 1+b1/(a1-b1)
  
  datos2$Mujer1=round(datos2$Mujeres*coefm, digits = 0)
  datos2$Hombre1=round(datos2$Hombres*coefh, digits = 0)
  
  Mujeres<-datos2$Mujer1[c(-102)]
  Hombres<-datos2$Hombre1[c(-102)]
  Edad<-datos2$Edad[c(-102)]
  
  NE_2010<-data.frame(Hombres=Hombres,Mujeres=Mujeres, Edad=Edad)
  porcentaje=(b+b1)*100/(a-b+a1-b1)
  save(NE_2010, file="NE_2010.Rda")
}



