load("SLP2005.Rda")
load("SLP2010.Rda")
load("quinquenios2005.Rda")
load("quinquenios2010.Rda")
load("correc2005.Rda")
load("correc2010.Rda")
load("NE_2005.Rda")
load("NE_2010.Rda")
################EVALUACIÓN DE LA INFORMACIÓN#################
##ÍNDICE DE WHIPPLE
Whipple<-function(datos1){
 
  library(data.table)
  
  rango<-seq(25,60,5)
  d1<-sum(datos1$Hombres[24:63])+sum(datos1$Mujeres[24:63])
  c1<-datos1[rango+1,]
  a1<-sum(c1$Hombres)+sum(c1$Mujeres)
  indice<-(a1/d1)*5*100
  resultado<-c("El índice de Whipple es:", indice)
  print(resultado)
  
  if(indice>=100 & indice<105){
    print('La información es: muy precisa')
  } else {
    if(indice>= 105&indice<110){
      print('La información es: precisa')
    } else {
      if(indice>=110&indice<125){
        print('La información es: aproximada')
      } else {
        if(indice>=125&indice<175){
          print('La información es: deficiente')
        } else {
          if(indice>=175){
            print('La información es: muy deficiente')
          }
        }
      }
    }
  }
}

#ÍNDICE DE MYERS
Myers<- function(datos1){
  library(data.table)
  Total<-vector(mode='numeric', length = 102)
  datos1=data.frame(datos1, Total)
  datos1$Total=datos1$Mujeres+datos1$Hombres
  x<-vector(mode='numeric', length = 10)
  A<-vector(mode='numeric', length = 10)
  B<-vector(mode='numeric', length = 10)
  C<-vector(mode='numeric', length = 10)  
  D<-vector(mode='numeric', length = 10)
  E<-vector(mode='numeric', length = 10)
  F<-vector(mode='numeric', length = 10)
  G<-vector(mode='numeric', length = 10)
  datos2<-data.frame(x,A,B,C,D,E,F,G)
  x1<-seq(0,9,by=1)
  datos2$x<-x1
  x2<-seq(10,60,10)
  x3<-seq(20,70,10)
  
  for(i in 1:10){
    c1<-datos1[x2+1,4]
    a=sum(c1)
    datos2$A[i]=a
    c2<-datos1[x3+i,4]
    b=sum(c2)
    datos2$B[i]=b
    datos2$C[i]=datos2$A[i]*(i)
    datos2$D[i]=datos2$B[i]*(10-i)
    datos2$E[i]=datos2$C[i]+datos2$D[i]
  }
  a<-sum(datos2$E)
  for(i in 1:10){
    datos2$F[i]=(datos2$E[i]/a)*100
    datos2$G[i]=abs(datos2$F[i] - 10)
  }
  
  indice=sum(datos2$G)
  resultado<-c("El índice de Myers es:", indice)
  print(resultado)
 
  if(indice>=0 & indice<5){
    print('Baja Concentración en un dígito')
  } else {
    if(indice>=5 & indice<15){
      print('Mediana Concentración en un dígito')
    } else {
      if(indice>=15&indice<30){
        print('Alta Concentración en un dígito')
      } else {
        if(indice>=30){
          print('Muy alta concentración en un dígito')
        }
      }
    }
  }
}

##ÍNDICE DE NACIONES UNIDAS
INU<-function(datos1){
  library(dplyr)
  tabla1<- datos1 %>% mutate(IM=(datos1$Hombres/datos1$Mujeres)*100)
  
  x<-vector(mode='numeric', length = 18)
  y<-vector(mode='numeric', length = 18)
  resul1<-vector(mode='numeric', length = 18)
  z<-vector(mode='numeric', length = 18)
  resul2<-vector(mode='numeric', length = 18)
  
  x[1]=y[1]=z[1]= -0.1
  x[18]=y[18]=z[18]= -0.1
  resul1[1]=resul2[1]= -0.1
  resul1[18]=resul2[18]=-0.1
  
  
  for(i in 1:16){
    x[i+1]=abs(tabla1$IM[i+1]-tabla1$IM[i])
    y[i+1]=(2*tabla1$Hombres[i+1]/(tabla1$Hombres[i]+tabla1$Hombres[i+2]))*100
    resul1[i+1]=abs(100-y[i+1])
    z[i+1]=(2*tabla1$Mujeres[i+1]/(tabla1$Mujeres[i]+tabla1$Mujeres[i+2]))*100
    resul2[i+1]=abs(100-z[i+1])
  }
  
  tabla1<-data.frame(tabla1,CIM=x, CEH=y, cienMenosCEH=resul1, CEM=z,cienMenosCEM=resul2)
  tabla1
  sumaCMI<-sum(tabla1$CIM)
  sumaCienMenosCEH<-sum(tabla1$cienMenosCEH)
  sumaCienMenosCEM<-sum(tabla1$cienMenosCEM)
  indice<-3*(sumaCMI/16)+(sumaCienMenosCEH/16)+(sumaCienMenosCEM/16)
  cat("El indice de Naciones Unidas es:", indice)
  
  if(indice>=0 & indice<=20){
    print("La informacación es satisfactoria")
  } else {
    if(indice>20 & indice<=40){
      print("La información es intermedia")
    } else print("La información es mala")
  }
  
}

############### MÉTODOS DE CORRECCIÓN# ####################

#DISTRIBUCIÓN NO ESPECIFICADOS
NE<-function(datos1){
Mujer1<-vector(mode='numeric', length=102)
Hombre1<-vector(mode='numeric', length=102)

datos2<-data.frame(datos1, Hombre1, Mujer1)

a <- sum(datos2$Mujeres)
a1 <- sum(datos2$Hombres)


b <- datos2$Mujeres[102]
b1 <- datos2$Hombres[102]

coefm <- 1+b1/(a-b)
coefh <- 1+b1/(a1-b1)

datos2$Mujer1=round(datos2$Mujeres*coefm, digits = 0)
datos2$Hombre1=round(datos2$Hombres*coefh, digits = 0)

Mujeres<-datos2[,5]
Hombres<-datos2[,4]
Edad<-datos2[,3]porcentaje=(b+b1)*100/(a-b+a1-b1)


NEResultado<-data.frame(Hombres=Hombres,Mujeres=Mujeres, Edad=Edad)
cat("El porcentaje de los datos NE respecto a la población total es del:", porcentaje)
print('La tabla por edades con los NE distribuídos queda como:')
print(NEResultado)

}

#AGRUPACIÓN POR QUINQUENIOS
quinquenios<-function(datos1){
  datos1<-data.table(datos1)
  x2<-c(-.1,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74, 79,84,101)
  
  y1<-datos1[,sum(Mujeres), by=(Edad=cut(Edad, x2))]
  y2<-datos1[, sum(Hombres), by=(Edad=cut(Edad, x2))]
  
  w1<-data.frame(y1)
  w2<-data.frame(y2)
  
  z1<-w1[,2]
  z2<-w2[,2]
  z3<-w2[,1]
  
  quinqueniosResultado<-data.frame(Hombres=z2, Mujeres=z1, Edad=z3)
  print("Esta es la información agrupada por quinquenios")
  print(quinqueniosResultado)
}

#MÉTODO 1/16
metodo16<-function(datos1){
  PobMujeres<-vector(mode='numeric', length=18)
  PobHombres<-vector(mode='numeric', length=18)
  metodo<-data.frame(datos1, PobMujeres, PobHombres)
  for(i in 1:18){
    if(i>=3&i<17){
      metodo$PobMujeres[i]=round((-metodo$Mujeres[i-2]+(4*metodo$Mujeres[i-1])+(10*metodo$Mujeres[i])+
                                    (4*metodo$Mujeres[i+1])-metodo$Mujeres[i+2])/16, digits = 0)
      metodo$PobHombres[i]=round((-metodo$Hombres[i-2]+(4*metodo$Hombres[i-1])+(10*metodo$Hombres[i])+
                                    (4*metodo$Hombres[i+1])-metodo$Hombres[i+2])/16, digits = 0)
    } else {
      metodo$PobMujeres[i]=metodo$Mujeres[i]
      metodo$PobHombres[i]=metodo$Hombres[i]
    }
  }
  
  a<-metodo$Edad
  b<-metodo$PobHombres
  c<-metodo$PobMujeres
  resultado<-data.frame(Hombres=b, Mujeres=c,Edad=a)
  
  print('La nueva tabla corregida por el método de 1/16 es:')
  print(resultado)
}

#MITAD DEL AÑO
MitadAnio<-function(datos1,datos2){

  th2010<-sum(datos1$Hombres)
  tm2010<-sum(datos1$Mujeres)

  th2005<-sum(datos2$Hombres)
  tm2005<-sum(datos2$Mujeres)
  
  dt<-as.Date("2010-06-25")
  dt1<-as.Date("2005-10-17")
  
  t<-(as.numeric(difftime(dt,dt1)))/365
  
  rm<-((tm2010/tm2005)^(1/t))-1
  rh<-((th2010/th2005)^(1/t))-1
  
  HombreN<-vector(mode="numeric",length=18)
  MujerN<-vector(mode="numeric",length=18)
  
  datosM<-data.frame(datos1,HombreN,MujerN)
  dt3<-as.Date("2010-06-30")
  t1<-(as.numeric(difftime(dt3,dt1)))/365
  
  datosM$HombreN=round((datosM$Hombres)*((1+rh)^(t1)),digits=0)
  datosM$MujerN=round(((datosM$Mujeres)*((1+rm)^(t1))),digits=0)

  resultado<-data.frame(Hombres=datosM$HombreN, Mujeres=datosM$MujerN, Edad=datosM$Edad)
  print("Esta es la estimación de la población a la mitad del año")
  print(resultado) 
  cat("La tasa de crecimiento de hombres es:", rh)
  cat("\n La tasa de crecimiento de mujeres es:", rm)
}


#PIRÁMIDE POBLACIONAL
piramide<-function(datos1, tipo, anio){
  library(pyramid)
  nombre<-c("Población de San Luis Potosí por", tipo, anio)
  pyramid(datos1, Llab="Hombres", Rlab="Mujeres", Clab="Edad", AxisFM="d", Rcol="pink", Lcol="light blue", main=nombre)
}


##############PRUEBAS##################################

Whipple(SLP2010)
Whipple(SLP2005)

NE_2005
INU(quinquenios2005)
INU(quinquenios2010)

NE(SLP2005)
NE(SLP2010)
NE2010

Myers(SLP2005)
Myers(SLP2010)


quinquenios(SLP2005)
quinquenios(SLP2010)

metodo16(quinquenios2005)
metodo16(quinquenios2010)

MitadAnio(correc2010, correc2005)


piramide(NEResultado2010,"NE", 2010)
correc2010

