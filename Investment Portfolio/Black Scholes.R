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
BlackScholes_t <- function(s,k,r,sigma,t=0,V,tipo="c",q=0){
  
  if(tipo=="c") j <- 1
  if(tipo=="p") j <- -1
  cat("s=",s, "r", r, "q=",q, "v=",V, "sig=", sigma, "t=", t)
  d1 <- (log(s/k)+(r+-q+(sigma^2)/2)*(V-t))/(sigma*sqrt(V-t))
  d2 <- (log(s/k)+(r-q-(sigma^2)/2)*(V-t))/(sigma*sqrt(V-t))
  cat("D1=", d1, "D2:", d2)
  f_t <- j*s*exp(-q*(V-t))*pnorm(j*d1) - j*k*exp(-r*(V-t))*pnorm(j*d2)
  cat("N(d1)=",pnorm(j*d1),"N(d2)=",pnorm(j*d2))
  f_t
} 
X=BlackScholes_t(20.209999,18.510947775,0.05145328159,0.15,0,0.0505555,"c",0.02955880224)
X
Y=BlackScholes_t(20.209999,21.90905025,0.05145328159,0.15,0,0.0505555,"c",0.02955880224)
Y