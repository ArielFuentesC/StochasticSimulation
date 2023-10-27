# PRÁCTICA 3
# Definimos la función del Generador lineal congruencial

glc <- function(a,c,m,semilla,n) {
  # Creamos el vector de longitud n
  vector1 <- vector(length = n)
  # Fijamos nuestra primera entrada del vector con el valor de la semilla
  vector1[1] <- semilla
  for (i in 2:n) {
    #Aplicando la fórmula
    vector1[i] <- (a * vector1[i-1] + c) %% m
  }
  return(vector1)
}


#Para el caso c=0 escogemos los siguientes parámetros y emplearemos n=500000;
base <-glc(a =5**5,c=0,m=3**18,semilla =23, n= 500000)
#Normalizando para obtener valores de 0 a 1, como lo piden
m = 3**18
base_normalizada <-base / m

#Mostramos el histograma
hist(base_normalizada, probability = TRUE)


#Graficando en R^3
#Usaremos plot3D como entorno

library(plot3D)
#Aplicamos nuestro método para cada coordenada con una a, m diferentes
#Para escoger la semilla, escogeremos la función runif()

x <- glc(a =5**5,c=0,m=4**30,semilla=runif(1), n= 1000) / 4**30
y <-glc(a =3**5,c=0,m=837429537,semilla=runif(1), n= 1000) / 837429537
z <-glc(a =7**5,c=0,m=2**32-1,semilla=runif(1), n= 1000) / 2**32-1
#Graficamos
scatter3D(x,y,z,main="Generador lineal congruencial")

#Repitiremos el histograma con a = 7**5 y m = 2**31 ??? 1

m = 2**31-1
base <-glc(a =7**5,c=0,m=2**31-1,semilla =runif(1), n= 500000)
#Normalizando
base_normalizada <-base / m
#Mostramos el histograma
hist(base_normalizada, probability = TRUE)

#graficando en 3D
x <- glc(a = 7**5,c=0,m = 2**31 -1,semilla=runif(1), n= 100000) /  2**31 -1
y <-glc(a = 7**5,c=0,m = 2**31 -1,semilla=runif(1), n= 100000) /  2**31 -1
z <-glc(a =7**5,c=0,m = 2**31 -1,semilla=runif(1), n= 100000) /  2**31 -1
scatter3D(x,y,z,main="Generador lineal congruencial con parámetros fijos")

#Lo haremos ahora también con la misma semilla
semilla0 <- runif(1)
x <- glc(a = 7**5,c=0,m = 2**31 -1,semilla0, n= 100000) /  2**31 -1
y <-glc(a = 7**5,c=0,m = 2**31 -1,semilla0, n= 100000) /  2**31 -1
z <-glc(a =7**5,c=0,m = 2**31 -1,semilla0, n= 100000) /  2**31 -1
scatter3D(x,y,z,main="Generador lineal congruencial con parámetros fijos")


#Para calcular pi con este método, crearemos una función;

#Definimos la función
glc_a <- function(a,c,m,n) {
  # Creamos el vector de longitud n
  vector1 <- vector(length = n)
  vector1[1] <- runif(1)
  for (i in 2:n) {
    #Aplicando la fórmula
    vector1[i] <- (a * vector1[i-1] + c) %% m
  }
  vector1 = vector1/m
  return(vector1)
}
#Emplearemos los mismos parámetros,
#a = 7**5 y m = 2**31 ??? 1, para estimar pi

#Definimos nuestra función
Montecarlo <- function(n){
  #Tomando los tiempos de ejecución
  t0 <- Sys.time()
  xs <- glc_a(7**5,0,2**31 -1,n)
  ys <- glc_a(7**5,0,2**31 -1,n)
  
  #Comenzamos el conteo de puntos dentro del círculo
  Circ <- 0
  for (i in 1:length(xs)) {
    if (((xs[i]-0.5)**2+(ys[i]-0.5)**2)**(1/2)<=0.5){
      Circ = Circ + 1
    }
  }
  
  #Aproximamos pi
  pi <- 4*(Circ/n)
  t1 <-Sys.time()
  te <- t1-t0
  #Regresamos la aproximación y el tiempo de ejecución
  vec <- c(pi,te)
  return(vec)
}
#Haciendo una prueba para 500 000
P <-Montecarlo(500000)
#Haciendo un análisis más profundo;
#Creamos un arreglo con valores para n
M <- seq(10,1e6,1e3)
#Aplicamos sobre el arreglo anterior a la función Montecarlo
YS <- sapply(M,FUN = Montecarlo)
YSpi = c(M)
YSt = c(M)
for (i in 1:length(M)){
  YSpi[i]=YS[2*i -1]
  YSt[i] =YS[2*i]
}

#Graficamos la convergencia de la aproximación
plot(M,YSpi,type="l",ylab="Valor aprox. de pi",main="Método Montecarlo")
abline(h=pi,col="blue")

plot(M,YSt,type="l",ylab="Valor aprox. de pi",main="Método Montecarlo")
