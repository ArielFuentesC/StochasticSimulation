###Práctica 11.

library(MASS) #Importamos librerías útiles para graficar
library(ggplot2) 
library(viridis)
library(scatterplot3d)
library(plot3D)


get_density <- function(x, y, ...) { #Función que determina la densidad
  dens <- MASS::kde2d(x, y, ...) #De los puntos; no importante
  ix <- findInterval(x, dens$x) #Para la práctica
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}
### Ejercicio 1.

Ejercicio1 <- function(n){
  set.seed(1) #Fijamos una semilla
  Y <- rexp(n,1) #Generamos las variables exponenciales para y
  X <- rexp(n,1/Y) #A partir de ellas generamos a x
  dat <- data.frame(x = X,y = Y) 
  dat$density <- get_density(dat$x, dat$y, n = 100) #Sacamos las densidades
  gg0 <- ggplot(dat) + geom_point(aes(X, Y, color = density)) + #Graficamos con densidad
    scale_color_viridis() + ggtitle(paste("X vs Y con n=",n)) +
    labs(y="y", x = "x")
  show(gg0)
}

### Llamadas a las funciones de ejemplo

Ejercicio1(205)
Ejercicio1(1000)

### Ejercicio 2.

Ejercicio2 <- function(n,M,m){
  set.seed(1) #Fijamos una semilla
  vec <- c()
  l <- ncol(M) #Obtenemos el número de columnas de M
  for (i in 1:n){
    x <- rnorm(l) #Generamos variables normales a partir de las columnas
    y = M%*%x+m #Definimos a "y" como una combinación lineal
    vec <- cbind(vec, y)
  }
  vec <- t(vec) #Transponemos
  return (vec)
}

#Llamadas a la función de ejemplo
a <- matrix(data = c(1,0,0,1),nrow = 2,ncol = 2) #Definimos "M"
a2 <- c(0,0) #Vector "m"
a3 <- Ejercicio2(1000,a,a2) #Llamamos a la función con n=1000
dat <- data.frame(a3) #Se convierte a dataframe para graficarlo

x1_cut <- cut(t(a3[,1]),breaks = 30) #Damos formato para graficarlo
x2_cut <- cut(t(a3[,2]),breaks = 30)
z <- table(x1_cut,x2_cut)
hist3D(z=z, border="purple",main="Histograma en 3D con mu =(0,0) y E=[1 0;0 1]") #Generamos el histograma


h1 <- hist(dat$X1, breaks=30, plot=F) #Graficaremos con el estilo que se nos dio en la práctica
h2 <- hist(dat$X2, breaks=30, plot=F) #Con los datos generados
top <- max(dat$counts, h2$counts)
k <- kde2d(dat$X1, dat$X2, n=30)

oldpar <- par()
par(mar=c(3,3,1,1))
layout(matrix(c(2,0,1,3),2,2,byrow=T),c(3,1), c(1,3))
image(k) #plot the image
par(mar=c(0,2,1,0))
barplot(h1$counts, axes=F, ylim=c(0, top), space=0, col='red')
par(mar=c(2,0,0.5,1))
barplot(h2$counts, axes=F, xlim=c(0, top), space=0, col='red', horiz=T)

#Segundo ejemplo
#Definimos a la matriz M calculada
a <- matrix(data = c(sqrt(0.3)/sqrt(2),sqrt(1.7)/sqrt(2),-sqrt(0.3)/sqrt(2),sqrt(1.7)/sqrt(2)),nrow = 2,ncol = 2)
a2 <- c(1,0)
a3 <- Ejercicio2(1000,a,a2)
dat <- data.frame(a3)

x1_cut <- cut(t(a3[,1]),breaks = 30)
x2_cut <- cut(t(a3[,2]),breaks = 30)
z <- table(x1_cut,x2_cut)
hist3D(z=z, border="purple",main=" E=[1 0.7;0.7 1]") #Histograma en 3D


h1 <- hist(dat$X1, breaks=30, plot=F)
h2 <- hist(dat$X2, breaks=30, plot=F)
top <- max(dat$counts, h2$counts)
k <- kde2d(dat$X1, dat$X2, n=30)

oldpar <- par()
par(mar=c(3,3,1,1))
layout(matrix(c(2,0,1,3),2,2,byrow=T),c(3,1), c(1,3))
image(k) #plot the image
par(mar=c(0,2,1,0))
barplot(h1$counts, axes=F, ylim=c(0, top), space=0, col='red')
par(mar=c(2,0,0.5,1))
barplot(h2$counts, axes=F, xlim=c(0, top), space=0, col='red', horiz=T)
