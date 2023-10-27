### Práctica 10

library(MASS) #Agregamos algunas librerías que nos ayuden a graficar con el estilo de un heatmap
library(ggplot2)
library(viridis)
library(scatterplot3d)

#Definimos una función que determina la densidad de los puntos.
#No es relevante para la práctica
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

#Ejercicio 1
Ejer1 <- function(n){
  set.seed(1234)
  xs <- rnorm(n) #Creamos n normales
  ys <- rnorm(n)
  #Necesitamos un dataframe para la función que grafica de acuerdo a la densidad
  dat <- data.frame(x = xs,y = ys)
  dat$density <- get_density(dat$x, dat$y, n = 100) #Sacamos las densidades
  gg0 <- ggplot(dat) + geom_point(aes(xs, ys, color = density)) + #Graficamos con densidad
    scale_color_viridis() + ggtitle("X vs Y") +
    labs(y="y", x = "x")
  
  dat1 <- data.frame(x = xs+ys,y = xs-ys) #Repetimos el proceso para el segundo inciso
  dat1$density <- get_density(dat1$x, dat1$y, n = 100)
  gg1 <- ggplot(dat1) + geom_point(aes(xs+ys, xs-ys, color = density)) + 
    scale_color_viridis() + ggtitle("X+Y vs X-Y")+
    labs(y="x-y", x = "x+y")
  show(gg0)
  show(gg1)
}
Ejer1(1000) #Llamada muestra a la función 

#Ejercicio 2

#Definimos una función con los parámetros a emplear
Multinomial2 <- function(m,n,p1,p2,p3){
  set.seed(1)
  vec <- c()
  for (k in 1:m){ #Se repetirá m veces el experimento
    a <- runif(n) #Creamos las variables uniformes
    probas <- c(p1,p2,p3) #Vector de probabilidades
    multi <- c(0,0,0) #Llevaremos el conteo en este vector
    for (i in 1:n){ #El proceso se repetirá n veces
      start <- 0 
      azar <- a[i] #Tomamos una uniforme
      for(j in 1:3){
        end <- start + probas[j] #Vamos "caminando" sobre el intervalo
        if(azar<=end && azar>start){ #Revisamos en qué subintervalo está a[i]
          multi[j] <- multi[j]+1 #Aumentamos el contador cuando sabemos en qué intervalo está
        }
        start <- end 
      }
    }
    vec <- cbind(vec,multi) #Damos un formato adecuado
  }
  return(vec)
}

#Creamos una función para graficar
Ejercicio2 <- function(n){
  m <- t(Multinomial2(400,n,0.16,0.16,0.66)) #Pasamos los parámetros a la función creada anteriormente
  plotting <-
    scatterplot3d(m[,1:3], angle = 135, pch = 16, color = "steelblue", #Graficamos 
                  #type = 'h',
                  main = 'Multinomial',
                  sub = paste("n=",n),
                  xlab = "X1",
                  ylab = "X2",
                  zlab = "X3")
  show(plotting)
}

Ejercicio2(1000) #Llamada muestra de la función


#Ejercicio 3
#La siguiente es una función idéntica a la del ejercicio anterior
#pero modificada para graficar estéticamente
Ejercicio3 <- function(m,n,p1,p2,p3){
  set.seed(1)
  vec <- c()
  for (k in 1:m){
    a <- runif(n)
    probas <- c(p1,p2,p3)
    multi <- c(0,0)
    for (i in 1:n){
      start <- 0
      azar <- a[i]
      for(j in 1:2){
        end <- start + probas[j]
        if(azar<=end && azar>start){
          multi[j] <- multi[j]+1
        }
        start <- end
      }
    }
    vec <- cbind(vec,multi)
    vec1 <- data.frame(t(vec)) #Pasamos a un dataframe para graficar
  }
  vec1$density <- get_density(vec1$X1, vec1$X2, n = 100) #Sacamos densidades y graficamos
  gg2 <- ggplot(vec1) + geom_point(aes(X1, X2, color = density)) + 
    scale_color_viridis() + ggtitle(paste("Multinomial con n=",n,"m=",m,"p1=",p1,"p2=",p2,"p3=",p3))+
    labs(y="y", x = "x")
  show(gg2)
}
#Llamadas muestra a la función
Ejercicio3(1000,50,0.16,0.16,0.66)
Ejercicio3(1000,500,0.16,0.16,0.66)
Ejercicio3(1000,1000,0.16,0.16,0.66)