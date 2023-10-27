### Práctica 6

#Definimos la función de densidad
funden <- function(x,a,b){
  y <- (x^(a-1) * (1-x)^(b-1))/(beta(a,b))
  return(y)
}

#Función del método para generar n variables
aceprech <- function(a,b,n){
  #Creamos un arreglo donde guardar tanto el iterador como el valor de x
  output <- matrix(0,n,2)
  for (i in 1:n){
    cont = 0
    #Generamos el primer x y lo evaluamos
    x <- runif(1,0,1)
    f <- funden(x,a,b)
    #Encontramos el supremo aplicando a una secuencia la función de densidad
    #Y tomando su máximo
    xs <- seq(0,1,0.01)
    ys <- sapply(xs, funden,a=a,b=b)
    sup <- max(ys)
    #Comparamos
    y <- runif(1,0,sup)
    while (y > f){
      #Generamos nuevos valores en caso de ser necesario
      x <- runif(1,0,1)
      f <- funden(x,a,b)
      cont = cont + 1
    }
    #Almacenamos valores
    output[i,1] <- x
    output[i,2] <- cont
  }
  return(output)
}

#Código muestra para la generación de Beta(2,5) con n=1000
#Tanto el histograma como la función de densidad se muestran
ys0 <- aceprech(2,5,1000)
h <- hist(ys0[,2],probability = TRUE, main="Histograma Beta(2,5) n=1000",xlab="x")
xs <- seq(0,1,0.01)
lines(xs,funden(xs,2,5))

#Definimos una función para sacar el número de iteraciones necesarias para
#generar n variables
iterprom <- function(a,b,n){
  ys <- aceprech(a,b,n)
  r <- sum(ys[,2])
  return(r)
}

#Definimos otra función para graficar las iteraciones necesarias
graf <- function(a,b,n){
  #Definimos una secuencia de n, (número de variables a generar)
  ns <- seq(1,n,n/100)
  #Las evaluamos en la función de las iteraciones
  yns <- sapply(ns, iterprom,a=2,b=2)
  #Guardamos los resultados
  res <- c(list(ns), list(yns))
  return(res)
  
}
#Código muestra para mostrar las iteraciones necesarias con Beta(2,5)
#Se grafica una compración entre las dos Betas
beta25 <- graf(2,5,10000)
b25 <- plot(unlist(beta25[1]),unlist(beta25[2]),type = "l", xlab="Número de variables generadas", ylab="Iteraciones",main = "Beta(2,2) (verde) vs Beta (2,5) (rojo)",col="red")

#Código muestra para mostrar las iteraciones necesarias con Beta(2,5)
beta22 <- graf(2,2,10000)
par(new=TRUE)
#Graficamos en la misma ventana
b22 <- lines(unlist(beta22[1]),unlist(beta22[2]),type = "l", col="green")
