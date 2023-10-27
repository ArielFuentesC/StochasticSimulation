### Práctica 7
#Ejercicio 1

#Definimos la función de probabilidad binomial
f1 <- function(r,n,l){
  p = l/n
  return(choose(n,r)*p^r * (1-p)^(n-r))
}

#Y la distribución de Poisson
f2 <- function(r,n,l){
  p =l/n
  return( ( exp(-n*p)*(n*p)^r )/( factorial(r) ) )
}

### Definimos una función que haga más sencillo en proceso de emplear
#diferentes lambdas (l) y n's

#La función toma una n, una lambda, l, y un rango para graficar
graf <- function(n,l,rang){
  xs0 <- seq(0,rang,1)
  #Evaluamos a xs0 en la binomial (f1) y en Poisson (f2)
  ys1 <- sapply(xs0,f1,n=n,l=l)
  ys2 <- sapply(xs0,f2,n=n,l=l)
  #Graficamos en la misma ventana gráfica 
  plot(xs0,ys1,type="l",col="red",main=paste("Disbinom (azul) vs Dispois (rojo). n= ", n, "l= ", l),xlab="n",ylab="y")
  lines(xs0,ys2,type="l",col="blue")
  #Tomamos el resultado de las aplicaciones en f1 y f2 para definir el 
  #error requerido, mismo que graficamos 
  ys3 <- abs(ys2-ys1)
  error <- max(ys3)
  plot(ys3)
  return(error)
}

#Llamada de ejemplo para la función del error máximo
graf(10000, 10, 100)

#Nos ayudamos de unas líneas de código para graficar el error máximo
ks <- seq(10,100000,100)
KS <- sapply(ks,graf,l=5,rang=100)
plot(ks,KS,type="l",main="Error máximo, Dist. Bin vs Dist. Poisson",xlab="n",ylab="Error máximo",col="blue")


#Ejercicio 2

#Redefinimos la función de la binomial porque ya nos dan p
f3 <- function(r,n,p){
  return(choose(n,r)*p^r * (1-p)^(n-r))
}

#Una vez más, definimos una función que haga más sencillo el
#variar los parámetros
ejer2 <- function(n,rang,p){
  xs0 <- seq(0,rang,1)
  #Como en el primer ejercicio, evaluamos sobre la binomial
  #y sobre la normal
  ys1 <- sapply(xs0,f3,n=n,p=p)
  ys2 <- dnorm(xs0, n*p, sqrt(n*p*(1-p)))
  #Graficamos
  #plot(xs0,ys1,type="l",col="red",main=paste("Función de probabilidad (azul) vs  densidad (rojo). n= ", n, "p = ", p),xlab="n",ylab="y")
  #lines(xs0,ys2,type="l",col="blue")
  #Proponemos el error cuadrático medio como método alternativo
  #Para medir la velocidad de convergencia
  ys3 <- sum((ys2-ys1)^2)/rang
  return(ys3)
}
#Ejemplo de la llamada a la función
ejer2(100,100,0.85)

#Análogamente, para visualizar el error se usan las siguientes líneas de código
ks <- seq(10,100000,100)
KS <- sapply(ks,ejer2,rang=100,p=0.1)
plot(ks,KS,type="l",main="Error cuadrático medio",xlab="n",ylab="Error cuadrático",col="blue")

