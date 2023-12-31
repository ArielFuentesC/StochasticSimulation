### Pr�ctica 7
#Ejercicio 1

#Definimos la funci�n de probabilidad binomial
f1 <- function(r,n,l){
  p = l/n
  return(choose(n,r)*p^r * (1-p)^(n-r))
}

#Y la distribuci�n de Poisson
f2 <- function(r,n,l){
  p =l/n
  return( ( exp(-n*p)*(n*p)^r )/( factorial(r) ) )
}

### Definimos una funci�n que haga m�s sencillo en proceso de emplear
#diferentes lambdas (l) y n's

#La funci�n toma una n, una lambda, l, y un rango para graficar
graf <- function(n,l,rang){
  xs0 <- seq(0,rang,1)
  #Evaluamos a xs0 en la binomial (f1) y en Poisson (f2)
  ys1 <- sapply(xs0,f1,n=n,l=l)
  ys2 <- sapply(xs0,f2,n=n,l=l)
  #Graficamos en la misma ventana gr�fica 
  plot(xs0,ys1,type="l",col="red",main=paste("Disbinom (azul) vs Dispois (rojo). n= ", n, "l= ", l),xlab="n",ylab="y")
  lines(xs0,ys2,type="l",col="blue")
  #Tomamos el resultado de las aplicaciones en f1 y f2 para definir el 
  #error requerido, mismo que graficamos 
  ys3 <- abs(ys2-ys1)
  error <- max(ys3)
  plot(ys3)
  return(error)
}

#Llamada de ejemplo para la funci�n del error m�ximo
graf(10000, 10, 100)

#Nos ayudamos de unas l�neas de c�digo para graficar el error m�ximo
ks <- seq(10,100000,100)
KS <- sapply(ks,graf,l=5,rang=100)
plot(ks,KS,type="l",main="Error m�ximo, Dist. Bin vs Dist. Poisson",xlab="n",ylab="Error m�ximo",col="blue")


#Ejercicio 2

#Redefinimos la funci�n de la binomial porque ya nos dan p
f3 <- function(r,n,p){
  return(choose(n,r)*p^r * (1-p)^(n-r))
}

#Una vez m�s, definimos una funci�n que haga m�s sencillo el
#variar los par�metros
ejer2 <- function(n,rang,p){
  xs0 <- seq(0,rang,1)
  #Como en el primer ejercicio, evaluamos sobre la binomial
  #y sobre la normal
  ys1 <- sapply(xs0,f3,n=n,p=p)
  ys2 <- dnorm(xs0, n*p, sqrt(n*p*(1-p)))
  #Graficamos
  #plot(xs0,ys1,type="l",col="red",main=paste("Funci�n de probabilidad (azul) vs  densidad (rojo). n= ", n, "p = ", p),xlab="n",ylab="y")
  #lines(xs0,ys2,type="l",col="blue")
  #Proponemos el error cuadr�tico medio como m�todo alternativo
  #Para medir la velocidad de convergencia
  ys3 <- sum((ys2-ys1)^2)/rang
  return(ys3)
}
#Ejemplo de la llamada a la funci�n
ejer2(100,100,0.85)

#An�logamente, para visualizar el error se usan las siguientes l�neas de c�digo
ks <- seq(10,100000,100)
KS <- sapply(ks,ejer2,rang=100,p=0.1)
plot(ks,KS,type="l",main="Error cuadr�tico medio",xlab="n",ylab="Error cuadr�tico",col="blue")

