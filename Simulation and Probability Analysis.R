## Práctica 8
library(Rlab)
library(Pareto)
library(Bernoulli)
#Ejercicio 1

Ejer1 <- function(n,p,err){ #Función que nos ayudará a las sumas parciales
  y = c()
  sum = 0  #Inicialización de las sumas
  for (i in 1:n){
    sum = sum + rbern(1,p) #Bernoulli
    y[i] = sum/i
    if (y[i]-p <= err){ #Comparamos y[i] para saber si el error es menor
      N = i #Regresamos i en caso de serlo
    }
  }
  plot(y,type="l",main=paste("Sumas parciales Bernoulli n = ", n, "p = ", p),xlab="Ensayos",ylab="Sum par")
  abline(h=p,col="blue") #Graficamos la constante en la media, p en este caso
  return(N) #Regresamos la iteración para la cual el error es menor al pedido
}
res = Ejer1(1000,0.5,1e-5)
res

Ejer2 <- function(n,a,b,err){ #Análogamente para Pareto
  y = c()
  sum = 0 #Iniciamos la suma
  for (i in 1:n){
    sum = sum + rPareto(1,a,b)
    y[i] = sum/i #Suma Parcial
    if (y[i]-(a*b)/(b-1) <= err){ #Rectificación del error
      N = i #Guardar la iteración para la cuál el error es menor
    }
  }
  plot(y,type="l",main=paste("Sumas parciales Pareto. n = ", n, "a = ", a, "b= ",b), xlab="Ensayos",ylab="Sum par")
  abline(h=(a*b)/(b-1),col="blue") #Dibujamos la media dada por a*b/b-1
  return(N)
}
Ejer2(1000,1.98,6,1e-3)


Ejer3 <- function(n,k){ #Sea n el número de veces que se repite el experimento
  xs <- c() #Y k 7 =< k =< 42 el número del que se desea saber la probabilidad
  for (i in 1:n){
    xs[i] <- sum(sample(1:7,7,replace=T)) #Simulamos la tirada y sumamos
  } 
  #Contabilizamos cuántas veces apareció k y dividimos entre el número de 
  #experimentos para obtener la probabilidad
  res = table(xs)/n 
  print(res[k]) #Mostramos la probabilidad
}
Ejer3(1000,"32")
