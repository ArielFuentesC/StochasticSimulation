#######################################
#Ejercicio 1 Matriz de Hilbert
######################################

#Comenzamos definiendo la dimensión de nuestra matriz
n=14
M <- matrix(nrow=n,ncol=n)

#Definimos la función para la matriz de Hilbert
Matriz_Hilbert <- function(M,n){
  for (i in 1:n){
      for(j in 1:n){
        M[i,j] = 1/(i+j-1)
      }
  }
  return(M)
}

H <-Matriz_Hilbert(M,n)
#La mostramos
H
#Definimos la inversa de la matriz de Hilbert
Inversa_Hilbert <- function(n){
  M <- matrix(nrow=n,ncol=n)
  for (i in 1:n){
    for (j in 1:n){
      a <- choose(n+i-1,n-j)
      b <- choose(n+j-1,n-i)
      c <- choose(i+j-2,i-1)
      M[i,j] = ((-1)^(i+j))*(i+j-1)* a*b*(c^2 )
    }
  }
  return(M)
}

HH <-Inversa_Hilbert(n)
#Mostramos la inversa asociada a la matriz antes creada
HH

H %*% HH



################################################
#Ejercicio 2. Regla de Simpson.
################################################
#Comenzamos definiendo la función
Simpson <- function(f,a,b,n){
  #Definimos el tamaño de paso
  h <- (b-a)/n
    
    #Individualmente inicializamos y calculamos las sumas impares y pares de acuerdo al método
    sum_imp=0
    for (i in seq(1,n-1,2) ) {
      sum_imp = sum_imp + f(a + i*h)  
    }
    #Reescalamos de acuerdo al método
    sum_imp = 4*sum_imp
    
    #Análogamente para la suma sobre los pares
    sum_par=0
    for (j in seq(0,n-2,2) ) {
      sum_par = sum_par + f(a+j*h)
    }
    #Reescalando el resultado
    sum_par= 2*sum_par
    
    #Finalmente calculamos el valor numérico de la integral
    integral <- 0
    integral = (h/3)* (f(a) + sum_par + sum_imp + f(b) )
    return(integral)
    
}

# Definiendo la funció sobre las cuál probaremos el método
Cuadrado <- function(x){
  return(x**2)
  
}
#Haciendo las respectivas pruebas;
#Comenzaremos probando el algoritmo con la función "Cuadrado" en el intervalo 0 a 1
#por facilidad

Simpson(Cuadrado,0,1,100)

#En efecto, el método arroja el valor deseado; 0.33
#Ahora, para la función seno. Esta será evaluada de 0 a pi/2; un valor conocido

Simpson(sin,0,pi/2,100)

#Nuevamente la consola arroja lo deseado; un 1

#Definiendo pruebas más complicadas;

Exponencial <- function(x){
  return(exp(-x**(2)))
}

Raiz <- function(x){
  return( (1+x**3)**(1/2) )
  
}

#Poniéndola a prueba
#Con la función Exponencial, integrando de 0 a 2;
Simpson(Exponencial,0,2,1000)
#Cuyo resultado numérico es 0.8834, mientras que el valor "numérico real" es 0.8820

#Haciendo pruebas con la función "Raíz";
Simpson(Raiz,-1,1,1000)
#Que nos arroja un valor de 1.9527, mientras que, comparando con Wolfram, su valor es de 1.9527
#En efecto, el método muestra una relativa buena precisión para el número de intervalos dado