#### PRÁCTICA 2

#Hemos de crear una función para calcular la aproximación de pi

#Definimos la función en función del número de puntos requeridos
Montecarlo <- function(n){
  #Definimos los arreglos de longitud n 
  # Requerimos que estén dados entre -1 y 1 
  # Para que calcular la norma sea más sencillo
  xs <-runif(n, min=-1, max=1)
  ys <-runif(n, min=-1,max=1)
  
  #Comenzamos el conteo de puntos dentro del círculo
  Circ <- 0
  for (i in 1:n) {
    if ((xs[i]**2+ys[i]**2)**(1/2)<=1){
      Circ = Circ + 1
    }
  }
  
  #Aproximamos pi
  pi <- 4*(Circ/n)
  return(pi)
}

#Creamos un arreglo con valores para n
M <- seq(10,1e6,1e3)
#Aplicamos sobre el arreglo anterior a la función Montecarlo
YS <- lapply(M,FUN = Montecarlo)
#Graficamos la convergencia de la aproximación
plot(M,YS,type="l",ylab="Valor aprox. de pi",main="Método Montecarlo")
abline(h=pi,col="blue")


# Hagamos un ejemplo independiente para obtener una representación 
# gráfica del problema.

# Creamos los arreglos para los números aleatorios
xs <-runif(1e3, min=-1, max=1)
ys <-runif(1e3, min=-1,max=1)


# Definimos arreglos para graficar un círculo
cx <-seq(0,2*pi,0.01)
x <- cos(cx)
y <- sin(cx)

# Graficamos primero los puntos aleatorios
plot(xs,ys,col="green")
par(new=TRUE)
# Sobre esa misma ventana de graficación, graficamos el círculo
plot(x,y,col="blue")
