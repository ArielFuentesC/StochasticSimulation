#### PR�CTICA 2

#Hemos de crear una funci�n para calcular la aproximaci�n de pi

#Definimos la funci�n en funci�n del n�mero de puntos requeridos
Montecarlo <- function(n){
  #Definimos los arreglos de longitud n 
  # Requerimos que est�n dados entre -1 y 1 
  # Para que calcular la norma sea m�s sencillo
  xs <-runif(n, min=-1, max=1)
  ys <-runif(n, min=-1,max=1)
  
  #Comenzamos el conteo de puntos dentro del c�rculo
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
#Aplicamos sobre el arreglo anterior a la funci�n Montecarlo
YS <- lapply(M,FUN = Montecarlo)
#Graficamos la convergencia de la aproximaci�n
plot(M,YS,type="l",ylab="Valor aprox. de pi",main="M�todo Montecarlo")
abline(h=pi,col="blue")


# Hagamos un ejemplo independiente para obtener una representaci�n 
# gr�fica del problema.

# Creamos los arreglos para los n�meros aleatorios
xs <-runif(1e3, min=-1, max=1)
ys <-runif(1e3, min=-1,max=1)


# Definimos arreglos para graficar un c�rculo
cx <-seq(0,2*pi,0.01)
x <- cos(cx)
y <- sin(cx)

# Graficamos primero los puntos aleatorios
plot(xs,ys,col="green")
par(new=TRUE)
# Sobre esa misma ventana de graficaci�n, graficamos el c�rculo
plot(x,y,col="blue")
