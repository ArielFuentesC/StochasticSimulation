library(ggplot2) #Librería para graficar

#Función del ejercicio 1
Ejercicio1y2 <- function(n,M,Bool){
  set.seed(1)
  
  #Creación de variables auxiliares
  N <- 2*M
  tiempos <- c()
  
  X <- c(0)
  Y <- c(0)
  Iteracion <- c(0)
  
  g1 <- data.frame(X,Y,Iteracion)
  g2 <- g1[F,]
  
  
  for (i in 1:n){
    dinero <- M 
    Y <- c(dinero)
    
    #Las condiciones del while nos ayudan a 
    #asegurarnos para parar en 2M o en 0.
    while((dinero<N && dinero>0)){
      #Parte propiamente de la simulación
      dinero <- dinero + sample(c(1,-1),1)
      Y <- c(Y,dinero)
    }
    
    #Tiempos,es decir, X_n
    tiempos <- c(tiempos,length(Y))
    
    #Variables que usamos para graficar
    X <- c(1:length(Y))
    Iteracion <- rep(as.character(i) ,times=length(Y))
    g3 <- data.frame(X,Y,Iteracion)
    g2 <- rbind(g2,g3)
  }
  
  #Sección para graficar
  if (Bool){
    gg1 <- ggplot(data=g2,
                  mapping = aes(x=X,
                                y=Y,
                                color = Iteracion))+
      geom_line()
    show(gg1)}
  
  return(tiempos)
}

#Pregunta 2; simulación con los parámetros previstos
a <- Ejercicio1y2(5,50,T)

#Pregunta 3; comenzamos simulando el juego con los valores dados

#M=20
a1 <- Ejercicio1y2(50,20,F)

#M=40
a2 <- Ejercicio1y2(50,40,F)

#M=80
a3 <- Ejercicio1y2(50,80,F)

#M=150
a4 <- Ejercicio1y2(50,150,F)

#Sacamos los promedios
#M=20 k=30,40,50
b1 = mean(a1[1:30])
b2 = mean(a1[1:40])
b3 = mean(a1[1:50])

#M=40 k=30,40,50
b4 = mean(a2[1:30])
b5 = mean(a2[1:40])
b6 = mean(a2[1:50])

#M=80 k=30,40,50
b7 = mean(a3[1:30])
b8 = mean(a3[1:40])
b9 = mean(a3[1:50]) 

#M=150 k=30,40,50
b10 = mean(a4[1:30])
b11 = mean(a4[1:40])
b12 = mean(a4[1:50])

print(b1)
print(b2)
print(b3)
print(b4)
print(b5)
print(b6)
print(b7)
print(b8)
print(b9)
print(b10)
print(b11)
print(b12)


#Ejercicio 4 Se empleará una función modificada del primer ejercicio
#pero modificada para parar sólo en la ruina
Ejercicio4 <- function(n,M){
  set.seed(1)  
  N <- 2*M
  tiempos <- c()
  count <- 0
  for (i in 1:n){
    dinero <- M
    Y <- c(dinero)
    
    #Misma idea de el ejercicio anterior pero hay que tener
    #De solo contar los tiempos cuando se llega a la ruina
    while(dinero>0 && dinero<N && count < n){
      
      #Sumamos 1 o -1 con ayuda sample
      dinero <- dinero + sample(c(1,-1),1)
      Y <- c(Y,dinero)
      
      #Con esta parte nos aseguramos de hacer n ruinas
      if (dinero == 0){
        count <- count + 1
      }
    }    
    tiempos <- c(tiempos,length(Y))
  } 
  return(tiempos)
}

#Análogamente, comenzamos simulando los resultados
#M=20
a1 <- Ejercicio4(20,20)

#M=40
a2 <- Ejercicio4(20,40)

#M=80
a3 <- Ejercicio4(20,80)

#M=150
a4 <- Ejercicio4(20,150)

#Y luego calculamos los promedios
#M=20 k=10,15,20
b1 = mean(a1[1:10])
b2 = mean(a1[1:15])
b3 = mean(a1[1:20])

#M=40 k=10,15,20
b4 = mean(a2[1:10])
b5 = mean(a2[1:15])
b6 = mean(a2[1:20])

#M=80 k=10,15,20
b7 = mean(a3[1:10])
b8 = mean(a3[1:15])
b9 = mean(a3[1:20]) 

#M=150 k=10,15,20
b10 = mean(a4[1:10])
b11 = mean(a4[1:15])
b12 = mean(a4[1:20])


print(b1)
print(b2)
print(b3)
print(b4)
print(b5)
print(b6)
print(b7)
print(b8)
print(b9)
print(b10)
print(b11)
print(b12)

