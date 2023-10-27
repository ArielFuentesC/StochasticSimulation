### Distribución binomial negativa

Misterio <- function(mues,pob,A,dist){
  Iter <- replicate(dist,0)
  for (k in 1:dist){
    muestra <- replicate(mues,0)
    poblaciontotal <- sample(1:mues,A,replace=FALSE)
    for (i in poblaciontotal){
      muestra[i] = 1
    }
    Mues <- replicate(pob,0)
    w <- sample(1:mues,pob,replace=FALSE)
    for (i in 1:pob){
      Mues[i]=muestra[w[i]]
    }
    r <- sum(Mues == 1)
    Iter[k] = r
    #print(Iter[k])
  }
  Occur <- table(Iter)/dist
  print(Occur)
  return(Iter)
}
Resul <- Misterio(10,5,4,100000)

Misterio2 <- function(K,x,N,n){
  return( choose(K,x)*choose(N-K,n-x)/choose(N,n) )
}
Misterio2(4,2,10,5)
