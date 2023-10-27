### Distribución hipergeométrica

Misterio <- function(Nt,m,A,n){
  Iter <- replicate(n,0)
  for (k in 1:n){
    Pob <- replicate(Nt,0)
    PobA <- sample(1:Nt,A,replace=FALSE)
    for (i in PobA){
      Pob[i] = 1
    }
    #print(Pob)
    Mues <- replicate(m,0)
    w <- sample(1:Nt,m,replace=FALSE)
    for (i in 1:m){
      Mues[i]=Pob[w[i]]
    }
    #print(Mues)
    r <- sum(Mues == 1)
    Iter[k] = r
    #print(Iter[k])
  }
  Occur <- table(Iter)/n
  print(Occur)
  return(Iter)
}
Resul <- Misterio(10,5,4,100000)

Misterio2 <- function(K,x,N,n){
  return( choose(K,x)*choose(N-K,n-x)/choose(N,n) )
}
Misterio2(4,2,10,5)
