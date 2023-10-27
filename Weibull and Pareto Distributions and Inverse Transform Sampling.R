#Definamos las funciones y sus respectivas inversas

#Para Weibull
fweibull <- function(x,lamb,k){
  return( (k/lamb)*(x/lamb)^(k-1)*exp(-(x/lamb)^k) )
}
Fweibull <- function(x,lamb,k){
    return( 1 -  exp(-(x/lamb)^k)  )
}


weibullinv <- function(U,lamb, k){
  return( lamb*(-log(1-U))^(1/k) )
}
#Para Pareto
fPareto = function(x,a,b){
  return((a*b^a)/(b+x)^(a+1))
}
FPareto = function(x,a,b){
  return(1-(b/(b+x))^a)
}
paretoinv= function(x,a,b){
  f_x  <- -b +(b/((1-x)^(1/a)))
  return(f_x)
}

RectWeibull <- function(lamb,k,n){
  xs <- runif(n,min=0,max=1)
  ys <- c(n)
  for (i in 1:n){
    ys[i] = weibullinv(xs[i],lamb,k)
  }
  hist = hist(ys,probability=TRUE)
  M=max(hist$breaks)
  m=min(hist$breaks)
  XS <- seq(m,M,0.01)
  print(XS)
  par(new=TRUE)
  lines(XS,fweibull(XS,lamb,k))
  return(hist)
}

RectPareto <- function(a,b,n){
  xs <- runif(n,min=0,max=1)
  ys <- c(n)
  for (i in 1:n){
    ys[i] = paretoinv(xs[i],a,b)
  }
  hist = hist(ys,probability=TRUE)
  M=max(hist$breaks)
  m=min(hist$breaks)
  XS <- seq(m,M,0.01)
  par(new=TRUE)
  lines(XS,fPareto(XS,a,b))
  return(hist)
}


RectPareto(10,3,10000)
RectWeibull(1,5,10000)
