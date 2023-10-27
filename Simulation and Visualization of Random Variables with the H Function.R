H <- function(W,V,Th){
  if(W<(1-Th)*V**(1-Th)){
    return((W*V**Th)/(1-Th))
  }
  else if( (1-Th)*V**(1-Th) <= W  && W < V**(1-Th) ){
    return(V)
  }
  else if( W >= V**(1-Th) ){
    return(W**((1)/(1-Th)))
  }
}

vector_aleat <- function(n,Th){
  set.seed(1)
  W <- runif(n)
  V <- runif(n)
  U <- H(W,V,Th)
  vector <- cbind(U,V)
  return(vector)
}


dat1 <- data.frame(vector_aleat(1000,0))
dat2 <- data.frame(vector_aleat(1000,0.1))
dat3 <- data.frame(vector_aleat(1000,0.3))
dat4 <- data.frame(vector_aleat(1000,0.85))
dat5 <- data.frame(vector_aleat(1000,0.9))

h1 <- hist(dat1$U, breaks=30, plot=T)
h2 <- hist(dat1$V, breaks=30, plot=T)

h1 <- hist(dat2$U, breaks=30, plot=T)
h2 <- hist(dat2$V, breaks=30, plot=T)

h1 <- hist(dat3$U, breaks=30, plot=T)
h2 <- hist(dat3$V, breaks=30, plot=T)

h1 <- hist(dat4$U, breaks=30, plot=T)
h2 <- hist(dat4$V, breaks=30, plot=T)

h1 <- hist(dat5$U, breaks=30, plot=T)
h2 <- hist(dat5$V, breaks=30, plot=T)


vector_2 <- function(vector){
  X <- -log(vector$U)
  Y <- -log(vector$V)
  vector_ret <- cbind(X,Y)
  return(vector_ret)
}

dat1 <- data.frame(vector_2(dat1))
dat2 <- data.frame(vector_2(dat2))
dat3 <- data.frame(vector_2(dat3))
dat4 <- data.frame(vector_2(dat4))
dat5 <- data.frame(vector_2(dat5))

h1 <- hist(dat1$X, breaks=30, plot=T)
h2 <- hist(dat1$Y, breaks=30, plot=T)

h1 <- hist(dat1$X, breaks=30, plot=T)
h2 <- hist(dat1$Y, breaks=30, plot=T)

h1 <- hist(dat1$X, breaks=30, plot=T)
h2 <- hist(dat1$Y, breaks=30, plot=T)

h1 <- hist(dat1$X, breaks=30, plot=T)
h2 <- hist(dat1$Y, breaks=30, plot=T)

h1 <- hist(dat1$X, breaks=30, plot=T)
h2 <- hist(dat1$Y, breaks=30, plot=T)

