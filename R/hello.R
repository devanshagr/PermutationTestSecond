Permutation <- function(as.data.frame(df),c1,c2,n,w1,w2){
  D_null<-c()
  V1<-df[,c1]
  V2<-df[,c2]
  sub.value1 <- df[df[, c1] == w1, c2]
  sub.value2 <- df[df[, c1] == w2, c2]
  D <-  abs(mean(sub.value2) - mean(sub.value1))
  m=length(V1)
  l=length(V1[V1==w2])
  for(jj in 1:n){
    null <- rep(w1,length(V1))
    null[sample(m,l)] <- w2
    nf <- data.frame(Key=null, Value=V2)
    names(nf) <- c("Key","Value")
    w1_null <- nf[nf$Key == w1,2]
    w2_null <- nf[nf$Key == w2,2]
    D_null <- c(D_null,mean(w2_null) - mean(w1_null))
  }
  M<-mean(D_null>D)
  return(M)
}
