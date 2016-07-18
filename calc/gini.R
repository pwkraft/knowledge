library(ineq)

x <- runif(100)
ineq(x,"Gini")

n <- length(x)
x <- sort(x)
(2 * sum(x * 1L:n)/sum(x) - (n + 1L))/n

gini1 <- gini2 <- matrix(rep(NA,length(x)^2),ncol=length(x))
for(i in 1:length(x)){
  for(j in 1:length(x)){
    gini1[i,j] <- abs(x[i]-x[j])
    gini2[i,j] <- x[j]
  }
}
sum(gini1)/(2*sum(gini2))


