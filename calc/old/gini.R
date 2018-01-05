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

ineq(c(0,1),"Gini")
ineq(c(0,2),"Gini")
ineq(c(1,2),"Gini")
ineq(c(2,3),"Gini")

gini <- function(x){
  (1 - ineq(x, "Gini"))
}

giniNew <- function(x){
  tmp1 <- tmp2 <- matrix(rep(NA,length(x)^2),ncol=length(x))
  for(i in 1:length(x)){
    for(j in 1:length(x)){
      tmp1[i,j] <- abs(x[i]-x[j])
      tmp2[i,j] <- x[j]
    }
  }
  1 - sum(tmp1)/(2*sum(tmp2)) * length(x)/(length(x)-1) # the last part is needed to scale it back to 0-1
}

shannon <- function(x){
  (- sum(log(x^x)/log(length(x))))
}

simpson <- function(x){
  (1 - sum(x^2)) * length(x)/(length(x)-1) # the last part is needed to scale it back to 0-1
}

test2 <- list(c(0,0),c(1,0),c(0.99,.01),c(0.9,.1),c(.2,.8),c(.5,.5))
round(cbind(sapply(test2, shannon),sapply(test2, gini),sapply(test2, giniNew),sapply(test2, simpson)),3)
plot(sapply(test2, shannon)~sapply(test2, gini))
plot(sapply(test2, shannon)~sapply(test2, giniNew))

test3 <- list(c(0,0,0),c(0,0,1),c(0,.2,.8),c(.1,.1,.8),c(0,.5,.5),c(.34,.33,.33))
round(cbind(sapply(test3,shannon),sapply(test3,gini),sapply(test3, giniNew),sapply(test3, simpson)),3)
plot(sapply(test3,shannon)~sapply(test3,gini))
plot(sapply(test3,shannon)~sapply(test3,giniNew))

test4 <- list(c(0,0,0,0),c(0,0,0,1),c(0,0,.2,.8),c(0,.1,.1,.8),c(0,0,.5,.5),c(.25,.25,.25,.25))
round(cbind(sapply(test4,shannon),sapply(test4,gini),sapply(test4, giniNew),sapply(test4, simpson)),3)
plot(sapply(test4,shannon)~sapply(test4,gini))
plot(sapply(test4,shannon)~sapply(test4,giniNew))