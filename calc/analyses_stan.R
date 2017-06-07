### ============================================================= ###
### Measuring Political Sophistication using Open-ended Responses ###
### Patrick Kraft                                                 ###
### ============================================================= ###
## runs analyses for paper (based on data created in prep.R)


rm(list = ls())
library(car)
library(quanteda)
library(stm)
library(corrplot)
library(ggplot2)
library(xtable)
library(gridExtra)
library(GGally)
library(dplyr)
library(pmisc)
library(rstan)
library(ineq)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

setwd("/data/Dropbox/Uni/Projects/2016/knowledge/calc")

## load data and stm results
load("out/anes.Rdata")

## optional: load previous model results
load("out/tmp_stan.Rdata")

## drop spanish speaking + empty responses
keep <- anes2012$spanish == 0 & anes2012$wc != 0
anes2012spell <- anes2012spell[keep,]
anes2012opend <- anes2012opend[keep,]
anes2012 <- anes2012[keep,]


### model response length via poisson
iwc <- apply(anes2012spell[,-1], 2, function(x){
  unlist(lapply(strsplit(x,"\\s+"), length))
})

m1dl <- list(N = nrow(iwc), I = ncol(iwc), w = iwc)
m1stan <- stan(file = "poisson.stan", data=m1dl)

print(m1stan, par=c(paste0("theta[",1:10,"]")))
print(m1stan, par="lambda")

# compare to lambda for interpretation
apply(iwc,2,max)
apply(iwc,2,mean)

theta <- get_posterior_mean(m1stan, pars="theta")
theta <- theta[,ncol(theta)]

plot(density(theta))
plot(theta~apply(iwc,1,sum))


### model response length weigthed by topic diversity via lognormal
iwc_weighted <- iwc[anes2012$caseid %in% data$caseid, ] * data$topic_diversity
head(iwc_weighted/iwc[anes2012$caseid %in% data$caseid, ])

min(iwc_weighted)
plot(density(iwc_weighted))
plot(density(iwc_weighted[iwc_weighted!=0]))
plot(density(log(iwc_weighted[iwc_weighted!=0])))



### save temp results

save.image("out/tmp_stan.Rdata")
