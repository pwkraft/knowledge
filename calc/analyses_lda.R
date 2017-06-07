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


### model topic proportions via simple LDA
source("sim-lda.R")
m2stan <- stan("lda.stan")


### test model via variational bayes
m2model <- stan_model("lda.stan")
m2vb <- vb(m2model)
m2vb
theta

### use vb means as starting values
sval <- get_posterior_mean(m2vb)
sval_theta <- matrix(sval[grep("theta",rownames(sval))], ncol=2, byrow = T)
sval_theta
sval_phi <- matrix(sval[grep("phi",rownames(sval))], nrow=2, byrow = T)
sval_phi

## enforce simplex
simplex <- function(x) x/sum(x)
sval_theta <- t(apply(sval_theta, 1, simplex))
sval_phi <- t(apply(sval_phi, 1, simplex))

## list of initial values
init_list = list(list(theta=sval_theta, phi=sval_phi)
                 , list(theta=sval_theta, phi=sval_phi)
                 , list(theta=sval_theta, phi=sval_phi)
                 , list(theta=sval_theta, phi=sval_phi))

m2vbstan <- stan("lda.stan", init = init_list)


### try out actual data (remove infrequent words?)
#docs <- apply(anes2012spell[1:100,-1], 1, function(x) paste0(x, collapse=" "))
docs <- apply(anes2012spell[,-1], 1, function(x) paste0(x, collapse=" "))
docs_corpus <- corpus(docs)
docs_dfm <- dfm(docs_corpus, stem=TRUE, ignoredFeatures=stopwords())
docs_features <- docs_dfm@Dimnames$features

docs_matrix <- as.matrix(docs_dfm)[1:200,]
K <- 5 # number of topics
V <- ncol(docs_matrix) # number of unique words
M <- nrow(docs_matrix) # number of documents
N <- sum(docs_matrix) # total number of word instances
#w <- doc <- vector(mode="integer", N) # word and doc id
w <- doc <- NULL
alpha <- rep(1/K, K)
beta <- rep(1/V, V)

## inefficient, but whatever
pb <- txtProgressBar(min=0,max=M,style = 3)
for(m in 1:M){
  for(v in 1:V){
    if(docs_matrix[m,v] != 0){
      w <- c(w, rep(v, docs_matrix[m,v]))
      doc <- c(doc, rep(m, docs_matrix[m,v]))
    }
  }
  setTxtProgressBar(pb, m)
}

## test vb
m3model <- stan_model("lda.stan")

## only works with smaller dataset and 2 topics so far
m3vb <- vb(m3model)

## not sure what this does
m3vb <- vb(m3model, grad_samples=10)

## setting eta allows it to estimate
m3vb <- vb(m3model, eta = 0.001, adapt_engaged=F)

## compare topic diversity
topic_diversity <- get_posterior_mean(m3vb)
topic_diversity <- matrix(topic_diversity[grep("theta",rownames(topic_diversity))], ncol=25, byrow = T)
topic_diversity <- apply(topic_diversity, 1, function(x) 1 - ineq(x,type="Gini"))

plot(topic_diversity ~ anes2012$polknow_factual)
summary(lm(topic_diversity ~ anes2012$polknow_factual))

plot(topic_diversity[anes2012$caseid %in% data$caseid] ~ data$topic_diversity)
summary(lm(topic_diversity[anes2012$caseid %in% data$caseid] ~ data$topic_diversity))

plot(data$topic_diversity ~ data$polknow_factual)
summary(lm(data$topic_diversity ~ data$polknow_factual))

plot(data$topic_diversity ~ data$lwc)
summary(lm(data$topic_diversity ~ data$lwc))

plot(topic_diversity ~ anes2012$lwc)
summary(lm(topic_diversity ~ anes2012$lwc))


### add diversity as hierarchical prior in lda

m4model <- stan_model("lda_diversity.stan")

## test vb
m4vb <- vb(m4model)

## setting eta allows it to estimate
m4vb <- vb(m4model, eta = 0.001, adapt_engaged=F)


## compare with topic diversity

### test poisson irt with given probabilities


save.image("out/tmp_stan.Rdata")
