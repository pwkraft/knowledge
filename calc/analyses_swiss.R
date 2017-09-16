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
library(stargazer)
library(ggjoy)

setwd("/data/Dropbox/Uni/Projects/2016/knowledge/calc")

## load data and stm results
load("out/swiss.Rdata")

## QUESTION: remove wc=0 and spanish=1?

source("sim.R")
source("latexTable.R")

## plot defaults
plot_default <- theme_classic(base_size=8) + theme(panel.border = element_rect(fill=NA))


#######################
### WORK ON NEW MEASURE
#######################

## check stm fit
dim(stm_fit$beta$logbeta[[1]])
# beta: List containing the log of the word probabilities for each topic.
apply(exp(stm_fit$beta$logbeta[[1]]), 1, sum)

#hist(apply(exp(stm_fit$beta$logbeta[[1]]), 2, sum))
#hist(apply(exp(stm_fit$beta$logbeta[[1]]), 2, max))

length(stm_fit$vocab)
stm_fit$vocab[1:5]

## compute shannon entropy
shannon <- function(x, reversed = F){
  out <- (- sum(log(x^x)/log(length(x))))
  if(reversed) out <- 1 - out
  out
}

## which features have the highest probability for each topic?
stm_fit$vocab[apply(exp(stm_fit$beta$logbeta[[1]]), 1, which.max)]

## which topic has highest likelihood for each word
term_topic <- apply(stm_fit$beta$logbeta[[1]], 2, which.max)
# -> compute log(#topics)

## shannon entropy of each term as a measure for its distinctiveness -> vague term or clear topic
pseudop <- exp(stm_fit$beta$logbeta[[1]])
pseudop <- t(t(pseudop)/apply(pseudop,2,sum))
term_entropy <- apply(pseudop, 2, shannon, reversed=T) # theoretically, it should be reversed=T
# -> calculate average distinctiveness of words

## combine both measures with open-ended responses
know <- data.frame(ntopics = rep(NA, length(out$documents))
                   , entropy = rep(NA, length(out$documents)))
for(doc in 1:length(out$documents)){
  know$ntopics[doc] <- log(length(unique(term_topic[out$documents[[doc]][1,]])))
  know$entropy[doc] <- sum(term_entropy[out$documents[[doc]][1,]] * out$documents[[doc]][2,]) / sum(out$documents[[doc]][2,])
}
data <- cbind(data, know)

data$polknow_text_mean <- data$lwc
data$polknow_text_mean <- data$ntopics
data$polknow_text_mean <- data$entropy
data$polknow_text_mean <- data$ntopics * data$entropy


########
# correlation matrices: compare with common measures
########

datcor <- opend_german[,c("loj", "polknow_text_mean", "polknow_text","topic_diversity", "lwc", "opinionation")]
#datcor <- opend_german[,c("loj", "polknow_text_mean")]
colnames(datcor) <- paste0("v",1:ncol(datcor))

#pdf("../fig/corplot.pdf",width=5, height=5)
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.01, size=.2)), axisLabels="none"
        #, columnLabels = c("Text-based\nSophistication","Factual\nKnowledge","Office\nRecognition"
        #                   ,"Majorities\nin Congress", "Interviewer\nEvaluation (Pre)"
        #                   , "Interviewer\nEvaluation (Post)")
        ) + plot_default
#dev.off()


ggplot(opend_german, aes(x=loj, y=polknow_text_mean)) + geom_point(alpha=.1) + geom_smooth(method="lm") + plot_default


### joy plots

p_german <- ggplot(opend_german, aes(x=polknow_text_mean, y=as.factor(loj))) +
  geom_joy(scale = 4, alpha=.2, fill="lightblue") + theme_joy() +
  scale_y_discrete(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + xlim(.25,.85) +
  annotate("text", x=0.84, y=7, label = paste0("r = ",round(cor(opend_german$polknow_text_mean, opend_german$loj), 2))) +
  ggtitle("German respondents") + ylab("Level of Justification") + xlab("Text-based sophistication")

p_french <- ggplot(opend_french, aes(x=polknow_text_mean, y=as.factor(loj))) +
  geom_joy(scale = 4, alpha=.2, fill="lightblue") + theme_joy() +
  scale_y_discrete(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + xlim(.25,.85) +
  annotate("text", x=0.84, y=7, label = paste0("r = ",round(cor(opend_french$polknow_text_mean, opend_french$loj), 2))) +
  ggtitle("French respondents") + ylab("Level of Justification") + xlab("Text-based sophistication")

p_italian <- ggplot(opend_italian, aes(x=polknow_text_mean, y=as.factor(loj))) +
  geom_joy(scale = 4, alpha=.2, fill="lightblue") + theme_joy() +
  scale_y_discrete(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + xlim(.25,.85) +
  annotate("text", x=0.84, y=7, label = paste0("r = ",round(cor(opend_italian$polknow_text_mean, opend_italian$loj), 2))) +
  ggtitle("Italian respondents") + ylab("Level of Justification") + xlab("Text-based sophistication")

pdf("../fig/ggjoy.pdf", width=6, height=9)
grid.arrange(p_german, p_french, p_italian, ncol=1)
dev.off()
