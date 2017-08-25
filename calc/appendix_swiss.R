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

setwd("/data/Dropbox/Uni/Projects/2016/knowledge/calc")

## load data and stm results
load("out/swiss.Rdata")



########
# code for word count plots + components of diversity measure
########

## histogram/density of wc
wc_mean = mean(data$wc)

p1 <- ggplot(data, aes(wc)) + geom_histogram(fill = "grey", binwidth = 2) + 
  theme_classic(base_size = 8) + 
  theme(panel.border = element_rect(fill=NA)) + 
  geom_vline(xintercept = wc_mean, colour="red", linetype = "longdash") +
  ylab("Number of Respondents") + xlab("Word Count")

## histogram/density of lwc
lwc_mean = mean(data$lwc)

p2 <- ggplot(data, aes(lwc, ..density..)) + geom_histogram(binwidth = 0.05, fill='grey') + 
  geom_density() + theme_classic(base_size = 8) + 
  theme(panel.border = element_rect(fill=NA)) + 
  geom_vline(xintercept = lwc_mean, colour="red", linetype = "longdash") + 
  ylab("Density") + xlab("log(Word Count) / max[log(Word Count)]")


pdf("../fig/swiss_wc.pdf",width=5, height=2)
grid.arrange(p1, p2, ncol=2)
dev.off()

## histogram/density of topic_diversity
tmp <- rbind(opend_italian, opend_french, opend_german)
diversity_mean = mean(tmp$topic_diversity)

p3 <- ggplot(tmp, aes(topic_diversity, ..density..)) + geom_histogram(binwidth = 0.01,fill='grey') + geom_density() + theme_classic() + 
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  geom_vline(xintercept = diversity_mean, colour="red", linetype = "longdash") + 
  ylab("Density") + xlab("Topic Diversity")

## histogram/density of ditem
tmp$opinionation <- (tmp$opinionation - min(tmp$opinionation)) / (max(tmp$opinionation)-min(tmp$opinionation))
opinionation_mean = mean(tmp$opinionation)

p4 <- ggplot(tmp, aes(opinionation, ..density..)) + geom_histogram(binwidth = 0.01,fill='grey') + geom_density() + theme_classic() + 
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  geom_vline(xintercept = opinionation_mean, colour="red", linetype = "longdash") + 
  ylab("Density") + xlab("Opinionation")

pdf("../fig/swiss_diversity.pdf",width=5, height=2)
grid.arrange(p3, p4, ncol=2)
dev.off()
