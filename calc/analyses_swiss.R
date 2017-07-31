### ============================================================= ###
### Measuring Political Sophistication using Open-ended Responses ###
### Patrick Kraft                                                 ###
### ============================================================= ###
## runs analyses for paper (based on data created in prep.R)


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

setwd("/data/Dropbox/Uni/Projects/2016/knowledge/calc")

## load data and stm results
load("out/swiss.Rdata")

## QUESTION: remove wc=0 and spanish=1?

source("sim.R")
source("latexTable.R")

## plot defaults
plot_default <- theme_classic(base_size=8) + theme(panel.border = element_rect(fill=NA))


########
# correlation matrices: compare with common measures
########

datcor <- opend_german[,c("loj", "polknow_text_mean", "polknow_text","topic_diversity", "lwc", "opinionation")]
datcor <- opend_german[,c("loj", "polknow_text_mean")]
colnames(datcor) <- paste0("v",1:ncol(datcor))

#pdf("../fig/corplot.pdf",width=5, height=5)
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.01, size=.2)), axisLabels="none"
        #, columnLabels = c("Text-based\nSophistication","Factual\nKnowledge","Office\nRecognition"
        #                   ,"Majorities\nin Congress", "Interviewer\nEvaluation (Pre)"
        #                   , "Interviewer\nEvaluation (Post)")
        ) + plot_default
#dev.off()

ggplot(opend_german, aes(x=loj, y=polknow_text_mean)) + geom_point(alpha=.1) + geom_smooth(method="lm") + plot_default
