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
  scale_x_continuous(expand = c(0, 0)) +
  ggtitle("German respondents") + ylab("Level of Justification") + xlab("Text-based sophistication")

p_french <- ggplot(opend_french, aes(x=polknow_text_mean, y=as.factor(loj))) +
  geom_joy(scale = 4, alpha=.2, fill="lightblue") + theme_joy() +
  scale_y_discrete(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) +
  ggtitle("French respondents") + ylab("Level of Justification") + xlab("Text-based sophistication")

p_italian <- ggplot(opend_italian, aes(x=polknow_text_mean, y=as.factor(loj))) +
  geom_joy(scale = 4, alpha=.2, fill="lightblue") + theme_joy() +
  scale_y_discrete(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) +
  ggtitle("Italian respondents") + ylab("Level of Justification") + xlab("Text-based sophistication")

pdf("../fig/ggjoy.pdf", width=6, height=9)
grid.arrange(p_german, p_french, p_italian, ncol=1)
dev.off()
