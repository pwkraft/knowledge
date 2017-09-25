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


opend_combined <- rbind(data.frame(opend_german, language = "German")
                        , data.frame(opend_french, language = "French")
                        , data.frame(opend_italian, language = "Italian"))

opend_cor <- rbind(data.frame(cor = paste0("r = ",round(cor(opend_german$polknow_text_mean, opend_german$loj), 2)), language = "German")
                   , data.frame(cor = paste0("r = ",round(cor(opend_french$polknow_text_mean, opend_french$loj), 2)), language = "French")
                   , data.frame(cor = paste0("r = ",round(cor(opend_italian$polknow_text_mean, opend_italian$loj), 2)), language = "Italian"))
opend_cor$polknow_text_mean <- 0.3
opend_cor$loj <- 5

ggplot(opend_combined, aes(x=polknow_text_mean, y=as.factor(loj))) +
  geom_joy(scale = 4, alpha=.2, fill="lightblue") + plot_default +
  scale_y_discrete(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + xlim(.25,.85) + facet_wrap(~language,ncol=1) +
  geom_text(data=opend_cor, aes(label=cor),size=2) +
  ylab("Level of Justification") + xlab("Text-based sophistication")
ggsave("../fig/swiss_ggjoy.pdf",width=2.5,height=3)

ggplot(opend_combined, aes(x=polknow_text_mean, y=as.factor(loj))) +
  geom_joy(scale = 4, alpha=.2, fill="lightblue") + plot_default +
  scale_y_discrete(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + xlim(.25,.85) + facet_wrap(~language,ncol=1) +
  geom_text(data=opend_cor, aes(label=cor),size=2) + theme(panel.border = element_rect(fill="white")) +
  ylab("Level of Justification") + xlab("Text-based sophistication")
ggsave("../fig/swiss_ggjoy_empty.pdf",width=2.5,height=3)