### ============================================================= ###
### Measuring Political Sophistication using Open-ended Responses ###
### Patrick Kraft                                                 ###
### ============================================================= ###
## runs analyses for paper (based on data created in prep.R)


rm(list = ls())
library(tidyverse)
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
library(ggridges)

#setwd("/data/Dropbox/Uni/projects/2016/knowledge/calc")

## load data and stm results
load("calc/out/swiss.Rdata")

## QUESTION: remove wc=0 and spanish=1?

source("calc/func.R")

## plot defaults
plot_default <- theme_classic(base_size=9) + theme(panel.border = element_rect(fill=NA))


########
# Plot estimated topics
########

pdf("fig/swiss_stm_prop.pdf", width=12, height=7)
par(mfrow=c(1,3), mar=c(4.2,0.5,2.5,0.5))
plot(stm_fit_german
     , main=paste0("German Respondents (k = ",stm_fit_german$settings$dim$K,")",collapse = "")
     , n=5, labeltype = "prob", text.cex = 1)
plot(stm_fit_french
     , main=paste0("French Respondents (k = ",stm_fit_french$settings$dim$K,")",collapse = "")
     , n=5, labeltype = "prob", text.cex = 1)
plot(stm_fit_italian
     , main=paste0("Italian Respondents (k = ",stm_fit_italian$settings$dim$K,")",collapse = "")
     , n=5, labeltype = "prob", text.cex = 1)
dev.off()


########
# correlation matrices of individual components
########

datcor <- rbind(opend_german[,c("considerations","consistency","wordchoice")]
                , opend_french[,c("considerations","consistency","wordchoice")]
                , opend_italian[,c("considerations","consistency","wordchoice")]
                )
colnames(datcor) <- paste0("v",1:ncol(datcor))

ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
        , columnLabels = c("Considerations","Consistency","Word Choice")) + plot_default
ggsave("fig/swiss_corplot_components.pdf",width=2.6, height=2.6)



### ridge plots

plot_default <- theme_classic(base_size=8) +
  theme(panel.border = element_rect(fill=NA),
        plot.title = element_text(size = 8, vjust=-1),
        axis.title = element_text(size=8),
        axis.title.x = element_text(vjust=1.5),
        plot.margin = unit(c(-.1,.1,0,.1), "cm"))


p_german <- ggplot(opend_german, aes(x=polknow_text_mean, y=as.factor(loj))) +
  geom_density_ridges(scale = 4, alpha=.5, fill="blue") + plot_default +
  scale_y_discrete(expand = c(0.05, 0.05)) + # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + xlim(0,1) +
  annotate("text", x=0.1, y=8, size=2, label = paste0("r = ",round(cor(opend_german$polknow_text_mean, opend_german$loj), 2))) +
  ggtitle("German respondents") + ylab("") + xlab("")

p_german_empty <- ggplot(opend_german, aes(x=polknow_text_mean, y=as.factor(loj))) +
  geom_density_ridges(scale = 4, alpha=.5, fill="blue") + plot_default +
  scale_y_discrete(expand = c(0.05, 0.05)) +   # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + xlim(0,1) +
  annotate("text", x=0.1, y=8, size=2, label = paste0("r = ",round(cor(opend_german$polknow_text_mean, opend_german$loj), 2))) +
  ggtitle("German respondents") + ylab("") + xlab("") + theme(panel.border = element_rect(fill="white"))

p_french <- ggplot(opend_french, aes(x=polknow_text_mean, y=as.factor(loj))) +
  geom_density_ridges(scale = 4, alpha=.5, fill="blue") + plot_default +
  scale_y_discrete(expand = c(0.05, 0.05)) +   # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + xlim(0,1) +
  annotate("text", x=0.1, y=8, size=2, label = paste0("r = ",round(cor(opend_french$polknow_text_mean, opend_french$loj), 2))) +
  ggtitle("French respondents") + ylab("Level of Justification") + xlab("")

p_french_empty <- ggplot(opend_french, aes(x=polknow_text_mean, y=as.factor(loj))) +
  geom_density_ridges(scale = 4, alpha=.5, fill="blue") + plot_default +
  scale_y_discrete(expand = c(0.05, 0.05)) +   # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + xlim(0,1) +
  annotate("text", x=0.1, y=8, size=2, label = paste0("r = ",round(cor(opend_french$polknow_text_mean, opend_french$loj), 2))) +
  ggtitle("French respondents") + ylab("Level of Justification") + xlab("") + theme(panel.border = element_rect(fill="white"))

p_italian <- ggplot(opend_italian, aes(x=polknow_text_mean, y=as.factor(loj))) +
  geom_density_ridges(scale = 4, alpha=.5, fill="blue") + plot_default +
  scale_y_discrete(expand = c(0.05, 0.05)) +   # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + xlim(0,1) +
  annotate("text", x=0.1, y=8, size=2, label = paste0("r = ",round(cor(opend_italian$polknow_text_mean, opend_italian$loj), 2))) +
  ggtitle("Italian respondents") + ylab("") + xlab("Discursive sophistication")

p_italian_empty <- ggplot(opend_italian, aes(x=polknow_text_mean, y=as.factor(loj))) +
  geom_density_ridges(scale = 4, alpha=.5, fill="blue") + plot_default +
  scale_y_discrete(expand = c(0.05, 0.05)) +   # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + xlim(0,1) +
  annotate("text", x=0.1, y=8, size=2, label = paste0("r = ",round(cor(opend_italian$polknow_text_mean, opend_italian$loj), 2))) +
  ggtitle("Italian respondents") + ylab("") + xlab("Discursive sophistication") + theme(panel.border = element_rect(fill="white"))

pdf("fig/swiss_ggridges_0.pdf", width=2.5, height=3)
grid.arrange(p_german_empty, p_french_empty, p_italian_empty, ncol=1)
dev.off()

pdf("fig/swiss_ggridges_1.pdf", width=2.5, height=3)
grid.arrange(p_german, p_french_empty, p_italian_empty, ncol=1)
dev.off()

pdf("fig/swiss_ggridges_2.pdf", width=2.5, height=3)
grid.arrange(p_german, p_french, p_italian_empty, ncol=1)
dev.off()

pdf("fig/swiss_ggridges_3.pdf", width=2.5, height=3)
grid.arrange(p_german, p_french, p_italian, ncol=1)
dev.off()


######################
### old combined plot

plot_default <- theme_classic(base_size=8) + theme(panel.border = element_rect(fill=NA))

opend_combined <- rbind(data.frame(opend_german, language = "German")
                        , data.frame(opend_french, language = "French")
                        , data.frame(opend_italian, language = "Italian"))

opend_cor <- rbind(data.frame(cor = paste0("r = ",round(cor(opend_german$polknow_text_mean, opend_german$loj), 2)), language = "German")
                   , data.frame(cor = paste0("r = ",round(cor(opend_french$polknow_text_mean, opend_french$loj), 2)), language = "French")
                   , data.frame(cor = paste0("r = ",round(cor(opend_italian$polknow_text_mean, opend_italian$loj), 2)), language = "Italian"))
opend_cor$polknow_text_mean <- 0.05
opend_cor$loj <- 4

ggplot(opend_combined, aes(x=polknow_text_mean, y=as.factor(loj))) +
  geom_density_ridges(scale = 4, alpha=.5, fill="blue") + plot_default +
  scale_y_discrete(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + facet_wrap(~language,ncol=3) +
  geom_text(data=opend_cor, aes(label=cor),size=2,vjust=-9) +
  ylab("Level of Justification") + xlab("Discursive sophistication")
ggsave("fig/swiss_ggridges.pdf",width=6,height=2)

ggplot(opend_combined, aes(x=polknow_text_mean, y=as.factor(loj))) +
  geom_density_ridges(scale = 4, alpha=.5, fill="blue") + plot_default +
  scale_y_discrete(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + facet_wrap(~language,ncol=1) +
  geom_text(data=opend_cor, aes(label=cor),size=2) + theme(panel.border = element_rect(fill="white")) +
  ylab("Level of Justification") + xlab("Discursive sophistication")
ggsave("fig/swiss_ggridges_empty.pdf",width=2.5,height=5)

ggplot(opend_combined, aes(x=polknow_text_mean, y=as.factor(loj))) +
  geom_density_ridges(scale = 4, alpha=.5, fill="blue") + plot_default +
  scale_y_discrete(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) + facet_wrap(~language,ncol=1) +
  geom_text(data=opend_cor, aes(label=cor),size=2,vjust=-9) +
  ylab("Level of Justification") + xlab("Discursive sophistication")
ggsave("fig/swiss_ggridges_poster.pdf",width=4,height=4)


######################
### Is there a gender gap in manually coded level of justification?

m1 <- NULL
m1[[1]] <- lm(polknow_text_mean ~ female + edu + log(age), data = opend_german)
m1[[2]] <- lm(loj ~ female + edu + log(age), data = opend_german)
m1[[3]] <- lm(polknow_text_mean ~ female + edu + log(age), data = opend_french)
m1[[4]] <- lm(loj ~ female + edu + log(age), data = opend_french)
m1[[5]] <- lm(polknow_text_mean ~ female + edu + log(age), data = opend_italian)
m1[[6]] <- lm(loj ~ female + edu + log(age), data = opend_italian)
lapply(m1, summary)



