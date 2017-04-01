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
load("anes.Rdata")
#load("../data/anes_old.Rdata")

# correlation matrices
# datcor <- data[,c("polknow_factual", "polknow_office", "polknow_majority","polknow_text")]
# colnames(datcor) <- paste0("v",1:ncol(datcor))
# 
# png("../fig/corplot.png",width=7, height=7, units="in",res=300)
# ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.02, size=.2)), axisLabels="none") + 
#   theme_classic() + theme(panel.border = element_rect(fill=NA))
# dev.off()



########
# code for word count plots + components of diversity measure
########

## histogram/density of wc
wc_mean = mean(data$wc)

p1 <- ggplot(data, aes(wc)) + geom_histogram(fill = "grey", binwidth = 25) + 
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


pdf("../fig/wc.pdf",width=5, height=2)
grid.arrange(p1, p2, ncol=2)
dev.off()

## histogram/density of topic_diversity
diversity_mean = mean(data$topic_diversity)

p3 <- ggplot(data, aes(topic_diversity, ..density..)) + geom_histogram(binwidth = 0.01,fill='grey') + geom_density() + theme_classic() + 
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  geom_vline(xintercept = diversity_mean, colour="red", linetype = "longdash") + 
  ylab("Density") + xlab("Topic Diversity")

## histogram/density of ditem
ditem_mean = mean(data$ditem)

p4 <- ggplot(data, aes(ditem, ..density..)) + geom_histogram(binwidth = 0.01,fill='grey') + geom_density() + theme_classic() + 
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  geom_vline(xintercept = ditem_mean, colour="red", linetype = "longdash") + 
  ylab("Density") + xlab("Opinion Diversity")

pdf("../fig/diversity.pdf",width=5, height=2)
grid.arrange(p3, p4, ncol=2)
dev.off()


########
# code to get response samples
########

## max and min
varnames <- c("Case ID","Obama (likes)","Obama (dislikes)","Romney (likes)","Romney (dislikes)"
              ,"Democratic party (likes)","Democratic party (dislikes)"
              ,"Republican party (likes)","Republican party (dislikes)")

## max/min w/o wc restriction
tab_ex1 <- t(rbind(anes2012opend[anes2012opend$caseid == with(data, caseid[polknow_text==min(polknow_text)])[1],]
                 , anes2012opend[anes2012opend$caseid == with(data, caseid[polknow_text==max(polknow_text)]),]))
rownames(tab_ex1) <- varnames
colnames(tab_ex1) <- c("Minimum","Maximum")
xtable(tab_ex1)

## max/min w/ wc restiction + equal factual knowledge
tab_ex2 <- t(rbind(anes2012opend[anes2012opend$caseid == with(filter(data,wc>25 & wc<125 & polknow_factual == .6), caseid[polknow_text==min(polknow_text)]),]
                   , anes2012opend[anes2012opend$caseid == with(filter(data,wc>25 & wc<125 & polknow_factual == .6), caseid[polknow_text==max(polknow_text)]),]))
rownames(tab_ex2) <- varnames
colnames(tab_ex2) <- c("Minimum","Maximum")
xtable(tab_ex2)

arrange(data, polknow_text) %>% filter(wc>25 & wc<125 & polknow_factual == .6) %>% dplyr::select(resp, polknow_text) %>% head()
arrange(data, polknow_text) %>% filter(wc>25 & wc<125 & polknow_factual == .6) %>% dplyr::select(resp, polknow_text) %>% tail()

## max/min w/ wc restiction + different factual knowledge + female vs. male (issue: text measure might not be min<max due to different groups)
tab_ex3 <- t(rbind(anes2012opend[anes2012opend$caseid == with(filter(data,wc>25 & wc<125 & polknow_factual == .8 & female == 0), caseid[polknow_text==min(polknow_text)]),]
                   , anes2012opend[anes2012opend$caseid == with(filter(data,wc>25 & wc<125 & polknow_factual == .6 & female == 1), caseid[polknow_text==max(polknow_text)]),]))
rownames(tab_ex3) <- varnames
colnames(tab_ex3) <- c("Minimum","Maximum")
xtable(tab_ex3)

## max/min w/ lower wc limit + equal factual knowledge
tab_ex3 <- t(rbind(anes2012opend[anes2012opend$caseid %in% with(filter(data,wc>25 & polknow_factual == .6), caseid[polknow_text==min(polknow_text)]),]
                   , anes2012opend[anes2012opend$caseid == with(filter(data,wc>25 & polknow_factual == .6), caseid[polknow_text==max(polknow_text)]),]))
rownames(tab_ex3) <- varnames
colnames(tab_ex3) <- c("Minimum","Maximum")
xtable(tab_ex3)


#########
# determinants of political knowledge including wordsum score
#########

m1 <- NULL
m1[[1]] <- lm(polknow_text ~ female + polmedia + poldisc + educ + age + black + relig + wordsum + mode, data = data)
m1[[2]] <- lm(polknow_factual ~ female + polmedia + poldisc + educ + age + black + relig + wordsum + mode, data = data)
m1[[3]] <- lm(polknow_office ~ female + polmedia + poldisc + educ + age + black + relig + wordsum + mode, data = data)
m1[[4]] <- lm(polknow_majority ~ female + polmedia + poldisc + educ + age + black + relig + wordsum + mode, data = data)
lapply(m1, summary)
