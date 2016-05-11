### ============================= ###
### Text as Data - Final Project  ###
### Laura Buchanan, Patrick Kraft ###
### ============================= ###
## runs analyses for paper (based on data created in prep.R)


rm(list = ls())
library(dplyr)
library(quanteda)
library(stm)
library(corrplot)
library(ggplot2)

if(sessionInfo()$platform == "x86_64-pc-linux-gnu (64-bit)"){
  setwd("/data/Dropbox/Uni/Projects/2016/knowledge/calc")
} else {setwd('/Users/Laura/Desktop/knowledge/data')}

## load data and stm results
load("../data/anes.Rdata")

## Graphical display of estimated topic proportions 
plot.STM(stm_fit, type = "summary")
plot.STM(stm_fit, type = "perspectives", topics = c(18,20))
topic_words <- labelTopics(stm_fit)


# old correlation matrix
corrm <- cor(data[,c("polknow_office", "polknow_factual", "polknow_majority", "polknow_evalpre"
                  , "polknow_evalpost", "intpre", "intpost", "educ_cont", "polmedia", "poldisc"
                  , "wc", "lwc", "nitem", "pitem", "litem", "ditem", "topic_diversity"
                  , "topic_diversity_length", "topic_thresh_sum", "topic_thresh_count"
                  , "topic_diversity_length_litem", "topic_diversity_length_ditem")]
         ,use="pairwise.complete.obs", method = "pearson")

corrplot(corrm,method="square")




#######
#code for correlation matrix plot
#######



corrm <- cor(data[,c("polknow_office", "polknow_factual", "polknow_majority", "polknow_evalpre"
                     , "polknow_evalpost", "intpre", "intpost", "educ_cont"
                     , "lwc",  "ditem", "topic_diversity"
                     , "topic_diversity_length_ditem")]
             ,use="pairwise.complete.obs", method = "pearson")

corrplot(corrm,method="square")



########
# code for word count plots
########

## histogram/density of lwc
lwc_mean = mean(data$lwc)

ggplot(data, aes(lwc, ..density..)) + geom_histogram(binwidth = 0.25, fill='grey') + geom_density() + theme_classic() + 
  theme(panel.border = element_rect(fill=NA)) + geom_vline(xintercept = lwc_mean, colour="red", linetype = "longdash") + theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

## histogram/density of wc
wc_mean = mean(data$wc)

ggplot(data, aes(wc, ..density..)) + geom_histogram(binwidth = 10, fill='grey') + geom_density() + theme_classic() + 
  theme(panel.border = element_rect(fill=NA)) + geom_vline(xintercept = wc_mean, colour="red", linetype = "longdash") + theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())



########
# code for diversity measures
########

## histogram/density of topic_diversity
diversity_mean = mean(data$topic_diversity)

ggplot(data, aes(topic_diversity, ..density..)) + geom_histogram(binwidth = 0.25,fill='grey') + geom_density() + theme_classic() + 
  theme(panel.border = element_rect(fill=NA)) + geom_vline(xintercept = diversity_mean, colour="red", linetype = "longdash") + theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

## histogram/density of ditem
ditem_mean = mean(data$ditem)

ggplot(data, aes(ditem, ..density..)) + geom_histogram(binwidth = 0.25,fill='grey') + geom_density() + theme_classic() + 
  theme(panel.border = element_rect(fill=NA)) + geom_vline(xintercept = ditem_mean, colour="red", linetype = "longdash") + theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())


## histogram/density of weighted 
topic_diversity_length_ditem_mean = mean(data$topic_diversity_length_ditem)

ggplot(data, aes(topic_diversity_length_ditem, ..density..)) + geom_histogram(binwidth = 10,fill='grey') + geom_density() + theme_classic() + 
  theme(panel.border = element_rect(fill=NA)) + geom_vline(xintercept = topic_diversity_length_ditem_mean, colour="red", linetype = "longdash") + theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())


### example texts

## max and min
data$resp[data$topic_diversity_length_ditem==min(data$topic_diversity_length_ditem)]
data$resp[data$topic_diversity_length_ditem==max(data$topic_diversity_length_ditem)]

arrange(data, topic_diversity_length_ditem) %>% filter(wc>20 & wc<50) %>% select(resp) %>% head()
arrange(data, topic_diversity_length_ditem) %>% filter(wc>20 & wc<50) %>% select(resp) %>% tail()

### test some models
m1 <- NULL
m1[[1]] <- lm(topic_diversity_length_ditem ~ polmedia + poldisc + educ_cont + female + age + black + relig + ideol_ct + pid_cont + mode, data = data)
m1[[2]] <- lm(polknow_office ~ polmedia + poldisc + educ_cont + female + age + black + relig + ideol_ct + pid_cont + mode, data = data)
m1[[3]] <- lm(polknow_factual ~ polmedia + poldisc + educ_cont + female + age + black + relig + ideol_ct + pid_cont + mode, data = data)
m1[[4]] <- lm(polknow_majority ~ polmedia + poldisc + educ_cont + female + age + black + relig + ideol_ct + pid_cont + mode, data = data)
m1[[5]] <- lm(polknow_evalpre ~ polmedia + poldisc + educ_cont + female + age + black + relig + ideol_ct + pid_cont, data = data)
m1[[6]] <- lm(polknow_evalpost ~ polmedia + poldisc + educ_cont + female + age + black + relig + ideol_ct + pid_cont, data = data)

lapply(m1, summary)
