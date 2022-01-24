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
load("out/yougov.Rdata")

## plot defaults
plot_default <- theme_classic(base_size=9) + theme(panel.border = element_rect(fill=NA))

########
# code for word count plots + components of diversity measure
########

## histogram/density of wc
wc_mean = mean(data_yg$wc)

p1 <- ggplot(data_yg, aes(wc)) + geom_histogram(fill = "grey", binwidth = 25) + 
  theme_classic(base_size = 8) + 
  theme(panel.border = element_rect(fill=NA)) + 
  geom_vline(xintercept = wc_mean, colour="red", linetype = "longdash") +
  ylab("Number of Respondents") + xlab("Word Count")

## histogram/density of lwc
lwc_mean = mean(data_yg$lwc)

p2 <- ggplot(data_yg, aes(lwc, ..density..)) + geom_histogram(binwidth = 0.05, fill='grey') + 
  geom_density() + theme_classic(base_size = 8) + 
  theme(panel.border = element_rect(fill=NA)) + 
  geom_vline(xintercept = lwc_mean, colour="red", linetype = "longdash") + 
  ylab("Density") + xlab("log(Word Count) / max[log(Word Count)]")


# pdf("../fig/yg_wc.pdf",width=5, height=2)
# grid.arrange(p1, p2, ncol=2)
# dev.off()

ggsave("../fig/yg_wc.pdf", p1, width = 3, height = 2)

## histogram/density of topic_diversity
data_yg$topic_diversity <- (data_yg$topic_diversity - min(data_yg$topic_diversity)) / (max(data_yg$topic_diversity)-min(data_yg$topic_diversity))
diversity_mean = mean(data_yg$topic_diversity)

p3 <- ggplot(data_yg, aes(topic_diversity, ..density..)) + geom_histogram(binwidth = 0.01,fill='grey') + geom_density() + theme_classic() + 
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  geom_vline(xintercept = diversity_mean, colour="red", linetype = "longdash") + 
  ylab("Density") + xlab("Topic Diversity")

## histogram/density of ditem
ditem_mean = mean(data_yg$ditem)

p4 <- ggplot(data_yg, aes(ditem, ..density..)) + geom_histogram(binwidth = 0.01,fill='grey') + geom_density() + theme_classic() + 
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  geom_vline(xintercept = ditem_mean, colour="red", linetype = "longdash") + 
  ylab("Density") + xlab("Opinionation")

pdf("../fig/yg_diversity.pdf",width=5, height=2)
grid.arrange(p3, p4, ncol=2)
dev.off()


### Histograms of dependent variables (ANES 2016)
desc <- list(NULL)
desc[[1]] <- ggplot(data_yg, aes(x=polknow_text_mean)) + geom_histogram() +
  labs(y="Count", x="Discursive Sophistication") + plot_default
desc[[2]] <- ggplot(data_yg, aes(x=know_pol)) + geom_bar(stat="count") + 
  labs(y="Count", x="Factual Knowledge") + plot_default
desc[[3]] <- ggplot(data_yg, aes(x=know_dis)) + geom_bar(stat="count") + 
  labs(y="Count", x="Information Retrieval") + plot_default
desc[[4]] <- ggplot(data_yg, aes(x=factor(female,labels=c("Male","Female")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Gender") + plot_default
desc[[5]] <- ggplot(data_yg, aes(x=factor(educ, labels=c("No College","College")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Education") + plot_default
desc[[6]] <- ggplot(data_yg, aes(x=faminc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Family Income") + plot_default
desc[[7]] <- ggplot(data_yg, aes(x=age)) + geom_bar(stat="count") + 
  labs(y="Count", x="Age") + plot_default
desc[[8]] <- ggplot(data_yg, aes(x=factor(black,labels=c("Other","Black non-Hispanic")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Race/Ethnicity") + plot_default
desc[[9]] <- ggplot(data_yg, aes(x=relig)) + geom_bar(stat="count") + 
  labs(y="Count", x="Church Attendance") + plot_default
pdf("../fig/descriptives_yg.pdf", width=9, height=9)
grid.arrange(grobs=desc,ncol=3)
dev.off()