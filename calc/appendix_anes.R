### ============================================================= ###
### Measuring Political Sophistication using Open-ended Responses ###
### Patrick Kraft                                                 ###
### ============================================================= ###
## reduced analysis for presentation (based on data created in prep.R)


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
library(sampleSelection)

setwd("/data/Dropbox/Uni/Projects/2016/knowledge/calc")

## load data and stm results
load("out/anes2012.Rdata")
load("out/anes2016.Rdata")
#load("../data/anes_old.Rdata")

## QUESTION: remove wc=0 and spanish=1?

source("func.R")

## plot defaults
plot_default <- theme_classic(base_size=9) + theme(panel.border = element_rect(fill=NA))


########
# word count plots
########

wc_mean = mean(data2012$wc)
ggplot(data2012, aes(wc)) + geom_histogram(fill = "grey", binwidth = 25) + theme_classic(base_size = 8) + 
  theme(panel.border = element_rect(fill=NA)) + 
  geom_vline(xintercept = wc_mean, colour="red", linetype = "longdash") +
  ylab("Number of Respondents") + xlab("Word Count")
ggsave("../fig/anes2012_wc.pdf", width = 3, height = 2)

wc_mean = mean(data2016$wc)
ggplot(data2016, aes(wc)) + geom_histogram(fill = "grey", binwidth = 25) + theme_classic(base_size = 8) + 
  theme(panel.border = element_rect(fill=NA)) + 
  geom_vline(xintercept = wc_mean, colour="red", linetype = "longdash") +
  ylab("Number of Respondents") + xlab("Word Count")
ggsave("../fig/anes2016_wc.pdf", width = 3, height = 2)

########
# correlation matrices: compare with common measures
########

datcor <- data[,c("polknow_text_mean","polknow_factual","polknow_evalpre")]
colnames(datcor) <- paste0("v",1:ncol(datcor))

#pdf("../fig/corplot_pres.pdf",width=3.3, height=3.3)
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
        , columnLabels = c("Discursive\nSophistication","Factual\nKnowledge"
                           ,"Interviewer\nEvaluation (Pre)")) + plot_default
#dev.off()

#pdf("../fig/corplot_empty.pdf",width=3.3, height=3.3)
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
        , columnLabels = c("Discursive\nSophistication","Factual\nKnowledge"
                           ,"Interviewer\nEvaluation (Pre)")) + 
  theme_classic(base_size=9) + theme(panel.border = element_rect(fill="white"))
#dev.off()



########
# robustness checks: determinants of willingness to respond?
########

## check determinants of oe response >0
m2 <- glm(as.numeric(wc>0) ~ female + polmedia + poldisc + educ + faminc + log(age) + relig + black + mode, data = anes2012, family=binomial("logit"))
summary(m2)

## gender differences in willingness to respond & length of response
mean(data$wc)
t.test(wc~female, data=data)
t.test(as.numeric(wc>0)~female, data=anes2012)

## prep data for heckit model
heck_tmp <- data.frame(caseid=data$caseid, polknow_text_mean=data$polknow_text_mean)
heck_tmp <- merge(anes2012, heck_tmp, all=TRUE)
heck_tmp$select <- as.numeric(!is.na(heck_tmp$polknow_text_mean))

table(heck_tmp$select)
table(is.na(heck_tmp$polknow_text_mean), heck_tmp$wc>0)

## estimate heckit model (does this specification make sense theoretically?)
m3 <- heckit(select ~ female + polmedia + poldisc + educ + faminc + log(age) + relig + black + mode + wordsum
             , polknow_text_mean ~ female + polmedia + poldisc + educ + faminc + log(age) + relig + black + mode
             , data = heck_tmp)
summary(m3)

m3 <- heckit(select ~ female + polmedia + poldisc + educ + faminc + log(age) + relig + black + mode + wordsum
             , polknow_factual ~ female + polmedia + poldisc + educ + faminc + log(age) + relig + black + mode
             , data = heck_tmp)
summary(m3)


## heckit model for other outcomes

m3 <- heckit(select ~ female + polmedia + poldisc + educ + faminc + log(age) + relig + black + mode + wordsum
             , effic_int ~ polknow_text_mean + polknow_factual + female + polmedia + poldisc + educ + faminc + log(age) + relig + black + mode
             , data = heck_tmp)
summary(m3)


########
# check validity of knowledge measure similar to Prior
########

m2 <- lm(redist ~ tax * polknow_text + tax * polknow_factual + female + age + black + relig + educ + faminc + ideol_ct + pid_cont, data=data2012)
summary(m2)

summary(lm(redist ~ tax * polknow_text + tax * polknow_factual + female + age + black + relig + educ + faminc + ideol_ct + pid_cont, data=data2016))

## prepare dataframe for plotting (sloppy code)
dfplot <- summary(m2)$coefficients[c(2:4,13,14),1:2]
tmp <- rownames(dfplot); rownames(dfplot) <- NULL; dfplot <- data.frame(dfplot)
dfplot$iv <- gsub(":","_", tmp)
dfplot$ivname <- factor(dfplot$iv
                        , levels = c("tax_polknow_factual", "tax_polknow_text", "polknow_factual", "polknow_text", "tax")
                        , labels = c("Tax X Factual Know.","Tax X Discursive Soph."
                                     , "Factual Knowledge", "Discursive Sophistication"
                                     , "Support Tax Increase"))

ggplot(dfplot, aes(y=ivname, x=Estimate
                   , xmin = Estimate-1.96*Std..Error, xmax = Estimate+1.96*Std..Error)) + 
  geom_vline(xintercept = 0, color="grey") + xlab("Estimate") + ylab("Independent Variable") +
  geom_point(size=.75) + geom_errorbarh(height = 0) + 
  plot_default
ggsave("../fig/redistribution.pdf",height=2.5,width=4)


########
# sophistication as an independent variable: no gender interaction
########

m4a <- m4b <- m4c <- NULL
m4a[[1]] <- lm(effic_int ~ polknow_text_mean + female + educ + faminc + log(age) + black + relig + mode, data = data)
m4a[[2]] <- lm(effic_ext ~ polknow_text_mean + female + educ + faminc + log(age) + black + relig + mode, data = data)
m4a[[3]] <- lm(part ~ polknow_text_mean + female + educ + faminc + log(age) + black + relig + mode, data = data)
m4a[[4]] <- glm(vote ~ polknow_text_mean + female + educ + faminc + log(age) + black + relig + mode, data = data, family=binomial("logit"))
m4b[[1]] <- lm(effic_int ~ polknow_factual + female + educ + faminc + log(age) + black + relig + mode, data = data)
m4b[[2]] <- lm(effic_ext ~ polknow_factual + female + educ + faminc + log(age) + black + relig + mode, data = data)
m4b[[3]] <- lm(part ~ polknow_factual + female + educ + faminc + log(age) + black + relig + mode, data = data)
m4b[[4]] <- glm(vote ~ polknow_factual + female + educ + faminc + log(age) + black + relig + mode, data = data, family=binomial("logit"))
m4c[[1]] <- lm(effic_int ~ polknow_evalpre + female + educ + faminc + log(age) + black + relig, data = data)
m4c[[2]] <- lm(effic_ext ~ polknow_evalpre + female + educ + faminc + log(age) + black + relig, data = data)
m4c[[3]] <- lm(part ~ polknow_evalpre + female + educ + faminc + log(age) + black + relig, data = data)
m4c[[4]] <- glm(vote ~ polknow_evalpre + female + educ + faminc + log(age) + black + relig, data = data, family=binomial("logit"))

res <- rbind(sim(m4a, iv=data.frame(polknow_text_mean=range(data$polknow_text_mean, na.rm = T)))
             , sim(m4b, iv=data.frame(polknow_factual=range(data$polknow_factual, na.rm = T)))
             , sim(m4c, iv=data.frame(polknow_evalpre=range(data$polknow_evalpre, na.rm = T))))
res$dvlab <- factor(res$dv, labels = c("Internal Efficacy","External Efficacy"
                                       ,"Non-conv. Participation","Turnout"))
res$ivlab <- factor(res$iv, labels = dvnames)

ggplot(res, aes(y=ivlab, x=mean, xmin=cilo, xmax=cihi)) + 
  geom_point() + geom_errorbarh(height=0) + facet_wrap(~dvlab, scale="free_x") +
  geom_vline(xintercept = 0, color="grey") + 
  xlab("Marginal Effect") + ylab("Independent Variable") + plot_default +
  scale_y_discrete(limits = rev(levels(res$ivlab)))
#ggsave("../fig/knoweff_pres.pdf", width=4, height=3)

ggplot(res, aes(y=ivlab, x=mean, xmin=cilo, xmax=cihi)) + 
  geom_point() + geom_errorbarh(height=0) + facet_wrap(~dvlab, scale="free_x") +
  xlab("Marginal Effect") + ylab("Independent Variable") +
  theme_classic(base_size = 9) + theme(panel.border = element_rect(fill="white")) +
  geom_vline(xintercept = 0, color="grey") + 
  scale_y_discrete(limits = rev(levels(res$ivlab)))
#ggsave("../fig/knoweff_empty.pdf", width=4, height=3)

