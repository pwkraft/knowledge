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
load("out/anes.Rdata")
#load("../data/anes_old.Rdata")

## QUESTION: remove wc=0 and spanish=1?

source("func.R")

## plot defaults
plot_default <- theme_classic(base_size=9) + theme(panel.border = element_rect(fill=NA))


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
# histograms comparing men and women
########

## histogram/density of weighted 
plot_df <- data.frame(rbind(cbind(data$polknow_text_mean, data$female, 1)
                      , cbind(data$polknow_factual, data$female, 2)
                      , cbind(data$polknow_evalpre, data$female, 3))) %>% na.omit()
colnames(plot_df) <- c("Knowledge","Gender","Variable")
plot_df$Gender <- factor(plot_df$Gender, labels = c("Male","Female"))
plot_df$Variable <- factor(plot_df$Variable, labels = c("Discursive\nSophistication", "Factual\nKnowledge"
                                                        , "Interviewer\nEvaluation (Pre)"))
plot_means <- plot_df %>% group_by(Variable, Gender) %>% 
  summarize_each(funs(mean="mean",n=length(.),sd="sd",quant=quantile(.,.95),max="max")) %>%
  mutate(cilo = mean - 1.96*sd/sqrt(n)
         , cihi = mean + 1.96*sd/sqrt(n))

## bar plots comparing men and women
ggplot(plot_means, aes(y=mean,x=Gender,ymin=cilo,ymax=cihi)) + 
  theme_classic(base_size=9) + theme(panel.border = element_rect(fill="white")) +
  geom_bar(stat="identity", fill="grey80") + geom_errorbar(width=.25) + 
  facet_wrap(~Variable) + ylab("Mean Value on Knowledge Measure") +
  geom_point(aes(y=max), col="white")
ggsave("../fig/meandiff_empty.pdf", width=4.75, height=2.5)

ggplot(plot_means, aes(y=mean,x=Gender,ymin=cilo,ymax=cihi, fill=Gender)) + plot_default + 
  geom_bar(stat="identity") + geom_errorbar(width=.25) + 
  facet_wrap(~Variable) + ylab("Mean Value on Knowledge Measure") +
  geom_point(aes(y=max), col="white") + guides(fill=FALSE) + scale_fill_brewer(palette="Paired")
ggsave("../fig/meandiff_pres_3.pdf", width=4.75, height=2.5)

plot_means[plot_means$Variable=="Discursive\nSophistication",-1:-2] <- NA
ggplot(plot_means, aes(y=mean,x=Gender,ymin=cilo,ymax=cihi, fill=Gender)) + plot_default + 
  geom_bar(stat="identity") + geom_errorbar(width=.25) + 
  facet_wrap(~Variable) + ylab("Mean Value on Knowledge Measure") +
  geom_point(aes(y=max), col="white") + guides(fill=FALSE) + scale_fill_brewer(palette="Paired")
ggsave("../fig/meandiff_pres_2.pdf", width=4.75, height=2.5)

plot_means[plot_means$Variable=="Factual\nKnowledge",-1:-2] <- NA
ggplot(plot_means, aes(y=mean,x=Gender,ymin=cilo,ymax=cihi, fill=Gender)) + plot_default + 
  geom_bar(stat="identity") + geom_errorbar(width=.25) + 
  facet_wrap(~Variable) + ylab("Mean Value on Knowledge Measure") +
  geom_point(aes(y=max), col="white") + guides(fill=FALSE) + scale_fill_brewer(palette="Paired")
ggsave("../fig/meandiff_pres_1.pdf", width=4.75, height=2.5)



########
# determinants of political knowledge
########
# NOTE: control for wordsum?
# NOTE: rescale knowledge variables to unit variance?

m1 <- NULL
m1[[1]] <- lm(polknow_text_mean ~ female + polmedia + poldisc + educ + faminc + log(age) + relig + black + mode, data = data)
m1[[2]] <- lm(polknow_factual ~ female + polmedia + poldisc + educ + faminc + log(age) + relig + black + mode, data = data)
m1[[3]] <- lm(polknow_evalpre ~ female + polmedia + poldisc + educ + faminc + log(age) + relig + black + mode, data = data)
lapply(m1, summary)

dvnames <- c("Discursive\nSophistication","Factual\nKnowledge", "Interviewer\nEvaluation (Pre)")
ivnames <- c("Intercept", "Female", "Media", "Discussions", "College"
             , "Income", "log(Age)", "Church", "Black", "Online")


# prepare dataframe for plotting (sloppy code)
dfplot <- data.frame()
for(i in 1:length(m1)){
  tmp <- data.frame(summary(m1[[i]])$coefficients[,1:2])
  tmp$iv <- rownames(tmp)
  tmp$ivnames <- ivnames[1:nrow(tmp)]
  tmp$dv <- dvnames[i]
  rownames(tmp) <- NULL
  dfplot <- rbind(dfplot, tmp)
  rm(tmp)
}

# create factor variables, remove intercept for plotting
dfplot$ivnames <- factor(dfplot$ivnames, levels = rev(ivnames))
dfplot$dv <- factor(dfplot$dv, levels = dvnames)
dfplot <- dfplot[dfplot$ivnames!="Intercept",]

ggplot(dfplot, aes(y=ivnames, x=Estimate
                   , xmin = Estimate-1.96*Std..Error, xmax = Estimate+1.96*Std..Error)) + 
  geom_vline(xintercept = 0, color="grey") + xlab("Estimate") + ylab("Independent Variable") +
  geom_point(size=.75) + geom_errorbarh(height = 0) + facet_wrap(~dv,ncol=3) +
  plot_default + xlim(-0.1,0.32)
ggsave("../fig/determinants_3.pdf",width=4.75,height=3)

dfplot[dfplot$dv=="Discursive\nSophistication",c(1,2)] <- NA
ggplot(dfplot, aes(y=ivnames, x=Estimate
                   , xmin = Estimate-1.96*Std..Error, xmax = Estimate+1.96*Std..Error)) + 
  geom_vline(xintercept = 0, color="grey") + xlab("Estimate") + ylab("Independent Variable") +
  geom_point(size=.75) + geom_errorbarh(height = 0) + facet_wrap(~dv,ncol=3) +
  plot_default + xlim(-0.1,0.32)
ggsave("../fig/determinants_2.pdf",width=4.75,height=3)

dfplot[dfplot$dv=="Factual\nKnowledge",c(1,2)] <- NA
ggplot(dfplot, aes(y=ivnames, x=Estimate
                   , xmin = Estimate-1.96*Std..Error, xmax = Estimate+1.96*Std..Error)) + 
  geom_vline(xintercept = 0, color="grey") + xlab("Estimate") + ylab("Independent Variable") +
  geom_point(size=.75) + geom_errorbarh(height = 0) + facet_wrap(~dv,ncol=3) +
  plot_default + xlim(-0.1,0.32)
ggsave("../fig/determinants_1.pdf",width=4.75,height=3)

dfplot[dfplot$dv=="Interviewer\nEvaluation (Pre)",c(1,2)] <- NA
ggplot(dfplot, aes(y=ivnames, x=Estimate
                   , xmin = Estimate-1.96*Std..Error, xmax = Estimate+1.96*Std..Error)) + 
  geom_vline(xintercept = 0, color="grey") + xlab("Estimate") + ylab("Independent Variable") +
  geom_point(size=.75) + geom_errorbarh(height = 0) + facet_wrap(~dv,ncol=3) +
  plot_default + xlim(-0.1,0.32)
ggsave("../fig/determinants_0.pdf",width=4.75,height=3)


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

m2 <- lm(redist ~ tax * polknow_text + tax * polknow_factual + female + age + black + relig + educ + faminc + ideol_ct + pid_cont, data=data)
summary(m2)

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

