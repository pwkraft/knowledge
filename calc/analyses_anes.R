### ============================================================= ###
### Measuring Political Sophistication using Open-ended Responses ###
### Patrick Kraft                                                 ###
### ============================================================= ###
## main ANES analyses for paper + presentation (based on data created in prep.R)


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
library(MASS)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

setwd("/data/Dropbox/Uni/Projects/2016/knowledge/calc")

## load data and stm results
load("out/anes.Rdata")
#load("../data/anes_old.Rdata")

## QUESTION: remove wc=0 and spanish=1?

source("func.R")

## plot defaults
plot_default <- theme_classic(base_size=9) + theme(panel.border = element_rect(fill=NA))



## compare response behavior by gender
ggplot(anes2012, aes(wc, fill=factor(female))) + geom_bar(stat=mean)

ggplot(anes2012, aes(factor(female), y=as.numeric(wc!=0))) + 
  stat_summary_bin(fun.y = "mean", geom="bar")

ggplot(filter(anes2012, wc>0), aes(factor(female), y=wc)) + geom_point(alpha=.1, size=.1) + 
  stat_summary(fun.data = "mean_cl_boot", col="red")

ggplot(filter(anes2012, wc>0), aes(factor(female), y=wc)) + 
  stat_summary(fun.data = "mean_cl_boot", col="red")

table(anes2012$spanish==0)
table(anes2012$wc==0)


########
# correlation matrices: compare with common measures
########

datcor <- data[,c("polknow_text_mean","polknow_factual","polknow_evalpre")]
colnames(datcor) <- paste0("v",1:ncol(datcor))

pdf("../fig/corplot_pres.pdf",width=3.3, height=3.3)
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
        , columnLabels = c("Discursive\nSophistication","Factual\nKnowledge"
                           ,"Interviewer\nEvaluation (Pre)")) + plot_default
dev.off()

pdf("../fig/corplot_empty.pdf",width=3.3, height=3.3)
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
        , columnLabels = c("Discursive\nSophistication","Factual\nKnowledge"
                           ,"Interviewer\nEvaluation (Pre)")) +
  theme_classic(base_size=9) + theme(panel.border = element_rect(fill="white"))
dev.off()

datcor <- data[,c("ntopics","entropy","ditem")]
colnames(datcor) <- paste0("v",1:ncol(datcor))

pdf("../fig/corplot_components.pdf",width=3.3, height=3.3)
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
        , columnLabels = c("Considerations","Word Choice","Opinionation")) + plot_default
dev.off()

pdf("../fig/corplot_components0.pdf",width=3.3, height=3.3)
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
        , columnLabels = c("Considerations","Word Choice","Opinionation")) + 
  theme_classic(base_size=9) + theme(panel.border = element_rect(fill="white"))
dev.off()


## label definition
dvnames <- c("Discursive\nSophistication","Factual\nKnowledge")
ivnames <- c("Intercept", "Gender\n(Female)", "Media\nExposure", "Political\nDiscussions", "Education\n(College)"
             , "Income", "log(Age)", "Race\n(Black)", "Church\nAttendance", "Survey Mode\n(Online)")


########
# validation: effect on political engagement
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


res <- rbind(sim(m4a, iv=data.frame(polknow_text_mean=range(data$polknow_text_mean, na.rm = T)))
             , sim(m4b, iv=data.frame(polknow_factual=range(data$polknow_factual, na.rm = T))))
res$dvlab <- factor(res$dv, level = c("vote","part","effic_int","effic_ext")
                    , labels = c("Turnout","Non-conv. Participation"
                                 , "Internal Efficacy","External Efficacy"))
res$ivlab <- factor(res$iv, labels = dvnames)

ggplot(res, aes(y=ivlab, x=mean, xmin=cilo, xmax=cihi)) +
  geom_point() + geom_errorbarh(height=0) + facet_wrap(~dvlab, scale="free_x") +
  geom_vline(xintercept = 0, color="grey") +
  xlab("Marginal Effect") + ylab("Independent Variable") + plot_default +
  scale_y_discrete(limits = rev(levels(res$ivlab)))
ggsave("../fig/knoweff_pres.pdf", width=4, height=3)

ggplot(res, aes(y=ivlab, x=mean, xmin=cilo, xmax=cihi)) +
  geom_point() + geom_errorbarh(height=0) + facet_wrap(~dvlab, scale="free_x") +
  xlab("Marginal Effect") + ylab("Independent Variable") +
  theme_classic(base_size = 9) + theme(panel.border = element_rect(fill="white")) +
  geom_vline(xintercept = 0, color="grey") +
  scale_y_discrete(limits = rev(levels(res$ivlab)))
ggsave("../fig/knoweff_empty.pdf", width=4, height=3)


### Joint model controlling for both measures

m4c <- NULL
m4c[[1]] <- lm(effic_int ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode, data = data)
m4c[[2]] <- lm(effic_ext ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode, data = data)
m4c[[3]] <- lm(part ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode, data = data)
m4c[[4]] <- glm(vote ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode, data = data, family=binomial("logit"))

res <- rbind(sim(m4c, iv=data.frame(polknow_text_mean=range(data$polknow_text_mean, na.rm = T)))
             , sim(m4c, iv=data.frame(polknow_factual=range(data$polknow_factual, na.rm = T))))
res$dvlab <- factor(res$dv, level = c("vote","part","effic_int","effic_ext")
                    , labels = c("Turnout","Non-conv. Participation"
                                 , "Internal Efficacy","External Efficacy"))
res$ivlab <- factor(res$iv, labels = dvnames)

ggplot(res, aes(y=ivlab, x=mean, xmin=cilo, xmax=cihi)) +
  geom_point() + geom_errorbarh(height=0) + facet_wrap(~dvlab, scale="free_x") +
  geom_vline(xintercept = 0, color="grey") +
  xlab("Marginal Effect") + ylab("Independent Variable") + plot_default +
  scale_y_discrete(limits = rev(levels(res$ivlab)))
ggsave("../fig/knoweff_joint.pdf", width=4, height=3)


### Joint model controlling for both measures + wordsum index!

m4c <- NULL
m4c[[1]] <- lm(effic_int ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum, data = data)
m4c[[2]] <- lm(effic_ext ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum, data = data)
m4c[[3]] <- lm(part ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum, data = data)
m4c[[4]] <- glm(vote ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum, data = data, family=binomial("logit"))

res <- rbind(sim(m4c, iv=data.frame(polknow_text_mean=range(data$polknow_text_mean, na.rm = T)))
             , sim(m4c, iv=data.frame(polknow_factual=range(data$polknow_factual, na.rm = T))))
res$dvlab <- factor(res$dv, level = c("vote","part","effic_int","effic_ext")
                    , labels = c("Turnout","Non-conv. Participation"
                                 , "Internal Efficacy","External Efficacy"))
res$ivlab <- factor(res$iv, labels = dvnames)

ggplot(res, aes(y=ivlab, x=mean, xmin=cilo, xmax=cihi)) +
  geom_point() + geom_errorbarh(height=0) + facet_wrap(~dvlab, scale="free_x") +
  geom_vline(xintercept = 0, color="grey") +
  xlab("Marginal Effect") + ylab("Independent Variable") + plot_default +
  scale_y_discrete(limits = rev(levels(res$ivlab)))
ggsave("../fig/knoweff_wordsum.pdf", width=4, height=3)


### Joint model controlling for both measures + wordsum index!

m4c <- NULL
m4c[[1]] <- lm(effic_int ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + lwc, data = data)
m4c[[2]] <- lm(effic_ext ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + lwc, data = data)
m4c[[3]] <- lm(part ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + lwc, data = data)
m4c[[4]] <- glm(vote ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + lwc, data = data, family=binomial("logit"))

res <- rbind(sim(m4c, iv=data.frame(polknow_text_mean=range(data$polknow_text_mean, na.rm = T)))
             , sim(m4c, iv=data.frame(polknow_factual=range(data$polknow_factual, na.rm = T))))
res$dvlab <- factor(res$dv, level = c("vote","part","effic_int","effic_ext")
                    , labels = c("Turnout","Non-conv. Participation"
                                 , "Internal Efficacy","External Efficacy"))
res$ivlab <- factor(res$iv, labels = dvnames)

ggplot(res, aes(y=ivlab, x=mean, xmin=cilo, xmax=cihi)) +
  geom_point() + geom_errorbarh(height=0) + facet_wrap(~dvlab, scale="free_x") +
  geom_vline(xintercept = 0, color="grey") +
  xlab("Marginal Effect") + ylab("Independent Variable") + plot_default +
  scale_y_discrete(limits = rev(levels(res$ivlab)))
ggsave("../fig/knoweff_lwc.pdf", width=4, height=3)



### Joint model controlling for extraversion and being reserved!

m4c <- NULL
m4c[[1]] <- lm(effic_int ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + extraversion + reserved, data = data)
m4c[[2]] <- lm(effic_ext ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + extraversion + reserved, data = data)
m4c[[3]] <- lm(part ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + extraversion + reserved, data = data)
m4c[[4]] <- glm(vote ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + extraversion + reserved, data = data, family=binomial("logit"))

res <- rbind(sim(m4c, iv=data.frame(polknow_text_mean=range(data$polknow_text_mean, na.rm = T)))
             , sim(m4c, iv=data.frame(polknow_factual=range(data$polknow_factual, na.rm = T))))
res$dvlab <- factor(res$dv, level = c("vote","part","effic_int","effic_ext")
                    , labels = c("Turnout","Non-conv. Participation"
                                 , "Internal Efficacy","External Efficacy"))
res$ivlab <- factor(res$iv, labels = dvnames)

ggplot(res, aes(y=ivlab, x=mean, xmin=cilo, xmax=cihi)) +
  geom_point() + geom_errorbarh(height=0) + facet_wrap(~dvlab, scale="free_x") +
  geom_vline(xintercept = 0, color="grey") +
  xlab("Marginal Effect") + ylab("Independent Variable") + plot_default +
  scale_y_discrete(limits = rev(levels(res$ivlab)))
ggsave("../fig/knoweff_personality.pdf", width=4, height=3)


########
# validation: party/candidate placement precision
########

#hetreg_summary <- filter(hetreg_summary, policy!="ideol")
hetreg_summary$policy <- factor(hetreg_summary$policy
                                , labels = c("Ideology","Government\nSpending","Defense\nSpending"
                                             ,"Insurance\nPolicy","Job\nGuarantee"))
hetreg_summary$measure <- factor(hetreg_summary$measure, levels = rev(levels(hetreg_summary$measure))
                                 , labels = c("Factual\nKnowledge", "Discursive\nSophistication"))
hetreg_summary$target <- factor(hetreg_summary$target
                                , labels = c("Mitt\nRomney","Barack\nObama"
                                             ,"Republican\nParty","Democratic\nParty"))

ggplot(hetreg_summary, aes(y=measure, x=mean, xmin=cilo, xmax=cihi)) +
  geom_point() + geom_errorbarh(height=0) + facet_grid(policy~target) +
  geom_vline(xintercept = 0, color="grey") +
  xlab("Error Variance Reduction") + ylab("Independent Variable (Max - Min)") + plot_default
ggsave("../fig/hetreg.pdf",width = 6, height = 4)

hetreg_summary %>% filter(policy=="Ideology") %>%
  ggplot(aes(y=measure, x=mean, xmin=cilo, xmax=cihi)) +
  geom_point() + geom_errorbarh(height=0) + facet_wrap(~target, ncol=2) +
  geom_vline(xintercept = 0, color="grey") +
  xlab("Error Variance Reduction (Max - Min)") + ylab("Independent Variable") + plot_default
ggsave("../fig/hetreg_pres.pdf",width = 4, height = 3)

hetreg_summary %>% filter(policy=="Ideology") %>%
  ggplot(aes(y=measure, x=mean, xmin=cilo, xmax=cihi)) + plot_default +
  geom_point() + geom_errorbarh(height=0) + facet_wrap(~target, ncol=2) +
  geom_vline(xintercept = 0, color="grey") + theme(panel.border = element_rect(fill="white")) +
  xlab("Error Variance Reduction (Max - Min)") + ylab("Independent Variable")
ggsave("../fig/hetreg_empty.pdf",width = 4, height = 3)


#########
### validation: pre-post consistency
#########

m5a <- glm(vc_change ~ polknow_text_mean + female + educ + faminc + log(age) + black + relig + mode, data = data, family=binomial("logit"))
m5b <- glm(vc_change ~ polknow_factual + female + educ + faminc + log(age) + black + relig + mode, data = data, family=binomial("logit"))

summary(glm(vc_change ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode, data = data, family=binomial("logit")))

res <- rbind(sim(m5a, iv=data.frame(polknow_text_mean=range(data$polknow_text_mean, na.rm = T)))
             , sim(m5b, iv=data.frame(polknow_factual=range(data$polknow_factual, na.rm = T))))
res$ivlab <- factor(res$iv, labels = dvnames)

ggplot(res, aes(y=ivlab, x=mean, xmin=cilo, xmax=cihi)) +
  geom_point() + geom_errorbarh(height=0) +
  geom_vline(xintercept = 0, color="grey") +
  xlab("Marginal Effect") + ylab("Independent Variable") + plot_default +
  scale_y_discrete(limits = rev(levels(res$ivlab)))
ggsave("../fig/prepost.pdf",width = 3, height = 2)



res <- rbind(data.frame(sim(m5a, iv=data.frame(polknow_text_mean=seq(min(data$polknow_text_mean),max(data$polknow_text_mean),length=10)))
                        , value=seq(min(data$polknow_text_mean),max(data$polknow_text_mean),length=10),Variable="Discursive Sophistication")
             , data.frame(sim(m5b, iv=data.frame(polknow_factual=seq(0, 1,length=10)))
                          , value=seq(0, 1,length=10),Variable="Factual Knowledge"))
res$ivlab <- factor(res$iv, labels = dvnames)

ggplot(res, aes(x=value, y=mean, ymin=cilo,ymax=cihi)) + plot_default +
  #geom_errorbar(alpha=.5, width=0) +
  geom_ribbon(alpha=0.5, lwd=.1, fill="blue") + geom_line() +
  facet_grid(~Variable, scales="free_x") +
  ylab("Expected Probability\nto Keep Vote Choice") + xlab("Value of independent variable")
ggsave("../fig/prepost_exp.pdf",width=4,height=2)

ggplot(res, aes(x=value, y=mean, ymin=cilo,ymax=cihi)) + plot_default +
  #geom_errorbar(alpha=.5, width=0) +
  geom_ribbon(alpha=0.5, lwd=.1, fill="blue") + geom_line() +
  facet_grid(~Variable, scales="free_x") + xlab("Value of independent variable") +
  ylab("Expected Probability\nto Keep Vote Choice") + theme(panel.border = element_rect(fill="white"))
ggsave("../fig/prepost_empty.pdf",width=4,height=2)


###################
### Additional information: STM summaries
###################

pdf("../fig/stm_labels.pdf")
plot(stm_fit, "labels", topics=c(16,14,10,8), main="Sample Topics (2012 ANES)")
dev.off()

pdf("../fig/stm_prop.pdf")
plot(stm_fit)
dev.off()


########
# topic differences b/w men and women
########

prep <- estimateEffect(~ age + educ_cont + pid_cont + educ_pid + female
                       , stm_fit, meta = out$meta, uncertainty = "Global")

summary(stm_fit)
topics <- c("1: Compromise","2: Romney","3: Morality/Religion","4: Obama","5: Taxes"
            ,"6: Inequality","7: Social Security","8: Middle class","9: Immigration","10: Government Debt"
            ,"11: Abortion","12: Economic Policy","13: Foreign Policy","14: Evaluation/Sentiment","15: Values"
            ,"16: Presidential Performance","17: Patriotism","18: Health Care","19: Miscellaneous","20: Parties")

pdf("fig/stm_gender.pdf")
plot.estimateEffect(prep, covariate = "female", topics = 1:20, model = stm_fit
                    , xlim = c(-.1,.05), method = "difference", cov.value1 = 1, cov.value2 = 0
                    , main = "Gender Differences in Topic Proportions"
                    , labeltype = "custom", custom.labels = topics)
dev.off()