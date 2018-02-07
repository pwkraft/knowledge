### ============================================================= ###
### Measuring Political Sophistication using Open-ended Responses ###
### Patrick Kraft                                                 ###
### ============================================================= ###
## main ANES analyses for paper + presentation (based on data created in prep.R)


rm(list = ls())
gc()

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
load("out/anes2012.Rdata")
load("out/anes2016.Rdata")

## QUESTION: remove wc=0 and spanish=1?

source("func.R")

## plot defaults
plot_default <- theme_classic(base_size=9) + theme(panel.border = element_rect(fill=NA))

## compare response behavior by gender
ggplot(anes2012, aes(factor(female), y=as.numeric(wc!=0))) + 
  stat_summary_bin(fun.y = "mean", geom="bar")

ggplot(filter(anes2012, wc>0), aes(factor(female), y=wc)) + geom_point(alpha=.1, size=.1) + 
  stat_summary(fun.data = "mean_cl_boot", col="red")

ggplot(filter(anes2012, wc>0), aes(factor(female), y=wc)) + 
  stat_summary(fun.data = "mean_cl_boot", col="red")

table(anes2012$spanish==0)
table(anes2012$wc!=0)


########
# correlation matrices: compare with common measures
########

## 2012 ANES
datcor <- data2012[,c("polknow_text_mean","polknow_factual","polknow_evalpre")]
colnames(datcor) <- paste0("v",1:ncol(datcor))
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
              , columnLabels = c("Discursive\nSophistication","Factual\nKnowledge"
                                 ,"Interviewer\nEvaluation (Pre)")) + plot_default
ggsave("../fig/corplot_pres2012.pdf",width=3.2, height=3.2)

## 2012 ANES (empty)
pdf("../fig/corplot_empty.pdf",width=3.2, height=3.2)
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
        , columnLabels = c("Discursive\nSophistication","Factual\nKnowledge"
                           ,"Interviewer\nEvaluation (Pre)")) +
  theme_classic(base_size=9) + theme(panel.border = element_rect(fill="white"))
dev.off()

## 2016 ANES
datcor <- data2016[,c("polknow_text_mean","polknow_factual","polknow_evalpre")]
colnames(datcor) <- paste0("v",1:ncol(datcor))
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
              , columnLabels = c("Discursive\nSophistication","Factual\nKnowledge"
                                 ,"Interviewer\nEvaluation (Pre)")) + plot_default
ggsave("../fig/corplot_pres2016.pdf",width=3.2, height=3.2)


## ANES 2012 components
datcor <- data2012[,c("ntopics","distinct","ditem")]
colnames(datcor) <- paste0("v",1:ncol(datcor))
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
        , columnLabels = c("Considerations","Word Choice","Opinionation")) + plot_default
ggsave("../fig/corplot_components2012.pdf",width=3.3, height=3.3)

## ANES 2012 components (empty)
pdf("../fig/corplot_components0.pdf",width=3.3, height=3.3)
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
        , columnLabels = c("Considerations","Word Choice","Opinionation")) + 
  theme_classic(base_size=9) + theme(panel.border = element_rect(fill="white"))
dev.off()

## ANES 2016 components
datcor <- data2016[,c("ntopics","distinct","ditem")]
colnames(datcor) <- paste0("v",1:ncol(datcor))
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
        , columnLabels = c("Considerations","Word Choice","Opinionation")) + plot_default
ggsave("../fig/corplot_components2016.pdf",width=3.3, height=3.3)



## label definition
dvnames <- c("Discursive\nSophistication","Factual\nKnowledge")
ivnames <- c("Intercept", "Gender\n(Female)", "Media\nExposure", "Political\nDiscussions", "Education\n(College)"
             , "Income", "log(Age)", "Race\n(Black)", "Church\nAttendance", "Survey Mode\n(Online)")

#########
# Validation: compare number of topics
#########

p1 <- ggplot(data2012, aes(x=polknow_text_mean, y=polknow_text_mean_full)) +
  geom_point(alpha=.05) + geom_smooth(method="lm") +  ggtitle("2012 ANES") +
  xlab("Discursive Sophistication\n(20 Topics)") + ylab("Discursive Sophistication\n(40 Topics)") +
  annotate("text", x=0.1, y=max(data2012$polknow_text_mean_full), size=2
           , label = paste0("r = ",round(cor(data2012$polknow_text_mean, data2012$polknow_text_mean_full), 2))) +
  theme_classic(base_size=8) + theme(panel.border = element_rect(fill=NA))

p2 <- ggplot(data2016, aes(x=polknow_text_mean, y=polknow_text_mean_full)) +
  geom_point(alpha=.05) + geom_smooth(method="lm") + ggtitle("2016 ANES") +
  xlab("Discursive Sophistication\n(20 Topics)") + ylab("Discursive Sophistication\n(40 Topics)") +
  annotate("text", x=.1, y=max(data2016$polknow_text_mean_full), size=2
           , label = paste0("r = ",round(cor(data2016$polknow_text_mean, data2016$polknow_text_mean_full), 2))) +
  theme_classic(base_size=8) + theme(panel.border = element_rect(fill=NA))

(p0 <- grid.arrange(p1, p2, ncol=2))
ggsave("../fig/ktopic.pdf", p0, width=6, height=3)



########
# validation: effect on political engagement
########

### Joint model controlling for both measures + wordsum index

m4a <- m4b <- NULL
m4a[[1]] <- lm(effic_int ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum, data = data2012)
m4a[[2]] <- lm(effic_ext ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum, data = data2012)
m4a[[3]] <- lm(part ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum, data = data2012)
m4a[[4]] <- glm(vote ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum, data = data2012, family=binomial("logit"))
m4b[[1]] <- lm(effic_int ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum, data = data2016)
m4b[[2]] <- lm(effic_ext ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum, data = data2016)
m4b[[3]] <- lm(part ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum, data = data2016)
m4b[[4]] <- glm(vote ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum, data = data2016, family=binomial("logit"))

res <- rbind(sim(m4a, iv=data.frame(polknow_text_mean=sdrange(data2012$polknow_text_mean)))
             , sim(m4a, iv=data.frame(polknow_factual=sdrange(data2012$polknow_factual)))
             , sim(m4b, iv=data.frame(polknow_text_mean=sdrange(data2016$polknow_text_mean)))
             , sim(m4b, iv=data.frame(polknow_factual=sdrange(data2016$polknow_factual))))
res$dvlab <- factor(res$dv, level = c("vote","part","effic_int","effic_ext")
                    , labels = c("Turnout","Non-conv. Participation"
                                 , "Internal Efficacy","External Efficacy"))
res$ivlab <- factor(res$iv, labels = dvnames)
res$Year <- rep(c("2012 ANES","2016 ANES"), each=8)

ggplot(res, aes(y=ivlab, x=mean, xmin=cilo, xmax=cihi)) +
  geom_point() + geom_errorbarh(height=0) + facet_grid(Year~dvlab, scale="free_x") +
  geom_vline(xintercept = 0, color="grey") +
  xlab("Marginal Effect (-/+ 1 SD)") + ylab("Independent Variable") + plot_default +
  scale_y_discrete(limits = rev(levels(res$ivlab))) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("../fig/knoweff_pres.pdf", width=6.5, height=2.2)


### Robustness check: Joint model controlling for both measures + word count

m4a <- m4b <- NULL
m4a[[1]] <- lm(effic_int ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + lwc, data = data2012)
m4a[[2]] <- lm(effic_ext ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + lwc, data = data2012)
m4a[[3]] <- lm(part ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + lwc, data = data2012)
m4a[[4]] <- glm(vote ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + lwc, data = data2012, family=binomial("logit"))
m4b[[1]] <- lm(effic_int ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + lwc, data = data2016)
m4b[[2]] <- lm(effic_ext ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + lwc, data = data2016)
m4b[[3]] <- lm(part ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + lwc, data = data2016)
m4b[[4]] <- glm(vote ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + lwc, data = data2016, family=binomial("logit"))

res <- rbind(sim(m4a, iv=data.frame(polknow_text_mean=sdrange(data2012$polknow_text_mean)))
             , sim(m4a, iv=data.frame(polknow_factual=sdrange(data2012$polknow_factual)))
             , sim(m4b, iv=data.frame(polknow_text_mean=sdrange(data2016$polknow_text_mean)))
             , sim(m4b, iv=data.frame(polknow_factual=sdrange(data2016$polknow_factual))))
res$dvlab <- factor(res$dv, level = c("vote","part","effic_int","effic_ext")
                    , labels = c("Turnout","Non-conv. Participation"
                                 , "Internal Efficacy","External Efficacy"))
res$ivlab <- factor(res$iv, labels = dvnames)
res$Year <- rep(c("2012 ANES","2016 ANES"), each=8)

ggplot(res, aes(y=ivlab, x=mean, xmin=cilo, xmax=cihi)) +
  geom_point() + geom_errorbarh(height=0) + facet_grid(Year~dvlab, scale="free_x") +
  geom_vline(xintercept = 0, color="grey") +
  xlab("Marginal Effect (-/+ 1 SD)") + ylab("Independent Variable") + plot_default +
  scale_y_discrete(limits = rev(levels(res$ivlab))) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("../fig/knoweff_lwc.pdf", width=6.5, height=2.2)


### Robustness check: Joint model controlling for personality (extraversion and being reserved)

m4a <- m4b <- NULL
m4a[[1]] <- lm(effic_int ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + extraversion + reserved, data = data2012)
m4a[[2]] <- lm(effic_ext ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + extraversion + reserved, data = data2012)
m4a[[3]] <- lm(part ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + extraversion + reserved, data = data2012)
m4a[[4]] <- glm(vote ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + extraversion + reserved, data = data2012, family=binomial("logit"))
m4b[[1]] <- lm(effic_int ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + extraversion + reserved, data = data2016)
m4b[[2]] <- lm(effic_ext ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + extraversion + reserved, data = data2016)
m4b[[3]] <- lm(part ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + extraversion + reserved, data = data2016)
m4b[[4]] <- glm(vote ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + extraversion + reserved, data = data2016, family=binomial("logit"))

res <- rbind(sim(m4a, iv=data.frame(polknow_text_mean=sdrange(data2012$polknow_text_mean)))
             , sim(m4a, iv=data.frame(polknow_factual=sdrange(data2012$polknow_factual)))
             , sim(m4b, iv=data.frame(polknow_text_mean=sdrange(data2016$polknow_text_mean)))
             , sim(m4b, iv=data.frame(polknow_factual=sdrange(data2016$polknow_factual))))
res$dvlab <- factor(res$dv, level = c("vote","part","effic_int","effic_ext")
                    , labels = c("Turnout","Non-conv. Participation"
                                 , "Internal Efficacy","External Efficacy"))
res$ivlab <- factor(res$iv, labels = dvnames)
res$Year <- rep(c("2012 ANES","2016 ANES"), each=8)

ggplot(res, aes(y=ivlab, x=mean, xmin=cilo, xmax=cihi)) +
  geom_point() + geom_errorbarh(height=0) + facet_grid(Year~dvlab, scale="free_x") +
  geom_vline(xintercept = 0, color="grey") +
  xlab("Marginal Effect (-/+ 1 SD)") + ylab("Independent Variable") + plot_default +
  scale_y_discrete(limits = rev(levels(res$ivlab))) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("../fig/knoweff_personality.pdf", width=6.5, height=2.2)



########
# validation: party/candidate placement precision
########

#hetreg_summary2012 <- filter(hetreg_summary2012, policy!="ideol")
hetreg_summary <- rbind(hetreg_summary2012, mutate(hetreg_summary2016, target=paste0("x", target))) %>%
  filter(!target %in% c("rep", "dem"))
hetreg_summary$policy <- factor(hetreg_summary$policy
                                , labels = c("Ideology","Government\nSpending","Defense\nSpending"
                                             ,"Insurance\nPolicy","Job\nGuarantee"))
hetreg_summary$measure <- factor(hetreg_summary$measure, levels = rev(levels(hetreg_summary$measure))
                                 , labels = c("Factual\nKnowledge", "Discursive\nSophistication"))
hetreg_summary$target <- factor(hetreg_summary$target
                                , labels = c("Mitt\nRomney","Barack\nObama"
                                             ,"Donald\nTrump","Hillary\nClinton"))

ggplot(hetreg_summary, aes(y=measure, x=mean, xmin=cilo, xmax=cihi)) +
  geom_point() + geom_errorbarh(height=0) + facet_grid(target~policy) +
  geom_vline(xintercept = 0, color="grey") +
  xlab("Error Variance Reduction (-/+ 1 SD)") + ylab("Independent Variable") + 
  plot_default + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("../fig/hetreg.pdf",width = 6.5, height = 3.3)

hetreg_summary %>% filter(policy=="Ideology") %>%
  ggplot(aes(y=measure, x=mean, xmin=cilo, xmax=cihi)) +
  geom_point() + geom_errorbarh(height=0) + facet_wrap(~target, ncol=2) +
  geom_vline(xintercept = 0, color="grey") +
  xlab("Error Variance Reduction (-/+ 1 SD)") + ylab("Independent Variable") + plot_default
ggsave("../fig/hetreg_pres.pdf",width = 4, height = 3)



#########
### validation: pre-post consistency
#########

m5a <- glm(vc_change ~ polknow_text_mean + female + educ + faminc + log(age) + black + relig + mode + wordsum, data = data2012, family=binomial("logit"))
m5b <- glm(vc_change ~ polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum, data = data2012, family=binomial("logit"))
m5c <- glm(vc_change ~ polknow_text_mean + female + educ + faminc + log(age) + black + relig + mode + wordsum, data = data2016, family=binomial("logit"))
m5d <- glm(vc_change ~ polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum, data = data2016, family=binomial("logit"))

res <- rbind(data.frame(sim(m5a, iv=data.frame(polknow_text_mean=seq(min(data2012$polknow_text_mean),max(data2012$polknow_text_mean),length=10)))
                        , value=seq(min(data2012$polknow_text_mean),max(data2012$polknow_text_mean),length=10),Variable="Discursive Sophistication")
             , data.frame(sim(m5b, iv=data.frame(polknow_factual=seq(0, 1,length=10)))
                          , value=seq(0, 1,length=10),Variable="Factual Knowledge")
             , data.frame(sim(m5c, iv=data.frame(polknow_text_mean=seq(min(data2016$polknow_text_mean),max(data2016$polknow_text_mean),length=10)))
                          , value=seq(min(data2016$polknow_text_mean),max(data2016$polknow_text_mean),length=10),Variable="Discursive Sophistication")
             , data.frame(sim(m5d, iv=data.frame(polknow_factual=seq(0, 1,length=10)))
                          , value=seq(0, 1,length=10),Variable="Factual Knowledge"))
res$ivlab <- factor(res$iv, labels = dvnames)
res$Year <- rep(c("2012 ANES","2016 ANES"), each=20)

ggplot(res, aes(x=value, y=mean, ymin=cilo,ymax=cihi, fill=Variable, lty=Variable)) + plot_default +
  geom_ribbon(alpha=0.4, lwd=.1) + geom_line() +
  facet_grid(~Year, scales="free_x") +
  ylab("Expected Probability\nto Keep Vote Choice") + xlab("Value of Independent Variable")
ggsave("../fig/prepost_exp.pdf",width=5,height=2)


###################
### Additional information: STM summaries
###################

summary(stm_fit2012)
summary(stm_fit2016)
topics2012 <- c("1: Compromise","2: Romney","3: Morality/Religion","4: Obama","5: Taxes"
                ,"6: Inequality","7: Social Security","8: Middle class","9: Immigration","10: Government Debt"
                ,"11: Abortion","12: Economic Policy","13: Foreign Policy","14: Evaluation/Sentiment","15: Values"
                ,"16: Presidential Performance","17: Patriotism","18: Health Care","19: Miscellaneous","20: Parties")
topics2016 <- c("1: Compromise","2: Romney","3: Morality/Religion","4: Obama","5: Taxes"
                ,"6: Inequality","7: Social Security","8: Middle class","9: Immigration","10: Government Debt"
                ,"11: Abortion","12: Economic Policy","13: Foreign Policy","14: Evaluation/Sentiment","15: Values"
                ,"16: Presidential Performance","17: Patriotism","18: Health Care","19: Miscellaneous","20: Parties")


pdf("../fig/stm_prop.pdf")
par(mfrow=c(1,2), mar=c(4.2,0.5,2.5,0.5))
plot(stm_fit2012, main="2012 ANES")
plot(stm_fit2016, main="2016 ANES")
dev.off()

pdf("../fig/stm_labels.pdf")
par(mfrow=c(1,2), mar=c(0.5,0.5,2.5,0.5))
plot(stm_fit2012, "labels", topics=c(16,14,10,8), main="Sample Topics (2012 ANES)")
plot(stm_fit2016, "labels", topics=c(1,10,19,17), main="Sample Topics (2016 ANES)")
dev.off()

########
# topic differences b/w men and women
########

prep2012 <- estimateEffect(~ age + educ_cont + pid_cont + educ_pid + female
                           , stm_fit2012, meta = out2012$meta, uncertainty = "Global")
prep2016 <- estimateEffect(~ age + educ_cont + pid_cont + educ_pid + female
                           , stm_fit2016, meta = out2016$meta, uncertainty = "Global")

pdf("../fig/stm_gender.pdf")
par(mfrow=c(1,2), mar=c(2.2,0.5,2.2,0.5))
plot.estimateEffect(prep2012, covariate = "female", topics = 1:20, model = stm_fit2012
                    , xlim = c(-.1,.05), method = "difference", cov.value1 = 1, cov.value2 = 0
                    #, labeltype = "custom", custom.labels = topics
                    , main = "Gender Differences in Topic Proportions")
plot.estimateEffect(prep2016, covariate = "female", topics = 1:20, model = stm_fit2016
                    , xlim = c(-.1,.05), method = "difference", cov.value1 = 1, cov.value2 = 0
                    #, labeltype = "custom", custom.labels = topics
                    , main = "Gender Differences in Topic Proportions")
dev.off()