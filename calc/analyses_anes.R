### ============================================================= ###
### Measuring Political Sophistication using Open-ended Responses ###
### Patrick Kraft                                                 ###
### ============================================================= ###
## main ANES analyses for paper + presentation (based on data created in prep.R)


rm(list = ls())
gc()

library(MASS)
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
plot_empty <- theme_classic(base_size=9) + theme(panel.border = element_rect(fill="white"))


########
# correlation matrices: compare with common measures
########

## 2012 ANES
datcor <- data2012[,c("polknow_text_mean","polknow_factual","polknow_evalpre")]
colnames(datcor) <- paste0("v",1:ncol(datcor))
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
              , columnLabels = c("Discursive\nSophistication","Factual\nKnowledge"
                                 ,"Interviewer\nEvaluation (Pre)")) + plot_default
ggsave("../fig/anes2012_corplot.pdf",width=3.2, height=3.2)

## 2016 ANES
datcor <- data2016[,c("polknow_text_mean","polknow_factual","polknow_evalpre")]
colnames(datcor) <- paste0("v",1:ncol(datcor))
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
              , columnLabels = c("Discursive\nSophistication","Factual\nKnowledge"
                                 ,"Interviewer\nEvaluation (Pre)")) + plot_default
ggsave("../fig/anes2016_corplot.pdf",width=3.2, height=3.2)


## ANES 2012 components
datcor <- data2012[,c("ntopics","distinct","ditem")]
colnames(datcor) <- paste0("v",1:ncol(datcor))
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
        , columnLabels = c("Considerations","Word Choice","Opinionation")) + plot_default
ggsave("../fig/anes2012_corplot_components.pdf",width=3.2, height=3.2)

## ANES 2016 components
datcor <- data2016[,c("ntopics","distinct","ditem")]
colnames(datcor) <- paste0("v",1:ncol(datcor))
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
        , columnLabels = c("Considerations","Word Choice","Opinionation")) + plot_default
ggsave("../fig/anes2016_corplot_components.pdf",width=3.2, height=3.2)



## label definition
dvnames <- c("Discursive\nSophistication","Factual\nKnowledge")
ivnames <- c("Intercept", "Gender\n(Female)", "Media\nExposure", "Political\nDiscussions", "Education\n(College)"
             , "Income", "log(Age)", "Race\n(Black)", "Church\nAttendance", "Survey Mode\n(Online)")


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

ggplot(res, aes(y=ivlab, x=mean, xmin=cilo, xmax=cihi)) + geom_vline(xintercept = 0, color="grey") +
  geom_point() + geom_errorbarh(height=0) + facet_grid(Year~dvlab, scale="free_x") +
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

ggplot(res, aes(y=ivlab, x=mean, xmin=cilo, xmax=cihi)) + geom_vline(xintercept = 0, color="grey") +
  geom_point() + geom_errorbarh(height=0) + facet_grid(Year~dvlab, scale="free_x") +
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

ggplot(res, aes(y=ivlab, x=mean, xmin=cilo, xmax=cihi)) + geom_vline(xintercept = 0, color="grey") +
  geom_point() + geom_errorbarh(height=0) + facet_grid(Year~dvlab, scale="free_x") +
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
hetreg_summary$Year <- rep(c("2012 ANES","2016 ANES"), each=nrow(hetreg_summary)/2)

ggplot(hetreg_summary, aes(y=measure, x=mean, xmin=cilo, xmax=cihi)) + geom_vline(xintercept = 0, color="grey") +
  geom_point() + geom_errorbarh(height=0) + facet_grid(Year+target~policy) +
  xlab("Error Variance Reduction (-/+ 1 SD)") + ylab("Independent Variable") + 
  plot_default + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("../fig/hetreg.pdf",width = 6.5, height = 3.5)

hetreg_summary %>% filter(policy=="Ideology") %>%
  ggplot(aes(y=measure, x=mean, xmin=cilo, xmax=cihi)) + geom_vline(xintercept = 0, color="grey") +
  geom_point() + geom_errorbarh(height=0) + facet_wrap(~target, ncol=2) +
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
### Example Responses
###################

tmp  <- data2012 %>% 
  filter(wc > (median(wc) - 5) & wc < (median(wc) + 5)
         , polknow_factual == 0.6) %>%
  arrange(polknow_text_mean)
tmp_select <- c(head(tmp$caseid), tail(tmp$caseid))
tmp <- anes2012opend %>% 
  filter(caseid %in% tmp_select) %>%
  mutate(polknow_text_mean = data2012$polknow_text_mean[data2012$caseid %in% tmp_select]) %>%
  arrange(polknow_text_mean)
write.csv(t(tmp), file="tmp/select2012.csv")

tmp  <- data2016 %>% 
  filter(wc > (median(wc) - 5) & wc < (median(wc) + 5)
         , polknow_factual == 0.75) %>%
  arrange(polknow_text_mean)
tmp_select <- c(head(tmp$caseid), tail(tmp$caseid))
tmp <- anes2016opend %>% 
  filter(V160001 %in% tmp_select) %>%
  mutate(polknow_text_mean = data2016$polknow_text_mean[data2016$caseid %in% tmp_select]) %>%
  arrange(polknow_text_mean)
write.csv(t(tmp), file="tmp/select2016.csv")

data2016 %>% 
  filter(caseid %in% tmp_select) %>% 
  arrange(polknow_text_mean) %>%
  dplyr::select(caseid, pid, ideol, educ, vc_pre, vc_post, vc_change, polknow_text_mean, polknow_factual)


###################
### Additional information: STM summaries
###################

summary(stm_fit2012)
summary(stm_fit2016)

pdf("../fig/anes_stm_prop.pdf", width=12, height=10)
par(mfrow=c(1,2), mar=c(4.2,0.5,2.5,0.5))
plot(stm_fit2012
     , main=paste0("2012 ANES (k = ",stm_fit2012$settings$dim$K,")",collapse = "")
     , n=5, labeltype = "prob", text.cex = 1)
plot(stm_fit2016
     , main=paste0("2016 ANES (k = ",stm_fit2016$settings$dim$K,")",collapse = "")
     , n=5, labeltype = "prob", text.cex = 1)
dev.off()

pdf("../fig/stm_labels.pdf")
par(mfrow=c(1,2), mar=c(0.5,0.5,2.5,0.5))
plot(stm_fit2012, "labels", topics=c(16,14,10,8), main="Sample Topics (2012 ANES)")
plot(stm_fit2016, "labels", topics=c(1,10,19,17), main="Sample Topics (2016 ANES)")
dev.off()

