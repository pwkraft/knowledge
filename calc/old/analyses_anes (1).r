### ============================================================= ###
### Measuring Political Sophistication using Open-ended Responses ###
### Patrick Kraft                                                 ###
### ============================================================= ###
## main ANES analyses for paper + presentation (based on data created in prep.R)


rm(list = ls())
gc()

library(MASS)
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
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#setwd("/data/Dropbox/Uni/projects/2016/knowledge/calc")

## load data and stm results
load("calc/out/anes2012.Rdata")
load("calc/out/anes2016.Rdata")
load("calc/out/anes2020.Rdata")
load("calc/out/cces.Rdata")

## QUESTION: remove wc=0 and spanish=1?

source("calc/func.R")

## plot defaults
plot_default <- theme_classic(base_size=9) + theme(panel.border = element_rect(fill=NA))
plot_empty <- theme_classic(base_size=9) + theme(panel.border = element_rect(fill="white"))


### Check more comprehensive sophistication measure instead of pure factual knowledge
#data2012$polknow_factual <- with(data2012, polknow_factual + educ + faminc)/3
#data2016$polknow_factual <- with(data2016, polknow_factual + educ + faminc)/3

########
# correlation matrices: compare with common measures
########

## 2012 ANES
datcor <- data2012[,c("polknow_text_mean","polknow_factual","polknow_evalpre")]
colnames(datcor) <- paste0("v",1:ncol(datcor))
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
              , columnLabels = c("Discursive\nSophistication","Factual\nKnowledge"
                                 ,"Interviewer\nEvaluation")) + plot_default
ggsave("fig/anes2012_corplot.pdf",width=3.2, height=3.2)

## 2016 ANES
datcor <- data2016[,c("polknow_text_mean","polknow_factual","polknow_evalpre")]
colnames(datcor) <- paste0("v",1:ncol(datcor))
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
              , columnLabels = c("Discursive\nSophistication","Factual\nKnowledge"
                                 ,"Interviewer\nEvaluation")) + plot_default
ggsave("fig/anes2016_corplot.pdf",width=3.2, height=3.2)

## 2016 ANES poster version
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
        , columnLabels = c("Discursive\nSophistication","Factual\nKnowledge"
                           ,"Interviewer\nEvaluation")) + plot_default
#ggsave("fig/anes2016_corplot.pdf",width=4, height=4)

## 2020 ANES
datcor <- data2020[,c("polknow_text_mean","polknow_factual")]
colnames(datcor) <- paste0("v",1:ncol(datcor))
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
        , columnLabels = c("Discursive\nSophistication","Factual\nKnowledge")) + plot_default
ggsave("fig/anes2020_corplot.pdf",width=3.2, height=3.2)


## ANES 2012 components
datcor <- data2012[,c("considerations","consistency","wordchoice")]
colnames(datcor) <- paste0("v",1:ncol(datcor))
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
        , columnLabels = c("Considerations","Consistency","Word Choice")) + plot_default
ggsave("fig/anes2012_corplot_components.pdf",width=2.6, height=2.6)

## empty plot
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
        , columnLabels = c("Considerations","Consistency","Word Choice")) + plot_empty
ggsave("fig/anes2012_corplot_components0.pdf",width=2.6, height=2.6)

## ANES 2016 components
datcor <- data2016[,c("considerations","consistency","wordchoice")]
colnames(datcor) <- paste0("v",1:ncol(datcor))
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
        , columnLabels = c("Considerations","Consistency","Word Choice")) + plot_default
ggsave("fig/anes2016_corplot_components.pdf",width=2.6, height=2.6)



## label definition
dvnames <- c("Discursive\nSophistication","Factual\nKnowledge")
ivnames <- c("Intercept", "Gender\n(Female)", "Media\nExposure", "Political\nDiscussions", "Education\n(College)"
             , "Income", "log(Age)", "Race\n(Black)", "Church\nAttendance", "Survey Mode\n(Online)")


########
# validation: effect on political engagement
########

### Joint model controlling for both measures + wordsum index

m4a <- m4b <- m4c <- m4d <- NULL
m4a[[1]] <- glm(vote ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum, data = data2012, family=binomial("logit"))
m4a[[2]] <- lm(part ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum, data = data2012)
m4a[[3]] <- lm(effic_int ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum, data = data2012)
m4a[[4]] <- lm(effic_ext ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum, data = data2012)
m4b[[1]] <- glm(vote ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum, data = data2016, family=binomial("logit"))
m4b[[2]] <- lm(part ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum, data = data2016)
m4b[[3]] <- lm(effic_int ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum, data = data2016)
m4b[[4]] <- lm(effic_ext ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum, data = data2016)
m4c[[1]] <- glm(vote ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode, data = data2020, family=binomial("logit"))
m4c[[2]] <- lm(part ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode, data = data2020)
m4c[[3]] <- lm(effic_int ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig, data = data2020)
m4c[[4]] <- lm(effic_ext ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode, data = data2020)
m4d[[1]] <- glm(vote ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig, data = data_cces, family=binomial("logit"))
m4d[[2]] <- lm(polint ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig, data = data_cces)
m4d[[3]] <- lm(effic_int ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig, data = data_cces)
m4d[[4]] <- lm(effic_ext ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig, data = data_cces)

res <- rbind(sim(m4a, iv=data.frame(polknow_text_mean=sdrange(data2012$polknow_text_mean))),
             sim(m4a, iv=data.frame(polknow_factual=sdrange(data2012$polknow_factual))),
             sim(m4b, iv=data.frame(polknow_text_mean=sdrange(data2016$polknow_text_mean))),
             sim(m4b, iv=data.frame(polknow_factual=sdrange(data2016$polknow_factual))),
             sim(m4c, iv=data.frame(polknow_text_mean=sdrange(data2020$polknow_text_mean))),
             sim(m4c, iv=data.frame(polknow_factual=sdrange(data2020$polknow_factual))),
             sim(m4d, iv=data.frame(polknow_text_mean=sdrange(data_cces$polknow_text_mean))),
             sim(m4d, iv=data.frame(polknow_factual=range(data_cces$polknow_factual, na.rm = T))))
res$dvlab <- factor(res$dv, level = c("vote","part","effic_int","effic_ext")
                    , labels = c("Turnout","Non-conv. Participation"
                                 , "Internal Efficacy","External Efficacy"))
res$ivlab <- factor(res$iv, labels = dvnames)
res$Year <- rep(c("2012 ANES","2016 ANES", "2020 ANES", "2018 CCES"), each=8)

ggplot(res, aes(y=ivlab, x=mean, xmin=cilo, xmax=cihi)) +
  geom_vline(xintercept = 0, color="grey") +
  geom_point() + geom_errorbarh(height=0) + facet_grid(Year~dvlab, scale="free_x") +
  xlab("Marginal Effect (-/+ 1 SD)") + ylab("Independent Variable") + plot_default +
  scale_y_discrete(limits = rev(levels(res$ivlab))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("fig/knoweff_pres.pdf", width=6.5, height=3)

ggplot(res, aes(y=ivlab, x=mean, xmin=cilo, xmax=cihi, col = Year, shape = Year)) +
  geom_vline(xintercept = 0, color="grey") +
  geom_point(position = position_dodge(width = -.3)) +
  geom_errorbarh(height=0, position = position_dodge(width = -.3)) +
  facet_wrap(.~dvlab, scale="free_x", ncol = 4) +
  xlab("Marginal Effect (-/+ 1 SD)") + ylab("Independent Variable") + plot_default +
  scale_y_discrete(limits = rev(levels(res$ivlab))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(res, aes(y=ivlab, x=mean, xmin=cilo, xmax=cihi)) + geom_vline(xintercept = 0, color="grey") +
  geom_point() + geom_errorbarh(height=0) + facet_grid(Year~dvlab, scale="free_x") +
  xlab("Marginal Effect (-/+ 1 SD)") + ylab("Independent Variable") + plot_empty +
  scale_y_discrete(limits = rev(levels(res$ivlab))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("fig/knoweff_pres_empty.pdf", width=6.5, height=2.2)

## additional plots for presentation
filter(res, Year == "2012 ANES") %>%
  ggplot(aes(y=ivlab, x=mean, xmin=cilo, xmax=cihi)) + geom_vline(xintercept = 0, color="grey") +
  geom_point() + geom_errorbarh(height=0) + facet_wrap(.~dvlab, scale="free_x", ncol=2) +
  xlab("Marginal Effect (-/+ 1 SD)") + ylab("Independent Variable") + plot_default +
  scale_y_discrete(limits = rev(levels(res$ivlab)))
ggsave("fig/knoweff_pres1.pdf", width=4, height=3)

filter(res, Year == "2012 ANES") %>%
  ggplot(aes(y=ivlab, x=mean, xmin=cilo, xmax=cihi)) + geom_vline(xintercept = 0, color="grey") +
  geom_point() + geom_errorbarh(height=0) + facet_wrap(.~dvlab, scale="free_x", ncol=2) +
  xlab("Marginal Effect (-/+ 1 SD)") + ylab("Independent Variable") + plot_empty +
  scale_y_discrete(limits = rev(levels(res$ivlab)))
ggsave("fig/knoweff_pres0.pdf", width=4, height=3)

## plot for poster
res$dvlab <- factor(res$dv, level = c("vote","part","effic_int","effic_ext")
                    , labels = c("Turnout","Non-conv.\nParticip."
                                 , "Internal\nEfficacy","External\nEfficacy"))
ggplot(res, aes(y=ivlab, x=mean, xmin=cilo, xmax=cihi)) + geom_vline(xintercept = 0, color="grey") +
  geom_point(size=.5) + geom_errorbarh(height=0) + facet_grid(dvlab~Year, scale="free_x") +
  xlab("Marginal Effect (-/+ 1 SD)") + ylab("Independent Variable") + plot_default +
  scale_y_discrete(limits = rev(levels(res$ivlab)))
ggsave("fig/knoweff_pres_poster.pdf", width=4, height=3)



### Robustness check: Joint model controlling for both measures + word count

m4c <- m4d <- NULL
m4c[[1]] <- glm(vote ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + lwc, data = data2012, family=binomial("logit"))
m4c[[2]] <- lm(part ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + lwc, data = data2012)
m4c[[3]] <- lm(effic_int ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + lwc, data = data2012)
m4c[[4]] <- lm(effic_ext ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + lwc, data = data2012)
m4d[[1]] <- glm(vote ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + lwc, data = data2016, family=binomial("logit"))
m4d[[2]] <- lm(part ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + lwc, data = data2016)
m4d[[3]] <- lm(effic_int ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + lwc, data = data2016)
m4d[[4]] <- lm(effic_ext ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + lwc, data = data2016)

res <- rbind(sim(m4c, iv=data.frame(polknow_text_mean=sdrange(data2012$polknow_text_mean)))
             , sim(m4c, iv=data.frame(polknow_factual=sdrange(data2012$polknow_factual)))
             , sim(m4d, iv=data.frame(polknow_text_mean=sdrange(data2016$polknow_text_mean)))
             , sim(m4d, iv=data.frame(polknow_factual=sdrange(data2016$polknow_factual))))
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
ggsave("fig/knoweff_lwc.pdf", width=6.5, height=2.2)


### Robustness check: Joint model controlling for personality (extraversion and being reserved)

m4e <- m4f <- NULL
m4e[[1]] <- glm(vote ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + extraversion + reserved, data = data2012, family=binomial("logit"))
m4e[[2]] <- lm(part ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + extraversion + reserved, data = data2012)
m4e[[3]] <- lm(effic_int ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + extraversion + reserved, data = data2012)
m4e[[4]] <- lm(effic_ext ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + extraversion + reserved, data = data2012)
m4f[[1]] <- glm(vote ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + extraversion + reserved, data = data2016, family=binomial("logit"))
m4f[[2]] <- lm(part ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + extraversion + reserved, data = data2016)
m4f[[3]] <- lm(effic_int ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + extraversion + reserved, data = data2016)
m4f[[4]] <- lm(effic_ext ~ polknow_text_mean + polknow_factual + female + educ + faminc + log(age) + black + relig + mode + wordsum + extraversion + reserved, data = data2016)

res <- rbind(sim(m4e, iv=data.frame(polknow_text_mean=sdrange(data2012$polknow_text_mean)))
             , sim(m4e, iv=data.frame(polknow_factual=sdrange(data2012$polknow_factual)))
             , sim(m4f, iv=data.frame(polknow_text_mean=sdrange(data2016$polknow_text_mean)))
             , sim(m4f, iv=data.frame(polknow_factual=sdrange(data2016$polknow_factual))))
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
ggsave("fig/knoweff_personality.pdf", width=6.5, height=2.2)




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

## high/low considerations
tmp  <- data2016 %>%
  filter(wc > (median(wc) - 5) & wc < (median(wc) + 5)) %>%
  arrange(ntopics)
tmp_select <- c(head(tmp$caseid, 10), tail(tmp$caseid, 10))
tmp <- anes2016opend %>%
  filter(V160001 %in% tmp_select) %>%
  mutate(ntopics = data2016$ntopics[data2016$caseid %in% tmp_select]) %>%
  arrange(ntopics)
write.csv(t(tmp), file="tmp/select2016considerations.csv")

## high/low word choice
tmp  <- data2016 %>%
  filter(wc > (median(wc) - 5) & wc < (median(wc) + 5)) %>%
  arrange(distinct)
tmp_select <- c(head(tmp$caseid, 10), tail(tmp$caseid, 10))
tmp <- anes2016opend %>%
  filter(V160001 %in% tmp_select) %>%
  mutate(distinct = data2016$distinct[data2016$caseid %in% tmp_select]) %>%
  arrange(distinct)
write.csv(t(tmp), file="tmp/select2016wordchoice.csv")

## high/low ditem
tmp  <- data2016 %>%
  filter(wc > (median(wc) - 5) & wc < (median(wc) + 5)) %>%
  arrange(ditem)
tmp_select <- c(head(tmp$caseid, 10), tail(tmp$caseid, 10))
tmp <- anes2016opend %>%
  filter(V160001 %in% tmp_select) %>%
  mutate(ditem = data2016$ditem[data2016$caseid %in% tmp_select]) %>%
  arrange(ditem)
write.csv(t(tmp), file="tmp/select2016opinionation.csv")

data2016 %>%
  filter(caseid %in% tmp_select) %>%
  arrange(polknow_text_mean) %>%
  dplyr::select(caseid, pid, ideol, educ, vc_pre, vc_post, vc_change, polknow_text_mean, polknow_factual)


###################
### Additional information: STM summaries
###################

summary(stm_fit2012)
summary(stm_fit2016)

pdf("fig/anes_stm_prop.pdf", width=12, height=10)
par(mfrow=c(1,2), mar=c(4.2,0.5,2.5,0.5))
plot(stm_fit2012
     , main=paste0("2012 ANES (k = ",stm_fit2012$settings$dim$K,")",collapse = "")
     , n=5, labeltype = "prob", text.cex = 1)
plot(stm_fit2016
     , main=paste0("2016 ANES (k = ",stm_fit2016$settings$dim$K,")",collapse = "")
     , n=5, labeltype = "prob", text.cex = 1)
dev.off()

pdf("fig/stm_labels.pdf")
par(mfrow=c(1,2), mar=c(0.5,0.5,2.5,0.5))
plot(stm_fit2012, "labels", topics=c(16,14,10,8), main="Sample Topics (2012 ANES)")
plot(stm_fit2016, "labels", topics=c(1,10,19,17), main="Sample Topics (2016 ANES)")
dev.off()


###################
### Save results for appendix
###################

save(m4a, m4b, m4c, m4d, m4e, m4f, m5a, m5b, hetreg_summary, file="out/anes_res.Rdata")