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
library(MASS)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

setwd("/data/Dropbox/Uni/Projects/2016/knowledge/calc")

## load data and stm results
load("out/anes.Rdata")
#load("../data/anes_old.Rdata")

## QUESTION: remove wc=0 and spanish=1?

source("sim.R")
source("latexTable.R")

## plot defaults
plot_default <- theme_classic(base_size=9) + theme(panel.border = element_rect(fill=NA))



#######################
### WORK ON NEW MEASURE
#######################

## check stm fit
dim(stm_fit$beta$logbeta[[1]])
# beta: List containing the log of the word probabilities for each topic.
apply(exp(stm_fit$beta$logbeta[[1]]), 1, sum)

#hist(apply(exp(stm_fit$beta$logbeta[[1]]), 2, sum))
#hist(apply(exp(stm_fit$beta$logbeta[[1]]), 2, max))

length(stm_fit$vocab)
stm_fit$vocab[1:5]

## compute shannon entropy
shannon <- function(x, reversed = F){
  out <- (- sum(log(x^x)/log(length(x))))
  if(reversed) out <- 1 - out
  out
}

## which features have the highest probability for each topic?
stm_fit$vocab[apply(exp(stm_fit$beta$logbeta[[1]]), 1, which.max)]

## which topic has highest likelihood for each word
term_topic <- apply(stm_fit$beta$logbeta[[1]], 2, which.max)
# -> compute log(#topics)

## shannon entropy of each term as a measure for its distinctiveness -> vague term or clear topic
pseudop <- exp(stm_fit$beta$logbeta[[1]])
pseudop <- t(t(pseudop)/apply(pseudop,2,sum))
term_entropy <- apply(pseudop, 2, shannon, reversed=T) # theoretically, it should be reversed=T
# -> calculate average distinctiveness of words

## combine both measures with open-ended responses
know <- data.frame(ntopics = rep(NA, length(out$documents))
                   , entropy = rep(NA, length(out$documents)))
for(doc in 1:length(out$documents)){
  know$ntopics[doc] <- log(length(unique(term_topic[out$documents[[doc]][1,]])))
  know$entropy[doc] <- sum(term_entropy[out$documents[[doc]][1,]] * out$documents[[doc]][2,]) / sum(out$documents[[doc]][2,])
}
data <- cbind(data, know)

data$polknow_text_mean <- data$lwc
data$polknow_text_mean <- data$ntopics
data$polknow_text_mean <- data$entropy
data$polknow_text_mean <- data$ntopics * data$entropy



########
# correlation matrices: compare with common measures
########

datcor <- data[,c("polknow_text_mean","polknow_factual","polknow_evalpre")]
colnames(datcor) <- paste0("v",1:ncol(datcor))

pdf("../fig/corplot_pres.pdf",width=3.3, height=3.3)
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
        , columnLabels = c("Text-based\nSophistication","Factual\nKnowledge"
                           ,"Interviewer\nEvaluation (Pre)")) + plot_default
dev.off()

pdf("../fig/corplot_empty.pdf",width=3.3, height=3.3)
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
        , columnLabels = c("Text-based\nSophistication","Factual\nKnowledge"
                           ,"Interviewer\nEvaluation (Pre)")) + 
  theme_classic(base_size=9) + theme(panel.border = element_rect(fill="white"))
dev.off()


## label definition
dvnames <- c("Text-based\nSophistication","Factual\nKnowledge")
ivnames <- c("Intercept", "Gender\n(Female)", "Media\nExposure", "Political\nDiscussions", "Education\n(College)"
             , "Income", "log(Age)", "Race\n(Black)", "Church\nAttendance", "Survey Mode\n(Online)")


########
# validation: effect on political engagement
########

m4a <- m4b <- NULL
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
res$dvlab <- factor(res$dv, labels = c("Internal Efficacy","External Efficacy"
                                       ,"Non-conv. Participation","Turnout"))
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



########
# validation: party/candidate placement precision
########

hetreg_summary <- filter(hetreg_summary, policy!="ideol")
hetreg_summary$policy <- factor(hetreg_summary$policy
                                , labels = c("Government\nSpending","Defense\nSpending"
                                             ,"Insurance\nPolicy","Job\nGuarantee"))
hetreg_summary$measure <- factor(hetreg_summary$measure
                                 , labels = c("Text-based\nSophistication","Factual\nKnowledge"))
hetreg_summary$target <- factor(hetreg_summary$target
                                , labels = c("Mitt\nRomney","Barack\nObama"
                                             ,"Republican\nParty","Democratic\nParty"))

ggplot(hetreg_summary, aes(y=measure, x=mean, xmin=cilo, xmax=cihi)) + 
  geom_point() + geom_errorbarh(height=0) + facet_grid(policy~target) +
  geom_vline(xintercept = 0, color="grey") + 
  xlab("Error Variance Reduction") + ylab("Independent Variable (Max - Min)") + plot_default
ggsave("../fig/hetreg.pdf",width = 6, height = 4)


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
                        , value=seq(0.1798338, 0.7234572,length=10),Variable="Text-based Sophistication")
             , data.frame(sim(m5b, iv=data.frame(polknow_factual=seq(0, 1,length=10)))
                          , value=seq(0, 1,length=10),Variable="Factual Knowledge"))
res$ivlab <- factor(res$iv, labels = dvnames)

ggplot(res, aes(x=value, y=mean, ymin=cilo,ymax=cihi)) + plot_default +
  #geom_errorbar(alpha=.5, width=0) + 
  geom_ribbon(alpha=0.4, lwd=.1, fill="lightblue") + geom_line() + 
  facet_grid(~Variable, scales="free_x") +
  ylab("Expected Probability\nto Keep Vote Choice") + xlab("Value of independent variable")
ggsave("../fig/prepost_exp.pdf",width=4,height=2)
