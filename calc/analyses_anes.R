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

policies <- c("ideol","spsrvpr","defsppr","inspre","guarpr")
targets <- c("rpc","dpc","rep","dem")
measures <- c("polknow_text_mean","polknow_factual")

polknow_hetreg <- function(policy, target, measure
                           , controls = c("female", "educ", "faminc", "lage"
                                          , "black", "relig", "mode")
                           , df = data){
  tmp <- na.omit(df[,c(paste(c(policy,target),collapse="_")
                       , paste(c(policy,"ego"),collapse="_")
                       , measure, controls)])
  y <- tmp[,paste(c(policy,target),collapse="_")]
  X <- tmp[,c(paste(c(policy,"ego"),collapse="_"), controls)]
  Z <- as.matrix(tmp[,measure])
  
  dl <- list(N = nrow(X), B = ncol(X), G = ncol(Z)
             , y = y, X = X, Z = Z
             , S = 10, Zpred = as.matrix(seq(min(Z[,1]), max(Z[,1]), length.out = 10)))
  res <- stan(file = "hetreg.stan", data=dl)
  return(res)
}

m5summary <- data.frame(NULL)
for(p in policies){
  for(t in targets){
    for(m in measures){
      iterlab <- paste(c(p,t,m),collapse="_")
      cat("Iteration: ",iterlab,"\n")
      tmp <- polknow_hetreg(p, t, m)
      tmp_sigmadif <- extract(tmp, par="sigmadif")[[1]]
      tmp_df <- data.frame(policy = p, target = t, measure = m, mean = mean(tmp_sigmadif)
                           , cilo = quantile(tmp_sigmadif, .025), cihi = quantile(tmp_sigmadif, .975))
      m5summary <- rbind(m5summary, tmp_df)
    }
  }
}

ggplot(m5summary, aes(y=measure, x=mean, xmin=cilo, xmax=cihi)) + 
  geom_point() + geom_errorbarh(height=0) + facet_grid(policy~target) +
  geom_vline(xintercept = 0, color="grey") + 
  xlab("Error Variance Reduction") + ylab("Independent Variable (Max - Min)") + plot_default



### pre-post consistency
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

