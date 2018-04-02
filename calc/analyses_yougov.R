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
library(stargazer)

setwd("/data/Dropbox/Uni/Projects/2016/knowledge/calc")

## load data and stm results
load("out/yougov.Rdata")


## QUESTION: remove wc=0 and spanish=1?
mean(yougov$wc[yougov$wc>0])

source("func.R")

## plot defaults
plot_default <- theme_classic(base_size=9) + theme(panel.border = element_rect(fill=NA))


########
# Prelim results
########

### compare measures
summary(lm(know_dis ~ polknow_text_mean + know_pol, data=data_yg))
summary(lm(know_dis ~ polknow_text_mean + know_pol + female + log(age) + black + relig + educ + faminc, data=data_yg))
## argument: Discursive sophistication is a better measure of competence in the sense that the respondents pick up 
## information about the disease

## closing gender gap is replicated!
summary(lm(polknow_text_mean ~ female + educ + log(age) + black + relig + educ + faminc, data=data_yg))
summary(lm(know_pol ~ female + educ + log(age) + black + relig + educ + faminc, data=data_yg))
summary(lm(know_dis ~ female + educ + log(age) + black + relig + educ + faminc, data=data_yg))



########
# correlation matrices: compare with common measures
########

datcor <- data_yg[,c("polknow_text_mean","know_pol", "know_dis")]
colnames(datcor) <- paste0("v",1:ncol(datcor))

pdf("../fig/yg_corplot.pdf",width=2.5, height=2.5)
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.1, size=.2)), axisLabels="none"
        , columnLabels = c("Discursive\nSophistication","Factual\nKnowledge","Disease\nInformation")) + 
  plot_default
dev.off()

pdf("../fig/yg_corplot_empty.pdf",width=2.5, height=2.5)
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.01, size=.2)), axisLabels="none"
        , columnLabels = c("Discursive\nSophistication","Factual\nKnowledge","Disease\nInformatilon")) +
  theme_classic(base_size=8) + theme(panel.border = element_rect(fill="white"))
dev.off()


datcor <- data_yg[,c("ntopics","distinct","ditem")]
colnames(datcor) <- paste0("v",1:ncol(datcor))

ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.05, size=.2)), axisLabels="none"
        , columnLabels = c("Considerations","Word Choice","Opinionation")) + plot_default
ggsave("../fig/yg_corplot_components.pdf",width=2.6, height=2.6)


########
# predicting disease information retrieval
########
# NOTE: control for wordsum?

### Joint model controlling for both measures

m2full <- lm(know_dis ~ polknow_text_mean + know_pol + female + educ + faminc + log(age) + black + relig, data = data_yg)

res <- rbind(data.frame(sim(m2full, iv=data.frame(polknow_text_mean=range(data_yg$polknow_text_mean)))
                        , Variable="Discursive Sophistication")
             , data.frame(sim(m2full, iv=data.frame(know_pol=range(data_yg$know_pol)))
                          , Variable="Factual Knowledge"))

res <- rbind(data.frame(sim(m2full, iv=data.frame(polknow_text_mean=seq(min(data_yg$polknow_text_mean),max(data_yg$polknow_text_mean),length=10)))
                        ,value=seq(min(data_yg$polknow_text_mean),max(data_yg$polknow_text_mean),length=10),Variable="Discursive Sophistication")
             , data.frame(sim(m2full, iv=data.frame(know_pol=seq(0,1,length=10)))
                          ,value=seq(0,1,length=10),Variable="Factual Knowledge"))
ggplot(res, aes(x=value, y=mean, ymin=cilo,ymax=cihi, lty=Variable, fill=Variable)) + plot_default +
  geom_ribbon(alpha=0.4, lwd=.1) + geom_line() + 
  ylab("Information Retrieval") + xlab("Value of Independent Variable")
ggsave("../fig/yg_disease.pdf",width=4,height=2)


###################
### Additional information: STM summaries
###################

pdf("../fig/yg_stm_prop.pdf", width=6, height=8)
par(mar=c(4.2,0.5,2.5,0.5))
plot(stm_fit
     , main=paste0("YouGov Survey (k = ",stm_fit$settings$dim$K,")",collapse = "")
     , n=5, labeltype = "prob", text.cex = 1)
dev.off()


###################
###  Generate tables for appendix
###################

## print summary
summary(m2full)

## create table
stargazer(m2full, align = TRUE, column.sep.width = "0pt", no.space = TRUE, digits= 3, model.numbers = FALSE, 
          model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = c(.05,.01,.001),
          title="Effects of sophistication on information retrieval in the 2015 YouGov study.
          Standard errors in parentheses. Estimates are used for Figure 3 in the main text.",
          column.labels = "Information Retrieval",
          covariate.labels = c("Discursive Soph.","Factual Knowledge","Female",
                               "College Degree","Family Income","Age (log)",
                               "African American","Church Attendance","Constant"),
          keep.stat = c("n", "rsq"),
          out = "../tab/yg_disease.tex", label = "tab:yg_disease", type="text")
