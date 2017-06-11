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

source("sim.R")
source("latexTable.R")

## plot defaults
plot_default <- theme_classic(base_size=8) + theme(panel.border = element_rect(fill=NA))


########
# Prelim results
########

### compare measures
summary(lm(know_dis ~ polknow_text_mean + know_pol, data=data))
summary(lm(know_dis ~ polknow_text_mean + know_pol + female + log(age) + black + relig + educ + faminc, data=data))
## argument: text-based sophistication is a better measure of competence in the sense that the respondents pick up information about the disease

## closing gender gap is replicated!
summary(lm(polknow_text_mean ~ female + educ + log(age) + black + relig + educ + faminc, data=data))
summary(lm(know_pol ~ female + educ + log(age) + black + relig + educ + faminc, data=data))
summary(lm(know_dis ~ female + educ + log(age) + black + relig + educ + faminc, data=data))



########
# correlation matrices: compare with common measures
########

datcor <- data[,c("polknow_text_mean","know_pol", "know_dis")]
colnames(datcor) <- paste0("v",1:ncol(datcor))

#pdf("../fig/corplot.pdf",width=5, height=5)
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.1, size=.2)), axisLabels="none"
        , columnLabels = c("Text-based\nSophistication","Factual\nKnowledge","Disease\nKnowledge")) + 
  plot_default
#dev.off()

# pdf("../fig/corplot_empty.pdf",width=5, height=5)
# ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.01, size=.2)), axisLabels="none"
#         , columnLabels = c("Text-based\nSophistication","Factual\nKnowledge","Office\nRecognition"
#                            ,"Majorities\nin Congress", "Interviewer\nEvaluation (Pre)"
#                            , "Interviewer\nEvaluation (Post)")) + 
#   theme_classic(base_size=8) + theme(panel.border = element_rect(fill="white"))
# dev.off()


########
# histograms comparing men and women
########

## histogram/density of weighted 
plot_df <- data.frame(rbind(cbind(data$polknow_text_mean, data$female, 1)
                      , cbind(data$know_pol, data$female, 2)
                      , cbind(data$know_dis, data$female, 3))) %>% na.omit()
colnames(plot_df) <- c("Knowledge","Gender","Variable")
plot_df$Gender <- factor(plot_df$Gender, labels = c("Male","Female"))
plot_df$Variable <- factor(plot_df$Variable, labels = c("Text-based sophistication", "Factual Knowledge"
                                                        , "Disease Knowledge"))
plot_means <- plot_df %>% group_by(Variable, Gender) %>% 
  summarize_each(funs(mean="mean",n=length(.),sd="sd",quant=quantile(.,.95),max="max")) %>%
  mutate(cilo = mean - 1.96*sd/sqrt(n)
         , cihi = mean + 1.96*sd/sqrt(n))

## density plots
ggplot(plot_df, aes(x=Knowledge, col=Gender, lty=Gender)) + plot_default + 
  geom_density() + facet_wrap(~Variable, scale="free") + 
  xlab("Value on Knowledge Measure") + ylab("Density") +
  geom_vline(aes(xintercept = mean, col = Gender, lty=Gender), data=plot_means)
# ggsave("../fig/densities.pdf", width=6, height=3)

## bar plots comparing men and women
ggplot(plot_means, aes(y=mean,x=Gender,ymin=cilo,ymax=cihi)) + plot_default + 
  geom_bar(stat="identity", fill="grey80") + geom_errorbar(width=.25) + 
  facet_wrap(~Variable, scale="free") + ylab("Mean Value on Knowledge Measure") +
  geom_point(aes(y=max), col="white")
ggsave("../fig/yg_meandiff.png", width=5, height=3)
# ggsave("../fig/meandiff.pdf", width=6, height=5)
# ggsave("../fig/meandiff_small.pdf", width=5, height=4)

# ggplot(plot_means, aes(y=mean,x=Gender,ymin=cilo,ymax=cihi)) + 
#   theme_classic(base_size=8) + theme(panel.border = element_rect(fill="white")) +
#   geom_bar(stat="identity", fill="grey80") + geom_errorbar(width=.25) + 
#   facet_wrap(~Variable, scale="free") + ylab("Mean Value on Knowledge Measure") +
#   geom_point(aes(y=max), col="white")
# ggsave("../fig/meandiff_empty.pdf", width=5, height=4)

########
# determinants of political knowledge
########
# NOTE: control for wordsum?

### Q: ADD POLITICAL INTEREST AS IV???

m1 <- NULL
m1[[1]] <- lm(polknow_text_mean ~ female + educ + log(age) + black + relig + faminc, data = data)
m1[[2]] <- lm(know_pol ~ female + educ + log(age) + black + relig + faminc, data = data)
m1[[3]] <- lm(know_dis ~ female + educ + log(age) + black + relig + faminc, data = data)
lapply(m1, summary)

dvnames <- c("Text-based Sophistication","Factual Knowledge","Disease Knowledge")
ivnames <- c("Intercept", "Gender (Female)", "Education (College)"
             , "log(Age)", "Race (Black)", "Church Attendance", "Family Income")

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
  geom_point() + geom_errorbarh(height = 0) + facet_wrap(~dv, scales="free_x",ncol=3) +
  plot_default
ggsave("../fig/yg_determinants.png",width=5,height=3)
# ggsave("../fig/determinants.pdf",width=5,height=3)

# ggplot(dfplot, aes(y=ivnames, x=Estimate
#                    , xmin = Estimate-1.96*Std..Error, xmax = Estimate+1.96*Std..Error)) + 
#   geom_vline(xintercept = 0, color="grey") + xlab("Estimate") + ylab("Independent Variable") +
#   geom_point() + geom_errorbarh(height = 0) + facet_wrap(~dv, scales="free_x",ncol=3) +
#   theme_classic(base_size = 8) + theme(panel.border = element_rect(fill="white"))
# ggsave("../fig/determinants_empty.pdf",width=5,height=3)


########
# predicting disease knowledge
########
# NOTE: control for wordsum?

m2 <- NULL
m2[[1]] <- lm(know_dis ~ polknow_text_mean + female + educ + log(age) + black + relig + faminc, data = data)
m2[[2]] <- lm(know_dis ~ know_pol + female + educ + log(age) + black + relig + faminc, data = data)
m2[[3]] <- lm(know_dis ~ polknow_text_mean + know_pol + female + educ + log(age) + black + relig + faminc, data = data)
lapply(m2, summary)

res <- rbind(data.frame(sim(m2[[1]], iv=data.frame(polknow_text_mean=seq(0,1,length=10)))
                        ,value=seq(0,1,length=10),Variable="Text-based Sophistication")
             , data.frame(sim(m2[[2]], iv=data.frame(know_pol=seq(0,1,length=10)))
                          ,value=seq(0,1,length=10),Variable="Factual Knowledge")
             , data.frame(sim(m2[[3]], iv=data.frame(polknow_text_mean=seq(0,1,length=10)))
                          ,value=seq(0,1,length=10),Variable="Text-based Sophistication")
             , data.frame(sim(m2[[3]], iv=data.frame(know_pol=seq(0,1,length=10)))
                          ,value=seq(0,1,length=10),Variable="Factual Knowledge"))
res$model <- rep(c(1,2), each=nrow(res)/2)
res$model <- factor(res$model, labels = c("Individual Models","Combined Model"))

ggplot(res, aes(x=value, y=mean, ymin=cilo,ymax=cihi)) + plot_default +
  #geom_errorbar(alpha=.5, width=0) + 
  geom_ribbon(alpha=0.1, lwd=.1) + geom_line() + 
  facet_grid(model~Variable) +
  ylab("Expected disease knowledge") + xlab("Value of independent variable")
ggsave("../fig/yg_disease.png",width=5,height=3)
# ggsave("../fig/closing.pdf",width=5,height=3)

ggplot(res, aes(x=value, y=mean, col=Gender,ymin=cilo,ymax=cihi, lty=Gender)) + 
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill="white")) +
  #geom_errorbar(alpha=.5, width=0) + 
  geom_ribbon(alpha=0.1, lwd=.1) + geom_line() + 
  facet_grid(dvlab~Variable, scale="free_y") +
  ylab("Expected sophistication") + xlab("Value of independent variable")
# ggsave("../fig/closing_empty.pdf",width=5,height=3)
