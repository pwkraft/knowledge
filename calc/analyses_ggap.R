### ============================================================= ###
### Measuring Political Sophistication using Open-ended Responses ###
### Patrick Kraft                                                 ###
### ============================================================= ###
## gender gap analysis combining al datasets


rm(list = ls())
gc()

library(quanteda)
library(ggplot2)
library(xtable)
library(gridExtra)
library(dplyr)
library(stm)
library(stargazer)

setwd("/data/Dropbox/Uni/Projects/2016/knowledge/calc")

## load data and stm results
load("out/anes2012.Rdata")
load("out/anes2016.Rdata")
load("out/swiss.Rdata")
load("out/yougov.Rdata")

## QUESTION: remove wc=0 and spanish=1?

source("func.R")

## plot defaults
plot_default <- theme_classic(base_size=9) + theme(panel.border = element_rect(fill=NA))
plot_empty <- theme_classic(base_size=9) + theme(panel.border = element_rect(fill="white"))

## compare response behavior by gender
ggplot(anes2012, aes(factor(female), y=as.numeric(wc!=0))) + 
  stat_summary_bin(fun.y = "mean", geom="bar")

ggplot(filter(anes2012, wc>0), aes(factor(female), y=wc)) + geom_point(alpha=.1, size=.1) + 
  stat_summary(fun.data = "mean_cl_boot", col="red")

ggplot(filter(anes2012, wc>0), aes(factor(female), y=wc)) + 
  stat_summary(fun.data = "mean_cl_boot", col="red")

table(anes2012$spanish==0)
table(anes2012$wc!=0)

## label definition
dvnames <- c("Discursive\nSophistication","Factual\nKnowledge")
ivnames <- c("Intercept", "Gender\n(Female)", "Media\nExposure", "Political\nDiscussions", "Education\n(College)"
             , "Income", "log(Age)", "Race\n(Black)", "Church\nAttendance", "Survey Mode\n(Online)")


########
# bar plots comparing men and women
########


### 2012 ANES

plot_df <- data.frame(rbind(cbind(data2012$polknow_text_mean, data2012$female, 1)
                            , cbind(data2012$polknow_factual, data2012$female, 2))) %>% na.omit()
colnames(plot_df) <- c("Knowledge","Gender","Variable")
plot_df$Gender <- factor(plot_df$Gender, labels = c("Male","Female"))
plot_df$Variable <- factor(plot_df$Variable, labels = c("Discursive\nSophistication", "Factual\nKnowledge"))
plot_means <- plot_df %>% group_by(Variable, Gender) %>% 
  summarize_all(funs(mean="mean",n=length(.),sd="sd",quant=quantile(.,.95),max="max")) %>%
  mutate(cilo = mean - 1.96*sd/sqrt(n)
         , cihi = mean + 1.96*sd/sqrt(n))

p1 <- ggplot(plot_means, aes(y=mean,x=Gender,ymin=cilo,ymax=cihi, fill=Gender)) + plot_default + 
  geom_bar(stat="identity") + geom_errorbar(width=.25) + 
  facet_wrap(~Variable) + ylab("Average Values") + xlab(NULL) +
  geom_point(aes(y=max), col="white") + guides(fill=FALSE) + scale_fill_brewer(palette="Paired") +
  ggtitle("2012 ANES")


### Gender gap in interviewer assessment for male/female interviewers?

t.test(polknow_factual~female, data=data2012)
t.test(polknow_text_mean~female, data=data2012)
t.test(polknow_factual~female, data=data2012[data2012$iwr_female==0,])
t.test(polknow_factual~female, data=data2012[data2012$iwr_female==1,])


### 2016 ANES

plot_df <- data.frame(rbind(cbind(data2016$polknow_text_mean, data2016$female, 1)
                            , cbind(data2016$polknow_factual, data2016$female, 2))) %>% na.omit()
colnames(plot_df) <- c("Knowledge","Gender","Variable")
plot_df$Gender <- factor(plot_df$Gender, labels = c("Male","Female"))
plot_df$Variable <- factor(plot_df$Variable, labels = c("Discursive\nSophistication", "Factual\nKnowledge"))
plot_means <- plot_df %>% group_by(Variable, Gender) %>% 
  summarize_all(funs(mean="mean",n=length(.),sd="sd",quant=quantile(.,.95),max="max")) %>%
  mutate(cilo = mean - 1.96*sd/sqrt(n)
         , cihi = mean + 1.96*sd/sqrt(n))

p2 <- ggplot(plot_means, aes(y=mean,x=Gender,ymin=cilo,ymax=cihi, fill=Gender)) + plot_default + 
  geom_bar(stat="identity") + geom_errorbar(width=.25) + 
  facet_wrap(~Variable) + ylab("Average Values") + xlab(NULL) +
  geom_point(aes(y=max), col="white") + guides(fill=FALSE) + scale_fill_brewer(palette="Paired") +
  ggtitle("2016 ANES")


### Yougov Data

plot_df <- data.frame(rbind(cbind(data_yg$polknow_text_mean, data_yg$female, 1)
                            , cbind(data_yg$know_pol, data_yg$female, 2))) %>% na.omit()
colnames(plot_df) <- c("Knowledge","Gender","Variable")
plot_df$Gender <- factor(plot_df$Gender, labels = c("Male","Female"))
plot_df$Variable <- factor(plot_df$Variable, labels = c("Discursive\nSophistication", "Factual\nKnowledge"))
plot_means <- plot_df %>% group_by(Variable, Gender) %>% 
  summarize_all(funs(mean="mean",n=length(.),sd="sd",quant=quantile(.,.95),max="max")) %>%
  mutate(cilo = mean - 1.96*sd/sqrt(n)
         , cihi = mean + 1.96*sd/sqrt(n))

p3 <- ggplot(plot_means, aes(y=mean,x=Gender,ymin=cilo,ymax=cihi, fill=Gender)) + plot_default + 
  geom_bar(stat="identity") + geom_errorbar(width=.25) + 
  facet_wrap(~Variable) + ylab("Average Values") + xlab(NULL) +
  geom_point(aes(y=max), col="white") + guides(fill=FALSE) + scale_fill_brewer(palette="Paired") +
  ggtitle("2015 YouGov Survey")


### Swiss Data -  German

plot_df <- data.frame(rbind(cbind(opend_german$polknow_text_mean, opend_german$female, 1)
                            , cbind(opend_german$loj, opend_german$female, 2))) %>% na.omit()
colnames(plot_df) <- c("Knowledge","Gender","Variable")
plot_df$Gender <- factor(plot_df$Gender, labels = c("Male","Female"))
plot_df$Variable <- factor(plot_df$Variable, labels = c("Discursive\nSophistication", "Level of\nJustification"))
plot_means <- plot_df %>% group_by(Variable, Gender) %>% 
  summarize_all(funs(mean="mean",n=length(.),sd="sd",quant=quantile(.,.95),max="max")) %>%
  mutate(cilo = mean - 1.96*sd/sqrt(n)
         , cihi = mean + 1.96*sd/sqrt(n))

p4 <- ggplot(plot_means, aes(y=mean,x=Gender,ymin=cilo,ymax=cihi, fill=Gender)) + plot_default + 
  geom_bar(stat="identity") + geom_errorbar(width=.25) + 
  facet_wrap(~Variable, scale="free_y") + 
  geom_point(aes(y=max), col="white") + guides(fill=FALSE) + scale_fill_brewer(palette="Paired") +
  labs(title="Swiss Survey", subtitle="German Respondents", y="Average Values", x=NULL)


### Swiss Data - French

plot_df <- data.frame(rbind(cbind(opend_french$polknow_text_mean, opend_french$female, 1)
                            , cbind(opend_french$loj, opend_french$female, 2))) %>% na.omit()
colnames(plot_df) <- c("Knowledge","Gender","Variable")
plot_df$Gender <- factor(plot_df$Gender, labels = c("Male","Female"))
plot_df$Variable <- factor(plot_df$Variable, labels = c("Discursive\nSophistication", "Level of\nJustification"))
plot_means <- plot_df %>% group_by(Variable, Gender) %>% 
  summarize_all(funs(mean="mean",n=length(.),sd="sd",quant=quantile(.,.95),max="max")) %>%
  mutate(cilo = mean - 1.96*sd/sqrt(n)
         , cihi = mean + 1.96*sd/sqrt(n))

p5 <- ggplot(plot_means, aes(y=mean,x=Gender,ymin=cilo,ymax=cihi, fill=Gender)) + plot_default + 
  geom_bar(stat="identity") + geom_errorbar(width=.25) + 
  facet_wrap(~Variable, scale="free_y") + 
  geom_point(aes(y=max), col="white") + guides(fill=FALSE) + scale_fill_brewer(palette="Paired") +
  labs(title=" ", subtitle="French Respondents", y="Average Values", x=NULL)


### Swiss Data -  Italian

plot_df <- data.frame(rbind(cbind(opend_italian$polknow_text_mean, opend_italian$female, 1)
                            , cbind(opend_italian$loj, opend_italian$female, 2))) %>% na.omit()
colnames(plot_df) <- c("Knowledge","Gender","Variable")
plot_df$Gender <- factor(plot_df$Gender, labels = c("Male","Female"))
plot_df$Variable <- factor(plot_df$Variable, labels = c("Discursive\nSophistication", "Level of\nJustification"))
plot_means <- plot_df %>% group_by(Variable, Gender) %>% 
  summarize_all(funs(mean="mean",n=length(.),sd="sd",quant=quantile(.,.95),max="max")) %>%
  mutate(cilo = mean - 1.96*sd/sqrt(n)
         , cihi = mean + 1.96*sd/sqrt(n))

p6 <- ggplot(plot_means, aes(y=mean,x=Gender,ymin=cilo,ymax=cihi, fill=Gender)) + plot_default + 
  geom_bar(stat="identity") + geom_errorbar(width=.25) + 
  facet_wrap(~Variable, scale="free_y") + 
  geom_point(aes(y=max), col="white") + guides(fill=FALSE) + scale_fill_brewer(palette="Paired") +
  labs(title=" ", subtitle="Italian Respondents", y="Average Values", x=NULL)


(p0 <- grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3))
ggsave("../fig/meandiff.pdf", p0 ,width=6.5, height=4)



########
# determinants of political knowledge
########
# NOTE: control for wordsum?
# NOTE: rescale knowledge variables to unit variance?


### ANES data

dvnames <- rep(c("Discursive Sophistication","Factual Knowledge"), each=2)
ivnames <- c("Intercept", "Female", "Media", "Discussions", "College"
             , "Income", "log(Age)", "Black", "Church", "Online")

# model estimation
m1 <- NULL
m1[[1]] <- lm(polknow_text_mean ~ female + polmedia + poldisc + educ + faminc + log(age) + black + relig + mode, data = data2012)
m1[[2]] <- lm(polknow_text_mean ~ female + polmedia + poldisc + educ + faminc + log(age) + black + relig + mode, data = data2016)
m1[[3]] <- lm(polknow_factual ~ female + polmedia + poldisc + educ + faminc + log(age) + black + relig + mode, data = data2012)
m1[[4]] <- lm(polknow_factual ~ female + polmedia + poldisc + educ + faminc + log(age) + black + relig + mode, data = data2016)
lapply(m1, summary)

# prepare dataframe for plotting (sloppy code)
dfplot1 <- data.frame()
for(i in 1:length(m1)){
  tmp <- data.frame(summary(m1[[i]])$coefficients[,1:2])
  tmp$iv <- rownames(tmp)
  tmp$ivnames <- ivnames[1:nrow(tmp)]
  tmp$dv <- dvnames[i]
  rownames(tmp) <- NULL
  dfplot1 <- rbind(dfplot1, tmp)
  rm(tmp)
}
dfplot1$source <- rep(c("2012 ANES", "2016 ANES"), each=nrow(dfplot1)/4)

# create factor variables, remove intercept for plotting
dfplot1$ivnames <- factor(dfplot1$ivnames, levels = rev(ivnames))
dfplot1$dv <- factor(dfplot1$dv, levels = dvnames[c(1,3)])
dfplot1 <- dfplot1[dfplot1$ivnames!="Intercept",]

### Add YouGov data
## Q: ADD POLITICAL INTEREST AS IV???

dvnames_yg <- c("Discursive Sophistication","Factual Knowledge")#, "Disease\nInformation")
ivnames_yg <- c("Intercept", "Female", "College", "Income", "log(Age)", "Black", "Church")

# model estimation
m2 <- NULL
m2[[1]] <- lm(polknow_text_mean ~ female + educ + faminc + log(age) + black + relig, data = data_yg)
m2[[2]] <- lm(know_pol ~ female + educ + faminc + log(age) + black + relig, data = data_yg)
#m2[[3]] <- lm(know_dis ~ female + educ + faminc + log(age) + relig  + black, data = data_yg)
lapply(m2, summary)

# prepare dataframe for plotting (sloppy code)
dfplot2 <- data.frame()
for(i in 1:length(m2)){
  tmp <- data.frame(summary(m2[[i]])$coefficients[,1:2])
  tmp$iv <- rownames(tmp)
  tmp$ivnames <- ivnames_yg[1:nrow(tmp)]
  tmp$dv <- dvnames_yg[i]
  rownames(tmp) <- NULL
  dfplot2 <- rbind(dfplot2, tmp)
  rm(tmp)
}
dfplot2$source <- "2015 YouGov Survey"

# create factor variables, remove intercept for plotting
dfplot2$ivnames <- factor(dfplot2$ivnames, levels = rev(ivnames_yg))
dfplot2$dv <- factor(dfplot2$dv, levels = dvnames_yg)
dfplot2 <- dfplot2[dfplot2$ivnames!="Intercept",]


### combine ANES and YouGov

dfplot <- rbind(dfplot1, dfplot2)
dfplot$source <- factor(dfplot$source, levels=c("2012 ANES", "2016 ANES", "2015 YouGov Survey"))

ggplot(dfplot, aes(y=ivnames, x=Estimate
                   , xmin = Estimate-1.96*Std..Error, xmax = Estimate+1.96*Std..Error)) + 
  geom_vline(xintercept = 0, color="grey") + xlab("Estimate") + ylab("Independent Variable") +
  geom_point() + geom_errorbarh(height = 0) + facet_grid(source~dv, scale="free") +
  plot_default + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("../fig/determinants.pdf",width=5,height=4)

ggplot(dfplot, aes(y=ivnames, x=Estimate
                   , xmin = Estimate-1.96*Std..Error, xmax = Estimate+1.96*Std..Error)) + 
  geom_vline(xintercept = 0, color="grey") + xlab("Estimate") + ylab("Independent Variable") +
  geom_point() + geom_errorbarh(height = 0) + facet_grid(source~dv, scale="free") +
  plot_empty + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("../fig/determinants_empty.pdf",width=5,height=4)


########
# topic differences b/w men and women (ANES)
########

summary(stm_fit2012)
summary(stm_fit2016)

## estimate topic prevalence effects
prep2012 <- estimateEffect(~ age + educ_cont + pid_cont + educ_pid + female
                           , stm_fit2012, meta = out2012$meta, uncertainty = "Global")
prep2016 <- estimateEffect(~ age + educ_cont + pid_cont + educ_pid + female
                           , stm_fit2016, meta = out2016$meta, uncertainty = "Global")

## select topics with largest gender effects
tmp2012 <- tibble(estimate = sapply(summary(prep2012)[[3]], function(x) x["female","Estimate"])
              , topics = prep2012$topics) %>% arrange(estimate)
topics2012 <- c(head(tmp2012$topics), tail(tmp2012$topics))
tmp2016 <- tibble(estimate = sapply(summary(prep2016)[[3]], function(x) x["female","Estimate"])
                  , topics = prep2016$topics) %>% arrange(estimate)
topics2016 <- c(head(tmp2016$topics), tail(tmp2016$topics))

pdf("../fig/stm_gender.pdf", height=5, width=12)
par(mfrow=c(1,2), mar=c(2.2,0.5,2.2,0.5))
plot.estimateEffect(prep2012, covariate = "female", topics = topics2012, model = stm_fit2012
                    , xlim = c(-.05,.015), method = "difference", cov.value1 = 1, cov.value2 = 0
                    , labeltype = "prob", n=5, verbose.labels = F, width=50
                    , main = "Gender Differences in Topic Proportions (2012 ANES)")
plot.estimateEffect(prep2016, covariate = "female", topics = topics2016, model = stm_fit2016
                    , xlim = c(-.05,.015), method = "difference", cov.value1 = 1, cov.value2 = 0
                    , labeltype = "prob", n=5, verbose.labels = F, width=50
                    , main = "Gender Differences in Topic Proportions (2016 ANES)")
dev.off()



###############
# Generate tables for appendix
###############

## print summary
stargazer(m1, type = "text")

## create table
stargazer(m1, align = FALSE, column.sep.width = "0pt", no.space = TRUE, digits= 3, model.numbers = FALSE, 
          model.names=FALSE, dep.var.labels.include = TRUE, star.cutoffs = NA, omit.table.layout = "n",
          title="Effects of gender on discursive sophistication and factual knowledge in the 
          2012 ANES and 2016 ANES. Standard errors in parentheses. Estimates are used for 
          Figure 7 in the main text.",
          column.labels = c("2012 ANES","2016 ANES","2012 ANES","2016 ANES"),
          dep.var.labels = c("Discursive Sophistication","Factual Knowledge"),
          covariate.labels = c("Female","Media Exposure","Discussion Freqency",
                               "College Degree","Family Income","Age (log)",
                               "African American","Church Attendance",
                               "Mode: Online","Constant"),
          keep.stat = c("n", "rsq"),
          out = "../tab/determinants_anes.tex", label = "tab:determinants_anes", type="text")


## print summary
stargazer(m2, type = "text")

## create table
stargazer(m2, align = FALSE, column.sep.width = "0pt", no.space = TRUE, digits= 3, model.numbers = FALSE, 
          model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = NA, omit.table.layout = "n",
          title="Effects of gender on discursive sophistication and factual knowledge in the 
          2015 YouGov Study. Standard errors in parentheses. Estimates are used for 
          Figure 7 in the main text.",
          column.labels = c("Discursive Sophistication","Factual Knowledge"),
          #dep.var.labels = c("Discursive Sophistication","Factual Knowledge"),
          covariate.labels = c("Female","College Degree","Family Income","Age (log)",
                               "African American","Church Attendance","Constant"),
          keep.stat = c("n", "rsq"),
          out = "../tab/determinants_yg.tex", label = "tab:determinants_yg", type="text")
