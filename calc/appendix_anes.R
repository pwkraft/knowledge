### ============================================================= ###
### Measuring Political Sophistication using Open-ended Responses ###
### Patrick Kraft                                                 ###
### ============================================================= ###
## reduced analysis for presentation (based on data created in prep.R)


rm(list = ls())
library(car)
library(nFactors)
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
load("out/anes2012.Rdata")
load("out/anes2016.Rdata")
#load("../data/anes_old.Rdata")

## QUESTION: remove wc=0 and spanish=1?

source("func.R")

## plot defaults
plot_default <- theme_classic(base_size=9) + theme(panel.border = element_rect(fill=NA))


########
# word count plots
########

wc_mean = mean(data2012$wc)
ggplot(data2012, aes(wc)) + geom_histogram(fill = "grey", binwidth = 25) + theme_classic(base_size = 8) + 
  theme(panel.border = element_rect(fill=NA)) + 
  geom_vline(xintercept = wc_mean, colour="red", linetype = "longdash") +
  ylab("Number of Respondents") + xlab("Word Count")
ggsave("../fig/anes2012_wc.pdf", width = 3, height = 2)

wc_mean = mean(data2016$wc)
ggplot(data2016, aes(wc)) + geom_histogram(fill = "grey", binwidth = 25) + theme_classic(base_size = 8) + 
  theme(panel.border = element_rect(fill=NA)) + 
  geom_vline(xintercept = wc_mean, colour="red", linetype = "longdash") +
  ylab("Number of Respondents") + xlab("Word Count")
ggsave("../fig/anes2016_wc.pdf", width = 3, height = 2)

########
# correlation matrices: compare with common measures
########

datcor <- data2012[,c("polknow_text_mean","polknow_factual","polknow_evalpre")]
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
# factor analysis of individual components
########

datcor2012 <- data2012[,c("ditem","ntopics","distinct","polknow_factual","polknow_evalpre")]
datcor2016 <- data2016[,c("ditem","ntopics","distinct","polknow_factual","polknow_evalpre")]

fit2012 <- factanal(datcor2012[,1:3], 1, rotation="varimax")
fit2016 <- factanal(datcor2016[,1:3], 1, rotation="varimax")

tibble(Variable = c("Opinionation","Considerations", "Word Choice"),
       `2012` = fit2012$loadings[,1],
       `2016` = fit2016$loadings[,1]) %>%
  xtable(caption = "Factor Loadings of Discursive Sophistication Components
         in the American National Election Study (ANES)",
         label = "app:factload", digits = 3) %>%
  print(file = "../tab/factload.tex", 
        caption.placement = "top",
        include.rownames = FALSE)

## check factor analysis including conventional measures
(fit <- factanal(na.omit(datcor2012), 2, rotation="varimax"))
(fit <- factanal(na.omit(datcor2016), 2, rotation="varimax"))

## Determine the number of factors (including conventional measures)
ev <- eigen(cor(datcor2012, use = "complete.obs")) # get eigenvalues
ap <- parallel(subject=nrow(datcor2012),var=ncol(datcor2012),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) 

ev <- eigen(cor(datcor2016, use = "complete.obs")) # get eigenvalues
ap <- parallel(subject=nrow(datcor2016),var=ncol(datcor2016),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) 



########
# distribution of control variables
########

### Histograms of dependent variables (ANES 2012)
desc <- list(NULL)
desc[[1]] <- ggplot(anes2012, aes(x=factor(vote, labels=c("No","Yes")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Voted in Election") + plot_default
desc[[2]] <- ggplot(anes2012, aes(x=part)) + geom_bar(stat="count") + 
  labs(y="Count", x="Non-conv. Participation") + plot_default
desc[[3]] <- ggplot(anes2012, aes(x=effic_int)) + geom_bar(stat="count") + 
  labs(y="Count", x="Internal Efficacy") + plot_default
desc[[4]] <- ggplot(anes2012, aes(x=effic_ext)) + geom_bar(stat="count") + 
  labs(y="Count", x="Exterbal Efficacy") + plot_default
desc[[5]] <- ggplot(anes2012, aes(x=ideol_rpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Ideology (Romney)") + plot_default
desc[[6]] <- ggplot(anes2012, aes(x=ideol_dpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Ideology (Obama)") + plot_default
desc[[7]] <- ggplot(anes2012, aes(x=spsrvpr_rpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Government Spending (Romney)") + plot_default
desc[[8]] <- ggplot(anes2012, aes(x=spsrvpr_dpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Government Spending (Obama)") + plot_default
desc[[9]] <- ggplot(anes2012, aes(x=defsppr_rpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Defense Spending (Romney)") + plot_default
desc[[10]] <- ggplot(anes2012, aes(x=defsppr_dpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Defense Spending (Obama)") + plot_default
desc[[11]] <- ggplot(anes2012, aes(x=inspre_rpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Insurance Policy (Romney)") + plot_default
desc[[12]] <- ggplot(anes2012, aes(x=inspre_dpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Insurance Policy (Obama)") + plot_default
desc[[13]] <- ggplot(anes2012, aes(x=guarpr_rpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Job Guarantee (Romney)") + plot_default
desc[[14]] <- ggplot(anes2012, aes(x=guarpr_dpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Job Guarantee (Obama)") + plot_default
desc[[15]] <- ggplot(anes2012, aes(x=aidblack_rpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Aid to Blacks (Romney)") + plot_default
desc[[16]] <- ggplot(anes2012, aes(x=aidblack_dpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Aid to Blacks (Obama)") + plot_default
desc[[17]] <- ggplot(anes2012, aes(x=envjob_rpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Environment vs Jobs (Romney)") + plot_default
desc[[18]] <- ggplot(anes2012, aes(x=envjob_dpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Environment vs Jobs (Obama)") + plot_default
# desc[[19]] <- ggplot(anes2012, aes(x=factor(correct_vote, labels=c("No","Yes")))) + 
#   geom_bar(stat="count") + labs(y="Count", x="Correct Vote") + plot_default
pdf("../fig/descriptives_anes2012dv.pdf", width=9, height=9)
grid.arrange(grobs=desc,ncol=4)
dev.off()

### Histograms of dependent variables (ANES 2016)
desc <- list(NULL)
desc[[1]] <- ggplot(anes2016, aes(x=factor(vote, labels=c("No","Yes")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Voted in Election") + plot_default
desc[[2]] <- ggplot(anes2016, aes(x=part)) + geom_bar(stat="count") + 
  labs(y="Count", x="Non-conv. Participation") + plot_default
desc[[3]] <- ggplot(anes2016, aes(x=effic_int)) + geom_bar(stat="count") + 
  labs(y="Count", x="Internal Efficacy") + plot_default
desc[[4]] <- ggplot(anes2016, aes(x=effic_ext)) + geom_bar(stat="count") + 
  labs(y="Count", x="Exterbal Efficacy") + plot_default
desc[[5]] <- ggplot(anes2016, aes(x=ideol_rpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Ideology (Trump)") + plot_default
desc[[6]] <- ggplot(anes2016, aes(x=ideol_dpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Ideology (Clinton)") + plot_default
desc[[7]] <- ggplot(anes2016, aes(x=spsrvpr_rpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Government Spending (Trump)") + plot_default
desc[[8]] <- ggplot(anes2016, aes(x=spsrvpr_dpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Government Spending (Clinton)") + plot_default
desc[[9]] <- ggplot(anes2016, aes(x=defsppr_rpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Defense Spending (Trump)") + plot_default
desc[[10]] <- ggplot(anes2016, aes(x=defsppr_dpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Defense Spending (Clinton)") + plot_default
desc[[11]] <- ggplot(anes2016, aes(x=inspre_rpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Insurance Policy (Trump)") + plot_default
desc[[12]] <- ggplot(anes2016, aes(x=inspre_dpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Insurance Policy (Clinton)") + plot_default
desc[[13]] <- ggplot(anes2016, aes(x=guarpr_rpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Job Guarantee (Trump)") + plot_default
desc[[14]] <- ggplot(anes2016, aes(x=guarpr_dpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Job Guarantee (Clinton)") + plot_default
desc[[15]] <- ggplot(anes2016, aes(x=aidblack_rpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Aid to Blacks (Trump)") + plot_default
desc[[16]] <- ggplot(anes2016, aes(x=aidblack_dpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Aid to Blacks (Clinton)") + plot_default
desc[[17]] <- ggplot(anes2016, aes(x=envjob_rpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Environment vs Jobs (Trump)") + plot_default
desc[[18]] <- ggplot(anes2016, aes(x=envjob_dpc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Environment vs Jobs (Clinton)") + plot_default
# desc[[19]] <- ggplot(anes2016, aes(x=factor(correct_vote, labels=c("No","Yes")))) + 
#   geom_bar(stat="count") + labs(y="Count", x="Correct Vote") + plot_default
pdf("../fig/descriptives_anes2016dv.pdf", width=9, height=9)
grid.arrange(grobs=desc,ncol=4)
dev.off()

### Histograms of independent variables (ANES 2012)
desc <- list(NULL)
desc[[1]] <- ggplot(anes2012, aes(x=factor(female,labels=c("Male","Female")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Gender") + plot_default
desc[[2]] <- ggplot(anes2012, aes(x=factor(educ, labels=c("No College","College")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Education") + plot_default
desc[[3]] <- ggplot(anes2012, aes(x=faminc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Family Income") + plot_default
desc[[4]] <- ggplot(anes2012, aes(x=age)) + geom_bar(stat="count") + 
  labs(y="Count", x="Age") + plot_default
desc[[5]] <- ggplot(anes2012, aes(x=factor(black,labels=c("Other","Black non-Hispanic")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Race/Ethnicity") + plot_default
desc[[6]] <- ggplot(anes2012, aes(x=relig)) + geom_bar(stat="count") + 
  labs(y="Count", x="Church Attendance") + plot_default
desc[[7]] <- ggplot(anes2012, aes(x=factor(mode, labels=c("Face-to-Face","Online")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Survey Mode") + plot_default
desc[[8]] <- ggplot(anes2012, aes(x=wordsum)) + geom_bar(stat="count") + 
  labs(y="Count", x="Wordsum Literacy Test") + plot_default
desc[[9]] <- ggplot(anes2012, aes(x=polmedia)) + geom_bar(stat="count") + 
  labs(y="Count", x="Political Media Exposure") + plot_default
desc[[10]] <- ggplot(anes2012, aes(x=poldisc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Political Discussions") + plot_default
desc[[11]] <- ggplot(anes2012, aes(x=factor(vote, labels=c("No","Yes")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Voted in 2012") + plot_default
desc[[12]] <- ggplot(anes2012, aes(x=ideol_ego)) + geom_bar(stat="count") + 
  labs(y="Count", x="Ideology (Ego)") + plot_default
desc[[13]] <- ggplot(anes2012, aes(x=spsrvpr_ego)) + geom_bar(stat="count") + 
  labs(y="Count", x="Government Spending (Ego)") + plot_default
desc[[14]] <- ggplot(anes2012, aes(x=defsppr_ego)) + geom_bar(stat="count") + 
  labs(y="Count", x="Defense Spending (Ego)") + plot_default
desc[[15]] <- ggplot(anes2012, aes(x=inspre_ego)) + geom_bar(stat="count") + 
  labs(y="Count", x="Insurance Policy (Ego)") + plot_default
desc[[16]] <- ggplot(anes2012, aes(x=guarpr_ego)) + geom_bar(stat="count") + 
  labs(y="Count", x="Job Guarantee (Ego)") + plot_default
desc[[17]] <- ggplot(anes2012, aes(x=aidblack_ego)) + geom_bar(stat="count") + 
  labs(y="Count", x="Aid to Blacks (Ego)") + plot_default
desc[[18]] <- ggplot(anes2012, aes(x=envjob_ego)) + geom_bar(stat="count") + 
  labs(y="Count", x="Environment vs Jobs (Ego)") + plot_default
desc[[19]] <- ggplot(anes2012, aes(x=extraversion)) + geom_bar(stat="count") + 
  labs(y="Count", x="Personality: Extraversion") + plot_default
desc[[20]] <- ggplot(anes2012, aes(x=reserved)) + geom_bar(stat="count") + 
  labs(y="Count", x="Personality: Reserved") + plot_default
pdf("../fig/descriptives_anes2012iv.pdf", width=9, height=9)
grid.arrange(grobs=desc,ncol=4)
dev.off()

### Histograms of independent variables (ANES 2016)
desc <- list(NULL)
desc[[1]] <- ggplot(anes2016, aes(x=factor(female,labels=c("Male","Female")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Gender") + plot_default
desc[[2]] <- ggplot(anes2016, aes(x=factor(educ, labels=c("No College","College")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Education") + plot_default
desc[[3]] <- ggplot(anes2016, aes(x=faminc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Family Income") + plot_default
desc[[4]] <- ggplot(anes2016, aes(x=age)) + geom_bar(stat="count") + 
  labs(y="Count", x="Age") + plot_default
desc[[5]] <- ggplot(anes2016, aes(x=factor(black,labels=c("Other","Black non-Hispanic")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Race/Ethnicity") + plot_default
desc[[6]] <- ggplot(anes2016, aes(x=relig)) + geom_bar(stat="count") + 
  labs(y="Count", x="Church Attendance") + plot_default
desc[[7]] <- ggplot(anes2016, aes(x=factor(mode, labels=c("Face-to-Face","Online")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Survey Mode") + plot_default
desc[[8]] <- ggplot(anes2016, aes(x=wordsum)) + geom_bar(stat="count") + 
  labs(y="Count", x="Wordsum Literacy Test") + plot_default
desc[[9]] <- ggplot(anes2016, aes(x=polmedia)) + geom_bar(stat="count") + 
  labs(y="Count", x="Political Media Exposure") + plot_default
desc[[10]] <- ggplot(anes2016, aes(x=poldisc)) + geom_bar(stat="count") + 
  labs(y="Count", x="Political Discussions") + plot_default
desc[[11]] <- ggplot(anes2016, aes(x=factor(vote, labels=c("No","Yes")))) + 
  geom_bar(stat="count") + labs(y="Count", x="Voted in 2016") + plot_default
desc[[12]] <- ggplot(anes2016, aes(x=ideol_ego)) + geom_bar(stat="count") + 
  labs(y="Count", x="Ideology (Ego)") + plot_default
desc[[13]] <- ggplot(anes2016, aes(x=spsrvpr_ego)) + geom_bar(stat="count") + 
  labs(y="Count", x="Government Spending (Ego)") + plot_default
desc[[14]] <- ggplot(anes2016, aes(x=defsppr_ego)) + geom_bar(stat="count") + 
  labs(y="Count", x="Defense Spending (Ego)") + plot_default
desc[[15]] <- ggplot(anes2016, aes(x=inspre_ego)) + geom_bar(stat="count") + 
  labs(y="Count", x="Insurance Policy (Ego)") + plot_default
desc[[16]] <- ggplot(anes2016, aes(x=guarpr_ego)) + geom_bar(stat="count") + 
  labs(y="Count", x="Job Guarantee (Ego)") + plot_default
desc[[17]] <- ggplot(anes2016, aes(x=aidblack_ego)) + geom_bar(stat="count") + 
  labs(y="Count", x="Aid to Blacks (Ego)") + plot_default
desc[[18]] <- ggplot(anes2016, aes(x=envjob_ego)) + geom_bar(stat="count") + 
  labs(y="Count", x="Environment vs Jobs (Ego)") + plot_default
desc[[19]] <- ggplot(anes2016, aes(x=extraversion)) + geom_bar(stat="count") + 
  labs(y="Count", x="Personality: Extraversion") + plot_default
desc[[20]] <- ggplot(anes2016, aes(x=reserved)) + geom_bar(stat="count") + 
  labs(y="Count", x="Personality: Reserved") + plot_default
pdf("../fig/descriptives_anes2016iv.pdf", width=9, height=9)
grid.arrange(grobs=desc,ncol=4)
dev.off()


########
# robustness checks: determinants of willingness to respond?
########

## check determinants of oe response >0
m2 <- glm(as.numeric(wc>0) ~ female + polmedia + poldisc + educ + faminc + log(age) + relig + black + mode, data = anes2012, family=binomial("logit"))
summary(m2)

## gender differences in willingness to respond & length of response
mean(data2012$wc)
t.test(wc~female, data=data2012)
t.test(as.numeric(wc>0)~female, data=anes2012)

## prep data for heckit model
heck_tmp <- data.frame(caseid=data2012$caseid, polknow_text_mean=data2012$polknow_text_mean)
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

m2 <- lm(redist ~ tax * polknow_text + tax * polknow_factual + female + age + black + relig + educ + faminc + ideol_ct + pid_cont, data=data2012)
summary(m2)

summary(lm(redist ~ tax * polknow_text + tax * polknow_factual + female + age + black + relig + educ + faminc + ideol_ct + pid_cont, data=data2016))

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



#############################
### Tables of Model Estimates

### ANES analyses
load("out/anes_res.Rdata")


### Fig 2: Engagement and participation in politics

## print summary
stargazer(m4a, type="text")

## create table
stargazer(m4a, align = FALSE, column.sep.width = "0pt", no.space = TRUE, digits= 3, model.numbers = FALSE, 
          model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = NA, omit.table.layout = "n",
          title=c("Effects of sophistication on turnout, non-conventional participation, internal efficacy, 
          and external efficacy in the 2012 ANES. Standard errors in parentheses. Estimates are used for
          Figure 2 in the main text."),
          column.labels = c("Turnout","Participation","Internal Efficacy","External Efficacy"),
          covariate.labels = c("Discursive Soph.","Factual Knowledge","Female",
                               "College Degree","Family Income","Age (log)",
                               "African American","Church Attendance","Mode: Online",
                               "Wordsum Score","Constant"),
          keep.stat = c("n", "rsq", "ll"),
          out = "../tab/knoweff2012.tex", label = "tab:knoweff2012", type="text")

## print summary
stargazer(m4b, type="text")

## create table
stargazer(m4b, align = FALSE, column.sep.width = "0pt", no.space = TRUE, digits= 3, model.numbers = FALSE, 
          model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = NA, omit.table.layout = "n",
          title="Effects of sophistication on turnout, non-conventional participation, internal efficacy, 
          and external efficacy in the 2016 ANES. Standard errors in parentheses. Estimates are used for
          Figure 2 in the main text.",
          column.labels = c("Turnout","Participation","Internal Efficacy","External Efficacy"),
          covariate.labels = c("Discursive Soph.","Factual Knowledge","Female",
                               "College Degree","Family Income","Age (log)",
                               "African American","Church Attendance","Mode: Online",
                               "Wordsum Score","Constant"),
          keep.stat = c("n", "rsq", "ll"),
          out = "../tab/knoweff2016.tex", label = "tab:knoweff2016", type="text")


### Robustenss check for Fig.2: Control for personality characteristics

## print summary
stargazer(m4e, type="text")

## create table
stargazer(m4e, align = FALSE, column.sep.width = "0pt", no.space = TRUE, digits= 3, model.numbers = FALSE, 
          model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = NA, omit.table.layout = "n",
          title="Effects of sophistication on turnout, non-conventional participation, internal efficacy, 
          and external efficacy controlling for personality characteristics in the 2012 ANES. 
          Standard errors in parentheses. Estimates are used for Figure \\ref{fig:knoweff_personality} 
          in the appendix.",
          column.labels = c("Turnout","Participation","Internal Efficacy","External Efficacy"),
          covariate.labels = c("Discursive Soph.","Factual Knowledge","Female",
                               "College Degree","Family Income","Age (log)",
                               "African American","Church Attendance","Mode: Online",
                               "Wordsum Score","Extraversion","Reserved","Constant"),
          keep.stat = c("n", "rsq", "ll"),
          out = "../tab/knoweff2012_personality.tex", label = "tab:knoweff2012_personality", type="text")

## print summary
stargazer(m4f, type="text")

## create table
stargazer(m4f, align = FALSE, column.sep.width = "0pt", no.space = TRUE, digits= 3, model.numbers = FALSE, 
          model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = NA, omit.table.layout = "n",
          title="Effects of sophistication on turnout, non-conventional participation, internal efficacy, 
          and external efficacy controlling for personality characteristics in the 2016 ANES. 
          Standard errors in parentheses. Estimates are used for Figure \\ref{fig:knoweff_personality} 
          in the appendix.",
          column.labels = c("Turnout","Participation","Internal Efficacy","External Efficacy"),
          covariate.labels = c("Discursive Soph.","Factual Knowledge","Female",
                               "College Degree","Family Income","Age (log)",
                               "African American","Church Attendance","Mode: Online",
                               "Wordsum Score","Extraversion","Reserved","Constant"),
          keep.stat = c("n", "rsq", "ll"),
          out = "../tab/knoweff2016_personality.tex", label = "tab:knoweff2016_personality", type="text")


### Robustenss check for Fig.2: Control for individual response length

## print summary
stargazer(m4c, type="text")

## create table
stargazer(m4c, align = FALSE, column.sep.width = "0pt", no.space = TRUE, digits= 3, model.numbers = FALSE, 
          model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = NA, omit.table.layout = "n",
          title="Effects of sophistication on turnout, non-conventional participation, internal efficacy, 
          and external efficacy controlling for individual response length in the 2012 ANES. 
          Standard errors in parentheses. Estimates are used for Figure \\ref{fig:knoweff_lwc} 
          in the appendix.",
          column.labels = c("Turnout","Participation","Internal Efficacy","External Efficacy"),
          covariate.labels = c("Discursive Soph.","Factual Knowledge","Female",
                               "College Degree","Family Income","Age (log)",
                               "African American","Church Attendance","Mode: Online",
                               "Wordsum Score","Word Count (log)","Constant"),
          keep.stat = c("n", "rsq", "ll"),
          out = "../tab/knoweff2012_lwc.tex", label = "tab:knoweff2012_lwc", type="text")

## print summary
stargazer(m4d, type="text")

## create table
stargazer(m4d, align = FALSE, column.sep.width = "0pt", no.space = TRUE, digits= 3, model.numbers = FALSE, 
          model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = NA, omit.table.layout = "n",
          title="Effects of sophistication on turnout, non-conventional participation, internal efficacy, 
          and external efficacy controlling for individual response length in the 2016 ANES. 
          Standard errors in parentheses. Estimates are used for Figure \\ref{fig:knoweff_lwc} 
          in the appendix.",
          column.labels = c("Turnout","Participation","Internal Efficacy","External Efficacy"),
          covariate.labels = c("Discursive Soph.","Factual Knowledge","Female",
                               "College Degree","Family Income","Age (log)",
                               "African American","Church Attendance","Mode: Online",
                               "Wordsum Score","Word Count (log)","Constant"),
          keep.stat = c("n", "rsq", "ll"),
          out = "../tab/knoweff2016_lwc.tex", label = "tab:knoweff2016_lwc", type="text")


### Fig 4: Precise positioning of candidates

colnames(hetreg_summary2012) <- gsub("gamma_cihio","gamma_cihi",colnames(hetreg_summary2012))
colnames(hetreg_summary2016) <- gsub("gamma_cihio","gamma_cihi",colnames(hetreg_summary2016))

hetreg_summary2012 %>%
  transmute('Policy Position' = factor(policy, labels = c("Ideology","Government Spending","Defense Spending",
                                                          "Insurance Policy","Job Guarantee","Aid to Blacks",
                                                          "Environment vs Jobs")),
            'Candidate' = factor(target, labels = c("Romney","Obama")),
            'Independent Var.' = factor(measure, labels = c("Factual Knowl.", "Discursive Soph.")),
            'E[$\\gamma$]' = round(gamma, 3),
            '$\\text{sd}(\\gamma)$' = round(sd, 3),
            '95\\% Cred. Int.' = paste0("(",round(gamma_cilo,3),"; ",round(gamma_cihi,3),")"),
            '$\\hat{R}$' = round(Rhat, 3)) %>% 
  xtable(caption = "Error variance reduction in candidate placements on multiple issues in the 2012 ANES. 
         Estimates are used for Figure 4 in the main text.",
         label="app:hetreg2012") %>% 
  print(file = "../tab/hetreg2012.tex", caption.placement = "top",
        include.rownames=FALSE, sanitize.colnames.function = function(x){x})
  
hetreg_summary2016 %>%
  transmute('Policy Position' = factor(policy, labels = c("Ideology","Government Spending","Defense Spending",
                                                          "Insurance Policy","Job Guarantee","Aid to Blacks",
                                                          "Environment vs Jobs")),
            'Candidate' = factor(target, labels = c("Trump","Clinton")),
            'Independent Var.' = factor(measure, labels = c("Factual Knowl.", "Discursive Soph.")),
            'E[$\\gamma$]' = round(gamma, 3),
            '$\\text{sd}(\\gamma)$' = round(sd, 3),
            '95\\% Cred. Int.' = paste0("(",round(gamma_cilo,3),"; ",round(gamma_cihi,3),")"),
            '$\\hat{R}$' = round(Rhat, 3)) %>% 
  xtable(caption = "Error variance reduction in candidate placements on multiple issues in the 2016 ANES.
         Estimates are used for Figure 4 in the main text.",
         label="app:hetreg2016") %>% 
  print(file = "../tab/hetreg2016.tex", caption.placement = "top",
        include.rownames=FALSE, sanitize.colnames.function = function(x){x})


### Fig 6: Correct voting

## print summary
stargazer(m5a, m5b, type = "text")

## create table
stargazer(m5a, m5b, align = FALSE, column.sep.width = "0pt", no.space = TRUE, digits= 3, model.numbers = FALSE, 
          model.names=FALSE, dep.var.labels.include = T, star.cutoffs = NA, omit.table.layout = "n",
          title="Effects of sophistication on the probability of casting a correct vote in the 
          2012 and 2016 ANES (estimated via logistic regression). Standard errors in parentheses.
          Estimates are used for Figure 6 in the main text.",
          dep.var.labels = "Correct Vote",
          column.labels = c("2012 ANES","2016 ANES"),
          covariate.labels = c("Discursive Soph.","Factual Knowledge","Female",
                               "College Degree","Family Income","Age (log)",
                               "African American","Church Attendance","Mode: Online",
                               "Wordsum Score","Constant"),
          keep.stat = c("n", "ll"),
          out = "../tab/correctvote.tex", label = "tab:correctvote", type="text")



