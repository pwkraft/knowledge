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
load("anes.Rdata")
#load("../data/anes_old.Rdata")

source("sim.R")
source("latexTable.R")

## plot defaults
plot_default <- theme_classic(base_size=8) + theme(panel.border = element_rect(fill=NA))


########
# correlation matrices: compare with common measures
########

datcor <- data[,c("polknow_text_mean","polknow_factual", "polknow_office", "polknow_majority", 
                  "polknow_evalpre", "polknow_evalpost")]
colnames(datcor) <- paste0("v",1:ncol(datcor))

pdf("../fig/corplot.pdf",width=5, height=5)
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.01, size=.2)), axisLabels="none"
        , columnLabels = c("Text-based\nSophistication","Factual\nKnowledge","Office\nRecognition"
                           ,"Majorities\nin Congress", "Interviewer\nEvaluation (Pre)"
                           , "Interviewer\nEvaluation (Post)")) + plot_default
dev.off()


########
# histograms comparing men and women
########

## histogram/density of weighted 
plot_df <- data.frame(rbind(cbind(data$polknow_text_mean, data$female, 1)
                      , cbind(data$polknow_factual, data$female, 2)
                      , cbind(data$polknow_office, data$female, 3)
                      , cbind(data$polknow_majority, data$female, 4)
                      , cbind(data$polknow_evalpre, data$female, 5)
                      , cbind(data$polknow_evalpost, data$female, 6))) %>% na.omit()
colnames(plot_df) <- c("Knowledge","Gender","Variable")
plot_df$Gender <- factor(plot_df$Gender, labels = c("Male","Female"))
plot_df$Variable <- factor(plot_df$Variable, labels = c("Text-based sophistication", "Factual Knowledge"
                                                        , "Office Recognition", "Majorities in Congress"
                                                        , "Interviewer Evaluation (Pre)"
                                                        , "Interviewer Evaluation (Post)"))
plot_means <- plot_df %>% group_by(Variable, Gender) %>% 
  summarize_each(funs(mean="mean",n=length(.),sd="sd",quant=quantile(.,.95),max="max")) %>%
  mutate(cilo = mean - 1.96*sd/sqrt(n)
         , cihi = mean + 1.96*sd/sqrt(n))

## density plots
ggplot(plot_df, aes(x=Knowledge, col=Gender, lty=Gender)) + plot_default + 
  geom_density() + facet_wrap(~Variable, scale="free") + 
  xlab("Value on Knowledge Measure") + ylab("Density") +
  geom_vline(aes(xintercept = mean, col = Gender, lty=Gender), data=plot_means)
ggsave("../fig/densities.pdf", width=6, height=3)

## bar plots comparing men and women
ggplot(plot_means, aes(y=mean,x=Gender,ymin=cilo,ymax=cihi)) + plot_default + 
  geom_bar(stat="identity", fill="grey80") + geom_errorbar(width=.25) + 
  facet_wrap(~Variable, scale="free") + ylab("Mean Value on Knowledge Measure") +
  geom_point(aes(y=max), col="white")
ggsave("../fig/meandiff.pdf", width=6, height=5)


########
# determinants of political knowledge
########
# NOTE: control for wordsum?

m1 <- NULL
m1[[1]] <- lm(polknow_text_mean ~ female + polmedia + poldisc + educ + log(age) + black + relig + mode, data = data)
m1[[2]] <- lm(polknow_factual ~ female + polmedia + poldisc + educ + log(age) + black + relig + mode, data = data)
m1[[3]] <- lm(polknow_office ~ female + polmedia + poldisc + educ + log(age) + black + relig + mode, data = data)
m1[[4]] <- lm(polknow_majority ~ female + polmedia + poldisc + educ + log(age) + black + relig + mode, data = data)
m1[[5]] <- lm(polknow_evalpre ~ female + polmedia + poldisc + educ + log(age) + black + relig + mode, data = data)
m1[[6]] <- lm(polknow_evalpost ~ female + polmedia + poldisc + educ + log(age) + black + relig + mode, data = data)
lapply(m1, summary)

dvnames <- c("Text-based Sophistication","Factual Knowledge","Office Recognition"
             ,"Majorities in Congress", "Interviewer Evaluation (Pre)", "Interviewer Evaluation (Post)")
ivnames <- c("Intercept", "Gender (Female)", "Media Exposure", "Political Discussions", "Education (College)"
             , "log(Age)", "Race (Black)", "Church Attendance", "Survey Mode (Online)")

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
  geom_vline(xintercept = 0, color="grey") + xlab("Estimate") +
  geom_point() + geom_errorbarh(height = 0) + facet_wrap(~dv, scales="free_x",ncol=3) +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA), axis.title=element_blank())
ggsave("../fig/determinants.pdf",width=5,height=3)


########
# closing the knowledge gap
########
# NOTE: control for wordsum?

m2 <- NULL
m2[[1]] <- lm(polknow_text_mean ~ female * polmedia + female * poldisc + female * educ + log(age) + black + relig + mode, data = data)
m2[[2]] <- lm(polknow_factual ~ female * polmedia + female * poldisc + female * educ + log(age) + black + relig + mode, data = data)
#m2[[3]] <- lm(polknow_office ~ female * polmedia + female * poldisc + female * educ + female + log(age) + black + relig + mode, data = data)
#m2[[4]] <- lm(polknow_majority ~ female * polmedia + female * poldisc + female * educ + female + log(age) + black + relig + mode, data = data)
#m2[[5]] <- lm(polknow_evalpre ~ female * polmedia + female * poldisc + female * educ + female + log(age) + black + relig, data = data)
#m2[[6]] <- lm(polknow_evalpost ~ female * polmedia + female * poldisc + female * educ + female + log(age) + black + relig, data = data)

res <- rbind(data.frame(sim(m2, iv=data.frame(female = 0, polmedia=seq(0,1,length=10)))
                        ,value=seq(0,1,length=10),Variable="Media Exposure",Gender="Male")
             , data.frame(sim(m2, iv=data.frame(female = 0, poldisc=seq(0,1,length=10)))
                          ,value=seq(0,1,length=10),Variable="Political Discussion",Gender="Male")
             , data.frame(sim(m2, iv=data.frame(female = 0, educ=seq(0,1,length=10)))
                          ,value=seq(0,1,length=10),Variable="Education",Gender="Male")
             , data.frame(sim(m2, iv=data.frame(female = 1, polmedia=seq(0,1,length=10)))
                          ,value=seq(0,1,length=10),Variable="Media Exposure",Gender="Female")
             , data.frame(sim(m2, iv=data.frame(female = 1, poldisc=seq(0,1,length=10)))
                          ,value=seq(0,1,length=10),Variable="Political Discussion",Gender="Female")
             , data.frame(sim(m2, iv=data.frame(female = 1, educ=seq(0,1,length=10)))
                          ,value=seq(0,1,length=10),Variable="Education",Gender="Female"))
res$dvlab <- factor(res$dv, labels = dvnames[1:2])

ggplot(res, aes(x=value, y=mean, col=Gender,ymin=cilo,ymax=cihi, lty=Gender)) + plot_default +
  #geom_errorbar(alpha=.5, width=0) + 
  geom_ribbon(alpha=0.1, lwd=.1) + geom_line() + 
  facet_grid(dvlab~Variable, scale="free_y") +
  ylab("Expected sophistication") + xlab("Value of independent variable")
ggsave("../fig/closing.pdf",width=5,height=3)


########
# sophistication as an independent variable
########

m3a <- m3b <- m3c <- m3d <- NULL
m3a[[1]] <- lm(effic_int ~ polknow_text_mean*female + educ + log(age) + black + relig + mode, data = data)
m3a[[2]] <- lm(effic_ext ~ polknow_text_mean*female + educ + log(age) + black + relig + mode, data = data)
m3a[[3]] <- lm(part ~ polknow_text_mean*female + educ + log(age) + black + relig + mode, data = data)
m3a[[4]] <- glm(vote ~ polknow_text_mean*female + educ + log(age) + black + relig + mode, data = data, family=binomial("logit"))
m3b[[1]] <- lm(effic_int ~ polknow_factual*female + educ + log(age) + black + relig + mode, data = data)
m3b[[2]] <- lm(effic_ext ~ polknow_factual*female + educ + log(age) + black + relig + mode, data = data)
m3b[[3]] <- lm(part ~ polknow_factual*female + educ + log(age) + black + relig + mode, data = data)
m3b[[4]] <- glm(vote ~ polknow_factual*female + educ + log(age) + black + relig + mode, data = data, family=binomial("logit"))
# m3c[[1]] <- lm(effic_int ~ polknow_office*female + educ + log(age) + black + relig + mode, data = data)
# m3c[[2]] <- lm(effic_ext ~ polknow_office*female + educ + log(age) + black + relig + mode, data = data)
# m3c[[3]] <- lm(part ~ polknow_office*female + educ + log(age) + black + relig + mode, data = data)
# m3c[[4]] <- glm(vote ~ polknow_office*female + educ + log(age) + black + relig + mode, data = data, family=binomial("logit"))
# m3d[[1]] <- lm(effic_int ~ polknow_majority*female + educ + log(age) + black + relig + mode, data = data)
# m3d[[2]] <- lm(effic_ext ~ polknow_majority*female + educ + log(age) + black + relig + mode, data = data)
# m3d[[3]] <- lm(part ~ polknow_majority*female + educ + log(age) + black + relig + mode, data = data)
# m3d[[4]] <- glm(vote ~ polknow_majority*female + educ + log(age) + black + relig + mode, data = data, family=binomial("logit"))

res <- rbind(data.frame(sim(m3a, iv=data.frame(female = 0, polknow_text_mean=seq(0,max(data$polknow_text_mean),length=10)))
                        , value=seq(0,max(data$polknow_text_mean),length=10), Gender="Male"
                        , Variable="Text-based Sophistication")
             , data.frame(sim(m3a, iv=data.frame(female = 1, polknow_text_mean=seq(0,max(data$polknow_text_mean),length=10)))
                          , value=seq(0,max(data$polknow_text_mean),length=10), Gender="Female"
                          , Variable="Text-based Sophistication")
             , data.frame(sim(m3b, iv=data.frame(female = 0, polknow_factual=seq(0,1,length=6)))
                          , value=seq(0,1,length=6), Gender="Male"
                          , Variable="Factual Knowledge")
             , data.frame(sim(m3b, iv=data.frame(female = 1, polknow_factual=seq(0,1,length=6)))
                          , value=seq(0,1,length=6), Gender="Female"
                          , Variable="Factual Knowledge")
             # , data.frame(sim(m3c, iv=data.frame(female = 0, polknow_office=seq(0,1,length=5)))
             #              , value=seq(0,1,length=5), Gender="Male"
             #              , Variable="Office Recognition")
             # , data.frame(sim(m3c, iv=data.frame(female = 1, polknow_office=seq(0,1,length=5)))
             #              , value=seq(0,1,length=5), Gender="Female"
             #              , Variable="Office Recognition")
             # , data.frame(sim(m3d, iv=data.frame(female = 0, polknow_majority=seq(0,1,length=3)))
             #              , value=seq(0,1,length=3), Gender="Male"
             #              , Variable="Majorities in Congress")
             # , data.frame(sim(m3d, iv=data.frame(female = 1, polknow_majority=seq(0,1,length=3)))
             #              , value=seq(0,1,length=3), Gender="Female"
             #              , Variable="Majorities in Congress")
             )
res$dvlab <- factor(res$dv, labels = c("Internal Efficacy","External Efficacy"
                                       ,"Non-conv. Participation","Turnout"))

ggplot(res[res$dv%in%c("effic_int","effic_ext"),]
       , aes(x=value, y=mean, col=Gender,ymin=cilo,ymax=cihi, lty=Gender)) + plot_default +
  #geom_errorbar(alpha=.5, width=0) + 
  geom_ribbon(alpha=0.1, lwd=.1) + geom_line() + 
  facet_grid(dvlab~Variable, scale="free") +
  ylab("Expected value of dependent variable") + xlab("Value of sophistication measure")
ggsave("../fig/efficacy.pdf",width=4,height=3)

ggplot(res[res$dv%in%c("part","vote"),]
       , aes(x=value, y=mean, col=Gender,ymin=cilo,ymax=cihi, lty=Gender)) + plot_default +
  #geom_errorbar(alpha=.5, width=0) +
  geom_ribbon(alpha=0.1, lwd=.1) + geom_line() + 
  facet_grid(dvlab~Variable, scale="free") +
  ylab("Expected value of dependent variable") + xlab("Value of sophistication measure")
ggsave("../fig/participation.pdf",width=4,height=3)



########
# sophistication as an independent variable: no gender interaction
########

m4a <- m4b <- m4c <- m4d <- m4e <- m4f <- NULL
m4a[[1]] <- lm(effic_int ~ polknow_text_mean + educ + log(age) + black + relig + mode, data = data)
m4a[[2]] <- lm(effic_ext ~ polknow_text_mean + educ + log(age) + black + relig + mode, data = data)
m4a[[3]] <- lm(part ~ polknow_text_mean + educ + log(age) + black + relig + mode, data = data)
m4a[[4]] <- glm(vote ~ polknow_text_mean + educ + log(age) + black + relig + mode, data = data, family=binomial("logit"))
m4b[[1]] <- lm(effic_int ~ polknow_factual + educ + log(age) + black + relig + mode, data = data)
m4b[[2]] <- lm(effic_ext ~ polknow_factual + educ + log(age) + black + relig + mode, data = data)
m4b[[3]] <- lm(part ~ polknow_factual + educ + log(age) + black + relig + mode, data = data)
m4b[[4]] <- glm(vote ~ polknow_factual + educ + log(age) + black + relig + mode, data = data, family=binomial("logit"))
m4c[[1]] <- lm(effic_int ~ polknow_office + educ + log(age) + black + relig + mode, data = data)
m4c[[2]] <- lm(effic_ext ~ polknow_office + educ + log(age) + black + relig + mode, data = data)
m4c[[3]] <- lm(part ~ polknow_office + educ + log(age) + black + relig + mode, data = data)
m4c[[4]] <- glm(vote ~ polknow_office + educ + log(age) + black + relig + mode, data = data, family=binomial("logit"))
m4d[[1]] <- lm(effic_int ~ polknow_majority + educ + log(age) + black + relig + mode, data = data)
m4d[[2]] <- lm(effic_ext ~ polknow_majority + educ + log(age) + black + relig + mode, data = data)
m4d[[3]] <- lm(part ~ polknow_majority + educ + log(age) + black + relig + mode, data = data)
m4d[[4]] <- glm(vote ~ polknow_majority + educ + log(age) + black + relig + mode, data = data, family=binomial("logit"))
m4e[[1]] <- lm(effic_int ~ polknow_evalpre + educ + log(age) + black + relig, data = data)
m4e[[2]] <- lm(effic_ext ~ polknow_evalpre + educ + log(age) + black + relig, data = data)
m4e[[3]] <- lm(part ~ polknow_evalpre + educ + log(age) + black + relig, data = data)
m4e[[4]] <- glm(vote ~ polknow_evalpre + educ + log(age) + black + relig, data = data, family=binomial("logit"))
m4f[[1]] <- lm(effic_int ~ polknow_evalpost + educ + log(age) + black + relig, data = data)
m4f[[2]] <- lm(effic_ext ~ polknow_evalpost + educ + log(age) + black + relig, data = data)
m4f[[3]] <- lm(part ~ polknow_evalpost + educ + log(age) + black + relig, data = data)
m4f[[4]] <- glm(vote ~ polknow_evalpost + educ + log(age) + black + relig, data = data, family=binomial("logit"))

res <- rbind(sim(m4a, iv=data.frame(polknow_text_mean=range(data$polknow_text_mean, na.rm = T)))
             , sim(m4b, iv=data.frame(polknow_factual=range(data$polknow_factual, na.rm = T)))
             , sim(m4c, iv=data.frame(polknow_office=range(data$polknow_office, na.rm = T)))
             , sim(m4d, iv=data.frame(polknow_majority=range(data$polknow_majority, na.rm = T)))
             , sim(m4e, iv=data.frame(polknow_evalpre=range(data$polknow_evalpre, na.rm = T)))
             , sim(m4f, iv=data.frame(polknow_evalpost=range(data$polknow_evalpost, na.rm = T))))
res$dvlab <- factor(res$dv, labels = c("Internal Efficacy","External Efficacy"
                                       ,"Non-conv. Participation","Turnout"))
res$ivlab <- factor(res$iv, labels = dvnames)

ggplot(res, aes(y=ivlab, x=mean, xmin=cilo, xmax=cihi)) + 
  geom_point() + geom_errorbarh(height=0) + facet_wrap(~dvlab, scale="free_x") +
  geom_vline(xintercept = 0, color="grey") + 
  xlab("Marginal Effect") + ylab("Independent Variable") +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) +
  scale_y_discrete(limits = rev(levels(res$ivlab)))
ggsave("../fig/knoweff.pdf", width=4, height=3)


##### Generate tables for appendix

## create labels
ivlabs <- c("Sex (Female)","Media Exposure","Political Discussion"
            ,"Education (College Degree)","log(Age)","Race (African American)"
            ,"Church Attendance","Survey Mode (Online)")
dvlabs <- c("Text-based","Factual","Office","Majorities","Eval. (Pre)", "Eval. (Post)")
mlabs <- NULL

## create table
stargazer(m1, type="text", keep.stat = c("n","rsq")
          , covariate.labels = ivnames[-1]
          , dep.var.labels = dvlabs
          , dep.var.caption = "Dependent Variable: Political Knowledge Measure"
          , align = TRUE, label="tab:determinants"
          , model.numbers = FALSE, no.space = T
          , star.cutoffs = c(.05,.01,.001)
          , title="Determinants of Political Knowledge."
          , out="../tab/determinants.tex"
          , font.size = "scriptsize"
          , column.sep.width = "-5pt"
          , table.placement="ht")
