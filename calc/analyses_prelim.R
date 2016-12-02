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

setwd("/data/Dropbox/Uni/Projects/2016/knowledge/calc")

## load data and stm results
load("anes.Rdata")
#load("../data/anes_old.Rdata")

source("sim.R")

## plot defaults
plot_default <- theme_classic(base_size=8) + theme(panel.border = element_rect(fill=NA))


########
# correlation matrices: compare with common measures
########

datcor <- data[,c("polknow_factual", "polknow_office", "polknow_majority","polknow_text")]
colnames(datcor) <- paste0("v",1:ncol(datcor))

pdf("../fig/corplot.pdf",width=3, height=3)
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.01, size=.2)), axisLabels="none") + 
  theme_classic() + theme(panel.border = element_rect(fill=NA))
dev.off()


########
# histograms comparing men and women
########

## histogram/density of weighted 
plot_df <- data.frame(rbind(cbind(data$polknow_text, data$female, 1)
                      , cbind(data$polknow_factual, data$female, 2)
                      , cbind(data$polknow_office, data$female, 3)
                      , cbind(data$polknow_majority, data$female, 4))) %>% na.omit()
colnames(plot_df) <- c("Knowledge","Gender","Variable")
plot_df$Gender <- factor(plot_df$Gender, labels = c("Male","Female"))
plot_df$Variable <- factor(plot_df$Variable, labels = c("Text-based sophistication", "Factual Knowledge"
                                                        , "Office Recognition", "Majorities in Congress"))
plot_means <- plot_df %>% group_by(Variable, Gender) %>% summarize_each("mean")

ggplot(plot_df, aes(x=Knowledge, lty=Gender)) + plot_default + 
  geom_density() + facet_wrap(~Variable, scale="free") + 
  xlab("Value on knowledge measure") + ylab("Density") +
  geom_vline(aes(xintercept = Knowledge, linetype = Gender), data=plot_means)
ggsave("../fig/densities.pdf", width=6, height=3.5)


########
# determinants of political knowledge
########

m1 <- NULL
m1[[1]] <- lm(polknow_text ~ female + polmedia + poldisc + educ + age + black + relig + mode, data = data)
m1[[2]] <- lm(polknow_factual ~ female + polmedia + poldisc + educ + age + black + relig + mode, data = data)
m1[[3]] <- lm(polknow_office ~ female + polmedia + poldisc + educ + age + black + relig + mode, data = data)
m1[[4]] <- lm(polknow_majority ~ female + polmedia + poldisc + educ + age + black + relig + mode, data = data)
lapply(m1, summary)

dvnames <- c("Text-based Sophistication","Factual Knowledge","Office Recognition"
             ,"Majorities in Congress")
ivnames <- c("Intercept", "Gender (Female)", "Media exposure", "Political discussions", "Education"
             , "Age", "Race (Black)", "Religiosity", "Survey Mode (Online)")

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
  geom_point() + geom_errorbarh(height = 0) + facet_wrap(~dv, scales="free_x",ncol=2) +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA), axis.title=element_blank())
ggsave("../fig/determinants.pdf",width=6,height=3.5)


########
# closing the knowledge gap
########

m2 <- NULL
m2[[1]] <- lm(polknow_text ~ female * polmedia + female * poldisc + educ + age + black + relig + mode + wordsum, data = data)
m2[[2]] <- lm(polknow_factual ~ female * polmedia + female * poldisc + educ + age + black + relig + mode + wordsum, data = data)
m2[[3]] <- lm(polknow_office ~ female * polmedia + female * poldisc + educ + female + age + black + relig + mode + wordsum, data = data)
m2[[4]] <- lm(polknow_majority ~ female * polmedia + female * poldisc + educ + female + age + black + relig + mode + wordsum, data = data)

res <- rbind(data.frame(sim(m2, iv=data.frame(female = 0, polmedia=seq(0,1,length=10)))
                        ,value=seq(0,1,length=10),Variable="Media Exposure",Gender="Male")
             , data.frame(sim(m2, iv=data.frame(female = 0, poldisc=seq(0,1,length=10)))
                          ,value=seq(0,1,length=10),Variable="Political Discussion",Gender="Male")
             , data.frame(sim(m2, iv=data.frame(female = 1, polmedia=seq(0,1,length=10)))
                          ,value=seq(0,1,length=10),Variable="Media Exposure",Gender="Female")
             , data.frame(sim(m2, iv=data.frame(female = 1, poldisc=seq(0,1,length=10)))
                          ,value=seq(0,1,length=10),Variable="Political Discussion",Gender="Female"))
res$dvlab <- factor(res$dv, labels = dvnames)

ggplot(res, aes(x=value, y=mean, lty=Gender,ymin=cilo,ymax=cihi)) + plot_default +
  geom_line() + geom_errorbar(col="grey", width=0.01) + 
  facet_grid(dvlab~Variable, scale="free_y") +
  ylab("Expected sophistication") + xlab("Value of independent variable")
ggsave("../fig/closing.pdf",width=6,height=4.5)


########
# sophistication as an independent variable
########

summary(lm(effic_int ~ polknow_text*female + educ + age + black + relig + mode, data = data))
summary(lm(effic_int ~ polknow_factual*female + educ + age + black + relig + mode, data = data))
summary(lm(effic_int ~ polknow_office*female + educ + age + black + relig + mode, data = data))
summary(lm(effic_int ~ polknow_majority*female + educ + age + black + relig + mode, data = data))

summary(lm(effic_ext ~ polknow_text*female + educ + age + black + relig + mode, data = data))
summary(lm(effic_ext ~ polknow_factual*female + educ + age + black + relig + mode, data = data))
summary(lm(effic_ext ~ polknow_office*female + educ + age + black + relig + mode, data = data))
summary(lm(effic_ext ~ polknow_majority*female + educ + age + black + relig + mode, data = data))

summary(lm(part ~ polknow_text*female + educ + age + black + relig + mode, data = data))
summary(lm(part ~ polknow_factual*female + educ + age + black + relig + mode, data = data))
summary(lm(part ~ polknow_office*female + educ + age + black + relig + mode, data = data))
summary(lm(part ~ polknow_majority*female + educ + age + black + relig + mode, data = data))

summary(glm(vote ~ polknow_text*female + educ + age + black + relig + mode, data = data, family=binomial("logit")))
summary(glm(vote ~ polknow_factual*female + educ + age + black + relig + mode, data = data, family=binomial("logit")))
summary(glm(vote ~ polknow_office*female + educ + age + black + relig + mode, data = data, family=binomial("logit")))
summary(glm(vote ~ polknow_majority*female + educ + age + black + relig + mode, data = data, family=binomial("logit")))



###########################
# old analyses
###########################



