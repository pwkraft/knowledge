### ============================= ###
### Text as Data - Final Project  ###
### Laura Buchanan, Patrick Kraft ###
### ============================= ###
## runs analyses for paper (based on data created in prep.R)


rm(list = ls())
library(dplyr)
library(quanteda)
library(stm)
library(corrplot)
library(ggplot2)
library(xtable)
library(gridExtra)

if(sessionInfo()$platform == "x86_64-pc-linux-gnu (64-bit)"){
  setwd("/data/Dropbox/Uni/Projects/2016/knowledge/calc")
} else {setwd('/Users/Laura/Desktop/knowledge/data')}

## load data and stm results
load("../data/anes.Rdata")

## Graphical display of estimated topic proportions 
plot.STM(stm_fit, type = "summary")
plot.STM(stm_fit, type = "perspectives", topics = c(18,20))
topic_words <- labelTopics(stm_fit)


# old correlation matrix
corrm <- cor(data[,c("polknow_office", "polknow_factual", "polknow_majority", "polknow_evalpre"
                  , "polknow_evalpost", "intpre", "intpost", "educ_cont", "polmedia", "poldisc"
                  , "wc", "lwc", "nitem", "pitem", "litem", "ditem", "topic_diversity"
                  , "topic_diversity_length", "topic_thresh_sum", "topic_thresh_count"
                  , "topic_diversity_length_litem", "topic_diversity_length_ditem")]
         ,use="pairwise.complete.obs", method = "pearson")

corrplot(corrm,method="square")




#######
# code for correlation matrix plot
#######



corrm <- cor(data[,c("polknow_office", "polknow_factual", "polknow_majority", "polknow_evalpre"
                     , "polknow_evalpost", "intpre", "intpost", "educ_cont"
                     , "lwc",  "ditem", "topic_diversity"
                     , "topic_diversity_length_ditem")]
             ,use="pairwise.complete.obs", method = "pearson")

corrplot(corrm,method="square")



########
# code for word count plots
########

## histogram/density of lwc
lwc_mean = mean(data$lwc)

p1 <- ggplot(data, aes(lwc, ..density..)) + geom_histogram(binwidth = 0.25, fill='grey') + geom_density() + theme_classic(base_size = 8) + 
  theme(panel.border = element_rect(fill=NA)) + geom_vline(xintercept = lwc_mean, colour="red", linetype = 3) + 
  ylab("Density") + xlab("log(Word Count)")

## histogram/density of wc
wc_mean = mean(data$wc)

p2 <- ggplot(data, aes(wc)) + geom_histogram(fill = "grey", binwidth = 25) + theme_classic(base_size = 8) + 
  theme(panel.border = element_rect(fill=NA)) + 
  geom_vline(xintercept = wc_mean, colour="red", linetype = 3) +
  ylab("Number of Respondents") + xlab("Word Count")

pdf("../fig/wc.pdf",width=7, height=2)
grid.arrange(p2, p1, ncol=2)
dev.off()



########
# code for diversity measures
########

## histogram/density of topic_diversity
diversity_mean = mean(data$topic_diversity)

ggplot(data, aes(topic_diversity, ..density..)) + geom_histogram(binwidth = 0.25,fill='grey') + geom_density() + theme_classic() + 
  theme(panel.border = element_rect(fill=NA)) + geom_vline(xintercept = diversity_mean, colour="red", linetype = "longdash") + theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

## histogram/density of ditem
ditem_mean = mean(data$ditem)

ggplot(data, aes(ditem, ..density..)) + geom_histogram(binwidth = 0.25,fill='grey') + geom_density() + theme_classic() + 
  theme(panel.border = element_rect(fill=NA)) + geom_vline(xintercept = ditem_mean, colour="red", linetype = "longdash") + theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())


## histogram/density of weighted 
topic_diversity_length_ditem_mean = mean(data$topic_diversity_length_ditem)

ggplot(data, aes(topic_diversity_length_ditem, ..density..)) + geom_histogram(binwidth = 10,fill='grey') + geom_density() + theme_classic() + 
  theme(panel.border = element_rect(fill=NA)) + geom_vline(xintercept = topic_diversity_length_ditem_mean, colour="red", linetype = "longdash") + theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())


########
# code to get response samples
########

## max and min
varnames <- c("Case ID","Obama (likes)","Obama (dislikes)","Romney (likes)","Romney (dislikes)"
              ,"Democratic party (likes)","Democratic party (dislikes)"
              ,"Republican party (likes)","Republican party (dislikes)")
tab_ex1 <- t(rbind(anes2012opend[anes2012opend$caseid == with(data, caseid[topic_diversity_length_ditem==min(topic_diversity_length_ditem)]),]
                 , anes2012opend[anes2012opend$caseid == with(data, caseid[topic_diversity_length_ditem==max(topic_diversity_length_ditem)]),]))
rownames(tab_ex1) <- varnames
colnames(tab_ex1) <- c("Minimum","Maximum")
xtable(tab_ex1)

tab_ex2 <- t(rbind(anes2012opend[anes2012opend$caseid == with(filter(data,wc>50 & wc<100), caseid[topic_diversity_length_ditem==min(topic_diversity_length_ditem)]),]
                   , anes2012opend[anes2012opend$caseid == with(filter(data,wc>50 & wc<100), caseid[topic_diversity_length_ditem==max(topic_diversity_length_ditem)]),]))
rownames(tab_ex2) <- varnames
colnames(tab_ex2) <- c("Minimum","Maximum")
xtable(tab_ex2)

arrange(data, topic_diversity_length_ditem) %>% filter(wc>50 & wc<100) %>% select(resp, topic_diversity_length_ditem) %>% head()
arrange(data, topic_diversity_length_ditem) %>% filter(wc>50 & wc<100) %>% select(resp, topic_diversity_length_ditem) %>% tail()


########
# code to test common findings
########

m1 <- NULL
m1[[1]] <- lm(topic_diversity_length_ditem ~ polmedia + poldisc + educ_cont + female + age + black + relig + ideol_ct + pid_cont + mode, data = data)
m1[[2]] <- lm(polknow_office ~ polmedia + poldisc + educ_cont + female + age + black + relig + ideol_ct + pid_cont + mode, data = data)
m1[[3]] <- lm(polknow_factual ~ polmedia + poldisc + educ_cont + female + age + black + relig + ideol_ct + pid_cont + mode, data = data)
m1[[4]] <- lm(polknow_majority ~ polmedia + poldisc + educ_cont + female + age + black + relig + ideol_ct + pid_cont + mode, data = data)
m1[[5]] <- lm(polknow_evalpre ~ polmedia + poldisc + educ_cont + female + age + black + relig + ideol_ct + pid_cont, data = data)
m1[[6]] <- lm(polknow_evalpost ~ polmedia + poldisc + educ_cont + female + age + black + relig + ideol_ct + pid_cont, data = data)

lapply(m1, summary)

dvnames <- c("Text-based Sophistication Measure","Office Recognition","Factual Knowledge"
             ,"Majorities in Congress","Interviewer Evaluation (pre)"
             ,"Interviewer Evaluation (post)")
ivnames <- c("Intercept","Media exposure", "Political discussions", "Education"
             , "Gender (Female)", "Age", "Race (Black)", "Religiosity"
             , "Ideology", "Party Identification", "Survey Mode (Online)")

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
  geom_vline(xintercept = 0, color="grey") + 
  geom_point() + geom_errorbarh(height = 0) + facet_wrap(~dv, scales="free",ncol=2) +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA)) + 
  ylab("Independent Variables")
ggsave("../fig/models.pdf",height=5,width=7)

