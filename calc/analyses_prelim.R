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

## Graphical display of estimated topic proportions 
plot.STM(stm_fit, type = "summary")
plot.STM(stm_fit, type = "perspectives", topics = c(18,20))
topic_words <- labelTopics(stm_fit)


# correlation matrices
datcor <- data[,c("polknow_factual", "polknow_office", "polknow_majority", "polknow_eval", "int"
                  , "lwc","topic_diversity","ditem","polknow_text")]
colnames(datcor) <- paste0("v",1:ncol(datcor))

corrm <- cor(datcor,use="pairwise.complete.obs", method = "pearson")
corrplot(corrm,method="square")

png("../fig/corplot.png",width=7, height=7, units="in",res=300)
ggpairs(datcor, lower = list(continuous = wrap("smooth", alpha =.02, size=.2)), axisLabels="none") + theme_classic() + theme(panel.border = element_rect(fill=NA))
dev.off()



########
# code for word count plots
########

## histogram/density of lwc
lwc_mean = mean(data$lwc)

p1 <- ggplot(data, aes(lwc, ..density..)) + geom_histogram(binwidth = 0.05, fill='grey') + geom_density() + theme_classic(base_size = 8) + 
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

ggplot(data, aes(topic_diversity, ..density..)) + geom_histogram(binwidth = 0.01,fill='grey') + geom_density() + theme_classic() + 
  theme(panel.border = element_rect(fill=NA)) + geom_vline(xintercept = diversity_mean, colour="red", linetype = "longdash") + theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())

## histogram/density of ditem
ditem_mean = mean(data$ditem)

ggplot(data, aes(ditem, ..density..)) + geom_histogram(binwidth = 0.01,fill='grey') + geom_density() + theme_classic() + 
  theme(panel.border = element_rect(fill=NA)) + geom_vline(xintercept = ditem_mean, colour="red", linetype = "longdash") + theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())


## histogram/density of weighted 
polknow_text_mean = mean(data$polknow_text)

ggplot(data, aes(polknow_text, ..density..)) + geom_histogram(binwidth = .01,fill='grey') + geom_density() + theme_classic() + 
  theme(panel.border = element_rect(fill=NA)) + geom_vline(xintercept = polknow_text_mean, colour="red", linetype = "longdash") + theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank())


########
# code to get response samples
########

## max and min
varnames <- c("Case ID","Obama (likes)","Obama (dislikes)","Romney (likes)","Romney (dislikes)"
              ,"Democratic party (likes)","Democratic party (dislikes)"
              ,"Republican party (likes)","Republican party (dislikes)")
tab_ex1 <- t(rbind(anes2012opend[anes2012opend$caseid == with(data, caseid[polknow_text==min(polknow_text)])[1],]
                 , anes2012opend[anes2012opend$caseid == with(data, caseid[polknow_text==max(polknow_text)]),]))
rownames(tab_ex1) <- varnames
colnames(tab_ex1) <- c("Minimum","Maximum")
xtable(tab_ex1)

tab_ex2 <- t(rbind(anes2012opend[anes2012opend$caseid == with(filter(data,wc>50 & wc<100), caseid[polknow_text==min(polknow_text)]),]
                   , anes2012opend[anes2012opend$caseid == with(filter(data,wc>50 & wc<100), caseid[polknow_text==max(polknow_text)]),]))
rownames(tab_ex2) <- varnames
colnames(tab_ex2) <- c("Minimum","Maximum")
xtable(tab_ex2)

arrange(data, polknow_text) %>% filter(wc>50 & wc<100) %>% dplyr::select(resp, polknow_text) %>% head()
arrange(data, polknow_text) %>% filter(wc>50 & wc<100) %>% dplyr::select(resp, polknow_text) %>% tail()


########
# code to test common findings
########

m1 <- NULL
m1[[1]] <- lm(polknow_text ~ polmedia + poldisc + educ_cont + female + age + black + relig + ideol_ct + pid_cont + mode, data = data)
m1[[2]] <- lm(polknow_factual ~ polmedia + poldisc + educ_cont + female + age + black + relig + ideol_ct + pid_cont + mode, data = data)
m1[[3]] <- lm(polknow_office ~ polmedia + poldisc + educ_cont + female + age + black + relig + ideol_ct + pid_cont + mode, data = data)
m1[[4]] <- lm(polknow_majority ~ polmedia + poldisc + educ_cont + female + age + black + relig + ideol_ct + pid_cont + mode, data = data)
m1[[5]] <- lm(polknow_eval ~ polmedia + poldisc + educ_cont + female + age + black + relig + ideol_ct + pid_cont, data = data)

lapply(m1, summary)

dvnames <- c("Text-based Sophistication","Factual Knowledge","Office Recognition"
             ,"Majorities in Congress","Interviewer Evaluation")
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
  geom_point() + geom_errorbarh(height = 0) + facet_wrap(~dv, scales="free",ncol=1) +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA), axis.title=element_blank())
ggsave("../fig/models.pdf",height=6,width=3)
ggsave("../fig/models.png",height=6,width=3,dpi=300)


########
# check validity of knowledge measure similar to Prior
########

m2 <- NULL
m2[[1]] <- lm(redist ~ tax * polknow_text, data=data)
m2[[2]] <- lm(redist ~ tax * polknow_factual, data=data)
m2[[3]] <- lm(redist ~ tax * polknow_text + tax * polknow_factual, data=data)
m2[[4]] <- lm(redist ~ tax * polknow_text + female + age + black + relig + ideol_ct + pid_cont, data=data)
m2[[5]] <- lm(redist ~ tax * polknow_factual + female + age + black + relig + ideol_ct + pid_cont, data=data)
m2[[6]] <- lm(redist ~ tax * polknow_text + tax * polknow_factual + female + age + black + relig + ideol_ct + pid_cont, data=data)
lapply(m2, summary)

## coefs are equivalent to simulated difference in differences
sim(m2, iv=data.frame(polknow_text=c(0,1,0,1),tax=c(0,0,1,1)))
sim(m2, iv=data.frame(polknow_factual=c(0,1,0,1),tax=c(0,0,1,1)))

## prepare dataframe for plotting (sloppy code)
dfplot <- rbind(summary(m2[[1]])$coefficients[2:4,1:2]
                , summary(m2[[2]])$coefficients[2:4,1:2]
                , summary(m2[[3]])$coefficients[2:6,1:2]
                , summary(m2[[4]])$coefficients[c(2,3,10),1:2]
                , summary(m2[[5]])$coefficients[c(2,3,10),1:2]
                , summary(m2[[6]])$coefficients[c(2:4,11,12),1:2])
tmp <- rownames(dfplot); rownames(dfplot) <- NULL; dfplot <- data.frame(dfplot)
dfplot$iv <- gsub(":","_", tmp)
dfplot$ivname <- factor(recode(dfplot$iv, "'tax'='Support Tax Increase'
                        ; 'polknow_text'='Sophistication (Text)'
                        ; 'tax_polknow_text'='Tax X Soph. (Text)'
                        ; 'polknow_factual'='Factual Knowledge'
                        ; 'tax_polknow_factual'='Tax X Factual Know.'")
                        , levels = c("Tax X Factual Know.","Tax X Soph. (Text)"
                                     , "Factual Knowledge", "Sophistication (Text)"
                                     , "Support Tax Increase"))
dfplot$model <- factor(c(rep("Text-based Sophistication",3),rep("Factual Knowledge",3),rep("Both Measures",5)
                  ,rep("Text-based Sophistication",3),rep("Factual Knowledge",3),rep("Both Measures",5))
                  , levels = c("Text-based Sophistication","Factual Knowledge","Both Measures"))
dfplot$cont <- c(rep("No Controls",11),rep("Including Controls",11))

# create factor variables, remove intercept for plotting
#dfplot$ivnames <- factor(dfplot$ivnames, levels = rev(ivnames))
#dfplot$dv <- factor(dfplot$dv, levels = dvnames)
#dfplot <- dfplot[dfplot$ivnames!="Intercept",]

dodge <- position_dodge(width=.75)  
ggplot(dfplot, aes(x=ivname, y=Estimate, col = cont, shape = cont
                   , ymin = Estimate-1.96*Std..Error, ymax = Estimate+1.96*Std..Error)) + 
  geom_hline(yintercept = 0, color="grey") + 
  geom_point(position = dodge) + geom_errorbar(width = 0, position = dodge) + facet_wrap(~model, ncol=1) +
  theme_classic(base_size = 8) + theme(panel.border = element_rect(fill=NA), axis.title=element_blank(), legend.position="none") + 
  scale_color_brewer(palette = "Set1",name = element_blank()) + coord_flip() + scale_shape_discrete(name = element_blank())
ggsave("../fig/models2.pdf",height=3,width=3)

