### ============================================================= ###
### Measuring Political Sophistication using Open-ended Responses ###
### Patrick Kraft                                                 ###
### ============================================================= ###
## prepares the yougov data for subsequent analyses
## prepares open-ended responses (selecting variables, spell checking etc.)
## fits structural topic model for open-ended responses
## creates diversity measures


### load raw data

rm(list = ls())
library(car)
library(dplyr)
library(quanteda)
library(stm)
library(ineq)
library(ggplot2)
setwd("/data/Dropbox/Uni/Projects/2016/knowledge/")
datasrc <- "/data/Dropbox/Uni/Data/YouGov/"
raw <- read.csv(paste0(datasrc,"STBR0007_OUTPUT.csv"))


### closed items in yougov data

## respondent id
yougov <- data.frame(caseid=raw$caseid)

## treatments in study 1
raw$disgust <- raw$treat_rand1 == 3 | raw$treat_rand1 == 4
raw$anxiety <- raw$treat_rand1 == 2 | raw$treat_rand1 == 4

## recode missing values in disease knowledge question
raw$Q13[is.na(raw$Q13)] <- 8
raw$Q14[is.na(raw$Q14)] <- 8

## knowledge about diseases (study 1)
yougov$know_dis <- with(raw, (Q12_1==1)
                        + ((Q12_2==1)*!disgust) + ((Q12_2==2)*disgust)
                        + ((Q12_3==2)*!disgust) + ((Q12_3==1)*disgust)
                        + ((Q12_4==1)*!disgust) + ((Q12_4==2)*disgust)
                        + ((Q12_5==2)*!disgust) + ((Q12_5==1)*disgust)
                        + (Q12_6==2) + (Q12_7==2)
                        + (Q13==1) + (Q14==2))

## recode missing values in political knowledge questions
raw$Q24[is.na(raw$Q24)] <- 8
raw$Q25[is.na(raw$Q25)] <- 8
raw$Q26[is.na(raw$Q26)] <- 8
raw$Q27[is.na(raw$Q27)] <- 8
raw$Q28[is.na(raw$Q28)] <- 8
raw$Q29[is.na(raw$Q29)] <- 8
raw$Q30[is.na(raw$Q30)] <- 8
raw$Q31[is.na(raw$Q31)] <- 8

## political knowledge (study 3) CHECK ANSWERS!
yougov$know_pol <- with(raw, (Q24==3) + (Q25==1) + (Q26==1) + (Q27==2)
                        + (Q28==2) + (Q29==1) + (Q30==2) + (Q31==1))

## education (bachelor degree)
yougov$educ <- raw$educ>=5

## education (continuous)
yougov$educ_cont <- (raw$educ-1)/5

## ideology (factor/dummies)
yougov$ideol <- factor(car::recode(raw$ideo5, "1:2=1; 3=2; 4:5=3; else=NA")
                         , labels = c("Liberal","Moderate","Conservative"))
yougov$ideol_lib <- as.numeric(yougov$ideol=="Liberal")
yougov$ideol_con <- as.numeric(yougov$ideol=="Conservative")

## ideology (continuous, -1 to 1)
yougov$ideol_ct <- (car::recode(raw$ideo5, "6=NA") - 3)/2

## strength of ideology
yougov$ideol_str <- abs(yougov$ideol_ct)

## party identification (factor/dummies)
yougov$pid <- factor(car::recode(raw$pid3, "1=1; 2=3; 3=2; else=NA")
                       , labels = c("Democrat","Independent","Republican"))
yougov$pid_dem <- as.numeric(yougov$pid=="Democrat")
yougov$pid_rep <- as.numeric(yougov$pid=="Republican")

## pid continuous
yougov$pid_cont <- (car::recode(raw$pid7, "8:hi=NA") - 4)/3

## interaction: pid * education
yougov$educ_pid <- yougov$educ_cont * yougov$pid_cont

## strength of partisanship
yougov$pid_str <- abs(yougov$pid_cont)

## religiosity (church attendance)
yougov$relig <- (6 - car::recode(raw$pew_churatd, "7:hi=NA"))/5

## age
yougov$age <- 2015 - raw$birthyr

## sex
yougov$female <- raw$gender - 1  

## race
yougov$black <- as.numeric(raw$race == 2)

## income
yougov$faminc <- (car::recode(raw$faminc, "31=12; 97:hi=NA") -1)/15



### open-ended responses
### MORE WORK ON PRE-PROCESSING NEEDED, check all steps, spell checking, stopword removal etc.

## minor pre-processing
opend <- apply(select(raw, Q2, Q3, Q5, Q6), 2, function(x){
    x <- gsub("(^\\s+|\\s+$)","", x)
    x[x %in% c("N/A","n/a","na","Na","__NA__","no","not sure","none","nothing","good"
               ,"don't know","don't no","","I have no clue","I do not know")] <- ""
    x <- gsub("//"," ", x , fixed = T)
    x <- gsub("\\s+"," ", x)
    x <- gsub("(^\\s+|\\s+$)","", x)
    return(x)
})

## spell-checking
write.table(opend, file = "calc/out/spell.csv"
            , sep = ",", col.names = F, row.names = F)
spell <- aspell("calc/out/spell.csv") %>%
  filter(Suggestions!="NULL")

## replace incorrect words
for(i in 1:nrow(spell)){
  opend[spell$Line[i],] <- gsub(spell$Original[i], unlist(spell$Suggestions[i])[1]
                                        , opend[spell$Line[i],])
}
opend <- data.frame(caseid = raw$caseid, opend, stringsAsFactors = F)


### add meta information about responses

## overall response length
yougov$wc <- apply(opend[,-1], 1, function(x){
  length(unlist(strsplit(x,"\\s+")))
})
yougov$lwc <- log(yougov$wc)/max(log(yougov$wc))

## number of items answered
yougov$nitem <- apply(opend[,-1] != "", 1, sum, na.rm = T)

## diversity in item response
yougov$ditem <- apply(opend[,-1], 1, function(x){
  iwc <- unlist(lapply(strsplit(x,"\\s+"), length))
  1 - ineq(iwc,type="Gini")
})


### fit structural topic model

### prepare data

## combine regular survey and open-ended data, remove spanish and empty responses
meta <- c("age", "educ_cont", "pid_cont", "educ_pid")
data <- yougov %>% mutate(resp = apply(opend[,-1],1,paste,collapse=' '))

## remove additional whitespaces
data$resp <- gsub("\\s+"," ", data$resp)
data$resp <- gsub("(^\\s+|\\s+$)","", data$resp)

## remove missings on metadata
data <- data[apply(!is.na(data[,meta]),1,prod)==1,]

## process for stm
processed <- textProcessor(data$resp, metadata = data[,meta]
                           , customstopwords = c("dont", "just", "hes", "that"))
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

## remove discarded observations from data
data <- data[-processed$docs.removed,]
#data <- data[-out$docs.removed,]

## quick fit (60 topics)
# stm_fit <- stm(out$documents, out$vocab, prevalence = as.matrix(out$meta)
#                , K=60, init.type = "Spectral")

## slow, smart fit: estimates about 80 topics, might be too large (also, K is not deterministic here)
stm_fit <- stm(out$documents, out$vocab, prevalence = as.matrix(out$meta)
               , K=0, init.type = "Spectral")


### create new sophistication measures

## probability fits and transform for diversity score  
doc_topic_prob <- stm_fit$theta

## topic diversity score
data$topic_diversity <- apply(doc_topic_prob, 1, function(x) 1 - ineq(x,type="Gini"))

## text-based sophistication measures
data$polknow_text <- with(data, topic_diversity * lwc * ditem)
data$polknow_text_mean <- with(data, (topic_diversity + lwc + ditem)/3)


### compare measures
summary(lm(know_dis ~ polknow_text_mean + know_pol, data=data))
summary(lm(know_dis ~ polknow_text_mean + know_pol + female + log(age) + black + relig + educ + faminc, data=data))
## argument: text-based sophistication is a better measure of competence in the sense that the respondents pick up information about the disease

## closing gender gap is replicated!
summary(lm(polknow_text_mean ~ female + educ + log(age) + black + relig + educ + faminc, data=data))
summary(lm(know_pol ~ female + educ + log(age) + black + relig + educ + faminc, data=data))
summary(lm(know_dis ~ female + educ + log(age) + black + relig + educ + faminc, data=data))

### save output

save(yougov, opend, spell, data, meta, processed, out, stm_fit
     , file="calc/out/yougov.Rdata")
