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
gc()

library(tidyverse)
library(car)
library(dplyr)
library(quanteda)
library(quanteda.dictionaries)
library(stm)
library(ineq)
library(ggplot2)
datasrc <- "/data/Dropbox/Uni/Data/YouGov2015/"
raw <- read.csv(paste0(datasrc,"STBR0007_OUTPUT.csv"))
load("~/Dropbox/Uni/Data/LIWC/liwc2015.Rdata")

source("calc/func.R")


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
                        + (Q13==1) + (Q14==2))/9

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
yougov$polknow_factual <- with(raw, (Q24==3) + (Q25==1) + (Q26==1) + (Q27==2)
                               + (Q28==2) + (Q29==1) + (Q30==2) + (Q31==1))/8
yougov$polknow_factual_scale <- as.numeric(scale(yougov$polknow_factual))

## education (bachelor degree)
yougov$educ <- as.numeric(raw$educ>=5)

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
opend_yg <- apply(dplyr::select(raw, Q2, Q3, Q5, Q6), 2, function(x){
    x <- gsub("(^\\s+|\\s+$)","", x)
    x[x %in% c("N/A","n/a","na","Na","__NA__","no","not sure","none","nothing","good"
               ,"don't know","don't no","","I have no clue","I do not know")] <- ""
    x <- gsub("//"," ", x , fixed = T)
    x <- gsub("\\s+"," ", x)
    x <- gsub("(^\\s+|\\s+$)","", x)
    return(x)
})

## spell-checking
write.table(opend_yg, file = "calc/out/spell.csv"
            , sep = ",", col.names = F, row.names = F)
spell_yg <- aspell("calc/out/spell.csv") %>%
  filter(Suggestions!="NULL")

## replace incorrect words
for(i in 1:nrow(spell_yg)){
  opend_yg[spell_yg$Line[i],] <- gsub(spell_yg$Original[i], unlist(spell_yg$Suggestions[i])[1]
                                        , opend_yg[spell_yg$Line[i],])
}
opend_yg <- data.frame(caseid = raw$caseid, opend_yg, stringsAsFactors = F)



# Text-based political sophistication measure -----------------------------


## Range: Shannon entropy of response lengths ----------------------

### overall response length
yougov$wc <- apply(opend_yg[,-1], 1, function(x){
  sum(str_count(x, "\\w+"))
})
yougov$lwc <- log(yougov$wc)/max(log(yougov$wc), na.rm = T)

### consistency in item response
yougov$range <- apply(opend_yg[,-1], 1, function(x){
  iwc <- str_count(x, "\\w+")
  shannon(iwc/sum(iwc))
})


## Size: Number of topics mentioned -----------------------------

### combine regular survey and open-ended data, remove empty responses
meta <- c("age", "educ_cont", "pid_cont", "educ_pid", "female")
data_yg <- yougov %>% mutate(resp = apply(opend_yg[,-1],1,paste,collapse=' '))

### remove additional whitespaces
data_yg$resp <- gsub("\\s+"," ", data_yg$resp)
data_yg$resp <- gsub("(^\\s+|\\s+$)","", data_yg$resp)

### remove missings on metadata
data_yg <- data_yg[apply(!is.na(data_yg[,meta]),1,prod)==1,]

### process for stm
processed_yougov <- textProcessor(data_yg$resp, metadata = data_yg[,meta]
                                  , customstopwords = c("dont", "hes", "shes", "that", "etc"))
out_yougov <- prepDocuments(processed_yougov$documents, processed_yougov$vocab, processed_yougov$meta
                            , lower.thresh = 10)

### remove discarded observations from data
data_yg <- data_yg[-processed_yougov$docs.removed,]
data_yg <- data_yg[-out_yougov$docs.removed,]

### stm fit with 49 topics
stm_fit_yg <- stm(out_yougov$documents, out_yougov$vocab, prevalence = as.matrix(out_yougov$meta)
               , K=25, seed=12345)

### compute number of considerations
data_yg$size <- ntopics(stm_fit_yg, out_yougov)


## Constraint: LIWC component ---------------------------------------------

yougov_liwc <- liwcalike(data_yg$resp, liwc)

## combine exclusive words and conjunctions (see Tausczik and Pennebaker 2010: 35)
data_yg$constraint <- with(yougov_liwc,
                           (conj + differ) * WC,
                           #Sixltr + discrep + tentat + cause + insight - certain - negate - differ
                           )
# MISSING: Inclusiveness (incl), Inhibition (Inhib) -> replaced by Differentiation (differ)
data_yg$constraint <- data_yg$constraint - min(data_yg$constraint)
data_yg$constraint <- data_yg$constraint / max(data_yg$constraint)


## Merge with full data and save -------------------------------------------

### compute combined measures
data_yg$polknow_text <- with(data_yg, size * range * constraint)
data_yg$polknow_text_mean <- with(data_yg, size + range + constraint)/3
data_yg$polknow_text_scale <- as.numeric(scale(data_yg$polknow_text))


# Save Output -------------------------------------------------------------

save(yougov, opend_yg, spell_yg, data_yg, meta, processed_yougov, out_yougov, stm_fit_yg,
     file="calc/out/yougov.Rdata")
