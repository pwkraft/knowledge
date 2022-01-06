### ============================================================= ###
### Measuring Political Sophistication using Open-ended Responses ###
### Patrick Kraft                                                 ###
### ============================================================= ###
## prepares the survey data of the 2016 ANES for subsequent analyses
## prepares open-ended responses (selecting variables, spell checking etc.)
## fits structural topic model for open-ended responses
## creates diversity measures


### load raw data

rm(list = ls())
gc()

library(tidyverse)
library(car)
library(dplyr)
library(SnowballC)
library(quanteda)
library(quanteda.dictionaries)
library(stm)
library(readstata13)
library(ineq)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

datasrc <- "/data/Dropbox/Uni/Data/anes2016/"
raw2016 <- read.dta13(paste0(datasrc,"anes_timeseries_2016.dta"), convert.factors = F)
load("~/Dropbox/Uni/Data/LIWC/liwc2015.Rdata")

source("calc/func.R")

### 2016 regular survey data

## respondent id
anes2016 <- data.frame(caseid=raw2016$V160001)

## interview mode (1=FTF, 2=online)
anes2016$mode <- raw2016$V160501 - 1

## political knowledge (office recognition, post-election)
anes2016$polknow_office <- with(raw2016, (Recode(V162072, "lo:-1=NA")
                                + Recode(V162073b, "lo:-1=NA")
                                + Recode(V162074b, "lo:-1=NA")
                                + Recode(V162075b, "lo:-1=NA")
                                + Recode(V162076b, "lo:-1=NA"))/5)

## political knowledge (factual knowledge questions, pre-election, -5 is interview breakoff, -9 is refusal)
anes2016$polknow_factual <- with(raw2016, (Recode(V161513, "-5=NA; 6=1; else=0")
                                           + Recode(V161514, "-5=NA; 4=1; else=0")
                                           + Recode(V161515, "-5=NA; 2=1; else=0")
                                           + Recode(V161516, "-5=NA; 2=1; else=0"))/4)

## political knowledge (majorities in congress, post-election)
# not available in 2016

## political knowledge (interviewer evaluation, only in F2F part!)
anes2016$polknow_evalpre <- (5 - Recode(raw2016$V168016, "lo:-1=NA"))/4
anes2016$polknow_evalpost <- (5 - Recode(raw2016$V168112, "lo:-1=NA"))/4
anes2016$polknow_eval <- (anes2016$polknow_evalpre + anes2016$polknow_evalpost)/2

## intelligence (interviewer evaluation, only in F2F part!)
anes2016$intpre <- (5 - Recode(raw2016$V168017, "lo:-1=NA"))/4
anes2016$intpost <- (5 - Recode(raw2016$V168113, "lo:-1=NA"))/4
anes2016$int <- (anes2016$intpre + anes2016$intpost)/2

## education (bachelor degree)
anes2016$educ <- Recode(raw2016$V161270, "c(-9,95,90)=NA; 1:12=0; 13:16=1")

## education (continuous)
anes2016$educ_cont <- Recode(raw2016$V161270, "c(-9,95,90)=NA")

## political media exposure (only one item in 2016)
anes2016$polmedia <- Recode(raw2016$V161008, "lo:-1=NA")/7

## political discussion
anes2016$poldisc <- Recode(raw2016$V162174a, "lo:-1=NA")/7
anes2016$poldisc[raw2016$V162174==2] <- 0

## political interest (pay attention to politics)
anes2016$polint_att <- (5 - Recode(raw2016$V161003, "lo:-1 = NA"))/4

## political interest (following campaign)
anes2016$polint_cam <- (3 - Recode(raw2016$V161004, "lo:-1 = NA"))/2

## overall political interest
anes2016$polint <- with(anes2016, (polint_att + polint_cam)/2)

## internal efficacy
anes2016$effic_int <- with(raw2016, Recode(V162217, "lo:-1=NA") - 1
                           - Recode(V162218, "lo:-1=NA") + 5) / 8

## external efficacy
anes2016$effic_ext <- with(raw2016, Recode(V162215, "lo:-1=NA") - 1
                           + Recode(V162216, "lo:-1=NA") - 1) / 8

## overall efficacy
anes2016$effic <- with(anes2016, (effic_int + effic_ext)/2)

## voted in previous election
anes2016$pastvote <- Recode(raw2016$V161005, "2=0; lo:-1=NA")

## voted in current election
anes2016$vote <- Recode(raw2016$V161030, "2=0; lo:-1=NA")
anes2016$vote[raw2016$V161026==1] <- 1
anes2016$vote[raw2016$V161024x==1] <- 0

## participated in protest march / rally
anes2016$protest <- Recode(raw2016$V162018a, "2=0; lo:-1=NA")

## letter to congressman/senator
anes2016$letter <- Recode(raw2016$V162019, "2=0; lo:-1=NA")

## signed a petition
anes2016$petition <- Recode(raw2016$V162018b, "2=0; lo:-1=NA")

## wear a campaign button
anes2016$button <- Recode(raw2016$V162012, "2=0; lo:-1=NA")

## additive index non-conventional participation
anes2016$part <- with(anes2016, protest + petition + button + letter)

## vote choice (pre-election, 1=Clinton, 2=Trump)
anes2016$vc_pre <- Recode(raw2016$V161031,"lo:-1=NA; 1=1; 2=2; else=3")

## vote choice (post-election, 1=Clinton, 2=Trump)
anes2016$vc_post <- Recode(raw2016$V162034a, "lo:-1=NA; 1=1; 2=2; else=3")
anes2016$vote_rep <- Recode(anes2016$vc_post, "3=NA") - 1

## vote change (pre-post)
anes2016$vc_change <- as.numeric(anes2016$vc_pre == anes2016$vc_post)
#anes2016$vc_change[raw2016$V161026 == 1] <- 1
anes2016$vc_change[is.na(anes2016$vc_pre) & !is.na(anes2016$vc_post)] <- 0
anes2016$vc_change[!is.na(anes2016$vc_pre) & is.na(anes2016$vc_post)] <- 0

## party/candidate placements
anes2016$ideol_ego <- (Recode(raw2016$V161126, "lo:0=NA; 99=NA") - 4)/3
anes2016$spsrvpr_ego <- (Recode(raw2016$V161178, "lo:0=NA; 99=NA") - 4)/3
anes2016$defsppr_ego <- (Recode(raw2016$V161181, "lo:0=NA; 99=NA") - 4)/3
anes2016$inspre_ego <- (Recode(raw2016$V161184, "lo:0=NA; 99=NA") - 4)/3
anes2016$guarpr_ego <- (Recode(raw2016$V161189, "lo:0=NA; 99=NA") - 4)/3
anes2016$aidblack_ego <- (Recode(raw2016$V161198, "lo:0=NA; 99=NA") - 4)/3
anes2016$envjob_ego <- (Recode(raw2016$V161201, "lo:0=NA; 99=NA") - 4)/3

anes2016$ideol_dpc <- (Recode(raw2016$V161128, "lo:0=NA") - 4)/3
anes2016$ideol_rpc <- (Recode(raw2016$V161129, "lo:0=NA") - 4)/3
anes2016$spsrvpr_dpc <- (Recode(raw2016$V161179, "lo:0=NA") - 4)/3
anes2016$spsrvpr_rpc <- (Recode(raw2016$V161180, "lo:0=NA") - 4)/3
anes2016$defsppr_dpc <- (Recode(raw2016$V161182, "lo:0=NA") - 4)/3
anes2016$defsppr_rpc <- (Recode(raw2016$V161183, "lo:0=NA") - 4)/3
anes2016$inspre_dpc <- (Recode(raw2016$V161185, "lo:0=NA") - 4)/3
anes2016$inspre_rpc <- (Recode(raw2016$V161186, "lo:0=NA") - 4)/3
anes2016$guarpr_dpc <- (Recode(raw2016$V161190, "lo:0=NA") - 4)/3
anes2016$guarpr_rpc <- (Recode(raw2016$V161191, "lo:0=NA") - 4)/3
anes2016$aidblack_dpc <- (Recode(raw2016$V161199, "lo:0=NA") - 4)/3
anes2016$aidblack_rpc <- (Recode(raw2016$V161200, "lo:0=NA") - 4)/3
anes2016$envjob_dpc <- (Recode(raw2016$V161202, "lo:0=NA") - 4)/3
anes2016$envjob_rpc <- (Recode(raw2016$V161203, "lo:0=NA") - 4)/3


## ideology (factor/dummies)
anes2016$ideol <- factor(Recode(raw2016$V161126, "1:3=1; 4=2; 5:7=3; else=NA")
                  , labels = c("Liberal","Moderate","Conservative"))
anes2016$ideol_lib <- as.numeric(anes2016$ideol=="Liberal")
anes2016$ideol_con <- as.numeric(anes2016$ideol=="Conservative")

## ideology (continuous, -1 to 1)
anes2016$ideol_ct <- (Recode(raw2016$V161126, "lo:0=NA; 99=NA") - 4)/3

## strength of ideology
anes2016$ideol_str <- abs(anes2016$ideol_ct)

## party identification (factor/dummies)
anes2016$pid <- factor(Recode(raw2016$V161158x
                              , "1:2=1; 3:5=2; 6:7=3; else=NA")
                       , labels = c("Democrat","Independent","Republican"))
anes2016$pid_dem <- as.numeric(anes2016$pid=="Democrat")
anes2016$pid_rep <- as.numeric(anes2016$pid=="Republican")

## pid continuous
anes2016$pid_cont <- (Recode(raw2016$V161158x, "lo:0=NA") - 4)/3

## strength of partisanship
anes2016$pid_str <- abs(anes2016$pid_cont)

## interaction: pid * education
anes2016$educ_pid <- anes2016$educ_cont * anes2016$pid_cont

## religiosity (church attendance)
anes2016$relig <- (5 - Recode(raw2016$V161245, "lo:0 = NA"))/5
anes2016$relig[raw2016$V161244 != 1] <- 0
anes2016$relig[raw2016$V161245a == 2] <- 1

## age
anes2016$age <- Recode(raw2016$V161267, "lo:0 = NA")

## log(age)
anes2016$lage <- log(anes2016$age)

## sex
anes2016$female <- Recode(raw2016$V161342, "c(-9,3)=NA") - 1

## race
anes2016$black <- as.numeric(Recode(raw2016$V161310x, "lo:0 = NA") == 2)

## income
anes2016$faminc <- (Recode(raw2016$V161361x, "lo:0 = NA") -1)/27

## gender of interviewer
# not included in 2016 ANES (?)

## spanish speaking respondent
# not included in 2016 ANES (?)

## wordsum literacy test
anes2016$wordsum <- with(raw2016, Recode(V161497, "lo:-1=NA")
                         + Recode(V161498, "lo:-1=NA") + Recode(V161499, "lo:-1=NA")
                         + Recode(V161500, "lo:-1=NA") + Recode(V161501, "lo:-1=NA")
                         + Recode(V161502, "lo:-1=NA") + Recode(V161503, "lo:-1=NA")
                         + Recode(V161504, "lo:-1=NA") + Recode(V161505, "lo:-1=NA")
                         + Recode(V161500, "lo:-1=NA"))/10

## Pro-redistribution attitude: (new scale: 0-1)
## Services and spending tradeoff placement (1-7, max = increase spending)
## Standard of living (1-7, max = gov't should let each person get ahead on their own)
anes2016$redist <- (anes2016$spsrvpr_ego - anes2016$guarpr_ego + 6)/12

## Support tax increases (new scale: 0-1)
## favor tax on millionaires
## raising personal inc tax for over 250K inc to reduce deficit
anes2016$tax <- Recode(raw2016$V162140, "lo:-1=NA; 2=0; 3=0.5")

### NOTE:
# MAYBE ALSO CHECK THE FEDERAL SPENDING ITEMS (V161205) etc. for consistency
###

## Personality characteristics: Extraversion
anes2016$extraversion <- Recode(raw2016$V162333, "lo:0 = NA")
anes2016$reserved <- Recode(raw2016$V162338, "lo:0 = NA")


#########################
## Correct voting measure


## 1) Party identification (large values = Eepublican)

pid <- Recode(anes2016$pid_cont, "NA=0")


## 2) Issue positions

# expert positions
expert <- anes2016 %>%
  filter(polknow_factual > median(polknow_factual, na.rm = T)) %>%
  summarize_at(vars(spsrvpr_dpc:envjob_rpc), mean, na.rm = T)

# ego positions
issue_rep <- Recode(anes2016$spsrvpr_ego, "NA=0") * expert$spsrvpr_rpc +
  Recode(anes2016$defsppr_ego, "NA=0") * expert$defsppr_rpc +
  Recode(anes2016$inspre_ego, "NA=0") * expert$inspre_rpc +
  Recode(anes2016$guarpr_ego, "NA=0") * expert$guarpr_rpc +
  Recode(anes2016$aidblack_ego, "NA=0") * expert$aidblack_rpc +
  Recode(anes2016$envjob_ego, "NA=0") * expert$envjob_rpc

issue_dem <- Recode(anes2016$spsrvpr_ego, "NA=0") * expert$spsrvpr_dpc +
  Recode(anes2016$defsppr_ego, "NA=0") * expert$defsppr_dpc +
  Recode(anes2016$inspre_ego, "NA=0") * expert$inspre_dpc +
  Recode(anes2016$guarpr_ego, "NA=0") * expert$guarpr_dpc +
  Recode(anes2016$aidblack_ego, "NA=0") * expert$aidblack_dpc +
  Recode(anes2016$envjob_ego, "NA=0") * expert$envjob_dpc


## 3) Closeness to social groups
# (left out military, congress, supreme court, liberals, conservatives, and racial groups)

social_rep <- 0
social_rep_n <- 0
social_dem <- 0
social_dem_n <- 0
experts <- anes2016$polknow_factual > median(anes2016$polknow_factual, na.rm = T)
for(var in c("V162107","V162095","V162106","V162108", "V162096"
             , "V162111", "V162098", "V162099","V162100","V162113"
             , "V162110", "V162112", "V162103","V162105","V162109")){
  tmp <- (Recode(raw2016[,var], "lo:-1=NA; 101:hi=NA") - 50) / 50
  res <- t.test(tmp[experts]~anes2016$vote_rep[experts])
  tmp[is.na(tmp)] <- 0
  ## select only social groups that:
  ## - are positively evaluated by one voting block and negatively by the other
  ## - show significant difference in evaluations
  if((prod(res$estimate) < 0) & (res$p.value < .05)){
    if(diff(res$estimate)>0){
      social_rep <- social_rep + tmp
      social_rep_n <- social_rep_n + 1
    } else {
      social_dem <- social_dem + tmp
      social_dem_n <- social_dem_n + 1
    }
  }
}

## correct voting variables
cv_rpc <- (pid + issue_rep/6 + social_rep/social_rep_n)
cv_dpc <- (-pid + issue_dem/6 + social_dem/social_dem_n)
anes2016$correct_vote <- anes2016$vote_rep == as.numeric(cv_rpc >= cv_dpc)

table(anes2016$correct_vote)/sum(table(anes2016$correct_vote))



#############################
### 2016 open-ended responses
#############################
### MORE WORK ON PRE-PROCESSING NEEDED, check all steps, spell checking, stopword removal etc.

## read original open-ended responses (downloaded from anes website)
anes2016opend <- read.csv(paste0(datasrc,"anes_timeseries_2016_redacted_openends.csv"), as.is = T)

## identify spanish respondents
anes2016$spanish <- as.numeric(cld2::detect_language(apply(anes2016opend[,-1],1,paste,collapse=' ')) != "en")

## minor pre-processing
anes2016spell <- apply(anes2016opend[,-1], 2, function(x){
  x <- char_tolower(x)
  x <- gsub("//"," ", x , fixed = T)
  x <- gsub("\\s+"," ", x)
  x <- gsub("(^\\s+|\\s+$)","", x)
  return(x)
})


## num-lock issue
# maybe look into this later

## fix words without whitespace
# maybe look into this later

## spell-checking
write.table(anes2016spell, file = "calc/out/anes2016oe.csv"
            , sep = ",", col.names = F, row.names = F)
spell <- aspell("calc/out/anes2016oe.csv") %>%
  filter(Suggestions!="NULL")

## replace incorrect words
for(i in 1:nrow(spell)){
  anes2016spell[spell$Line[i],] <- gsub(spell$Original[i], unlist(spell$Suggestions[i])[1]
                                        , anes2016spell[spell$Line[i],])
}
anes2016spell <- data.frame(caseid = anes2016opend$V160001, anes2016spell,stringsAsFactors = F)



# Text-based political sophistication measure -----------------------------


## Consistency: Shannon entropy of response lengths ----------------------

### overall response length
anes2016$wc <- apply(anes2016spell[,-1], 1, function(x){
  sum(str_count(x, "\\w+"))
})
anes2016$lwc <- log(anes2016$wc)/max(log(anes2016$wc), na.rm = T)

### consistency in item response
anes2016$consistency <- apply(anes2016spell[,-1], 1, function(x){
  iwc <- str_count(x, "\\w+")
  shannon(iwc/sum(iwc))
})


## Considerations: Number of topics mentioned -----------------------------

### combine regular survey and open-ended data, remove spanish and empty responses
meta2016 <- c("age", "educ_cont", "pid_cont", "educ_pid", "female")
data2016 <- anes2016 %>% mutate(resp = apply(anes2016spell[,-1],1,paste,collapse=' ')) %>%
  filter(spanish == 0 & wc != 0)

### remove additional whitespaces
data2016$resp <- gsub("\\s+"," ", data2016$resp)
data2016$resp <- gsub("(^\\s+|\\s+$)","", data2016$resp)

### remove missings on metadata
data2016 <- data2016[apply(!is.na(data2016[,meta2016]),1,prod)==1,]

### process for stm
processed2016 <- textProcessor(data2016$resp, metadata = data2016[,meta2016],
                               customstopwords = c("dont", "hes", "shes", "that", "etc"))
out2016 <- prepDocuments(processed2016$documents, processed2016$vocab, processed2016$meta,
                         lower.thresh = 10)

### remove discarded observations from data
data2016 <- data2016[-processed2016$docs.removed,]
data2016 <- data2016[-out2016$docs.removed,]

### stm fit with 49 topics
stm_fit2016 <- stm(out2016$documents, out2016$vocab, prevalence = as.matrix(out2016$meta),
                   K=25, seed=12345)

### compute number of considerations
data2016$considerations <- ntopics(stm_fit2016, out2016)


## Word choice: LIWC component ---------------------------------------------

anes2016_liwc <- liwcalike(data2016$resp, liwc)

### combine exclusive words and conjunctions (see Tausczik and Pennebaker 2010: 35)
data2016$wordchoice <- with(anes2016_liwc,
                            (conj + differ) * WC,
                            #Sixltr + discrep + tentat + cause + insight - certain - negate - differ
                            )
# MISSING: Inclusiveness (incl), Inhibition (Inhib) -> replaced by Differentiation (differ)
data2016$wordchoice <- data2016$wordchoice - min(data2016$wordchoice)
data2016$wordchoice <- data2016$wordchoice / max(data2016$wordchoice)


## Merge with full data and save -------------------------------------------

### compute combined measures
data2016$polknow_text <- with(data2016, considerations * consistency * wordchoice)
data2016$polknow_text_mean <- with(data2016, considerations + consistency + wordchoice)/3



# save output -------------------------------------------------------------

save(anes2016, anes2016opend, anes2016spell, data2016, meta2016, processed2016, out2016, stm_fit2016,
     file="calc/out/anes2016.Rdata")
