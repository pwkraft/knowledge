### ============================= ###
### Text as Data - Final Project  ###
### Laura Buchanan, Patrick Kraft ###
### ============================= ###
## prepares the survey data of the 2008 and 2012 ANES for subsequent analyses
## prepares open-ended responses (selecting variables, spell checking etc.)


### load raw data

rm(list = ls())
library(car)
library(dplyr)
library(readstata13)
if(sessionInfo()$platform == "x86_64-pc-linux-gnu (64-bit)"){
  setwd("/data/Dropbox/Uni/Projects/2016/knowledge/calc")
} else {setwd('/Users/Laura/Desktop/knowledge/data')}
raw2012 <- read.dta13("../data/anes_timeseries_2012.dta", convert.factors = F)
#raw2008 <- read.dta13("../data/anes_timeseries_2008.dta", convert.factors = F)


### 2012 survey data

## respondent id
anes2012 <- data.frame(caseid=raw2012$caseid)

## interview mode (1=FTF, 2=online)
anes2012$mode <- raw2012$mode

## political knowledge (office recognition, post-election)
anes2012$polknow_office <- with(raw2012, recode(ofcrec_speaker_correct, "lo:-1=NA")
                                + recode(ofcrec_vp_correct, "lo:-1=NA")
                                + recode(ofcrec_pmuk_correct, "lo:-1=NA")
                                + recode(ofcrec_cj_correct, "lo:-1=NA"))

## political knowledge (factual knowledge questions, pre-election)
anes2012$polknow_factual <- with(raw2012, (preknow_prestimes==2) + (preknow_sizedef==1)
                                 + (preknow_senterm==6) + (preknow_medicare==1)
                                 + (preknow_leastsp==1))

## political knowledge (majorities in congress, post-election)
anes2012$polknow_majority <- with(raw2012, (recode(raw2012$knowl_housemaj, "c(-6,-7)=NA")==2)
                                  + (recode(raw2012$knowl_housemaj, "c(-6,-7)=NA")==1))

## political knowledge (interviewer evaluation, only in F2F part!)
anes2012$polknow_evalpre <- 5 - recode(raw2012$iwrobspre_levinfo, "lo:-1=NA")
anes2012$polknow_evalpost <- 5 - recode(raw2012$iwrobspost_levinfo, "lo:-1=NA")

## intelligence (interviewer evaluation, only in F2F part!)
anes2012$intpre <- 5 - recode(raw2012$iwrobspre_intell, "lo:-1=NA")
anes2012$intpost <- 5 - recode(raw2012$iwrobspost_intell, "lo:-1=NA")

## education (bachelor degree)
anes2012$educ <- as.numeric(raw2012$dem_edugroup_x >= 4)
anes2012$educ[raw2012$raw2012$dem_edugroup_x < 0] <- NA

## education (continuous)
anes2012$educ_cont <- recode(raw2012$dem_edugroup_x, "lo:0=NA") - 1

## political media exposure
anes2012$polmedia <- with(raw2012, recode(prmedia_wkinews, "lo:-4=NA; -1=0")
                          + recode(prmedia_wktvnws, "lo:-4=NA; -1=0")
                          + recode(prmedia_wkpaprnws, "lo:-4=NA; -1=0")
                          + recode(prmedia_wkrdnws, "lo:-4=NA; -1=0")) / 4

## political discussion
anes2012$poldisc <- recode(raw2012$discuss_discpstwk, "lo:-1 = NA")
anes2012$poldisc[raw2012$discuss_disc>1] <- 0

## voted in previous election
anes2012$pastvote <- recode(raw2012$interest_voted2008, "c(2,5)=0; lo:-1=NA")

## voted in current election
anes2012$vote <- recode(raw2012$rvote2012_x, "2=0; lo:-1=NA")

## intends to vote for democratic presidential candianes2012e
anes2012$vote_dem <- recode(raw2012$prevote_intpreswho, "lo:0=NA; 2=0; c(5,7)=NA")

## ideology (factor/dummies)
anes2012$ideol <- factor(recode(raw2012$libcpre_self, "1:3=1; 4=2; 5:7=3; else=NA")
                  , labels = c("Liberal","Moderate","Conservative"))
anes2012$ideol_lib <- as.numeric(anes2012$ideol=="Liberal")
anes2012$ideol_con <- as.numeric(anes2012$ideol=="Conservative")

## ideology (continuous)
anes2012$ideol_ct <- recode(raw2012$libcpre_self, "lo:0=NA") - 4

## strength of ideology
anes2012$ideol_str <- abs(anes2012$ideol_ct)

## party identification (factor/dummies)
anes2012$pid <- factor(recode(raw2012$pid_x
                              , "1:2=1; c(3,4,5)=2; 6:7=3; else=NA")
                       , labels = c("Democrat","Independent","Republican"))
anes2012$pid_dem <- as.numeric(dat$pid=="Democrat")
anes2012$pid_rep <- as.numeric(dat$pid=="Republican")

## pid continuous
anes2012$pid_cont <- recode(raw2012$pid_x, "lo:0=NA") - 4

## interaction: pid * education
anes2012$educ_pid <- anes2012$educ_cont * anes2012$pid_cont

## strength of partisanship
anes2012$pid_str <- abs(anes2012$pid_cont)

## religiosity (church attendance)
anes2012$relig <- 5 - recode(raw2012$relig_churchoft, "lo:0 = NA")
anes2012$relig[raw2012$relig_church != 1] <- 0
anes2012$relig[raw2012$relig_churchwk == 2] <- 5

## age
anes2012$age <- recode(raw2012$dem_age_r_x, "c(-2,-9,-8) = NA")

## sex
anes2012$female <- raw2012$gender_respondent_x - 1

## race
anes2012$black <- as.numeric(recode(raw2012$dem_raceeth_x, "lo:0 = NA") == 2)

## spanish speaking respondent
anes2012$spanish <- as.numeric(raw2012$profile_spanishsurv == 1 |
                              raw2012$admin_pre_lang_start == 2 |
                              raw2012$admin_post_lang_start == 2)


### 2012 open-ended responses

## read original open-ended responses (downloaded from anes website)
anes2012pre <- read.csv("../data/anes2012TS_pre.csv", as.is = T) %>%
  select(caseid, candlik_likewhatdpc, candlik_dislwhatdpc, candlik_likewhatrpc, candlik_dislwhatrpc
         , ptylik_lwhatdp, ptylik_dwhatdp, ptylik_lwhatrp, ptylik_dwhatrp)
anes2012post <- read.csv("../data/anes2012TS_post.csv", as.is = T) %>%
  select(caseid, mip_prob1, mip_prob2, mip_prob3, mip_mostprob)
anes2012opend <- merge(anes2012pre, anes2012post)

## optional: onlt select likes/dislikes
anes2012opend <- anes2012pre

## minor pre-processing
table(anes2012opend[,4]%in%c("no"))
anes2012spell <- apply(anes2012opend[,-1], 2, function(x){
  x <- gsub("//"," ", x , fixed = T)
  x <- gsub("[[:punct:]]"," ", x)
  x <- gsub("\\s+"," ", x)
  x <- gsub("(^\\s+|\\s+$)","", x)
  x[x %in% c("-1 Inapplicable","-7 Refused","N/A","no","none","#(43042)","i am")] <- ""
  return(x)
})

## num-lock issue
# maybe look into this later

## fix words without whitespace
# maybe look into this later

## spell-checking
write.table(anes2012spell, file = "../data/anes2012TS_combined.csv"
            , sep = ",", col.names = F, row.names = F)
spell <- aspell("../data/anes2012TS_combined.csv") %>%
  filter(Suggestions!="NULL")

## replace incorrect words
for(i in 1:nrow(spell)){
  anes2012spell[spell$Line[i],] <- gsub(spell$Original[i], unlist(spell$Suggestions[i])[1]
                                        , anes2012spell[spell$Line[i],])
}
anes2012spell <- data.frame(caseid = anes2012opend$caseid, anes2012spell,stringsAsFactors = F)


### add meta information about responses

## overall response length
anes2012$wc <- apply(anes2012spell[,-1], 1, function(x){
  length(unlist(strsplit(x,"\\s+")))
})
anes2012$lwc <- log(anes2012$wc)

## number of items answered
anes2012$nitem <- apply(anes2012spell[,-1] != "", 1, sum, na.rm = T)
anes2012$pitem <- anes2012$nitem/ncol(anes2012spell[,-1])
anes2012$litem <- log(anes2012$nitem)

## diversity in item response
anes2012$ditem <- apply(anes2012spell[,-1], 1, function(x){
  iwc <- unlist(lapply(strsplit(x,"\\s+"), length))
  p <- iwc/sum(iwc)
  plp <- p * log2(p)
  plp[is.na(plp)] <- 0
  entrop <- - sum(plp)
  return(entrop)
})


### save output

save(anes2012, anes2012opend, anes2012spell, file = "../data/anes.Rdata")


