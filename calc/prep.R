### ============================= ###
### Text as Data - Final Project  ###
### Laura Buchanan, Patrick Kraft ###
### ============================= ###
## This file prepares the survey data of the 2008 and 2012 ANES for subsequent analyses



### load raw data

rm(list = ls())
library(car)
library(dplyr)
library(readstata13)
raw2012 <- read.dta13("../data/anes_timeseries_2012.dta", convert.factors = F)
#raw2008 <- read.dta13("../data/anes_timeseries_2008.dta", convert.factors = F)



### 2012 data

## respondent id
anes2012 <- data.frame(id=raw2012$caseid)

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
anes2012$ideol_ct <- recode(raw2012$libcpre_self, "lo:0=NA")

## strength of ideology
anes2012$ideol_str <- abs(recode(raw2012$libcpre_self, "lo:0=NA") - 4)

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
anes2012spanish <- as.numeric(raw2012$profile_spanishsurv == 1 |
                              raw2012$admin_pre_lang_start == 2 |
                              raw2012$admin_post_lang_start == 2)



### 2008 data


