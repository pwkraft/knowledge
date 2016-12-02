### ============================================================= ###
### Measuring Political Sophistication using Open-ended Responses ###
### Patrick Kraft                                                 ###
### ============================================================= ###
## prepares the survey data of the 2008 and 2012 ANES for subsequent analyses
## prepares open-ended responses (selecting variables, spell checking etc.)
## fits structural topic model for open-ended responses
## creates diversity measures


### load raw data

rm(list = ls())
library(car)
library(dplyr)
library(quanteda)
library(stm)
library(readstata13)
library(ineq)
setwd("/data/Dropbox/Uni/Projects/2016/knowledge/")
datasrc <- "/data/Dropbox/Uni/Data/anes2012/"
raw2012 <- read.dta13(paste0(datasrc,"anes_timeseries_2012.dta"), convert.factors = F)


### 2012 regular survey data

## respondent id
anes2012 <- data.frame(caseid=raw2012$caseid)

## interview mode (1=FTF, 2=online)
anes2012$mode <- raw2012$mode

## political knowledge (office recognition, post-election)
anes2012$polknow_office <- with(raw2012, (recode(ofcrec_speaker_correct, "lo:-1=NA")
                                + recode(ofcrec_vp_correct, "lo:-1=NA")
                                + recode(ofcrec_pmuk_correct, "lo:-1=NA")
                                + recode(ofcrec_cj_correct, "lo:-1=NA"))/4)

## political knowledge (factual knowledge questions, pre-election)
anes2012$polknow_factual <- with(raw2012, ((preknow_prestimes==2) + (preknow_sizedef==1)
                                 + (preknow_senterm==6) + (preknow_medicare==1)
                                 + (preknow_leastsp==1))/5)

## political knowledge (majorities in congress, post-election)
anes2012$polknow_majority <- with(raw2012, ((recode(raw2012$knowl_housemaj, "c(-6,-7)=NA")==2)
                                  + (recode(raw2012$knowl_senmaj, "c(-6,-7)=NA")==1))/2)

## political knowledge (interviewer evaluation, only in F2F part!)
anes2012$polknow_evalpre <- (5 - recode(raw2012$iwrobspre_levinfo, "lo:-1=NA"))/4
anes2012$polknow_evalpost <- (5 - recode(raw2012$iwrobspost_levinfo, "lo:-1=NA"))/4
anes2012$polknow_eval <- (anes2012$polknow_evalpre + anes2012$polknow_evalpost)/2

## intelligence (interviewer evaluation, only in F2F part!)
anes2012$intpre <- (5 - recode(raw2012$iwrobspre_intell, "lo:-1=NA"))/4
anes2012$intpost <- (5 - recode(raw2012$iwrobspost_intell, "lo:-1=NA"))/4
anes2012$int <- (anes2012$intpre + anes2012$intpost)/2

## education (bachelor degree)
anes2012$educ <- as.numeric(raw2012$dem_edugroup_x >= 4)
anes2012$educ[raw2012$raw2012$dem_edugroup_x < 0] <- NA

## education (continuous)
anes2012$educ_cont <- (recode(raw2012$dem_edugroup_x, "lo:0=NA") - 1)/4

## political media exposure
anes2012$polmedia <- with(raw2012, recode(prmedia_wkinews, "lo:-4=NA; -1=0")
                          + recode(prmedia_wktvnws, "lo:-4=NA; -1=0")
                          + recode(prmedia_wkpaprnws, "lo:-4=NA; -1=0")
                          + recode(prmedia_wkrdnws, "lo:-4=NA; -1=0")) / 28

## political discussion
anes2012$poldisc <- recode(raw2012$discuss_discpstwk, "lo:-1 = NA")/7
anes2012$poldisc[raw2012$discuss_disc>1] <- 0

## political interest (pay attention to politics)
anes2012$polint_att <- 5 - recode(raw2012$interest_attention, "lo:-1 = NA")/4

## political interest (following campaign)
anes2012$polint_cam <- 3 - recode(raw2012$interest_following, "lo:-1 = NA")/2

## overall political interest
anes2012$polint <- with(anes2012, (polint_att + polint_cam)/2)

## internal efficacy
anes2012$effic_int <- with(raw2012, recode(effic_complicstd, "lo:-8=NA; -1=1") - 1
                           + recode(effic_complicrev, "lo:-8=NA; -1=1") - 1
                           - recode(effic_undstd, "lo:-8=NA; -1=5") + 5
                           - recode(effic_undrev, "lo:-8=NA; -1=5") + 5
                           ) / 8

## external efficacy
anes2012$effic_ext <- with(raw2012, recode(effic_carestd, "lo:-8=NA; -1=1") - 1
                           - recode(effic_carerev, "lo:-8=NA; -1=5") + 5
                           + recode(effic_saystd, "lo:-8=NA; -1=1") - 1
                           - recode(effic_sayrev, "lo:-8=NA; -1=5") + 5
                           ) / 8

## overall efficacy
anes2012$effic <- with(anes2012, (effic_int + effic_ext)/2)

## voted in previous election
anes2012$pastvote <- recode(raw2012$interest_voted2008, "c(2,5)=0; lo:-1=NA")

## voted in current election
anes2012$vote <- recode(raw2012$rvote2012_x, "2=0; lo:-1=NA")

## participated in protest march / rally
anes2012$protest <- recode(raw2012$dhsinvolv_march, "c(2,5)=0; lo:-1=NA")

## letter to congressman/senator
anes2012$letter <- recode(raw2012$dhsinvolv_contact1, "2=0; lo:-1=NA")

## signed a petition
anes2012$petition <- as.numeric((recode(raw2012$dhsinvolv_netpetition, "c(2,5)=0; lo:-1=NA") +
                                   recode(raw2012$dhsinvolv_petition, "c(2,5)=0; lo:-1=NA")) > 0)

## wear a campaign button
anes2012$button <- recode(raw2012$mobilpo_sign, "c(2,5)=0; lo:-1=NA")

## additive index protest behavior
anes2012$part <- with(anes2012, as.numeric((protest + petition + button)>0))

## ideology (factor/dummies)
anes2012$ideol <- factor(recode(raw2012$libcpre_self, "1:3=1; 4=2; 5:7=3; else=NA")
                  , labels = c("Liberal","Moderate","Conservative"))
anes2012$ideol_lib <- as.numeric(anes2012$ideol=="Liberal")
anes2012$ideol_con <- as.numeric(anes2012$ideol=="Conservative")

## ideology (continuous, -1 to 1)
anes2012$ideol_ct <- (recode(raw2012$libcpre_self, "lo:0=NA") - 4)/3

## strength of ideology
anes2012$ideol_str <- abs(anes2012$ideol_ct)

## party identification (factor/dummies)
anes2012$pid <- factor(recode(raw2012$pid_x
                              , "1:2=1; c(3,4,5)=2; 6:7=3; else=NA")
                       , labels = c("Democrat","Independent","Republican"))
anes2012$pid_dem <- as.numeric(dat$pid=="Democrat")
anes2012$pid_rep <- as.numeric(dat$pid=="Republican")

## pid continuous
anes2012$pid_cont <- (recode(raw2012$pid_x, "lo:0=NA") - 4)/3

## interaction: pid * education
anes2012$educ_pid <- anes2012$educ_cont * anes2012$pid_cont

## strength of partisanship
anes2012$pid_str <- abs(anes2012$pid_cont)

## religiosity (church attendance)
anes2012$relig <- (5 - recode(raw2012$relig_churchoft, "lo:0 = NA"))/5
anes2012$relig[raw2012$relig_church != 1] <- 0
anes2012$relig[raw2012$relig_churchwk == 2] <- 1

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

## wordsum literacy test
anes2012$wordsum <- with(raw2012, (wordsum_setb == 5) + (wordsum_setd == 3)
                         + (wordsum_sete == 1) + (wordsum_setf == 3)
                         + (wordsum_setg == 5) + (wordsum_seth == 4)
                         + (wordsum_setj == 1) + (wordsum_setk == 1)
                         + (wordsum_setl == 4) + (wordsum_seto == 2))/10

## Pro-redistribution attitude: (new scale: 0-1)
## Services and spending tradeoff placement (1-7, max = increase spending)
## Standard of living (1-7, max = gov't should let each person get ahead on their own)
anes2012$redist <- (recode(raw2012$spsrvpr_ssself, "lo:0 = NA") - recode(raw2012$guarpr_self, "lo:0 = NA") + 6)/12

## Support tax increases (new scale: 0-1)
## favor tax on millionaires
## raising personal inc tax for over 250K inc to reduce deficit
anes2012$tax <- ((-recode(raw2012$milln_milltax_x, "lo:0 = NA") + 7)/3 + recode(raw2012$budget_rdef250k, "lo:0 = NA; 1=2; 2=0; 3=1"))/4


### 2012 open-ended responses

## read original open-ended responses (downloaded from anes website)
anes2012pre <- read.csv(paste0(datasrc,"anes2012TS_openends.csv"), as.is = T) %>%
  select(caseid, candlik_likewhatdpc, candlik_dislwhatdpc, candlik_likewhatrpc, candlik_dislwhatrpc
         , ptylik_lwhatdp, ptylik_dwhatdp, ptylik_lwhatrp, ptylik_dwhatrp)
#anes2012post <- read.csv(paste0(datasrc,"anes2012TS_post.csv"), as.is = T) %>%
#  select(caseid, mip_prob1, mip_prob2, mip_prob3, mip_mostprob)
#anes2012opend <- merge(anes2012pre, anes2012post)

## optional: only select likes/dislikes
anes2012opend <- anes2012pre

## minor pre-processing
anes2012spell <- apply(anes2012opend[,-1], 2, function(x){
    x <- gsub("(^\\s+|\\s+$)","", x)
    x[x %in% c("-1 Inapplicable","-7 Refused","N/A","no","none","#(43042)","i am","Nome")] <- ""
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
write.table(anes2012spell, file = "../data/anes2012TS_combined.csv"
            , sep = ",", col.names = F, row.names = F)
spell <- aspell("out/anes2012TS_combined.csv") %>%
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
anes2012$lwc <- log(anes2012$wc)/max(log(anes2012$wc))

## number of items answered
anes2012$nitem <- apply(anes2012spell[,-1] != "", 1, sum, na.rm = T)

## diversity in item response
anes2012$ditem <- apply(anes2012spell[,-1], 1, function(x){
  iwc <- unlist(lapply(strsplit(x,"\\s+"), length))
  1 - ineq(iwc,type="Gini")
})


### fit structural topic model

### prepare data

## combine regular survey and open-ended data, remove spanish and empty responses
meta <- c("age", "educ_cont", "pid_cont", "educ_pid")
data <- anes2012 %>% mutate(resp = apply(anes2012spell[,-1],1,paste,collapse=' ')) %>%
  filter(spanish == 0 & wc != 0)

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
data <- data[-out$docs.removed,]

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


### save output

save(anes2012, anes2012opend, anes2012spell, data, meta, processed, out, stm_fit
     , file="out/anes.Rdata")
