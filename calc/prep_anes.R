### ============================================================= ###
### Measuring Political Sophistication using Open-ended Responses ###
### Patrick Kraft                                                 ###
### ============================================================= ###
## prepares the survey data of the 2012 ANES for subsequent analyses
## prepares open-ended responses (selecting variables, spell checking etc.)
## fits structural topic model for open-ended responses
## creates diversity measures


### load raw data

rm(list = ls())
library(car)
library(dplyr)
library(SnowballC)
library(quanteda)
library(stm)
library(readstata13)
library(ineq)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

setwd("/data/Dropbox/Uni/Projects/2016/knowledge/")
datasrc <- "/data/Dropbox/Uni/Data/anes2012/"
raw2012 <- read.dta13(paste0(datasrc,"anes_timeseries_2012.dta"), convert.factors = F)

source("calc/func.R")

### 2012 regular survey data

## respondent id
anes2012 <- data.frame(caseid=raw2012$caseid)

## interview mode (1=FTF, 2=online)
anes2012$mode <- raw2012$mode

## political knowledge (office recognition, post-election)
anes2012$polknow_office <- with(raw2012, (Recode(ofcrec_speaker_correct, "lo:-1=NA")
                                + Recode(ofcrec_vp_correct, "lo:-1=NA")
                                + Recode(ofcrec_pmuk_correct, "lo:-1=NA")
                                + Recode(ofcrec_cj_correct, "lo:-1=NA"))/4)

## political knowledge (factual knowledge questions, pre-election)
anes2012$polknow_factual <- with(raw2012, ((preknow_prestimes==2) + (preknow_sizedef==1)
                                 + (preknow_senterm==6) + (preknow_medicare==1)
                                 + (preknow_leastsp==1))/5)

## political knowledge (majorities in congress, post-election)
anes2012$polknow_majority <- with(raw2012, ((Recode(raw2012$knowl_housemaj, "c(-6,-7)=NA")==2)
                                  + (Recode(raw2012$knowl_senmaj, "c(-6,-7)=NA")==1))/2)

## political knowledge (interviewer evaluation, only in F2F part!)
anes2012$polknow_evalpre <- (5 - Recode(raw2012$iwrobspre_levinfo, "lo:-1=NA"))/4
anes2012$polknow_evalpost <- (5 - Recode(raw2012$iwrobspost_levinfo, "lo:-1=NA"))/4
anes2012$polknow_eval <- (anes2012$polknow_evalpre + anes2012$polknow_evalpost)/2

## intelligence (interviewer evaluation, only in F2F part!)
anes2012$intpre <- (5 - Recode(raw2012$iwrobspre_intell, "lo:-1=NA"))/4
anes2012$intpost <- (5 - Recode(raw2012$iwrobspost_intell, "lo:-1=NA"))/4
anes2012$int <- (anes2012$intpre + anes2012$intpost)/2

## education (bachelor degree)
anes2012$educ <- as.numeric(raw2012$dem_edugroup_x >= 4)
anes2012$educ[raw2012$raw2012$dem_edugroup_x < 0] <- NA

## education (continuous)
anes2012$educ_cont <- (Recode(raw2012$dem_edugroup_x, "lo:0=NA") - 1)/4

## political media exposure
anes2012$polmedia <- with(raw2012, Recode(prmedia_wkinews, "lo:-4=NA; -1=0")
                          + Recode(prmedia_wktvnws, "lo:-4=NA; -1=0")
                          + Recode(prmedia_wkpaprnws, "lo:-4=NA; -1=0")
                          + Recode(prmedia_wkrdnws, "lo:-4=NA; -1=0")) / 28

## political discussion
anes2012$poldisc <- Recode(raw2012$discuss_discpstwk, "lo:-1 = NA")/7
anes2012$poldisc[raw2012$discuss_disc>1] <- 0

## political interest (pay attention to politics)
anes2012$polint_att <- 5 - Recode(raw2012$interest_attention, "lo:-1 = NA")/4

## political interest (following campaign)
anes2012$polint_cam <- 3 - Recode(raw2012$interest_following, "lo:-1 = NA")/2

## overall political interest
anes2012$polint <- with(anes2012, (polint_att + polint_cam)/2)

## internal efficacy
anes2012$effic_int <- with(raw2012, Recode(effic_complicstd, "lo:-8=NA; -1=1") - 1
                           + Recode(effic_complicrev, "lo:-8=NA; -1=1") - 1
                           - Recode(effic_undstd, "lo:-8=NA; -1=5") + 5
                           - Recode(effic_undrev, "lo:-8=NA; -1=5") + 5
                           ) / 8

## external efficacy
anes2012$effic_ext <- with(raw2012, Recode(effic_carestd, "lo:-8=NA; -1=1") - 1
                           - Recode(effic_carerev, "lo:-8=NA; -1=5") + 5
                           + Recode(effic_saystd, "lo:-8=NA; -1=1") - 1
                           - Recode(effic_sayrev, "lo:-8=NA; -1=5") + 5
                           ) / 8

## overall efficacy
anes2012$effic <- with(anes2012, (effic_int + effic_ext)/2)

## voted in previous election
anes2012$pastvote <- Recode(raw2012$interest_voted2008, "c(2,5)=0; lo:-1=NA")

## voted in current election
anes2012$vote <- Recode(raw2012$rvote2012_x, "2=0; lo:-1=NA")

## participated in protest march / rally
anes2012$protest <- Recode(raw2012$dhsinvolv_march, "c(2,5)=0; lo:-1=NA")

## letter to congressman/senator
anes2012$letter <- Recode(raw2012$dhsinvolv_contact1, "2=0; lo:-1=NA")

## signed a petition
anes2012$petition <- as.numeric((Recode(raw2012$dhsinvolv_netpetition, "c(2,5)=0; lo:-1=NA") +
                                   Recode(raw2012$dhsinvolv_petition, "c(2,5)=0; lo:-1=NA")) > 0)

## wear a campaign button
anes2012$button <- Recode(raw2012$mobilpo_sign, "c(2,5)=0; lo:-1=NA")

## additive index non-conventional participation
anes2012$part <- with(anes2012, protest + petition + button + letter)

## vote choice (pre-election)
anes2012$vc_pre <- Recode(raw2012$prevote_intpreswho,"-8=-2; -9=NA")

## vote choice (post-election)
anes2012$vc_post <- Recode(raw2012$postvote_presvtwho, "-9:-6=NA")

## vote change (pre-post)
anes2012$vc_change <- anes2012$vc_pre == anes2012$vc_post
anes2012$vc_change[raw2012$prevote_presvt == 1] <- 1

## party/candidate placements
anes2012$ideol_ego <- Recode(raw2012$libcpre_self, "lo:0=NA")
anes2012$ideol_rpc <- Recode(raw2012$libcpre_rpc, "lo:0=NA")
anes2012$ideol_dpc <- Recode(raw2012$libcpre_dpc, "lo:0=NA")
anes2012$ideol_rep <- Recode(raw2012$libcpre_ptyr, "lo:0=NA")
anes2012$ideol_dem <- Recode(raw2012$libcpre_ptyd, "lo:0=NA")
anes2012$spsrvpr_ego <- Recode(raw2012$spsrvpr_ssself, "lo:0=NA")
anes2012$spsrvpr_rpc <- Recode(raw2012$spsrvpr_ssrpc, "lo:0=NA")
anes2012$spsrvpr_dpc <- Recode(raw2012$spsrvpr_ssdpc, "lo:0=NA")
anes2012$spsrvpr_rep <- Recode(raw2012$spsrvpr_ssrep, "lo:0=NA")
anes2012$spsrvpr_dem <- Recode(raw2012$spsrvpr_ssdem, "lo:0=NA")
anes2012$defsppr_ego <- Recode(raw2012$defsppr_self, "lo:0=NA")
anes2012$defsppr_rpc <- Recode(raw2012$defsppr_rpc, "lo:0=NA")
anes2012$defsppr_dpc <- Recode(raw2012$defsppr_dpc, "lo:0=NA")
anes2012$defsppr_rep <- Recode(raw2012$defsppr_rep, "lo:0=NA")
anes2012$defsppr_dem <- Recode(raw2012$defsppr_dem, "lo:0=NA")
anes2012$inspre_ego <- Recode(raw2012$inspre_self, "lo:0=NA")
anes2012$inspre_rpc <- Recode(raw2012$inspre_rpc, "lo:0=NA")
anes2012$inspre_dpc <- Recode(raw2012$inspre_dpc, "lo:0=NA")
anes2012$inspre_rep <- Recode(raw2012$inspre_rep, "lo:0=NA")
anes2012$inspre_dem <- Recode(raw2012$inspre_dem, "lo:0=NA")
anes2012$guarpr_ego <- Recode(raw2012$guarpr_self, "lo:0=NA")
anes2012$guarpr_rpc <- Recode(raw2012$guarpr_rpc, "lo:0=NA")
anes2012$guarpr_dpc <- Recode(raw2012$guarpr_dpc, "lo:0=NA")
anes2012$guarpr_rep <- Recode(raw2012$guarpr_rep, "lo:0=NA")
anes2012$guarpr_dem <- Recode(raw2012$guarpr_dem, "lo:0=NA")

## ideology (factor/dummies)
anes2012$ideol <- factor(Recode(raw2012$libcpre_self, "1:3=1; 4=2; 5:7=3; else=NA")
                  , labels = c("Liberal","Moderate","Conservative"))
anes2012$ideol_lib <- as.numeric(anes2012$ideol=="Liberal")
anes2012$ideol_con <- as.numeric(anes2012$ideol=="Conservative")

## ideology (continuous, -1 to 1)
anes2012$ideol_ct <- (Recode(raw2012$libcpre_self, "lo:0=NA") - 4)/3

## strength of ideology
anes2012$ideol_str <- abs(anes2012$ideol_ct)

## party identification (factor/dummies)
anes2012$pid <- factor(Recode(raw2012$pid_x
                              , "1:2=1; c(3,4,5)=2; 6:7=3; else=NA")
                       , labels = c("Democrat","Independent","Republican"))
anes2012$pid_dem <- as.numeric(anes2012$pid=="Democrat")
anes2012$pid_rep <- as.numeric(anes2012$pid=="Republican")

## pid continuous
anes2012$pid_cont <- (Recode(raw2012$pid_x, "lo:0=NA") - 4)/3

## interaction: pid * education
anes2012$educ_pid <- anes2012$educ_cont * anes2012$pid_cont

## strength of partisanship
anes2012$pid_str <- abs(anes2012$pid_cont)

## religiosity (church attendance)
anes2012$relig <- (5 - Recode(raw2012$relig_churchoft, "lo:0 = NA"))/5
anes2012$relig[raw2012$relig_church != 1] <- 0
anes2012$relig[raw2012$relig_churchwk == 2] <- 1

## age
anes2012$age <- Recode(raw2012$dem_age_r_x, "c(-2,-9,-8) = NA")

## log(age)
anes2012$lage <- log(anes2012$age)

## sex
anes2012$female <- raw2012$gender_respondent_x - 1

## race
anes2012$black <- as.numeric(Recode(raw2012$dem_raceeth_x, "lo:0 = NA") == 2)

## income
anes2012$faminc <- (Recode(raw2012$incgroup_prepost_x, "lo:0 = NA") -1)/27

## gender of inerviewer
anes2012$iwrmale <- Recode(raw2012$iwrdesc_pre_gender, "lo:0=NA; 2=0")

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
anes2012$redist <- (Recode(raw2012$spsrvpr_ssself, "lo:0 = NA") - Recode(raw2012$guarpr_self, "lo:0 = NA") + 6)/12

## Support tax increases (new scale: 0-1)
## favor tax on millionaires
## raising personal inc tax for over 250K inc to reduce deficit
anes2012$tax <- ((-Recode(raw2012$milln_milltax_x, "lo:0 = NA") + 7)/3 + Recode(raw2012$budget_rdef250k, "lo:0 = NA; 1=2; 2=0; 3=1"))/4

## Personality characteristics: Extraversion
anes2012$extraversion <- Recode(raw2012$tipi_extra, "lo:0 = NA")
anes2012$reserved <- Recode(raw2012$tipi_resv, "lo:0 = NA")

### 2012 open-ended responses
### MORE WORK ON PRE-PROCESSING NEEDED, check all steps, spell checking, stopword removal etc.

## read original open-ended responses (downloaded from anes website)
anes2012opend <- read.csv(paste0(datasrc,"anes2012TS_openends.csv"), as.is = T) %>%
  dplyr::select(caseid, candlik_likewhatdpc, candlik_dislwhatdpc, candlik_likewhatrpc, candlik_dislwhatrpc
         , ptylik_lwhatdp, ptylik_dwhatdp, ptylik_lwhatrp, ptylik_dwhatrp)

## minor pre-processing
anes2012spell <- apply(anes2012opend[,-1], 2, function(x){
  x <- char_tolower(x)
  x <- gsub("(^\\s+|\\s+$)","", x)
  x <- gsub("//"," ", x , fixed = T)
  x <- gsub("[[:punct:]]"," ", x)
  x <- gsub("\\s+"," ", x)
  x <- gsub("(^\\s+|\\s+$)","", x)
  x[x %in% c("1 inapplicable","7 refused","n a","no","none","43042","i am","nome"
             ,"i refuse", "i rwfuse to disclose", "refuse to disclose"
             ,"dk","skip","no5","don t know","same","not really"
             ,"no idea", "can t say","no comment","no views","nope","not at all"
             ,"no i can t","no i cant", "i don t know","iguess not","i dont know"
             , "dont know", "dint care","no no comment","no not really", "again no"
             , "1", "1 dk","dk5","no answer","hi","i","not","nothing","no commont"
             , "can t answer","no can not","dosen t know","he is not sure"
             , "its confidential","no answwer","not reaslly","lkjlkj","skjzhdkjhsd"
             , "you can", "even", "can","dont know dont talk about politics"
             , "dont knoiw","nono","not sure","do not know it","quit"
             , "doesnt know","she doesnt know","no not thinking","cant say"
             , "i don t know much", "would rather not explain","past"
             , "skipped question", "skip the question", "hjkdhfkjhdskjh"
             , "theuyidhfjdhkjdhfiaesjrhdjhflike shit", "dfdsjfksdjfkdsjf","dfsadfsf"
             , "god knows no i can t","no comments","dont want to comment"
             , "doesn t know","wants to skip","no not sure","no i caint", "not really no"
             , "i really cant say let me think","nope i don t know what liberal is"
             , "dont know what a conservative is dont care","she cannot"
             , "doesn t klnow", "no i cain t", "decline", "really can t"
             , "i choose not to","no i don t want to","no skip")] <- ""
  return(x)
})


## num-lock issue
# maybe look into this later

## fix words without whitespace
# maybe look into this later

## spell-checking
write.table(anes2012spell, file = "calc/out/anes2012TS_combined.csv"
            , sep = ",", col.names = F, row.names = F)
spell <- aspell("calc/out/anes2012TS_combined.csv") %>%
  filter(Suggestions!="NULL")

## replace incorrect words
for(i in 1:nrow(spell)){
  anes2012spell[spell$Line[i],] <- gsub(spell$Original[i], unlist(spell$Suggestions[i])[1]
                                        , anes2012spell[spell$Line[i],])
}
anes2012spell <- data.frame(caseid = anes2012opend$caseid, anes2012spell,stringsAsFactors = F)


### add meta information about responses

## function to compute shannon entropy (rescaled to 0-1??)
shannon <- function(x, reversed = F){
  out <- (- sum(log(x^x)/log(length(x))))
  if(reversed) out <- 1 - out
  out
}

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
  shannon(iwc/sum(iwc))
})


### fit structural topic model

### prepare data

## combine regular survey and open-ended data, remove spanish and empty responses
meta <- c("age", "educ_cont", "pid_cont", "educ_pid", "female")
data <- anes2012 %>% mutate(resp = apply(anes2012spell[,-1],1,paste,collapse=' ')) %>%
  filter(spanish == 0 & wc != 0)

## remove additional whitespaces
data$resp <- gsub("\\s+"," ", data$resp)
data$resp <- gsub("(^\\s+|\\s+$)","", data$resp)

## remove missings on metadata
data <- data[apply(!is.na(data[,meta]),1,prod)==1,]

## process for stm
processed <- textProcessor(data$resp, metadata = data[,meta]
                           , customstopwords = c("dont", "hes", "that", "etc"))
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

## remove discarded observations from data
#data <- data[-processed$docs.removed,]
data <- data[-out$docs.removed,]

## stm fit with 20 topics
stm_fit <- stm(out$documents, out$vocab, prevalence = as.matrix(out$meta)
                , K=20, init.type = "Spectral")

## stm fit with 50 topics (estimating number of topics gives ~70, but creates computational issues)
stm_fit_full <- stm(out$documents, out$vocab, prevalence = as.matrix(out$meta)
                    , K=40, init.type = "Spectral")


#######################
### Discursive sophistication measure
#######################

## combine sophistication components with remaining data
data <- cbind(data, sophistication(stm_fit))

## compute combined measures
data$polknow_text <- data$ntopics * data$distinct * data$ditem
data$polknow_text_mean <- (data$ntopics + data$distinct + data$ditem)/3


###################
### Replicate measure with larger number of topics
###################

## combine sophistication components with remaining data
know <- sophistication(stm_fit_full)

## compute combined measures
data$polknow_text_mean_full <- (know$ntopics + know$distinct + data$ditem)/3


### Compare 20 topic sophistication to 77 topic sophistication

ggplot(data, aes(x=polknow_text_mean_full, y=polknow_text_mean)) +
  geom_point(alpha=.05) + geom_smooth(method="lm") +
  ylab("Discursive Sophistication\n(20 Topics)") + xlab("Discursive Sophistication\n(40 Topics)") +
  annotate("text", x=0.1, y=.9, size=2
           , label = paste0("r = ",round(cor(data$polknow_text_mean, data$polknow_text_mean_full), 2))) +
  theme_classic(base_size=8) + theme(panel.border = element_rect(fill=NA))
ggsave("fig/ktopic.pdf", width=3, height=3)


#################
### Estimate hetreg models (in prep because it takes a long time)
#################


policies <- c("ideol","spsrvpr","defsppr","inspre","guarpr")
targets <- c("rpc","dpc","rep","dem")
measures <- c("polknow_text_mean","polknow_factual")

polknow_hetreg <- function(policy, target, measure
                           , controls = c("female", "educ", "faminc", "lage"
                                          , "black", "relig", "mode")
                           , df = data, control = NULL){
  tmp <- na.omit(df[,c(paste(c(policy,target),collapse="_")
                       , paste(c(policy,"ego"),collapse="_")
                       , measure, controls)])
  y <- tmp[,paste(c(policy,target),collapse="_")]
  X <- tmp[,c(paste(c(policy,"ego"),collapse="_"), controls)]
  Z <- as.matrix(tmp[,measure])
  
  dl <- list(N = nrow(X), B = ncol(X), G = ncol(Z)
             , y = y, X = X, Z = Z
             , S = 10, Zpred = as.matrix(seq(min(Z[,1]), max(Z[,1]), length.out = 10)))
  res <- stan(file = "calc/hetreg.stan", data=dl, control=control)
  return(res)
}

hetreg_summary <- data.frame(NULL)
for(p in policies){
  for(t in targets){
    for(m in measures){
      iterlab <- paste(c(p,t,m),collapse="_")
      cat("Iteration: ",iterlab,"\n")
      tmp <- polknow_hetreg(p, t, m)
      tmp_sigmadif <- extract(tmp, par="sigmadif")[[1]]
      tmp_df <- data.frame(policy = p, target = t, measure = m, mean = mean(tmp_sigmadif)
                           , cilo = quantile(tmp_sigmadif, .025), cihi = quantile(tmp_sigmadif, .975))
      hetreg_summary <- rbind(hetreg_summary, tmp_df)
    }
  }
}

## estimation issue with liberal/conservative assessment!
tmp <- polknow_hetreg("ideol", "dpc", "polknow_factual", control = list(adapt_delta=.99))
tmp_sigmadif <- extract(tmp, par="sigmadif")[[1]]
tmp_df <- data.frame(policy = "ideol", target = "dpc", measure = "dpc"
                     , mean = mean(tmp_sigmadif)
                     , cilo = quantile(tmp_sigmadif, .025)
                     , cihi = quantile(tmp_sigmadif, .975))



### save output

save(anes2012, anes2012opend, anes2012spell, data, meta, processed, out
     , stm_fit, stm_fit_full, hetreg_summary
     , file="calc/out/anes.Rdata")
