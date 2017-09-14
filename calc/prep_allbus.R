### ============================================================= ###
### Measuring Political Sophistication using Open-ended Responses ###
### Patrick Kraft                                                 ###
### ============================================================= ###
## prepares the survey data of the 2008 ALLBUS for subsequent analyses
## prepares open-ended responses (selecting variables, spell checking etc.)
## fits structural topic model for open-ended responses
## creates diversity measures


### load raw data

rm(list = ls())
library(car)
library(dplyr)
library(quanteda)
library(haven)
library(stm)
library(readstata13)
library(ineq)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

setwd("/data/Dropbox/Uni/Projects/2016/knowledge/")
datasrc <- "/data/Dropbox/Uni/Data/ALLBUS2008/"

raw <- read_dta(paste0(datasrc,"ZA4600_v2-1-0.dta"))


## respondent id
allbus2008 <- data.frame(caseid=raw$V2)

## political knowledge (factual knowledge questions)
allbus2008$polknow_factual <- with(raw, ((V732==5) + (V733==3) + (V734==1) + (V735==2)
                                         + (V736==1) + (V737==1) + (V738==4) + (V739==5)
                                         + (V740==3) + (V741==3) + (V742==2) + (V743==4)
                                         + (V744==2) + (V745==3) + (V746==2) + (V747==2)
                                         + (V748==2))/17)

## education (bachelor degree)
allbus2008$educ <- as.numeric(raw$V173==5)
allbus2008$educ[raw$V173==99] <- NA

## education (continuous)
allbus2008$educ_cont <- (Recode(raw$V173, "6:99=NA") - 1)/4

## political media exposure
allbus2008$polmedia <- with(raw, Recode(V15, "9=NA") + Recode(V17, "9=NA")
                            + Recode(V18, "9=NA"))/21

## political discussion
allbus2008$poldisc <- ((5 - Recode(raw$V492, "6=5; 9=NA"))
                       + (5 - Recode(raw$V493, "6=5; 9=NA")) 
                       + (5 - Recode(raw$V494, "6=5; 9=NA"))
                       + (5 - Recode(raw$V495, "9=NA")))/16

## overall political interest
allbus2008$polint <- (5 - Recode(raw$V100, "9=NA"))/4

## internal efficacy
allbus2008$effic_int <- with(raw, 4 - Recode(V86, "8:9=NA")
                             + Recode(V87, "8:9=NA") - 1
                             + Recode(V89, "8:9=NA") - 1) / 9

## external efficacy
allbus2008$effic_ext <- with(raw, Recode(V85, "8:9=NA") - 1
                             - Recode(V88, "8:9=NA") + 4) / 6

## overall efficacy
allbus2008$effic <- with(allbus2008, (effic_int + effic_ext)/2)

## voted in previous election
allbus2008$pastvote <- Recode(raw$V535, "2=0; 9=NA")

## non-conventional participation index
allbus2008$part <- (Recode(raw$V57, "6=0; 9=NA") + Recode(raw$V59, "6=0; 9=NA")
                    + Recode(raw$V60, "6=0; 9=NA") + Recode(raw$V62, "6=0; 9=NA")
                    + Recode(raw$V63, "6=0; 9=NA") + Recode(raw$V66, "6=0; 9=NA")
                    + Recode(raw$V67, "6=0; 9=NA") + Recode(raw$V68, "6=0; 9=NA"))

## party/candidate placements
allbus2008$ideol_ego <- Recode(raw$libcpre_dpc, "lo:0=NA")
allbus2008$ideol_rpc <- Recode(raw$libcpre_rpc, "lo:0=NA")
allbus2008$ideol_dpc <- Recode(raw$libcpre_dpc, "lo:0=NA")
allbus2008$ideol_rep <- Recode(raw$libcpre_ptyr, "lo:0=NA")
allbus2008$ideol_dem <- Recode(raw$libcpre_ptyd, "lo:0=NA")
allbus2008$spsrvpr_ego <- Recode(raw$spsrvpr_ssself, "lo:0=NA")
allbus2008$spsrvpr_rpc <- Recode(raw$spsrvpr_ssrpc, "lo:0=NA")
allbus2008$spsrvpr_dpc <- Recode(raw$spsrvpr_ssdpc, "lo:0=NA")
allbus2008$spsrvpr_rep <- Recode(raw$spsrvpr_ssrep, "lo:0=NA")
allbus2008$spsrvpr_dem <- Recode(raw$spsrvpr_ssdem, "lo:0=NA")
allbus2008$defsppr_ego <- Recode(raw$defsppr_self, "lo:0=NA")
allbus2008$defsppr_rpc <- Recode(raw$defsppr_rpc, "lo:0=NA")
allbus2008$defsppr_dpc <- Recode(raw$defsppr_dpc, "lo:0=NA")
allbus2008$defsppr_rep <- Recode(raw$defsppr_rep, "lo:0=NA")
allbus2008$defsppr_dem <- Recode(raw$defsppr_dem, "lo:0=NA")
allbus2008$inspre_ego <- Recode(raw$inspre_self, "lo:0=NA")
allbus2008$inspre_rpc <- Recode(raw$inspre_rpc, "lo:0=NA")
allbus2008$inspre_dpc <- Recode(raw$inspre_dpc, "lo:0=NA")
allbus2008$inspre_rep <- Recode(raw$inspre_rep, "lo:0=NA")
allbus2008$inspre_dem <- Recode(raw$inspre_dem, "lo:0=NA")
allbus2008$guarpr_ego <- Recode(raw$guarpr_self, "lo:0=NA")
allbus2008$guarpr_rpc <- Recode(raw$guarpr_rpc, "lo:0=NA")
allbus2008$guarpr_dpc <- Recode(raw$guarpr_dpc, "lo:0=NA")
allbus2008$guarpr_rep <- Recode(raw$guarpr_rep, "lo:0=NA")
allbus2008$guarpr_dem <- Recode(raw$guarpr_dem, "lo:0=NA")

## ideology (factor/dummies)
raw$V106
allbus2008$ideol <- factor(Recode(raw$libcpre_self, "1:3=1; 4=2; 5:7=3; else=NA")
                  , labels = c("Liberal","Moderate","Conservative"))
allbus2008$ideol_lib <- as.numeric(allbus2008$ideol=="Liberal")
allbus2008$ideol_con <- as.numeric(allbus2008$ideol=="Conservative")

## ideology (continuous, -1 to 1)
allbus2008$ideol_ct <- (Recode(raw$libcpre_self, "lo:0=NA") - 4)/3

## strength of ideology
allbus2008$ideol_str <- abs(allbus2008$ideol_ct)

## party identification (factor/dummies)
allbus2008$pid <- factor(Recode(raw$pid_x
                              , "1:2=1; c(3,4,5)=2; 6:7=3; else=NA")
                       , labels = c("Democrat","Independent","Republican"))
allbus2008$pid_dem <- as.numeric(allbus2008$pid=="Democrat")
allbus2008$pid_rep <- as.numeric(allbus2008$pid=="Republican")

## pid continuous
allbus2008$pid_cont <- (Recode(raw$pid_x, "lo:0=NA") - 4)/3

## interaction: pid * education
allbus2008$educ_pid <- allbus2008$educ_cont * allbus2008$pid_cont

## strength of partisanship
allbus2008$pid_str <- abs(allbus2008$pid_cont)

## religiosity (church attendance)
allbus2008$relig <- (5 - Recode(raw$relig_churchoft, "lo:0 = NA"))/5
allbus2008$relig[raw$relig_church != 1] <- 0
allbus2008$relig[raw$relig_churchwk == 2] <- 1

## age
allbus2008$age <- Recode(raw$dem_age_r_x, "c(-2,-9,-8) = NA")

## log(age)
allbus2008$lage <- log(allbus2008$age)

## sex
allbus2008$female <- raw$gender_respondent_x - 1

## race
allbus2008$black <- as.numeric(Recode(raw$dem_raceeth_x, "lo:0 = NA") == 2)

## income
allbus2008$faminc <- (Recode(raw$incgroup_prepost_x, "lo:0 = NA") -1)/27

## gender of inerviewer
allbus2008$iwrmale <- Recode(raw$iwrdesc_pre_gender, "lo:0=NA; 2=0")

## spanish speaking respondent
allbus2008$spanish <- as.numeric(raw$profile_spanishsurv == 1 |
                              raw$admin_pre_lang_start == 2 |
                              raw$admin_post_lang_start == 2)

## wordsum literacy test
allbus2008$wordsum <- with(raw, (wordsum_setb == 5) + (wordsum_setd == 3)
                         + (wordsum_sete == 1) + (wordsum_setf == 3)
                         + (wordsum_setg == 5) + (wordsum_seth == 4)
                         + (wordsum_setj == 1) + (wordsum_setk == 1)
                         + (wordsum_setl == 4) + (wordsum_seto == 2))/10

## Pro-redistribution attitude: (new scale: 0-1)
## Services and spending tradeoff placement (1-7, max = increase spending)
## Standard of living (1-7, max = gov't should let each person get ahead on their own)
allbus2008$redist <- (Recode(raw$spsrvpr_ssself, "lo:0 = NA") - Recode(raw$guarpr_self, "lo:0 = NA") + 6)/12

## Support tax increases (new scale: 0-1)
## favor tax on millionaires
## raising personal inc tax for over 250K inc to reduce deficit
allbus2008$tax <- ((-Recode(raw$milln_milltax_x, "lo:0 = NA") + 7)/3 + Recode(raw$budget_rdef250k, "lo:0 = NA; 1=2; 2=0; 3=1"))/4


### 2012 open-ended responses
### MORE WORK ON PRE-PROCESSING NEEDED, check all steps, spell checking, stopword removal etc.

## read original open-ended responses (downloaded from anes website)
allbus2008pre <- read.csv(paste0(datasrc,"allbus2008TS_openends.csv"), as.is = T) %>%
  dplyr::select(caseid, candlik_likewhatdpc, candlik_dislwhatdpc, candlik_likewhatrpc, candlik_dislwhatrpc
         , ptylik_lwhatdp, ptylik_dwhatdp, ptylik_lwhatrp, ptylik_dwhatrp)
#allbus2008post <- read.csv(paste0(datasrc,"allbus2008TS_post.csv"), as.is = T) %>%
#  select(caseid, mip_prob1, mip_prob2, mip_prob3, mip_mostprob)
#allbus2008opend <- merge(allbus2008pre, allbus2008post)

## optional: only select likes/dislikes
allbus2008opend <- allbus2008pre

## minor pre-processing
allbus2008spell <- apply(allbus2008opend[,-1], 2, function(x){
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
write.table(allbus2008spell, file = "calc/out/allbus2008TS_combined.csv"
            , sep = ",", col.names = F, row.names = F)
spell <- aspell("calc/out/allbus2008TS_combined.csv") %>%
  filter(Suggestions!="NULL")

## replace incorrect words
for(i in 1:nrow(spell)){
  allbus2008spell[spell$Line[i],] <- gsub(spell$Original[i], unlist(spell$Suggestions[i])[1]
                                        , allbus2008spell[spell$Line[i],])
}
allbus2008spell <- data.frame(caseid = allbus2008opend$caseid, allbus2008spell,stringsAsFactors = F)


### add meta information about responses

## overall response length
allbus2008$wc <- apply(allbus2008spell[,-1], 1, function(x){
  length(unlist(strsplit(x,"\\s+")))
})
allbus2008$lwc <- log(allbus2008$wc)/max(log(allbus2008$wc))

## number of items answered
allbus2008$nitem <- apply(allbus2008spell[,-1] != "", 1, sum, na.rm = T)

## diversity in item response
allbus2008$ditem <- apply(allbus2008spell[,-1], 1, function(x){
  iwc <- unlist(lapply(strsplit(x,"\\s+"), length))
  1 - ineq(iwc,type="Gini")
})


## compare response behavior by gender
ggplot(allbus2008, aes(wc, fill=factor(female))) + geom_bar(stat=mean)

ggplot(allbus2008, aes(factor(female), y=as.numeric(wc!=0))) + 
  stat_summary_bin(fun.y = "mean", geom="bar")

ggplot(filter(allbus2008, wc>0), aes(factor(female), y=wc)) + geom_point(alpha=.1, size=.1) + 
  stat_summary(fun.data = "mean_cl_boot", col="red")

ggplot(filter(allbus2008, wc>0), aes(factor(female), y=wc)) + 
  stat_summary(fun.data = "mean_cl_boot", col="red")

table(allbus2008$spanish==0)
table(allbus2008$wc==0)

### fit structural topic model

### prepare data

## combine regular survey and open-ended data, remove spanish and empty responses
meta <- c("age", "educ_cont", "pid_cont", "educ_pid")
data <- allbus2008 %>% mutate(resp = apply(allbus2008spell[,-1],1,paste,collapse=' ')) %>%
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
stm_fit_20 <- stm(out$documents, out$vocab, prevalence = as.matrix(out$meta)
                , K=20, init.type = "Spectral")

## slow, smart fit: estimates about 80 topics, might be too large (also, K is not deterministic here)
stm_fit <- stm(out$documents, out$vocab, prevalence = as.matrix(out$meta)
               , K=0, init.type = "Spectral")


### create new sophistication measures

## probability fits and transform for diversity score  
doc_topic_prob <- stm_fit$theta

## topic diversity score
data$topic_diversity <- apply(doc_topic_prob, 1, function(x) 1 - ineq(x,type="Gini"))

## check sensitivity of diversity based on number of topics
data$topic_diversity_20 <- apply(stm_fit_20$theta, 1, function(x) 1 - ineq(x,type="Gini"))

## text-based sophistication measures
data$polknow_text <- with(data, topic_diversity * lwc * ditem)
data$polknow_text_mean <- with(data, (topic_diversity + lwc + ditem)/3)


### estimate hetreg models (in prep because it takes a long time)

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

save(allbus2008, allbus2008opend, allbus2008spell, data, meta, processed, out, stm_fit, hetreg_summary
     , file="calc/out/anes.Rdata")
