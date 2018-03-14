### ============================================================= ###
### Measuring Political Sophistication using Open-ended Responses ###
### Patrick Kraft                                                 ###
### ============================================================= ###
## prepares the survey data of the Swiss referendum data (Colombo 2016) for subsequent analyses
## prepares open-ended responses (selecting variables, spell checking etc.)
## fits structural topic model for open-ended responses
## creates diversity measures


### load raw data

rm(list = ls())
gc()

library(car)
library(dplyr)
library(quanteda)
library(stm)
library(haven)
library(ineq)
setwd("/data/Dropbox/Uni/Projects/2016/knowledge/")
datasrc <- "/home/patrick/Dropbox/Uni/Data/colombo/"
raw <- read_dta(paste0(datasrc,"citizencompetence_colombo.dta"), encoding = "latin 1")

source("calc/func.R")


### prep/recode survey data

## respondent id
swiss <- dplyr::select(raw, nummer, prostring1, prostring2, constring1, constring2)

## language (-> separate dataset later)
swiss$lang <- raw$sprache

## level of justification, manual coding
swiss$loj <- raw$lojr

## age
swiss$age <- raw$age

## education
swiss$edu <- raw$edu

## left-right self-assessment
swiss$ideol <- na_if(raw$P04, 11) %>% na_if(12)

## ideol * edu
swiss$edu_ideol <- swiss$edu * swiss$ideol

## gender
swiss$female <- 1 - raw$male


### open-ended responses
### MORE WORK ON PRE-PROCESSING NEEDED, check all steps, spell checking, stopword removal etc.

## empty strings
swiss$prostring1[swiss$prostring1 %in% c(99999997,99999998)] <- ""
swiss$prostring2[swiss$prostring2 %in% c(99999997,99999998)] <- ""
swiss$constring1[swiss$constring1 %in% c(99999997,99999998)] <- ""
swiss$constring2[swiss$constring2 %in% c(99999997,99999998)] <- ""

## combine pro and con
swiss$string1 <- paste(swiss$prostring1, swiss$constring1)
swiss$string2 <- paste(swiss$prostring2, swiss$constring2)
  
## minor pre-processing
opend <- apply(swiss[,c("string1","string2")], 2, function(x){
  x <- char_tolower(x)
  x <- gsub("(^\\s+|\\s+$)","", x)
  x[x %in% c("no","non","nein","rien","nichts","keinen","nsp","ka","n","prive","nono"
             ,"si","oooooooooooo","ooooooooooo","ooooooooooo","ooooooooooooooo"
             ,"llll","lll","xxx","xx","niente","weiss nicht","weis nicht","weiss nichts"
             ,"keine ahnung","c","keine gründe","même raison","pas d'opinion","non mi ricordo"
             ,"nessun motivo","non so","weis nicht mehr","weiss nicht mehr","kein kommentar"
             ,"keine 2 grund","rien dautre","weis es nicht mehr","weiss es nicht mehr"
             ,"keins","k angaben","kein weitere grund","keine anderen gründen","wn","keine"
             ,"keiner","nichts mehr")] <- ""
  x <- gsub("//"," ", x , fixed = T)
  x <- gsub("\\s+"," ", x)
  x <- gsub("(^\\s+|\\s+$)","", x)
  return(x)
  })


### add meta information about responses

## function to compute shannon entropy (rescaled to 0-1??)
shannon <- function(x, reversed = F){
  out <- (- sum(log(x^x)/log(length(x))))
  if(reversed) out <- 1 - out
  out
}

## overall response length (this should actually be done by language!!!)
swiss$wc <- apply(opend, 1, function(x){
  length(unlist(strsplit(x,"\\s+")))
})
swiss$lwc <- log(swiss$wc)/max(log(swiss$wc))

## number of items answered
swiss$nitem <- apply(opend != "", 1, sum, na.rm = T)

## diversity in item response (respondents either answered pro OR con, I should take that into account!!!)
swiss$ditem <- apply(opend, 1, function(x){
  iwc <- unlist(lapply(strsplit(x,"\\s+"), length))
  shannon(iwc/sum(iwc))
})


### fit structural topic model

### prepare data

## combine regular survey and open-ended data
## ADD: type of referendum as a meta-covariate
meta <- c("age", "edu", "ideol", "edu_ideol", "female")
swiss$resp <- apply(dplyr::select(opend, string1:string2),1,paste,collapse=' ')

## remove additional whitespaces
swiss$resp <- gsub("\\s+"," ", swiss$resp)
swiss$resp <- gsub("(^\\s+|\\s+$)","", swiss$resp)

## remove missings on metadata
data <- swiss[apply(!is.na(swiss[,meta]),1,prod)==1,]

## remove empty strings
data <- data[data$wc != 0, ]



###################################################
### German respondents

## select language
opend_german <- data %>% filter(lang == 1)

## process for stm
processed <- textProcessor(opend_german$resp, metadata = opend_german[,meta]
                           , language = "german")
out_german <- prepDocuments(processed$documents, processed$vocab, processed$meta
                            , lower.thresh = 10)

## remove discarded observations from data
opend_german <- opend_german[-processed$docs.removed,]
opend_german <- opend_german[-out_german$docs.removed,]

## quick fit (30 topics)
stm_fit_german <- stm(out_german$documents, out_german$vocab, prevalence = as.matrix(out_german$meta)
                      , K=0, init.type = "Spectral", seed=12345)


### Discursive sophistication measure

## combine sophistication components with remaining data
opend_german <- cbind(opend_german, sophistication(stm_fit_german, out_german))

## compute combined measures
opend_german$polknow_text <- with(opend_german, ntopics * distinct * ditem)
opend_german$polknow_text_mean <- with(opend_german, ntopics + distinct + ditem)/3



#####################################################
### French respondents

## select language
opend_french <- data %>% filter(lang == 2)

## process for stm
processed <- textProcessor(opend_french$resp, metadata = opend_french[,meta]
                           , language = "french")
out_french <- prepDocuments(processed$documents, processed$vocab, processed$meta
                     , lower.thresh = 10)

## remove discarded observations from data
opend_french <- opend_french[-processed$docs.removed,]
opend_french <- opend_french[-out_french$docs.removed,]

## quick fit (60 topics)
stm_fit_french <- stm(out_french$documents, out_french$vocab, prevalence = as.matrix(out_french$meta)
                      , K=0, init.type = "Spectral")


### Discursive sophistication measure

## combine sophistication components with remaining data
opend_french <- cbind(opend_french, sophistication(stm_fit_french, out_french))

## compute combined measures
opend_french$polknow_text <- with(opend_french, ntopics * distinct * ditem)
opend_french$polknow_text_mean <- with(opend_french, ntopics + distinct + ditem)/3



#####################################################
### Italian respondents

## select language
opend_italian <- data %>% filter(lang == 3)

## process for stm
processed <- textProcessor(opend_italian$resp, metadata = opend_italian[,meta]
                           , language = "italian")
out_italian <- prepDocuments(processed$documents, processed$vocab, processed$meta
                     , lower.thresh = 10)

## remove discarded observations from data
opend_italian <- opend_italian[-processed$docs.removed,]
opend_italian <- opend_italian[-out_italian$docs.removed,]

## quick fit (60 topics)
stm_fit_italian <- stm(out_italian$documents, out_italian$vocab, prevalence = as.matrix(out_italian$meta)
                       , K=0, init.type = "Spectral")


### Discursive sophistication measure

## combine sophistication components with remaining data
opend_italian <- cbind(opend_italian, sophistication(stm_fit_italian, out_italian))

## compute combined measures
opend_italian$polknow_text <- with(opend_italian, ntopics * distinct * ditem)
opend_italian$polknow_text_mean <- with(opend_italian, ntopics + distinct + ditem)/3


### save output

save(swiss, opend_german, opend_french, opend_italian, meta, processed
     , out_german, out_french, out_italian
     , stm_fit_german, stm_fit_french, stm_fit_italian
     , file="calc/out/swiss.Rdata")
