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

library(tidyverse)
library(car)
library(dplyr)
library(quanteda)
library(quanteda.dictionaries)
library(stm)
library(haven)
library(ineq)
datasrc <- "/home/patrick/Dropbox/Uni/Data/colombo/"
raw <- read_dta(paste0(datasrc,"citizencompetence_colombo.dta"), encoding = "latin 1")
liwc_de <- dictionary(file = "~/Dropbox/Uni/Data/LIWC/German_LIWC2001_Dictionary.dic", format = "LIWC")
liwc_fr <- dictionary(file = "~/Dropbox/Uni/Data/LIWC/French_LIWC2007_Dictionary.dic", format = "LIWC", encoding = "LATIN1")
liwc_it <- dictionary(file = "~/Dropbox/Uni/Data/LIWC/Italian_LIWC2007_Dictionary.dic", format = "LIWC", encoding = "LATIN1")

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

## minor pre-processing
preprocess <- function(x){
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
}

## combine pro and con
swiss <- swiss %>%
  mutate(
    string1 = preprocess(paste(prostring1, constring1)),
    string2 = preprocess(paste(prostring2, constring2))
  )



# GERMAN: Text-based political sophistication measure ----------------------------

opend_german <- swiss %>% filter(lang == 1)


## Consistency: Shannon entropy of response lengths ----------------------

## overall response length
opend_german$wc <- apply(dplyr::select(opend_german, string1:string2), 1, function(x){
  sum(str_count(x, "\\w+"))
})
opend_german$lwc <- log(opend_german$wc)/max(log(opend_german$wc), na.rm = T)

### consistency in item response  (respondents either answered pro OR con, I should take that into account!!!)
opend_german$consistency <- apply(dplyr::select(opend_german, string1:string2), 1, function(x){
  iwc <- str_count(x, "\\w+")
  shannon(iwc/sum(iwc))
})


## Considerations: Number of topics mentioned -----------------------------

## combine regular survey and open-ended data
## ADD: type of referendum as a meta-covariate
meta <- c("age", "edu", "ideol", "edu_ideol", "female")
opend_german$resp <- apply(dplyr::select(opend_german, string1:string2),1,paste,collapse=' ')

## remove additional whitespaces
opend_german$resp <- gsub("\\s+"," ", opend_german$resp)
opend_german$resp <- gsub("(^\\s+|\\s+$)","", opend_german$resp)

## remove missings on metadata
opend_german <- opend_german[apply(!is.na(opend_german[,meta]),1,prod)==1, ]

## remove empty strings
opend_german <- opend_german[opend_german$wc != 0, ]

### process for stm
processed <- textProcessor(opend_german$resp, metadata = opend_german[,meta],
                           language = "german")
out_german <- prepDocuments(processed$documents, processed$vocab, processed$meta,
                            lower.thresh = 10)

### remove discarded observations from data
opend_german <- opend_german[-processed$docs.removed,]
opend_german <- opend_german[-out_german$docs.removed,]

### quick fit (30 topics)
stm_fit_german <- stm(out_german$documents, out_german$vocab, prevalence = as.matrix(out_german$meta),
                      K=25, seed=12345)

### compute number of considerations
opend_german$considerations <- ntopics(stm_fit_german, out_german)


## Word choice: LIWC component ---------------------------------------------

opend_german_liwc <- liwcalike(opend_german$resp, liwc_de)

### combine exclusive words and conjunctions (see Tausczik and Pennebaker 2010: 35)
opend_german$wordchoice <- with(opend_german_liwc,
                                (incl + excl) * WC
                                #discrep + tentat + incl + cause + insight + inhib
                                #- certain - negate - excl
                                )
# NOTE: based on 2007 dict
opend_german$wordchoice <- opend_german$wordchoice - min(opend_german$wordchoice)
opend_german$wordchoice <- opend_german$wordchoice / max(opend_german$wordchoice)


## Merge with full data and save -------------------------------------------

### compute combined measures
opend_german$polknow_text <- with(opend_german, considerations * consistency * wordchoice)
opend_german$polknow_text_mean <- with(opend_german, considerations + consistency + wordchoice)/3



# FRENCH: Text-based political sophistication measure --------------------

opend_french <- swiss %>% filter(lang == 2)


## Consistency: Shannon entropy of response lengths ----------------------

## overall response length
opend_french$wc <- apply(dplyr::select(opend_french, string1:string2), 1, function(x){
  sum(str_count(x, "\\w+"))
})
opend_french$lwc <- log(opend_french$wc)/max(log(opend_french$wc), na.rm = T)

### consistency in item response  (respondents either answered pro OR con, I should take that into account!!!)
opend_french$consistency <- apply(dplyr::select(opend_french, string1:string2), 1, function(x){
  iwc <- str_count(x, "\\w+")
  shannon(iwc/sum(iwc))
})


## Considerations: Number of topics mentioned -----------------------------

## combine regular survey and open-ended data
## ADD: type of referendum as a meta-covariate
meta <- c("age", "edu", "ideol", "edu_ideol", "female")
opend_french$resp <- apply(dplyr::select(opend_french, string1:string2),1,paste,collapse=' ')

## remove additional whitespaces
opend_french$resp <- gsub("\\s+"," ", opend_french$resp)
opend_french$resp <- gsub("(^\\s+|\\s+$)","", opend_french$resp)

## remove missings on metadata
opend_french <- opend_french[apply(!is.na(opend_french[,meta]),1,prod)==1, ]

## remove empty strings
opend_french <- opend_french[opend_french$wc != 0, ]

### process for stm
processed <- textProcessor(opend_french$resp, metadata = opend_french[,meta],
                           language = "french")
out_french <- prepDocuments(processed$documents, processed$vocab, processed$meta,
                            lower.thresh = 10)

### remove discarded observations from data
opend_french <- opend_french[-processed$docs.removed,]
opend_french <- opend_french[-out_french$docs.removed,]

## quick fit (60 topics)
stm_fit_french <- stm(out_french$documents, out_french$vocab, prevalence = as.matrix(out_french$meta),
                      K=25, seed=12345)

### compute number of considerations
opend_french$considerations <- ntopics(stm_fit_french, out_french)


## Word choice: LIWC component ---------------------------------------------

opend_french_liwc <- liwcalike(opend_french$resp, liwc_fr)

### combine exclusive words and conjunctions (see Tausczik and Pennebaker 2010: 35)
opend_french$wordchoice <- with(opend_french_liwc,
                                conjonction + exclusion
                                #divergence + tentative + inclusion + cause + perspicacité + inhibition
                                #- certitude - négation - exclusion
                                )
# NOTE: based on 2007 dict
opend_french$wordchoice <- opend_french$wordchoice - min(opend_french$wordchoice)
opend_french$wordchoice <- opend_french$wordchoice / max(opend_french$wordchoice)


## Merge with full data and save -------------------------------------------

### compute combined measures
opend_french$polknow_text <- with(opend_french, considerations * consistency * wordchoice)
opend_french$polknow_text_mean <- with(opend_french, considerations + consistency + wordchoice)/3



# ITALIAN: Text-based political sophistication measure  --------------------

opend_italian <- swiss %>% filter(lang == 3)


## Consistency: Shannon entropy of response lengths ----------------------

## overall response length
opend_italian$wc <- apply(dplyr::select(opend_italian, string1:string2), 1, function(x){
  sum(str_count(x, "\\w+"))
})
opend_italian$lwc <- log(opend_italian$wc)/max(log(opend_italian$wc), na.rm = T)

### consistency in item response  (respondents either answered pro OR con, I should take that into account!!!)
opend_italian$consistency <- apply(dplyr::select(opend_italian, string1:string2), 1, function(x){
  iwc <- str_count(x, "\\w+")
  shannon(iwc/sum(iwc))
})


## Considerations: Number of topics mentioned -----------------------------

## combine regular survey and open-ended data
## ADD: type of referendum as a meta-covariate
meta <- c("age", "edu", "ideol", "edu_ideol", "female")
opend_italian$resp <- apply(dplyr::select(opend_italian, string1:string2),1,paste,collapse=' ')

## remove additional whitespaces
opend_italian$resp <- gsub("\\s+"," ", opend_italian$resp)
opend_italian$resp <- gsub("(^\\s+|\\s+$)","", opend_italian$resp)

## remove missings on metadata
opend_italian <- opend_italian[apply(!is.na(opend_italian[,meta]),1,prod)==1, ]

## remove empty strings
opend_italian <- opend_italian[opend_italian$wc != 0, ]

### process for stm
processed <- textProcessor(opend_italian$resp, metadata = opend_italian[,meta],
                           language = "italian")
out_italian <- prepDocuments(processed$documents, processed$vocab, processed$meta,
                            lower.thresh = 10)

### remove discarded observations from data
opend_italian <- opend_italian[-processed$docs.removed,]
opend_italian <- opend_italian[-out_italian$docs.removed,]

## quick fit (60 topics)
stm_fit_italian <- stm(out_italian$documents, out_italian$vocab,
                       prevalence = as.matrix(out_italian$meta),
                       K=25, seed=12345)

### compute number of considerations
opend_italian$considerations <- ntopics(stm_fit_italian, out_italian)


## Word choice: LIWC component ---------------------------------------------

opend_italian_liwc <- liwcalike(opend_italian$resp, liwc_it)

### combine exclusive words and conjunctions (see Tausczik and Pennebaker 2010: 35)
opend_italian$wordchoice <- with(opend_italian_liwc,
                                 inclusi + esclusi
                                 #discrep + possib + inclusi + causa + intros + inibiz
                                 #- certez - negazio - esclusi
                                 )
# NOTE: based on 2007 dict
opend_italian$wordchoice <- opend_italian$wordchoice - min(opend_italian$wordchoice)
opend_italian$wordchoice <- opend_italian$wordchoice / max(opend_italian$wordchoice)


## Merge with full data and save -------------------------------------------

### compute combined measures
opend_italian$polknow_text <- with(opend_italian, considerations * consistency * wordchoice)
opend_italian$polknow_text_mean <- with(opend_italian, considerations + consistency + wordchoice)/3




# save output -------------------------------------------------------------

save(swiss, opend_german, opend_french, opend_italian, meta, processed,
     out_german, out_french, out_italian,
     stm_fit_german, stm_fit_french, stm_fit_italian,
     file="calc/out/swiss.Rdata")
