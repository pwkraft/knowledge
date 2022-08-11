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
swiss$loj_scale <- as.numeric(scale(swiss$loj))

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

## college degree
swiss$educ <- as.numeric(raw$edu == 4)

## political interest
swiss$polint <- raw$polint

## media usage
swiss$media <- raw$media


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
             ,"keiner","nichts mehr" # added below to address German responses (went through all loj = 0 codes)
             , "war mir nicht klar", "-", "---", "keine", "nein", "kam nicht draus um was es ging"
             , "nichts", "kam nicht draus", "97", "nicht^s", "wusste nicht um was es geht."
             , "keine antwort", "nein.", "wusste nicht genau um was es geht", "keinen"
             , "1, nicht so befasst damit2. kein zweiter grund", "gar kein intresse"
             , "will grund nicht angeben", "ooooooooooooooo", "die gleichen gründe"
             , "nicht gedacht dabei", "war ratlos also nein", "ok", "weeeee", "wieso nicht"
             , "sie weiss es nicht mehr genau", "- weiss nicht", "gleiche antwort", "wie vorhin"
             , "weil das muss nicht jeder wissen", "weiss nicht mehr", "nichts mehr", "."
             , "k.a.", "nix", "keinen wirklichen grund", "weiss es nicht mehr so genau."
             , "wn", "nichts.", "n", "oooooooooo", "keine genauen gründe", "rigrnbrdtimmung"
             , "grund war ihm nicht so klar.", "s", "habe es nicht verstanden", "weis nicht"
             , "lesefehler gemacht darum falsch gewaehlt", "wenig informiert", "ooooooooooo"
             , "weiss nichts", "nicht genau verstanden um was es geht", "kene", "gleich wie vorher"
             , "habe es nicht ganz begriffen", "weiss nichts mehr.", "-----", "----"
             , "keine gründe", "grundlos", "hat sie nicht interessiert", "keinen", "vergessen"
             , "kann es nicht mehr sagen", "nicht mehr", "weiss nicht genau", "keines"
             , "weiss nicht genau,", "ssfds", "interessiert mich nicht", "weis nich habe"
             , "weis nich habe", "nichts mehr", "llll", "lll", "keine 2. grund", "..."
             , "war zuwenig informiert", "das gleiche", "kein weiterer grund", "..........."
             , "kein grund mehr", "kann nicht begründet", "kein 2. grund", "keine 2.grund"
             , "kann ich nicht sagen", "nichts.", "xx", "weis es nicht mehr", "weiss es nicht mehr"
             , "weiss nicht genau", "kein anderes grund", "bin nicht drausgekommen", "="
             , "wusste nicht genau", "hat sich nicht damit befasst", "nein.", "das war alles"
             , "eigentliche nicht genau überlegt", "alles", "zu enig informiert", "hg"
             , "dito", "keine angabe", "hat sich icht damit b efasst, deshalb das nein"
             , "habe das zu wenig verstanden darum abgelehnt", "man hat sie nicht ganz verstanden"
             , "zuviel auf einmal", "macht sinn", "---------", "ich weissnicht"
             , "zu wenig befasst weiss nicht", "nichts mehr anderse", "weil bin nicht nachnahmen"
             , "habe mich nicht wirklich damit auseinander gesetzt", "schlecht erklärt"
             , "ist schon lange her", "weiss ich schlicht nicht mehr", "weiss nicht wieso"
             , "weiss es nicht mehr", "niochts", "der glich berüdig  lang so wie es ist"
             , "frage falsch verstanden", "kennt die gründe nicht.", "ist ihr zu fremt"
             , "das braucht es nicht", "nicht formulierbar", "bin nicht ganz genau nachgekommen"
             , "oooooooooooo", "hat mich nicht interessiert", "(verwechselt es mit osterweiterung)"
             , "weiss nicht mehr genau", "nichst mehr", "nein w.n.", "keines", "...."
             , ".....kennt sich zuwenig aus...", "neinn", "pers.", "-------------", "keiner"
             , "ein anderer grund", "nichts mehr", "hatte keinen durchblick.", "nicht genau"
             , "gleich", "überflog es nur kurz", "keine angaben", "kannte mich zuwenig aus"
             , "weiss grund nicht mehr", "nicht mehr", "kein", "ws", "ooooooooooo", "??"
             , "das interessiert uns garnicht", "der kugelschreiber hat nein geschrieben."
             , "persönlich", "auch das gleiche", "kann ich nicht sagen bin sehr emotional"
             )] <- ""
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

## overall response length
swiss$wc <- apply(dplyr::select(swiss, string1:string2), 1, function(x){
  sum(str_count(x, "\\w+"))
})



# GERMAN: Text-based political sophistication measure ----------------------------

opend_german <- swiss %>% filter(lang == 1)


## Range: Shannon entropy of response lengths ----------------------

## logged response length
opend_german$lwc <- log(opend_german$wc)/max(log(opend_german$wc), na.rm = T)

### consistency in item response  (respondents either answered pro OR con, I should take that into account!!!)
opend_german$range <- apply(dplyr::select(opend_german, string1:string2), 1, function(x){
  iwc <- str_count(x, "\\w+")
  shannon(iwc/sum(iwc))
})


## Size: Number of topics mentioned -----------------------------

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
opend_german$size <- ntopics(stm_fit_german, out_german)


## Constraint: LIWC component ---------------------------------------------

opend_german_liwc <- liwcalike(opend_german$resp, liwc_de)

### combine exclusive words and conjunctions (see Tausczik and Pennebaker 2010: 35)
opend_german$constraint <- with(opend_german_liwc,
                                (incl + excl) * WC
                                #discrep + tentat + incl + cause + insight + inhib
                                #- certain - negate - excl
                                )
# NOTE: based on 2007 dict
opend_german$constraint <- opend_german$constraint - min(opend_german$constraint)
opend_german$constraint <- opend_german$constraint / max(opend_german$constraint)


## Merge with full data and save -------------------------------------------

### compute combined measures
opend_german$polknow_text <- with(opend_german, size * range * constraint)
opend_german$polknow_text_mean <- with(opend_german, size + range + constraint)/3
opend_german$polknow_text_scale <- as.numeric(scale(opend_german$polknow_text_mean))



# FRENCH: Text-based political sophistication measure --------------------

opend_french <- swiss %>% filter(lang == 2)


## Range: Shannon entropy of response lengths ----------------------

## logged response length
opend_french$lwc <- log(opend_french$wc)/max(log(opend_french$wc), na.rm = T)

### consistency in item response  (respondents either answered pro OR con, I should take that into account!!!)
opend_french$range <- apply(dplyr::select(opend_french, string1:string2), 1, function(x){
  iwc <- str_count(x, "\\w+")
  shannon(iwc/sum(iwc))
})


## Size: Number of topics mentioned -----------------------------

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
opend_french$size <- ntopics(stm_fit_french, out_french)


## Constraint: LIWC component ---------------------------------------------

opend_french_liwc <- liwcalike(opend_french$resp, liwc_fr)

### combine exclusive words and conjunctions (see Tausczik and Pennebaker 2010: 35)
opend_french$constraint <- with(opend_french_liwc,
                                conjonction + exclusion
                                #divergence + tentative + inclusion + cause + perspicacité + inhibition
                                #- certitude - négation - exclusion
                                )
# NOTE: based on 2007 dict
opend_french$constraint <- opend_french$constraint - min(opend_french$constraint)
opend_french$constraint <- opend_french$constraint / max(opend_french$constraint)


## Merge with full data and save -------------------------------------------

### compute combined measures
opend_french$polknow_text <- with(opend_french, size * range * constraint)
opend_french$polknow_text_mean <- with(opend_french, size + range + constraint)/3
opend_french$polknow_text_scale <- as.numeric(scale(opend_french$polknow_text_mean))



# ITALIAN: Text-based political sophistication measure  --------------------

opend_italian <- swiss %>% filter(lang == 3)


## Range: Shannon entropy of response lengths ----------------------

## logged response length
opend_italian$lwc <- log(opend_italian$wc)/max(log(opend_italian$wc), na.rm = T)

### consistency in item response  (respondents either answered pro OR con, I should take that into account!!!)
opend_italian$range <- apply(dplyr::select(opend_italian, string1:string2), 1, function(x){
  iwc <- str_count(x, "\\w+")
  shannon(iwc/sum(iwc))
})


## Size: Number of topics mentioned -----------------------------

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
opend_italian$size <- ntopics(stm_fit_italian, out_italian)


## Constraint: LIWC component ---------------------------------------------

opend_italian_liwc <- liwcalike(opend_italian$resp, liwc_it)

### combine exclusive words and conjunctions (see Tausczik and Pennebaker 2010: 35)
opend_italian$constraint <- with(opend_italian_liwc,
                                 inclusi + esclusi
                                 #discrep + possib + inclusi + causa + intros + inibiz
                                 #- certez - negazio - esclusi
                                 )
# NOTE: based on 2007 dict
opend_italian$constraint <- opend_italian$constraint - min(opend_italian$constraint)
opend_italian$constraint <- opend_italian$constraint / max(opend_italian$constraint)


## Merge with full data and save -------------------------------------------

### compute combined measures
opend_italian$polknow_text <- with(opend_italian, size * range * constraint)
opend_italian$polknow_text_mean <- with(opend_italian, size + range + constraint)/3
opend_italian$polknow_text_scale <- as.numeric(scale(opend_italian$polknow_text_mean))




# save output -------------------------------------------------------------

save(swiss, opend_german, opend_french, opend_italian, meta, processed,
     out_german, out_french, out_italian,
     stm_fit_german, stm_fit_french, stm_fit_italian,
     file="calc/out/swiss.Rdata")
