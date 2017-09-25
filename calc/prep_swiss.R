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
library(car)
library(dplyr)
library(quanteda)
library(stm)
library(haven)
library(ineq)
setwd("/data/Dropbox/Uni/Projects/2016/knowledge/")
datasrc <- "/home/patrick/Dropbox/Uni/Data/colombo/"
raw <- read_dta(paste0(datasrc,"citizencompetence_colombo.dta"), encoding = "latin 1")



### prep/recode survey data

## respondent id
swiss <- select(raw, nummer, prostring1, prostring2, constring1, constring2)

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



### open-ended responses
### MORE WORK ON PRE-PROCESSING NEEDED, check all steps, spell checking, stopword removal etc.

## empty strings
swiss$prostring1[swiss$prostring1 %in% c(99999997,99999998)] <- ""
swiss$prostring2[swiss$prostring2 %in% c(99999997,99999998)] <- ""
swiss$constring1[swiss$constring1 %in% c(99999997,99999998)] <- ""
swiss$constring2[swiss$constring2 %in% c(99999997,99999998)] <- ""

## combine pro and con
swiss$string1 <- tolower(paste(swiss$prostring1, swiss$constring1))
swiss$string2 <- tolower(paste(swiss$prostring2, swiss$constring2))
  
## minor pre-processing
opend <- apply(swiss[,c("string1","string2")], 2, function(x){
  x <- gsub("[[:punct:]]","", x)  
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

## overall response length (this should actually be done by language!!!)
swiss$wc <- apply(opend, 1, function(x){
  length(unlist(strsplit(x,"\\s+")))
})
swiss$lwc <- log(swiss$wc)/max(log(swiss$wc))

## opinionation (respondents either answered pro OR con, I should take that into account!!!)
swiss$opinionation <- apply(opend, 1, function(x){
  iwc <- unlist(lapply(strsplit(x,"\\s+"), length))
  1 - ineq(iwc,type="Gini")
})



### fit structural topic model

### prepare data

## combine regular survey and open-ended data
## ADD: type of referendum as a meta-covariate
meta <- c("age", "edu", "ideol", "edu_ideol")
swiss$resp <- apply(select(swiss, prostring1:constring2),1,paste,collapse=' ')
  

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
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

## remove discarded observations from data
opend_german <- opend_german[-processed$docs.removed,]
opend_german <- opend_german[-out$docs.removed,]

## quick fit (30 topics)
stm_fit <- stm(out$documents, out$vocab, prevalence = as.matrix(out$meta)
              , K=30, init.type = "Spectral")

## slow, smart fit: estimates about 80 topics, might be too large (also, K is not deterministic here)
#stm_fit <- stm(out$documents, out$vocab, prevalence = as.matrix(out$meta)
#               , K=0, init.type = "Spectral")


### create new sophistication measures

## probability fits and transform for diversity score  
doc_topic_prob <- stm_fit$theta

## topic diversity score
opend_german$topic_diversity <- apply(doc_topic_prob, 1, function(x) 1 - ineq(x,type="Gini"))

## text-based sophistication measures
opend_german$polknow_text <- with(opend_german, topic_diversity * lwc * opinionation)
opend_german$polknow_text_mean <- with(opend_german, (topic_diversity + lwc + opinionation)/3)


################
### NEW MEASURE
################

## compute shannon entropy
shannon <- function(x, reversed = F){
  out <- (- sum(log(x^x)/log(length(x))))
  if(reversed) out <- 1 - out
  out
}

## which topic has highest likelihood for each word
term_topic <- apply(stm_fit$beta$logbeta[[1]], 2, which.max)

## shannon entropy of each term as a measure for its distinctiveness -> vague term or clear topic
pseudop <- exp(stm_fit$beta$logbeta[[1]])
term_entropy <- apply(pseudop, 2, max)

## combine both measures with open-ended responses
know <- data.frame(ntopics = rep(NA, length(out$documents))
                   , entropy = rep(NA, length(out$documents)))
for(doc in 1:length(out$documents)){
  know$ntopics[doc] <- length(unique(term_topic[out$documents[[doc]][1,]]))
  know$entropy[doc] <- log(sum(term_entropy[out$documents[[doc]][1,]] * out$documents[[doc]][2,]))
}
know$ntopics <- know$ntopic/max(know$ntopics)
know$entropy <- (know$entropy-min(know$entropy))/(max(know$entropy)-min(know$entropy))
opend_german <- cbind(opend_german, know)

opend_german$polknow_text <- opend_german$ntopics * opend_german$entropy * opend_german$ditem
opend_german$polknow_text_mean <- (opend_german$ntopics + opend_german$entropy + opend_german$ditem)/3

###################
### END NEW MEASURE
###################


#####################################################
### French respondents

## select language
opend_french <- data %>% filter(lang == 2)

## process for stm
processed <- textProcessor(opend_french$resp, metadata = opend_french[,meta]
                           , language = "french")
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

## remove discarded observations from data
opend_french <- opend_french[-processed$docs.removed,]
opend_french <- opend_french[-out$docs.removed,]

## quick fit (60 topics)
stm_fit <- stm(out$documents, out$vocab, prevalence = as.matrix(out$meta)
              , K=30, init.type = "Spectral")

## slow, smart fit: estimates about 80 topics, might be too large (also, K is not deterministic here)
#stm_fit <- stm(out$documents, out$vocab, prevalence = as.matrix(out$meta)
#               , K=0, init.type = "Spectral")


### create new sophistication measures

## probability fits and transform for diversity score  
doc_topic_prob <- stm_fit$theta

## topic diversity score
opend_french$topic_diversity <- apply(doc_topic_prob, 1, function(x) 1 - ineq(x,type="Gini"))

## text-based sophistication measures
opend_french$polknow_text <- with(opend_french, topic_diversity * lwc * opinionation)
opend_french$polknow_text_mean <- with(opend_french, (topic_diversity + lwc + opinionation)/3)


################
### NEW MEASURE
################

## compute shannon entropy
shannon <- function(x, reversed = F){
  out <- (- sum(log(x^x)/log(length(x))))
  if(reversed) out <- 1 - out
  out
}

## which topic has highest likelihood for each word
term_topic <- apply(stm_fit$beta$logbeta[[1]], 2, which.max)

## shannon entropy of each term as a measure for its distinctiveness -> vague term or clear topic
pseudop <- exp(stm_fit$beta$logbeta[[1]])
term_entropy <- apply(pseudop, 2, max)

## combine both measures with open-ended responses
know <- data.frame(ntopics = rep(NA, length(out$documents))
                   , entropy = rep(NA, length(out$documents)))
for(doc in 1:length(out$documents)){
  know$ntopics[doc] <- length(unique(term_topic[out$documents[[doc]][1,]]))
  know$entropy[doc] <- log(sum(term_entropy[out$documents[[doc]][1,]] * out$documents[[doc]][2,]))
}
know$ntopics <- know$ntopic/max(know$ntopics)
know$entropy <- (know$entropy-min(know$entropy))/(max(know$entropy)-min(know$entropy))
opend_french <- cbind(opend_french, know)

opend_french$polknow_text <- opend_french$ntopics * opend_french$entropy * opend_french$ditem
opend_french$polknow_text_mean <- (opend_french$ntopics + opend_french$entropy + opend_french$ditem)/3

###################
### END NEW MEASURE
###################


#####################################################
### Italian respondents

## select language
opend_italian <- data %>% filter(lang == 3)

## process for stm
processed <- textProcessor(opend_italian$resp, metadata = opend_italian[,meta]
                           , language = "italian")
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

## remove discarded observations from data
opend_italian <- opend_italian[-processed$docs.removed,]
opend_italian <- opend_italian[-out$docs.removed,]

## quick fit (60 topics)
stm_fit <- stm(out$documents, out$vocab, prevalence = as.matrix(out$meta)
              , K=30, init.type = "Spectral")

## slow, smart fit: estimates about 80 topics, might be too large (also, K is not deterministic here)
#stm_fit <- stm(out$documents, out$vocab, prevalence = as.matrix(out$meta)
#               , K=0, init.type = "Spectral")


### create new sophistication measures

## probability fits and transform for diversity score  
doc_topic_prob <- stm_fit$theta

## topic diversity score
opend_italian$topic_diversity <- apply(doc_topic_prob, 1, function(x) 1 - ineq(x,type="Gini"))

## text-based sophistication measures
opend_italian$polknow_text <- with(opend_italian, topic_diversity * lwc * opinionation)
opend_italian$polknow_text_mean <- with(opend_italian, (topic_diversity + lwc + opinionation)/3)

################
### NEW MEASURE
################

## compute shannon entropy
shannon <- function(x, reversed = F){
  out <- (- sum(log(x^x)/log(length(x))))
  if(reversed) out <- 1 - out
  out
}

## which topic has highest likelihood for each word
term_topic <- apply(stm_fit$beta$logbeta[[1]], 2, which.max)

## shannon entropy of each term as a measure for its distinctiveness -> vague term or clear topic
pseudop <- exp(stm_fit$beta$logbeta[[1]])
term_entropy <- apply(pseudop, 2, max)

## combine both measures with open-ended responses
know <- data.frame(ntopics = rep(NA, length(out$documents))
                   , entropy = rep(NA, length(out$documents)))
for(doc in 1:length(out$documents)){
  know$ntopics[doc] <- length(unique(term_topic[out$documents[[doc]][1,]]))
  know$entropy[doc] <- log(sum(term_entropy[out$documents[[doc]][1,]] * out$documents[[doc]][2,]))
}
know$ntopics <- know$ntopic/max(know$ntopics)
know$entropy <- (know$entropy-min(know$entropy))/(max(know$entropy)-min(know$entropy))
opend_italian <- cbind(opend_italian, know)

opend_italian$polknow_text <- opend_italian$ntopics * opend_italian$entropy * opend_italian$ditem
opend_italian$polknow_text_mean <- (opend_italian$ntopics + opend_italian$entropy + opend_italian$ditem)/3

###################
### END NEW MEASURE
###################

### save output

save(swiss, opend_german, opend_french, opend_italian, data, meta, processed, out, stm_fit
     , file="calc/out/swiss.Rdata")
