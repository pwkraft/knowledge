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
raw <- read_dta(paste0(datasrc,"citizencompetence_colombo.dta"))



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

## overall response length (this should actually be done by language!!!)
swiss$wc <- apply(select(swiss, prostring1:constring2), 1, function(x){
  length(unlist(strsplit(x,"\\s+")))
})
swiss$lwc <- log(swiss$wc)/max(log(swiss$wc))


## minor pre-processing skipped for now
# dash has to be fixed
# wc is incorrect at the moment (I think I still have to trim spaces)


### add meta information about responses

## opinionation (respondents either answered pro OR con, I should take that into account!!!)
swiss$opinionation <- apply(select(swiss, prostring1:constring2), 1, function(x){
  iwc <- unlist(lapply(strsplit(x,"\\s+"), length))
  1 - ineq(iwc,type="Gini")
})



### fit structural topic model

### prepare data

## combine regular survey and open-ended data
## ADD: type of eferendum as a meta-covariate
meta <- c("age", "edu", "ideol", "edu_ideol")
swiss$resp <- apply(select(swiss, prostring1:constring2),1,paste,collapse=' ')
  

## remove additional whitespaces
swiss$resp <- gsub("\\s+"," ", swiss$resp)
swiss$resp <- gsub("(^\\s+|\\s+$)","", swiss$resp)

## remove missings on metadata
data <- swiss[apply(!is.na(swiss[,meta]),1,prod)==1,]

## remove empty strings
data <- data[data$wc != 0, ]

## select language
opend_german <- data %>% filter(lang == 1)

## process for stm
processed <- textProcessor(opend_german$resp, metadata = opend_german[,meta]
                           , language = "german")
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

## remove discarded observations from data
opend_german <- opend_german[-processed$docs.removed,]
opend_german <- opend_german[-out$docs.removed,]

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
opend_german$topic_diversity <- apply(doc_topic_prob, 1, function(x) 1 - ineq(x,type="Gini"))

## text-based sophistication measures
opend_german$polknow_text <- with(opend_german, topic_diversity * lwc * opinionation)
opend_german$polknow_text_mean <- with(opend_german, (topic_diversity + lwc + opinionation)/3)


### save output

save(swiss, opend_german, data, meta, processed, out, stm_fit
     , file="calc/out/swiss.Rdata")
