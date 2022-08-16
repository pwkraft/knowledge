# =================================
# Measuring Sophistication in OE responses
# - Common cleaning of Immigration data
# - Author: Patrick Kraft
# - Date: 08/16/22
# =================================

library(tidyverse)
library(car)
library(dplyr)
library(SnowballC)
library(quanteda)
library(quanteda.dictionaries)
library(stm)
load("~/Dropbox/Uni/Data/LIWC/liwc2015.Rdata")
source("calc/func.R")

## custom functions
se <- function(x, na.rm = T){
  sd(x, na.rm = na.rm)/sqrt(length(na.omit(x)))
}
cilo <- function(x, na.rm = T){
  mean(x, na.rm = na.rm) - qnorm(.975) * se(x, na.rm)
}
cihi <- function(x, na.rm = T){
  mean(x, na.rm = na.rm) + qnorm(.975) * se(x, na.rm)
}

### Load raw data
num <- read_csv(here::here("~/Dropbox/Uni/Lab/immigration/data",
                           "Immigration_December 19, 2019_05.20.csv"), skip = 3,
                col_names = here::here("~/Dropbox/Uni/Lab/immigration/data",
                                       "Immigration_December 19, 2019_05.20.csv") %>%
                  read_lines(n_max = 1) %>%
                  strsplit(",") %>%
                  unlist)

### Clean/recode complete set of variables
immig <- num %>%
  transmute(
    ranid = `Random ID`,
    id = row_number(),
    age = age,
    female = dplyr::recode(gender, `1`=1, `2`=0, `3`=NA_real_),
    black = race == 2,
    educ_cont = (educ - 1)/5,
    educ = educ > 4,
    married = marital == 2,
    faminc = (income - 1)/6,  ## 0 = less than $20,000, 1 = $120,000 or more
    relig = (church - 1)/8,  ## 0 = never, 1 = more than once per week
    pid_cont = (dplyr::recode(pid, `1`=6, `2`=2, `3`=4, `4`=4) +  ## 0 = democrat, 1 = republican
                  dplyr::recode(pid_lean, `1`=1, `2`=-1, `3`=0, .missing=0) +
                  dplyr::recode(pid_rep, `1`=1, `2`=0, .missing=0) +
                  dplyr::recode(pid_dem, `1`=-1, `2`=0, .missing=0) - 1)/6,
    polint_att = (5 - polint)/4,  ## 0 = never, 1 = always
    educ_pid = educ_cont * pid_cont,
    taxes_oe = taxes_oe,
    jobs_oe = jobs_oe,
    comments = comments) %>%
  filter(!is.na(ranid)) %>%
  select(-ranid)


# Text-based political sophistication measure -----------------------------

## minor pre-processing
opend_immig <- data.frame(select(immig, taxes_oe, jobs_oe, comments)) %>%
  apply(2, function(x){
    x <- gsub("(^\\s+|\\s+$)","", x)
    x[x %in% c("nice....", "good", "I really don't have a clue.", "it's my decision",
               "I have nothing to say", "nice and good", "super and good",
               "it seems really", "it seems really good i love every part of it",
               "use services is to be contented", "immigration work is satisified",
               "I have no idea I work at a hospital", "I don't think it changes much of anything",
               "very nice", "like it", "unsure", "really don't know possble",
               "that is how i feel", "the movement is very useful too me.",
               "very different", "nice", "ITS MY OPINION", "I THINK ITS MY OPINION",
               "GREAT", "VERY NICE", "based on the above information",
               "fackbook, instagram, twitter,youtube", "environmental",
               "based on the information", "nothing", "I CHOOSE MY OPINION",
               "I READ ALL THE INSTRUCTION SO I CHOOSE MY OPINION", "ITS MY OPINION READ A STORY RESULTS",
               "us people is mostly invested in medical uses",
               "my investment is based oon the environmental situation based on returns iis comes",
               "based on the previous information", "its my opinion", "I dont know",
               "based on my experience", "Please see response to previous question.",
               "normal person like ones", "I think is that better", "Nothing",
               "that our wish", "that's my own wish", "more than is that better",
               "jobs is create for better", "Just a guess because I have no idea",
               "Good study work.", "We are conducting an academic survey about media usage and news consumption. I hope this article has encouraged you to start answering or at least improve your answering method to benefit your business. This is definitely an important strategy to implement, but it shouldn’t be abused - don’t always link your answers back to your business.", "The taxes used for more people.",
               "The creative job helps for more people.", "You have to give and take.",
               "I think it is about equal", "breaking news", "economy rate",
               "additional job", "additional jobs")] <- ""
    x <- gsub("//"," ", x , fixed = T)
    x <- gsub("\\s+"," ", x)
    x <- gsub("(^\\s+|\\s+$)","", x)
    return(x)
  })

## spell-checking
write.table(opend_immig, file = "calc/out/spell.csv",
            sep = ",", col.names = F, row.names = F)
spell_immig <- aspell("calc/out/spell.csv") %>%
  filter(Suggestions!="NULL")

## replace incorrect words
for(i in 1:nrow(spell_immig)){
  opend_immig[spell_immig$Line[i],] <- gsub(spell_immig$Original[i],
                                            unlist(spell_immig$Suggestions[i])[1],
                                            opend_immig[spell_immig$Line[i],])
}
opend_immig <- data.frame(id = immig$id, opend_immig, stringsAsFactors = F)

## word count in OE responses
immig$wc <- opend_immig[,c(-1,-4)] |>
  apply(1, paste, collapse = " ") |>
  stringr::str_count('\\w+')
immig$wc_comments <- stringr::str_count(immig$comments, '\\w+')


## Range: Shannon entropy of response lengths ----------------------

### range in item response
immig$range <- apply(opend_immig[,c(-1,-4)], 1, function(x){
  iwc <- str_count(x, "\\w+")
  shannon(iwc/sum(iwc))
})


## Size: Number of topics mentioned -----------------------------

### combine regular survey and open-ended data, remove empty responses
meta <- c("age", "educ_cont", "pid_cont", "educ_pid", "female")
data_oe <- immig %>% mutate(resp = apply(opend_immig[,c(-1,-4)], 1, paste, collapse = " "))

### remove additional whitespaces
data_oe$resp <- gsub("\\s+"," ", data_oe$resp)
data_oe$resp <- gsub("(^\\s+|\\s+$)","", data_oe$resp)

### remove missings on metadata
data_oe <- data_oe[apply(!is.na(data_oe[,meta]),1,prod)==1,]

### process for stm
processed_oe <- textProcessor(data_oe$resp, metadata = data_oe[,meta],
                                customstopwords = c("dont", "hes", "shes", "that", "etc"))
out_oe <- prepDocuments(processed_oe$documents, processed_oe$vocab,
                        processed_oe$meta, lower.thresh = 1)

### remove discarded observations from data
data_oe <- data_oe[-processed_oe$docs.removed,]
if(!is.null(out_oe$docs.removed)) data_oe <- data_oe[-out_oe$docs.removed,]

### stm fit with 49 topics
stm_fit_oe <- stm(out_oe$documents, out_oe$vocab,
                     prevalence = as.matrix(out_oe$meta),
                     K=0, seed=12345)

### compute number of considerations
data_oe$size <- ntopics(stm_fit_oe, out_oe)
data_oe <- select(data_oe, id, resp, size)


## Size: Number of topics mentioned (comments) -----------------------------

### combine regular survey and open-ended data, remove empty responses
meta <- c("age", "educ_cont", "pid_cont", "educ_pid", "female")
data_comments <- immig

### remove additional whitespaces
data_comments$comments <- gsub("\\s+"," ", data_comments$comments)
data_comments$comments <- gsub("(^\\s+|\\s+$)","", data_comments$comments)

### remove missings on metadata
data_comments <- data_comments[apply(!is.na(data_comments[,meta]),1,prod)==1,]

### process for stm
processed_comments <- textProcessor(data_comments$comments, metadata = data_comments[,meta],
                                    customstopwords = c("dont", "hes", "shes", "that", "etc"))
out_comments <- prepDocuments(processed_comments$documents, processed_comments$vocab,
                              processed_comments$meta, lower.thresh = 1)

### remove discarded observations from data
data_comments <- data_comments[-processed_comments$docs.removed,]
if(!is.null(out_comments$docs.removed)) data_comments <- data_comments[-out_comments$docs.removed,]

### stm fit with 49 topics
stm_fit_comments <- stm(out_comments$documents, out_comments$vocab,
                        prevalence = as.matrix(out_comments$meta),
                        K=0, seed=12345)

### compute number of considerations
data_comments$size_comments <- ntopics(stm_fit_comments, out_comments)
data_comments <- select(data_comments, id, size_comments)


# Merge data --------------------------------------------------------------

immig <- immig %>%
  left_join(data_oe) %>%
  left_join(data_comments)


## Constraint: LIWC component ---------------------------------------------

oe_liwc <- liwcalike(immig$resp, liwc)
comments_liwc <- liwcalike(immig$comments, liwc)

## combine exclusive words and conjunctions (see Tausczik and Pennebaker 2010: 35)
immig$constraint <- with(oe_liwc, (conj + differ) * WC)
immig$constraint_comments <- with(comments_liwc, (conj + differ) * WC)

immig$constraint <- immig$constraint - min(immig$constraint)
immig$constraint <- immig$constraint / max(immig$constraint)

immig$constraint_comments <- immig$constraint_comments - min(immig$constraint_comments)
immig$constraint_comments <- immig$constraint_comments / max(immig$constraint_comments)


## Merge with full data and save -------------------------------------------

### compute combined measures
immig$polknow_text <- with(immig, size * range * constraint)
immig$polknow_text_mean <- with(immig, size + range + constraint)/3
immig$polknow_text_scale <- as.numeric(scale(immig$polknow_text_mean))

immig$polknow_comments <- with(immig, size_comments * constraint_comments)
immig$polknow_comments_mean <- with(immig, size_comments + constraint_comments)/2
immig$polknow_comments_scale <- as.numeric(scale(immig$polknow_comments_mean))



# Save Output -------------------------------------------------------------

## make object name consistent with other scripts
data_immig <- immig

save(data_immig, opend_immig, spell_immig, meta,
     processed_oe, out_oe, stm_fit_oe,
     processed_comments, out_comments, stm_fit_comments,
     file="calc/out/immig.Rdata")
