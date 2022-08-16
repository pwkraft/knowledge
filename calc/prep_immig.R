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
    x[x %in% c("N/A","n/a","na","Na","__NA__","no","not sure","none","nothing","good"
               ,"don't know","don't no","","I have no clue","I do not know", "Don't know",
               "Not sure", "No idea", "Idk", "None", "no comments", "?", "no", "yes",
               "I dont know", "Dk", "???", "Don't know.", "Do not know", "NO idea",
               "Unsure", "unsure", "Can't think of any", "Dont know", "None.", "bad", "for",
               "Rt7", "Btgj", "Ftujvfjnu", "Btvni", "Ctbhhhh UK bbg", "Fvvgffvg", "Rthgfh",
               "Egg digging f ghjvg", "Vytbn", "By my b", "ok", "yueah", "UNK",
               "Against", "Favor", "on", "None!", "Bullshit", "No reason", "never",
               "always", "Disagree", "No", "Yes", "Kind of", "i don't know", "do not know",
               "nonsense", "unknown", "Know comment", "No comment", "NONE", "Nobe", "not",
               "Nothing", "I don't care", "N/a", "im not sure", "do not know", "like it",
               "i don't know", "I don’t nnow", "No opinion", "000", "I don’t really know",
               "No response", "NA", "Not against", "NoZ.", "Dj.", "Who cares", "Not against",
               "yes agree", "dont agree", "_____?", "-----????", "i dont know", "not against it.",
               "no reason", "common sense", "no reasons", "Not", "notsure", "No thoughts", "NO",
               "YES", "dont know", "I don't know.", "whatever", "get reak", "I don't know", "idk",
               "dont know", "Wrong", "Right", "I dont kno", "H", ".", "Not against",
               "I'm not against it", "Yep", "dont know", "I don't know.", "Not.","No option",
               "Don’t know", "Im not sure", "I don't know", "Not", "NOT SURE", "NO OPINION",
               "no idea", "I I'm not sure.", "I'm not sure.", "g jk;l g", "gre m;jkl e",
               "jkl; grjkl g", "gs hjldsf", "tr.knjnjkger", "Zest Ajax's ;Oreg;Orr Gehrig arague rag",
               "rehi ECG's Iguassu", "eragnk jg", "g JFK;l g", "Gere m;JFKl e", "JFKl; grJFKl g",
               "gs holds", "tr.knjnjger", "no info", "I'm not.", "Have nothing", "Fuck if I know",
               "not at this time", "Not applicable.", "NOTHING", "not against ...",
               "I am not against it.", "I do not have the expertise in this area",
               "i do not have expertise in this area", "I am against them", "Can’t think of one",
               "know of no good reason..", "Hhh", "Agree", "Agree with this issue", "Ggg",
               'See my answer in "for" appeal.', "Im for them", "None come to mind",
               "None come to mind.", "needed", "not against", "na/", "None, this is crazy",
               "Uncheck", "Check", "Mot sure", "Vheck", "Non check", "not sure what this is",
               "nothing about his one", "Democrats", "Republicans", "I am against.",
               "It should not happen.", "yes i am", "no im not", "the democrats", "democrats",
               "republicans", "mpt sire", "bit syre", "Not against it.",
               "i honestly have no response to this", "i do not have a response to this",
               "no response sorry", "i dont know enough", "i dont enough about this",
               "i dont know what a tariffs is", "no comment", "I have no argument",
               "I don't have an opinion.", "I don't have an opinion", "I don’t knew", "I don’t know",
               "I don’t make", "I’m not sure.", "Not completely sure.", "IDK", "conservatives",
               "liberals", "Republican", "Honestly, I have no idea.", "i have no clue", "idk mah dude",
               "Non", "for it", "no opinion", "not for it", "not for a repeal", "Same",
               "Absolutely nothing.", "Nothing.", "ITS OK", "No clue", "I do not know of any",
               "We need", "Not good", "I'm not at all", "I’m not against this", "No against",
               "YEs I am", "No I am not", "Hell no I am not", "Yes, I am for this", "I am for this",
               "I am not for this", "not against", "i'M NOT AGAINST THEM", "Jerbs",
               "bad things", "good things", "goos things", "good news", "No reason not to.",
               "tricky", "No at all", "Not for it.", "IM NOT AGAINST", "NOTHING AGAINST",
               "in favor", "against", "not right", "right", "no comment", "Never.", "For it",
               "none i know of", "none should be.", "Conservatives", "Liberals", "Not for it",
               "no comment", "?? - not sure I can think of any", "nothing on this matter",
               "All", "Rep", "Dem", "In not against", "No answer", "No knowledge", "No ideas on this",
               "See other box.", "I have no idea", "I also have no idea", "Don'tknow", 'no good',
               "a very crazy", "no good idea", "is a cool", "is a cool", "crazy", "is crazy",
               "crazy total", "nothing to say", "also nothing", "positive", "negative",
               "I am for", "I don't see any", "Don't know on this one", "do not know much about it",
               "Can’t think of any.", "Don’t know enough on this topic", "confused", "High",
               "Keep this policy", "I don't really know.", "I I'm not sure.", "I'm not sure.",
               "I'm not sure", "I don't know what a tariffs is", "Don’t know.", "in certain cases",
               "In favor", "ID.", "No idea.", "no opinion", "I'm for it", "Not sure.", "I do not know.",
               "I don't know much about these.", "no good reasons", "I DINT KNOW MUCH ABOUT THIS TOPIC",
               "I dint know much", "I'm not sure", "No need", "<-", "..?", "Please!!", "DINT KNOW",
               "NOT AGAINST", "I really don't know", "not for it.","Can't think of a thing.",
               "None I can think of", "dint have a good answer for this")] <- ""
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
                     K=25, seed=12345)

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
                        K=25, seed=12345)

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
