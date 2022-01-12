# =================================
# Measuring Sophistication in OE responses
# - Common cleaning of CCES data
# - Author: Patrick Kraft
# - Date: 01/12/22
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

## load raw CCES data
raw <- haven::read_sav("/data/Dropbox/Uni/Data/cces2018/CCES18_UWM_OUTPUT_vv.sav")

## recode cces data
cces <- raw |>
  dplyr::transmute(
    weight = teamweight,

    ## sociodemographics
    age = 2018 - birthyr,
    #female = factor(gender - 1, labels = c("Male","Female")),
    female = gender - 1,
    educ_cont =  educ,
    educ = as.numeric(educ > 4),
    black = as.numeric(race == 2),
    married = as.numeric(marstat == 1),
    faminc = (na_if(faminc_new, 97)-1)/15,
    relig = (6-na_if(pew_churatd, 7))/5,

    ## political attitudes
    pid = dplyr::recode_factor(as.numeric(pid3),
                               `1` = "Democrat",
                               `2` = "Republican",
                               `3` = "Other",
                               .default = NA_character_),
    pid_cont = dplyr::recode(as.numeric(pid7),
                             `8` = 4),
    educ_pid = educ_cont * pid_cont,

    ## treatment groups
    treat_optimism = dplyr::if_else(UWM301 %in% c(1,3),
                                    "Optimism treatment", "Pessimism treatment"),
    treat_dk = dplyr::if_else(UWM301 %in% c(1,2),
                              "DK included", "DK excluded"), # true = include DK option

    ## CCES ideological placements
    ideo_trump = dplyr::recode(as.numeric(CC18_334C), `8` = NA_real_),
    ideo_dem = dplyr::recode(as.numeric(CC18_334D), `8` = NA_real_),
    ideo_rep = dplyr::recode(as.numeric(CC18_334E), `8` = NA_real_),
    ideo_sc = dplyr::recode(as.numeric(CC18_334F), `8` = NA_real_),
    ideo_warren = dplyr::recode(as.numeric(UWM401), `8` = NA_real_),
    ideo_ryan = dplyr::recode(as.numeric(UWM402), `8` = NA_real_),
    ideo_mcconnel = dplyr::recode(as.numeric(UWM403), `8` = NA_real_),
    ideo_schumer = dplyr::recode(as.numeric(UWM404), `8` = NA_real_),
    ideo_pelosi = dplyr::recode(as.numeric(UWM405), `8` = NA_real_),
    ideo_murkowski = dplyr::recode(as.numeric(UWM406), `8` = NA_real_),
    ideo_collins = dplyr::recode(as.numeric(UWM407), `8` = NA_real_),
    ideo_feinstein = dplyr::recode(as.numeric(UWM408), `8` = NA_real_),
    ideo_booker = dplyr::recode(as.numeric(UWM409), `8` = NA_real_),
    ideo_haley = dplyr::recode(as.numeric(UWM410), `8` = NA_real_),

    ## CCES common core knowledge questions
    pk_house = as.numeric(CC18_309a == 1),
    pk_house_dk = as.numeric(dplyr::recode(as.numeric(CC18_309a), `4` = NA_real_) == 1),
    pk_senate = as.numeric(CC18_309b == 1),
    pk_senate_dk = as.numeric(dplyr::recode(as.numeric(CC18_309b), `4` = NA_real_) == 1),
    pk_ideol = ifelse(is.na(ideo_dem) | is.na(ideo_rep),
                      0, as.numeric(ideo_dem < ideo_rep)),
    pk_ideol_dk = as.numeric(ideo_dem < ideo_rep),
    pk_combined = (pk_house + pk_senate)/2,
    polknow_factual = (pk_house + pk_senate + pk_ideol)/3,
    # NOTE - knowledge about state senates etc. (CC18_309c...) missing

    ## UWM knowledge about women representation
    repknow_cong = as.numeric(UWM302 >= 10 & UWM302 <= 30),
    repknow_sc = as.numeric(UWM303 == 3),
    repknow_bias = as.numeric(UWM304 == 1),
    repknow = (repknow_cong + repknow_sc + repknow_bias),
    # NOTE - UWM306: name woman senator missing

    ## UWM policy knowledge
    know_guns = as.numeric(UWM311 == 2),
    know_abortion = as.numeric(UWM314 == 4),
    know_daca = as.numeric(UWM317 == 4),
    know_health = as.numeric(UWM320 == 3),
    know_trade = as.numeric(UWM323 == 1),
    know = (know_trade + know_daca + know_abortion)/3,
    know5 = (know_trade + know_daca + know_health +
              know_guns + know_abortion)/5,
    know_female = (know_daca + know_health + know_abortion)/3,
    know_male = (know_guns + know_trade + know_daca)/3,
    dk_count = ifelse(treat_dk == "DK included",
                      (UWM311==5) + (UWM314==5) + (UWM317==5) + (UWM320==5) + (UWM323==4), NA),

    ## political interest etc.
    polint_att = (5 - UWM329)/4,
    polint_cam = (3 - UWM330)/2,
    polint = (polint_att + polint_cam)/2,
    effic_int = (UWM331 - UWM332 + 4)/8,
    effic_ext = (UWM333 + UWM334 - 2)/8,

    ## political participation
    vote = dplyr::recode(as.numeric(CC18_350), `1` = 1, `3` = 1, .default = 0),
    vote_post = dplyr::recode(as.numeric(raw$CC18_401), `5` = 1, .default = 0),

    ## attitudes toward women representation
    want_more_women = (3 - UWM335)/2,
    expect_progress = (5 - UWM336)/4,
    govern_better = dplyr::recode(as.numeric(UWM337), `1` = 1, `2` = 0, `3` = .5),
    # NOTE - check other attitudinal items on women representation in post-election wave

  )



# Text-based political sophistication measure -----------------------------

### open-ended responses
### MORE WORK ON PRE-PROCESSING NEEDED, check all steps, spell checking, stopword removal etc.

## minor pre-processing
opend <- raw |>
  dplyr::select(UWM309, UWM310, UWM312, UWM313, UWM315,
                UWM316, UWM318, UWM319, UWM321, UWM322) |>
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
             "NOT AGAINST"
             )] <- ""
  x <- gsub("//"," ", x , fixed = T)
  x <- gsub("\\s+"," ", x)
  x <- gsub("(^\\s+|\\s+$)","", x)
  return(x)
})

## spell-checking
write.table(opend, file = "calc/out/spell.csv"
            , sep = ",", col.names = F, row.names = F)
spell <- aspell("calc/out/spell.csv") %>%
  filter(Suggestions!="NULL")

## replace incorrect words
for(i in 1:nrow(spell)){
  opend[spell$Line[i],] <- gsub(spell$Original[i], unlist(spell$Suggestions[i])[1],
                                opend[spell$Line[i],])
}
opend <- data.frame(caseid = raw$caseid, opend, stringsAsFactors = F)

## word count in OE responses
cces$wc <- opend[,-1] |>
  apply(1, paste, collapse = " ") |>
  stringr::str_count('\\w+')
cces$lwc = log(cces$wc)/max(log(cces$wc), na.rm = T)


## Range: Shannon entropy of response lengths ----------------------

### range in item response
cces$range <- apply(opend[,-1], 1, function(x){
  iwc <- str_count(x, "\\w+")
  shannon(iwc/sum(iwc))
})


## Size: Number of topics mentioned -----------------------------

### combine regular survey and open-ended data, remove empty responses
meta <- c("age", "educ_cont", "pid_cont", "educ_pid", "female")
data_cces <- cces %>% mutate(resp = apply(opend[,-1], 1, paste, collapse = " "))

### remove additional whitespaces
data_cces$resp <- gsub("\\s+"," ", data_cces$resp)
data_cces$resp <- gsub("(^\\s+|\\s+$)","", data_cces$resp)

### remove missings on metadata
data_cces <- data_cces[apply(!is.na(data_cces[,meta]),1,prod)==1,]

### process for stm
processed_cces <- textProcessor(data_cces$resp, metadata = data_cces[,meta],
                                customstopwords = c("dont", "hes", "shes", "that", "etc"))
out_cces <- prepDocuments(processed_cces$documents, processed_cces$vocab, processed_cces$meta,
                          lower.thresh = 10)

### remove discarded observations from data
data_cces <- data_cces[-processed_cces$docs.removed,]
data_cces <- data_cces[-out_cces$docs.removed,]

### stm fit with 49 topics
stm_fit <- stm(out_cces$documents, out_cces$vocab, prevalence = as.matrix(out_cces$meta)
               , K=25, seed=12345)

### compute number of considerations
data_cces$size <- ntopics(stm_fit, out_cces)


## Constraint: LIWC component ---------------------------------------------

cces_liwc <- liwcalike(data_cces$resp, liwc)

## combine exclusive words and conjunctions (see Tausczik and Pennebaker 2010: 35)
data_cces$constraint <- with(cces_liwc,
                           (conj + differ) * WC,
                           #Sixltr + discrep + tentat + cause + insight - certain - negate - differ
)
# MISSING: Inclusiveness (incl), Inhibition (Inhib) -> replaced by Differentiation (differ)
data_cces$constraint <- data_cces$constraint - min(data_cces$constraint)
data_cces$constraint <- data_cces$constraint / max(data_cces$constraint)


## Merge with full data and save -------------------------------------------

### compute combined measures
data_cces$polknow_text <- with(data_cces, size * range * constraint)
data_cces$polknow_text_mean <- with(data_cces, size + range + constraint)/3



# Save Output -------------------------------------------------------------

save(cces, opend, spell, data_cces, meta, processed_cces, out_cces, stm_fit,
     file="calc/out/cces.Rdata")
