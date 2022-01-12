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

## word count in OE responses
wc <- raw |>
  dplyr::select(UWM309, UWM310, UWM312, UWM313, UWM315,
                UWM316, UWM318, UWM319, UWM321, UWM322) |>
  apply(1, paste, collapse = " ") |>
  stringr::str_count('\\w+')

wc_sep <- raw |>
  dplyr::select(UWM309, UWM310, UWM312, UWM313, UWM315,
                UWM316, UWM318, UWM319, UWM321, UWM322) |>
  dplyr::transmute_all(~str_count(.,'\\w+')) |>
  dplyr::transmute(gun = UWM309 + UWM310,
                   abortion = UWM312 + UWM313,
                   daca = UWM315 + UWM316,
                   health = UWM318 + UWM319,
                   trade = UWM321 + UWM322,
                   female = factor(raw$gender - 1, labels = c("Male","Female"))) |>
  tidyr::gather(oe_item, wc, -female) |>
  dplyr::group_by(oe_item, female) |>
  dplyr::summarize(mean = mean(wc, na.rm = T), cilo = cilo(wc), cihi = cihi(wc))

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
    pk_combined = (pk_house + pk_senate + pk_ideol)/3,
    polknow_factual = (pk_house + pk_senate)/2,
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
    polatt = (5 - UWM329)/4,
    campint = (3 - UWM330)/2,
    polint = (polatt + campint)/2,
    effic_int = (UWM331 - UWM332 + 4)/8,
    effic_ext = (UWM333 + UWM334 - 2)/8,

    ## political participation
    vote_intent = dplyr::recode(as.numeric(CC18_350), `1` = 1, `3` = 1, .default = 0),
    vote = dplyr::recode(as.numeric(raw$CC18_401), `5` = 1, .default = 0),

    ## attitudes toward women representation
    want_more_women = (3 - UWM335)/2,
    expect_progress = (5 - UWM336)/4,
    govern_better = dplyr::recode(as.numeric(UWM337), `1` = 1, `2` = 0, `3` = .5),
    # NOTE - check other attitudinal items on women representation in post-election wave

    ## open-ended responses
    wc = wc,
    lwc = log(wc)/max(log(wc), na.rm = T)
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
             ,"don't know","don't no","","I have no clue","I do not know")] <- ""
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
  opend[spell$Line[i],] <- gsub(spell$Original[i], unlist(spell$Suggestions[i])[1]
                                , opend[spell$Line[i],])
}
opend <- data.frame(caseid = raw$caseid, opend, stringsAsFactors = F)


## Consistency: Shannon entropy of response lengths ----------------------

### consistency in item response
cces$consistency <- apply(opend, 1, function(x){
  iwc <- str_count(x, "\\w+")
  shannon(iwc/sum(iwc))
})


## Considerations: Number of topics mentioned -----------------------------

### combine regular survey and open-ended data, remove empty responses
meta <- c("age", "educ_cont", "pid_cont", "educ_pid", "female")
data_cces <- cces %>% mutate(resp = apply(opend, 1, paste, collapse = " "))

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
data_cces$considerations <- ntopics(stm_fit, out_cces)


## Word choice: LIWC component ---------------------------------------------

cces_liwc <- liwcalike(data_cces$resp, liwc)

## combine exclusive words and conjunctions (see Tausczik and Pennebaker 2010: 35)
data_cces$wordchoice <- with(cces_liwc,
                           (conj + differ) * WC,
                           #Sixltr + discrep + tentat + cause + insight - certain - negate - differ
)
# MISSING: Inclusiveness (incl), Inhibition (Inhib) -> replaced by Differentiation (differ)
data_cces$wordchoice <- data_cces$wordchoice - min(data_cces$wordchoice)
data_cces$wordchoice <- data_cces$wordchoice / max(data_cces$wordchoice)


## Merge with full data and save -------------------------------------------

### compute combined measures
data_cces$polknow_text <- with(data_cces, considerations * consistency * wordchoice)
data_cces$polknow_text_mean <- with(data_cces, considerations + consistency + wordchoice)/3



# Save Output -------------------------------------------------------------

save(cces, opend, spell, data_cces, meta, processed_cces, out_cces, stm_fit
     , file="calc/out/cces.Rdata")
