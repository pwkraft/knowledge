# ========================================================================= #
# Measuring Political Sophistication using Open-ended Responses
# Patrick Kraft
# ========================================================================= #
# - prepares the survey data of the 2020 ANES for subsequent analyses
# - prepares open-ended responses (selecting variables, spell checksing etc.)
# - fits structural topic model for open-ended responses
# - computes discursive sophistication measure



# Packages, data, and custom functions ------------------------------------

library(car)
library(tidyverse)
library(haven)
library(stm)
library(quanteda)
library(quanteda.dictionaries)
library(here)

raw2020 <- read_dta("~/Dropbox/Uni/Data/anes2020/anes_timeseries_2020_stata_20210324.dta")
raw2020_oe <- read_csv("~/Dropbox/Uni/Data/anes2020/anes_timeseries_2020_redacted_openends.csv")
load("~/Dropbox/Uni/Data/LIWC/liwc2015.Rdata")

source("calc/func.R")



# Recode main survey data -------------------------------------------------

anes2020 <- raw2020 %>% transmute(

  ## respondent id
  caseid = V200001,

  ## interview mode (1=Video, 2=Phone, 3=Web)
  mode = as.numeric(V200002 == 3),

  ## political knowledge (office recognition, post-election)
  polknow_office = ((na_in(V202138y, -7:-1) == 1) +
                      (na_in(V202139y1, -7:-1) == 1) +
                      (na_in(V202140y1, -7:-1) == 1) +
                      (na_in(V202141y1, -7:-1) == 1) +
                      (na_in(V202142y1, -7:-1) == 1))/5,

  ## political knowledge (factual knowledge questions, pre-election
  polknow_factual = ((na_if(V201644, -5) == 6) +
                       (na_if(V201645, -5) == 1) +
                       (na_if(V201646, -5) == 1) +
                       (na_if(V201647, -5) == 2))/4,

  ## education (bachelor degree)
  educ = as.numeric(na_in(V201511x, -9:-2) >= 4),

  ## education (continuous)
  educ_cont = as.numeric(na_in(V201511x, -9:-2)),

  ## political media exposure
  polmedia = (na_in(V201629a, -9:-1) + na_in(V201629b, -9:-1) +
                na_in(V201629c, -9:-1) + na_in(V201629d, -9:-1)) / 4,
  polmedia = if_else(V201629e == 1, 0, polmedia),

  ## political discussion
  poldisc = na_in(V202023, -9:-1)/7,
  poldisc = if_else(V202022 == 2, 0, poldisc),

  ## political interest (pay attention to politics)
  polint_att = (5 - na_if(V201005, -9))/4,

  ## political interest (following campaign)
  polint_cam = (3 - na_if(V201006, -9))/2,

  ## overall political interest
  polint = (polint_att + polint_cam)/2,

  ## internal efficacy
  effic_int = (na_in(V202214, -9:-1) - 1
               - na_in(V202213, -9:-1) + 5)/8,

  ## external efficacy
  effic_ext = (na_in(V202212, -9:-1) - 1
               + na_in(V202213, -9:-1) - 1)/8,

  ## overall efficacy
  effic = (effic_int + effic_ext)/2,

  ## voted in previous election
  pastvote = as.numeric(na_in(V201101, -9:-1) == 1 |
                          na_in(V201102, -9:-1) == 1),

  ## voted in current election
  vote = na_if(V202109x, -2),

  ## participated in protest march / rally
  protest = as.numeric(na_in(V202025, -9:-1) == 1),

  ## signed a petition
  petition = as.numeric(na_in(V202026, -9:-1) == 1),

  ## wear a campaign button
  button = as.numeric(na_in(V202015, -9:-1) == 1),

  ## letter to congressman/senator
  letter = as.numeric(na_in(V202030, -9:-1) == 1),

  ## additive index non-conventional participation
  part = protest + petition + button + letter,

  ## party identification (factor/dummies)
  pid = recode_factor(as.numeric(V201231x),
                      `1` = "Democrat",
                      `2` = "Democrat",
                      `3` = "Independent",
                      `4` = "Independent",
                      `5` = "Independent",
                      `6` = "Republican",
                      `7` = "Republican",
                      .default = NA_character_),
  pid_dem = as.numeric(pid=="Democrat"),
  pid_rep = as.numeric(pid=="Republican"),

  ## pid continuous
  pid_cont = (na_in(V201231x, -9:-1) - 4)/3,

  ## strength of partisanship
  pid_str = abs(pid_cont),

  ## interaction: pid * education
  educ_pid = educ_cont * pid_cont,

  ## religiosity (church attendance)
  relig = (5 - na_in(V201453, -9:-1))/5,
  relig = ifelse(V201452 == 2, 0, relig),
  relig = ifelse(V201454 == 2, 1, relig),

  ## age
  age = na_if(V201507x, -9),

  ## log(age)
  lage = log(age),

  ## sex
  female = na_if(V201600, -9) - 1,

  ## race
  black = as.numeric(na_in(V201549x, -9:-1) == 2),

  ## income
  faminc = na_in(V201617x, -9:-1)/21,

  ## spanish speaking respondent
  spanish = as.numeric(V201001 == 2 | V202001 == 2)
)



# Prepare open-ended responses --------------------------------------------

### NOTE: MORE WORK ON PRE-PROCESSING NEEDED, check all steps, spell checking, stopword removal etc.

## read original open-ended responses (downloaded from anes website)
anes2020opend <- read_csv("~/Dropbox/Uni/Data/anes2020/anes_timeseries_2020_redacted_openends.csv")

## identify spanish respondents
anes2020$spanish2 <- as.numeric(cld2::detect_language(apply(anes2020opend[,-1],1,paste,collapse=' ')) != "en")

## minor pre-processing
anes2020spell <- apply(anes2020opend[,-1], 2, function(x){
  x <- char_tolower(x)
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
write.table(anes2020spell, file = "calc/out/anes2020oe.csv",
            sep = ",", col.names = F, row.names = F)
spell <- aspell("calc/out/anes2020oe.csv") %>%
  filter(Suggestions!="NULL")

## replace incorrect words
for(i in 1:nrow(spell)){
  anes2020spell[spell$Line[i],] <- gsub(spell$Original[i], unlist(spell$Suggestions[i])[1],
                                        anes2020spell[spell$Line[i],])
}
anes2020spell <- data.frame(caseid = anes2020opend$V200001, anes2020spell,stringsAsFactors = F)



# Text-based political sophistication measure -----------------------------


## Consistency: Shannon entropy of response lengths ----------------------

### overall response length
anes2020$wc <- apply(anes2020spell[,-1], 1, function(x){
  sum(str_count(x, "\\w+"))
})
anes2020$lwc <- log(anes2020$wc)/max(log(anes2020$wc), na.rm = T)

### consistency in item response
anes2020$consistency <- apply(anes2020spell[,-1], 1, function(x){
  iwc <- str_count(x, "\\w+")
  shannon(iwc/sum(iwc))
})


## Considerations: Number of topics mentioned -----------------------------

### combine regular survey and open-ended data, remove spanish and empty responses
meta2020 <- c("age", "educ_cont", "pid_cont", "educ_pid", "female")
data2020 <- anes2020 %>% mutate(resp = apply(anes2020spell[,-1],1,paste,collapse=' ')) %>%
  filter(spanish == 0 & wc != 0)

### remove additional whitespaces
data2020$resp <- gsub("\\s+"," ", data2020$resp)
data2020$resp <- gsub("(^\\s+|\\s+$)","", data2020$resp)

### remove missings on metadata
data2020 <- data2020[apply(!is.na(data2020[,meta2020]),1,prod)==1,]

### process for stm
processed2020 <- textProcessor(data2020$resp, metadata = data2020[,meta2020],
                               customstopwords = c("dont", "hes", "shes", "that", "etc"))
out2020 <- prepDocuments(processed2020$documents, processed2020$vocab, processed2020$meta,
                         lower.thresh = 10)

### remove discarded observations from data
data2020 <- data2020[-processed2020$docs.removed,]
data2020 <- data2020[-out2020$docs.removed,]

### stm fit with 49 topics
stm_fit2020 <- stm(out2020$documents, out2020$vocab, prevalence = as.matrix(out2020$meta),
                   K=25, seed=12345)

### compute number of considerations
data2020$considerations <- ntopics(stm_fit2020, out2020)


## Word choice: LIWC component ---------------------------------------------

anes2020_liwc <- liwcalike(data2020$resp, liwc)

### combine exclusive words and conjunctions (see Tausczik and Pennebaker 2010: 35)
data2020$wordchoice <- with(anes2020_liwc,
                            (conj + differ) * WC,
                            # Sixltr + discrep + tentat + cause + insight - certain - negate - differ
                            )
# MISSING: Inclusiveness (incl), Inhibition (Inhib) -> replaced by Differentiation (differ)
data2020$wordchoice <- data2020$wordchoice - min(data2020$wordchoice)
data2020$wordchoice <- data2020$wordchoice / max(data2020$wordchoice)


## Merge with full data and save -------------------------------------------

### compute combined measures
data2020$polknow_text <- with(data2020, considerations * consistency * wordchoice)
data2020$polknow_text_mean <- with(data2020, considerations + consistency + wordchoice)/3



# save output -------------------------------------------------------------

save(anes2020, anes2020opend, anes2020spell, data2020, meta2020, processed2020, out2020, stm_fit2020,
     file="calc/out/anes2020.Rdata")
