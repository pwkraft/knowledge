# =================================
# Measuring Sophistication in OE responses
# - Prepare raw data for dataverse package
# - Author: Patrick Kraft
# =================================

library(tidyverse)



# 2018 CES ----------------------------------------------------------------

raw_ces <- haven::read_sav("~/Dropbox/Uni/Data/cces2018/CCES18_UWM_OUTPUT_vv.sav") %>%
  select(caseid, birthyr, gender, educ)


dvs <- c("vote", "polint_att", "effic_int", "effic_ext")
ivs <- c("polknow_text_scale", "polknow_factual_scale",
         "female", "age", "black", "educ", "faminc", "relig"
)
ivs <- c("polknow_text_scale * polknow_factual_scale",
         "educ", "age", "black", "faminc", "relig")

ideo <- c("ideo_dem", "ideo_rep", "ideo_sc", "ideo_trump", "ideo_warren", "ideo_ryan",
          "ideo_mcconnel", "ideo_schumer", "ideo_pelosi", "ideo_murkowski", "ideo_collins",
          "ideo_feinstein", "ideo_booker", "ideo_haley")

SenCand1Avg <- data_cces %>%
  group_by(SenCand1Name) %>%
  summarize(SenCand1Avg = mean(ideo_cand1, na.rm = T)) %>%
  na.omit()

SenCand2Avg <- data_cces %>%
  group_by(SenCand2Name) %>%
  summarize(SenCand2Avg = mean(ideo_cand2, na.rm = T)) %>%
  na.omit()

data_cces <- data_cces %>%
  left_join(SenCand1Avg) %>%
  left_join(SenCand2Avg) %>%
  mutate(correct_vote = as.numeric(abs(ideo_ego - SenCand1Avg) > abs(ideo_ego - SenCand2Avg)) + 1,
         correct_vote = as.numeric(correct_vote == senate_vote))


# 2015 YouGov -------------------------------------------------------------

raw_yg <- read_csv("/data/Dropbox/Uni/Data/YouGov2015/STBR0007_OUTPUT.csv") %>%
  select(caseid, Q2, Q3, Q5, Q6, starts_with("Q12"), Q13, Q14, Q24:Q31,
         gender, birthyr, race, educ, faminc, pew_churatd, pid7)



# Open-ended missing responses --------------------------------------------

oe_na <- sort(unique(c("N/A","n/a","na","Na","__NA__","no","not sure","none","nothing","good",
                       "don't know","don't no","","I have no clue","I do not know",
                       "N/A","n/a","na","Na","__NA__","no","not sure","none","nothing","good",
                       "don't know","don't no","","I have no clue","I do not know", "Don't know",
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
                       "None I can think of", "dint have a good answer for this")))
