# =================================
# Measuring Sophistication in OE responses
# - Prepare raw data for dataverse package
# - Author: Patrick Kraft
# =================================

library(tidyverse)
library(haven)

rm(list = ls())



# 2018 CES ----------------------------------------------------------------

ces2018raw <- read_sav("~/Dropbox/Uni/Data/cces2018/CCES18_UWM_OUTPUT_vv.sav") %>%
  select(caseid, birthyr, gender, educ, race, faminc_new, pew_churatd, pid7,
         CC18_334A:CC18_334J, CC18_350, CC18_351, CC18_351x,
         UWM309:UWM323, UWM329, UWM331:UWM334, UWM401:UWM410,
         SenCand1Name, SenCand2Name)



# 2020 ANES ---------------------------------------------------------------

anes2020raw <- read_dta("~/Dropbox/Uni/Data/anes2020/anes_timeseries_2020_stata_20210324.dta") %>%
  select(V200001, V201001, V201005, V201600, V201231x, V201452:V201454, V201507x, V201511x,
         V201549x, V201617x, V201644:V201647, V202001, V202109x, V202212:V202215) %>%
  left_join(read_csv("~/Dropbox/Uni/Data/anes2020/anes_timeseries_2020_redacted_openends.csv"))



# 2016 ANES ---------------------------------------------------------------

anes2016raw <- read_dta("~/Dropbox/Uni/Data/anes2016/anes_timeseries_2016.dta") %>%
  select(V160001, V160501, V161003, V161024x, V161026, V161030, V161158x,
         V161244, V161245, V161245a, V161267, V161270, V161310x, V161342,
         V161361x, V161497:V161506, V161513:V161516, V162215:V162218,
         V162333, V162337, V162338, V168016) %>%
  left_join(read.csv("~/Dropbox/Uni/Data/anes2016/anes_timeseries_2016_redacted_openends.csv", as.is = T))



# 2012 ANES ---------------------------------------------------------------

anes2012raw <- read_dta("~/Dropbox/Uni/Data/anes2012/anes_timeseries_2012.dta") %>%
  select(caseid, mode, gender_respondent_x, dem_age_r_x, dem_raceeth_x,
         dem_edugroup_x, incgroup_prepost_x, iwrobspre_levinfo,
         relig_churchoft, relig_church, relig_churchwk, pid_x,
         rvote2012_x, interest_attention, wordsum_setb:wordsum_seto,
         effic_complicstd, effic_complicrev, effic_undstd, effic_undrev,
         effic_carestd, effic_carerev, effic_saystd, effic_sayrev,
         profile_spanishsurv, admin_pre_lang_start, admin_post_lang_start,
         preknow_prestimes, preknow_sizedef, preknow_senterm,
         preknow_medicare, preknow_leastsp, tipi_extra, tipi_open, tipi_resv) %>%
  left_join(select(read.csv("~/Dropbox/Uni/Data/anes2012/anes2012TS_openends.csv", as.is = T),
                   caseid, candlik_likewhatdpc, candlik_dislwhatdpc, candlik_likewhatrpc,
                   candlik_dislwhatrpc, ptylik_lwhatdp, ptylik_dwhatdp,
                   ptylik_lwhatrp, ptylik_dwhatrp))



# 2015 YouGov -------------------------------------------------------------

yg2015raw <- read_csv("~/Dropbox/Uni/Data/YouGov2015/STBR0007_OUTPUT.csv") %>%
  select(caseid, Q2, Q3, Q5, Q6, starts_with("Q12"), Q13, Q14, Q24:Q31,
         gender, birthyr, race, educ, faminc, pew_churatd, pid7, treat_rand1)

## recode missing values in disease knowledge question
yg2015raw$Q13[is.na(yg2015raw$Q13)] <- 8
yg2015raw$Q14[is.na(yg2015raw$Q14)] <- 8

## recode missing values in political knowledge questions
yg2015raw$Q24[is.na(yg2015raw$Q24)] <- 8
yg2015raw$Q25[is.na(yg2015raw$Q25)] <- 8
yg2015raw$Q26[is.na(yg2015raw$Q26)] <- 8
yg2015raw$Q27[is.na(yg2015raw$Q27)] <- 8
yg2015raw$Q28[is.na(yg2015raw$Q28)] <- 8
yg2015raw$Q29[is.na(yg2015raw$Q29)] <- 8
yg2015raw$Q30[is.na(yg2015raw$Q30)] <- 8
yg2015raw$Q31[is.na(yg2015raw$Q31)] <- 8



# 2008 - 2012 Swiss Surveys -----------------------------------------------

swiss2012raw <- read_dta("~/Dropbox/Uni/Data/colombo/citizencompetence_colombo.dta", encoding = "latin 1") %>%
  select(nummer, sprache, lojr, age, edu, male, P04,
         prostring1, prostring2, constring1, constring2)

## empty strings
swiss2012raw$prostring1[swiss2012raw$prostring1 %in% c(99999997,99999998)] <- ""
swiss2012raw$prostring2[swiss2012raw$prostring2 %in% c(99999997,99999998)] <- ""
swiss2012raw$constring1[swiss2012raw$constring1 %in% c(99999997,99999998)] <- ""
swiss2012raw$constring2[swiss2012raw$constring2 %in% c(99999997,99999998)] <- ""



# 2019 MTurk Study --------------------------------------------------------

mturk2019raw <- read_csv(
  "~/Dropbox/Uni/Lab/immigration/data/Immigration_December 19, 2019_05.20.csv", skip = 3,
  col_names = read_lines("~/Dropbox/Uni/Lab/immigration/data/Immigration_December 19, 2019_05.20.csv", n_max = 1) %>%
    strsplit(",") %>% unlist()
) %>%
  select(`Random ID`, gender, age, race, educ, income, church, pid, pid_lean, pid_rep, pid_dem,
         employ, sales, taxes_oe, jobs_oe, contains("_trust_")) %>%
  filter(!is.na(`Random ID`))



# Open-ended missing responses --------------------------------------------

oe_na <- sort(unique(c("-1 inapplicable","-7 refused","n/a","no","none","#(43042)","i am","nome",
                       "i refuse", "i rwfuse to disclose", "refuse to disclose",
                       "dk","skip","no5","don't know","same","not really", "ditto",
                       "no idea", "can't say","no comment","no views","nope","not at all",
                       "no i can't","no i cant", "i don't know","iguess not","i dont know",
                       "dont know", "dint care","no no comment","no not really", "again no",
                       "1", "1 dk","dk5","no answer","hi","i","not","no commont",
                       "can't answer","no can not","dosen't know","he is not sure",
                       "its confidential","no answwer","not reaslly","lkjlkj","skjzhdkjhsd",
                       "you can", "even", "can","dont know dont talk about politics",
                       "dont knoiw","nono","not sure","do not know it","quit",
                       "doesnt know","she doesnt know","no not thinking","cant say",
                       "i don't know much", "would rather not explain","past",
                       "skipped question", "skip the question", "hjkdhfkjhdskjh",
                       "theuyidhfjdhkjdhfiaesjrhdjhflike shit", "dfdsjfksdjfkdsjf","dfsadfsf",
                       "god knows no i can't","no comments","dont want to comment",
                       "doesn't know","wants to skip","no not sure","no i caint", "not really no",
                       "i really cant say let me think","nope i don't know what liberal is",
                       "dont know what a conservative is dont care","she cannot",
                       "doesn't klnow", "no i cain't", "decline", "really can't",
                       "i choose not to","no i don't want to","no skip", "-1", "-9",
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
                       "None I can think of", "dint have a good answer for this",
                       "nice....", "good", "I really don't have a clue.", "it's my decision",
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
                       "additional job", "additional jobs",
                       "N/A","n/a","na","Na","__NA__","no","not sure","none","nothing","good",
                       "don't know","don't no","","I have no clue","I do not know",
                       "no","non","nein","rien","nichts","keinen","nsp","ka","n","prive","nono",
                       "si","oooooooooooo","ooooooooooo","ooooooooooo","ooooooooooooooo",
                       "llll","lll","xxx","xx","niente","weiss nicht","weis nicht","weiss nichts",
                       "keine ahnung","c","keine gründe","même raison","pas d'opinion","non mi ricordo",
                       "nessun motivo","non so","weis nicht mehr","weiss nicht mehr","kein kommentar",
                       "keine 2 grund","rien dautre","weis es nicht mehr","weiss es nicht mehr",
                       "keins","k angaben","kein weitere grund","keine anderen gründen","wn","keine",
                       "keiner","nichts mehr", # added below to address German responses (all loj = 0 codes)
                       "war mir nicht klar", "-", "---", "keine", "nein", "kam nicht draus um was es ging",
                       "nichts", "kam nicht draus", "97", "nicht^s", "wusste nicht um was es geht.",
                       "keine antwort", "nein.", "wusste nicht genau um was es geht", "keinen",
                       "1, nicht so befasst damit2. kein zweiter grund", "gar kein intresse",
                       "will grund nicht angeben", "ooooooooooooooo", "die gleichen gründe",
                       "nicht gedacht dabei", "war ratlos also nein", "ok", "weeeee", "wieso nicht",
                       "sie weiss es nicht mehr genau", "- weiss nicht", "gleiche antwort", "wie vorhin",
                       "weil das muss nicht jeder wissen", "weiss nicht mehr", "nichts mehr", ".",
                       "k.a.", "nix", "keinen wirklichen grund", "weiss es nicht mehr so genau.",
                       "wn", "nichts.", "n", "oooooooooo", "keine genauen gründe", "rigrnbrdtimmung",
                       "grund war ihm nicht so klar.", "s", "habe es nicht verstanden", "weis nicht",
                       "lesefehler gemacht darum falsch gewaehlt", "wenig informiert", "ooooooooooo",
                       "weiss nichts", "nicht genau verstanden um was es geht", "kene", "gleich wie vorher",
                       "habe es nicht ganz begriffen", "weiss nichts mehr.", "-----", "----",
                       "keine gründe", "grundlos", "hat sie nicht interessiert", "keinen", "vergessen",
                       "kann es nicht mehr sagen", "nicht mehr", "weiss nicht genau", "keines",
                       "weiss nicht genau,", "ssfds", "interessiert mich nicht", "weis nich habe",
                       "weis nich habe", "nichts mehr", "llll", "lll", "keine 2. grund", "...",
                       "war zuwenig informiert", "das gleiche", "kein weiterer grund", "...........",
                       "kein grund mehr", "kann nicht begründet", "kein 2. grund", "keine 2.grund",
                       "kann ich nicht sagen", "nichts.", "xx", "weis es nicht mehr", "weiss es nicht mehr",
                       "weiss nicht genau", "kein anderes grund", "bin nicht drausgekommen", "=",
                       "wusste nicht genau", "hat sich nicht damit befasst", "nein.", "das war alles",
                       "eigentliche nicht genau überlegt", "alles", "zu enig informiert", "hg",
                       "dito", "keine angabe", "hat sich icht damit b efasst, deshalb das nein",
                       "habe das zu wenig verstanden darum abgelehnt", "man hat sie nicht ganz verstanden",
                       "zuviel auf einmal", "macht sinn", "---------", "ich weissnicht",
                       "zu wenig befasst weiss nicht", "nichts mehr anderse", "weil bin nicht nachnahmen",
                       "habe mich nicht wirklich damit auseinander gesetzt", "schlecht erklärt",
                       "ist schon lange her", "weiss ich schlicht nicht mehr", "weiss nicht wieso",
                       "weiss es nicht mehr", "niochts", "der glich berüdig  lang so wie es ist",
                       "frage falsch verstanden", "kennt die gründe nicht.", "ist ihr zu fremt",
                       "das braucht es nicht", "nicht formulierbar", "bin nicht ganz genau nachgekommen",
                       "oooooooooooo", "hat mich nicht interessiert", "(verwechselt es mit osterweiterung)",
                       "weiss nicht mehr genau", "nichst mehr", "nein w.n.", "keines", "....",
                       ".....kennt sich zuwenig aus...", "neinn", "pers.", "-------------", "keiner",
                       "ein anderer grund", "nichts mehr", "hatte keinen durchblick.", "nicht genau",
                       "gleich", "überflog es nur kurz", "keine angaben", "kannte mich zuwenig aus",
                       "weiss grund nicht mehr", "nicht mehr", "kein", "ws", "ooooooooooo", "??",
                       "das interessiert uns garnicht", "der kugelschreiber hat nein geschrieben.",
                       "persönlich", "auch das gleiche", "kann ich nicht sagen bin sehr emotional")))



# Custom stopwords for STM ------------------------------------------------

stopwords <- c("dont", "hes", "shes", "that", "etc")



# LIWC word lists ---------------------------------------------------------

## English dictionary
load("~/Dropbox/Uni/Data/LIWC/liwc2015.Rdata")
dict_constraint <- data.frame(original = c(as.list(liwc)$Conj, as.list(liwc)$Differ)) %>%
  mutate(regex = paste0("\\b", original, "\\b"),
         regex = gsub("*\\b", "", regex, fixed = T))

## German dictionary
liwc_de <- as.list(
  dictionary(file = "~/Dropbox/Uni/Data/LIWC/German_LIWC2001_Dictionary.dic",
             format = "LIWC")
)
dict_constraint_de <- data.frame(original = c(liwc_de$Incl, liwc_de$Excl)) %>%
  mutate(regex = paste0("\\b", original, "\\b"),
         regex = gsub("*\\b", "", regex, fixed = T))

## French dictionary
liwc_fr <- as.list(
  dictionary(file = "~/Dropbox/Uni/Data/LIWC/French_LIWC2007_Dictionary.dic",
             format = "LIWC", encoding = "LATIN1")
)
dict_constraint_fr <- data.frame(original = c(liwc_fr$conjonction, liwc_fr$exclusion)) %>%
  mutate(regex = paste0("\\b", original, "\\b"),
         regex = gsub("*\\b", "", regex, fixed = T))

## Italian dictionary
liwc_it <- as.list(
  dictionary(file = "~/Dropbox/Uni/Data/LIWC/Italian_LIWC2007_Dictionary.dic",
             format = "LIWC", encoding = "LATIN1")
)
dict_constraint_it <- data.frame(original = c(liwc_it$Inclusi, liwc_it$Esclusi)) %>%
  mutate(regex = paste0("\\b", original, "\\b"),
         regex = gsub("*\\b", "", regex, fixed = T))

## remove LIWC files
rm(list = ls()[grep("liwc", ls())])



# Save raw data files -----------------------------------------------------

save.image("~/Dropbox/Uni/projects/2016/knowledge/dataverse/data/raw.Rdata")
