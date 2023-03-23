# ========================================================================= #
# Project: Women Also Know Stuff (APSR)
# - Script: Load raw data, preprocess text, export for analysis
# - Author: Patrick Kraft (patrickwilli.kraft@uc3m.es)
# ========================================================================= #


# Load raw data, packages, and custom functions ---------------------------

source(here::here("00-func.R"))
load(here("data/raw.Rdata"))


# 2018 CES ----------------------------------------------------------------

ces2018 <- ces2018raw %>% transmute(

  ## Sociodemographics and other controls
  caseid = caseid,
  female = gender - 1,
  age = 2018 - birthyr,
  black = as.numeric(race == 2),
  educ_cont =  educ,
  educ = as.numeric(educ > 4),
  pid_cont = recode(as.numeric(pid7), `8` = 4),
  educ_pid = educ_cont * pid_cont,
  faminc = (na_if(faminc_new, 97)-1)/15,
  relig = (6-na_if(pew_churatd, 7))/5,

  ## Political engagement
  vote = recode(as.numeric(CC18_350), `1` = 1, `3` = 1, .default = 0),
  polint = (5 - UWM329)/4,
  effic_int = (UWM331 - UWM332 + 4)/8,
  effic_ext = (UWM333 + UWM334 - 2)/8,

  ## Factual knowledge
  know_guns = as.numeric(UWM311 == 2),
  know_abortion = as.numeric(UWM314 == 4),
  know_daca = as.numeric(UWM317 == 4),
  know_health = as.numeric(UWM320 == 3),
  know_trade = as.numeric(UWM323 == 1),
  polknow = as.numeric(scale(know_trade + know_daca + know_health + know_guns + know_abortion)),

  ## Open-ended responses
  oe_guns_pro = oe_clean(UWM309),
  oe_guns_con = oe_clean(UWM310),
  oe_abortion_pro = oe_clean(UWM312),
  oe_abortion_con = oe_clean(UWM313),
  oe_daca_pro = oe_clean(UWM315),
  oe_daca_con = oe_clean(UWM316),
  oe_health_pro = oe_clean(UWM318),
  oe_health_con = oe_clean(UWM319),
  oe_trade_pro = oe_clean(UWM321),
  oe_trade_con = oe_clean(UWM322),
  wc = str_count(oe_guns_pro, '\\w+') + str_count(oe_guns_con, '\\w+') +
    str_count(oe_abortion_pro, '\\w+') + str_count(oe_abortion_con, '\\w+') +
    str_count(oe_daca_pro, '\\w+') + str_count(oe_daca_con, '\\w+') +
    str_count(oe_health_pro, '\\w+') + str_count(oe_health_con, '\\w+') +
    str_count(oe_trade_pro, '\\w+') + str_count(oe_trade_con, '\\w+'),

  ## Miscellaneous
  ideo_trump = recode(as.numeric(CC18_334C), `8` = NA_real_),
  ideo_dem = recode(as.numeric(CC18_334D), `8` = NA_real_),
  ideo_rep = recode(as.numeric(CC18_334E), `8` = NA_real_),
  ideo_sc = recode(as.numeric(CC18_334F), `8` = NA_real_),
  ideo_warren = recode(as.numeric(UWM401), `8` = NA_real_),
  ideo_ryan = recode(as.numeric(UWM402), `8` = NA_real_),
  ideo_mcconnel = recode(as.numeric(UWM403), `8` = NA_real_),
  ideo_schumer = recode(as.numeric(UWM404), `8` = NA_real_),
  ideo_pelosi = recode(as.numeric(UWM405), `8` = NA_real_),
  ideo_murkowski = recode(as.numeric(UWM406), `8` = NA_real_),
  ideo_collins = recode(as.numeric(UWM407), `8` = NA_real_),
  ideo_feinstein = recode(as.numeric(UWM408), `8` = NA_real_),
  ideo_booker = recode(as.numeric(UWM409), `8` = NA_real_),
  ideo_haley = recode(as.numeric(UWM410), `8` = NA_real_),
  ideo_cand1 = recode(as.numeric(CC18_334I), `8` = NA_real_),
  ideo_cand2 = recode(as.numeric(CC18_334J), `8` = NA_real_),
  ideo_ego = recode(as.numeric(CC18_334A), `8` = NA_real_),
  senate_vote = as.numeric(CC18_351),
  senate_vote = ifelse(!is.na(senate_vote), senate_vote, as.numeric(CC18_351x))
)
ces2018disc <- discursive(data = ces2018,
                          openends = colnames(ces2018)[grep("oe_", colnames(ces2018))],
                          meta = c("age", "educ_cont", "pid_cont", "educ_pid", "female"),
                          args_textProcessor = list(customstopwords = stopwords),
                          args_prepDocuments = list(lower.thresh = 10),
                          args_stm = list(K = 25, seed = 12345, verbose = FALSE),
                          dictionary = dict_constraint$regex)
ces2018 <- bind_cols(ces2018, ces2018disc$output)


# 2020 ANES ---------------------------------------------------------------

anes2020 <- anes2020raw %>% transmute(

  ## Sociodemographics and other controls
  caseid = V200001,
  female = na_if(V201600, -9) - 1,
  age = as.numeric(na_if(V201507x, -9)),
  black = as.numeric(na_in(V201549x, -9:-1) == 2),
  educ = as.numeric(na_in(V201511x, -9:-2) >= 4),
  educ_cont = as.numeric(na_in(V201511x, -9:-2)),
  pid_cont = (na_in(V201231x, -9:-1) - 4)/3,
  educ_pid = educ_cont * pid_cont,
  faminc = na_in(V201617x, -9:-1)/21,
  relig = (5 - na_in(V201453, -9:-1))/5,
  relig = ifelse(V201452 == 2, 0, relig),
  relig = ifelse(V201454 == 2, 1, relig),

  ## Political engagement
  vote = na_if(V202109x, -2),
  polint = (5 - na_if(V201005, -9))/4,
  effic_int = (na_in(V202214, -9:-1) - 1 - na_in(V202215, -9:-1) + 5)/8,
  effic_ext = (na_in(V202212, -9:-1) - 1 + na_in(V202213, -9:-1) - 1)/8,

  ## Factual knowledge
  polknow = as.numeric(scale((na_if(V201644, -5) == 6) +
                               (na_if(V201645, -5) == 1) +
                               (na_if(V201646, -5) == 1) +
                               (na_if(V201647, -5) == 2))),

  ## Open-ended responses
  oe_likedpc = oe_clean(V201107, tolower = TRUE),
  oe_disldpc = oe_clean(V201109, tolower = TRUE),
  oe_likerpc = oe_clean(V201111, tolower = TRUE),
  oe_dislrpc = oe_clean(V201113, tolower = TRUE),
  oe_likedp = oe_clean(V201159, tolower = TRUE),
  oe_disldp = oe_clean(V201161, tolower = TRUE),
  oe_likerp = oe_clean(V201163, tolower = TRUE),
  oe_dislrp = oe_clean(V201165, tolower = TRUE),
  wc = str_count(oe_likedpc, '\\w+') + str_count(oe_disldpc, '\\w+') +
    str_count(oe_likerpc, '\\w+') + str_count(oe_dislrpc, '\\w+') +
    str_count(oe_likedp, '\\w+') + str_count(oe_disldp, '\\w+') +
    str_count(oe_likerp, '\\w+') + str_count(oe_dislrp, '\\w+'),

  ## Miscellaneous
  spanish = as.numeric(V201001 == 2 | V202001 == 2)) %>%
  filter(spanish == 0)
anes2020disc <- discursive(data = anes2020,
                           openends = colnames(anes2020)[grep("oe_", colnames(anes2020))],
                           meta = c("age", "educ_cont", "pid_cont", "educ_pid", "female"),
                           args_textProcessor = list(customstopwords = stopwords),
                           args_prepDocuments = list(lower.thresh = 10),
                           args_stm = list(K = 25, seed = 12345, verbose = FALSE),
                           dictionary = dict_constraint$regex)
anes2020 <- bind_cols(anes2020, anes2020disc$output)


# 2016 ANES ---------------------------------------------------------------

anes2016 <- anes2016raw %>% transmute(

  ## Sociodemographics and other controls
  caseid = V160001,
  mode = V160501 - 1,
  female = na_in(V161342, c(-9,3)) - 1,
  age = na_in(V161267, c(-9,-8)),
  black = as.numeric(na_if(V161310x, -9) == 2),
  educ = as.numeric(na_in(V161270, c(-9,95,90)) >= 13),
  educ_cont = na_in(V161270, c(-9,95,90)),
  pid_cont = (na_in(V161158x, -9:-1) - 4)/3,
  educ_pid = educ_cont * pid_cont,
  faminc = na_in(V161361x, -9:-1)/21,
  relig = (5 - na_in(V161245, -9:-1))/5,
  relig = ifelse(V161244 != 1, 0, relig),
  relig = ifelse(V161245a == 2, 1, relig),

  ## Political engagement
  vote = as.numeric(na_in(V161030, -9:-1) == 1),
  vote = ifelse(V161026==1, 1, vote),
  vote = ifelse(V161024x==1, 0, vote),
  polint = (5 - na_if(V161003, -9))/4,
  effic_int = (na_in(V162217, -9:-1) - 1 - na_in(V162218, -9:-1) + 5)/8,
  effic_ext = (na_in(V162215, -9:-1) - 1 + na_in(V162216, -9:-1) - 1)/8,

  ## Factual knowledge
  polknow = as.numeric(scale((na_if(V161513, -5) == 6) +
                               (na_if(V161514, -5) == 4) +
                               (na_if(V161515, -5) == 2) +
                               (na_if(V161516, -5) == 2))),

  ## Open-ended responses
  oe_likedpc = oe_clean(V161069, tolower = TRUE),
  oe_disldpc = oe_clean(V161072, tolower = TRUE),
  oe_likerpc = oe_clean(V161075, tolower = TRUE),
  oe_dislrpc = oe_clean(V161078, tolower = TRUE),
  oe_likedp = oe_clean(V161098, tolower = TRUE),
  oe_disldp = oe_clean(V161101, tolower = TRUE),
  oe_likerp = oe_clean(V161104, tolower = TRUE),
  oe_dislrp = oe_clean(V161106, tolower = TRUE),
  wc = str_count(oe_likedpc, '\\w+') + str_count(oe_disldpc, '\\w+') +
    str_count(oe_likerpc, '\\w+') + str_count(oe_dislrpc, '\\w+') +
    str_count(oe_likedp, '\\w+') + str_count(oe_disldp, '\\w+') +
    str_count(oe_likerp, '\\w+') + str_count(oe_dislrp, '\\w+'),

  ## Miscellaneous
  polknow_evalpre = (5 - na_in(V168016, -9:-1))/4,
  wordsum = (na_in(V161497, -9:-1) + na_in(V161498, -9:-1)
             + na_in(V161499, -9:-1) + na_in(V161500, -9:-1)
             + na_in(V161501, -9:-1) + na_in(V161502, -9:-1)
             + na_in(V161503, -9:-1) + na_in(V161504, -9:-1)
             + na_in(V161505, -9:-1) + na_in(V161506, -9:-1))/10,
  extraversion = (na_in(V162333, -9:-1) - 1)/6,
  newexperience = (na_in(V162337, -9:-1) - 1)/6,
  reserved = (na_in(V162338, -9:-1) - 1)/6,
  language = detect_language(
    paste(oe_likedpc, oe_disldpc, oe_likerpc, oe_dislrpc,
          oe_likedp, oe_disldp, oe_likerp, oe_dislrp))) %>%
  filter(language == "en" | wc == 0)
anes2016disc <- discursive(data = anes2016,
                           openends = colnames(anes2016)[grep("oe_", colnames(anes2016))],
                           meta = c("age", "educ_cont", "pid_cont", "educ_pid", "female"),
                           args_textProcessor = list(customstopwords = stopwords),
                           args_prepDocuments = list(lower.thresh = 10),
                           args_stm = list(K = 25, seed = 12345, verbose = FALSE),
                           dictionary = dict_constraint$regex)
anes2016 <- bind_cols(anes2016, anes2016disc$output)


# 2012 ANES ---------------------------------------------------------------

anes2012 <- anes2012raw %>% transmute(

  ## Sociodemographics and other controls
  caseid = caseid,
  mode = mode - 1,
  female = gender_respondent_x - 1,
  age = na_in(dem_age_r_x, c(-2,-9,-8)),
  black = as.numeric(na_in(dem_raceeth_x, -9:-1) == 2),
  educ = as.numeric(na_in(dem_edugroup_x, c(-9,-2)) >= 4),
  educ_cont = (na_in(dem_edugroup_x, c(-9,-2)) - 1)/4,
  pid_cont = (na_if(pid_x, -2) - 4)/3,
  educ_pid = educ_cont * pid_cont,
  faminc = (na_in(incgroup_prepost_x, -9:-1) - 1)/27,
  relig = (5 - na_in(relig_churchoft, -9:-1))/5,
  relig = ifelse(relig_church != 1, 0, relig),
  relig = ifelse(relig_churchwk == 2, 1, relig),

  ## Political engagement
  vote = as.numeric(na_if(rvote2012_x, -2) == 1),
  polint = (5 - na_in(interest_attention, -9:-1))/4,
  effic_int = (recode(na_in(as.numeric(effic_complicstd), -9:-8), `-1` = 1) - 1
               + recode(na_in(as.numeric(effic_complicrev), -9:-8), `-1` = 1) - 1
               - recode(na_in(as.numeric(effic_undstd), -9:-8), `-1` = 5) + 5
               - recode(na_in(as.numeric(effic_undrev), -9:-8), `-1` = 5) + 5) / 8,
  effic_ext = (recode(na_in(as.numeric(effic_carestd), -9:-8), `-1` = 1) - 1
               - recode(na_in(as.numeric(effic_carerev), -9:-8), `-1` = 5) + 5
               + recode(na_in(as.numeric(effic_saystd), -9:-8), `-1` = 1) - 1
               - recode(na_in(as.numeric(effic_sayrev), -9:-8), `-1` = 5) + 5) / 8,

  ## Factual knowledge
  polknow = as.numeric(scale((preknow_prestimes==2) + (preknow_sizedef==1)
                             + (preknow_senterm==6) + (preknow_medicare==1)
                             + (preknow_leastsp==1))),

  ## Open-ended responses
  oe_likedpc = oe_clean(candlik_likewhatdpc, tolower = TRUE),
  oe_disldpc = oe_clean(candlik_dislwhatdpc, tolower = TRUE),
  oe_likerpc = oe_clean(candlik_likewhatrpc, tolower = TRUE),
  oe_dislrpc = oe_clean(candlik_dislwhatrpc, tolower = TRUE),
  oe_likedp = oe_clean(ptylik_lwhatdp, tolower = TRUE),
  oe_disldp = oe_clean(ptylik_dwhatdp, tolower = TRUE),
  oe_likerp = oe_clean(ptylik_lwhatrp, tolower = TRUE),
  oe_dislrp = oe_clean(ptylik_dwhatrp, tolower = TRUE),
  wc = str_count(oe_likedpc, '\\w+') + str_count(oe_disldpc, '\\w+') +
    str_count(oe_likerpc, '\\w+') + str_count(oe_dislrpc, '\\w+') +
    str_count(oe_likedp, '\\w+') + str_count(oe_disldp, '\\w+') +
    str_count(oe_likerp, '\\w+') + str_count(oe_dislrp, '\\w+'),

  ## Miscellaneous
  polknow_evalpre = (5 - na_in(iwrobspre_levinfo, -9:-1))/4,
  wordsum = ((wordsum_setb == 5) + (wordsum_setd == 3)
             + (wordsum_sete == 1) + (wordsum_setf == 3)
             + (wordsum_setg == 5) + (wordsum_seth == 4)
             + (wordsum_setj == 1) + (wordsum_setk == 1)
             + (wordsum_setl == 4) + (wordsum_seto == 2))/10,
  extraversion = (na_in(tipi_extra, -9:-1) - 1)/6,
  newexperience = (na_in(tipi_open, -9:-1) - 1)/6,
  reserved = (na_in(tipi_resv, -9:-1) - 1)/6,
  spanish = as.numeric(profile_spanishsurv == 1 |
                         admin_pre_lang_start == 2 |
                         admin_post_lang_start == 2)) %>%
  filter(!spanish)
anes2012disc <- discursive(data = anes2012,
                           openends = colnames(anes2012)[grep("oe_", colnames(anes2012))],
                           meta = c("age", "educ_cont", "pid_cont", "educ_pid", "female"),
                           args_textProcessor = list(customstopwords = stopwords),
                           args_prepDocuments = list(lower.thresh = 10),
                           args_stm = list(K = 25, seed = 12345, verbose = FALSE),
                           dictionary = dict_constraint$regex)
anes2012 <- bind_cols(anes2012, anes2012disc$output)


# 2015 YouGov -------------------------------------------------------------

yg2015 <- yg2015raw %>% transmute(

  ## Sociodemographics and other controls
  caseid = caseid,
  female = gender - 1,
  age = 2015 - birthyr,
  black = as.numeric(race == 2),
  educ_cont = (educ - 1)/5,
  educ = as.numeric(educ >= 5),
  pid_cont = (na_if(pid7, 8) - 4)/3,
  educ_pid = educ_cont * pid_cont,
  faminc = (recode(faminc, `31`=12, `97`=NA_real_) - 1)/15,
  relig = (6 - na_if(pew_churatd, 7))/5,

  ## Factual knowledge
  polknow = as.numeric(scale((Q24==3) + (Q25==1) + (Q26==1) + (Q27==2)
                             + (Q28==2) + (Q29==1) + (Q30==2) + (Q31==1))),

  ## Open-ended responses
  oe_guns1 = oe_clean(Q2),
  oe_guns2 = oe_clean(Q3),
  oe_health1 = oe_clean(Q5),
  oe_health2 = oe_clean(Q6),
  wc = str_count(oe_guns1, '\\w+') + str_count(oe_guns2, '\\w+') +
    str_count(oe_health1, '\\w+') + str_count(oe_health2, '\\w+'),

  ## Miscellaneous
  treat = treat_rand1 == 3 | treat_rand1 == 4,
  know_disease = ((Q12_1==1)
                  + ((Q12_2==1)*!treat) + ((Q12_2==2)*treat)
                  + ((Q12_3==2)*!treat) + ((Q12_3==1)*treat)
                  + ((Q12_4==1)*!treat) + ((Q12_4==2)*treat)
                  + ((Q12_5==2)*!treat) + ((Q12_5==1)*treat)
                  + (Q12_6==2) + (Q12_7==2)
                  + (Q13==1) + (Q14==2)))
yg2015disc <- discursive(data = yg2015,
                         openends = colnames(yg2015)[grep("oe_", colnames(yg2015))],
                         meta = c("age", "educ_cont", "pid_cont", "educ_pid", "female"),
                         args_textProcessor = list(customstopwords = stopwords),
                         args_prepDocuments = list(lower.thresh = 10),
                         args_stm = list(K = 25, seed = 12345, verbose = FALSE),
                         dictionary = dict_constraint$regex)
yg2015 <- bind_cols(yg2015, yg2015disc$output)


# 2008 - 2012 Swiss Surveys -----------------------------------------------

swiss2012 <- swiss2012raw %>% transmute(

  ## Sociodemographics and other controls
  caseid = nummer,
  lang = sprache,
  female = 1 - male,
  age = age,
  educ = as.numeric(edu == 4),
  educ_cont = (edu - 1)/3,
  ideo_cont = (na_in(P04, c(11,12)) - 5)/5,
  educ_ideo = educ_cont * ideo_cont,

  ## Open-ended responses
  oe_string1 = oe_clean(paste(prostring1, constring1), tolower = TRUE, spell = FALSE),
  oe_string2 = oe_clean(paste(prostring2, constring2), tolower = TRUE, spell = FALSE),
  wc = str_count(oe_string1, '\\w+') + str_count(oe_string2, '\\w+'),

  ## Miscellaneous
  loj = lojr,
  loj_scale = as.numeric(scale(loj))
)

swiss2012_de <- filter(swiss2012, lang == 1)
swiss2012disc_de <- discursive(data = swiss2012_de,
                               openends = colnames(swiss2012_de)[grep("oe_", colnames(swiss2012_de))],
                               meta = c("age", "educ_cont", "ideo_cont", "educ_ideo", "female"),
                               args_textProcessor = list(language = "german"),
                               args_prepDocuments = list(lower.thresh = 10),
                               args_stm = list(K = 25, seed = 12345, verbose = FALSE),
                               dictionary = dict_constraint_de$regex)
swiss2012_de <- bind_cols(swiss2012_de, swiss2012disc_de$output)

swiss2012_fr <- filter(swiss2012, lang == 2)
swiss2012disc_fr <- discursive(data = swiss2012_fr,
                               openends = colnames(swiss2012_fr)[grep("oe_", colnames(swiss2012_fr))],
                               meta = c("age", "educ_cont", "ideo_cont", "educ_ideo", "female"),
                               args_textProcessor = list(language = "french"),
                               args_prepDocuments = list(lower.thresh = 10),
                               args_stm = list(K = 25, seed = 12345, verbose = FALSE),
                               dictionary = dict_constraint_fr$regex)
swiss2012_fr <- bind_cols(swiss2012_fr, swiss2012disc_fr$output)

swiss2012_it <- filter(swiss2012, lang == 3)
swiss2012disc_it <- discursive(data = swiss2012_it,
                               openends = colnames(swiss2012_it)[grep("oe_", colnames(swiss2012_it))],
                               meta = c("age", "educ_cont", "ideo_cont", "educ_ideo", "female"),
                               args_textProcessor = list(language = "italian"),
                               args_prepDocuments = list(lower.thresh = 10),
                               args_stm = list(K = 25, seed = 12345, verbose = FALSE),
                               dictionary = dict_constraint_it$regex)
swiss2012_it <- bind_cols(swiss2012_it, swiss2012disc_it$output)


# 2019 MTurk Study --------------------------------------------------------

mturk2019 <- mturk2019raw %>% transmute(

  ## Sociodemographics and other controls
  caseid = row_number(),
  ranid = `Random ID`,
  female = recode(gender, `1`=1, `2`=0, `3`=NA_real_),
  age = age,
  black = race == 2,
  educ_cont = (educ - 1)/5,
  educ = educ > 4,
  pid_cont = (recode(pid, `1`=6, `2`=2, `3`=4, `4`=4) +
                recode(pid_lean, `1`=1, `2`=-1, `3`=0, .missing=0) +
                recode(pid_rep, `1`=1, `2`=0, .missing=0) +
                recode(pid_dem, `1`=-1, `2`=0, .missing=0) - 1)/6,
  educ_pid = educ_cont * pid_cont,
  faminc = (income - 1)/6,
  relig = (church - 1)/8,

  ## Factual knowledge
  polknow = as.numeric(scale((employ == 4) + (sales == 3))),

  ## Open-ended responses
  oe_taxes = oe_clean(taxes_oe),
  oe_jobs = oe_clean(jobs_oe),
  wc = str_count(oe_taxes, '\\w+') + str_count(oe_jobs, '\\w+'),

  ## Miscellaneous
  tv_trust_fox = (5 - tv_trust_1)/4,
  tv_trust_msnbc = (5 - tv_trust_2)/4,
  tv_trust_cnn = (5 - tv_trust_3)/4,
  tv_trust_nbc = (5 - tv_trust_4)/4,
  tv_trust_cbs = (5 - tv_trust_5)/4,
  print_trust_nyt = (5 - print_trust_1)/4,
  print_trust_wapo = (5 - print_trust_2)/4,
  print_trust_wsj = (5 - print_trust_3)/4,
  print_trust_ust = (5 - print_trust_4)/4,
  print_trust_nyp = (5 - print_trust_5)/4
)
mturk2019disc <- discursive(data = mturk2019,
                            openends = colnames(mturk2019)[grep("oe_", colnames(mturk2019))],
                            meta = c("age", "educ_cont", "pid_cont", "educ_pid", "female"),
                            args_textProcessor = list(customstopwords = stopwords),
                            args_prepDocuments = list(lower.thresh = 10),
                            args_stm = list(K = 25, seed = 12345, verbose = FALSE),
                            dictionary = dict_constraint$regex)
mturk2019 <- bind_cols(mturk2019, mturk2019disc$output)


# Export processed data files ---------------------------------------------

save.image(here("data/processed.Rdata"))
