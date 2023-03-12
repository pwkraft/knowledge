# ========================================================================= #
# Project: Women Also Know Stuff (APSR)
# - Script: Load raw data, preprocess text, export for analysis
# - Author: Patrick Kraft (patrickwilli.kraft@uc3m.es)
# ========================================================================= #


# Load raw data, packages, and custom functions ---------------------------

load("data/raw.Rdata")
source("00-func.R")


# 2018 CES ----------------------------------------------------------------

ces2018 <- ces2018raw %>%
  transmute(
    caseid = caseid,
    age = 2018 - birthyr,
    female = gender - 1,
    educ_cont =  educ,
    educ = as.numeric(educ > 4),
    black = as.numeric(race == 2),
    faminc = (na_if(faminc_new, 97)-1)/15,
    relig = (6-na_if(pew_churatd, 7))/5,
    pid = recode(as.numeric(pid7), `8` = 4),
    educ_pid = educ_cont * pid,
    polint = (5 - UWM329)/4,
    vote = recode(as.numeric(CC18_350), `1` = 1, `3` = 1, .default = 0),
    effic_int = (UWM331 - UWM332 + 4)/8,
    effic_ext = (UWM333 + UWM334 - 2)/8,
    know_guns = as.numeric(UWM311 == 2),
    know_abortion = as.numeric(UWM314 == 4),
    know_daca = as.numeric(UWM317 == 4),
    know_health = as.numeric(UWM320 == 3),
    know_trade = as.numeric(UWM323 == 1),
    polknow = as.numeric(scale(know_trade + know_daca + know_health + know_guns + know_abortion)),
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
                          meta = c("age", "educ_cont", "pid", "educ_pid", "female"),
                          args_textProcessor = list(customstopwords = oe_sw),
                          args_prepDocuments = list(lower.thresh = 10),
                          args_stm = list(K = 25, seed = 12345, verbose = FALSE),
                          dictionary = dict_constraint$regex)
ces2018 <- bind_cols(ces2018, ces2018disc$output)

