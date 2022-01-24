# ========================================================================= #
# Women Also Know Stuff
# - Code includes additional analyses for the online appendix
# - Author: Patrick Kraft
# - Date: 01/20/22
# ========================================================================= #



# Preamble ----------------------------------------------------------------

## Packages ----

library(tidyverse)
library(quanteda)
library(nFactors)
library(gridExtra)
library(ggridges)
library(GGally)
library(xtable)
library(stm)
library(broom)
library(marginaleffects)
library(stargazer)
library(preText)


## Load data ----

rm(list = ls())
load(here::here("calc/out/anes2012.Rdata"))
load(here::here("calc/out/anes2016.Rdata"))
load(here::here("calc/out/anes2020.Rdata"))
load(here::here("calc/out/swiss.Rdata"))
load(here::here("calc/out/yougov.Rdata"))
load(here::here("calc/out/cces.Rdata"))
source("calc/func.R")


## Plot defaults ----
plot_default <- theme_classic(base_size=9) +
  theme(panel.border = element_rect(fill=NA))
plot_empty <- theme_classic(base_size=9) +
  theme(panel.border = element_rect(fill="white"))



# Word count plot ---------------------------------------------------------

wc_mean <- tibble(
  study = factor(1:6, labels = c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES",
                                 "2015 YouGov", "Swiss Survey")),
  wc = c(mean(data_cces$wc), mean(data2020$wc), mean(data2016$wc), mean(data2012$wc),
         mean(data_yg$wc), mean(c(opend_french$wc, opend_german$wc, opend_italian$wc)))
)

bind_rows(
  transmute(data_cces, wc = wc, study = 1),
  transmute(data2020, wc = wc, study = 2),
  transmute(data2016, wc = wc, study = 3),
  transmute(data2012, wc = wc, study = 4),
  transmute(data_yg, wc = wc, study = 5),
  transmute(opend_french, wc = wc, study = 6),
  transmute(opend_german, wc = wc, study = 6),
  transmute(opend_italian, wc = wc, study = 6)
) %>% mutate(
  study = factor(study,
                 labels = c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES",
                            "2015 YouGov", "Swiss Survey"))) %>%
  ggplot(aes(wc)) + geom_histogram(fill = "grey") + theme_classic(base_size = 8) +
  theme(panel.border = element_rect(fill=NA)) +
  facet_wrap(.~study, ncol = 2, scales = "free") +
  geom_vline(aes(xintercept = wc), colour="red", linetype = "longdash",data = wc_mean) +
  ylab("Number of Respondents") + xlab("Word Count")
ggsave("fig/wc.png", width = 6, height = 5)



# Plot non-response by gender ---------------------------------------------

bind_rows(
  transmute(cces, noresp = wc == 0, female = female, study = 1),
  transmute(anes2020, noresp = wc == 0, female = female, study = 2),
  transmute(anes2016, noresp = wc == 0, female = female, study = 3),
  transmute(anes2012, noresp = wc == 0, female = female, study = 4),
  transmute(yougov, noresp = wc == 0, female = female, study = 5),
  transmute(swiss, noresp = wc == 0, female = female, study = 6)
) %>% mutate(
  study = factor(study,
                 labels = c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES",
                            "2015 YouGov", "Swiss Survey")),
  Gender = factor(female, labels = c("Male","Female"))) %>%
  na.omit() %>%
  group_by(study, Gender) %>%
  summarize(avg = mean(noresp),
            sd = sd(noresp),
            n = n(),
            cilo = avg - 1.96*sd/sqrt(n),
            cihi = avg + 1.96*sd/sqrt(n)) %>%
  ggplot(aes(y=avg, x=Gender, ymin=cilo, ymax=cihi)) + plot_default +
  geom_bar(stat="identity", fill="grey") + geom_errorbar(width=.1) +
  facet_wrap(~study, ncol = 2) + ylab("Average Values") + xlab(NULL) +
  guides(fill="none") + scale_fill_brewer(palette="Paired")
ggsave("fig/noresponse.png", width = 6, height = 5)

transmute(cces, noresp = wc == 0, female = female) %>%
  t.test(noresp ~ female, data = .)



# STM topic proportions ---------------------------------------------------

pdf("fig/stm_prop.pdf", width=12, height=10)
par(mfrow = c(2,4), mar=c(4.2,0.5,2.5,0.5))
plot(stm_fit_cces, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("2018 CES (k = ", stm_fit_cces$settings$dim$K,")", collapse = ""))
plot(stm_fit2020, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("2020 ANES (k = ", stm_fit2020$settings$dim$K,")", collapse = ""))
plot(stm_fit2016, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("2016 ANES (k = ", stm_fit2016$settings$dim$K,")", collapse = ""))
plot(stm_fit2012, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("2012 ANES (k = ", stm_fit2012$settings$dim$K,")", collapse = ""))
plot(stm_fit_yg, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("2015 YouGov (k = ", stm_fit_yg$settings$dim$K,")", collapse = ""))
plot(stm_fit_french, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("Swiss Survey - French (k = ", stm_fit_french$settings$dim$K,")", collapse = ""))
plot(stm_fit_german, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("Swiss Survey - German (k = ", stm_fit_german$settings$dim$K,")", collapse = ""))
plot(stm_fit_italian, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("Swiss Survey - Italian (k = ", stm_fit_italian$settings$dim$K,")", collapse = ""))
dev.off()



# Discursive sophistication components ------------------------------------

## 2018 CES
data_cces %>% transmute(
  v1 = size,
  v2 = range,
  v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")
  ) + plot_default
ggsave("fig/cces2018_components.png", width = 2.6, height = 2.6)

## 2020 ANES
data2020 %>% transmute(
  v1 = size,
  v2 = range,
  v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")
  ) + plot_default
ggsave("fig/anes2020_components.png", width = 2.6, height = 2.6)

## 2016 ANES
data2016 %>% transmute(
  v1 = size,
  v2 = range,
  v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")
  ) + plot_default
ggsave("fig/anes2016_components.png", width = 2.6, height = 2.6)

## 2012 ANES
data2012 %>% transmute(
  v1 = size,
  v2 = range,
  v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")
  ) + plot_default
ggsave("fig/anes2012_components.png", width = 2.6, height = 2.6)

## 2015 YouGov
data_yg %>% transmute(
  v1 = size,
  v2 = range,
  v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")
  ) + plot_default
ggsave("fig/yg_components.png", width = 2.6, height = 2.6)

## Swiss - French
opend_french %>% transmute(
  v1 = size,
  v2 = range,
  v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")
  ) + plot_default
ggsave("fig/french_components.png", width = 2.6, height = 2.6)

## Swiss - German
opend_german %>% transmute(
  v1 = size,
  v2 = range,
  v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")
  ) + plot_default
ggsave("fig/german_components.png", width = 2.6, height = 2.6)

## Swiss - Italian
opend_italian %>% transmute(
  v1 = size,
  v2 = range,
  v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")
  ) + plot_default
ggsave("fig/italian_components.png", width = 2.6, height = 2.6)



# PreText analysis --------------------------------------------------------

## select raw documents
res <- list(
  cces2018 = apply(opend_cces[opend_cces$caseid %in% data_cces$caseid[1:500], -1],
                   1, paste, collapse = " "),
  anes2020 = apply(anes2020spell[anes2020spell$caseid %in% data2020$caseid[1:500], -1],
                   1, paste, collapse = " "),
  anes2016 = apply(anes2016spell[anes2016spell$caseid %in% data2016$caseid[1:500], -1],
                   1, paste, collapse = " "),
  anes2012 = apply(anes2012spell[anes2012spell$caseid %in% data2012$caseid[1:500], -1],
                   1, paste, collapse = " "),
  yougov = apply(opend_yg[opend_yg$caseid %in% data_yg$caseid[1:500], -1],
                 1, paste, collapse = " "),
  french = apply(cbind(opend_french$string1[1:500],opend_french$string2[1:500]),
                 1, paste, collapse=' '),
  german = apply(cbind(opend_german$string1[1:500],opend_german$string2[1:500]),
                 1, paste, collapse=' '),
  italian = apply(cbind(opend_italian$string1[1:500],opend_italian$string2[1:500]),
                  1, paste, collapse=' ')
) %>%
  map(factorial_preprocessing, use_ngrams = FALSE, parallel = TRUE, cores = 12) %>%
  map(preText, parallel = TRUE, cores = 12)

## generate preText score plot
res %>% map(preText_score_plot)

## plot regression results
extractData <- function(x){
  out <- x[[2]]
  out$xmean <- x[[3]]$x
  out$ylab <- factor(out$y, labels = c("Lowercase","Remove Infrequent Terms","Remove Numbers",
                                       "Remove Punctuation","Remove Stopwords","Stemming"))
  out
}

res %>%
  map(regression_coefficient_plot, remove_intercept = TRUE) %>%
  map_dfr("data", .id = "study") %>% #map_dfr(extractData) %>%
  mutate(study = factor(study, levels = c("cces2018", "anes2020", "anes2016", "anes2012",
                                          "yougov", "french","german","italian"),
                        labels = c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES","2015 YouGov",
                                   "Swiss (French)","Swiss (German)","Swiss (Italian)"))) %>%
  ggplot(aes(x = Coefficient, xmin = Coefficient-2*SE, xmax = Coefficient+2*SE, y = Variable)) +
  geom_point() + geom_errorbarh(height=0) + geom_vline(xintercept = 0) +
  facet_wrap(~study, ncol=2) + labs(y=NULL, x = "Regression Coefficient") + plot_default
ggsave("fig/pretext.png",width = 6, height = 4.5)



# Controlling for Extraversion and Verbal Skills --------------------------

dvs <- c("vote", "polint_att", "effic_int", "effic_ext")
ivs <- c("female", "educ", "faminc", "age", "black", "relig")
ivs_rob <- c("extraversion", "mode", "wordsum", "wc")

m1text <- c(
  map(ivs_rob,
      ~glm(reformulate(c("polknow_text_mean", ivs, .), response = "vote"),
           data = data2016, subset = !is.na(polknow_factual), family=binomial("logit"))),
  map(ivs_rob,
      ~lm(reformulate(c("polknow_text_mean", ivs, .), response = "polint_att"),
          data = data2016, subset = !is.na(polknow_factual))),
  map(ivs_rob,
      ~lm(reformulate(c("polknow_text_mean", ivs, .), response = "effic_int"),
          data = data2016, subset = !is.na(polknow_factual))),
  map(ivs_rob,
      ~lm(reformulate(c("polknow_text_mean", ivs, .), response = "effic_ext"),
          data = data2016, subset = !is.na(polknow_factual))),

  map(ivs_rob,
      ~glm(reformulate(c("polknow_text_mean", ivs, .), response = "vote"),
           data = data2012, subset = !is.na(polknow_factual), family=binomial("logit"))),
  map(ivs_rob,
      ~lm(reformulate(c("polknow_text_mean", ivs, .), response = "polint_att"),
          data = data2012, subset = !is.na(polknow_factual))),
  map(ivs_rob,
      ~lm(reformulate(c("polknow_text_mean", ivs, .), response = "effic_int"),
          data = data2012, subset = !is.na(polknow_factual))),
  map(ivs_rob,
      ~lm(reformulate(c("polknow_text_mean", ivs, .), response = "effic_ext"),
          data = data2012, subset = !is.na(polknow_factual)))
  )

m1factual <- c(
  map(ivs_rob,
      ~glm(reformulate(c("polknow_factual", ivs, .), response = "vote"),
           data = data2016, subset = !is.na(polknow_factual), family=binomial("logit"))),
  map(ivs_rob,
      ~lm(reformulate(c("polknow_factual", ivs, .), response = "polint_att"),
          data = data2016, subset = !is.na(polknow_factual))),
  map(ivs_rob,
      ~lm(reformulate(c("polknow_factual", ivs, .), response = "effic_int"),
          data = data2016, subset = !is.na(polknow_factual))),
  map(ivs_rob,
      ~lm(reformulate(c("polknow_factual", ivs, .), response = "effic_ext"),
          data = data2016, subset = !is.na(polknow_factual))),

  map(ivs_rob,
      ~glm(reformulate(c("polknow_factual", ivs, .), response = "vote"),
           data = data2012, subset = !is.na(polknow_factual), family=binomial("logit"))),
  map(ivs_rob,
      ~lm(reformulate(c("polknow_factual", ivs, .), response = "polint_att"),
          data = data2012, subset = !is.na(polknow_factual))),
  map(ivs_rob,
      ~lm(reformulate(c("polknow_factual", ivs, .), response = "effic_int"),
          data = data2012, subset = !is.na(polknow_factual))),
  map(ivs_rob,
      ~lm(reformulate(c("polknow_factual", ivs, .), response = "effic_ext"),
          data = data2012, subset = !is.na(polknow_factual)))
)

c(m1text, m1factual) %>%
  map_dfr(~summary(marginaleffects(.)), .id = "model") %>%
  as_tibble() %>%
  filter(term %in% c("polknow_text_mean", "polknow_factual")) %>%
  mutate(
    study = factor(rep(rep(c("2016 ANES", "2012 ANES"), each = 16), 2),
                   levels = c("2016 ANES", "2012 ANES")),
    dv = recode_factor(rep(rep(dvs, each = 4), 4),
                       `vote` = "Turnout",
                       `polint_att` = "Political Interest",
                       `effic_iznt` = "Internal Efficacy",
                       `effic_ext` = "External Efficacy"),
    Control = factor(rep(1:4, 16),
                     labels = c("Extraversion",
                                "Survey mode",
                                "Wordsum score",
                                "Response length")),
    term = recode_factor(term,
                         `polknow_factual` = "Factual\nKnowledge",
                         `polknow_text_mean` = "Discursive\nSophistication")) %>%
  ggplot(aes(y=term, x=estimate, xmin=conf.low, xmax=conf.high,
             col = Control, shape = Control)) +
  geom_vline(xintercept = 0, color="grey") +
  geom_point(position = position_dodge(width = -.5)) +
  geom_errorbarh(height=0, position = position_dodge(width = -.5)) +
  facet_grid(study~dv) +
  labs(x = "Average Marginal Effect", y = "Independent Variable") +
  plot_default + theme(legend.position = "bottom")
ggsave("fig/knoweff_robust.pdf", width=6.5, height=3.5)

