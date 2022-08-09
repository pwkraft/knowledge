# ========================================================================= #
# Women Also Know Stuff
# - Code includes all analyses for updated paper + presentation
# - Author: Patrick Kraft
# - Date: 01/12/22
# ========================================================================= #



# Preamble ----------------------------------------------------------------

## Packages ----

library(tidyverse)
library(quanteda)
library(nFactors)
library(grid)
library(gridExtra)
library(cowplot)
library(ggridges)
library(GGally)
library(xtable)
library(stm)
library(broom)
library(marginaleffects)
library(stargazer)


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


# Validation: competence and engagement -----------------------------------

dvs <- c("vote", "polint_att", "effic_int", "effic_ext")
ivs <- c("female", "educ", "faminc", "age", "black", "relig")

m1text <- c(
  map(list(data_cces, data2020, data2016, data2012),
      ~glm(reformulate(c("polknow_text_scale", ivs), response = "vote"),
           data = ., subset = !is.na(polknow_factual_scale), family=binomial("logit"))),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(c("polknow_text_scale", ivs), response = "polint_att"),
          data = ., subset = !is.na(polknow_factual_scale))),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(c("polknow_text_scale", ivs), response = "effic_int"),
          data = ., subset = !is.na(polknow_factual_scale))),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(c("polknow_text_scale", ivs), response = "effic_ext"),
          data = ., subset = !is.na(polknow_factual_scale))))

m1factual <- c(
  map(list(data_cces, data2020, data2016, data2012),
      ~glm(reformulate(c("polknow_factual_scale", ivs), response = "vote"),
           data = ., subset = !is.na(polknow_text_scale), family=binomial("logit"))),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(c("polknow_factual_scale", ivs), response = "polint_att"),
          data = ., subset = !is.na(polknow_text_scale))),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(c("polknow_factual_scale", ivs), response = "effic_int"),
          data = ., subset = !is.na(polknow_text_scale))),
  map(list(data_cces, data2020, data2016, data2012),
      ~lm(reformulate(c("polknow_factual_scale", ivs), response = "effic_ext"),
          data = ., subset = !is.na(polknow_text_scale))))

marginaleffects(m1text[[1]], variables = "polknow_text_scale") %>% summary()
comparisons(m1text[[1]], variables = "polknow_text_scale") %>% tidy()
comparisons(m1text[[1]], variables = list(polknow_text_scale = "sd")) %>% tidy()
comparisons(m1text[[1]], variables = list(polknow_text_scale = "2sd")) %>% tidy()
comparisons(m1text[[1]], variables = list(polknow_text_scale = c(-1,0))) %>% tidy()
comparisons(m1text[[1]], variables = list(polknow_text_scale = c(0,1))) %>% tidy()

marginaleffects(m1factual[[1]], variables = "polknow_factual_scale") %>% summary()
comparisons(m1factual[[1]], variables = "polknow_factual_scale") %>% tidy()
comparisons(m1factual[[1]], variables = list(polknow_factual_scale = "sd")) %>% tidy()
comparisons(m1factual[[1]], variables = list(polknow_factual_scale = "2sd")) %>% tidy()
comparisons(m1factual[[1]], variables = list(polknow_factual_scale = c(-1,0))) %>% tidy()
comparisons(m1factual[[1]], variables = list(polknow_factual_scale = c(0,1))) %>% tidy()

c(m1text, m1factual) %>%
  map_dfr(~summary(marginaleffects(.)), .id = "model") %>%
  as_tibble() %>%
  filter(term %in% c("polknow_text_scale", "polknow_factual_scale")) %>%
  mutate(
    study = factor(rep(c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES"), 8),
                   levels = c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES")),
    dv = recode_factor(rep(rep(dvs, each = 4), 2),
                       `vote` = "Turnout",
                       `polint_att` = "Political Interest",
                       `effic_int` = "Internal Efficacy",
                       `effic_ext` = "External Efficacy"),
    term = recode_factor(term,
                         `polknow_factual_scale` = "Factual\nKnowledge",
                         `polknow_text_scale` = "Discursive\nSophistication")) %>%
  ggplot(aes(y=term, x=estimate, xmin=conf.low, xmax=conf.high)) +
  geom_vline(xintercept = 0, color="grey") +
  geom_point() + geom_errorbarh(height=0) + facet_grid(study~dv) +
  xlab("Average Marginal Effect") + ylab("Independent Variable") + plot_default



# Validation: interaction b/w measures ------------------------------------

dvs <- c("vote", "polint_att", "effic_int", "effic_ext")
ivs <- c("female", "educ", "faminc", "age", "black", "relig")

m1interaction <- map(list(data_cces, data2020, data2016, data2012),
                     ~glm(vote ~ polknow_text_scale * polknow_factual_scale +
                            female + educ + faminc + age + black + relig,
                          data = ., family=binomial("logit")))
m1interaction %>% map(summary)

summary(glm(vote ~ polknow_text_scale * polknow_factual2_scale +
              female + educ + faminc + age + black + relig,
            data = data_cces, family=binomial("logit")))

summary(glm(vote ~ polknow_text_scale * scale(know5) +
              female + educ + faminc + age + black + relig,
            data = data_cces, family=binomial("logit")))

summary(glm(vote ~ polknow_text_scale + scale(know5) +
              female + educ + faminc + age + black + relig,
            data = data_cces, family=binomial("logit")))

summary(glm(vote ~ polknow_text_scale +
              female + educ + faminc + age + black + relig,
            data = data_cces, family=binomial("logit")))

summary(glm(vote ~ scale(know5) +
              female + educ + faminc + age + black + relig,
            data = data_cces, family=binomial("logit")))

map(list(data_cces, data2020, data2016, data2012),
    ~glm(vote ~ polknow_text_scale * polknow_factual_scale,
         data = ., family=binomial("logit"))) %>%
  map(summary)


# Test gender differences with alternative index --------------------------

data_cces %>%
  select(polknow_text_scale, know5, female) %>%
  mutate(know5 = as.numeric(scale(know5))) %>%
  pivot_longer(-female) %>%
  mutate(Gender = factor(female, labels = c("Male","Female")),
         Variable = recode_factor(name,
                                  `polknow_text_scale` = "Discursive\nSophistication",
                                  `polknow_factual_scale` = "Factual\nKnowledge")) %>%
  na.omit() %>%
  ggplot(aes(y=value, x=Gender, fill=Variable)) + plot_default +
  geom_violin(trim=TRUE) +
  stat_summary(fun.data=data_summary, geom="pointrange", shape=23, fill="white", size = .4) +
  facet_wrap(~Variable) + ylab("Average Values") + xlab(NULL) +
  guides(fill="none") + scale_fill_brewer(palette="Paired") +
  ggtitle("2018 CES")

summary(lm(polknow_text_scale ~ female + educ + faminc + age + black + relig, data = data_cces))
summary(lm(know5 ~ female + educ + faminc + age + black + relig, data = data_cces))

summary(lm(polknow_text_scale ~ female, data = data_cces))
summary(lm(know5 ~ female, data = data_cces))

m3text <- list(data_cces, data2020, data2016, data2012, data_yg) %>%
  map(~lm(reformulate(ivs, response = "polknow_text_scale"),
          data = ., subset = !is.na(polknow_factual_scale)))
m3factual <- list(data_cces, data2020, data2016, data2012, data_yg) %>%
  map(~lm(reformulate(ivs, response = "polknow_factual_scale"),
          data = ., subset = !is.na(polknow_text_scale)))
