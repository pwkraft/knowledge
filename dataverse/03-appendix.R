# ========================================================================= #
# Project: Women Also Know Stuff (APSR)
# - Script: Additional robustness checks reported in the Appendix
# - Author: Patrick Kraft (patrickwilli.kraft@uc3m.es)
# ========================================================================= #


# Load raw data, packages, and custom functions ---------------------------

source(here::here("00-func.R"))
load(here("data/processed.Rdata"))


# Figure B.1: Total word count across all open-ended responses ------------

## Compute average word count
wc_mean <- tibble(
  study = factor(1:6, labels = c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES",
                                 "2015 YouGov", "Swiss Survey")),
  wc = c(mean(ces2018$wc), mean(anes2020$wc), mean(anes2016$wc), mean(anes2012$wc),
         mean(yg2015$wc), mean(c(swiss2012_fr$wc, swiss2012_de$wc, swiss2012_it$wc)))
)

## Create figure
bind_rows(
  transmute(anes2020, wc = wc, study = 1),
  transmute(anes2016, wc = wc, study = 2),
  transmute(anes2012, wc = wc, study = 3),
  transmute(ces2018, wc = wc, study = 4),
  transmute(yg2015, wc = wc, study = 5),
  transmute(swiss2012_fr, wc = wc, study = 6),
  transmute(swiss2012_de, wc = wc, study = 6),
  transmute(swiss2012_it, wc = wc, study = 6)
) %>% mutate(
  study = factor(study,
                 labels = c("2020 ANES", "2016 ANES", "2012 ANES",
                            "2018 CES", "2015 YouGov", "Swiss Survey"))) %>%
  ggplot(aes(wc)) + geom_histogram(fill = "grey") + theme_classic(base_size = 8) +
  theme(panel.border = element_rect(fill=NA)) +
  facet_wrap(.~study, ncol = 3, scales = "free") +
  geom_vline(aes(xintercept = wc), colour="red", linetype = "longdash",data = wc_mean) +
  ylab("Number of Respondents") + xlab("Word Count")
ggsave(here("out/app-wc.png"), width = 6, height = 2.5)



# Plot non-response by gender ---------------------------------------------

bind_rows(
  transmute(anes2020, noresp = wc == 0, female = female, study = 1),
  transmute(anes2016, noresp = wc == 0, female = female, study = 2),
  transmute(anes2012, noresp = wc == 0, female = female, study = 3),
  transmute(cces, noresp = wc == 0, female = female, study = 4),
  transmute(yougov, noresp = wc == 0, female = female, study = 5),
  transmute(swiss, noresp = wc == 0, female = female, study = 6)
) %>% mutate(
  study = factor(study,
                 labels = c("2020 ANES", "2016 ANES", "2012 ANES",
                            "2018 CES", "2015 YouGov", "Swiss Survey")),
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
  facet_wrap(~study, ncol = 3) + ylab("Average Values") + xlab(NULL) +
  guides(fill="none") + scale_fill_brewer(palette="Paired")
ggsave("out/app-noresponse.png", width = 6, height = 2.5)

transmute(cces, noresp = wc == 0, female = female) %>%
  t.test(noresp ~ female, data = .)



# STM topic proportions ---------------------------------------------------

pdf("out/app-stm_prop.pdf", width=12, height=10)
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

pdf("out/app-stm_prop_pres.pdf", width=16, height=8)
par(mfrow = c(1,2), mar=c(4.2,0.5,2.5,0.5))
plot(stm_fit_cces, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("2018 CES (k = ", stm_fit_cces$settings$dim$K,")", collapse = ""))
plot(stm_fit2020, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("2020 ANES (k = ", stm_fit2020$settings$dim$K,")", collapse = ""))
dev.off()



# Discursive sophistication components ------------------------------------

## 2018 CES
ces2018 %>% transmute(
  v1 = size,
  v2 = range,
  v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")
  ) + plot_default
ggsave("out/app-cces2018_components.png", width = 2.6, height = 2.6)

ces2018 %>% transmute(
  v1 = size,
  v2 = range,
  v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")
  ) + plot_empty
ggsave("out/app-cces2018_components0.png", width = 2.6, height = 2.6)

## 2020 ANES
anes2020 %>% transmute(
  v1 = size,
  v2 = range,
  v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")
  ) + plot_default
ggsave("out/app-anes2020_components.png", width = 2.6, height = 2.6)

## 2016 ANES
anes2016 %>% transmute(
  v1 = size,
  v2 = range,
  v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")
  ) + plot_default
ggsave("out/app-anes2016_components.png", width = 2.6, height = 2.6)

## 2012 ANES
anes2012 %>% transmute(
  v1 = size,
  v2 = range,
  v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")
  ) + plot_default
ggsave("out/app-anes2012_components.png", width = 2.6, height = 2.6)

## 2015 YouGov
yg2015 %>% transmute(
  v1 = size,
  v2 = range,
  v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")
  ) + plot_default
ggsave("out/app-yg_components.png", width = 2.6, height = 2.6)

## Swiss - French
swiss2012_fr %>% transmute(
  v1 = size,
  v2 = range,
  v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")
  ) + plot_default
ggsave("out/app-french_components.png", width = 2.6, height = 2.6)

## Swiss - German
swiss2012_de %>% transmute(
  v1 = size,
  v2 = range,
  v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")
  ) + plot_default
ggsave("out/app-german_components.png", width = 2.6, height = 2.6)

## Swiss - Italian
swiss2012_it %>% transmute(
  v1 = size,
  v2 = range,
  v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")
  ) + plot_default
ggsave("out/app-italian_components.png", width = 2.6, height = 2.6)



# PreText analysis --------------------------------------------------------

## select raw documents
res <- list(
  cces2018 = apply(opend_cces[opend_cces$caseid %in% ces2018$caseid[1:500], -1],
                   1, paste, collapse = " "),
  anes2020 = apply(anes2020spell[anes2020spell$caseid %in% anes2020$caseid[1:500], -1],
                   1, paste, collapse = " "),
  anes2016 = apply(anes2016spell[anes2016spell$caseid %in% anes2016$caseid[1:500], -1],
                   1, paste, collapse = " "),
  anes2012 = apply(anes2012spell[anes2012spell$caseid %in% anes2012$caseid[1:500], -1],
                   1, paste, collapse = " "),
  yougov = apply(opend_yg[opend_yg$caseid %in% yg2015$caseid[1:500], -1],
                 1, paste, collapse = " "),
  french = apply(cbind(swiss2012_fr$string1[1:500],swiss2012_fr$string2[1:500]),
                 1, paste, collapse=' '),
  german = apply(cbind(swiss2012_de$string1[1:500],swiss2012_de$string2[1:500]),
                 1, paste, collapse=' '),
  italian = apply(cbind(swiss2012_it$string1[1:500],swiss2012_it$string2[1:500]),
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
  map_dfr("data", .id = "study") %>%
  mutate(study = factor(study, levels = c("cces2018", "anes2020", "anes2016", "anes2012",
                                          "yougov", "french","german","italian"),
                        labels = c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES","2015 YouGov",
                                   "Swiss (French)","Swiss (German)","Swiss (Italian)"))) %>%
  ggplot(aes(x = Coefficient, xmin = Coefficient-2*SE, xmax = Coefficient+2*SE, y = Variable)) +
  geom_point() + geom_errorbarh(height=0) + geom_vline(xintercept = 0) +
  facet_wrap(~study, ncol=2) + labs(y=NULL, x = "Regression Coefficient") + plot_default
ggsave("out/app-pretext.png",width = 6, height = 4.5)



# Discursive sophistication for varying model specifications --------------

## compute alternative measures (save intermediate steps)
plot_df <- bind_rows(
  robustSoph(ces2018, 35, stm_fit_cces$settings$dim$K, "2018 CES"),
  robustSoph(anes2020, 35, stm_fit2020$settings$dim$K, "2020 ANES"),
  robustSoph(anes2016, 35, stm_fit2016$settings$dim$K, "2016 ANES"),
  robustSoph(anes2012, 35, stm_fit2012$settings$dim$K, "2012 ANES"),
  robustSoph(yg2015, 35, stm_fit_yg$settings$dim$K, "2015 YouGov"),
  robustSoph(swiss2012_fr, 35, stm_fit_french$settings$dim$K, "Swiss (French)",
             lang = "french", meta = c("age", "edu", "ideol", "edu_ideol", "female")),
  robustSoph(swiss2012_de, 35, stm_fit_german$settings$dim$K, "Swiss (German)",
             lang = "german", meta = c("age", "edu", "ideol", "edu_ideol", "female")),
  robustSoph(swiss2012_it, 35, stm_fit_italian$settings$dim$K, "Swiss (Italian)",
             lang = "italian", meta = c("age", "edu", "ideol", "edu_ideol", "female"))
)
save(plot_df, file = "calc/tmp/tmp01.Rdata")

plot_df <- plot_df %>% bind_rows(
  robustSoph(ces2018, 25, stm_fit_cces$settings$dim$K, "2018 CES", stem = FALSE),
  robustSoph(anes2020, 25, stm_fit2020$settings$dim$K, "2020 ANES", stem = FALSE),
  robustSoph(anes2016, 25, stm_fit2016$settings$dim$K, "2016 ANES", stem = FALSE),
  robustSoph(anes2012, 25, stm_fit2012$settings$dim$K, "2012 ANES", stem = FALSE),
  robustSoph(yg2015, 25, stm_fit_yg$settings$dim$K, "2015 YouGov", stem = FALSE),
  robustSoph(swiss2012_fr, 25, stm_fit_french$settings$dim$K, "Swiss (French)", stem = FALSE,
             lang = "french", meta = c("age", "edu", "ideol", "edu_ideol", "female")),
  robustSoph(swiss2012_de, 25, stm_fit_german$settings$dim$K, "Swiss (German)", stem = FALSE,
             lang = "german", meta = c("age", "edu", "ideol", "edu_ideol", "female")),
  robustSoph(swiss2012_it, 25, stm_fit_italian$settings$dim$K, "Swiss (Italian)", stem = FALSE,
             lang = "italian", meta = c("age", "edu", "ideol", "edu_ideol", "female"))
)
save(plot_df, file = "calc/tmp/tmp02.Rdata")

plot_df <- plot_df %>% bind_rows(
  robustSoph(ces2018, 25, stm_fit_cces$settings$dim$K, "2018 CES", removestopwords = FALSE),
  robustSoph(anes2020, 25, stm_fit2020$settings$dim$K, "2020 ANES", removestopwords = FALSE),
  robustSoph(anes2016, 25, stm_fit2016$settings$dim$K, "2016 ANES", removestopwords = FALSE),
  robustSoph(anes2012, 25, stm_fit2012$settings$dim$K, "2012 ANES", removestopwords = FALSE),
  robustSoph(yg2015, 25, stm_fit_yg$settings$dim$K, "2015 YouGov", removestopwords = FALSE),
  robustSoph(swiss2012_fr, 25, stm_fit_french$settings$dim$K, "Swiss (French)", removestopwords = FALSE,
             lang = "french", meta = c("age", "edu", "ideol", "edu_ideol", "female")),
  robustSoph(swiss2012_de, 25, stm_fit_german$settings$dim$K, "Swiss (German)", removestopwords = FALSE,
             lang = "german", meta = c("age", "edu", "ideol", "edu_ideol", "female")),
  robustSoph(swiss2012_it, 25, stm_fit_italian$settings$dim$K, "Swiss (Italian)", removestopwords = FALSE,
             lang = "italian", meta = c("age", "edu", "ideol", "edu_ideol", "female"))
)
save(plot_df, file = "calc/tmp/tmp03.Rdata")

## prepare data for plotting
plot_df <- plot_df %>%
  mutate(
    datalab = factor(datalab,
                     levels = c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES", "2015 YouGov",
                                "Swiss (French)", "Swiss (German)", "Swiss (Italian)")),
    condition = factor(100*k + 10*stem + 1*removestopwords, levels = c("3511","2501","2510"),
                       labels = c("More topics (k = 35)", "No stemming", "Keep stopwords"))
  )

## compute correlations for subgroups
plot_cor <- plot_df %>%
  group_by(datalab, condition) %>%
  summarize(cor = paste0("r = ",round(cor(polknow_text_mean, polknow_text_rep), 3))) %>%
  mutate(polknow_text_mean = .9, polknow_text_rep = .1)

## generate plot
ggplot(plot_df, aes(y=polknow_text_mean, x=polknow_text_rep)) +
  geom_point(alpha=.05) + geom_smooth(method="lm") +
  facet_grid(datalab~condition) +
  geom_text(data=plot_cor, aes(label=cor), size=2) + xlim(0,1) + ylim(0,1) +
  labs(y = "Discursive Sophistication (Preferred Specification)",
       x = "Discursive Sophistication (Alternative Specifications)") +
  plot_default
ggsave("out/app-pretext_robustness.png", width=5, height=8.5)



# PreText Robustness for presentation -------------------------------------

## compute alternative measures (save intermediate steps)
plot_df <- bind_rows(
  robustSoph(ces2018, 35, stm_fit_cces$settings$dim$K, "2018 CES"),
  robustSoph(anes2020, 35, stm_fit2020$settings$dim$K, "2020 ANES"),
  robustSoph(ces2018, 25, stm_fit_cces$settings$dim$K, "2018 CES", stem = FALSE),
  robustSoph(anes2020, 25, stm_fit2020$settings$dim$K, "2020 ANES", stem = FALSE),
  robustSoph(ces2018, 25, stm_fit_cces$settings$dim$K, "2018 CES", removestopwords = FALSE),
  robustSoph(anes2020, 25, stm_fit2020$settings$dim$K, "2020 ANES", removestopwords = FALSE),
)

## prepare data for plotting
plot_df <- plot_df %>%
  mutate(
    datalab = factor(datalab,
                     levels = c("2018 CES", "2020 ANES")),
    condition = factor(100*k + 10*stem + 1*removestopwords, levels = c("3511","2501","2510"),
                       labels = c("More topics (k = 35)", "No stemming", "Keep stopwords"))
  )

## compute correlations for subgroups
plot_cor <- plot_df %>%
  group_by(datalab, condition) %>%
  summarize(cor = paste0("r = ",round(cor(polknow_text_mean, polknow_text_rep), 3))) %>%
  mutate(polknow_text_mean = .9, polknow_text_rep = .1)

## generate plot
ggplot(plot_df, aes(y=polknow_text_mean, x=polknow_text_rep)) +
  geom_point(alpha=.05) + geom_smooth(method="lm") +
  facet_grid(datalab~condition) +
  geom_text(data=plot_cor, aes(label=cor), size=2) + xlim(0,1) + ylim(0,1) +
  labs(y = "Discursive Sophistication (Preferred Specification)",
       x = "Discursive Sophistication (Alternative Specifications)") +
  plot_default
ggsave("out/app-pretext_robustness_pres.png", width=5, height=3)



# Controlling for Personality and Verbal Skills --------------------------

dvs <- c("vote", "polint_att", "effic_int", "effic_ext")
ivs <- c("polknow_text_scale", "polknow_factual_scale",
         "female", "age", "black", "educ", "faminc", "relig")
ivs_rob <- c("extraversion + newexperience + reserved", "wordsum", "wc", "mode")

m1cont <- c(
  map(ivs_rob,
      ~glm(reformulate(c(ivs, .), response = "vote"), data = anes2016, family=binomial("logit"))),
  map(ivs_rob,
      ~lm(reformulate(c(ivs, .), response = "polint_att"), data = anes2016)),
  map(ivs_rob,
      ~lm(reformulate(c(ivs, .), response = "effic_int"), data = anes2016)),
  map(ivs_rob,
      ~lm(reformulate(c(ivs, .), response = "effic_ext"), data = anes2016)),

  map(ivs_rob,
      ~glm(reformulate(c(ivs, .), response = "vote"), data = anes2012, family=binomial("logit"))),
  map(ivs_rob,
      ~lm(reformulate(c(ivs, .), response = "polint_att"), data = anes2012)),
  map(ivs_rob,
      ~lm(reformulate(c(ivs, .), response = "effic_int"), data = anes2012)),
  map(ivs_rob,
      ~lm(reformulate(c(ivs, .), response = "effic_ext"), data = anes2012))
)

m1cont %>%
  map_dfr(~tidy(comparisons(
    ., variables = list(polknow_text_scale = c(-1,1),
                        polknow_factual_scale = c(-1,1)))),
    .id = "model") %>%
  as_tibble() %>%
  mutate(
    study = factor(rep(c("2016 ANES", "2012 ANES"), each = 32),
                   levels = c("2016 ANES", "2012 ANES")),
    dv = recode_factor(rep(rep(dvs, each = 8), 2),
                       `vote` = "Turnout",
                       `polint_att` = "Political Interest",
                       `effic_int` = "Internal Efficacy",
                       `effic_ext` = "External Efficacy"),
    Control = factor(rep(rep(1:4, each = 2), 8),
                     labels = c("Personality",
                                "Wordsum score",
                                "Response length",
                                "Survey mode")),
    term = recode_factor(term,
                         `polknow_factual_scale` = "Factual\nKnowledge",
                         `polknow_text_scale` = "Discursive\nSophistication")) %>%
  ggplot(aes(y=term, x=estimate, xmin=conf.low, xmax=conf.high,
             col = Control, shape = Control)) +
  geom_vline(xintercept = 0, color="grey") +
  geom_point(position = position_dodge(width = -.5)) +
  geom_errorbarh(height=0, position = position_dodge(width = -.5)) +
  facet_grid(study~dv) +
  labs(x = "Estimated Effect of Discursive Sophistication and Factual Knowledge\n(for an increase from 1 SD below mean to 1 SD above mean)", y = "Independent Variable") +
  plot_default + theme(legend.position = "bottom")
ggsave("out/app-knoweff_robust.pdf", width=6.5, height=3.5)



# Compare political OE to other OEs ---------------------------------------

# TODO: CONTINUE HERE


# Check selection models for CCES 2018 ------------------------------------

## check determinants of oe response >0
glm(as.numeric(wc>0) ~ female + age + black + educ + faminc + relig,
    data = cces, family=binomial("logit")) %>%
  summary()

## gender differences in willingness to respond & length of response
mean(cces$wc>0)
t.test(as.numeric(wc>0)~female, data = cces)
prop.test(table(cces$female, cces$wc==0))
mean(ces2018$wc)
t.test(wc~female, data = ces2018)

## prep data for heckit model
heck_tmp <- ces2018 %>%
  select(caseid, polknow_text_mean) %>%
  right_join(cces) %>%
  mutate(select = as.numeric(!is.na(polknow_text_mean)))

table(heck_tmp$select)
table(is.na(heck_tmp$polknow_text_mean), heck_tmp$wc>0)

## estimate heckit model (does this specification make sense theoretically?)
heckit(select ~ female + age + black + educ + faminc + relig,
       polknow_text_mean ~ female + age + black + educ + faminc + relig,
       data = heck_tmp) %>%
  summary()

heckit(select ~ female + age + black + educ + faminc + relig,
       polknow_factual ~ female + age + black + educ + faminc + relig,
       data = heck_tmp) %>%
  summary()

## heckit model for other outcomes
heckit(select ~ female + age + black + educ + faminc + relig,
       vote ~ polknow_text_mean + female + age + black + educ + faminc + relig,
       data = heck_tmp) %>%
  summary()

heckit(select ~ female + age + black + educ + faminc + relig,
       vote ~ polknow_factual + female + age + black + educ + faminc + relig,
       data = heck_tmp) %>%
  summary()

heckit(select ~ female + age + black + educ + faminc + relig,
       polint_att ~ polknow_text_mean + female + age + black + educ + faminc + relig,
       data = heck_tmp) %>%
  summary()

heckit(select ~ female + age + black + educ + faminc + relig,
       polint_att ~ polknow_factual + female + age + black + educ + faminc + relig,
       data = heck_tmp) %>%
  summary()

heckit(select ~ female + age + black + educ + faminc + relig,
       effic_int ~ polknow_text_mean + female + age + black + educ + faminc + relig,
       data = heck_tmp) %>%
  summary()

heckit(select ~ female + age + black + educ + faminc + relig,
       effic_int ~ polknow_factual + female + age + black + educ + faminc + relig,
       data = heck_tmp) %>%
  summary()

heckit(select ~ female + age + black + educ + faminc + relig,
       effic_ext ~ polknow_text_mean + female + age + black + educ + faminc + relig,
       data = heck_tmp) %>%
  summary()

heckit(select ~ female + age + black + educ + faminc + relig,
       effic_ext ~ polknow_factual + female + age + black + educ + faminc + relig,
       data = heck_tmp) %>%
  summary()



# Discursive sophistication as a DV (reviewer request) --------------------

m3rob_disc <- list(
  lm(polknow_text_scale ~ extraversion + newexperience + reserved +
       wordsum + mode +
       female + age + black + pid_dem + pid_rep +
       educ_fact + faminc + relig, data = anes2016),
  lm(polknow_text_scale ~ extraversion + newexperience + reserved +
       wordsum + mode + polknow_factual_scale +
       female + age + black + pid_dem + pid_rep +
       educ_fact + faminc + relig, data = anes2016),
  lm(polknow_text_scale ~ extraversion + newexperience + reserved +
       wordsum + mode +
       female + age + black + pid_dem + pid_rep +
       educ_fact + faminc + relig, data = anes2012),
  lm(polknow_text_scale ~ extraversion + newexperience + reserved +
       wordsum + mode + polknow_factual_scale +
       female + age + black + pid_dem + pid_rep +
       educ_fact + faminc + relig, data = anes2012)
)

stargazer(m3rob_disc, type="text", align = TRUE, column.sep.width = "-5pt", no.space = TRUE, digits = 3,
          model.names=FALSE, star.cutoffs = c(.05,.01,.001),
          dep.var.labels = "Discursive Sophistication",
          title="Personality, verbal skills, and survey mode as predictors
          of discursive sophistication in the 2016 and 2012 ANES.",
          column.labels = rep(c("2016 ANES", "2012 ANES"), 2),
          column.separate = rep(2,4),
          covariate.labels = c("Personality: Extraversion",
                               "Personality: Openness to Experience",
                               "Personality: Reserved",
                               "Verbal Skills (Wordsum score)",
                               "Survey Mode (Online)",
                               "Factual Knowledge",
                               "Female","Age", "Black",
                               "PID: Democrat", "PID: Republican",
                               "Education: High School", "Education: Some College",
                               "Education: Bachelor's Degree", "Education: Graduate Degree",
                               "Household Income", "Church Attendance", "Constant"),
          keep.stat = c("n", "rsq"), font.size = "footnotesize",
          out = "out/app-determinants_rob_disc.tex", label = "tab:determinants_rob_disc")

m3rob_fact <- list(
  lm(polknow_factual_scale ~ extraversion + newexperience + reserved +
       wordsum + mode +
       female + age + black + pid_dem + pid_rep +
       educ_fact + faminc + relig, data = anes2016),
  lm(polknow_factual_scale ~ extraversion + newexperience + reserved +
       wordsum + mode + polknow_text_scale +
       female + age + black + pid_dem + pid_rep +
       educ_fact + faminc + relig, data = anes2016),
  lm(polknow_factual_scale ~ extraversion + newexperience + reserved +
       wordsum + mode +
       female + age + black + pid_dem + pid_rep +
       educ_fact + faminc + relig, data = anes2012),
  lm(polknow_factual_scale ~ extraversion + newexperience + reserved +
       wordsum + mode + polknow_text_scale +
       female + age + black + pid_dem + pid_rep +
       educ_fact + faminc + relig, data = anes2012)
)

stargazer(m3rob_fact, type="text", align = TRUE, column.sep.width = "-5pt", no.space = TRUE, digits = 3,
          model.names=FALSE, star.cutoffs = c(.05,.01,.001),
          dep.var.labels = "Factual Knowledge",
          title="Personality, verbal skills, and survey mode as predictors
          of factual knowledge in the 2016 and 2012 ANES.",
          column.labels = rep(c("2016 ANES", "2012 ANES"), 2),
          column.separate = rep(2,4),
          covariate.labels = c("Personality: Extraversion",
                               "Personality: Openness to Experience",
                               "Personality: Reserved",
                               "Verbal Skills (Wordsum score)",
                               "Survey Mode (Online)",
                               "Discursive Soph.",
                               "Female","Age", "Black",
                               "PID: Democrat", "PID: Republican",
                               "Education: High School", "Education: Some College",
                               "Education: Bachelor's Degree", "Education: Graduate Degree",
                               "Household Income", "Church Attendance", "Constant"),
          keep.stat = c("n", "rsq"), font.size = "footnotesize",
          out = "out/app-determinants_rob_fact.tex", label = "tab:determinants_rob_fact")



# Discursive sophistication and media consumption -------------------------

data_immig %>%
  select(polknow_factual_scale, polknow_text_scale,
         tv_fox:tv_cbs, print_nyt:print_nyp, smedia_yt:smedia_tb) %>%
  pivot_longer(-c(polknow_factual_scale, polknow_text_scale),
               names_to = "source", values_to = "exposure") %>%
  pivot_longer(-c(source, exposure),
               names_to = "Variable", values_to = "polknow") %>%
  ggplot(aes(x = exposure, y = polknow, col = Variable)) +
  geom_smooth(method = "lm") +
  facet_wrap(~source, ncol = 3, dir = "v")

data_immig %>%
  select(polknow_factual_scale, polknow_text_scale,
         tv_trust_fox:tv_trust_cbs, print_trust_nyt:print_trust_nyp) %>%
  pivot_longer(-c(polknow_factual_scale, polknow_text_scale),
               names_to = "src", values_to = "trust") %>%
  pivot_longer(-c(src, trust),
               names_to = "iv", values_to = "polknow") %>%
  mutate(Variable = recode_factor(iv,
                                  `polknow_text_scale` = "Discursive Sophistication",
                                  `polknow_factual_scale` = "Factual Knowledge"),
         Source = recode_factor(src,
                                `print_trust_nyt` = "New York Times",
                                `print_trust_wapo` = "Washington Post",
                                `print_trust_wsj` = "Wall Street Journal",
                                `print_trust_ust` = "USA Today",
                                `print_trust_nyp` = "New York Post",
                                `tv_trust_cnn` = "CNN",
                                `tv_trust_nbc` = "NBC",
                                `tv_trust_cbs` = "CBS",
                                `tv_trust_msnbc` = "MSNBC",
                                `tv_trust_fox` = "FOX News"),
         Type = ifelse(grepl("print", src), "Print", "TV"),
         Type = factor(Type, levels = c("TV","Print"))) %>%
  ggplot(aes(y = trust, x = polknow, lty = Variable, fill = Variable)) +
  plot_default +
  geom_smooth(method = "lm", col = "black", alpha=0.6, lwd=.1) +
  labs(y = "Trust that Reporting is Accurate",
       x = "Value of Independent Variable") +
  theme(legend.position = "bottom") +
  facet_wrap(~Type+Source, ncol = 5, dir = "h") +
  scale_fill_brewer(palette = "Dark2")
ggsave("out/app-media_trust.pdf", width=6.5, height=4)



# Variance in ideological placements ----------

ideo_compare <- function(yname, xname, data, quantiles = c(.25, .75)) {
  y <- data.frame(data)[, yname]
  x <- data.frame(data)[, xname]
  x_lim <- quantile(x, probs = quantiles, na.rm = T)
  y_lo <- y[x <= x_lim[1]]
  y_hi <- y[x > x_lim[2]]
  y_lo_na <- as.numeric(is.na(y_lo))
  y_hi_na <- as.numeric(is.na(y_hi))

  dplyr::tibble(
    dv = yname,
    iv = xname,
    x_lo = x_lim[1],
    x_hi = x_lim[2],
    n_lo = length(y_lo),
    n_hi = length(y_hi),
    sd_lo = sd(y_lo, na.rm = T),
    sd_hi = sd(y_hi, na.rm = T),
    sd_pval = var.test(y_lo, y_hi)$p.value,
    na_lo = mean(y_lo_na, na.rm = T),
    na_hi = mean(y_hi_na, na.rm = T),
    na_pval = t.test(y_lo_na, y_hi_na)$p.value
  ) %>%
    mutate(
      sd_stars = case_when(
        sd_pval < .001 ~ "***",
        sd_pval < .01 ~ "**",
        sd_pval < .05 ~ "*",
        sd_pval >= .05 ~ "ns"
      ),
      na_stars = case_when(
        na_pval < .001 ~ "***",
        na_pval < .01 ~ "**",
        na_pval < .05 ~ "*",
        na_pval >= .05 ~ "ns"
      )
    )
}

ideo <- c("ideo_dem", "ideo_rep", "ideo_sc", "ideo_trump", "ideo_warren", "ideo_ryan",
          "ideo_mcconnel", "ideo_schumer", "ideo_pelosi", "ideo_murkowski", "ideo_collins",
          "ideo_feinstein", "ideo_booker", "ideo_haley")

bind_rows(
  map_dfr(ideo, ~ideo_compare(., "polknow_text_scale", ces2018)),
  map_dfr(ideo, ~ideo_compare(., "polknow_factual_scale", ces2018))
) %>%
  mutate(
    iv = recode_factor(iv,
                       `polknow_text_scale` = "Discursive\nSophistication",
                       `polknow_factual_scale` = "Factual\nKnowledge"),
    sd_mean = (sd_lo + sd_hi)/2,
    dv = recode_factor(dv,
                       `ideo_sc` = "Supreme Court",
                       `ideo_collins` = "Susan Collins",
                       `ideo_booker` = "Cory Booker",
                       `ideo_haley` = "Nikki Haley",
                       `ideo_feinstein` = "Dianne Feinstein",
                       `ideo_murkowski` = "Lisa Murkowski",
                       `ideo_dem` = "Democratic Party",
                       `ideo_mcconnel` = "Mitch McConnell",
                       `ideo_ryan` = "Paul Ryan",
                       `ideo_schumer` = "Chuck Schumer",
                       `ideo_rep` = "Republican Party",
                       `ideo_warren` = "Elizabeth Warren",
                       `ideo_pelosi` = "Nancy Pelosi",
                       `ideo_trump` = "Donald Trump")
  ) %>%
  ggplot(aes(y = dv)) +
  geom_vline(xintercept = 0, color="gray") +
  geom_segment(aes(x = sd_lo, yend = dv,
                   xend = ifelse(sd_hi < sd_lo, sd_hi + .02, sd_hi - .02)),
               lineend = "round", linejoin = "mitre",
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = sd_lo, shape = "Low (25th Percentile)",
                 col = iv), size = 2) +
  geom_point(aes(x = sd_hi, shape = "High (75th Percentile)",
                 col = iv), size = 2) +
  geom_text(aes(x = sd_mean, label = sd_stars), nudge_y = .25, size = 2.5) +
  facet_wrap(~iv) +
  scale_color_brewer(palette = "Dark2", guide = "none") +
  labs(y = NULL,
       x = "Uncertainty in Ideological Placements (in Standard Deviations)",
       col = "Sophistication/Knowledge",
       shape = "Sophistication/Knowledge") +
  plot_default +
  theme(legend.position = "bottom")
ggsave("out/app-placements.pdf", width = 6.5, height = 4)

bind_rows(
  map_dfr(ideo, ~ideo_compare(., "polknow_text_scale", ces2018)),
  map_dfr(ideo, ~ideo_compare(., "polknow_factual_scale", ces2018))
) %>%
  mutate(
    iv = recode_factor(iv,
                       `polknow_text_scale` = "Discursive\nSophistication",
                       `polknow_factual_scale` = "Factual\nKnowledge"),
    na_mean = (na_lo + na_hi)/2,
    dv = recode_factor(dv,
                       `ideo_murkowski` = "Lisa Murkowski",
                       `ideo_collins` = "Susan Collins",
                       `ideo_booker` = "Cory Booker",
                       `ideo_haley` = "Nikki Haley",
                       `ideo_schumer` = "Chuck Schumer",
                       `ideo_mcconnel` = "Mitch McConnell",
                       `ideo_feinstein` = "Dianne Feinstein",
                       `ideo_warren` = "Elizabeth Warren",
                       `ideo_ryan` = "Paul Ryan",
                       `ideo_pelosi` = "Nancy Pelosi",
                       `ideo_sc` = "Supreme Court",
                       `ideo_trump` = "Donald Trump",
                       `ideo_rep` = "Republican Party",
                       `ideo_dem` = "Democratic Party")
  ) %>%
  ggplot(aes(y = dv)) +
  geom_vline(xintercept = 0, color="gray") +
  geom_segment(aes(x = na_lo, yend = dv,
                   xend = ifelse(na_hi < na_lo, na_hi + .01, na_hi - .01)),
               lineend = "round", linejoin = "mitre",
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = na_lo, shape = "Low (25th Percentile)",
                 col = iv), size = 2) +
  geom_point(aes(x = na_hi, shape = "High (75th Percentile)",
                 col = iv), size = 2) +
  geom_text(aes(x = na_mean, label = na_stars), nudge_y = .25, size = 2.5) +
  facet_wrap(~iv) +
  scale_color_brewer(palette = "Dark2", guide = "none") +
  scale_x_continuous(labels = scales::percent) +
  labs(y = NULL,
       x = "`Don't Know` in Ideological Placements (in Percent)",
       col = "Sophistication/Knowledge",
       shape = "Sophistication/Knowledge") +
  plot_default +
  theme(legend.position = "bottom")
ggsave("out/app-placements_dk.pdf", width = 6.5, height = 4)



# Correct voting / proximity voting analysis ------------------------------

SenCand1Avg <- ces2018 %>%
  group_by(SenCand1Name) %>%
  summarize(SenCand1Avg = mean(ideo_cand1, na.rm = T)) %>%
  na.omit()

SenCand2Avg <- ces2018 %>%
  group_by(SenCand2Name) %>%
  summarize(SenCand2Avg = mean(ideo_cand2, na.rm = T)) %>%
  na.omit()

ces2018 <- ces2018 %>%
  left_join(SenCand1Avg) %>%
  left_join(SenCand2Avg) %>%
  mutate(correct_vote = as.numeric(abs(ideo_ego - SenCand1Avg) > abs(ideo_ego - SenCand2Avg)) + 1,
         correct_vote = as.numeric(correct_vote == senate_vote))

m1cv <- glm(correct_vote ~ polknow_text_scale + polknow_factual_scale +
              female + age + black + educ + faminc + relig, data = ces2018, family=binomial("logit"))

bind_rows(
  plot_cap(m1cv, condition = c("polknow_text_scale"), draw = F) %>%
    transmute(iv = "polknow_text_scale", ivval = condition1,
              mean = predicted, cilo = conf.low, cihi = conf.high),
  plot_cap(m1cv, condition = c("polknow_factual_scale"), draw = F) %>%
    transmute(iv = "polknow_factual_scale", ivval = condition1,
              mean = predicted, cilo = conf.low, cihi = conf.high),
) %>%
  as_tibble() %>%
  mutate(Variable = recode_factor(iv,
                                  `polknow_text_scale` = "Discursive Sophistication",
                                  `polknow_factual_scale` = "Factual Knowledge")) %>%
  ggplot(aes(x=ivval, y=mean, ymin=cilo,ymax=cihi, lty=Variable, fill=Variable)) + plot_default +
  geom_ribbon(alpha=0.6, lwd=.1) + geom_line() +
  ylab("Pr(Ideological Proximity Vote)") + xlab("Value of Independent Variable") +
  scale_fill_brewer(palette = "Dark2")
ggsave("out/app-correct_vote.pdf", width=4, height=2)

stargazer(m1cv, type="text", align = TRUE, column.sep.width = "-25pt", no.space = TRUE, digits = 3,
          model.names=FALSE, dep.var.labels.include = FALSE, star.cutoffs = c(.05,.01,.001),
          title="Logistic regression predicting ideological proximity-based voting for
          US Senators in the 2018 CES. Standard errors in parentheses.
          Estimates are used for Figure \\ref{fig:correct_vote}.",
          column.labels = "Ideological Proximity Vote",
          covariate.labels = c("Discursive Soph.","Factual Knowledge",
                               "Female", "Age", "Black", "College Degree",
                               "Household Income","Church Attendance","Constant"),
          keep.stat = c("n", "rsq", "aic"), font.size = "footnotesize",
          out = "out/app-correct_vote.tex", label = "tab:correct_vote")
