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
  study = factor(1:6, labels = c("2020 ANES", "2016 ANES", "2012 ANES",
                                 "2018 CES", "2015 YouGov", "Swiss Survey")),
  wc = c(mean(anes2020$wc[anes2020$wc != 0]), mean(anes2016$wc[anes2016$wc != 0]),
         mean(anes2012$wc[anes2012$wc != 0]), mean(ces2018$wc[ces2018$wc != 0]),
         mean(yg2015$wc[yg2015$wc != 0]), mean(swiss2012$wc[swiss2012$wc != 0]))
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
) %>%
  filter(wc != 0) %>%
  mutate(study = factor(study, labels = c("2020 ANES", "2016 ANES", "2012 ANES",
                                          "2018 CES", "2015 YouGov", "Swiss Survey"))) %>%
  ggplot(aes(wc)) + geom_histogram(fill = "grey") + theme_classic(base_size = 8) +
  theme(panel.border = element_rect(fill=NA)) +
  facet_wrap(.~study, ncol = 3, scales = "free") +
  geom_vline(aes(xintercept = wc), colour="red", linetype = "longdash",data = wc_mean) +
  ylab("Number of Respondents") + xlab("Word Count")
ggsave(here("out/appB1-wc.png"), width = 6, height = 2.5)


# Figure B.2: Proportion of non-response comparing male and female --------

## Create figure
bind_rows(
  transmute(anes2020, noresp = wc == 0, female = female, study = 1),
  transmute(anes2016, noresp = wc == 0, female = female, study = 2),
  transmute(anes2012, noresp = wc == 0, female = female, study = 3),
  transmute(ces2018, noresp = wc == 0, female = female, study = 4),
  transmute(yg2015, noresp = wc == 0, female = female, study = 5),
  transmute(swiss2012, noresp = wc == 0, female = female, study = 6)
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
ggsave("out/appB2-noresponse.png", width = 6, height = 2.5)


# Figure B.3: Estimated topic proportions based on the structural  --------

pdf("out/appB3-stm_prop.pdf", width=12, height=10)
par(mfrow = c(2,4), mar=c(4.2,0.5,2.5,0.5))
plot(ces2018disc$stm, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("2018 CES (k = ", ces2018disc$stm$settings$dim$K,")", collapse = ""))
plot(anes2020disc$stm, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("2020 ANES (k = ", anes2020disc$stm$settings$dim$K,")", collapse = ""))
plot(anes2016disc$stm, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("2016 ANES (k = ", anes2016disc$stm$settings$dim$K,")", collapse = ""))
plot(anes2012disc$stm, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("2012 ANES (k = ", anes2012disc$stm$settings$dim$K,")", collapse = ""))
plot(yg2015disc$stm, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("2015 YouGov (k = ", yg2015disc$stm$settings$dim$K,")", collapse = ""))
plot(swiss2012disc_fr$stm, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("Swiss Survey - French (k = ", swiss2012disc_fr$stm$settings$dim$K,")", collapse = ""))
plot(swiss2012disc_de$stm, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("Swiss Survey - German (k = ", swiss2012disc_de$stm$settings$dim$K,")", collapse = ""))
plot(swiss2012disc_it$stm, n = 5, labeltype = "frex", text.cex = 1,
     main = paste0("Swiss Survey - Italian (k = ", swiss2012disc_it$stm$settings$dim$K,")", collapse = ""))
dev.off()


# Figure B.4: Correlation matrix of individual components of discu --------

## 2018 CES
ces2018 %>%
  transmute(v1 = size, v2 = range, v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")) +
  plot_default
ggsave("out/appB4a-ces20182018_components.png", width = 2.6, height = 2.6)

## 2015 YouGov
yg2015 %>%
  transmute(v1 = size, v2 = range, v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")) +
  plot_default
ggsave("out/appB4b-yg_components.png", width = 2.6, height = 2.6)

## 2020 ANES
anes2020 %>%
  transmute(v1 = size, v2 = range, v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")) +
  plot_default
ggsave("out/appB4c-anes2020_components.png", width = 2.6, height = 2.6)

## 2016 ANES
anes2016 %>%
  transmute(v1 = size, v2 = range, v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")) +
  plot_default
ggsave("out/appB4d-anes2016_components.png", width = 2.6, height = 2.6)

## 2012 ANES
anes2012 %>%
  transmute(v1 = size, v2 = range, v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")) +
  plot_default
ggsave("out/appB4e-anes2012_components.png", width = 2.6, height = 2.6)


## Swiss - French
swiss2012_fr %>%
  transmute(v1 = size, v2 = range, v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")) +
  plot_default
ggsave("out/appB4f-french_components.png", width = 2.6, height = 2.6)

## Swiss - German
swiss2012_de %>%
  transmute(v1 = size, v2 = range, v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")) +
  plot_default
ggsave("out/appB4g-german_components.png", width = 2.6, height = 2.6)

## Swiss - Italian
swiss2012_it %>%
  transmute(v1 = size, v2 = range, v3 = constraint) %>%
  ggpairs(lower = list(continuous = wrap("smooth", alpha = .05, size = .2)),
          axisLabels = "none",
          columnLabels = c("Size", "Range", "Constraint")) +
  plot_default
ggsave("out/appB4h-italian_components.png", width = 2.6, height = 2.6)


# Figure C.1: PreText analysis of preprocessing decisions of open- --------

## Select raw documents
set.seed(12345)
preText_res <- list(ces2018, anes2020, anes2016, anes2012, yg2015,
                    swiss2012_fr, swiss2012_de, swiss2012_it) %>%
  map(oe_sample) %>%
  map(factorial_preprocessing, use_ngrams = FALSE, parallel = TRUE, cores = 12) %>%
  map(preText, parallel = TRUE, cores = 12)
names(preText_res) <- c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES","2015 YouGov",
                        "Swiss (French)","Swiss (German)","Swiss (Italian)")

## Create plot
preText_res %>%
  map(regression_coefficient_plot, remove_intercept = TRUE) %>%
  map_dfr("data", .id = "study") %>%
  mutate(study = factor(study, levels = c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES","2015 YouGov",
                                          "Swiss (French)","Swiss (German)","Swiss (Italian)"))) %>%
  ggplot(aes(x = Coefficient, xmin = Coefficient-2*SE, xmax = Coefficient+2*SE, y = Variable)) +
  geom_point() + geom_errorbarh(height=0) + geom_vline(xintercept = 0) +
  facet_wrap(~study, ncol=2) + labs(y=NULL, x = "Regression Coefficient") + plot_default
ggsave("out/appC1-pretext.png",width = 6, height = 4.5)


# Figure C.2: Robustness of discursive sophistication measure for  --------

## Compute discursive sophistication with alternative specifications
plot_df <- bind_rows(
  robust_discursive(data = ces2018, datalab = "2018 CES", K = 35),
  robust_discursive(data = anes2020, datalab = "2020 ANES", K = 35),
  robust_discursive(data = anes2016, datalab = "2016 ANES", K = 35),
  robust_discursive(data = anes2012, datalab = "2012 ANES", K = 35),
  robust_discursive(data = yg2015, datalab = "2015 YouGov", K = 35),
  robust_discursive(data = swiss2012_fr, datalab = "Swiss (French)", K = 35,
                    meta = c("age", "educ_cont", "ideo_cont", "educ_ideo", "female"),
                    language = "french", dictionary = dict_constraint$fr),
  robust_discursive(data = swiss2012_de, datalab = "Swiss (German)", K = 35,
                    meta = c("age", "educ_cont", "ideo_cont", "educ_ideo", "female"),
                    language = "german", dictionary = dict_constraint$de),
  robust_discursive(data = swiss2012_it, datalab = "Swiss (Italian)", K = 35,
                    meta = c("age", "educ_cont", "ideo_cont", "educ_ideo", "female"),
                    language = "italian", dictionary = dict_constraint$it),

  robust_discursive(data = ces2018, datalab = "2018 CES", stem = FALSE),
  robust_discursive(data = anes2020, datalab = "2020 ANES", stem = FALSE),
  robust_discursive(data = anes2016, datalab = "2016 ANES", stem = FALSE),
  robust_discursive(data = anes2012, datalab = "2012 ANES", stem = FALSE),
  robust_discursive(data = yg2015, datalab = "2015 YouGov", stem = FALSE),
  robust_discursive(data = swiss2012_fr, datalab = "Swiss (French)", stem = FALSE,
                    meta = c("age", "educ_cont", "ideo_cont", "educ_ideo", "female"),
                    language = "french", dictionary = dict_constraint$fr),
  robust_discursive(data = swiss2012_de, datalab = "Swiss (German)", stem = FALSE,
                    meta = c("age", "educ_cont", "ideo_cont", "educ_ideo", "female"),
                    language = "german", dictionary = dict_constraint$de),
  robust_discursive(data = swiss2012_it, datalab = "Swiss (Italian)", stem = FALSE,
                    meta = c("age", "educ_cont", "ideo_cont", "educ_ideo", "female"),
                    language = "italian", dictionary = dict_constraint$it),

  robust_discursive(data = ces2018, datalab = "2018 CES", removestopwords = FALSE),
  robust_discursive(data = anes2020, datalab = "2020 ANES", removestopwords = FALSE),
  robust_discursive(data = anes2016, datalab = "2016 ANES", removestopwords = FALSE),
  robust_discursive(data = anes2012, datalab = "2012 ANES", removestopwords = FALSE),
  robust_discursive(data = yg2015, datalab = "2015 YouGov", removestopwords = FALSE),
  robust_discursive(data = swiss2012_fr, datalab = "Swiss (French)", removestopwords = FALSE,
                    meta = c("age", "educ_cont", "ideo_cont", "educ_ideo", "female"),
                    language = "french", dictionary = dict_constraint$fr),
  robust_discursive(data = swiss2012_de, datalab = "Swiss (German)", removestopwords = FALSE,
                    meta = c("age", "educ_cont", "ideo_cont", "educ_ideo", "female"),
                    language = "german", dictionary = dict_constraint$de),
  robust_discursive(data = swiss2012_it, datalab = "Swiss (Italian)", removestopwords = FALSE,
                    meta = c("age", "educ_cont", "ideo_cont", "educ_ideo", "female"),
                    language = "italian", dictionary = dict_constraint$it)
) %>%
  mutate(
    datalab = factor(datalab,
                     levels = c("2018 CES", "2020 ANES", "2016 ANES", "2012 ANES", "2015 YouGov",
                                "Swiss (French)", "Swiss (German)", "Swiss (Italian)")),
    condition = factor(100*K + 10*stem + 1*removestopwords,
                       levels = c("3511","2501","2510"),
                       labels = c("More topics (k = 35)", "No stemming", "Keep stopwords"))
  )

## Compute correlations for subgroups
plot_cor <- plot_df %>%
  group_by(datalab, condition) %>%
  summarize(cor = paste0("r = ",round(cor(original, replication), 3))) %>%
  mutate(original = 4, replication = -2.5)

## Create figure
ggplot(plot_df, aes(y = original, x = replication)) +
  geom_point(alpha=.05) + geom_smooth(method="lm") +
  facet_grid(datalab~condition) +
  geom_text(data=plot_cor, aes(label=cor), size=2) +
  labs(y = "Discursive Sophistication (Preferred Specification)",
       x = "Discursive Sophistication (Alternative Specifications)") +
  plot_default
ggsave("out/appC2-pretext_robustness.png", width=5, height=8.5)


# Figure C.3: Controlling for personality and verbal skills ---------------

## Select dependent and independent variables
dvs <- c("vote", "polint", "effic_int", "effic_ext")
ivs <- c("discursive", "polknow",
         "female", "age", "black", "educ", "faminc", "relig")
ivs_rob <- c("extraversion + newexperience + reserved", "wordsum", "wc", "mode")

## Estimate models
m1cont <- c(
  map(ivs_rob,
      ~glm(reformulate(c(ivs, .), response = "vote"), data = anes2016, family=binomial("logit"))),
  map(ivs_rob,
      ~lm(reformulate(c(ivs, .), response = "polint"), data = anes2016)),
  map(ivs_rob,
      ~lm(reformulate(c(ivs, .), response = "effic_int"), data = anes2016)),
  map(ivs_rob,
      ~lm(reformulate(c(ivs, .), response = "effic_ext"), data = anes2016)),
  map(ivs_rob,
      ~glm(reformulate(c(ivs, .), response = "vote"), data = anes2012, family=binomial("logit"))),
  map(ivs_rob,
      ~lm(reformulate(c(ivs, .), response = "polint"), data = anes2012)),
  map(ivs_rob,
      ~lm(reformulate(c(ivs, .), response = "effic_int"), data = anes2012)),
  map(ivs_rob,
      ~lm(reformulate(c(ivs, .), response = "effic_ext"), data = anes2012))
)

## Create figure
m1cont %>%
  map_dfr(~tidy(comparisons(
    ., variables = list(discursive = c(-1,1),
                        polknow = c(-1,1)))),
    .id = "model") %>%
  as_tibble() %>%
  mutate(
    study = factor(rep(c("2016 ANES", "2012 ANES"), each = 32),
                   levels = c("2016 ANES", "2012 ANES")),
    dv = recode_factor(rep(rep(dvs, each = 8), 2),
                       `vote` = "Turnout",
                       `polint` = "Political Interest",
                       `effic_int` = "Internal Efficacy",
                       `effic_ext` = "External Efficacy"),
    Control = factor(rep(rep(1:4, each = 2), 8),
                     labels = c("Personality",
                                "Wordsum score",
                                "Response length",
                                "Survey mode")),
    term = recode_factor(term,
                         `polknow` = "Factual\nKnowledge",
                         `discursive` = "Discursive\nSophistication")) %>%
  ggplot(aes(y=term, x=estimate, xmin=conf.low, xmax=conf.high,
             col = Control, shape = Control)) +
  geom_vline(xintercept = 0, color="grey") +
  geom_point(position = position_dodge(width = -.5)) +
  geom_errorbarh(height=0, position = position_dodge(width = -.5)) +
  facet_grid(study~dv) +
  labs(x = "Estimated Effect of Discursive Sophistication and Factual Knowledge\n(for an increase from 1 SD below mean to 1 SD above mean)", y = "Independent Variable") +
  plot_default + theme(legend.position = "bottom")
ggsave("out/appC3-knoweff_robust.pdf", width=6.5, height=3.5)


# Figure C.4: Effects of sophistication on the probability of resp --------

## Select variables
ideo <- c("ideo_dem", "ideo_rep", "ideo_sc", "ideo_trump", "ideo_warren", "ideo_ryan",
          "ideo_mcconnel", "ideo_schumer", "ideo_pelosi", "ideo_murkowski", "ideo_collins",
          "ideo_feinstein", "ideo_booker", "ideo_haley")

## Create figure
bind_rows(
  map_dfr(ideo, ~ideo_compare(., "discursive", filter(ces2018, wc != 0))),
  map_dfr(ideo, ~ideo_compare(., "polknow", filter(ces2018, wc != 0)))
) %>%
  mutate(
    iv = recode_factor(iv,
                       `discursive` = "Discursive\nSophistication",
                       `polknow` = "Factual\nKnowledge"),
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
       x = "'Don't Know' in Ideological Placements (in Percent)",
       col = "Sophistication/Knowledge",
       shape = "Sophistication/Knowledge") +
  plot_default +
  theme(legend.position = "bottom")
ggsave("out/appC4-placements_dk.pdf", width = 6.5, height = 4)


# Figure C.5: Effects of sophistication on the uncertainty around  --------

## Create figure
bind_rows(
  map_dfr(ideo, ~ideo_compare(., "discursive", filter(ces2018, wc != 0))),
  map_dfr(ideo, ~ideo_compare(., "polknow", filter(ces2018, wc != 0)))
) %>%
  mutate(
    iv = recode_factor(iv,
                       `discursive` = "Discursive\nSophistication",
                       `polknow` = "Factual\nKnowledge"),
    sd_mean = (sd_lo + sd_hi)/2,
    dv = recode_factor(dv,
                       `ideo_sc` = "Supreme Court",
                       `ideo_collins` = "Susan Collins",
                       `ideo_feinstein` = "Dianne Feinstein",
                       `ideo_haley` = "Nikki Haley",
                       `ideo_murkowski` = "Lisa Murkowski",
                       `ideo_booker` = "Cory Booker",
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
ggsave("out/appC5-placements.pdf", width = 6.5, height = 4)


# Figure C.6: Expected probability to vote for the senatorial cand --------

## Compute average candidate positions
SenCand1Avg <- ces2018 %>%
  group_by(SenCand1Name) %>%
  summarize(SenCand1Avg = mean(ideo_cand1, na.rm = T)) %>%
  na.omit()
SenCand2Avg <- ces2018 %>%
  group_by(SenCand2Name) %>%
  summarize(SenCand2Avg = mean(ideo_cand2, na.rm = T)) %>%
  na.omit()

## Compare vote choice to relative candidate proximity
ces2018 <- ces2018 %>%
  left_join(SenCand1Avg) %>%
  left_join(SenCand2Avg) %>%
  mutate(correct_vote = as.numeric(abs(ideo_ego - SenCand1Avg) > abs(ideo_ego - SenCand2Avg)) + 1,
         correct_vote = as.numeric(correct_vote == senate_vote))

## Estimate model
m1cv <- glm(correct_vote ~ discursive + polknow +
              female + age + black + educ + faminc + relig,
            data = ces2018, family=binomial("logit"))

## Create figure
bind_rows(
  plot_cap(m1cv, condition = c("discursive"), draw = F) %>%
    transmute(iv = "discursive", ivval = condition1,
              mean = predicted, cilo = conf.low, cihi = conf.high),
  plot_cap(m1cv, condition = c("polknow"), draw = F) %>%
    transmute(iv = "polknow", ivval = condition1,
              mean = predicted, cilo = conf.low, cihi = conf.high)) %>%
  as_tibble() %>%
  mutate(Variable = recode_factor(iv,
                                  `discursive` = "Discursive Sophistication",
                                  `polknow` = "Factual Knowledge")) %>%
  ggplot(aes(x=ivval, y=mean, ymin=cilo,ymax=cihi, lty=Variable, fill=Variable)) + plot_default +
  geom_ribbon(alpha=0.6, lwd=.1) + geom_line() +
  ylab("Pr(Ideological Proximity Vote)") + xlab("Value of Independent Variable") +
  scale_fill_brewer(palette = "Dark2")
ggsave("out/appC6-correct_vote.pdf", width=4, height=2)


# Figure C.7: Average trust in different news outlets as a functio --------

## Create figure
mturk2019 %>%
  select(polknow, discursive,
         tv_trust_fox:tv_trust_cbs, print_trust_nyt:print_trust_nyp) %>%
  pivot_longer(-c(polknow, discursive),
               names_to = "src", values_to = "trust") %>%
  pivot_longer(-c(src, trust),
               names_to = "iv", values_to = "polknow") %>%
  mutate(Variable = recode_factor(iv,
                                  `discursive` = "Discursive Sophistication",
                                  `polknow` = "Factual Knowledge"),
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
ggsave("out/appC7-media_trust.pdf", width=6.5, height=4)


# Table C.1: Logistic regression predicting ideological proximity- --------

## Create table
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
          out = "out/appC1-correct_vote.tex", label = "tab:correct_vote")


# Table C.2: Personality, verbal skills, and survey mode as predic --------

## Estimate models
m3rob_disc <- list(
  lm(discursive ~ extraversion + newexperience + reserved +
       wordsum + mode +
       female + age + black + pid_dem + pid_rep +
       educ_fact + faminc + relig,
     data = anes2016, subset = !is.na(polknow)),
  lm(discursive ~ extraversion + newexperience + reserved +
       wordsum + mode + polknow +
       female + age + black + pid_dem + pid_rep +
       educ_fact + faminc + relig,
     data = anes2016, subset = !is.na(polknow)),
  lm(discursive ~ extraversion + newexperience + reserved +
       wordsum + mode +
       female + age + black + pid_dem + pid_rep +
       educ_fact + faminc + relig,
     data = anes2012, subset = !is.na(polknow)),
  lm(discursive ~ extraversion + newexperience + reserved +
       wordsum + mode + polknow +
       female + age + black + pid_dem + pid_rep +
       educ_fact + faminc + relig,
     data = anes2012, subset = !is.na(polknow))
)

## Create table
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
          out = "out/appC2-determinants_rob_disc.tex", label = "tab:determinants_rob_disc")


# Table C.3: Personality, verbal skills, and survey mode as predic --------

## Estimate models
m3rob_fact <- list(
  lm(polknow ~ extraversion + newexperience + reserved +
       wordsum + mode +
       female + age + black + pid_dem + pid_rep +
       educ_fact + faminc + relig,
     data = anes2016, subset = !is.na(discursive)),
  lm(polknow ~ extraversion + newexperience + reserved +
       wordsum + mode + discursive +
       female + age + black + pid_dem + pid_rep +
       educ_fact + faminc + relig,
     data = anes2016, subset = !is.na(discursive)),
  lm(polknow ~ extraversion + newexperience + reserved +
       wordsum + mode +
       female + age + black + pid_dem + pid_rep +
       educ_fact + faminc + relig,
     data = anes2012, subset = !is.na(discursive)),
  lm(polknow ~ extraversion + newexperience + reserved +
       wordsum + mode + discursive +
       female + age + black + pid_dem + pid_rep +
       educ_fact + faminc + relig,
     data = anes2012, subset = !is.na(discursive))
)

## Create table
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
          out = "out/appC3-determinants_rob_fact.tex", label = "tab:determinants_rob_fact")
