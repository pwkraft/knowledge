### ============================================================= ###
### Measuring Political Sophistication using Open-ended Responses ###
### Patrick Kraft                                                 ###
### ============================================================= ###
## analyzing citations of Delli Carpini/Keeter


rm(list = ls())
library(tidyverse)

setwd("/data/Dropbox/Uni/Projects/2016/knowledge/calc")

## load data and stm results
raw <- read_csv("in/PoPCites_clean.csv")

## Distiguishing b/w Journals
cites <- raw %>%
  mutate(Journal = recode(Source, 
                          "American Journal of Political Science" = "AJPS",
                          "American Journal of" = "AJPS",
                          "American Journal of Political" = "AJPS",
                          "American Political Science Review" = "APSR",
                          "American Political Science" = "APSR",
                          "The Journal of Politics" = "JOP",
                          "The Journal of" = "JOP",
                          "Journal of Politics" = "JOP",
                          .default = "Other")) %>%
  mutate(Journal = factor(replace_na(Journal, "Other"))) %>%
  filter(Year >= 1993 & Year <= 2017) %>%
  group_by(Year, Journal) %>%
  summarise(Count = n()) %>%
  complete(Year, Journal, fill = list(Count = 0)) %>%
  group_by(Journal) %>%
  mutate(Total = cumsum(Count))

ggplot(cites, aes(x = Year, y = Total, lty = Journal, col = Journal)) +
  geom_line()


## All citations
raw %>%
  filter(Year >= 1993 & Year <= 2017) %>%
  group_by(Year) %>%
  summarise(Citations = n()) %>%
  ggplot(aes(x = Year, y = Citations)) +
  geom_line() +
  ylab("Citations per Year") +
  xlab("") +
  theme_classic(base_size=9) + 
  theme(panel.border = element_rect(fill=NA)) +
  scale_x_continuous(breaks=seq(1995,2015,5))
ggsave("../fig/cites.pdf", width=3.5, height=2.2)
ggsave("../fig/cites.png", width=3.5, height=2.2)


