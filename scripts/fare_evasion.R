#' Fare evasion deep dive
source("scripts/prep_data.R")
source("utils/analytics.R")


#• 481.1 (B) Counterfeit Fare
#• 587 (C) Evade Railroad Fare
#• 640(c)(1) Fare Evasion
#• 640(c)(2) Misuse Transfer Pass
#• 640(c)(3)(a) Fraudulent Ticket


# filter on fare evasion charges
cases.filter <-
  bart_uof.df %>%
  filter(charge_norm %in% c("PC481.1B", "PC587C", "PC640C1", "PC640C2", "PC640C3A")) %>%
  select(`Case #`) %>%
  unique() %>%
  pull()

#total cases
bart_uof.df %>%
  filter(`Case #` %in% cases.filter) %>%
  summarise(num_cases = n_distinct(`Case #`))

#age and race distribution of fare evasion charges
bart_uof.df %>%
  select(`Case #`, `Citizen Race`) %>%
  filter(`Case #` %in% cases.filter) %>%
  group_by(`Citizen Race`) %>%
  summarize(num_cases = n_distinct(`Case #`)) %>%
  arrange(-num_cases) %>%
  mutate(pct_cases = num_cases / 104) %>%
  View(.)

bart_uof.df %>%
  select(`Case #`, `Citizen Race`, citizen_age) %>%
  filter(`Case #` %in% cases.filter) %>%
  unique() %>%
  filter(`Citizen Race` %in% c("Black", "White", "Hispanic")) %>%
  ggplot(aes(x = citizen_age, color = `Citizen Race`)) +
  geom_freqpoly(binwidth = 5, alpha = 1) +
  scale_x_continuous(breaks = seq(0, 80, by = 10))

bart_uof.df %>%
  select(`Case #`, `Citizen Race`, citizen_age) %>%
  filter(`Case #` %in% cases.filter) %>%
  unique() %>%
  filter(`Citizen Race` %in% c("Black", "White", "Hispanic")) %>%
  ggplot(aes(x = citizen_age, color = `Citizen Race`, y = after_stat(density))) +
  geom_freqpoly(binwidth = 5, alpha = 1) +
  scale_x_continuous(breaks = seq(0, 80, by = 10))

         