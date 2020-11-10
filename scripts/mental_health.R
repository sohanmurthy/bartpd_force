#' 5150 mental health
source("scripts/prep_data.R")
source("utils/analytics.R")

# filter on fare evasion charges
cases.filter <-
  bart_uof.df %>%
  filter(charge_norm %in% c("WI5150")) %>%
  select(`Case #`) %>%
  unique() %>%
  pull()

#total cases
case_total.val <-
bart_uof.df %>%
  filter(`Case #` %in% cases.filter) %>%
  summarise(num_cases = n_distinct(`Case #`)) %>%
  pull(num_cases)

#age and race distribution of fare evasion charges
bart_uof.df %>%
  select(`Case #`, `Citizen Race`) %>%
  filter(`Case #` %in% cases.filter) %>%
  group_by(`Citizen Race`) %>%
  summarize(num_cases = n_distinct(`Case #`)) %>%
  arrange(-num_cases) %>%
  mutate(pct_cases = num_cases / case_total.val) %>%
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