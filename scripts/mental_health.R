#' 5150 mental health
source("scripts/prep_data.R")
source("utils/analytics.R")

# filter on mental health codes
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


#CORRELATED charges
bart_uof.df %>%  
  filter(`Case #` %in% cases.filter) %>%
  select(`Case #`, charge_norm) %>%
  group_by(charge_norm) %>%
  summarize(num_cases = n_distinct(`Case #`)) %>%
  mutate(pct_cases = num_cases / length(cases.filter)) %>%
  arrange(-num_cases) %>%
  left_join(pc_dict.df, by = "charge_norm") %>%
  View(.)


#age and race distribution of mental health
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
  ggplot(aes(x = citizen_age, fill = `Citizen Race`)) +
  geom_histogram(position = "identity", binwidth = 5, alpha = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.25) +
  scale_x_continuous(breaks = seq(0, 80, by = 10))

bart_uof.df %>%
  select(`Case #`, `Citizen Race`, citizen_age) %>%
  filter(`Case #` %in% cases.filter) %>%
  unique() %>%
  filter(`Citizen Race` %in% c("Black", "White", "Hispanic")) %>%
  ggplot(aes(x = citizen_age, fill = `Citizen Race`, y = after_stat(density))) +
  geom_histogram(position = "identity", binwidth = 5, alpha = 0.7) +
  scale_x_continuous(breaks = seq(0, 80, by = 10))