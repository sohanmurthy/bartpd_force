#' analysis sandbox for BART PD use of force data
source("utils/analytics.R")
source("scripts/prep_data.R")

bart_uof.df %>%
  summarize(n_distinct(`Case #`))

# distribution by charge
bart_uof.df %>%
  select(`Case #`, charge_norm) %>%
  group_by(charge_norm) %>%
  summarize(num_cases = n_distinct(`Case #`)) %>%
  mutate(pct_cases = num_cases / 792) %>%
  arrange(-num_cases) %>%
  View(.)


# filter specific charges
cases.filter <-
  bart_uof.df %>%
  filter(str_detect(charge_norm, "PC640C1")) %>%
  select(`Case #`) %>%
  unique() %>%
  pull()

bart_uof.df %>%
  filter(`Case #` %in% cases.filter) %>%
  select(`Case #`, charge_norm) %>%
  group_by(charge_norm) %>%
  summarize(num_cases = n_distinct(`Case #`)) %>%
  mutate(pct_cases = num_cases / length(cases.filter)) %>%
  arrange(-num_cases) %>%
  left_join(pc_crosswalk.df, by = "charge_norm") %>%
  View(.)


#age distribution
bart_uof.df %>%
  select(`Case #`, `Citizen Race`, citizen_age) %>%
  unique() %>%
  filter(`Citizen Race` %in% c("Black", "White", "Hispanic")) %>%
  ggplot(aes(x = citizen_age, color = `Citizen Race`)) +
  geom_freqpoly(binwidth = 5, alpha = 0.8)


#station distribution
bart_uof.df %>%
  select(`Case #`, `County/location`) %>%
  group_by(`County/location`) %>%
  summarize(num_cases = n_distinct(`Case #`)) %>%
  ggplot(aes(x = reorder(`County/location`, num_cases), y = num_cases)) +
  geom_bar(stat = "identity") +
  coord_flip()





