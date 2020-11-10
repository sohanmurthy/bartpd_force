#' Charge Distribution
#' 

source("scripts/prep_data.R")
source("utils/analytics.R")


# distribution by charge
bart_uof.df %>%
  select(`Case #`, charge_norm) %>%
  group_by(charge_norm) %>%
  summarize(num_cases = n_distinct(`Case #`)) %>%
  mutate(pct_cases = num_cases / 792) %>%
  arrange(-num_cases) %>%
  left_join(pc_dict.df, by = "charge_norm") %>%
  select(charge_norm, code_type, statutory_code, literal_display, num_cases, pct_cases) %>%
  View(.)

#output to csv

bart_uof.df %>%
  select(`Case #`, charge_norm) %>%
  group_by(charge_norm) %>%
  summarize(num_cases = n_distinct(`Case #`)) %>%
  mutate(pct_cases = num_cases / 792) %>%
  arrange(-num_cases) %>%
  left_join(pc_dict.df, by = "charge_norm") %>%
  select(charge_norm, code_type, statutory_code, literal_display, num_cases, pct_cases) %>%
  write.csv("output/charge_dist.csv", row.names = FALSE)
