#' analysis sandbox for BART PD use of force data
library(rcartocolor)

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
  left_join(pc_dict.df, by = "charge_norm") %>%
  View(.)


# filter specific charges
cases.filter <-
  bart_uof.df %>%
  filter(str_detect(charge_norm, "640")) %>%
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
  left_join(pc_dict.df, by = "charge_norm") %>%
  View(.)


#age and race distribution
bart_uof.df %>%
  select(`Case #`, `Citizen Race`) %>%
  group_by(`Citizen Race`) %>%
  summarize(num_cases = n_distinct(`Case #`)) %>%
  arrange(-num_cases) %>%
  mutate(pct_cases = num_cases / 792) %>%
  View(.)

bart_uof.df %>%
  select(`Case #`, `Citizen Race`, citizen_age) %>%
  unique() %>%
  filter(`Citizen Race` %in% c("Black", "White", "Hispanic")) %>%
  ggplot(aes(x = citizen_age, color = `Citizen Race`)) +
  geom_freqpoly(binwidth = 5, alpha = 1) +
  scale_x_continuous(breaks = seq(0, 80, by = 10))

bart_uof.df %>%
  select(`Case #`, `Citizen Race`, citizen_age) %>%
  unique() %>%
  filter(`Citizen Race` %in% c("Black", "White", "Hispanic")) %>%
  ggplot(aes(x = citizen_age, color = `Citizen Race`, y = after_stat(density))) +
  geom_freqpoly(binwidth = 5, alpha = 1) +
  scale_x_continuous(breaks = seq(0, 80, by = 10))




#station distribution
bart_uof.df %>%
  select(`Case #`, `County/location`) %>%
  group_by(`County/location`) %>%
  summarize(num_cases = n_distinct(`Case #`)) %>%
  arrange(-num_cases) %>%
  View(.)

bart_uof.df %>%
  select(`Case #`, `County/location`) %>%
  group_by(`County/location`) %>%
  summarize(num_cases = n_distinct(`Case #`)) %>%
  ggplot(aes(x = reorder(`County/location`, num_cases), y = num_cases)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.title.y = element_blank())




#time/dow distribution
bart_uof.df %>%
  filter(!is.na(incident_timestamp)) %>%
  select(`Case #`, incident_timestamp) %>%
  unique() %>%
  mutate(hour = hour(incident_timestamp),
         dow = wday(incident_timestamp, label = TRUE, abbr = TRUE)) %>%
  group_by(hour, dow) %>%
  summarise(num_cases = n_distinct(`Case #`)) %>%
  ggplot(aes(x = hour, y = dow, fill = num_cases, label = num_cases)) +
  geom_tile(color = "white", size = 0.5) +
  scale_x_continuous(breaks = seq(0,23, by = 2)) +
  scale_fill_gradientn(colors = carto_pal(7, "PurpOr"), name = "Cases") +
  labs(x = "Hour of Day") +
  theme(panel.grid.major = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(margin = margin(r = -15)))

#time/dow by station
station.filter <-
  bart_uof.df %>%
  select(`Case #`, `County/location`) %>%
  group_by(`County/location`) %>%
  summarize(num_cases = n_distinct(`Case #`)) %>%
  top_n(10) %>%
  arrange(-num_cases) %>%
  pull(`County/location`)

      
bart_uof.df %>%
  filter(!is.na(incident_timestamp),
         `County/location` %in% station.filter) %>%
  select(`Case #`, `County/location`, incident_timestamp) %>%
  unique() %>%
  mutate(hour = hour(incident_timestamp),
         dow = wday(incident_timestamp, label = TRUE, abbr = TRUE)) %>%
  group_by(hour, dow, `County/location`) %>%
  summarise(num_cases = n_distinct(`Case #`)) %>%
  mutate(`County/location` = factor(`County/location`, levels = station.filter)) %>%
  ggplot(aes(x = hour, y = dow, fill = num_cases, label = num_cases)) +
  geom_tile(color = "white", size = 0.5) +
  scale_x_continuous(breaks = seq(0,23, by = 2)) +
  scale_fill_gradientn(colors = carto_pal(7, "PurpOr"), name = "Cases") +
  facet_wrap(~`County/location`, ncol = 1) +
  labs(x = "Hour of Day") +
  theme(panel.grid.major = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(margin = margin(r = -15)))
    

#type of force used
bart_uof.df %>%
  select(`Case #`, `UOF: Type force used`) %>%
  group_by(`UOF: Type force used`) %>%
  summarize(num_cases = n_distinct(`Case #`)) %>%
  arrange(-num_cases) %>%
  mutate(pct_cases = num_cases / 792) %>%
  View(.)

race_case_dist.df <-
  bart_uof.df %>%
  select(`Case #`, `Citizen Race`) %>%
  group_by(`Citizen Race`) %>%
  summarize(total_cases = n_distinct(`Case #`)) %>%
  arrange(-total_cases)

bart_uof.df %>%
  select(`Case #`, `UOF: Type force used`, `Citizen Race`) %>%
  filter(`Citizen Race` %in% c("Black", "White", "Hispanic", "Asian")) %>%
  group_by(`UOF: Type force used`, `Citizen Race`) %>%
  summarize(num_cases = n_distinct(`Case #`)) %>%
  left_join(race_case_dist.df, by = "Citizen Race") %>%
  mutate(case_dist = num_cases / total_cases) %>%
  ggplot(aes(x = `Citizen Race`, y = `UOF: Type force used`, fill = case_dist, label = scales::percent(case_dist, accuracy = 0.1L))) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(size = 3) +
  scale_x_discrete(position = "top") +
  scale_fill_gradientn(colors = carto_pal(7, "PurpOr"), name = "Cases") +
  labs(title = "Distribution of Force Tactics by Race") +
  theme(panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0)
        )


bart_uof.df %>%
  select(`Case #`, `UOF: Type force used`, `Citizen Race`) %>%
  filter(`Citizen Race` %in% c("Black", "White", "Hispanic", "Asian")) %>%
  group_by(`UOF: Type force used`, `Citizen Race`) %>%
  summarize(num_cases = n_distinct(`Case #`)) %>%
  left_join(race_case_dist.df, by = "Citizen Race") %>%
  mutate(case_dist = num_cases / total_cases) %>%
  ggplot(aes(x = `Citizen Race`, y = `UOF: Type force used`, fill = num_cases, label = num_cases)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(size = 3) +
  scale_x_discrete(position = "top") +
  scale_fill_gradientn(colors = carto_pal(7, "PurpOr"), name = "Cases") +
  labs(title = "Distribution of Force Tactics by Race") +
  theme(panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0)
  )



