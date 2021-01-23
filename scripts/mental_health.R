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
  write.csv("output/mental_health_correlated_charges.csv", row.names = FALSE)

#case distribution by race
bart_uof.df %>%
  select(`Case #`, `Citizen Race`) %>%
  filter(`Case #` %in% cases.filter) %>%
  mutate(`Citizen Race` = str_replace_all(`Citizen Race`, c("Mid. Eastern" = "Other",
                                                            "Pac Islander" = "Other",
                                                            "Asian" = "Other",
                                                            "Unknown" = "Other"))) %>%
  group_by(`Citizen Race`) %>%
  summarize(num_cases = n_distinct(`Case #`)) %>%
  arrange(-num_cases) %>%
  mutate(pct_cases = num_cases / as.numeric(case_total.val),
         `Citizen Race` = factor(`Citizen Race`, levels = c("Black", "Hispanic", "White", "Other"))) %>%
  ggplot(aes(x = reorder(`Citizen Race`, pct_cases), y = pct_cases)) +
  geom_bar(aes(fill = `Citizen Race`), stat = "identity", alpha = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.25) +
  scale_y_continuous(position = "right", labels = scales::percent, limits = c(0, 0.7), breaks = scales::pretty_breaks(n = 4)) +
  scale_fill_manual(values =  carto_pal(7, name = "Vivid")) +
  coord_flip() +
  labs(title = "Distribution of cases by race",
       subtitle = "Involving mental health charges") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title.position = "plot",
        legend.position = "none"
  )

ggsave("output/mental_health_cases_by_race.png", dpi = 300, width = 5, height = 3)


#age distribution by race
#absolute
bart_uof.df %>%
  select(`Case #`, `Citizen Race`, citizen_age) %>%
  filter(`Case #` %in% cases.filter) %>%
  unique() %>%
  filter(`Citizen Race` %in% c("Black", "White", "Hispanic")) %>%
  ggplot(aes(x = citizen_age, fill = `Citizen Race`)) +
  geom_histogram(position = "identity", binwidth = 5, alpha = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.25) +
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  scale_fill_manual(values = carto_pal(7, "Vivid")) +
  labs(title = "Age distribution of cases involving mental health",
       subtitle = "",
       y = "Cases",
       x = "Citizen Age") +
  theme(plot.title.position = "plot")

ggsave("output/mental_health_age_distribution_by_race.png", dpi = 300, width = 4.5, height = 3.5)

#relative to race
bart_uof.df %>%
  select(`Case #`, `Citizen Race`, citizen_age) %>%
  filter(`Case #` %in% cases.filter) %>%
  unique() %>%
  filter(`Citizen Race` %in% c("Black", "White", "Hispanic")) %>%
  ggplot(aes(x = citizen_age, fill = `Citizen Race`, y = after_stat(density))) +
  geom_histogram(position = "identity", binwidth = 5, alpha = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.25) +
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = carto_pal(7, "Vivid")) +
  labs(title = "Age distribution of cases involving mental health",
       subtitle = "",
       y = "% of Cases",
       x = "Citizen Age") +
  theme(plot.title.position = "plot")

ggsave("output/mental_health_age_distribution_by_race_pct.png", dpi = 300, width = 4.5, height = 3.5)



#mental health force tactics used
tactics.order <-
  bart_uof.df %>%
  select(`Case #`, `UOF: Type force used`) %>%
  filter(`Case #` %in% cases.filter) %>%
  group_by(`UOF: Type force used`) %>%
  summarize(num_cases = n_distinct(`Case #`)) %>%
  arrange(-num_cases) %>%
  mutate(pct_cases = num_cases / as.numeric(case_total.val)) %>%
  pull(`UOF: Type force used`)

bart_uof.df %>%
  select(`Case #`, `UOF: Type force used`, `Citizen Race`) %>%
  filter(`Citizen Race` %in% c("Black", "White", "Hispanic"),
         `Case #` %in% cases.filter) %>%
  group_by(`UOF: Type force used`, `Citizen Race`) %>%
  summarize(num_cases = n_distinct(`Case #`)) %>%
  mutate(pct_cases = num_cases / as.numeric(case_total.val),
         `UOF: Type force used` = factor(`UOF: Type force used`, levels = rev(tactics.order)),
         `Citizen Race` = factor(`Citizen Race`, levels = c("Black", "White", "Hispanic", "Asian"))) %>%
  ggplot(aes(x = `Citizen Race`, y = `UOF: Type force used`, fill = pct_cases, label = scales::percent(pct_cases, accuracy = 0.1L))) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(color = num_cases > 30), size = 2.5) +
  scale_x_discrete(position = "top") +
  scale_fill_gradientn(colors = carto_pal(7, "PurpOr"), name = "Freq.\nDistribution") +
  scale_color_manual(values = c("black", color.light_gray), guide = FALSE) + 
  labs(title = "Distribution of Force Tactics by Race",
       subtitle = "As percent of cases involving mental health disorders") +
  theme(panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0),
        plot.title.position = "plot"
        
  )

ggsave("output/mental_health_force_tactics_dist_race.png", dpi = 300, width = 4.5, height = 6.5)


#station distribution
bart_uof.df %>%
  select(`Case #`, `County/location`) %>%
  filter(`Case #` %in% cases.filter) %>%
  group_by(`County/location`) %>%
  summarize(num_cases = n_distinct(`Case #`)) %>%
  arrange(-num_cases) %>%
  View(.)

bart_uof.df %>%
  select(`Case #`, `County/location`) %>%
  filter(`Case #` %in% cases.filter) %>%
  group_by(`County/location`) %>%
  summarize(num_cases = n_distinct(`Case #`)) %>%
  ggplot(aes(x = reorder(`County/location`, num_cases), y = num_cases)) +
  geom_bar(stat = "identity", fill = carto_pal(name = "Vivid")[1], alpha = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.25) +
  scale_y_continuous(position = "right", limits = c(0,20), breaks = scales::pretty_breaks(n = 4)) +
  coord_flip() +
  labs(title = "Cases by station",
       subtitle = "Involving mental health disorders") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title.position = "plot"
  )

ggsave("output/mental_health_cases_by_station.png", dpi = 300, width = 5, height = 7)


#time of day, day of week
bart_uof.df %>%
  filter(!is.na(incident_timestamp),
         `Case #` %in% cases.filter) %>%
  select(`Case #`, incident_timestamp) %>%
  unique() %>%
  mutate(hour = hour(incident_timestamp),
         dow = wday(incident_timestamp, label = TRUE, abbr = TRUE)) %>%
  group_by(hour, dow) %>%
  summarise(num_cases = n_distinct(`Case #`)) %>%
  ggplot(aes(x = hour, y = dow, fill = num_cases, label = num_cases)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(color = num_cases > 3), size = 2.5) +
  scale_x_continuous(breaks = seq(0,23, by = 1)) +
  scale_fill_gradientn(colors = carto_pal(7, "PurpOr"), name = "Cases") +
  scale_color_manual(values = c("black", color.light_gray), guide = FALSE) + 
  labs(x = "Hour of Day",
       title = "Cases by hour of day and day of week",
       subtitle = "Involving mental health disorders") +
  theme(panel.grid.major = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 5)),
        plot.title.position = "plot")


ggsave("output/mental_health_cases_by_hoddow.png", dpi = 300, width = 8, height = 3)

