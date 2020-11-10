#' Force tactics by race
#' 

source("scripts/prep_data.R")
source("utils/analytics.R")

tactics.order <-
  bart_uof.df %>%
  select(`Case #`, `UOF: Type force used`) %>%
  group_by(`UOF: Type force used`) %>%
  summarize(num_cases = n_distinct(`Case #`)) %>%
  arrange(-num_cases) %>%
  mutate(pct_cases = num_cases / 792) %>%
  pull(`UOF: Type force used`)

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
  mutate(case_dist = num_cases / total_cases,
         `UOF: Type force used` = factor(`UOF: Type force used`, levels = rev(tactics.order)),
         `Citizen Race` = factor(`Citizen Race`, levels = c("Black", "White", "Hispanic", "Asian"))) %>%
  ggplot(aes(x = `Citizen Race`, y = `UOF: Type force used`, fill = case_dist, label = scales::percent(case_dist, accuracy = 0.1L))) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(color = case_dist > 0.3), size = 2.5) +
  scale_x_discrete(position = "top") +
  scale_fill_gradientn(colors = carto_pal(7, "PurpOr"), name = "Freq.\nDistribution") +
  scale_color_manual(values = c("black", color.light_gray), guide = FALSE) + 
  labs(title = "Distribution of Force Tactics by Race",
       subtitle = "As percent of cases by race") +
  theme(panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0),
        plot.title.position = "plot"
        
  )

ggsave("output/force_tactics_dist_race.png", dpi = 300, width = 4.5, height = 6.5)



bart_uof.df %>%
  select(`Case #`, `UOF: Type force used`, `Citizen Race`) %>%
  filter(`Citizen Race` %in% c("Black", "White", "Hispanic", "Asian")) %>%
  group_by(`UOF: Type force used`, `Citizen Race`) %>%
  summarize(num_cases = n_distinct(`Case #`)) %>%
  mutate(pct_cases = num_cases / 792,
         `UOF: Type force used` = factor(`UOF: Type force used`, levels = rev(tactics.order)),
         `Citizen Race` = factor(`Citizen Race`, levels = c("Black", "White", "Hispanic", "Asian"))) %>%
  ggplot(aes(x = `Citizen Race`, y = `UOF: Type force used`, fill = pct_cases, label = scales::percent(pct_cases, accuracy = 0.1L))) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(color = num_cases > 150), size = 2.5) +
  scale_x_discrete(position = "top") +
  scale_fill_gradientn(colors = carto_pal(7, "PurpOr"), name = "Freq.\nDistribution") +
  scale_color_manual(values = c("black", color.light_gray), guide = FALSE) + 
  labs(title = "Distribution of Force Tactics by Race",
       subtitle = "As percent of total cases") +
  theme(panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0),
        plot.title.position = "plot"
        
  )

ggsave("output/force_tactics_dist_total_pct.png", dpi = 300, width = 4.5, height = 6.5)


bart_uof.df %>%
  select(`Case #`, `UOF: Type force used`, `Citizen Race`) %>%
  filter(`Citizen Race` %in% c("Black", "White", "Hispanic", "Asian")) %>%
  group_by(`UOF: Type force used`, `Citizen Race`) %>%
  summarize(num_cases = n_distinct(`Case #`)) %>%
  mutate(`UOF: Type force used` = factor(`UOF: Type force used`, levels = rev(tactics.order)),
         `Citizen Race` = factor(`Citizen Race`, levels = c("Black", "White", "Hispanic", "Asian"))) %>%
  ggplot(aes(x = `Citizen Race`, y = `UOF: Type force used`, fill = num_cases, label = num_cases)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(color = num_cases > 150), size = 2.5) +
  scale_x_discrete(position = "top") +
  scale_fill_gradientn(colors = carto_pal(7, "PurpOr"), name = "Cases") +
  scale_color_manual(values = c("black", color.light_gray), guide = FALSE) + 
  labs(title = "Distribution of Force Tactics by Race",
       subtitle = "Total cases") +
  theme(panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0),
        plot.title.position = "plot"
        
  )

ggsave("output/force_tactics_dist_total.png", dpi = 300, width = 4.5, height = 6.5)
