entries_work_2021 <- entries_work %>%
  filter(year(date) == 2021)

entries_work_2021_summary <- entries_work_2021 %>%
  mutate(
    grouping = case_when(
      group %in% c(
        "Team",
        "Admin",
        "Evolution",
        "HR"
      ) ~ "Other",
      group == "Policy" ~ paste0(group, "/", subgroup),
      TRUE ~ group
    )
  ) %>%
  group_by(month, grouping) %>%
  summarize(hrs = sum(hours)) %>%
  mutate(hrs_prop = round(hrs / sum(hrs), 2)) %>%
  arrange(month, -hrs_prop)

entries_work_2021_summary %>%
  ggplot(aes(x = month, y = hrs_prop, fill = grouping, color = grouping)) +
  geom_point() +
  facet_wrap(vars(grouping)) +
  scale_x_date(breaks = "1 month", date_labels = "%b")

entries_work_2021_summary %>%
  ungroup %>%
  mutate(month = strftime(month, format = "%B")) %>%
  select(-hrs) %>%
  pivot_wider(names_from = month, values_from = hrs_prop) %>%
  View()
