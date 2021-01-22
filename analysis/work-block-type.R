tagged_work_blocks <- entries_work %>%
  filter(year(date) >= 2021) %>%
  separate(activity, into = c("activity", "block_type"), sep = "\\{") %>%
  mutate(block_type = str_remove(block_type, fixed("}"))) %>%
  mutate(week = floor_date(date, "1 week", week_start = 1))

tagged_work_blocks %>%
  mutate(block_type = case_when(
    block_type == "hd" ~ "heads-down",
    block_type == "ms" ~ "meeting (1:1)",
    block_type == "mm" ~ "meeting (3+, camera on)",
    block_type == "ml" ~ "meeting (large, passive participant)"
  )) %>%
  group_by(week, block_type) %>%
  summarize(hrs = sum(hours)) %>%
  ggplot(aes(x = week, y = hrs, fill = block_type)) +
  geom_col(position = "fill") +
  ylab("% of time") +
  labs(title = "Lucas’s weeks")
