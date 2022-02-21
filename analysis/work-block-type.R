entries_to_tag <- entries_work

# to compare with SB Harvest
entries_to_tag <- entries_harvest

entries_to_tag <- (entries_harvest %>% mutate(person = "Sean")) %>%
  bind_rows(entries_work %>% mutate(person = "Lucas")) %>%
  filter(date >= as_date("2021-03-01"), date < as_date("2021-04-01"))

tag_entries_by_work_block_type <- function(entries) {
  entries %>%
    separate(activity, into = c("activity", "block_type"), sep = "\\{") %>%
    mutate(block_type = str_remove(block_type, fixed("}"))) %>%
    mutate(block_type = case_when(
      block_type == "hd" ~ "heads-down",
      block_type == "ms" ~ "meeting (1:1)",
      block_type == "mm" ~ "meeting (3+, camera on)",
      block_type == "ml" ~ "meeting (large, passive participant)"
    ))
}

tagged_work_blocks <- entries_to_tag %>%
  tag_entries_by_work_block_type() %>%
  mutate(week = floor_date(date, "1 week", week_start = 1))

tagged_work_blocks_summary <- tagged_work_blocks %>%
  group_by(person, week, block_type) %>%
  summarize(hrs = sum(hours)) %>%
  mutate(hrs_prop = hrs / sum(hrs))

tagged_work_blocks_summary %>%
  ggplot(aes(x = week, y = hrs, fill = block_type)) +
  geom_col(position = "fill") +
  ylab("% of time") +
  facet_grid(cols = vars(person))

tagged_work_blocks_summary %>%
  filter(! is.na(block_type)) %>%
  ggplot(aes(x = week, y = hrs_prop, fill = block_type)) +
  geom_line() +
  facet_grid(vars(person), vars(block_type)) +
  ylim(c(0,1)) +
  ylab("% of time") +
  theme(axis.text.x = element_text(angle = 90))


