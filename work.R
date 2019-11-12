source("load.R")

entries_work <- entries %>%
  filter(project == "Canadian Digital Service") %>%
  select(date, note, hours) %>%
  separate(note, c("group", "activity"), sep = ", ", extra = "merge") %>%
  separate(group, c("group", "subgroup"), sep = "/", extra = "merge")

entries_work %>%
  group_by(group, subgroup) %>%
  summarize(
    count = n(),
    hours = sum(hours),
    first = min(date),
    last = max(date),
    time_between = time_length(interval(first, last), "days")
  ) %>%
  arrange(first, last) %>%
  View()

entries_work %>%
  mutate(
    month = floor_date(date, unit = "month")
  ) %>%
  filter(
    group %in% (entries_work %>%
      group_by(group, subgroup) %>%
      summarize(
        count = n(),
        hours = sum(hours),
        first = min(date),
        last = max(date),
        time_between = time_length(interval(first, last), "days")
      ) %>%
      arrange(first, last) %>%
      filter(count >= 10) %>%
      select(group) %>%
      unique() %>%
      pull())
  ) %>%
  group_by(
    group, month
  ) %>%
  summarize(hours = sum(hours)) %>%
  ggplot(aes(x = month, y = hours, fill = group)) +
  geom_col(position = "fill") +
  scale_fill_brewer(palette = "RdGy")




entries_work %>%
  filter(group %in% (entries_work %>%
    group_by(group, subgroup) %>%
    summarize(
      count = n(),
      hours = sum(hours),
      first = min(date),
      last = max(date),
      time_between = time_length(interval(first, last), "days")
    ) %>%
    arrange(first, last) %>%
    filter(count < 10) %>%
    select(group) %>%
    unique() %>%
    pull())
  )



entries_work_2019_winter <- entries_work %>%
  filter(date > "2019-01-01" & date < "2019-05-01")

entries_work_2019_winter %>%
  group_by(note) %>%
  summarize(
    hours = sum(hours),
    count = n(),
    avg_hours = round(hours / count, digits = 2)
  ) %>%
  arrange(-hours)
