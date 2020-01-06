source("load.R")

entries_work <- entries %>%
  filter(project == "Canadian Digital Service") %>%
  select(date, note, hours) %>%
  separate(note, c("group", "activity"), sep = ", ", extra = "merge") %>%
  separate(group, c("group", "subgroup"), sep = "/", extra = "merge") %>%
  mutate(
    month = floor_date(date, unit = "month")
  )

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
  filter(group %in% c("RCMP")) %>%
  group_by(
    subgroup, month
  ) %>%
  summarize(hours = sum(hours)) %>%
  ggplot(aes(x = month, y = hours, fill = subgroup)) +
  geom_col(position = "fill") +
  scale_fill_brewer(palette = "RdGy")





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
  mutate(subgroup = case_when(
    group != "Policy" & group != "RCMP" ~ "zzOther",
    group == "RCMP" ~ "zRCMP",
    TRUE ~ subgroup
  )) %>%
  group_by(
    subgroup, month
  ) %>%
  summarize(hours = sum(hours)) %>%
  ggplot(aes(x = month, y = hours, fill = subgroup)) +
  geom_col(position = "fill") +
  scale_fill_brewer(palette = "RdGy") +
  scale_x_date(
    limits = c(date("2018-04-01"), date("2019-12-01")),
    date_breaks = "2 months",
    date_labels = "%b %g"
  )

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
  mutate(subgroup = case_when(
    group != "Policy" & group != "RCMP" ~ "zzOther",
    group == "RCMP" ~ "zRCMP",
    TRUE ~ subgroup
  )) %>%
  group_by(
    subgroup, month
  ) %>%
  summarize(hours = sum(hours)) %>%
  ggplot(aes(x = month, y = hours, fill = subgroup)) +
  geom_col() +
  scale_fill_brewer(palette = "RdGy") +
  scale_x_date(
    limits = c(date("2018-04-01"), date("2019-12-01")),
    date_breaks = "2 months",
    date_labels = "%b %g"
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

entries_work %>%
  filter(group == "RCMP") %>%
  group_by(month, subgroup, activity) %>%
  summarize(
    earliest = min(date),
    latest = max(date),
    count = n(),
    total = sum(hours)
  ) %>% View()
