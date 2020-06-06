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





# to plot grouped work entries, rolling up RCMP and non Policy/non RCMP
entries_work_to_plot <- entries_work %>%
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
    ! group %in% c("Policy", "RCMP", "Forms") ~ "zzOther",
    group == "RCMP" ~ "zRCMP",
    group == "Forms" ~ "zForms",
    subgroup %in% c("B19", "B20") ~ "B",
    TRUE ~ subgroup
  )) %>%
  group_by(
    subgroup, month
  ) %>%
  summarize(hours = sum(hours)) %>%
  ungroup() %>%
  group_by(month) %>%
  mutate(hours_pct = hours / sum(hours)) %>%
  arrange(month, -hours_pct)

plot_work_hours <- function(entries_to_plot, col_position = "stack") {
  entries_to_plot %>%
    ggplot(aes(x = month, y = hours, fill = subgroup)) +
    geom_col(position = col_position) +
    scale_fill_brewer(palette = "RdGy") +
    scale_x_date(
      limits = c(date("2018-04-01"), ceiling_date(today(), "months") - days(1)),
      date_breaks = "2 months",
      date_labels = "%b %g"
    )
}

## scaled 0-1 (`position = "fill"`)
entries_work_to_plot %>% plot_work_hours("fill")

## by number of hours
entries_work_to_plot %>% plot_work_hours("stack")



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


fy_201920_entries <- entries_work %>%
  filter(date >= "2019-04-01") %>%
  filter(date <= "2020-03-31")

fy_201920_entries %>%
  mutate_at(vars(group, subgroup, activity), ~ str_remove_all(.x, fixed("-"))) %>%
  mutate_at(vars(group, subgroup, activity), ~ str_remove_all(.x, fixed("ing "))) %>%
  group_by(group, subgroup, activity) %>%
  summarize(
    earliest = min(date),
    latest = max(date),
    count = n(),
    total = sum(hours)
  ) %>% View()

