library(tidyverse)

source("load.R")

entries_work <- entries %>%
  filter(project == "Canadian Digital Service") %>%
  select(date, note, hours)

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
