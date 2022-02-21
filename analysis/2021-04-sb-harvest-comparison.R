library(janitor)

entries_harvest <- read_csv("data/source/harvest_time_report_from2021-03-01to2021-03-31.csv") %>%
  clean_names %>%
  select(date, group = project, subgroup = task, activity = notes, hours)

## data entry errors (missing block category)
entries_harvest %>%
  filter(is.na(activity))
