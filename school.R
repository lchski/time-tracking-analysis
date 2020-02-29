source("load.R")

library(skimr)

entries_school <- entries %>%
  filter(project == "School") %>%
  separate(note, c("course", "activity"), sep = ", ", extra = "merge") %>%
  separate(course, c("course", "project"), sep="/", extra = "merge") %>%
  mutate(for_exam = case_when(
    project == "E" ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  select(date, course, project, activity, hours, for_exam)
  
courses_school_2018_winter <- c("HIS2326", "HIS3108", "POL2508", "FLS2771", "POL2103")

courses_school_2018_fall <- c("HIS2300", "HIS2552", "HIS3305", "POL3102", "POL3564")

courses_school_2019_fall <- c("HIS1110", "HIS4100", "POL3537", "POL4554")

entries_school_2018_winter <- entries_school %>%
  filter(date > "2018-01-01" & date < "2018-09-01")

entries_school_2018_fall <- entries_school %>%
  filter(date > "2018-09-01" & date < "2018-12-31")

entries_school_2019_fall <- entries_school %>%
  filter(date > "2019-09-01" & date < "2019-12-31")

hours_by_course_2018_fall <- entries_school_2018_fall %>%
  separate(note, c("course", "activity"), sep = ", ") %>%
  separate(course, c("course", "project"), sep="/") %>% mutate(for_exam = case_when(
    project == "E" ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  select(date, course, project, activity, hours, for_exam)

hours_by_course_2018_winter <- entries_school_2018_winter %>%
  separate(note, c("course", "activity"), sep = ", ") %>%
  separate(course, c("course", "project"), sep="/") %>% mutate(for_exam = case_when(
    project == "E" ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  select(date, course, project, activity, hours, for_exam)

hours_by_course_2019_fall <- entries_school_2019_fall %>%
  separate(note, c("course", "activity"), sep = ", ") %>%
  separate(course, c("course", "project"), sep="/") %>% mutate(for_exam = case_when(
    project == "E" ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  select(date, course, project, activity, hours, for_exam)

# hours_by_course_2018_fall %>% filter(course %in% courses_school_2018_fall) %>% arrange(course, project, activity, date) %>% View()
hours_by_course_2018_fall %>%
  filter(course %in% courses_school_2018_fall) %>%
  group_by(course, project) %>%
  summarize(hours = sum(hours), start = min(date), end = max(date)) %>%
  arrange(course, start, hours) %>%
  View()

hours_by_course_2018_winter %>%
  filter(course %in% courses_school_2018_winter) %>%
  group_by(course, project) %>%
  summarize(hours = sum(hours), start = min(date), end = max(date)) %>%
  arrange(course, start, hours) %>%
  View()

hours_by_course <- hours_by_course_2018_fall %>% full_join(hours_by_course_2018_winter)

hours_by_course %>% select(course) %>% unique()

history_courses <- hours_by_course %>% filter(substr(course, 1, 3) == 'HIS')
polsci_courses <- hours_by_course %>% filter(substr(course, 1, 3) == 'POL')

hours_by_course %>%
  group_by(course) %>%
  summarize(hours = sum(hours), start = min(date), end = max(date)) %>%
  arrange(course, start, hours) %>%
  View()

entries_school_2019_fall %>%
  mutate(date_weekday = wday(date, label = TRUE, week_start = 1)) %>%
  group_by(date, date_weekday) %>%
  summarize(hours = sum(hours)) %>%
  ungroup() %>%
  group_by(date_weekday) %>%
  summarize(
    count = n(),
    hours_tot = sum(hours),
    hours_avg = mean(hours),
    hours_min = min(hours),
    hours_max = max(hours)
  )

library(skimr)
entries_school_2019_fall %>%
  mutate(date_weekday = wday(date, label = TRUE, week_start = 1)) %>%
  group_by(date, date_weekday) %>%
  summarize(hours = sum(hours)) %>%
  ungroup() %>%
  group_by(date_weekday) %>%
  skim()


entries_school %>%
  filter(date > "2018-12-04" & date < "2018-12-12") %>%
  group_by(date) %>%
  skim()



## get a sense of when we're running ragged
school_work_weekly_summary <- entries %>%
  mutate(project = case_when( ## recode for nicer column names
    project == "School" ~ "school",
    project == "Canadian Digital Service" ~ "cds",
    TRUE ~ project
  )) %>%
  mutate(
    week_start = floor_date(date, "1 week", week_start = 1),
    year = year(week_start)
  ) %>%
  filter(project %in% c("school", "cds")) %>%
  filter(year >= 2018) %>% ## get rid of the incomplete months before I got rigorous
  group_by(year, week_start, project) %>%
  summarize(
    hours = sum(hours)
  ) %>%
  pivot_wider(
    names_from = project,
    values_from = hours
  ) %>%
  mutate(
    total = sum(school, cds, na.rm = TRUE)
    ) %>%
  ungroup()

school_work_weekly_summary %>%
  skim()

school_work_weekly_summary %>%
  mutate(full_week = total >= 40) %>%
  View()

school_work_weekly_summary %>%
  mutate(full_week = total >= 40) %>%
  ggplot(aes(x = week_start, y = total)) +
  geom_point(aes(color = full_week))




## look for days worked past 5 PM
school_work_past_5 <- entries %>%
  mutate(project = case_when( ## recode for nicer column names
    project == "School" ~ "school",
    project == "Canadian Digital Service" ~ "cds",
    TRUE ~ project
  )) %>%
  mutate(
    week_start = floor_date(date, "1 week", week_start = 1),
    year = year(week_start)
  ) %>%
  filter(project %in% c("school", "cds")) %>%
  filter(year >= 2018) %>%
  filter(! is.na(start) & ! is.na(end)) %>% ## get rid of entries without a start/end time (ah, my roguish days)
  group_by(year, week_start, date, project) %>%
  nest(entries = c(note, start, end, hours)) %>%
  mutate(
    school_after_5 = map_int(entries, function(etc) {
      etc %>%
        filter(project == "school") %>%
        filter(hour(start) >= 17 | hour(end) > 17) %>%
        count() %>%
        pull(n)
    }) > 0,
    cds_after_5 = map_int(entries, function(etc) {
      etc %>%
        filter(project == "cds") %>%
        filter(hour(start) >= 17 | hour(end) > 17) %>%
        count() %>%
        pull(n)
    }) > 0
  )
