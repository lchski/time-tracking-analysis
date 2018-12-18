library(tidyverse)

source("load.R")

entries_school <- entries %>%
  filter(project == "School")

courses_school_2018_fall <- c("HIS2300", "HIS2552", "HIS3305", "POL3102", "POL3564")

entries_school_2018_fall <- entries_school %>%
  filter(date > "2018-09-01")

hours_by_course_2018_fall <- entries_school_2018_fall %>%
  separate(note, c("course", "activity"), sep = ", ") %>%
  separate(course, c("course", "project"), sep="/") %>% mutate(for_exam = case_when(
    project == "E" ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  select(date, course, project, activity, hours, for_exam)

# hours_by_course_2018_fall %>% filter(course %in% courses_school_2018_fall) %>% arrange(course, project, activity, date) %>% View()
hours_by_course_2018_fall %>% filter(course %in% courses_school_2018_fall) %>% group_by(course, project) %>% summarize(hours = sum(hours)) %>% View()

history_courses <- hours_by_course %>% filter(substr(course, 1, 3) == 'HIS')
polsci_courses <- hours_by_course %>% filter(substr(course, 1, 3) == 'POL')

hours_by_course %>% select(course) %>% unique()
