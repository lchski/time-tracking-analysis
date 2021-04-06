source("work.R")

entries_work_2020 <- entries_work %>%
  filter(year(date) == 2020)

entries_work_2020 %>%
  group_by(date) %>%
  summarize(hours = sum(hours, na.rm = TRUE)) %>%
  mutate(wday = wday(date, label = TRUE)) %>% 
  filter(date > "2020-03-13") %>%
  filter(hours > 1) %>%
  View()
  
