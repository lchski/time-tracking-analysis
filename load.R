library(tidyverse)

entries <- read_csv("data/source/entries.csv")
entries <- entries %>%
  select(date, client, project, note, start, end, hours)
