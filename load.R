library(tidyverse)
library(lubridate)

entries <- tibble(path = fs::dir_ls("data/source/quarters/", regexp = "\\.csv$")) %>%
  pull(path) %>%
  map_dfr(
    read_csv,
    col_types = cols(
      date = col_date(format = ""),
      client = col_character(),
      project = col_character(),
      note = col_character(),
      billable = col_logical(),
      billed = col_logical(),
      rate = col_logical(),
      start = col_time(format = ""),
      end = col_time(format = ""),
      hours = col_double()
    )
  ) %>%
  select(date, client, project, note, start, end, hours)
