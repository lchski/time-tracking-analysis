en_dates <- entries_work %>%
  filter(group == "Policy") %>%
  filter(subgroup == "EN") %>%
  pull(date) %>%
  unique()

en_hours_by_date <- entries_work %>%
  filter(date %in% en_dates) %>%
  filter(group == "Policy") %>%
  filter(subgroup == "EN") %>%
  group_by(date) %>%
  summarize(hours_en = sum(hours))

en_days_summary <- entries_work %>%
  filter(date %in% en_dates) %>%
  group_by(date) %>%
  summarize(hours = sum(hours)) %>%
  mutate(date_wday = wday(date, week_start = 1, label = TRUE)) %>%
  mutate(is_weekend = date_wday %in% c("Sat", "Sun")) %>%
  left_join(en_hours_by_date) %>%
  mutate(pct_hours_en = hours_en / hours) %>%
  select(date, date_wday, is_weekend, everything())

en_days_summary %>% mutate(pct_hours_en = round(pct_hours_en, 1)) %>% ggplot(aes(x = date, y = pct_hours_en)) + geom_point()

en_ot_days <- en_days_summary %>%
  filter(date < "2020-07-01") %>%
  filter(is_weekend | hours > 8) %>%
  mutate(ot_multiplier = case_when(
    date_wday == "Sun" ~ 2,
    date_wday == "Sat" ~ 1.5,
    TRUE ~ 1.5
  )) %>%
  select(date, date_wday, is_weekend, ot_multiplier, everything()) %>%
  mutate(pct_hours_en = round(pct_hours_en, 1)) %>%
  mutate(is_eligible_ot = case_when(
    is_weekend ~ TRUE,
    pct_hours_en >= 0.6 ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  filter(is_eligible_ot) %>%
  remove_extra_columns() %>%
  mutate(ot_hours_raw = if_else(
    is_weekend,
    hours_en,
    hours - 7.5
  )) %>%
  mutate(ot_hours_calc = ot_hours_raw * ot_multiplier)

## OT hours by OT type (1.5x / 2x)
en_ot_days_summary <- en_ot_days %>%
  group_by(ot_multiplier) %>%
  summarize(hours_raw = sum(ot_hours_raw), hours_calc = sum(ot_hours_calc))

## OT hours total
en_ot_days_summary %>% summarize(ot_hours_raw = sum(hours_raw))

## OT hours for reporting
en_ot_days_report <- en_ot_days %>%
  select(date, date_wday, hours_worked = hours, ot_hours_raw, ot_multiplier, ot_hours_calc)

en_ot_days_report %>% write_csv("data/out/2020-06-30-cds-en-ot.csv")
