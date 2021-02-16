float_entries <- entries_work %>%
  filter(year(date) >= 2020) %>%
  mutate(
    group = if_else(
      ! group %in% c("Policy", "RCMP", "Forms", "Notify"),
      "Other",
      group
    )
  ) %>%
  mutate(
    group = if_else(! is.na(subgroup) & subgroup == "EN", "EN", group),
    group = if_else(! is.na(subgroup) & subgroup == "PMI", "PMI", group)
  ) %>%
  group_by(month = floor_date(date, "1 month"), group) %>%
  summarize(
    hrs = sum(hours)
  ) %>%
  mutate(
    pct = round(hrs / sum(hrs), 2)
  )

float_monthly_summary <- float_entries %>%
  pivot_longer(
    cols = hrs:pct,
    names_to = "key"
  ) %>%
  pivot_wider(
    names_from = group
  ) %>%
  select(month, key, Policy, EN, Forms, PMI, RCMP, Notify, Other) %>%
  ungroup() %>%
  mutate(
    Total = select(., Policy:Other) %>% rowSums(na.rm = TRUE)
  )

float_monthly_summary %>%
  mutate(hrs_total = lag(Total)) %>%
  filter(key == "pct") %>%
  select(-Total) %>%
  mutate(month = strftime(month, format = "%B %Y"))
