library(slider)

tt_hours <- entries %>%
  filter(client == "Lucas Cherkewski") %>%
  filter(project %in% c(
    "Canadian Digital Service",
    "Me",
    "School",
    "Social"
    # "Apartment",
    # "lucascherkewski.com"
  )) %>%
  group_by(month = floor_date(date, "3 months"), project) %>%
  summarize(hours = sum(hours)) %>%
  group_by(month) %>%
  mutate(hours_pct = hours / sum(hours)) %>%
  pivot_wider(id_cols = month, names_from = project, values_from = hours_pct) %>%
  mutate_at(vars(-month), ~ if_else(is.na(.x), 0, .x)) %>%
  pivot_longer(cols = -month, names_to = "project", values_to = "hours_pct") %>%
  mutate(project = as_factor(project) %>% fct_relevel(
    "School",
    "Me",
    "Canadian Digital Service",
    "Social"
  ))

tt_hours %>%
  ggplot(aes(x = month, y = hours_pct, color = project, fill = project)) +
  geom_point() +
  geom_line()

tt_hours %>%
  filter(month >= floor_date(today() - years(2), "1 month")) %>%
  ggplot(aes(x = month, y = hours_pct, color = project, fill = project)) +
  # geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE, size = 1.5) +
  scale_color_grey() +
  theme_void()

tt_hours_area_plot <- tt_hours %>%
  filter(month >= floor_date(today() - years(2), "1 month")) %>%
  ggplot(aes(x = month, y = hours_pct, color = project, fill = project)) +
  stat_smooth(
    geom = "area",
    method = "loess",
    span = 6/10,
    position = "stack"
  ) +
  scale_fill_brewer(palette = "BuPu") +
  scale_color_brewer(palette = "BuPu") +
  scale_y_continuous(limits = c(0, 1.01)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.ticks.length = unit(0, "pt"), #length of tick marks
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.margin = margin(0, 0, 0, 0, "pt"),
        plot.caption = element_blank()) +
  guides(color = FALSE, linetype = FALSE, fill = FALSE)

tt_hours_area_plot

tt_hours_area_plot +
  ggsave(
    filename = "data/out/tt_hours.eps",
    bg = "transparent",
    width = 10,
    height = 10
  )

tt_hours_area_plot +
  ggsave(
    filename = "data/out/tt_hours.png",
    bg = "transparent",
    width = 10,
    height = 10
  )

## just website hours (for footer)
tt_hours %>%
  filter(project == "lucascherkewski.com") %>%
  filter(month >= floor_date(today() - years(2), "1 month")) %>%
  ggplot(aes(x = month, y = hours_pct, color = project, fill = project)) +
  # geom_point(show.legend = FALSE) +
  geom_area(show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_fill_brewer(palette = "BuPu") +
  scale_color_brewer(palette = "BuPu") +
  theme_void() +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.ticks.length = unit(0, "pt"), #length of tick marks
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.margin = margin(0, 0, 0, 0, "pt"),
        plot.caption = element_blank()) +
  guides(color = FALSE, linetype = FALSE, fill = FALSE)


tt_hours %>%
  filter(month >= floor_date(today() - years(2), "1 month")) %>%
  # mutate(project = factor(c("Me", "Social", "School", "Canadian Digital Service"))) %>% ## TODO verify this isn't just renaming the existing groups
  mutate(project = as_factor(project) %>% fct_relevel(
    "Canadian Digital Service",
    "School",
    "Social",
    "Me"
  )) %>%
  ggplot(aes(x = month, y = hours_pct, color = project, fill = project)) +
  stat_smooth(
    geom = "area",
    method = "loess",
    span = 6/10,
    position = "stack"
  ) +
  scale_fill_brewer(palette = "BuPu") +
  scale_color_brewer(palette = "BuPu") +
  scale_y_continuous(limits = c(0, 1.01))

