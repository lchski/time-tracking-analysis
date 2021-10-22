## movie entries from everywhere
entries %>% filter(str_detect(note, fixed("movie", ignore_case = TRUE)))

apt_movies <- entries %>%
  filter(
    project == "Social",
    str_detect(note, regex("^Apt, movie", ignore_case = TRUE))
  ) %>%
  select(-client, -project) %>%
  mutate(note = str_remove(note, regex("^Apt, movie ", ignore_case = TRUE))) %>%
  filter(note != "Apt, movie") %>% ## filter entries where we know we watched a movie, but sadly don't know which
  separate(note, into = c("movie", "comment"), sep = "\\[", fill = "right") %>%
  mutate_at(c("movie", "comment"), trimws) %>%
  mutate_at(c("movie", "comment"), ~ str_remove(.x, "^\\(|\\]$")) %>%
  mutate(movie = str_remove(movie, "\\)$"))
  
## all apt movies
apt_movies %>%
  select(date, movie) %>%
  mutate(movie_no_punct = str_remove_all(movie, ",")) %>%
  write_csv("data/out/apt_movies.csv")
  

## pandemic apartment movies
apt_movies %>%
  select(date, movie) %>%
  filter(date > "2020-03-13") %>%
  write_csv("data/out/apt_pandemic_movies.csv")
