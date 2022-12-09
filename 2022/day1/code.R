library(tidyverse)
dat <- read_lines("2022/day1/input.txt")
split_points <- map_lgl(dat, ~ .x == "")

bind_cols(
  new = split_points,
  calories = as.numeric(dat)
) %>% 
  mutate(
    id = cumsum(new) + 1L
  ) %>% 
  filter(!new) %>% 
  group_by(id) %>% 
  summarise(
    total = sum(calories),
    .groups = "drop"
  ) %>% 
  filter(total == max(total))
