library(tidyverse)
dat <- read_delim("2022/day3/input.txt", col_names = "items", delim = " ")

score <- 1:52
names(score) <- c(letters, LETTERS)

# Part 1
dat %>% 
  mutate(
    length = str_length(items),
    part1 = substr(items, start = 1, stop = floor(length/2)),
    part2 = substr(items, start = floor(length/2) + 1L, stop = length),
    common = map2_chr(part1, part2, function(x, y){
      intersect(
        strsplit(x, split = "")[[1]],
        strsplit(y, split = "")[[1]]
      )
    }),
    score = score[common]
  ) %>% 
  summarise(
    sum = sum(score)
  )

# Part 2
dat %>% 
  mutate(
    id = rep(1:100, each = 3)
  ) %>% 
  nest_by(id) %>% 
  pmap_dfr(function(id, data){
    x1 <- strsplit(data$items[[1]], "")[[1]]
    x2 <- strsplit(data$items[[2]], "")[[1]]
    x3 <- strsplit(data$items[[3]], "")[[1]]
    tibble(
      id = id,
      common = intersect(x3, intersect(x1, x2))
    )
  }) %>% 
  mutate(
    score = score[common]
  ) %>% 
  summarise(sum = sum(score))
