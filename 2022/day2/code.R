library(tidyverse)

# Part 1
dat <- read_delim("2022/day2/input.txt", delim = " ", col_names = c("elf", "me"))

dat %>% 
  mutate(
    elf = recode(elf, "A" = "rock", "B" = "paper", "C" = "scissors"),
    me = recode(me, "X" = "rock", "Y" = "paper", "Z" = "scissors"),
    score1 = case_when(
      me == "rock" ~ 1,
      me == "paper" ~ 2,
      me == "scissors" ~ 3
    ),
    score2 = case_when(
      (me == "rock" & elf == "paper") | (me == "paper" & elf == "scissors") |
        (me == "scissors" & elf == "rock") ~ 0,
      me == elf ~ 3,
      TRUE ~ 6
    ),
    score = score1 + score2
  ) %>% 
  summarise(
    score = sum(score)
  )

# Part 2
mapping <- tibble(
  wins = c("rock", "paper", "scissors"),
  loses = c("scissors", "rock", "paper")
)

dat %>% 
  rename(outcome = me) %>% 
  mutate(
    elf = recode(elf, "A" = "rock", "B" = "paper", "C" = "scissors"),
    outcome = recode(outcome, "X" = "loss", "Y" = "draw", "Z" = "win"),
    me = case_when(
      outcome == "loss" ~ map_chr(elf, ~ mapping[mapping$wins == .x, "loses"][[1]]),
      outcome == "win" ~ map_chr(elf, ~ mapping[mapping$loses == .x, "wins"][[1]]),
      outcome == "draw" ~ elf
    ),
    score1 = case_when(
      me == "rock" ~ 1,
      me == "paper" ~ 2,
      me == "scissors" ~ 3
    ),
    score2 = case_when(
      (me == "rock" & elf == "paper") | (me == "paper" & elf == "scissors") |
        (me == "scissors" & elf == "rock") ~ 0,
      me == elf ~ 3,
      TRUE ~ 6
    ),
    score = score1 + score2
  ) %>% 
  summarise(
    score = sum(score)
  )
