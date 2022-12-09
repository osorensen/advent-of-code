library(tidyverse)
dat <- read_delim("2022/day4/input.txt", col_names = c("elf1", "elf2"), delim = ",")

# Part 1
dat %>% 
  mutate(
    contained = 
      (as.integer(str_extract(elf1, "^[:digit:]+")) <= as.integer(str_extract(elf2, "^[:digit:]+")) &
      as.integer(str_extract(elf1, "[:digit:]+$")) >= as.integer(str_extract(elf2, "[:digit:]+$"))) |
      (as.integer(str_extract(elf2, "^[:digit:]+")) <= as.integer(str_extract(elf1, "^[:digit:]+")) &
         as.integer(str_extract(elf2, "[:digit:]+$")) >= as.integer(str_extract(elf1, "[:digit:]+$")))
  ) %>% 
  summarise(sum(contained))
  
# Part 2
dat %>% 
  mutate(
    range1 = map(elf1, ~ seq(from = as.integer(str_extract(.x, "^[:digit:]+")),
                             to = as.integer(str_extract(.x, "[:digit:]+$")))),
    range2 = map(elf2, ~ seq(from = as.integer(str_extract(.x, "^[:digit:]+")),
                             to = as.integer(str_extract(.x, "[:digit:]+$")))),
    overlap = map2_lgl(range1, range2, function(x, y){
      length(intersect(x, y)) != 0
    })
  ) %>% 
  summarise(sum(overlap))

  