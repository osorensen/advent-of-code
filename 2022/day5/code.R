library(tidyverse)

crates <- vector(mode = "list", length = 9)
setup <- rev(read_lines("2022/day5/input.txt", n_max = 8))
instructions <- read_lines("2022/day5/input.txt", skip = 10)

for(i in seq_along(setup)){
  re <- "(?<=[:space:])[:space:]{3}(?=[:space:])"
  
  # Interior
  while(str_detect(setup[[i]], re)){
    setup[[i]] <- str_replace(setup[[i]], re, "none")
  }
  # Right boundary
  re <- "(?<=[:space:])[:space:]{3}$"
  while(str_detect(setup[[i]], re)){
    setup[[i]] <- str_replace(setup[[i]], re, "none")
  }
  # Left boundary
  re <- "^[:space:]{3}(?=[:space:])"
  while(str_detect(setup[[i]], re)){
    setup[[i]] <- str_replace(setup[[i]], re, "none")
  }
  
  a <- strsplit(setup[[i]], " ")[[1]]
  for(j in seq_along(a)){
    crates[[j]] <- c(crates[[j]], a[[j]])
  }
}

crates <- map(crates, function(x){
  x <- x[x != "none"]
})


instructions <- map_dfr(instructions, function(x){
  amount <- as.integer(str_extract(x, "(?<=move )[:digit:]+"))
  from <- as.integer(str_extract(x, "(?<=from )[:digit:]+"))
  to <- as.integer(str_extract(x, "(?<=to )[:digit:]+"))
  
  tibble(
    amount = amount, from = from, to = to
  )
})

part <- 2
# Part 1
if(part == 1){
  for(i in seq_len(nrow(instructions))){
    instr <- instructions[i, ]
    if(min(length(crates[[instr$from]])) > 0){
      ins <- rev(crates[[instr$from]])[seq(from = 1, to = min(length(crates[[instr$from]]), instr$amount), by = 1)]
    } else {
      ins <- character()
    }
    
    crates[[instr$to]] <- c(crates[[instr$to]], ins)
    crates[[instr$from]] <- c(integer(0), crates[[instr$from]][-rev(seq(from = length(crates[[instr$from]]), length.out = length(ins), by = -1))])
  }
  paste(map_chr(crates, ~ str_extract(.x[[length(.x)]], "[:alpha:]")), collapse = "")
}

# Part 2
for(i in seq_len(nrow(instructions))){
  instr <- instructions[i, ]
  if(min(length(crates[[instr$from]])) > 0){
    ins <- rev(rev(crates[[instr$from]])[seq(from = 1, to = min(length(crates[[instr$from]]), instr$amount), by = 1)])
  } else {
    ins <- character()
  }
  
  crates[[instr$to]] <- c(crates[[instr$to]], ins)
  crates[[instr$from]] <- c(integer(0), crates[[instr$from]][-rev(seq(from = length(crates[[instr$from]]), length.out = length(ins), by = -1))])
}

paste(map_chr(crates, ~ str_extract(.x[[length(.x)]], "[:alpha:]")), collapse = "")
