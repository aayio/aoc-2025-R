library(stringr)

options(scipen = 999)

lines <- readLines("07/input")
start_col <- str_locate(lines[1], "S")[1, "start"]

paths <- numeric(nchar(lines[1]))
paths[start_col] <- 1

split_count <- 0

for (row in 2:length(lines)) {
  new_paths <- numeric(length(paths))

  for (col in which(paths > 0)) {
    if (substr(lines[row], col, col) == "^") {
      split_count <- split_count + 1
      new_paths[col - 1] <- new_paths[col - 1] + paths[col]
      new_paths[col + 1] <- new_paths[col + 1] + paths[col]
    } else {
      new_paths[col] <- new_paths[col] + paths[col]
    }
  }

  paths <- new_paths
}

split_count  # Part 1 = 1533
sum(paths)   # Part 2 = 10733529153890
