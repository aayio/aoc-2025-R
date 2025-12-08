library(readr)
library(dplyr)

options(scipen = 999)

coords <- read_csv("08/input", col_names = c("x", "y", "z"), show_col_types = FALSE)

system.time({
  n <- nrow(coords)

  pairs <- expand.grid(i = 1:n, j = 1:n) |>
    filter(i < j)

  pairs <- pairs |>
    mutate(
      dist = sqrt(
        (coords$x[i] - coords$x[j])^2 +
        (coords$y[i] - coords$y[j])^2 +
        (coords$z[i] - coords$z[j])^2
      )
    ) |>
    arrange(dist)

  # Every box starts off as its own circuit
  circuit <- 1:n

  join_circuits <- function(box1, box2) {
    circuit1 <- circuit[box1]
    circuit2 <- circuit[box2]

    # do nothing and return false if already on same circuit
    if (circuit1 == circuit2) {
      return(FALSE)
    }

    # everything which was on circuit2 goes to circuit1
    circuit[circuit == circuit2] <<- circuit1

    return(TRUE)
  }

  # Clarification of problem wording: even if you don't make a connection
  # because the boxes are already on the same circuit, it still counts
  # towards the 1000 connections you have to 'make'

  # part 1
  for (k in 1:1000) {
    i <- pairs$i[k]
    j <- pairs$j[k]
    join_circuits(i, j)
  }

  # Find the size of each circuit
  circuit_sizes <- table(circuit) |>
    sort(decreasing = TRUE)

  part1_ans <- circuit_sizes |> head(3) |> prod()

  # part 2
  for (k in 1001:nrow(pairs)) {
    i <- pairs$i[k]
    j <- pairs$j[k]

    if (join_circuits(i, j)) {
      if (length(unique(circuit)) == 1) {
        last_i <- i
        last_j <- j
        break
      }
    }
  }

  part2_ans <- coords$x[last_i] * coords$x[last_j]
})

part1_ans # 330786

part2_ans # 3276581616
