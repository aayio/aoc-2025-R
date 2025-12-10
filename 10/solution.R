library(magrittr)

parse_machine <- function(line) {
  target <- line %>% # "[.##.] (3) ..."
    regexpr("\\[.*?\\]", .) %>% # match object for first (only) match
    regmatches(line, .) %>% # "[.##.]"
    gsub("\\[|\\]", "", .) %>% # ".##."
    strsplit("") %>% # list(c(".", "#", "#", "."))
    .[[1]] %>% # c(".", "#", "#", ".")
    {. == "#"} %>% # c(FALSE, TRUE, TRUE, FALSE)
    as.integer() # c(0, 1, 1, 0)

  buttons <- line %>% # "... (1,2) (3,4,5) ..."
    gregexpr("\\([0-9,]+\\)", .) %>% # match object for all matches
    regmatches(line, .) %>% # list(c("(1,2)", "(3,4,5)"))
    .[[1]] %>% # c("(1,2)", "(3,4,5)")
    lapply(function(btn) {
      btn %>%
        gsub("\\(|\\)", "", .) %>% # "1,2"
        strsplit(",") %>% # list(c("1", "2"))
        .[[1]] %>% # c("1", "2")
        as.integer() # c(1, 2)
    })

  joltages <- line %>% # "... {5,10,3}"
    regexpr("\\{[0-9,]+\\}", .) %>% # match object for first (only) match
    regmatches(line, .) %>% # "{5,10,3}"
    gsub("\\{|\\}", "", .) %>% # "5,10,3"
    strsplit(",") %>% # list(c("5", "10", "3"))
    .[[1]] %>% # c("5", "10", "3")
    as.integer() # c(5, 10, 3)

  list(
    target = target,
    buttons = buttons,
    joltages = joltages
  )
}

min_presses_lights <- function(machine) {
  n_lights <- length(machine$target)
  n_buttons <- length(machine$buttons)

  min_presses <- Inf

  for (combo in 0:(2^n_buttons - 1)) {
    presses <- as.integer(intToBits(combo)[1:n_buttons])

    state <- integer(n_lights)

    for (btn_idx in 1:n_buttons) {
      if (presses[btn_idx] == 1) {
        button <- machine$buttons[[btn_idx]]
        for (light_idx in button) {
          state[light_idx + 1] <-
            1 - state[light_idx + 1] # lights are 0-indexed in the input
        }
      }
    }

    if (all(state == machine$target)) {
      total_presses <- sum(presses)
      if (total_presses < min_presses) {
        min_presses <- total_presses
      }
    }
  }

  # assume a solution will be found
  # if (is.infinite(min_presses)) {
  #   return(NA)
  # }

  min_presses
}

min_presses_joltages <- function(machine) {
  n_counters <- length(machine$joltages)
  n_buttons <- length(machine$buttons)

  coeffs <- matrix(0, nrow = n_counters, ncol = n_buttons)

  # set to 1 if button increments counter
  for (btn_idx in 1:n_buttons) {
    button <- machine$buttons[[btn_idx]]
    for (counter_idx in button) {
      coeffs[counter_idx + 1, btn_idx] <- 1 # counters are 0-indexed in the input
    }
  }

  result <- lpSolve::lp(
    direction = "min",
    objective.in = rep(1, n_buttons),
    const.mat = coeffs,
    const.dir = rep("=", n_counters),
    const.rhs = machine$joltages,
    all.int = TRUE
  )

  if (result$status == 0) {
    return(result$objval)
  } else {
    return(NA)
  }
}

input <- readLines("10/input")

system.time({
  machines <- lapply(input, parse_machine)
})

system.time({
  part1_ans <- machines |>
    sapply(min_presses_lights) |>
    sum()
})

system.time({
  part2_ans <- machines |>
    sapply(min_presses_joltages) |>
    sum()
})

part1_ans # 550
part2_ans # 20042
