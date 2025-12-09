library(readr)
library(dplyr)

tiles <- read_csv("09/input", col_names = c("x", "y"), show_col_types = FALSE)

n <- nrow(tiles)

# part 1
system.time({
  max_area <- 0

  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      width <- abs(tiles$x[i] - tiles$x[j]) + 1 # because it's inclusive
      height <- abs(tiles$y[i] - tiles$y[j]) + 1
      area <- width * height

      if (area > max_area) {
        max_area <- area
      }
    }
  }

  part1_ans <- max_area
})

# part 2
system.time({
  all_edges <- tibble(
    x1 = tiles$x,
    y1 = tiles$y,
    x2 = c(tiles$x[-1], tiles$x[1]),
    y2 = c(tiles$y[-1], tiles$y[1])
  )

  v_edges <- all_edges |>
    filter(x1 == x2) |>
    mutate(
      x = x1,
      ymin = pmin(y1, y2),
      ymax = pmax(y1, y2)
    ) |>
    select(x, ymin, ymax)

  h_edges <- all_edges |>
    filter(y1 == y2) |>
    mutate(
      y = y1,
      xmin = pmin(x1, x2),
      xmax = pmax(x1, x2)
    ) |>
    select(y, xmin, xmax)

  are_points_inside <- function(px, py) {
    # px, py are vectors of each coord for all points
    # ray casting to the right
    # odd number of crossings = point is inside
    sapply(seq_along(px), function(i) {
      crossings <- sum(v_edges$x > px[i] & v_edges$ymin <= py[i] & v_edges$ymax > py[i])
      crossings %% 2 == 1
    })
  }

  pairs <- expand.grid(i = 1:(n-1), j = 1:n) |>
    filter(i < j) |> # unique pairs
    mutate(
      x1 = tiles$x[i],
      y1 = tiles$y[i],
      x2 = tiles$x[j],
      y2 = tiles$y[j]
    )

  inferred_corners_inside <- are_points_inside(pairs$x1, pairs$y2) & are_points_inside(pairs$x2, pairs$y1)

  pairs <- pairs |>
    filter(inferred_corners_inside)

  are_rects_valid <- function(rx1, ry1, rx2, ry2) {
    # rx1, ry1, rx2, ry2 are vectors of each coord for all rectangles
    n_rects <- length(rx1)

    rect_xmin <- pmin(rx1, rx2)
    rect_xmax <- pmax(rx1, rx2)
    rect_ymin <- pmin(ry1, ry2)
    rect_ymax <- pmax(ry1, ry2)

    result <- rep(TRUE, n_rects)

    # Polygon vertical edges vs rectangle horizontal edges
    for (i in 1:nrow(v_edges)) {
      v_x <- v_edges$x[i]
      v_ymin <- v_edges$ymin[i]
      v_ymax <- v_edges$ymax[i]

      intersects <- (v_x > rect_xmin) & (v_x < rect_xmax) &
                    ((ry1 > v_ymin) & (ry1 < v_ymax) | (ry2 > v_ymin) & (ry2 < v_ymax))
      result[intersects] <- FALSE
    }

    # Polygon horizontal edges vs rectangle vertical edges
    for (i in 1:nrow(h_edges)) {
      h_y <- h_edges$y[i]
      h_xmin <- h_edges$xmin[i]
      h_xmax <- h_edges$xmax[i]

      intersects <- (h_y > rect_ymin) & (h_y < rect_ymax) &
                    ((rx1 > h_xmin) & (rx1 < h_xmax) | (rx2 > h_xmin) & (rx2 < h_xmax))
      result[intersects] <- FALSE
    }

    return(result)
  }

  valid_rects_vec <- are_rects_valid(
    pairs$x1,
    pairs$y1,
    pairs$x2,
    pairs$y2
  )

  pairs <- pairs |>
    filter(valid_rects_vec) |>
    mutate(
      area = (abs(x2 - x1) + 1) * (abs(y2 - y1) + 1)
    )

  part2_ans <- max(pairs$area)
})

part1_ans # 4741848414

part2_ans # 1508918480
