library(data.table)
library(lubridate)

# ── shared dummy data ──────────────────────────────────────────────────────────
test_data <- data.table(
  date     = as.IDate(c("2022-01-15", "2022-06-10", "2022-11-03",
                        "2023-02-20", "2023-08-14", "2023-12-01")),
  geo_unit = c("A", "A", "B", "B", "A", "B"),
  geo_grp  = c("G1", "G1", "G2", "G2", "G1", "G2")
)

col_map <- list(
  date         = "date",
  geo_unit     = "geo_unit",
  geo_unit_grp = "geo_grp"
)

# ── tests ──────────────────────────────────────────────────────────────────────
test_that("dt_by = 'month' returns first-of-month dates only", {
  result <- make_xgrid(test_data, col_map, dt_by = "month")
  expect_true(all(lubridate::day(result$date) == 1))
})

test_that("dt_by = 'month' respects months_subset", {
  result <- make_xgrid(test_data, col_map, dt_by = "month", months_subset = 1:6)
  expect_true(all(lubridate::month(result$date) %in% 1:6))
})

test_that("dt_by = 'month' covers correct years", {
  result <- make_xgrid(test_data, col_map, dt_by = "month")
  expect_setequal(unique(lubridate::year(result$date)), c(2022, 2023))
})

test_that("dt_by = 'day' and 'week' still work", {
  expect_no_error(make_xgrid(test_data, col_map, dt_by = "day"))
  expect_no_error(make_xgrid(test_data, col_map, dt_by = "week"))
})

test_that("invalid dt_by throws error", {
  expect_error(make_xgrid(test_data, col_map, dt_by = "year"))
})
