test_that("affirm_range() works", {

  expect_snapshot({
    affirm_init(replace = TRUE)
    affirm_range(
      mtcars,
      label = "MPG is >=21 and <=30.4",
      column = mpg,
      range = c(21, 30.4),
      boundaries = c(TRUE, TRUE),
      report_listing = filter(., !lgl_condition) |> select(mpg) |>  mutate(..label.. = label)
    )
    affirm_range(
      mtcars,
      label = "MPG is >21 and <=30.4",
      column = mpg,
      range = c(21, 30.4),
      boundaries = c(FALSE, TRUE),
      report_listing = filter(., !lgl_condition) |> select(mpg) |>  mutate(..label.. = label)
    )
    affirm_range(
      mtcars,
      label = "MPG is >=21 and <30.4",
      column = mpg,
      range = c(21, 30.4),
      boundaries = c(TRUE, FALSE),
      report_listing = filter(., !lgl_condition) |> select(mpg) |>  mutate(..label.. = label)
    )
    affirm_range(
      mtcars,
      label = "MPG is >21 and <30.4",
      column = mpg,
      range = c(21, 30.4),
      boundaries = c(FALSE, FALSE),
      report_listing = filter(., !lgl_condition) |> select(mpg) |>  mutate(..label.. = label)
    )
    affirm_report_raw_data()$data}
  )

  # test a date column
  expect_snapshot({
    affirm_init(replace = TRUE)
    affirm_range(
      data = data.frame(x = as.Date("2000-01-01")),
      label = "Check a date range",
      column = "x",
      range = c(as.Date("2000-01-01") - 1, as.Date("2000-01-01") + 1)
    )
    affirm_range(
      data = data.frame(x = as.Date("2000-01-01")),
      label = "Check a date range, excluding boundaries points",
      column = "x",
      range = c(as.Date("2000-01-01"), as.Date("2000-01-01") + 1),
      boundaries = FALSE
    )
    affirm_report_raw_data()$data}
  )

})

test_that("affirm_range() throws errors", {
  # ! Arguments `data`, `column`, and `range` are required.
  expect_error({
    affirm_init(replace = TRUE)
    affirm_range()},
    "are required"
  )

  # ! The `range` argument must be a vector of length 2.
  expect_error({
    affirm_init(replace = TRUE)
    affirm_range(
      mtcars,
      label = "bad range",
      column = "mpg",
      range = 1:6
    )},
    "argument must be a vector of length 2"
  )

  # ! The `column` argument must select one and only one column.
  expect_error({
    affirm_init(replace = TRUE)
    affirm_range(
      mtcars,
      label = "too many cols",
      column = c("mpg", "cyl"),
      range = c(20, 30)
    )},
    "argument must select one and only one column"
  )

  # ! The "mpg" column must be numeric.
  expect_error({
    affirm_init(replace = TRUE)
    affirm_range(
      mtcars |> dplyr::mutate(mpg = as.character(mpg)),
      label = "checking class errors",
      column = "mpg",
      range = c(20, 30)
    )},
    "argument class to match column"
  )

  # ! The `boundaries` argument must be class logical.
  expect_error({
    affirm_init(replace = TRUE)
    affirm_range(
      mtcars,
      column = "mpg",
      label = "checking boundaries errors",
      range = c(20, 30),
      boundaries = letters
    )},
    "argument must be class logical"
  )
})
