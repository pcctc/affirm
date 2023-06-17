test_that("affirm_values() works", {
  # return all obs
  expect_snapshot({
    affirm_init(replace = TRUE);
    mtcars |>
      affirm_values(
        label = "No. cylinders must be 4, 6, or 8",
        column = cyl,
        values = c(4, 6, 8)
      ) |>
      dplyr::as_tibble()}
  )

  # works with an externally defined object
  expect_snapshot({
    acceptable_levels <- c(4, 6, 8);
    affirm_init(replace = TRUE);
    affirm_values(
      mtcars,
      label = "externally defined object",
      column = "cyl",
      values = acceptable_levels
    ) |>
      dplyr::as_tibble()}
  )


  # ! The `column` argument must select one and only one column.
  expect_error({
    affirm_init(replace = TRUE);
    mtcars |>
      affirm_values(
        label = "No. cylinders must be 4, 6, or 8",
        column = c(cyl, mpg),
        values = c(4, 6, 8)
      )},
    "argument must select one and only one column"
  )

  # ! The `values` argument must be a vector.
  expect_error({
    affirm_init(replace = TRUE);
    mtcars |>
      affirm_values(
        label = "No. cylinders must be 4, 6, or 8",
        column = cyl,
        values = list(4, 6, 8)
      )},
    "argument must be a vector"
  )
})
