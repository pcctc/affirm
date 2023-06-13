test_that("affirm_na() works", {
  expect_snapshot({
    affirm_init(replace = TRUE)
    affirm_na(
      data = dplyr::tibble(x = c(NA, TRUE, NA, FALSE, NA)),
      label = "All NA values",
      column = x
    )
    affirm_not_na(
      data = dplyr::tibble(x = c(NA, TRUE, NA, FALSE, NA)),
      label = "No NA values",
      column = x
    )
    affirm_report_raw_data()$data
  })
})

test_that("affirm_na() thows errors", {
  # ! The `column` argument must select one and only one column.
  expect_error({
    affirm_init(replace = TRUE)
    affirm_na(
      data = dplyr::tibble(x = c(NA, TRUE, NA, FALSE, NA), y = x),
      label = "All NA values",
      column = c(x, y)
    )},
    "argument must select one and only one column"
  )

  # ! The `column` argument must select one and only one column.
  expect_error({
    affirm_init(replace = TRUE)
    affirm_not_na(
      data = dplyr::tibble(x = c(NA, TRUE, NA, FALSE, NA), y = x),
      label = "No NA values",
      column = any_of("not_a_variable")
    )},
    "argument must select one and only one column"
  )

  # ! Arguments `data` and `column` are required.
  expect_error({
    affirm_init(replace = TRUE)
    affirm_na(
      data = dplyr::tibble(x = c(NA, TRUE, NA, FALSE, NA), y = x),
      label = "All NA values"
    )},
    "are required"
  )

  # ! Arguments `data` and `column` are required.
  expect_error({
    affirm_init(replace = TRUE)
    affirm_not_na(
      data = dplyr::tibble(x = c(NA, TRUE, NA, FALSE, NA), y = x),
      label = "No NA values"
    )},
    "are required"
  )
})
