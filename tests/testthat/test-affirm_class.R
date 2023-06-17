test_that("affirm_class() works", {
  expect_snapshot({
    affirm_init(replace = TRUE)
    affirm_class(
      dplyr::as_tibble(iris),
      label = "all cols are numeric (but Species really isn't)",
      columns = everything(),
      class = "numeric"
    )
    affirm_report_raw_data()$data}
  )
})


test_that("affirm_class() throws errors", {
  # ! The `columns` argument must select at least one column.
  expect_error({
    affirm_init(replace = TRUE)
    affirm_class(
      dplyr::as_tibble(iris),
      label = "all cols are numeric (but Species really isn't)",
      columns = any_of("not_a_column"),
      class = "numeric"
    )},
    "argument must select at least one column"
  )

  # ! Arguments `data`, `columns`, and `class` are required.
  expect_error({
    affirm_init(replace = TRUE)
    affirm_class(
      dplyr::as_tibble(iris),
      label = "all cols are numeric (but Species really isn't)"
    )},
    "are required"
  )
})
