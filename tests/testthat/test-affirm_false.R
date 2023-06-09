test_that("affirm_false() works", {
  expect_equal({
    affirm_init(replace = TRUE)
    dplyr::as_tibble(mtcars) |>
      affirm_true(
        label = "No. cylinders must be 4, 6, or 8",
        condition = cyl %in% c(4, 6)
      )
    affirm_report_raw_data()$data},
    {affirm_init(replace = TRUE)
      dplyr::as_tibble(mtcars) |>
        affirm_false(
          label = "No. cylinders must be 4, 6, or 8",
          condition = !cyl %in% c(4, 6)
        )
      affirm_report_raw_data()$data}
  )
})

test_that("affirm_false() throws errors", {
  # ! Arguments `data`, `label`, and `condition` are required.
  expect_error({
    affirm_init(replace = TRUE)
    affirm_false(
      mtcars,
      condition = !cyl %in% c(4, 6)
    )},
    "are required"
  )
})
