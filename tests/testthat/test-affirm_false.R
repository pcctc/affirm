test_that("affirm_false() works", {
  expect_equal({
    affirm_init(replace = TRUE)
    as_tibble(mtcars) |>
      affirm_true(
        label = "No. cylinders must be 4, 6, or 8",
        condition = cyl %in% c(4, 6)
      )
    affirm_report_raw_data()$data},
    {affirm_init(replace = TRUE)
      as_tibble(mtcars) |>
        affirm_false(
          label = "No. cylinders must be 4, 6, or 8",
          condition = !cyl %in% c(4, 6)
        )
      affirm_report_raw_data()$data}
  )
})
