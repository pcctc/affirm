test_that("affirm_clean_join() works", {
  expect_snapshot({
    affirm_init(replace = TRUE)
    df <-
      dplyr::tibble(lgl = c(NA, TRUE, NA, FALSE, NA)) |>
      dplyr::mutate(id = dplyr::row_number())
    affirm_clean_join(
      dplyr::full_join(df, df, by = "id"),
      label = "Checking for clean merge"
    )
    affirm_report_raw_data()
    affirm_report_raw_data()$data}
  )
})

test_that("affirm_clean_join() throws errors", {
  # ! Arguments `data` and `label` are required.
  expect_error({
    affirm_init(replace = TRUE)
    affirm_clean_join()},
    "are required"
  )
})
