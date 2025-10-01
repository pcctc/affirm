test_that("affirm_dupe_free() works", {
  expect_snapshot({
    affirm_init(replace = TRUE)
    affirm_dupe_free(
      mtcars,
      label = "duplicates in all vars",
      columns = everything()
    )
    affirm_report_raw_data()
  })

  expect_snapshot({
    affirm_init(replace = TRUE)
    affirm_dupe_free(
      mtcars,
      label = "duplicates in one var",
      columns = disp
    )
    affirm_report_raw_data()
  })

  expect_snapshot({
    affirm_init(replace = TRUE)
    affirm_dupe_free(
      mtcars,
      label = "duplicates in two vars",
      columns = c(disp, am)
    )
    affirm_report_raw_data()
  })
})

test_that("affirm_dupe_free() throws errors", {
  # ! The `column` argument must select at least one column from `data`.
  expect_error({
    affirm_init(replace = TRUE)
    affirm_dupe_free(
      mtcars,
      label = "duplicates in two vars",
      columns = any_of("not_a_variable")
    )},
    "argument must select at least one column from"
  )

  # ! Arguments `data`, `label`, and `columns` are required.
  expect_error({
    affirm_init(replace = TRUE)
    affirm_dupe_free()},
    "are required"
  )
})
