test_that("affirm_no_dupes() works", {
  expect_snapshot({
    affirm_init(replace = TRUE)
    affirm_no_dupes(
      mtcars,
      label = "duplicates in all vars",
      columns = everything()
    )
    affirm_report_raw_data()
  })

  expect_snapshot({
    affirm_init(replace = TRUE)
    affirm_no_dupes(
      mtcars,
      label = "duplicates in one var",
      columns = cyl
    )
    affirm_report_raw_data()
  })

  expect_snapshot({
    affirm_init(replace = TRUE)
    affirm_no_dupes(
      mtcars,
      label = "duplicates in two vars",
      columns = c(cyl, am)
    )
    affirm_report_raw_data()
  })
})

test_that("affirm_no_dupes() throws errors", {
  # ! The `columm` argument must select at least one column from `data`.
  expect_error({
    affirm_init(replace = TRUE)
    affirm_no_dupes(
      mtcars,
      label = "duplicates in two vars",
      columns = any_of("not_a_variable")
    )},
    "argument must select at least one column from"
  )

  # ! Arguments `data`, `label`, and `columns` are required.
  expect_error({
    affirm_init(replace = TRUE)
    affirm_no_dupes()},
    "are required"
  )
})
