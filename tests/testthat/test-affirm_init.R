test_that("affirm_init() works", {
  expect_error(
    affirm_init(),
    NA
  )

  expect_error({
    affirm_init(replace = TRUE);
    affirm_true(mtcars, label = "asdf", condition = TRUE);
    affirm_init(replace = FALSE)},
    "Affirmations are in-progress"
  )

  expect_error(
    affirm_init(replace = "a"),
    "argument must be a scalar"
  )
})
