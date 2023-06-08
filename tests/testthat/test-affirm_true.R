test_that("affirm_true() works", {
  # return all obs
  expect_snapshot({
    affirm_init(replace = TRUE);
    affirm_true(
      mtcars,
      label = "leave it all, no actions",
      condition = mpg > 33
    ) |>
      as_tibble()}
  )

  # return the full mtcars df in the report
  expect_snapshot({
    affirm_init(replace = TRUE);
    affirm_true(
      mtcars,
      label = "export the full mtcars in the report",
      condition = mpg > 33,
      report_listing = .
    );
    affirm_report_raw_data()}
  )

  # return modified mtcars data
  expect_snapshot({
    affirm_init(replace = TRUE);
    affirm_true(
      mtcars,
      label = "export the full mtcars in the report",
      condition = mpg > 33,
      # return data frame with zero rows
      data_action = filter(., FALSE)
    )}
  )
})

test_that("affirm_true() error messaging", {
  # ✖ The result of evaluated expression of `report_listing=`
  # must be a data frame.
  expect_error({
    affirm_init(replace = TRUE);
    affirm_true(
      mtcars,
      label = "it's a label",
      condition = mpg > 20,
      report_listing = letters
    )},
    "must be a data frame"
  )

  # ! The `condition=` expression must resolve to a vector of
  # class <logical> with length 1 or 32 (size of the data frame).
  expect_error({
    affirm_init(replace = TRUE);
    affirm_true(
      mtcars,
      label = "it's a label",
      condition = letters
    )},
    "expression must resolve to a vector of class"
  )

  # ! The `condition=` expression must resolve to a vector of
  # class <logical> with length 1 or 32 (size of the data frame).
  expect_error({
    affirm_init(replace = TRUE);
    affirm_true(
      mtcars,
      label = "it's a label",
      condition = c("TRUE, FALSE")
    )},
    "with length"
  )

  # ✖ Affirmation "it's a label" did not pass for 1
  # observation.
  expect_error({
    affirm_init(replace = TRUE);
    affirm_true(
      mtcars,
      label = "it's a label",
      condition = FALSE,
      error = TRUE
    )},
    "did not pass"
  )

  # ! `data=` argument must be a data frame.
  expect_error({
    affirm_init(replace = TRUE);
    affirm_true(
      letters,
      label = "it's a label",
      condition = TRUE
    )},
    "argument must be a data frame"
  )

  # ! The data frame cannot have a column named ".".
  expect_error({
    affirm_init(replace = TRUE);
    affirm_true(
      data.frame(. = letters),
      label = "it's a label",
      condition = TRUE
    )},
    "The data frame cannot have a column named"
  )

  # ! Arguments `label=`, `data_frames=`, `columns=` must be a string of length one.
  expect_error({
    affirm_init(replace = TRUE);
    affirm_true(
      mtcars,
      label = letters,
      condition = TRUE
    )},
    "ust be a string of length one"
  )

  expect_error({
    affirm_init(replace = TRUE);
    affirm_true(
      mtcars,
      label = "a label",
      condition = TRUE
    );
    affirm_true(
      mtcars,
      label = "a label",
      condition = TRUE
    )},
    "Duplicated labels"
  )

  # ✖ First run `affirm_init()` to begin affirmations.
  expect_error({
    affirm_close();
    affirm_true(
      mtcars,
      label = "leave it all, no actions",
      condition = mpg > 33
    )},
    "First run"
  )
})