test_that("affirm_report() works", {


  expect_error({
    affirm_init(replace = TRUE);
    affirm_true(
      mtcars,
      label = "leave it all, no actions",
      condition = mpg > 33
    );
    affirm_report_gt()},
    NA
  )

  expect_error({
    affirm_init(replace = TRUE);
    affirm_true(
      mtcars,
      label = "leave it all, no actions",
      condition = mpg > 33
    );
    affirm_report_excel(file = tempfile(fileext = ".xlsx"))},
    NA
  )

  # labels are added to report when requested
  expect_snapshot({
    mtcars2 <- dplyr::as_tibble(mtcars)
    attr(mtcars2$mpg, 'label') <- "MPG LABEL"
    affirm_init(replace = TRUE)
    affirm_true(
      mtcars2,
      label = "leave it all, no actions",
      condition = mpg > 33
    )
    affirm_report_raw_data(variable_labels = TRUE)$data}
  )
})


test_that("affirm_report() works, but skip in CI", {
  skip_on_ci()

  # helper function to create png of report, and return path
  save_affirm_report_gt_png <- function(affirm_report) {
    path <- tempfile(fileext = ".png")
    gt::gtsave(affirm_report, filename = path)
    path
  }

  expect_snapshot_file({
    affirm_init(replace = TRUE);
    affirm_true(
      mtcars,
      label = "leave it all, no actions",
      condition = mpg > 33
    );
    affirm_report_gt() |> save_affirm_report_gt_png()},
    "affirm_report.png"
  )
})
