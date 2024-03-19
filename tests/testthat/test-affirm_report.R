mtcars_modified <- mtcars |>
  tibble::rownames_to_column(var = "car")

attr(mtcars_modified$car, 'label') <- "Car model"
attr(mtcars_modified$mpg, 'label') <- "Miles/(US) gallon"
attr(mtcars_modified$cyl, 'label') <- "Number of cylinders"
attr(mtcars_modified$disp, 'label') <- "Displacement (cu.in.)"


test_that("affirm_report() works", {

  expect_error({
    affirm_init(replace = TRUE)
    affirm_not_na(
      mtcars,
      label = "mpg is not missing",
      column = mpg
    )
    affirm_true(
      mtcars,
      label = "leave it all, no actions",
      condition = mpg > 33
    )
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

  # names are added according to glue syntax
  expect_snapshot({
    affirm_init(replace = TRUE)
    affirm_true(
      mtcars,
      label = "leave it all, no actions",
      condition = mpg > 33,
      data_frames = "mtcars",
      id = 1
    )
    affirm_true(
      mtcars,
      label = "No. cylinders must be 4 or 6",
      condition = cyl %in% c(4, 6),
      data_frames = "mtcars",
      id = 2
    )
    affirm_report_raw_data()$data
  })
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


test_that("affirmation name details", {


  expect_error({
    affirm_init(replace = TRUE)
    options('affirm.id_cols' = "car")
    affirm_true(
      mtcars_modified,
      label = "No. cylinders must be 4 or 6",
      condition = cyl %in% c(4, 6),
      id = 1,
      data_frames = "mtcars"
    )
    affirm_true(
      mtcars_modified,
      label = "mpg lt 33",
      id = 2,
      condition = mpg < 33,
      data_frames = "mtcars"
    );
    affirm_report_excel(file = tempfile(fileext = ".xlsx"))},
    NA
  )

  expect_error({
    affirm_init(replace = TRUE)
    options('affirm.id_cols' = "car")
    affirm_true(
      mtcars_modified,
      label = "No. cylinders must be 4 or 6",
      condition = cyl %in% c(4, 6),
      id = 1,
      data_frames = "mtcars"
    )
    affirm_true(
      mtcars_modified,
      label = "mpg lt 33",
      id = 2,
      condition = mpg < 33,
      data_frames = "mtcars"
    );
    tmp_xlsx <- tempfile(fileext = ".xlsx")
    affirm_report_excel(file = tmp_xlsx, affirmation_name = "{data_frames} {id} {total_n}")
    openxlsx2::read_xlsx(tmp_xlsx, sheet = "mtcars 1 32")},
    NA
  )

  expect_error({
    affirm_init(replace = TRUE)
    affirm_true(
      mtcars_modified,
      label = "No. cylinders must be 4 or 6",
      condition = cyl %in% c(4, 6),
      id = 1,
      data_frames = "mtcars"
    )
    affirm_report_excel(file = tempfile(fileext = ".xlsx"), affirmation_name = "{data_frames}{id}{label}moooooooorreeeeecharacters")
  },
  "At least one sheet name exceeds the allowed 31 characters"
  )

  expect_error({
    affirm_init(replace = TRUE)
    affirm_true(
      mtcars_modified,
      label = "No. cylinders must be 4 or 6",
      condition = cyl %in% c(4, 6),
      id = 1,
      data_frames = "mtcars"
    )
    affirm_report_excel(file = tempfile(fileext = ".xlsx"), affirmation_name = "{data.frames}{id}")
  },
  )

})

test_that("affirmations with zero errors carried forward", {

  expect_error({
    affirm_init(replace = TRUE)
    affirm_true(
      mtcars,
      label = "No. cylinders must be 4, 6, or 8",
      condition = cyl %in% c(4, 6, 8),
      id = 1,
      data_frames = "mtcars"
    )
    affirm_report_raw_data()},
    NA
  )

  expect_true({
    affirm_init(replace = TRUE)
    affirm_true(
      mtcars,
      label = "No. cylinders must be 4, 6, or 8",
      condition = cyl %in% c(4, 6, 8),
      id = 1,
      data_frames = "mtcars"
    )
    tempxlsx <- tempfile(fileext = ".xlsx")
    affirm_report_excel(file = tempxlsx)
    df_summary <- openxlsx2::read_xlsx(tempxlsx)
    df_summary$error_rate == 0
    }
  )


  expect_s3_class({
    affirm_init(replace = TRUE)
    affirm_true(
      mtcars,
      label = "No. cylinders must be 4, 6, or 8",
      condition = cyl %in% c(4, 6, 8),
      id = 1,
      data_frames = "mtcars"
    )
    affirm_report_raw_data()[["data"]][[1]]
  },
  "data.frame"
  )

})


test_that("excel report helpers", {

  expect_equal(
    .compute_col_width(mtcars_modified[, 1:4]),
    c("car" = 22, "mpg" = 8, "cyl" = 8, "disp" = 8, "Notes" = 30)
  )

  expect_equal(
    .retrieve_labels(mtcars_modified[, 1:5]),
    data.frame(
      "car" = "Car model",
      "mpg" = "Miles/(US) gallon",
      "cyl" = "Number of cylinders",
      "disp" = "Displacement (cu.in.)",
      "hp" = NA_character_
      )
  )

})
