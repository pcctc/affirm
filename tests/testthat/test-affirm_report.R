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

  # Zero error rates remain#
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

  # All relevant variables/columns present in blank worksheets
  expect_true({
    affirm_init(replace = TRUE)

    options(
      'affirm.id_cols' =  c(
        "mpg", "cyl", "disp", "hp",
        "drat", "wt", "qsec", "vs",
        "am", "gear", "carb"
      )
    )

    affirm_true(
      mtcars,
      label = "No. cylinders must be 4, 6, or 8",
      condition = cyl %in% c(4, 6, 8),
      id = 2,
      data_frames = "mtcars"
    )
    tempxlsx <- tempfile(fileext = ".xlsx")
    affirm_report_excel(file = tempxlsx)
    df_empty_sheet <- openxlsx2::read_xlsx(tempxlsx, sheet = "mtcars2", start_row = 4)
    all(names(mtcars) %in% names(df_empty_sheet))
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
    c("car" = 22, "mpg" = 8, "cyl" = 8, "disp" = 8, "Comment" = 30)
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

test_that("Excel report summary sheet has 'assigned_to' column", {

  expect_true({
    affirm_init(replace = TRUE)
    options('affirm.id_cols' = "car")
    affirm_true(
      mtcars_modified,
      label = "mpg lt 33",
      id = 1,
      condition = mpg < 33,
      data_frames = "mtcars"
    )
    affirm_true(
      mtcars_modified,
      label = "No. cylinders must be 4 or 6",
      condition = cyl %in% c(4, 6),
      id = 2,
      data_frames = "mtcars"
    )
    affirm_true(
      mtcars_modified,
      label = "car must be a number",
      condition = is.numeric(car),
      id = 3,
      data_frames = "mtcars"
    );
    tempxlsx <- tempfile(fileext = ".xlsx")
    affirm_report_excel(file = tempxlsx, affirmation_name = "{data_frames}{id}")
    vec_summary_cols <- openxlsx2::read_xlsx(tempxlsx, sheet = "Summary") |> names()
    first_summary_col <- head(vec_summary_cols, 1)
    vec_summary_cols[1] == "assigned_to"
  }
  )
}
)

test_that("Excel report summary sheet has 'Status' and 'Comment' columns at the end", {

  expect_true({
    affirm_init(replace = TRUE)
    options('affirm.id_cols' = "car")
    affirm_true(
      mtcars_modified,
      label = "mpg gt 33",
      id = 1,
      condition = mpg > 33,
      data_frames = "mtcars"
    )
    affirm_true(
      mtcars_modified,
      label = "No. cylinders must be 4 or 6",
      condition = cyl %in% c(4, 6),
      id = 2,
      data_frames = "mtcars"
    )
    affirm_true(
      mtcars_modified,
      label = "car must be a number",
      condition = is.numeric(car),
      id = 3,
      data_frames = "mtcars"
    );
    tempxlsx <- tempfile(fileext = ".xlsx")
    affirm_report_excel(file = tempxlsx, affirmation_name = "{data_frames}{id}")
    vec_summary_cols <- openxlsx2::read_xlsx(tempxlsx, sheet = "Summary") |> names()
    last_summary_cols <- tail(vec_summary_cols, 2)
    all(last_summary_cols == c("Status", "Comment"))
  }
  )
}
)

test_that("Excel report affirmation sheet has 'Status' and 'Comment' columns at the end", {

  expect_true({
    affirm_init(replace = TRUE)
    affirm_true(
      mtcars_modified,
      label = "mpg gt 33",
      id = 1,
      condition = mpg > 33,
      data_frames = "mtcars"
    );

    tempxlsx <- tempfile(fileext = ".xlsx")
    affirm_report_excel(file = tempxlsx, affirmation_name = "{data_frames}{id}")
    vec_affirm_cols <- openxlsx2::read_xlsx(tempxlsx, sheet = "mtcars1", start_row = 4) |> names()
    last_affirm_cols <- tail(vec_affirm_cols, 2)
    all(last_affirm_cols == c("Status", "Comment"))
  }
  )
}
)


cli::test_that_cli(
  configs = c("plain", "ansi"),
  desc = "Test that cli output is as expected if `previous_file` class throws an error.",
  code = {
    affirm_init(replace = TRUE)
    affirm_true(
      mtcars,
      label = "mpg gt 20",
      id = 1,
      condition = mpg > 20,
      data_frames = "mtcars"
    );

    tempxlsx <- tempfile(fileext = ".xlsx");

    testthat::expect_snapshot({
      affirm_report_excel(
        file = tempxlsx,
        affirmation_name = "{data_frames}{id}",
        previous_file = data.frame(path = "a valid path.xlsx")
      )
    },
    error = TRUE
    )
  }
)

#==============================================================================#
# Updating/Previous Affirmation Test Suite--------------------------------------
#==============================================================================#
attr(mtcars_modified$hp, 'label') <- "Gross horsepower"
attr(mtcars_modified$drat, 'label') <- "Rear axle ratio"
attr(mtcars_modified$wt, 'label') <- "Weight (1000 lbs)"
attr(mtcars_modified$qsec, 'label') <- "1/4 mile time"
attr(mtcars_modified$vs, 'label') <- "Engine"
attr(mtcars_modified$am, 'label') <- "Transmission"
attr(mtcars_modified$gear, 'label') <- "Number of forward gears"
attr(mtcars_modified$carb, 'label') <- "Number of carburetors"

test_that("Test that previous assigned_to info is carried forward in summary sheet", {

  affirm_init(replace = TRUE)
  options('affirm.id_cols' = "car")

  affirm_false(
    mtcars_modified[1:16,],
    label = "mpg gt 20",
    id = 1,
    condition = mpg > 20,
    data_frames = "mtcars"
  );

  tempxlsx <- tempfile(fileext = ".xlsx")
  affirm_report_excel(file = tempxlsx, affirmation_name = "{data_frames}{id}")

  affirm_close()

  wb_prev_init <- openxlsx2::wb_load(tempxlsx)

  # Adding a value to "assigned to" to be carried over in new wb
  wb_prev <-
    openxlsx2::wb_add_data(
      wb = wb_prev_init,
      sheet = "Summary",
      x = "Meghan",
      start_row = 2,
      start_col = 1
    )

  openxlsx2::wb_save(
    wb_prev,
    file = tempxlsx,
    overwrite = TRUE
  )

  affirm_init(replace = TRUE)
  options('affirm.id_cols' = "car")
  affirm_false(
    mtcars_modified,
    label = "mpg gt 20",
    id = 1,
    condition = mpg > 20,
    data_frames = "mtcars"
  )

  updated_tempxlsx <- tempfile(fileext = ".xlsx")
  affirm_report_excel(file = updated_tempxlsx, affirmation_name = "{data_frames}{id}", previous_file = tempxlsx)

  updated_summary_sheet_value <-
    openxlsx2::wb_to_df(
      file = updated_tempxlsx,
      sheet = "Summary"
    ) |>
    dplyr::pull("assigned_to")

  expect_equal(updated_summary_sheet_value, "Meghan")

}
)

test_that("Test that previous comments and status info is carried forward in summary sheet", {

  affirm_init(replace = TRUE)
  options('affirm.id_cols' = "car")

  affirm_false(
    mtcars_modified[1:16,],
    label = "mpg gt 20",
    id = 1,
    condition = mpg > 20,
    data_frames = "mtcars"
  );

  tempxlsx <- tempfile(fileext = ".xlsx")
  affirm_report_excel(file = tempxlsx, affirmation_name = "{data_frames}{id}")

  affirm_close()

  wb_prev_init <- openxlsx2::wb_load(tempxlsx)

  # Adding a value to "Status" and "Comment" to be carried over in new wb
  wb_prev <-
    wb_prev_init |>
    openxlsx2::wb_add_data(
      sheet = "Summary",
      x = "Not OK",
      start_row = 2,
      start_col = 10
    ) |>
    openxlsx2::wb_add_data(
      sheet = "Summary",
      x = "This is a comment",
      start_row = 2,
      start_col = 11
    )

  openxlsx2::wb_save(
    wb_prev,
    file = tempxlsx,
    overwrite = TRUE
  )

  affirm_init(replace = TRUE)
  options('affirm.id_cols' = "car")
  affirm_false(
    mtcars_modified,
    label = "mpg gt 20",
    id = 1,
    condition = mpg > 20,
    data_frames = "mtcars"
  )

  updated_tempxlsx <- tempfile(fileext = ".xlsx")
  affirm_report_excel(file = updated_tempxlsx, affirmation_name = "{data_frames}{id}", previous_file = tempxlsx)

  updated_summary_sheet_status_value <-
    openxlsx2::wb_to_df(
      file = updated_tempxlsx,
      sheet = "Summary"
    ) |>
    dplyr::pull("Status")

  updated_summary_sheet_comment_value <-
    openxlsx2::wb_to_df(
      file = updated_tempxlsx,
      sheet = "Summary"
    ) |>
    dplyr::pull("Comment")

  expect_true(
    updated_summary_sheet_status_value == "Not OK" &
      updated_summary_sheet_comment_value == "This is a comment"
  )

}
)

test_that("Test that previous comments and status info is carried forward in affirmation sheet", {

  affirm_init(replace = TRUE)
  options('affirm.id_cols' = "car")
  affirm_true(
    mtcars_modified,
    label = "mpg gt 20",
    id = 1,
    condition = mpg > 20,
    data_frames = "mtcars"
  );

  tempxlsx <- tempfile(fileext = ".xlsx")
  affirm_report_excel(file = tempxlsx, affirmation_name = "{data_frames}{id}")

  affirm_close()

  wb_prev <- openxlsx2::wb_load(tempxlsx)

  # Adding a value to "status and comment" to be carried over in new wb
  wb_prev2 <-
    wb_prev |>
    openxlsx2::wb_add_data(
      sheet = "mtcars1",
      x = "OK",
      start_row = 8,
      start_col = 3
    ) |>
    openxlsx2::wb_add_data(
      sheet = "mtcars1",
      x = "Queried",
      start_row = 8,
      start_col = 4
    )

  openxlsx2::wb_save(
    wb_prev2,
    file = tempxlsx,
    overwrite = TRUE
  )

  affirm_init(replace = TRUE)
  options('affirm.id_cols' = "car")

  affirm_true(
    mtcars_modified,
    label = "mpg gt 20",
    id = 1,
    condition = mpg > 20,
    data_frames = "mtcars"
  )

  updated_tempxlsx <- tempfile(fileext = ".xlsx")
  affirm_report_excel(file = updated_tempxlsx, affirmation_name = "{data_frames}{id}", previous_file = tempxlsx)

  updated_affirm_sheet_status <-
    openxlsx2::wb_to_df(
      file = updated_tempxlsx,
      sheet = "mtcars1",
      start_row = 4
    ) |>
    dplyr::filter(!is.na(Status)) |>
    dplyr::pull("Status")

  updated_affirm_sheet_comment <-
    openxlsx2::wb_to_df(
      file = updated_tempxlsx,
      sheet = "mtcars1",
      start_row = 4
    ) |>
    dplyr::filter(!is.na(Comment)) |>
    dplyr::pull("Comment")

  expect_true(updated_affirm_sheet_status == "OK" & updated_affirm_sheet_comment == "Queried")

}
)

test_that("Test that blank comments and status info is present when a previous affirmation sheet is blank", {

  affirm_init(replace = TRUE)
  options('affirm.id_cols' = "car")
  affirm_false(
    mtcars_modified,
    label = "mpg lt 0",
    id = 1,
    condition = mpg < 0,
    data_frames = "mtcars"
  );

  tempxlsx <- tempfile(fileext = ".xlsx")
  affirm_report_excel(file = tempxlsx, affirmation_name = "{data_frames}{id}")

  affirm_close()


  affirm_init(replace = TRUE)
  options('affirm.id_cols' = "car")

  affirm_false(
    mtcars_modified,
    label = "mpg lt 0",
    id = 1,
    condition = mpg < 0,
    data_frames = "mtcars"
  )

  updated_tempxlsx <- tempfile(fileext = ".xlsx")
  affirm_report_excel(file = updated_tempxlsx, affirmation_name = "{data_frames}{id}", previous_file = tempxlsx)

  updated_affirm_sheet_status <-
    openxlsx2::wb_to_df(
      file = updated_tempxlsx,
      sheet = "mtcars1",
      start_row = 4
    ) |>
    dplyr::pull("Status") |>
    is.na() |>
    all()

  updated_affirm_sheet_comment <-
    openxlsx2::wb_to_df(
      file = updated_tempxlsx,
      sheet = "mtcars1",
      start_row = 4
    ) |>
    dplyr::pull("Comment") |>
    is.na() |>
    all()

  expect_true(updated_affirm_sheet_status & updated_affirm_sheet_comment)

}
)

cli::test_that_cli(
  configs = c("plain", "ansi"),
  desc = "Test that duplicate data throws an error when updating a previous Affirm report.",
  code = {
    affirm_init(replace = TRUE);

    options('affirm.id_cols' = "car");

    affirm_true(
      mtcars_modified,
      label = "mpg gt 20",
      id = 1,
      condition = mpg > 20,
      data_frames = "mtcars"
    );

    tempxlsx <- tempfile(fileext = ".xlsx");

    affirm_report_excel(file = tempxlsx, affirmation_name = "{data_frames}{id}");

    affirm_close();

    wb_prev_init <- openxlsx2::wb_load(tempxlsx);

    # Adding a value to "status and comment" to be carried over in new wb
    wb_prev_init2 <-
      openxlsx2::wb_add_data(
        wb = wb_prev_init,
        sheet = "mtcars1",
        x = "OK",
        start_row = 6,
        start_col = 3
      );

    wb_prev <-
      openxlsx2::wb_add_data(
        wb = wb_prev_init2,
        sheet = "mtcars1",
        x = "Queried",
        start_row = 5,
        start_col = 4
      );

    openxlsx2::wb_save(
      wb_prev,
      file = tempxlsx,
      overwrite = TRUE
    );

    affirm_init(replace = TRUE);

    options('affirm.id_cols' = "car");

    affirm_true(
      mtcars_modified |>
        rbind(mtcars_modified),
      label = "mpg gt 20",
      id = 1,
      condition = mpg > 20,
      data_frames = "mtcars"
    );

    updated_tempxlsx <- tempfile(fileext = ".xlsx");

    testthat::expect_snapshot({
      affirm_report_excel(
        file = updated_tempxlsx,
        affirmation_name = "{data_frames}{id}",
        previous_file = tempxlsx
      )
    },
    error = TRUE
    )
  }
)

test_that("Test that reports can run and be updated when newly created affirmations are present", {

  affirm_init(replace = TRUE)
  options('affirm.id_cols' = "car")
  affirm_false(
    mtcars_modified,
    label = "mpg lt 15",
    id = 1,
    condition = mpg < 15,
    data_frames = "mtcars"
  );

  tempxlsx <- tempfile(fileext = ".xlsx")
  affirm_report_excel(file = tempxlsx, affirmation_name = "{data_frames}{id}")

  affirm_close()


  affirm_init(replace = TRUE)
  options('affirm.id_cols' = "car")

  affirm_false(
    mtcars_modified,
    label = "mpg gt 15",
    id = 1,
    condition = mpg < 15,
    data_frames = "mtcars"
  )

  affirm_false(
    mtcars_modified,
    label = "mpg gt 10",
    id = 2,
    condition = mpg > 10,
    data_frames = "mtcars"
  )

  updated_tempxlsx <- tempfile(fileext = ".xlsx")
  affirm_report_excel(file = updated_tempxlsx, affirmation_name = "{data_frames}{id}", previous_file = tempxlsx)

  # We expect no error because mtcars2 should be a valid sheet if added correctly
  openxlsx2::wb_to_df(
    file = updated_tempxlsx,
    sheet = "mtcars2",
    start_row = 4
  ) |>
    expect_no_error()

}
)

test_that("Test that reports can run and be updated when old affirmations are removed", {

  affirm_init(replace = TRUE)
  options('affirm.id_cols' = "car")

  affirm_false(
    mtcars_modified,
    label = "mpg gt 15",
    id = 1,
    condition = mpg > 15,
    data_frames = "mtcars"
  )

  affirm_false(
    mtcars_modified,
    label = "mpg gt 10",
    id = 2,
    condition = mpg > 10,
    data_frames = "mtcars"
  )

  tempxlsx <- tempfile(fileext = ".xlsx")
  affirm_report_excel(file = tempxlsx, affirmation_name = "{data_frames}{id}")

  affirm_init(replace = TRUE)
  options('affirm.id_cols' = "car")
  affirm_false(
    mtcars_modified,
    label = "mpg gt 15",
    id = 1,
    condition = mpg > 15,
    data_frames = "mtcars"
  );
  updated_tempxlsx <- tempfile(fileext = ".xlsx")
  affirm_report_excel(file = updated_tempxlsx, affirmation_name = "{data_frames}{id}", previous_file = tempxlsx)


  affirm_close()

  old_wb_sheets <- openxlsx2::wb_load(tempxlsx)$sheet_names
  new_wb_sheets <- openxlsx2::wb_load(updated_tempxlsx)$sheet_names

  # Check mtcars2 was removed from the new report, so
  # old_wb_sheets should have 3 sheets
  # new_wb_sheets should have 2 sheets
  # And the new wb sheets names should be the first two of the old wb sheets

  wb_sheet_check <-
    (
      (length(old_wb_sheets) == 3 & length(new_wb_sheets) == 2) &
        new_wb_sheets == old_wb_sheets[1:2]
    ) |> all()

  expect_true(wb_sheet_check)

}
)

test_that("Test that when columns are removed from old to new affirmations, that the correct error is thrown", {

  affirm_init(replace = TRUE)
  options('affirm.id_cols' = c("car", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"))

  mtcars_modified |>
  affirm_false(
    label = "mpg gt 15",
    id = 1,
    condition = mpg > 15,
    data_frames = "mtcars"
  ) |>
  affirm_false(
    label = "mpg gt 10",
    id = 2,
    condition = mpg > 10,
    data_frames = "mtcars"
  )

  tempxlsx <- tempfile(fileext = ".xlsx")
  affirm_report_excel(file = tempxlsx, affirmation_name = "{data_frames}{id}")

  affirm_init(replace = TRUE)
  options('affirm.id_cols' = c("car", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs"))
  mtcars_modified |>
    affirm_false(
      label = "mpg gt 15",
      id = 1,
      condition = mpg > 15,
      data_frames = "mtcars"
    ) |>
    affirm_false(
      label = "mpg gt 10",
      id = 2,
      condition = mpg > 10,
      data_frames = "mtcars"
    );
  updated_tempxlsx <- tempfile(fileext = ".xlsx")

  testthat::expect_snapshot({
    affirm_report_excel(
      file = updated_tempxlsx,
      affirmation_name = "{data_frames}{id}",
      previous_file = tempxlsx
    )
  },
  error = TRUE
  )
  affirm_close()

}
)

test_that("Test that when columns are mismatched from old to new affirmations, that the correct error is thrown", {

  affirm_init(replace = TRUE)

  options(
    'affirm.id_cols' =  c("car", "mpg", "cyl", "disp", "hp", "drat", "wt")
  )

  mtcars_modified |>
    affirm_false(
      label = "mpg gt 15",
      id = 1,
      condition = mpg > 15,
      data_frames = "mtcars"
    ) |>
    affirm_false(
      label = "mpg gt 10",
      id = 2,
      condition = mpg > 10,
      data_frames = "mtcars"
    )

  tempxlsx <- tempfile(fileext = ".xlsx")
  affirm_report_excel(file = tempxlsx, affirmation_name = "{data_frames}{id}")

  affirm_init(replace = TRUE)

  options(
    'affirm.id_cols' =  c("car", "mpg", "cyl", "disp",  "am", "gear", "carb")
  )

  mtcars_modified |>
    affirm_false(
      label = "mpg gt 15",
      id = 1,
      condition = mpg > 15,
      data_frames = "mtcars"
    ) |>
    affirm_false(
      label = "mpg gt 10",
      id = 2,
      condition = mpg > 10,
      data_frames = "mtcars"
    )

  updated_tempxlsx <- tempfile(fileext = ".xlsx")

  testthat::expect_snapshot({
    affirm_report_excel(
      file = updated_tempxlsx,
      affirmation_name = "{data_frames}{id}",
      previous_file = tempxlsx
    )
  },
  error = TRUE
  )
  affirm_close()

}
)

test_that("Test that when columns are added to new affirmations, that the report still renders", {

  affirm_init(replace = TRUE)
  options('affirm.id_cols' = c("car", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs"))

  affirm_false(
    mtcars_modified,
    label = "mpg gt 15",
    id = 1,
    condition = mpg > 15,
    data_frames = "mtcars"
  )

  affirm_false(
    mtcars_modified,
    label = "mpg gt 10",
    id = 2,
    condition = mpg > 10,
    data_frames = "mtcars"
  )

  tempxlsx <- tempfile(fileext = ".xlsx")
  affirm_report_excel(file = tempxlsx, affirmation_name = "{data_frames}{id}")

  affirm_init(replace = TRUE)
  options('affirm.id_cols' = c("car", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"))
  mtcars_modified |>
    affirm_false(
      label = "mpg gt 15",
      id = 1,
      condition = mpg > 15,
      data_frames = "mtcars"
    ) |>
    affirm_false(
      label = "mpg gt 10",
      id = 2,
      condition = mpg > 10,
      data_frames = "mtcars"
    );
  updated_tempxlsx <- tempfile(fileext = ".xlsx")

    affirm_report_excel(
      file = updated_tempxlsx,
      affirmation_name = "{data_frames}{id}",
      previous_file = tempxlsx
    ) |>
      expect_no_error()

    affirm_close()
}
)

test_that("Test that when columns are added to new affirmations, that the report still carries previous status and comment values over", {

  affirm_init(replace = TRUE)
  options('affirm.id_cols' = c("car", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs"))

  affirm_false(
    mtcars_modified,
    label = "mpg gt 15",
    id = 1,
    condition = mpg > 15,
    data_frames = "mtcars"
  )

  affirm_false(
    mtcars_modified,
    label = "mpg gt 10",
    id = 2,
    condition = mpg > 10,
    data_frames = "mtcars"
  )

  tempxlsx <- tempfile(fileext = ".xlsx")
  affirm_report_excel(file = tempxlsx, affirmation_name = "{data_frames}{id}")

  wb_prev_init <- openxlsx2::wb_load(tempxlsx);

  # Adding a value to "status and comment" to be carried over in new wb
  wb_prev <-
    wb_prev_init |>
    openxlsx2::wb_add_data(
      sheet = "mtcars1",
      x = "OK",
      start_row = 6,
      start_col = 10
    ) |>
    openxlsx2::wb_add_data(
      sheet = "mtcars1",
      x = "Queried",
      start_row = 5,
      start_col = 11
    );

  openxlsx2::wb_save(
    wb_prev,
    file = tempxlsx,
    overwrite = TRUE
  );


  affirm_init(replace = TRUE)
  options('affirm.id_cols' = c("car", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"))
  mtcars_modified |>
    affirm_false(
      label = "mpg gt 15",
      id = 1,
      condition = mpg > 15,
      data_frames = "mtcars"
    ) |>
    affirm_false(
      label = "mpg gt 10",
      id = 2,
      condition = mpg > 10,
      data_frames = "mtcars"
    );
  updated_tempxlsx <- tempfile(fileext = ".xlsx")

  affirm_report_excel(
    file = updated_tempxlsx,
    affirmation_name = "{data_frames}{id}",
    previous_file = tempxlsx
  )

  updated_affirm_sheet_status <-
    openxlsx2::wb_to_df(
      file = updated_tempxlsx,
      sheet = "mtcars1",
      start_row = 4
    ) |>
    dplyr::filter(!is.na(Status)) |>
    dplyr::pull("Status")

  updated_affirm_sheet_comment <-
    openxlsx2::wb_to_df(
      file = updated_tempxlsx,
      sheet = "mtcars1",
      start_row = 4
    ) |>
    dplyr::filter(!is.na(Comment)) |>
    dplyr::pull("Comment")

  expect_true(updated_affirm_sheet_status == "OK" & updated_affirm_sheet_comment == "Queried" )

  affirm_close()
}
)

