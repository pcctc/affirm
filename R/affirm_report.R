#' Affirmation Report
#'
#' - `affirm_report_gt()` returns styled gt table summarizing results of affirmation session.
#' - `affirm_report_excel()` returns excel file with one sheet per affirmation (excluding those with no errors)
#' - `affirm_report_raw_data()` returns raw data used to generate summary in `affirm_report_gt()`
#'
#' @inheritParams openxlsx::write.xlsx
#' @param affirmation_name A string for affirmation names; the item name
#' in curly brackets is replaced with the item value (see glue::glue). Item names
#' accepted include: `id`, `label`, `priority`, `data_frames`, `columns`, `error_n`, `total_n`.
#' Defaults to `"{data_frames}{id}"`.
#'
#' @return gt table
#' @name affirm_report
#'
#' @examples
#' affirm_init(replace = TRUE)
#'
#' dplyr::as_tibble(mtcars) |>
#'  affirm_true(
#'    label = "No. cylinders must be 4, 6, or 8",
#'    condition = cyl %in% c(4, 6, 8)
#'  ) |>
#'  affirm_true(
#'     label = "MPG should be less than 33",
#'     condition = mpg < 33
#'  )
#'
#' gt_report <- affirm_report_gt()
#'
#' affirm_close()
NULL

#' @rdname affirm_report
#' @export
affirm_report_gt <- function() {
  affirm_report_raw_data() |>
    dplyr::mutate(status_color = NA_character_, .before = 1L) |>
    dplyr::mutate(
      csv_download_link =
        mapply(
          FUN = .as_csv_encoded_html_download_link,
          # these two args are the ones being passed to FUN
          .data$data,
          paste0("extract_", dplyr::row_number(), ".csv"),
          # additional mapply args
          SIMPLIFY = TRUE,
          USE.NAMES = FALSE
        )
    ) |>
    dplyr::select(-"data") |>
    gt::gt() |>
    .affirm_report_gt_stylings()
}


#' @rdname affirm_report
#' @export
affirm_report_excel <- function(file, affirmation_name = "{data_frames}{id}", overwrite = TRUE) {

  df_summary <-
    affirm_report_raw_data() |>
    dplyr::mutate(
      affirmation_name = glue::glue(affirmation_name) |>
        gsub(pattern = "[[:punct:]]", replacement = "", x = _)
    ) |>
    # change order of excel report
    dplyr::select(
      "affirmation_name", "data_frames", "id", "priority", "columns", "error_n",
      "total_n", "error_rate", "label", "data"
      )

  # checking to make sure sheet names are not too long
  if (any(nchar(df_summary$affirmation_name) > 31)){
    stop("At least one sheet name exceeds the allowed 31 characters.")
  }

  # this is the affirmation data that gets exported to each sheets
  # drops data column and columns with all NAs
  df_export <- .identify_keep_data(df_summary) |>
    # add empty Notes column to end
    mutate(Notes = NA)

  # create excel workbook with all affirmations
  wb <- openxlsx2::wb_workbook() |>
    .add_summary_sheet(df_export)

  for (i in seq_len(nrow(df_summary))){
    wb <- .add_affirmation_sheet(wb, df_summary[i, ])
  }

  openxlsx2::wb_save(wb, file = file, overwrite = TRUE)
}

#' @rdname affirm_report
#' @export
affirm_report_raw_data <- function() {
  .check_affirm_initialized()
  df_report <-
    get(x = "df_affirmations", envir = env_affirm_logs) |>
    dplyr::mutate(
      error_rate = .data$error_n / .data$total_n,
      .after = "total_n"
    ) |>
    dplyr::arrange(.data$error_n == 0L, .data$priority, dplyr::desc(.data$error_rate))

  df_report
}


