#' Affirmation Report
#'
#' - `affirm_report_gt()` returns styled gt table summarizing results of affirmation session.
#' - `affirm_report_excel()` returns excel file with one sheet per affirmation (excluding those with no errors)
#' - `affirm_report_raw_data()` returns raw data used to generate summary in `affirm_report_gt()`
#'
#' @inheritParams openxlsx::write.xlsx
#' @param variable_labels logical indicating whether to add a row to exported
#' data with the variable labels. If label does not exist, the variable name
#' is printed. Default is `FALSE`
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
affirm_report_gt <- function(variable_labels = FALSE) {
  affirm_report_raw_data(variable_labels = variable_labels) |>
    dplyr::mutate(status_color = NA_character_, .before = 1L) |>
    dplyr::mutate(
      csv_download_link =
        mapply(
          FUN = .as_csv_encoded_html_download_link,
          # these two args are the ones being passed to FUN
          .data$data,
          paste0("extract_", dplyr::row_number(), ".csv"),
          variable_labels,
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
affirm_report_excel <- function(file, sheet_name = "{data_frames}{id}", variable_labels = FALSE, overwrite = TRUE) {

  # checking to make sure sheet name glue syntax has acceptable column names
  sheet_name_cols <- unlist(strsplit(sheet_name, "[{}]"))
  sheet_name_cols <- sheet_name_cols[sheet_name_cols != ""]
  glue_accept <- c("id", "label", "priority", "data_frames", "columns", "error_n", "total_n")
  if (any(!sheet_name_cols %in% glue_accept)){
    stop(paste0("`sheet_name` glue syntax expects one of ", paste(glue_accept, collapse = ", ")))
  }

  df_report <-
    affirm_report_raw_data(variable_labels = variable_labels) |>
    dplyr::filter(.data$error_n > 0L) |>
    dplyr::mutate(
      label_final = glue::glue(sheet_name) |>
        gsub(pattern = "[[:punct:]]", replacement = "", x = _)
    )

  # checking to make sure sheet names are not too long
  if (any(nchar(df_report$label_final) > 31)){
    stop("At least one sheet name exceeds the allowed 31 characters.")
  }

  df_report$data |>
    stats::setNames(df_report$label_final) |>
    openxlsx::write.xlsx(file = file, overwrite = overwrite, colNames = !variable_labels)
}

#' @rdname affirm_report
#' @export
affirm_report_raw_data <- function(variable_labels = FALSE) {
  .check_affirm_initialized()
  df_report <-
    get(x = "df_affirmations", envir = env_affirm_logs) |>
    dplyr::mutate(
      error_rate = .data$error_n / .data$total_n,
      .after = "total_n"
    ) |>
    dplyr::arrange(.data$error_n == 0L, .data$priority, dplyr::desc(.data$error_rate))

  # add the variable labels to the top row if requested
  if (isTRUE(variable_labels)) {
    df_report$data <- lapply(df_report$data, .add_row_of_column_labels)
  }

  df_report
}




