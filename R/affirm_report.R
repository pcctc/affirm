#' Affirmation Report
#'
#' - `affirm_report_gt()` returns styled gt table summarizing results of affirmation session.
#' - `affirm_report_raw_data()` returns raw data used to generate summary in `affirm_report_gt()`
#' - `affirm_export_excel()` returns excel file with one sheet per affirmation (excluding those with no errors)
#'
#' @inheritParams openxlsx::write.xlsx
#'
#' @return gt table
#' @name affirm_report
#'
#' @examples
#' affirm_init(replace = TRUE)
#'
#' as_tibble(mtcars) |>
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
affirm_export_excel <- function(file, overwrite = TRUE) {
  df_report <-
    affirm_report_raw_data() |>
    dplyr::filter(.data$error_n > 0L) |>
    dplyr::mutate(
      label_no_specials = gsub(pattern = "[[:punct:]]", replacement = "", x = .data$label),
      label_final =
        ifelse(
          is.na(.data$id),
          .data$label_no_specials,
          paste(.data$id, .data$label_no_specials)
        )
    )

  df_report$data |>
    stats::setNames(df_report$label_no_specials) |>
    openxlsx::write.xlsx(file = file, overwrite = overwrite)
}

#' @rdname affirm_report
#' @export
affirm_report_raw_data <- function() {
  .check_affirm_initialized()
  get(x = "df_affirmations", envir = env_affirm_logs) |>
    dplyr::mutate(
      error_rate = .data$error_n / .data$total_n,
      .after = "total_n"
    ) |>
    dplyr::arrange(.data$error_n == 0L, .data$priority, dplyr::desc(.data$error_rate))
}




