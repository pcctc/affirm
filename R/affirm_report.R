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

  # checking to make sure sheet name glue syntax has acceptable column names
  affirmation_name_cols <- regmatches(affirmation_name, gregexpr("\\{([^\\}]+)\\}", affirmation_name))[[1]] |>
    gsub("\\{|\\}", "", x = _)

  # acceptable variables to pass through glue syntax for sheet names
  glue_accept <- c("id", "label", "priority", "data_frames", "columns", "error_n", "total_n")

  # readable version for error messaging
  glue_accept_str <- paste0("`", glue_accept, "`", collapse = ", ")
  if (any(!affirmation_name_cols %in% glue_accept)){
    stop(paste0("`affirmation_name` glue syntax expects one of ", glue_accept_str))
  }

  df_report <-
    affirm_report_raw_data() |>
    dplyr::filter(.data$error_n > 0L) |>
    dplyr::mutate(
      affirmation_name = glue::glue(affirmation_name) |>
        gsub(pattern = "[[:punct:]]", replacement = "", x = _)
    ) |>
    dplyr::select(affirmation_name, dplyr::everything())

  # checking to make sure sheet names are not too long
  if (any(nchar(df_report$affirmation_name) > 31)){
    stop("At least one sheet name exceeds the allowed 31 characters.")
  }

  df_report$data <- df_report$data |>
    stats::setNames(df_report$affirmation_name)

  openxlsx::write.xlsx(df_report$data, file = file, overwrite = overwrite)
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


