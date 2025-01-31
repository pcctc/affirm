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
#' @param previous_file A string of the file path to the previous affirmation workbook that needs to be updated
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
affirm_report_excel <- function(file, affirmation_name = "{data_frames}{id}", overwrite = TRUE, previous_file = NULL) {

  # Check to see if previous file is supplied#
  prev_exists <- !is.null(previous_file)

  if(prev_exists){
    # Check that previous file input is a character#
    prev_class_check <- !is.character(previous_file)

    # If prev file isn't a character, throw an error#
    if (prev_class_check){

      # Grab actual class
      actual_prev_class <- previous_file |> class()

      # Abort to the console#
      c("{cli::col_yellow('`previous_file`')} should be of class {.cls character}",
        "x" = 'You\'ve supplied a {cli::col_yellow("`previous_file`")} input of class {cli::col_red("<",{actual_prev_class},">")}') |>
        cli::cli_abort()
    }

  }

  # Create an initial summary df#
  df_summary_init <-
    affirm_report_raw_data() |>
    dplyr::mutate(
      affirmation_name = glue::glue(affirmation_name) |>
        gsub(pattern = "[[:punct:]]", replacement = "", x = _),
      assigned_to = NA
    ) |>
    # change order of excel report#
    dplyr::select(
      "assigned_to", "affirmation_name", "data_frames", "id", "priority", "columns", "error_n",
      "total_n", "error_rate", "label", "data"
      ) |>
    dplyr::arrange(affirmation_name)

  # checking to make sure sheet names are not too long#
  if (any(nchar(df_summary_init$affirmation_name) > 31)){
    stop("At least one sheet name exceeds the allowed 31 characters.")
  }

  # If a previous report is supplied, remove 'assigned_to'#
  # as we'll carry it forward from the previous report#
  if(prev_exists){
    df_summary_current <-
      df_summary_init |>
      dplyr::select(-"assigned_to")

    # Start the process to update the report#
    df_summary <- .update_summary_sheet(df_summary_current, previous_file)

  } else{
    # Otherwise, proceed without updating#
    df_summary <- df_summary_init
  }

  # this is the affirmation data that gets exported to each sheets
  # drops data column and columns with all NAs
    df_export_init <- .identify_keep_data(df_summary)
    vec_summary_cols <- names(df_export_init)
    original_summary_cols <- vec_summary_cols[!vec_summary_cols %in% c("Status", "Comment")]
    add_status <- !"Status" %in% vec_summary_cols
    add_comment <- !"Comment" %in% vec_summary_cols

    # If status column is missing, add it#
    if(add_status){
      df_export_init <-
        df_export_init |>
        dplyr::mutate(Status = NA)
    }

    # If comment column is missing, add it
    if(add_comment){
      df_export_init <-
        df_export_init |>
        dplyr::mutate(Comment = NA)
    }

    # Create a final export with all applicable columns#
    df_export <-
      df_export_init |>
      dplyr::select(
        dplyr::all_of(original_summary_cols), "Status", "Comment"
        )

  # create excel workbook with all affirmations
  wb <- openxlsx2::wb_workbook() |>
    .add_summary_sheet(df_export)

  for (i in seq_len(nrow(df_export))){
    wb <- .add_affirmation_sheet(wb, df_summary[i, ], prev_exists)
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
    )

  df_report
}


