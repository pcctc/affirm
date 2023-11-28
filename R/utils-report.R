.affirm_report_gt_stylings <- function(x) {
  x |>
  gt::cols_width("status_color" ~ gt::px(6)) |>
    gt::cols_label(
      status_color = "",
      id = gt::md("**ID**"),
      label = gt::md("**Affirmation**"),
      priority = gt::md("**Priority**"),
      data_frames = gt::md("**Data Frames**"),
      columns = gt::md("**Columns**"),
      error_n = gt::md("**No. Errors**"),
      total_n = gt::md("**Total No. Checks**"),
      error_rate = gt::md("**Error Rate**"),
      csv_download_link = gt::md("**Listing Download**")
    ) |>
    gt::fmt_markdown(columns = "csv_download_link") |>
    gt::cols_align(align = "center") |>
    gt::cols_align(align = "left", columns = any_of(c("id", "label", "priority", "data_frames", "columns"))) |>
    gt::fmt_percent(
      columns = .data$error_rate,
      decimals = 1
    ) |>
    gt::tab_options(table.font.size = 15, data_row.padding = gt::px(1),
                    summary_row.padding = gt::px(1), grand_summary_row.padding = gt::px(1),
                    footnotes.padding = gt::px(1), source_notes.padding = gt::px(1),
                    row_group.padding = gt::px(1)) |>
    gt::sub_missing() |>
    gt::sub_missing(columns = "status_color", missing_text = "") |>
    gt::tab_style(
      style = gt::cell_fill(color = "#D61F1F"),
      locations = gt::cells_body(
        columns = "status_color",
        rows = .data$priority %in% 1L | dplyr::between(.data$error_rate, 0.50, 1)
      )
    ) |>
    gt::tab_style(
      style = gt::cell_fill(color = "#FFD301"),
      locations = gt::cells_body(
        columns = "status_color",
        rows = !.data$priority %in% 1L & dplyr::between(.data$error_rate, 0.10, 0.50)
      )
    ) |>
    gt::tab_style(
      style = gt::cell_fill(color = "#7BB662", alpha = 0.5),
      locations = gt::cells_body(
        columns = "status_color",
        rows = !.data$priority %in% 1L & !.data$error_rate %in% 0 & dplyr::between(.data$error_rate, 0, 0.10)
      )
    ) |>
    gt::tab_style(
      style = gt::cell_fill(color = "#006B3D"),
      locations = gt::cells_body(
        columns = "status_color",
        rows = .data$error_rate %in% 0
      )
    )
  # # this hides optional columns that are all NA
  # # this code does not work in gt 0.9.0, because it errors if no columns are selected.
  # # I submitted a PR to gt to allow for no columns being selected and should be in the next release
  # gt::cols_hide(columns = where(\(x) all(is.na(x))))
}


# converts a data frame to a clickable CSV download link
.as_csv_encoded_html_download_link <- function(data,
                                               output_file_name = "extract.csv") {
  if (is.null(data)) return("&mdash;")

  temp_file <-
    tempfile(pattern = paste0("csv_file"), fileext = ".csv")

  readr::write_csv(data, file = temp_file)
  on.exit(unlink(temp_file))

  file_encoded <- base64enc::base64encode(temp_file)
  title_text <- "Not sure what title text is for"

  as.character(
    htmltools::a(
      href = paste0("data:text/csv;base64,", file_encoded),
      download = output_file_name,
      htmltools::tags$button(
        `aria-label` = title_text,
        `data-balloon-pos` = "left",
        style = htmltools::css(
          `background-color` = "#02304D",
          color = "#FFFFFF",
          border = "none",
          padding = "5px",
          `font-weight` = "bold",
          cursor = "pointer",
          `border-radius` = "4px"
        ),
        "CSV"
      )
    )
  )
}


# this function adds two new rows to the data frame: column names and column labels
.add_row_of_column_labels <- function(data) {
  # extract labels as list -----------------------------------------------------
  lst_labels <-
    mapply(
      FUN = function(x, y) attr(x, 'label') %||% y,
      data |> as.list(),
      names(data),
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE
    ) |>
    stats::setNames(names(data))

  # make all columns character -------------------------------------------------
  data <- data |> dplyr::mutate(dplyr::across(everything(), as.character))

  # add a row with just the column names ---------------------------------------
  data <- tibble::add_row(data, !!!stats::setNames(as.list(names(data)), names(data)), .before = 0L)

  # add a row with the column labels -------------------------------------------
  data <- tibble::add_row(data, !!!lst_labels, .before = 0L)

  data
}

#' Utils-report: compute column widths for excel exports
#'
#' @param data a data frame of individual affirmation results
#' @param min_width minimum column width for excel export
#' @param max_width maximum column width for excel export
#' @param pad the number of characters to pad the width; so if there are two
#' characters you would pad by an additional set amount for some breathing room
#' @return a named numeric vector
#'
.compute_col_width <- function(data, min_width = 8, max_width = 50, pad = 3){
  # create dummy data frame with column names and variable values
  # in order to compute max length for setting column widths
  vec_lengths <- data.frame(t(names(data))) |>
    stats::setNames(names(data)) |>
    # add data as character values
    dplyr::bind_rows(data |> lapply(as.character)) |>
    # find character length of all entries in each column
    lapply(nchar) |>
    # pad the character length for extra space
    lapply(\(x) {x + pad}) |>
    # find max character length with a set minimum value
    lapply(max, min_width, na.rm = TRUE) |>
    # now set maximum width
    lapply(min, max_width, na.rm = TRUE) |>
    unlist()

  # set a width for notes columns
  vec_lengths[["Notes"]] <- 30

  return(vec_lengths)
}

#' Utils-report: extract variable labels from a data frame
#' @param data a data frame of individual affirmation results
#' @return a data frame with one row of variable labels
.retrieve_labels <- function(data){
  lapply(data, attr, "label") |>
    lapply(\(x) {ifelse(is.null(x), NA_character_, x)}) |>
    data.frame()
}


#' Utils-report: identify non-NA data to output in excel report.
#'
#' If an argument is not supplied to affirm_ # and all values of column are NA,
#' then remove that column from the excel export.
#'
#' @param df_summary a data frame of the overall affirmation report
#' @return a data frame for the excel export
.identify_keep_data <- function(df_summary){
  # identify which columns have affirmation fields entered
  vec_present <- lapply(df_summary, \(x) {!all(is.na(x))})
  # identify which columns to keep
  vec_keep_cols <- names(unlist(vec_present[unlist(vec_present)]))
  # create a data frame with columns to keep
  df_keep <- df_summary |>
    dplyr::select(dplyr::all_of(vec_keep_cols)) |>
    dplyr::select(-"data")

  return(df_keep)
}

#' Utils-report:  add first sheet with summary of affirmations to excel workbook
#'
#' @param wb a workbook object
#' @param df_export a data frame summarizing the affirmations to export
#' @return a workbook object
#'
.add_summary_sheet <- function(wb, df_export){
  wb |>
  # add front page with summary information ----
  openxlsx2::wb_add_worksheet("Summary") |>
    openxlsx2::wb_add_data_table(
      x = df_export,
      na.strings = "",
      table_style = "TableStyleLight8"
    ) |>
    openxlsx2::wb_set_col_widths(
      cols = 1:ncol(df_export),
      widths = .compute_col_width(df_export)
    )
}

#' Utils-report:  add sheet for an individual affirmation
#' @param wb a workbook object
#' @param df_summary_row a data frame with a single row from the affirmation
#' summary table
#' @return a workbook object
#'
.add_affirmation_sheet <- function(wb, df_summary_row){

  # data frame of single affirmation results
  df_affirmation <- df_summary_row[["data"]][[1]] |>
    dplyr::mutate(Notes = NA)

  # labels of the data frame of single affirmation results

  df_labels <- .retrieve_labels(df_affirmation)
  vec_widths <- .compute_col_width(df_affirmation)

  wb <- wb |>
    openxlsx2::wb_add_worksheet(df_summary_row[["affirmation_name"]]) |>
    # add affirmation label on first row
    openxlsx2::wb_add_data(
      x = df_summary_row[["label"]][[1]],
      na.strings = "",
      start_row = 1
    ) |>
    # style affirmation label
    openxlsx2::wb_add_font(
      dims = "A1:A1",
      bold = "double"
    ) |>
    # merge cells on affirmation label
    openxlsx2::wb_merge_cells(
      dims = "A1:P1"
    ) |>
    # wrap text on affirmation label
    openxlsx2::wb_add_cell_style(
      dims = "A1:A1",
      wrap_text = TRUE
    ) |>
    # add variable labels above variable names
    openxlsx2::wb_add_data(
      x = df_labels,
      na.strings = "",
      start_row = 3,
      col_names = FALSE
    ) |>
    # style variable labels
    openxlsx2::wb_add_font(
      dims = openxlsx2::wb_dims(x = df_labels, from_row = 3, col_names = FALSE),
      italic = "italic",
      color = openxlsx2::wb_color(hex = "#7F7F7F")
    ) |>
    # wrap text on variable labels
    openxlsx2::wb_add_cell_style(
      dims = openxlsx2::wb_dims(x = df_labels, from_row = 3, col_names = FALSE),
      wrap_text = TRUE
    ) |>
    # add data on lower row
    openxlsx2::wb_add_data_table(
      x = df_affirmation,
      na.strings = "",
      table_style = "TableStyleLight8",
      start_row = 4
    ) |>
    openxlsx2::wb_set_col_widths(
      cols = seq_len(ncol(df_affirmation)),
      widths = .compute_col_width(df_affirmation)
    )

  return(wb)
}
