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
                                               output_file_name = "extract.csv",
                                               variable_labels = FALSE) {
  if (is.null(data)) return("&mdash;")

  temp_file <-
    tempfile(pattern = paste0("csv_file"), fileext = ".csv")

  readr::write_csv(data, file = temp_file, col_names = !variable_labels)
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
